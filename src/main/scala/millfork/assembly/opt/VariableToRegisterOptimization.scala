package millfork.assembly.opt

import millfork.{CompilationOptions, NonOverlappingIntervals}
import millfork.assembly.{AddrMode, AssemblyLine}
import millfork.assembly.Opcode._
import millfork.assembly.AddrMode._
import millfork.env._
import millfork.error.ErrorReporting

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * @author Karol Stasiak
  */
object VariableToRegisterOptimization extends AssemblyOptimization {

  // If any of these opcodes is present within a method,
  // then it's too hard to assign any variable to a register.
  private val opcodesThatAlwaysPrecludeXAllocation = Set(JSR, STX, TXA, PHX, PLX, INX, DEX, CPX, SBX, SAX)

  private val opcodesThatAlwaysPrecludeYAllocation = Set(JSR, STY, TYA, PHY, PLY, INY, DEY, CPY)

  // If any of these opcodes is used on a variable
  // then it's too hard to assign that variable to a register.
  // Also, LDY prevents assigning a variable to X and LDX prevents assigning a variable to Y.
  private val opcodesThatCannotBeUsedWithIndexRegistersAsParameters =
  Set(EOR, ORA, AND, BIT, ADC, SBC, CMP, CPX, CPY, STY, STX)

  override def name = "Allocating variables to index registers"


  override def optimize(f: NormalFunction, code: List[AssemblyLine], options: CompilationOptions): List[AssemblyLine] = {
    val paramVariables = f.params match {
      case NormalParamSignature(ps) =>
        ps.map(_.name).toSet
      case _ =>
        // assembly functions do not get this optimization
        return code
    }
    val stillUsedVariables = code.flatMap {
      case AssemblyLine(_, _, MemoryAddressConstant(th), _) => Some(th.name)
      case _ => None
    }.toSet
    val localVariables = f.environment.getAllLocalVariables.filter {
      case MemoryVariable(name, typ, VariableAllocationMethod.Auto) =>
        typ.size == 1 && !paramVariables(name) && stillUsedVariables(name)
      case _ => false
    }

    val variablesWithLifetimes = localVariables.map(v =>
      v.name -> VariableLifetime.apply(v.name, code)
    )

    val importances = ReverseFlowAnalyzer.analyze(f, code)

    val xCandidates = variablesWithLifetimes.filter {
      case (vName, range) =>
        importances(range.start).x != Important
    }.flatMap {
      case (vName, range) =>
        canBeInlined(Some(vName), None, code.slice(range.start, range.end)).map(score => (vName, range, score))
    }

    val yCandidates = variablesWithLifetimes.filter {
      case (vName, range) =>
        importances(range.start).y != Important
    }.flatMap {
      case (vName, range) =>
        canBeInlined(None, Some(vName), code.slice(range.start, range.end)).map(score => (vName, range, score))
    }

    val xCandidateSets = NonOverlappingIntervals.apply[(String, Range, Int)](xCandidates, _._2.start, _._2.end)
    val yCandidateSets = NonOverlappingIntervals.apply[(String, Range, Int)](yCandidates, _._2.start, _._2.end)

    val variants = for {
      vx <- xCandidateSets.par
      vy <- yCandidateSets
      if (vx & vy).isEmpty
      score = vx.toSeq.map(_._3).sum + vy.toSeq.map(_._3).sum
    } yield (score, vx, vy)

    if (variants.isEmpty) {
      return code
    }

    val (_, bestXs, bestYs) = variants.maxBy(_._1)

    if (bestXs.nonEmpty || bestYs.nonEmpty) {
      (bestXs.size, bestYs.size) match {
        case (0, 0) =>
        case (_, 0) => ErrorReporting.debug(s"Inlining ${bestXs.map(_._1).mkString(", ")} to register X")
        case (0, _) => ErrorReporting.debug(s"Inlining ${bestYs.map(_._1).mkString(", ")} to register Y")
        case (_, _) => ErrorReporting.debug(s"Inlining ${bestXs.map(_._1).mkString(", ")} to register X and ${bestYs.map(_._1).mkString(", ")} to register Y")
      }
      bestXs.foreach(v => f.environment.removeVariable(v._1))
      bestYs.foreach(v => f.environment.removeVariable(v._1))
      val output = ListBuffer[AssemblyLine]()
      var i = 0
      while (i < code.length) {
        var done = false
        bestXs.find(_._2.start == i).foreach {
          case (v, range, _) =>
            output ++= inlineVars(Some(v), None, code.slice(range.start, range.end))
            i = range.end
            done = true
        }
        if (!done) {
          bestYs.find(_._2.start == i).foreach {
            case (v, range, _) =>
              output ++= inlineVars(None, Some(v), code.slice(range.start, range.end))
              i = range.end
              done = true
          }
        }
        if (!done) {
          output += code(i)
          i += 1
        }
      }
      output.toList
    } else {
      code
    }
  }


  def canBeInlined(xCandidate: Option[String], yCandidate: Option[String], lines: List[AssemblyLine]): Option[Int] = {
    val vx = xCandidate.getOrElse("-")
    val vy = yCandidate.getOrElse("-")
    lines match {
      case AssemblyLine(_, Immediate, SubbyteConstant(MemoryAddressConstant(th), _), _) :: xs
        if th.name == vx || th.name == vy =>
        // if an address of a variable is used, then that variable cannot be assigned to a register
        None
      case AssemblyLine(_, Immediate, HalfWordConstant(MemoryAddressConstant(th), _), _) :: xs
        if th.name == vx || th.name == vy =>
        // if an address of a variable is used, then that variable cannot be assigned to a register
        None

      case AssemblyLine(_, AbsoluteX | AbsoluteY | ZeroPageX | ZeroPageY, MemoryAddressConstant(th), _) :: xs =>
        // if a variable is used as an array, then it cannot be assigned to a register
        if (th.name == vx || th.name == vy) {
          None
        } else {
          canBeInlined(xCandidate, yCandidate, xs)
        }

      case AssemblyLine(opcode, Absolute, MemoryAddressConstant(th), _) :: xs
        if th.name == vx && (opcode == LDY || opcodesThatCannotBeUsedWithIndexRegistersAsParameters(opcode)) =>
        // if a variable is used by some opcodes, then it cannot be assigned to a register
        None

      case AssemblyLine(opcode, Absolute, MemoryAddressConstant(th), _) :: xs
        if th.name == vy && (opcode == LDX || opcode == LAX || opcodesThatCannotBeUsedWithIndexRegistersAsParameters(opcode)) =>
        // if a variable is used by some opcodes, then it cannot be assigned to a register
        None

      case AssemblyLine(LDX, Absolute, MemoryAddressConstant(th), elidable) :: xs
        if xCandidate.isDefined =>
        // if a register is populated with a different variable, then this variable cannot be assigned to that register
        // removing LDX saves 3 cycles
        if (elidable && th.name == vx) {
          canBeInlined(xCandidate, yCandidate, xs).map(_ + 3)
        } else {
          None
        }

      case AssemblyLine(LAX, Absolute, MemoryAddressConstant(th), elidable) :: xs
        if xCandidate.isDefined =>
        // LAX = LDX-LDA, and since LDX simplifies to nothing and LDA simplifies to TXA,
        // LAX simplifies to TXA, saving two bytes
        if (elidable && th.name == vx) {
          canBeInlined(xCandidate, yCandidate, xs).map(_ + 2)
        } else {
          None
        }

      case AssemblyLine(LDY, Absolute, MemoryAddressConstant(th), elidable) :: xs if yCandidate.isDefined =>
        // if a register is populated with a different variable, then this variable cannot be assigned to that register
        // removing LDX saves 3 cycles
        if (elidable && th.name == vy) {
          canBeInlined(xCandidate, yCandidate, xs).map(_ + 3)
        } else {
          None
        }

      case AssemblyLine(LDX, _, _, _) :: xs if xCandidate.isDefined =>
        // if a register is populated with something else than a variable, then no variable cannot be assigned to that register
        None

      case AssemblyLine(LDY, _, _, _) :: xs if yCandidate.isDefined =>
        // if a register is populated with something else than a variable, then no variable cannot be assigned to that register
        None

      case AssemblyLine(LDA, Absolute, MemoryAddressConstant(th), elidable) :: AssemblyLine(TAX, _, _, elidable2) :: xs
        if xCandidate.isDefined =>
        // a variable cannot be inlined if there is TAX not after LDA of that variable
        // but LDA-TAX can be simplified to TXA
        if (elidable && elidable2 && th.name == vx) {
          canBeInlined(xCandidate, yCandidate, xs).map(_ + 3)
        } else {
          None
        }

      case AssemblyLine(LDA, Absolute, MemoryAddressConstant(th), elidable) :: AssemblyLine(TAY, _, _, elidable2) :: xs
        if yCandidate.isDefined =>
        // a variable cannot be inlined if there is TAY not after LDA of that variable
        // but LDA-TAY can be simplified to TYA
        if (elidable && elidable2 && th.name == vy) {
          canBeInlined(xCandidate, yCandidate, xs).map(_ + 3)
        } else {
          None
        }

      case AssemblyLine(LDA | STA | INC | DEC, Absolute, MemoryAddressConstant(th), elidable) :: xs =>
        // changing LDA->TXA, STA->TAX, INC->INX, DEC->DEX saves 2 cycles
        if (th.name == vy || th.name == vx) {
          if (elidable) {
            canBeInlined(xCandidate, yCandidate, xs).map(_ + 2)
          } else {
            None
          }
        } else {
          canBeInlined(xCandidate, yCandidate, xs)
        }

      case AssemblyLine(TAX, _, _, _) :: xs if xCandidate.isDefined =>
        // a variable cannot be inlined if there is TAX not after LDA of that variable
        None

      case AssemblyLine(TAY, _, _, _) :: xs if yCandidate.isDefined =>
        // a variable cannot be inlined if there is TAY not after LDA of that variable
        None

      case AssemblyLine(LABEL, _, _, _) :: xs =>
        // labels always end the initial section
        canBeInlined(xCandidate, yCandidate, xs)

      case x :: xs =>
        if (xCandidate.isDefined && opcodesThatAlwaysPrecludeXAllocation(x.opcode)) {
          None
        } else if (yCandidate.isDefined && opcodesThatAlwaysPrecludeYAllocation(x.opcode)) {
          None
        } else {
          canBeInlined(xCandidate, yCandidate, xs)
        }

      case Nil => Some(0)
    }
  }

  def inlineVars(xCandidate: Option[String], yCandidate: Option[String], lines: List[AssemblyLine]): List[AssemblyLine] = {
    val vx = xCandidate.getOrElse("-")
    val vy = yCandidate.getOrElse("-")
    lines match {
      case AssemblyLine(INC, Absolute, MemoryAddressConstant(th), _) :: xs
        if th.name == vx =>
        AssemblyLine.implied(INX) :: inlineVars(xCandidate, yCandidate, xs)

      case AssemblyLine(INC, Absolute, MemoryAddressConstant(th), _) :: xs
        if th.name == vy =>
        AssemblyLine.implied(INY) :: inlineVars(xCandidate, yCandidate, xs)

      case AssemblyLine(DEC, Absolute, MemoryAddressConstant(th), _) :: xs
        if th.name == vx =>
        AssemblyLine.implied(DEX) :: inlineVars(xCandidate, yCandidate, xs)

      case AssemblyLine(DEC, Absolute, MemoryAddressConstant(th), _) :: xs
        if th.name == vy =>
        AssemblyLine.implied(DEY) :: inlineVars(xCandidate, yCandidate, xs)

      case AssemblyLine(LDX, Absolute, MemoryAddressConstant(th), _) :: xs
        if th.name == vx =>
        inlineVars(xCandidate, yCandidate, xs)

      case AssemblyLine(LAX, Absolute, MemoryAddressConstant(th), _) :: xs
        if th.name == vx =>
        AssemblyLine.implied(TXA) :: inlineVars(xCandidate, yCandidate, xs)

      case AssemblyLine(LDY, Absolute, MemoryAddressConstant(th), _) :: xs
        if th.name == vy =>
        inlineVars(xCandidate, yCandidate, xs)

      case AssemblyLine(LDA, Absolute, MemoryAddressConstant(th), true) :: AssemblyLine(TAX, _, _, true) :: xs
        if th.name == vx =>
        // these TXA's may get optimized away by a different optimization
        AssemblyLine.implied(TXA) :: inlineVars(xCandidate, yCandidate, xs)

      case AssemblyLine(LDA, Absolute, MemoryAddressConstant(th), true) :: AssemblyLine(TAY, _, _, true) :: xs
        if th.name == vy =>
        // these TYA's may get optimized away by a different optimization
        AssemblyLine.implied(TYA) :: inlineVars(xCandidate, yCandidate, xs)

      case AssemblyLine(LDA, am, param, true) :: AssemblyLine(STA, Absolute, MemoryAddressConstant(th), true) :: xs
        if th.name == vx && doesntUseX(am) =>
        // these TXA's may get optimized away by a different optimization
        AssemblyLine(LDX, am, param) :: AssemblyLine.implied(TXA) :: inlineVars(xCandidate, yCandidate, xs)

      case AssemblyLine(LDA, am, param, true) :: AssemblyLine(STA, Absolute, MemoryAddressConstant(th), true) :: xs
        if th.name == vy && doesntUseY(am) =>
        // these TYA's may get optimized away by a different optimization
        AssemblyLine(LDY, am, param) :: AssemblyLine.implied(TYA) :: inlineVars(xCandidate, yCandidate, xs)

      case AssemblyLine(LDA, Absolute, MemoryAddressConstant(th), _) :: AssemblyLine(CMP, am, param, true) :: xs
        if th.name == vx && doesntUseXOrY(am) =>
        // ditto
        AssemblyLine.implied(TXA) :: AssemblyLine(CPX, am, param) :: inlineVars(xCandidate, yCandidate, xs)

      case AssemblyLine(LDA, Absolute, MemoryAddressConstant(th), _) :: AssemblyLine(CMP, am, param, true) :: xs
        if th.name == vy && doesntUseXOrY(am) =>
        // ditto
        AssemblyLine.implied(TYA) :: AssemblyLine(CPY, am, param) :: inlineVars(xCandidate, yCandidate, xs)

      case AssemblyLine(LDA, Absolute, MemoryAddressConstant(th), _) :: xs
        if th.name == vx =>
        AssemblyLine.implied(TXA) :: inlineVars(xCandidate, yCandidate, xs)

      case AssemblyLine(LDA, Absolute, MemoryAddressConstant(th), _) :: xs
        if th.name == vy =>
        AssemblyLine.implied(TYA) :: inlineVars(xCandidate, yCandidate, xs)

      case AssemblyLine(STA, Absolute, MemoryAddressConstant(th), _) :: xs
        if th.name == vx =>
        AssemblyLine.implied(TAX) :: inlineVars(xCandidate, yCandidate, xs)

      case AssemblyLine(STA, Absolute, MemoryAddressConstant(th), _) :: xs
        if th.name == vy =>
        AssemblyLine.implied(TAY) :: inlineVars(xCandidate, yCandidate, xs)

      case AssemblyLine(TAX, _, _, _) :: xs if xCandidate.isDefined =>
        ErrorReporting.fatal("Unexpected TAX")

      case AssemblyLine(TAY, _, _, _) :: xs if yCandidate.isDefined =>
        ErrorReporting.fatal("Unexpected TAY")

      case x :: xs => x :: inlineVars(xCandidate, yCandidate, xs)

      case Nil => Nil
    }
  }

  def doesntUseY(am: AddrMode.Value): Boolean = am match {
    case AbsoluteY | ZeroPageY | IndexedY => false
    case _ => true
  }

  def doesntUseX(am: AddrMode.Value): Boolean = am match {
    case AbsoluteX | ZeroPageX | IndexedX => false
    case _ => true
  }

  def doesntUseXOrY(am: AddrMode.Value): Boolean = am match {
    case Immediate | ZeroPage | Absolute | Relative | Indirect => true
    case _ => false
  }
}
