package millfork.assembly.opt

import millfork.CompilationOptions
import millfork.assembly.{AddrMode, AssemblyLine}
import millfork.assembly.Opcode._
import millfork.assembly.AddrMode._
import millfork.env._
import millfork.error.ErrorReporting

import scala.annotation.tailrec

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

    val candidates = None :: localVariables.map(v => Option(v.name))

    val variants = for {
      vx <- candidates.par
      vy <- candidates
      if vx != vy
      (score, prologueLength) <- canBeInlined(vx, vy, code.tail, Some(1))
      if prologueLength >= 1
    } yield (score, prologueLength, vx, vy)

    if (variants.isEmpty) {
      return code
    }

    val (_, bestPrologueLength, bestX, bestY) = variants.max

    if ((bestX.isDefined || bestY.isDefined) && bestPrologueLength != 0xffff) {
      (bestX, bestY) match {
        case (Some(x), Some(y)) => ErrorReporting.debug(s"Inlining $x to X and $y to Y")
        case (Some(x), None) => ErrorReporting.debug(s"Inlining $x to X")
        case (None, Some(y)) => ErrorReporting.debug(s"Inlining $y to Y")
        case _ =>
      }
      bestX.foreach(f.environment.removeVariable)
      bestY.foreach(f.environment.removeVariable)
      code.take(bestPrologueLength) ++ inlineVars(bestX, bestY, code.drop(bestPrologueLength))
    } else {
      code
    }
  }


  private def add(i: Int) = (p: (Int, Int)) => (p._1 + i) -> p._2

  private def mark(i: Option[Int]) = (p: (Int, Int)) => p._1 -> i.getOrElse(p._2)

  def canBeInlined(xCandidate: Option[String], yCandidate: Option[String], lines: List[AssemblyLine], instrCounter: Option[Int]): Option[(Int, Int)] = {
    val vx = xCandidate.getOrElse("-")
    val vy = yCandidate.getOrElse("-")
    val next = instrCounter.map(_ + 1)
    val next2 = instrCounter.map(_ + 2)
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
          canBeInlined(xCandidate, yCandidate, xs, next)
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
          canBeInlined(xCandidate, yCandidate, xs, None).map(add(3)).map(mark(instrCounter))
        } else {
          None
        }

      case AssemblyLine(LAX, Absolute, MemoryAddressConstant(th), elidable) :: xs
        if xCandidate.isDefined =>
        // LAX = LDX-LDA, and since LDX simplifies to nothing and LDA simplifies to TXA,
        // LAX simplifies to TXA, saving two bytes
        if (elidable && th.name == vx) {
          canBeInlined(xCandidate, yCandidate, xs, None).map(add(2)).map(mark(instrCounter))
        } else {
          None
        }

      case AssemblyLine(LDY, Absolute, MemoryAddressConstant(th), elidable) :: xs if yCandidate.isDefined =>
        // if a register is populated with a different variable, then this variable cannot be assigned to that register
        // removing LDX saves 3 cycles
        if (elidable && th.name == vy) {
          canBeInlined(xCandidate, yCandidate, xs, None).map(add(3)).map(mark(instrCounter))
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
          canBeInlined(xCandidate, yCandidate, xs, None).map(add(3)).map(mark(instrCounter))
        } else {
          None
        }

      case AssemblyLine(LDA, Absolute, MemoryAddressConstant(th), elidable) :: AssemblyLine(TAY, _, _, elidable2) :: xs
        if yCandidate.isDefined =>
        // a variable cannot be inlined if there is TAY not after LDA of that variable
        // but LDA-TAY can be simplified to TYA
        if (elidable && elidable2 && th.name == vy) {
          canBeInlined(xCandidate, yCandidate, xs, None).map(add(3)).map(mark(instrCounter))
        } else {
          None
        }

      case AssemblyLine(LDA | STA | INC | DEC, Absolute, MemoryAddressConstant(th), elidable) :: xs =>
        // changing LDA->TXA, STA->TAX, INC->INX, DEC->DEX saves 2 cycles
        if (th.name == vy || th.name == vx) {
          if (elidable) canBeInlined(xCandidate, yCandidate, xs, None).map(add(2)).map(mark(instrCounter))
          else None
        } else {
          canBeInlined(xCandidate, yCandidate, xs, next)
        }

      case AssemblyLine(TAX, _, _, _) :: xs if xCandidate.isDefined =>
        // a variable cannot be inlined if there is TAX not after LDA of that variable
        if (instrCounter.isDefined) {
          canBeInlined(xCandidate, yCandidate, xs, next)
        } else None

      case AssemblyLine(TAY, _, _, _) :: xs if yCandidate.isDefined =>
        // a variable cannot be inlined if there is TAY not after LDA of that variable
        if (instrCounter.isDefined) {
          canBeInlined(xCandidate, yCandidate, xs, next)
        } else None

      case AssemblyLine(LABEL, _, _, _) :: xs =>
        // labels always end the initial section
        canBeInlined(xCandidate, yCandidate, xs, None).map(mark(instrCounter))

      case x :: xs =>
        if (instrCounter.isDefined) {
          canBeInlined(xCandidate, yCandidate, xs, next)
        } else {
          if (xCandidate.isDefined && opcodesThatAlwaysPrecludeXAllocation(x.opcode)) {
            None
          } else if (yCandidate.isDefined && opcodesThatAlwaysPrecludeYAllocation(x.opcode)) {
            None
          } else {
            canBeInlined(xCandidate, yCandidate, xs, next)
          }
        }

      case Nil => Some(0 -> -1)
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
