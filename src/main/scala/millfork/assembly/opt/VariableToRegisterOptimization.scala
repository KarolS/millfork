package millfork.assembly.opt

import millfork.{CompilationFlag, CompilationOptions, NonOverlappingIntervals}
import millfork.assembly.{AddrMode, AssemblyLine, OpcodeClasses}
import millfork.assembly.Opcode._
import millfork.assembly.AddrMode._
import millfork.env._
import millfork.error.ErrorReporting

import scala.collection.mutable.ListBuffer

/**
  * @author Karol Stasiak
  */
object VariableToRegisterOptimization extends AssemblyOptimization {

  object CyclesAndBytes {
    val Zero = CyclesAndBytes(0, 0)
  }
  case class CyclesAndBytes(bytes: Int, cycles: Int) {
    def +(that: CyclesAndBytes) = CyclesAndBytes(this.bytes + that.bytes, this.cycles + that.cycles)
  }

  case class Features(
                     blastProcessing: Boolean,
                     izIsAlwaysZero: Boolean,
                     indexRegisterTransfers: Boolean,
                     identityArray: Constant)

  // If any of these opcodes is present within a method,
  // then it's too hard to assign any variable to a register.
  private val opcodesThatAlwaysPrecludeXAllocation = Set(
    JSR, STX, TXA, INX, DEX, CPX,
    LDX_W, STX_W, CPX_W, DEX_W, INX_W,
    PHX, PLX,
    SBX, SAX, LXA, XAA, AHX, SHX, SHY, LAS, TAS,
    HuSAX, SXY, TXY, TXY,
  )

  private val opcodesThatAlwaysPrecludeYAllocation = Set(
    JSR, STY, TYA, INY, DEY, CPY,
    LDY_W, STY_W, CPY_W, DEY_W, INY_W,
    PHY, PLY,
    AHX, SHX, SHY, LAS, TAS,
    SAY, SXY, TXY, TYX,
  )

  private val opcodesThatAlwaysPrecludeZAllocation = Set(
    JSR, STZ, TZA, INZ, DEZ, CPZ,
    PHZ, PLZ,
  )

  private val opcodesThatAlwaysPrecludeAAllocation = Set(
    JSR, PHA, PLA,
    ADC, SBC, ORA, EOR, AND, BIT,
    ADC_W, SBC_W, ORA_W, EOR_W, AND_W, BIT_W,
    RRA, RLA, ISC, SLO, SRE,
    ALR, ARR, ANC, SBX, LXA, XAA,
    AHX, SHX, SHY, LAS, TAS,
    HuSAX, SAY,
    TCD, TDC, TSC, TCS, XBA,
  )

  // If any of these opcodes is used on a variable
  // then it's too hard to assign that variable to a register.
  // Also, LDY prevents assigning a variable to X and LDX and LAX prevent assigning a variable to Y.
  private val opcodesThatCannotBeUsedWithIndexRegistersAsParameters = Set(
    LDX_W, LDY_W, LDA_W,
    BIT, CPX, CPY, STY,
    BIT_W, CPX_W, CPY_W, STY_W,
    EOR, ORA, AND, ADC, SBC, CMP,
    EOR_W, ORA_W, AND_W, ADC_W, SBC_W, CMP_W,
    ROL, ROR, LSR, ASL, STX,
    ROL_W, ROR_W, LSR_W, ASL_W, STX_W,
    INC_W, DEC_W,
    SAX, SLO, SRE, ISC, DCP, RLA, RRA,
    AHX, SHY, SHX, LAS, TAS,
    TRB, TSB,
    ASR,
  )

  private val opcodesThatCannotBeUsedWithAccumulatorAsParameter = Set(
    BIT, CPX, CPY,
    BIT_W, CPX_W, CPY_W,
    EOR, ORA, AND, ADC, SBC, CMP, STA,
    EOR_W, ORA_W, AND_W, ADC_W, SBC_W, CMP_W, STA_W,
    INC_W, DEC_W,
    SAX, SLO, SRE, ISC, DCP, RLA, RRA,
    AHX, SHY, SHX, LAS, TAS,
    TRB, TSB,
    ASR,
  )

  private val opcodesCommutative = Set(AND, ORA, EOR, ADC)
  private val opcodesIdentityTable = Set(AND, ORA, EOR, CMP, ADC, SBC)

  private val LdxAddrModes = Set(ZeroPage, Absolute, Immediate, AbsoluteY, ZeroPageY)
  private val LdyAddrModes = Set(ZeroPage, Absolute, Immediate, AbsoluteX, ZeroPageX)
  private val LdzAddrModes = Set(Absolute, Immediate, AbsoluteX)
  private val CpxyzAddrModes = Set(Absolute, Immediate, ZeroPage)

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
    val variablesWithAddressesTaken = code.flatMap {
      case AssemblyLine(_, _, HalfWordConstant(MemoryAddressConstant(th), _), _) => Some(th.name)
      case AssemblyLine(_, _, SubbyteConstant(MemoryAddressConstant(th), _), _) => Some(th.name)
      case _ => None
    }.toSet
    val localVariables = f.environment.getAllLocalVariables.filter {
      case MemoryVariable(name, typ, VariableAllocationMethod.Auto | VariableAllocationMethod.Register) =>
        typ.size == 1 && !paramVariables(name) && stillUsedVariables(name) && !variablesWithAddressesTaken(name)
      case _ => false
    }
    val variablesWithRegisterHint = f.environment.getAllLocalVariables.filter {
      case MemoryVariable(name, typ, VariableAllocationMethod.Register) =>
        typ.size == 1 && !paramVariables(name) && stillUsedVariables(name) && !variablesWithAddressesTaken(name)
      case _ => false
    }.map(_.name).toSet

    val variablesWithLifetimes = localVariables.map(v =>
      v.name -> VariableLifetime.apply(v.name, code)
    )

    val costFunction: CyclesAndBytes => Int = if (options.flag(CompilationFlag.OptimizeForSpeed)) _.cycles else _.bytes
    val importances = ReverseFlowAnalyzer.analyze(f, code)
    val blastProcessing = options.flag(CompilationFlag.OptimizeForSonicSpeed)
    val identityArray = f.environment.maybeGet[ThingInMemory]("identity$").map(MemoryAddressConstant).getOrElse(Constant.Zero)
    val izIsAlwaysZero = !options.flag(CompilationFlag.Emit65CE02Opcodes)
    val features = Features(
      blastProcessing = blastProcessing,
      izIsAlwaysZero = izIsAlwaysZero,
      indexRegisterTransfers = options.flag(CompilationFlag.EmitEmulation65816Opcodes),
      identityArray = identityArray
    )

    val xCandidates = variablesWithLifetimes.filter {
      case (vName, range) =>
        importances(range.start).x != Important
    }.flatMap {
      case (vName, range) =>
        canBeInlined(Some(vName), None, None, features, code.zip(importances).slice(range.start, range.end)).map { score =>
          (vName, range, if (variablesWithRegisterHint(vName)) score + CyclesAndBytes(16, 16) else score)
        }
    }

    val yCandidates = variablesWithLifetimes.filter {
      case (vName, range) =>
        importances(range.start).y != Important
    }.flatMap {
      case (vName, range) =>
        canBeInlined(None, Some(vName), None, features, code.zip(importances).slice(range.start, range.end)).map { score =>
          (vName, range, if (variablesWithRegisterHint(vName)) score + CyclesAndBytes(16, 16) else score)
        }
    }

    val zCandidates = if (izIsAlwaysZero) Nil else variablesWithLifetimes.filter {
      case (vName, range) =>
        importances(range.start).iz != Important
    }.flatMap {
      case (vName, range) =>
        canBeInlined(None, None, Some(vName), features, code.zip(importances).slice(range.start, range.end)).map { score =>
          (vName, range, if (variablesWithRegisterHint(vName)) score + CyclesAndBytes(16, 16) else score)
        }
    }

    val aCandidates = variablesWithLifetimes.filter {
      case (vName, range) =>
        importances(range.start).a != Important
    }.flatMap {
      case (vName, range) =>
        canBeInlinedToAccumulator(
          options,
          start = true,
          synced = false,
          vName,
          code.zip(importances).slice(range.start, range.end)).map { score =>
          (vName, range, if (variablesWithRegisterHint(vName)) score + CyclesAndBytes(16, 16) else score)
        }
    }
//    println(s"X: $xCandidates")
//    println(s"Y: $yCandidates")
//    println(s"Z: $zCandidates")
//    println(s"A: $aCandidates")

    val xCandidateSets = NonOverlappingIntervals.apply[(String, Range, CyclesAndBytes)](xCandidates, _._2.start, _._2.end)
    val yCandidateSets = NonOverlappingIntervals.apply[(String, Range, CyclesAndBytes)](yCandidates, _._2.start, _._2.end)
    val zCandidateSets = NonOverlappingIntervals.apply[(String, Range, CyclesAndBytes)](zCandidates, _._2.start, _._2.end)
    val aCandidateSets = NonOverlappingIntervals.apply[(String, Range, CyclesAndBytes)](aCandidates, _._2.start, _._2.end)

    val variants = for {
      vx <- if (options.flag(CompilationFlag.SingleThreaded)) xCandidateSets else xCandidateSets.par
      nx = vx.map(_._1)

      vy <- yCandidateSets
      ny = vy.map(_._1)
      if (nx & ny).isEmpty

      vz <- zCandidateSets
      nz = vz.map(_._1)
      if (nx & nz).isEmpty
      if (ny & nz).isEmpty

      va <- aCandidateSets
      na = va.map(_._1)
      if (nz & na).isEmpty
      if (nx & na).isEmpty
      if (ny & na).isEmpty

      score = vx.toSeq.map(x => costFunction(x._3)).sum +
        vy.toSeq.map(x => costFunction(x._3)).sum +
        va.toSeq.map(x => costFunction(x._3)).sum +
        vz.toSeq.map(x => costFunction(x._3)).sum
    } yield (score, vx, vy, vz, va)

    if (variants.isEmpty) {
      return code
    }

//    variants.foreach(println)

    val (_, bestXs, bestYs, bestZs, bestAs) = variants.maxBy(_._1)

    def reportOptimizedBlock[T](oldCode: List[(AssemblyLine, T)], newCode: List[AssemblyLine]): Unit = {
      oldCode.foreach(l => ErrorReporting.trace(l._1.toString))
      ErrorReporting.trace("     ↓")
      newCode.foreach(l => ErrorReporting.trace(l.toString))
    }

    if (bestXs.nonEmpty || bestYs.nonEmpty || bestZs.nonEmpty || bestAs.nonEmpty) {
      bestXs.foreach(v => f.environment.removeVariable(v._1))
      bestYs.foreach(v => f.environment.removeVariable(v._1))
      bestZs.foreach(v => f.environment.removeVariable(v._1))
      bestAs.foreach(v => f.environment.removeVariable(v._1))
      val output = ListBuffer[AssemblyLine]()
      var i = 0
      while (i < code.length) {
        var done = false
        bestXs.find(_._2.start == i).foreach {
          case (v, range, _) =>
            ErrorReporting.debug(s"Inlining $v to register X")
            val oldCode = code.zip(importances).slice(range.start, range.end)
            val newCode = inlineVars(Some(v), None, None, None, features, oldCode)
            reportOptimizedBlock(oldCode, newCode)
            output ++= newCode
            i = range.end
            done = true
        }
        if (!done) {
          bestYs.find(_._2.start == i).foreach {
            case (v, range, _) =>
              ErrorReporting.debug(s"Inlining $v to register Y")
              val oldCode = code.zip(importances).slice(range.start, range.end)
              val newCode = inlineVars(None, Some(v), None, None, features, oldCode)
              reportOptimizedBlock(oldCode, newCode)
              output ++= newCode
              i = range.end
              done = true
          }
        }
        if (!done) {
          bestZs.find(_._2.start == i).foreach {
            case (v, range, _) =>
              ErrorReporting.debug(s"Inlining $v to register Z")
              val oldCode = code.zip(importances).slice(range.start, range.end)
              val newCode = inlineVars(None, None, Some(v), None, features, oldCode)
              reportOptimizedBlock(oldCode, newCode)
              output ++= newCode
              i = range.end
              done = true
          }
        }
        if (!done) {
          bestAs.find(_._2.start == i).foreach {
            case (v, range, _) =>
              ErrorReporting.debug(s"Inlining $v to register A")
              val oldCode = code.zip(importances).slice(range.start, range.end)
              val newCode = inlineVars(None, None, None, Some(v), features, oldCode)
              reportOptimizedBlock(oldCode, newCode)
              output ++= newCode
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

  // TODO: STA has different flag behaviour than TAX, keep it in mind!
  def canBeInlined(xCandidate: Option[String], yCandidate: Option[String], zCandidate: Option[String], features: Features, lines: List[(AssemblyLine, CpuImportance)]): Option[CyclesAndBytes] = {
    val vx = xCandidate.getOrElse("-")
    val vy = yCandidate.getOrElse("-")
    val vz = zCandidate.getOrElse("-")
    lines match {
      case (AssemblyLine(_, Immediate, SubbyteConstant(MemoryAddressConstant(th), _), _), _) :: xs
        if th.name == vx || th.name == vy || th.name == vz =>
        // if an address of a variable is used, then that variable cannot be assigned to a register
        None
      case (AssemblyLine(_, Immediate, HalfWordConstant(MemoryAddressConstant(th), _), _), _) :: xs
        if th.name == vx || th.name == vy || th.name == vz =>
        // if an address of a variable is used, then that variable cannot be assigned to a register
        None

      case (AssemblyLine(_, AbsoluteX | AbsoluteY | LongAbsoluteX |
                            ZeroPageX | ZeroPageY |
                            IndexedY | IndexedX | IndexedZ |
                            LongIndexedY | LongIndexedZ |
                            Indirect | LongIndirect |
                            AbsoluteIndexedX, MemoryAddressConstant(th), _), _) :: xs =>
        // if a variable is used as an array or a pointer, then it cannot be assigned to a register
        if (th.name == vx || th.name == vy || th.name == vz) {
          None
        } else {
          canBeInlined(xCandidate, yCandidate, zCandidate, features, xs)
        }

      case (AssemblyLine(SEP | REP, Immediate, NumericConstant(nn, _), _), _) :: xs =>
        if ((nn & 0x10) == 0) canBeInlined(xCandidate, yCandidate, zCandidate, features, xs)
        else None

      case (AssemblyLine(SEP | REP, _, _, _), _) :: xs => None

      case (AssemblyLine(STY | LDY, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs if th.name == vx =>
        if (features.indexRegisterTransfers) canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 2))
        else None

      case (AssemblyLine(STX | LDX, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs if th.name == vy =>
        if (features.indexRegisterTransfers) canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 2))
        else None

      case (AssemblyLine(op, Absolute | ZeroPage, MemoryAddressConstant(th), elidable),_) :: xs
        if opcodesIdentityTable(op) && features.blastProcessing =>
        if (th.name == vx || th.name == vy) {
          if (elidable) canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 0, cycles = -1))
          else None
        } else {
          if (th.name == vz) None
          else canBeInlined(xCandidate, yCandidate, zCandidate, features, xs)
        }

      case (AssemblyLine(opcode, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if th.name == vx && (opcode == LDY || opcodesThatCannotBeUsedWithIndexRegistersAsParameters(opcode)) =>
        // if a variable is used by some opcodes, then it cannot be assigned to a register
        None

      case (AssemblyLine(opcode, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if th.name == vy && (opcode == LDX || opcode == LAX || opcodesThatCannotBeUsedWithIndexRegistersAsParameters(opcode)) =>
        // if a variable is used by some opcodes, then it cannot be assigned to a register
        None

      case (AssemblyLine(opcode, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if th.name == vz && (opcode == LDZ || opcodesThatCannotBeUsedWithIndexRegistersAsParameters(opcode)) =>
        // if a variable is used by some opcodes, then it cannot be assigned to a register
        None

      case (AssemblyLine(LDX, Absolute | ZeroPage, MemoryAddressConstant(th), elidable), imp) :: xs
        if xCandidate.isDefined =>
        // if a register is populated with a different variable, then this variable cannot be assigned to that register
        // removing LDX saves 3 cycles
        if (elidable && th.name == vx) {
          if (imp.z == Unimportant && imp.n == Unimportant) {
            canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 2))
          } else {
            canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 1, cycles = 2))
          }
        } else {
          None
        }

      case (AssemblyLine(LAX, Absolute | ZeroPage, MemoryAddressConstant(th), elidable), _) :: xs
        if xCandidate.isDefined =>
        // LAX = LDX-LDA, and since LDX simplifies to nothing and LDA simplifies to TXA,
        // LAX simplifies to TXA, saving two bytes
        if (elidable && th.name == vx) {
          canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 2))
        } else {
          None
        }

      case (AssemblyLine(LDY, Absolute | ZeroPage, MemoryAddressConstant(th), elidable), imp) :: xs if yCandidate.isDefined =>
        // if a register is populated with a different variable, then this variable cannot be assigned to that register
        // removing LDX saves 3 bytes
        // sometimes that LDX has to be converted into CPX#0
        if (elidable && th.name == vy) {
          if (imp.z == Unimportant && imp.n == Unimportant) {
            canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 3, cycles = 4))
          } else {
            canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 1, cycles = 2))
          }
        } else {
          None
        }

      case (AssemblyLine(LDZ, Absolute | ZeroPage, MemoryAddressConstant(th), elidable), imp) :: xs if zCandidate.isDefined =>
        if (elidable && th.name == vz) {
          if (imp.z == Unimportant && imp.n == Unimportant) {
            canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 3, cycles = 4))
          } else {
            canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 1, cycles = 2))
          }
        } else {
          None
        }

      case (AssemblyLine(LDX, _, _, _), _) :: xs if xCandidate.isDefined =>
        // if a register is populated with something else than a variable, then no variable cannot be assigned to that register
        None

      case (AssemblyLine(LDY, _, _, _), _) :: xs if yCandidate.isDefined =>
        // if a register is populated with something else than a variable, then no variable cannot be assigned to that register
        None

      case (AssemblyLine(LDZ, _, _, _), _) :: xs if zCandidate.isDefined =>
        // if a register is populated with something else than a variable, then no variable cannot be assigned to that register
        None

      case (AssemblyLine(LDA, _, _, elidable),_) :: (AssemblyLine(op, Absolute | ZeroPage, MemoryAddressConstant(th), elidable2),_) :: xs
        if opcodesCommutative(op) =>
        // LDAw/ANDx -> TXA/ANDw
        if (th.name == vx || th.name == vy) {
          if (elidable && elidable2) canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 2))
          else None
        } else {
          if (th.name == vz) None
          else canBeInlined(xCandidate, yCandidate, zCandidate, features, xs)
        }

      case (AssemblyLine(LDA, _, _, elidable),_) :: (AssemblyLine(CLC, _, _, _),_) :: (AssemblyLine(op, Absolute | ZeroPage, MemoryAddressConstant(th), elidable2),_) :: xs
        if opcodesCommutative(op) =>
        if (th.name == vx || th.name == vy) {
          if (elidable && elidable2) canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 2))
          else None
        } else {
          if (th.name == vz) None
          else canBeInlined(xCandidate, yCandidate, zCandidate, features, xs)
        }

      case (AssemblyLine(LDA, Absolute | ZeroPage, MemoryAddressConstant(th), elidable), _) :: (AssemblyLine(TAX, _, _, elidable2), _) :: xs
        if xCandidate.isDefined =>
        // a variable cannot be inlined if there is TAX not after LDA of that variable
        // but LDA-TAX can be simplified to TXA
        if (elidable && elidable2 && th.name == vx) {
          canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 3, cycles = 4))
        } else {
          None
        }

      case (AssemblyLine(LDA, Absolute | ZeroPage, MemoryAddressConstant(th), elidable), _) :: (AssemblyLine(TAY, _, _, elidable2), _) :: xs
        if yCandidate.isDefined =>
        // a variable cannot be inlined if there is TAY not after LDA of that variable
        // but LDA-TAY can be simplified to TYA
        if (elidable && elidable2 && th.name == vy) {
          canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 3, cycles = 4))
        } else {
          None
        }

      case (AssemblyLine(LDA, Absolute | ZeroPage, MemoryAddressConstant(th), elidable), _) :: (AssemblyLine(TAZ, _, _, elidable2), _) :: xs
        if zCandidate.isDefined =>
        // a variable cannot be inlined if there is TAZ not after LDA of that variable
        // but LDA-TAZ can be simplified to TZA
        if (elidable && elidable2 && th.name == vy) {
          canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 3, cycles = 4))
        } else {
          None
        }

      case (AssemblyLine(LDA | STA, Absolute | ZeroPage, MemoryAddressConstant(th), elidable), _) :: xs =>
        // changing LDA->TXA, STA->TAX, INC->INX, DEC->DEX saves 2 bytes
        if (th.name == vy || th.name == vx || th.name == vz) {
          if (elidable) {
            canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 2))
          } else {
            None
          }
        } else {
          canBeInlined(xCandidate, yCandidate, zCandidate, features, xs)
        }

      case (AssemblyLine(INC | DEC, Absolute | ZeroPage, MemoryAddressConstant(th), elidable), _) :: xs =>
        // changing LDA->TXA, STA->TAX, INC->INX, DEC->DEX saves 2 bytes
        if (th.name == vy || th.name == vx || th.name == vz) {
          if (elidable) {
            canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 4))
          } else {
            None
          }
        } else {
          canBeInlined(xCandidate, yCandidate, zCandidate, features, xs)
        }

      case (AssemblyLine(STZ, Absolute | ZeroPage, MemoryAddressConstant(th), elidable), _) :: xs =>
        // changing STZ->LDX saves 1 byte
        if (th.name == vy || th.name == vx) {
          if (elidable && features.izIsAlwaysZero) {
            canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 1, cycles = 2))
          } else {
            None
          }
        } else {
          canBeInlined(xCandidate, yCandidate, zCandidate, features, xs)
        }

      case (AssemblyLine(TAX, _, _, _), _) :: xs if xCandidate.isDefined =>
        // a variable cannot be inlined if there is TAX not after LDA of that variable
        None

      case (AssemblyLine(TAY, _, _, _), _) :: xs if yCandidate.isDefined =>
        // a variable cannot be inlined if there is TAY not after LDA of that variable
        None

      case (AssemblyLine(TAZ, _, _, _), _) :: xs if zCandidate.isDefined =>
        // a variable cannot be inlined if there is TAZ not after LDA of that variable
        None

      case (AssemblyLine(LABEL, _, _, _), _) :: xs =>
        // labels always end the initial section
        canBeInlined(xCandidate, yCandidate, zCandidate, features, xs)

      case (x, _) :: xs =>
        if (xCandidate.isDefined && opcodesThatAlwaysPrecludeXAllocation(x.opcode)) {
          None
        } else if (yCandidate.isDefined && opcodesThatAlwaysPrecludeYAllocation(x.opcode)) {
          None
        } else if (zCandidate.isDefined && opcodesThatAlwaysPrecludeZAllocation(x.opcode)) {
          None
        } else {
          canBeInlined(xCandidate, yCandidate, zCandidate, features, xs)
        }

      case Nil => Some(CyclesAndBytes.Zero)
    }
  }

  def canBeInlinedToAccumulator(options: CompilationOptions, start: Boolean, synced: Boolean, candidate: String, lines: List[(AssemblyLine, CpuImportance)]): Option[CyclesAndBytes] = {
    val cmos = options.flags(CompilationFlag.EmitCmosOpcodes)
    lines match {

      case (AssemblyLine(STA, Absolute | ZeroPage, MemoryAddressConstant(th), true),_) :: xs
        if th.name == candidate && start || synced =>
        canBeInlinedToAccumulator(options, start = false, synced = true, candidate, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 4))

      case (AssemblyLine(op, _, _, _),_) :: xs if opcodesThatAlwaysPrecludeAAllocation(op) =>
        None

      case (AssemblyLine(op, Absolute | ZeroPage, MemoryAddressConstant(th), _),_) :: xs
        if th.name == candidate && opcodesThatCannotBeUsedWithAccumulatorAsParameter(op) =>
        // if a variable is used by some opcodes, then it cannot be assigned to a register
        None

      case (AssemblyLine(_, Immediate, SubbyteConstant(MemoryAddressConstant(th), _), _),_) :: xs
        if th.name == candidate =>
        // if an address of a variable is used, then that variable cannot be assigned to a register
        None

      case (AssemblyLine(_, Immediate, HalfWordConstant(MemoryAddressConstant(th), _), _),_) :: xs
        if th.name == candidate =>
        // if an address of a variable is used, then that variable cannot be assigned to a register
        None

      case (AssemblyLine(_, AbsoluteX | AbsoluteY | ZeroPageX | ZeroPageY | IndexedY | IndexedX | IndexedZ | Indirect | AbsoluteIndexedX, MemoryAddressConstant(th), _),_) :: xs
        if th.name == candidate =>
        // if a variable is used as an array or a pointer, then it cannot be assigned to a register
        None

      case (AssemblyLine(SEP | REP, Immediate, NumericConstant(nn, _), _), _) :: xs =>
        if ((nn & 0x20) == 0) canBeInlinedToAccumulator(options, start = false, synced = synced, candidate, xs)
        else None

      case (AssemblyLine(SEP | REP, _, _, _), _) :: xs => None

      case (AssemblyLine(STA, _, MemoryAddressConstant(th), elidable) ,_):: xs if th.name == candidate =>
        if (synced && elidable) {
          canBeInlinedToAccumulator(options, start = false, synced = true, candidate, xs).map(_ + CyclesAndBytes(bytes = 3, cycles = 4))
        } else {
          None
        }

      case (AssemblyLine(DCP, Absolute | ZeroPage, MemoryAddressConstant(th), _) ,_):: xs if th.name == candidate =>
        if (synced) {
          canBeInlinedToAccumulator(options, start = false, synced = true, candidate, xs)
        } else {
          None
        }

      case (AssemblyLine(STA | SAX, _, MemoryAddressConstant(th), elidable) ,_):: xs if th.name != candidate =>
        if (synced) {
          canBeInlinedToAccumulator(options, start = false, synced = true, candidate, xs)
        } else {
          None
        }

      case (AssemblyLine(STA | SAX, _, NumericConstant(_, _), _) ,_):: xs =>
        if (synced) {
          canBeInlinedToAccumulator(options, start = false, synced = true, candidate, xs)
        } else {
          None
        }

      case (AssemblyLine(SAX, _, MemoryAddressConstant(th), _) ,_):: xs if th.name == candidate =>
        // if XAA had stable magic $FF, then SAXv/LDAv would correspond to XAA#ff
        // but there's no point in even thinking about that
        None

      case (AssemblyLine(TAX | TAY, _, _, _),_) :: xs =>
        if (synced) {
          canBeInlinedToAccumulator(options, start = false, synced = true, candidate, xs)
        } else {
          None
        }

      case (AssemblyLine(LDA, _, _, elidable),_) :: (AssemblyLine(op, Absolute | ZeroPage, MemoryAddressConstant(th), elidable2),_) :: xs
        if opcodesCommutative(op) =>
        if (th.name == candidate) {
          if (elidable && elidable2) canBeInlinedToAccumulator(options, start = false, synced = true, candidate, xs).map(_ + CyclesAndBytes(bytes = 3, cycles = 4))
          else None
        } else canBeInlinedToAccumulator(options, start = false, synced = synced, candidate, xs)

      case (AssemblyLine(LDA, _, _, elidable),_) :: (AssemblyLine(CLC, _, _, _),_) :: (AssemblyLine(op, Absolute | ZeroPage, MemoryAddressConstant(th), elidable2),_) :: xs
        if opcodesCommutative(op) =>
        if (th.name == candidate) {
          if (elidable && elidable2) canBeInlinedToAccumulator(options, start = false, synced = true, candidate, xs).map(_ + CyclesAndBytes(bytes = 3, cycles = 4))
          else None
        } else canBeInlinedToAccumulator(options, start = false, synced = synced, candidate, xs)

      case (AssemblyLine(LDA, Absolute | ZeroPage, MemoryAddressConstant(th), true), imp) :: xs
        if th.name == candidate =>
        // removing LDA saves 3 bytes
        if (imp.z == Unimportant && imp.n == Unimportant) {
          canBeInlinedToAccumulator(options, start = false, synced = true, candidate, xs).map(_ + CyclesAndBytes(bytes = 3, cycles = 4))
        } else {
          canBeInlinedToAccumulator(options, start = false, synced = true, candidate, xs).map(_ + CyclesAndBytes(bytes = 1, cycles = 2))
        }

      case (AssemblyLine(LDX | LDY | LAX, Absolute | ZeroPage, MemoryAddressConstant(th), elidable),_) :: xs
        if th.name == candidate =>
        // converting a load into a transfer saves 2 bytes
        if (elidable) {
          canBeInlinedToAccumulator(options, start = false, synced = true, candidate, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 2))
        } else {
          None
        }

      case (AssemblyLine(LDA | LAX, _, _, _),_) :: xs =>
        // if a register is populated with something else than a variable, then no variable cannot be assigned to that register
        None

      case (AssemblyLine(ASL | LSR | ROR | ROL, Absolute | ZeroPage, MemoryAddressConstant(th), elidable),_) :: xs
        if th.name == candidate =>
        if (elidable) {
          canBeInlinedToAccumulator(options, start = false, synced = false, candidate, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 4))
        } else {
          None
        }

      case (AssemblyLine(INC | DEC, Absolute | ZeroPage, MemoryAddressConstant(th), elidable),_) :: xs
        if th.name == candidate =>
        if (cmos && elidable) {
          canBeInlinedToAccumulator(options, start = false, synced = false, candidate, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 4))
        } else {
          None
        }

      case (AssemblyLine(TXA | TYA, _, _, elidable), imp) :: xs =>
        if (imp.a == Unimportant && imp.c == Unimportant && imp.v == Unimportant && elidable) {
          // TYA/TXA has to be converted to CPY#0/CPX#0
          canBeInlinedToAccumulator(options, start = false, synced = false, candidate, xs).map(_ + CyclesAndBytes(bytes = -1, cycles = 0))
        } else {
          None
        }

      case (x, _) :: xs => canBeInlinedToAccumulator(options, start = false, synced = synced && OpcodeClasses.AllLinear(x.opcode), candidate, xs)

      case Nil => Some(CyclesAndBytes.Zero)
    }
  }

  def inlineVars(xCandidate: Option[String], yCandidate: Option[String], zCandidate: Option[String], aCandidate: Option[String], features: Features, lines: List[(AssemblyLine, CpuImportance)]): List[AssemblyLine] = {
    val vx = xCandidate.getOrElse("-")
    val vy = yCandidate.getOrElse("-")
    val vz = zCandidate.getOrElse("-")
    val va = aCandidate.getOrElse("-")
    lines match {
      case (AssemblyLine(INC, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if th.name == vx =>
        AssemblyLine.implied(INX) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(INC, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if th.name == vy =>
        AssemblyLine.implied(INY) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(INC, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if th.name == vz =>
        AssemblyLine.implied(INZ) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(DEC, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if th.name == vx =>
        AssemblyLine.implied(DEX) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(DEC, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if th.name == vy =>
        AssemblyLine.implied(DEY) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(DEC, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if th.name == vz =>
        AssemblyLine.implied(DEZ) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(opcode@(DEC | INC | ROL | ROR | ASL | LSR), Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if th.name == va =>
        AssemblyLine.implied(opcode) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(LDX, Absolute | ZeroPage, MemoryAddressConstant(th), _), imp) :: xs
        if th.name == vx =>
        if (imp.z == Unimportant && imp.n == Unimportant) {
          inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)
        } else {
          AssemblyLine.immediate(CPX, 0) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)
        }

      case (AssemblyLine(LAX, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if th.name == vx =>
        AssemblyLine.implied(TXA) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (l@AssemblyLine(op, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if opcodesIdentityTable(op) && th.name == vx =>
        l.copy(addrMode = AbsoluteX, parameter = features.identityArray) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (l@AssemblyLine(op, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if opcodesIdentityTable(op) && th.name == vy =>
        l.copy(addrMode = AbsoluteY, parameter = features.identityArray) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (l@AssemblyLine(LDA, _, _, _), _) ::  (AssemblyLine(op, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if opcodesCommutative(op) && th.name == va =>
        l.copy(opcode = op) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (l@AssemblyLine(LDA, _, _, _), _) :: (clc@AssemblyLine(CLC, _, _, _), _) :: (AssemblyLine(op, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if opcodesCommutative(op) && th.name == va =>
        l.copy(opcode = op) :: clc :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (l@AssemblyLine(LDA, _, _, _), _) ::  (AssemblyLine(op, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if opcodesCommutative(op) && th.name == vx =>
        AssemblyLine.implied(TXA) :: l.copy(opcode = op) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (l@AssemblyLine(LDA, _, _, _), _) :: (clc@AssemblyLine(CLC, _, _, _), _) :: (AssemblyLine(op, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if opcodesCommutative(op) && th.name == vx =>
        AssemblyLine.implied(TXA) :: l.copy(opcode = op) :: clc :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (l@AssemblyLine(LDA, _, _, _), _) ::  (AssemblyLine(op, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if opcodesCommutative(op) && th.name == vy =>
        AssemblyLine.implied(TYA) :: l.copy(opcode = op) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (l@AssemblyLine(LDA, _, _, _), _) :: (clc@AssemblyLine(CLC, _, _, _), _) :: (AssemblyLine(op, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if opcodesCommutative(op) && th.name == vy =>
        AssemblyLine.implied(TYA) :: l.copy(opcode = op) :: clc :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(LDA | STA, Absolute | ZeroPage, MemoryAddressConstant(th), _), imp) :: xs
        if th.name == va =>
        if (imp.z == Unimportant && imp.n == Unimportant) {
          inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)
        } else {
          AssemblyLine.immediate(CMP, 0) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)
        }

      case (AssemblyLine(LAX, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if th.name == va =>
        AssemblyLine.implied(TAX) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(LDY, Absolute | ZeroPage, MemoryAddressConstant(th), _), imp) :: xs
        if th.name == vy =>
        if (imp.z == Unimportant && imp.n == Unimportant) {
          inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)
        } else {
          AssemblyLine.immediate(CPY, 0) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)
        }

      case (AssemblyLine(LDZ, Absolute | ZeroPage, MemoryAddressConstant(th), _), imp) :: xs
        if th.name == vz =>
        if (imp.z == Unimportant && imp.n == Unimportant) {
          inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)
        } else {
          AssemblyLine.immediate(CPZ, 0) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)
        }

      case (AssemblyLine(LDA, Absolute | ZeroPage, MemoryAddressConstant(th), true), _) :: (AssemblyLine(TAX, _, _, true), _) :: xs
        if th.name == vx =>
        // these TXA's may get optimized away by a different optimization
        AssemblyLine.implied(TXA) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(LDA, Absolute | ZeroPage, MemoryAddressConstant(th), true), _) :: (AssemblyLine(TAY, _, _, true), _) :: xs
        if th.name == vy =>
        // these TYA's may get optimized away by a different optimization
        AssemblyLine.implied(TYA) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(LDA, Absolute | ZeroPage, MemoryAddressConstant(th), true), _) :: (AssemblyLine(TAZ, _, _, true), _) :: xs
        if th.name == vz =>
        // these TZA's may get optimized away by a different optimization
        AssemblyLine.implied(TZA) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(LDX, Absolute | ZeroPage, MemoryAddressConstant(th), true), _) :: (AssemblyLine(TXA, _, _, true), _) :: xs
        if th.name == va =>
        // these TAX's may get optimized away by a different optimization
        AssemblyLine.implied(TAX) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(LDY, Absolute | ZeroPage, MemoryAddressConstant(th), true), _) :: (AssemblyLine(TYA, _, _, true), _) :: xs
        if th.name == va =>
        // these TAY's may get optimized away by a different optimization
        AssemblyLine.implied(TAY) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(LDA, am, param, true), _) :: (AssemblyLine(STA, Absolute | ZeroPage, MemoryAddressConstant(th), true), _) :: xs
        if th.name == vx && LdxAddrModes(am) =>
        // these TXA's may get optimized away by a different optimization
        AssemblyLine(LDX, am, param) :: AssemblyLine.implied(TXA) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(LDA, am, param, true), _) :: (AssemblyLine(STA, Absolute | ZeroPage, MemoryAddressConstant(th), true), _) :: xs
        if th.name == vy && LdyAddrModes(am) =>
        // these TYA's may get optimized away by a different optimization
        AssemblyLine(LDY, am, param) :: AssemblyLine.implied(TYA) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(LDA, am, param, true), _) :: (AssemblyLine(STA, Absolute | ZeroPage, MemoryAddressConstant(th), true), _) :: xs
        if th.name == vz && LdzAddrModes(am) =>
        // these TZA's may get optimized away by a different optimization
        AssemblyLine(LDZ, am, param) :: AssemblyLine.implied(TZA) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(LDA, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: (AssemblyLine(CMP, am, param, true), _) :: xs
        if th.name == vx && CpxyzAddrModes(am) =>
        // ditto
        AssemblyLine.implied(TXA) :: AssemblyLine(CPX, am, param) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(LDA, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: (AssemblyLine(CMP, am, param, true), _) :: xs
        if th.name == vy && CpxyzAddrModes(am) =>
        // ditto
        AssemblyLine.implied(TYA) :: AssemblyLine(CPY, am, param) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(LDA, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: (AssemblyLine(CMP, am, param, true), _) :: xs
        if th.name == vy && CpxyzAddrModes(am) =>
        // ditto
        AssemblyLine.implied(TZA) :: AssemblyLine(CPZ, am, param) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(LDA, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if th.name == vx =>
        AssemblyLine.implied(TXA) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(LDA, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if th.name == vy =>
        AssemblyLine.implied(TYA) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(LDY, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if th.name == vx =>
        AssemblyLine.implied(TXY) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(LDX, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if th.name == vy =>
        AssemblyLine.implied(TYX) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(LDA, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if th.name == vz =>
        AssemblyLine.implied(TZA) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(LDX, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if th.name == va =>
        AssemblyLine.implied(TAX) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(LDY, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if th.name == va =>
        AssemblyLine.implied(TAY) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(STA, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if th.name == vx =>
        AssemblyLine.implied(TAX) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(STA, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if th.name == vy =>
        AssemblyLine.implied(TAY) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(STA, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if th.name == vz =>
        AssemblyLine.implied(TAZ) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(STX, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if th.name == va =>
        AssemblyLine.implied(TXA) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(STY, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if th.name == va =>
        AssemblyLine.implied(TYA) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(STX, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if th.name == vy =>
        AssemblyLine.implied(TXY) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(STY, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if th.name == vx =>
        AssemblyLine.implied(TYX) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(STZ, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if th.name == vx =>
        if (features.izIsAlwaysZero) AssemblyLine.immediate(LDX, 0) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)
        else  ErrorReporting.fatal("Unexpected STZ")

      case (AssemblyLine(STZ, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if th.name == vy =>
        if (features.izIsAlwaysZero) AssemblyLine.immediate(LDY, 0) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)
        else  ErrorReporting.fatal("Unexpected STZ")

      case (AssemblyLine(STZ, Absolute | ZeroPage, MemoryAddressConstant(th), _), _) :: xs
        if th.name == va =>
        if (features.izIsAlwaysZero) AssemblyLine.immediate(LDA, 0) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)
        else AssemblyLine.implied(TZA) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(TAX, _, _, _), _) :: xs if xCandidate.isDefined =>
        ErrorReporting.fatal("Unexpected TAX")

      case (AssemblyLine(TAY, _, _, _), _) :: xs if yCandidate.isDefined =>
        ErrorReporting.fatal("Unexpected TAY")

      case (AssemblyLine(TAZ, _, _, _), _) :: xs if zCandidate.isDefined =>
        ErrorReporting.fatal("Unexpected TAZ")

      case (AssemblyLine(TXA, _, _, _), _) :: xs if aCandidate.isDefined =>
        AssemblyLine.immediate(CPX, 0) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(TYA, _, _, _), _) :: xs if aCandidate.isDefined =>
        AssemblyLine.immediate(CPY, 0) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (AssemblyLine(TZA, _, _, _), _) :: xs if aCandidate.isDefined =>
        AssemblyLine.immediate(CPZ, 0) :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case (x, _) :: xs => x :: inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)

      case Nil => Nil
    }
  }

  def doesntUseXOrY(am: AddrMode.Value): Boolean = am match {
    case Immediate | WordImmediate | ZeroPage | Absolute | LongAbsolute | Relative | LongRelative | Indirect | LongIndirect | Stack | IndexedZ => true
    case _ => false
  }
}
