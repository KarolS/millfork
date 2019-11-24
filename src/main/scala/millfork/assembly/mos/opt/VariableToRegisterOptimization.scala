package millfork.assembly.mos.opt

import millfork.{CompilationFlag, NonOverlappingIntervals}
import millfork.assembly.{AssemblyOptimization, Elidability, OptimizationContext}
import millfork.assembly.mos._
import millfork.assembly.mos.Opcode._
import AddrMode._
import millfork.env._
import millfork.error.Logger
import millfork.node.MosNiceFunctionProperty

import scala.collection.mutable.ListBuffer
import scala.util.control.TailCalls.{TailRec, done, tailcall}

/**
  * @author Karol Stasiak
  */
object VariableToRegisterOptimization extends AssemblyOptimization[AssemblyLine] {

  override def requiredFlags: Set[CompilationFlag.Value] = Set(CompilationFlag.RegisterVariables)

  object CyclesAndBytes {
    val Zero = CyclesAndBytes(0, 0)
  }
  case class CyclesAndBytes(bytes: Int, cycles: Int) {
    def +(that: CyclesAndBytes) = CyclesAndBytes(this.bytes + that.bytes, this.cycles + that.cycles)
  }

  case class FeaturesForIndexRegisters(
                     blastProcessing: Boolean,
                     izIsAlwaysZero: Boolean,
                     indexRegisterTransfers: Boolean,
                     functionsSafeForX: Set[String],
                     functionsSafeForY: Set[String],
                     functionsSafeForZ: Set[String],
                     identityArray: Constant,
                     log: Logger)

  case class FeaturesForAccumulator(
                     cmos: Boolean,
                     safeFunctions: Set[String],
                     labelsUsedOnce: Set[String],
                     labelsSyncedAt: Set[String],
                     log: Logger) {
    override def toString: String = s"{cmos=$cmos, safeFunctions=[${safeFunctions.mkString(",")}], labelsSyncedAt=[${safeFunctions.mkString(",")}]}"

    def addSafeLabel(label: String): FeaturesForAccumulator = copy(labelsSyncedAt = this.labelsSyncedAt + label)
  }

  // If any of these opcodes is present within a method,
  // then it's too hard to assign any variable to a register.
  private val opcodesThatAlwaysPrecludeXAllocation = Set(
    JSR, STX, TXA, INX, DEX, CPX,
    LDX_W, STX_W, CPX_W, DEX_W, INX_W,
    PHX, PLX, PHX_W, PLX_W,
    SBX, SAX, LXA, XAA, AHX, SHX, SHY, LAS, TAS,
    HuSAX, SXY, TXY, TXY,
    BYTE,
  )

  private val opcodesThatAlwaysPrecludeYAllocation = Set(
    JSR, STY, TYA, INY, DEY, CPY,
    LDY_W, STY_W, CPY_W, DEY_W, INY_W,
    PHY, PLY, PHY_W, PLY_W,
    AHX, SHX, SHY, LAS, TAS,
    SAY, SXY, TXY, TYX,
    BYTE,
  )

  private val opcodesThatAlwaysPrecludeZAllocation = Set(
    JSR, STZ, TZA, INZ, DEZ, CPZ,
    PHZ, PLZ,
    BYTE,
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
    BYTE,
  )

  private val opcodesThatNeverPrecludeAAllocation = Set(
    INX, INY, DEX, DEY,
    CLC, SEC, CLV, SED, CLD, CLI, SEI, NOP, BRK,
    INX_W, DEX_W, INY_W, DEY_W,
    PHX, PHY, PHZ, PHX_W, PHY_W,
    PLX, PLY, PLZ, PLX_W, PLY_W,
    TXY, TYX, TXS, TSX,
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


  override def optimize(f: NormalFunction, code: List[AssemblyLine], optimizationContext: OptimizationContext): List[AssemblyLine] = {
    val options = optimizationContext.options
    val log = options.log
    val paramVariables = f.params match {
      case NormalParamSignature(List(MemoryVariable(_, typ, _))) if typ.size == 1 =>
        Set[String]()
      case NormalParamSignature(ps) =>
        ps.map(_.name).toSet
      case _ =>
        // assembly functions do not get this optimization
        return code
    }
    val stillUsedVariables = code.flatMap {
      case AssemblyLine0(_, _, MemoryAddressConstant(th)) => Some(th.name)
      case _ => None
    }.toSet
    val variablesWithAddressesTaken = code.flatMap {
      case AssemblyLine0(_, _, SubbyteConstant(MemoryAddressConstant(th), _)) => Some(th.name)
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
    ).toMap

    val removeVariablesForReal = !options.flag(CompilationFlag.InternalCurrentlyOptimizingForMeasurement)
    val costFunction: CyclesAndBytes => Int = if (options.flag(CompilationFlag.OptimizeForSpeed)) _.cycles else _.bytes
    val importances = ReverseFlowAnalyzer.analyze(f, code, optimizationContext)
    val blastProcessing = options.flag(CompilationFlag.OptimizeForSonicSpeed)
    val identityArray = f.environment.maybeGet[ThingInMemory]("identity$").map(MemoryAddressConstant).getOrElse(Constant.Zero)
    val izIsAlwaysZero = !options.flag(CompilationFlag.Emit65CE02Opcodes)
    val featuresForIndices = FeaturesForIndexRegisters(
      blastProcessing = blastProcessing,
      izIsAlwaysZero = izIsAlwaysZero,
      indexRegisterTransfers = options.flag(CompilationFlag.EmitEmulation65816Opcodes),
      functionsSafeForX = optimizationContext.niceFunctionProperties.filter(x => x._1 == MosNiceFunctionProperty.DoesntChangeX).map(_._2),
      functionsSafeForY = optimizationContext.niceFunctionProperties.filter(x => x._1 == MosNiceFunctionProperty.DoesntChangeY).map(_._2),
      functionsSafeForZ = optimizationContext.niceFunctionProperties.filter(x => x._1 == MosNiceFunctionProperty.DoesntChangeIZ).map(_._2),
      identityArray = identityArray,
      log = log
    )

    val labelsUsedOnce: Set[String] = code.flatMap {
      case AssemblyLine0(op, _, MemoryAddressConstant(Label(l))) if op != Opcode.LABEL => Some(l)
      case _ => None
    }.groupBy(identity).filter(_._2.size == 1).keySet

    val featuresForAcc = FeaturesForAccumulator(
      cmos = options.flag(CompilationFlag.EmitCmosOpcodes),
      safeFunctions = optimizationContext.niceFunctionProperties.filter(x => x._1 == MosNiceFunctionProperty.DoesntChangeA).map(_._2),
      labelsUsedOnce = labelsUsedOnce,
      labelsSyncedAt = Set(),
      log = log
    )

    val xCandidates = variablesWithLifetimes.filter {
      case (vName, range) =>
        importances(range.start).x != Important
    }.flatMap {
      case (vName, range) =>
        canBeInlined(Some(vName), None, None, featuresForIndices, code.zip(importances).slice(range.start, range.end)).map { score =>
          (vName, range, if (variablesWithRegisterHint(vName)) score + CyclesAndBytes(16, 16) else score)
        }
    }

    val yCandidates = variablesWithLifetimes.filter {
      case (vName, range) =>
        importances(range.start).y != Important
    }.flatMap {
      case (vName, range) =>
        canBeInlined(None, Some(vName), None, featuresForIndices, code.zip(importances).slice(range.start, range.end)).map { score =>
          (vName, range, if (variablesWithRegisterHint(vName)) score + CyclesAndBytes(16, 16) else score)
        }
    }

    val zCandidates = if (izIsAlwaysZero) Nil else variablesWithLifetimes.filter {
      case (vName, range) =>
        importances(range.start).iz != Important
    }.flatMap {
      case (vName, range) =>
        canBeInlined(None, None, Some(vName), featuresForIndices, code.zip(importances).slice(range.start, range.end)).map { score =>
          (vName, range, if (variablesWithRegisterHint(vName)) score + CyclesAndBytes(16, 16) else score)
        }
    }

    val aCandidates = variablesWithLifetimes.filter {
      case (vName, range) =>
        importances(range.start).a != Important
    }.flatMap {
      case (vName, range) =>
        canBeInlinedToAccumulator(
          featuresForAcc,
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
      if (log.traceEnabled) {
        oldCode.foreach(l => log.trace(l._1.toString))
        log.trace("     ↓")
        newCode.foreach(l => log.trace(l.toString))
      }
    }

    if (bestXs.nonEmpty || bestYs.nonEmpty || bestZs.nonEmpty || bestAs.nonEmpty) {
      val output = ListBuffer[AssemblyLine]()
      var i = 0
      while (i < code.length) {
        var done = false
        bestXs.find(_._2.start == i).foreach {
          case (v, range, _) =>
            log.debug(s"Inlining $v to register X")
            val oldCode = code.zip(importances).slice(range.start, range.end)
            val newCode = inlineVars(Some(v), None, None, None, featuresForIndices, oldCode).result
            reportOptimizedBlock(oldCode, newCode)
            output ++= newCode
            i = range.end
            if (removeVariablesForReal && contains(range, variablesWithLifetimes(v))) {
              f.environment.removeVariable(v)
            }
            done = true
        }
        if (!done) {
          bestYs.find(_._2.start == i).foreach {
            case (v, range, _) =>
              log.debug(s"Inlining $v to register Y")
              val oldCode = code.zip(importances).slice(range.start, range.end)
              val newCode = inlineVars(None, Some(v), None, None, featuresForIndices, oldCode).result
              reportOptimizedBlock(oldCode, newCode)
              output ++= newCode
              i = range.end
              if (removeVariablesForReal && contains(range, variablesWithLifetimes(v))) {
                f.environment.removeVariable(v)
              }
              done = true
          }
        }
        if (!done) {
          bestZs.find(_._2.start == i).foreach {
            case (v, range, _) =>
              log.debug(s"Inlining $v to register Z")
              val oldCode = code.zip(importances).slice(range.start, range.end)
              val newCode = inlineVars(None, None, Some(v), None, featuresForIndices, oldCode).result
              reportOptimizedBlock(oldCode, newCode)
              output ++= newCode
              i = range.end
              if (removeVariablesForReal && contains(range, variablesWithLifetimes(v))) {
                f.environment.removeVariable(v)
              }
              done = true
          }
        }
        if (!done) {
          bestAs.find(_._2.start == i).foreach {
            case (v, range, _) =>
              log.debug(s"Inlining $v to register A")
              val oldCode = code.zip(importances).slice(range.start, range.end)
              val newCode = inlineVars(None, None, None, Some(v), featuresForIndices, oldCode).result
              reportOptimizedBlock(code.zip(importances), newCode)
              output ++= newCode
              i = range.end
              if (removeVariablesForReal && contains(range, variablesWithLifetimes(v))) {
                f.environment.removeVariable(v)
              }
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

  def contains(outer: Range, inner: Range): Boolean = {
    outer.contains(inner.start) && outer.contains(inner.end - 1)
  }

  // TODO: STA has different flag behaviour than TAX, keep it in mind!
  def canBeInlined(xCandidate: Option[String], yCandidate: Option[String], zCandidate: Option[String], features: FeaturesForIndexRegisters, lines: List[(AssemblyLine, CpuImportance)]): Option[CyclesAndBytes] = {
    val vx = xCandidate.getOrElse("-")
    val vy = yCandidate.getOrElse("-")
    val vz = zCandidate.getOrElse("-")
    lines match {
      case (AssemblyLine0(_, Immediate, MemoryAddressConstant(th)), _) :: xs
        if th.name == vx || th.name == vy || th.name == vz =>
        // if an address of a variable is used, then that variable cannot be assigned to a register
        None

      case (AssemblyLine0(_, Immediate, SubbyteConstant(MemoryAddressConstant(th), _)), _) :: xs
        if th.name == vx || th.name == vy || th.name == vz =>
        // if an address of a variable is used, then that variable cannot be assigned to a register
        None

      case (AssemblyLine0(LDX | STX, AbsoluteY | ZeroPageY | IndexedY | IndexedSY | IndexedZ, _), _) :: xs if xCandidate.isDefined =>
        None

      case (AssemblyLine0(LDY | STY, AbsoluteX | ZeroPageX | IndexedX | LongAbsoluteX | IndexedZ, _), _) :: xs if yCandidate.isDefined =>
        None

      case (AssemblyLine0(LDZ | STZ, AbsoluteX | ZeroPageX | IndexedX | LongAbsoluteX | AbsoluteY | ZeroPageY | IndexedY | IndexedSY, _), _) :: xs if zCandidate.isDefined =>
        None

      case (AssemblyLine0(_, AbsoluteX | AbsoluteY | LongAbsoluteX |
                            ZeroPageX | ZeroPageY |
                            IndexedY | IndexedX | IndexedZ |
                            LongIndexedY | LongIndexedZ |
                            Indirect | LongIndirect |
                            AbsoluteIndexedX, MemoryAddressConstant(th)), _) :: xs =>
        // if a variable is used as an array or a pointer, then it cannot be assigned to a register
        if (th.name == vx || th.name == vy || th.name == vz) {
          None
        } else {
          canBeInlined(xCandidate, yCandidate, zCandidate, features, xs)
        }

      case (AssemblyLine0(SEP | REP, Immediate, NumericConstant(nn, _)), _) :: xs =>
        if ((nn & 0x10) == 0) canBeInlined(xCandidate, yCandidate, zCandidate, features, xs)
        else None

      case (AssemblyLine0(SEP | REP, _, _), _) :: xs => None

      case (AssemblyLine0(STY | LDY, Absolute | ZeroPage, MemoryAddressConstant(th)), _) :: xs if th.name == vx =>
        if (features.indexRegisterTransfers) canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 2))
        else None

      case (AssemblyLine0(STX | LDX, Absolute | ZeroPage, MemoryAddressConstant(th)), _) :: xs if th.name == vy =>
        if (features.indexRegisterTransfers) canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 2))
        else None

      case (AssemblyLine(op, Absolute | ZeroPage, MemoryAddressConstant(th), elidability, _), _) :: xs
        if opcodesIdentityTable(op) && features.blastProcessing =>
        if (th.name == vx || th.name == vy) {
          if (elidability == Elidability.Elidable) canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 0, cycles = -1))
          else None
        } else {
          if (th.name == vz) None
          else canBeInlined(xCandidate, yCandidate, zCandidate, features, xs)
        }

      case (AssemblyLine0(opcode, Absolute | ZeroPage, MemoryAddressConstant(th)), _) :: xs
        if th.name == vx && (opcode == LDY || opcode == LDZ || opcodesThatCannotBeUsedWithIndexRegistersAsParameters(opcode)) =>
        // if a variable is used by some opcodes, then it cannot be assigned to a register
        None

      case (AssemblyLine0(opcode, Absolute | ZeroPage, MemoryAddressConstant(th)), _) :: xs
        if th.name == vy && (opcode == LDX || opcode == LAX || opcode == LDZ || opcodesThatCannotBeUsedWithIndexRegistersAsParameters(opcode)) =>
        // if a variable is used by some opcodes, then it cannot be assigned to a register
        None

      case (AssemblyLine0(opcode, Absolute | ZeroPage, MemoryAddressConstant(th)), _) :: xs
        if th.name == vz && (opcode == LDX || opcode == LDY || opcodesThatCannotBeUsedWithIndexRegistersAsParameters(opcode)) =>
        // if a variable is used by some opcodes, then it cannot be assigned to a register
        None

      case (AssemblyLine(LDX, Absolute | ZeroPage, MemoryAddressConstant(th), elidability, _), imp) :: xs
        if xCandidate.isDefined =>
        // if a register is populated with a different variable, then this variable cannot be assigned to that register
        // removing LDX saves 3 cycles
        if (elidability == Elidability.Elidable && th.name == vx) {
          if (imp.z == Unimportant && imp.n == Unimportant) {
            canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 2))
          } else {
            canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 1, cycles = 2))
          }
        } else {
          None
        }

      case (AssemblyLine(LAX, Absolute | ZeroPage, MemoryAddressConstant(th), elidability, _), _) :: xs
        if xCandidate.isDefined =>
        // LAX = LDX-LDA, and since LDX simplifies to nothing and LDA simplifies to TXA,
        // LAX simplifies to TXA, saving two bytes
        if (elidability == Elidability.Elidable && th.name == vx) {
          canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 2))
        } else {
          None
        }

      case (AssemblyLine(LDY, Absolute | ZeroPage, MemoryAddressConstant(th), elidability, _), imp) :: xs if yCandidate.isDefined =>
        // if a register is populated with a different variable, then this variable cannot be assigned to that register
        // removing LDX saves 3 bytes
        // sometimes that LDX has to be converted into CPX#0
        if (elidability == Elidability.Elidable && th.name == vy) {
          if (imp.z == Unimportant && imp.n == Unimportant) {
            canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 3, cycles = 4))
          } else {
            canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 1, cycles = 2))
          }
        } else {
          None
        }

      case (AssemblyLine(LDZ, Absolute | ZeroPage, MemoryAddressConstant(th), elidability, _), imp) :: xs if zCandidate.isDefined =>
        if (elidability == Elidability.Elidable && th.name == vz) {
          if (imp.z == Unimportant && imp.n == Unimportant) {
            canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 3, cycles = 4))
          } else {
            canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 1, cycles = 2))
          }
        } else {
          None
        }

      case (AssemblyLine0(LDX, _, _), _) :: xs if xCandidate.isDefined =>
        // if a register is populated with something else than a variable, then no variable cannot be assigned to that register
        None

      case (AssemblyLine0(LDY, _, _), _) :: xs if yCandidate.isDefined =>
        // if a register is populated with something else than a variable, then no variable cannot be assigned to that register
        None

      case (AssemblyLine0(LDZ, _, _), _) :: xs if zCandidate.isDefined =>
        // if a register is populated with something else than a variable, then no variable cannot be assigned to that register
        None

      case (AssemblyLine(LDA, _, _, elidability, _), _) :: (AssemblyLine(op, Absolute | ZeroPage, MemoryAddressConstant(th), elidability2, _), _) :: xs
        if opcodesCommutative(op) =>
        // LDAw/ANDx -> TXA/ANDw
        if (th.name == vx || th.name == vy) {
          if (elidability == Elidability.Elidable && elidability2 == Elidability.Elidable) canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 2))
          else None
        } else {
          if (th.name == vz) None
          else canBeInlined(xCandidate, yCandidate, zCandidate, features, xs)
        }

      case (AssemblyLine(LDA, _, _, elidability, _), _) :: (AssemblyLine0(CLC, _, _),_) :: (AssemblyLine(op, Absolute | ZeroPage, MemoryAddressConstant(th), elidability2, _), _) :: xs
        if opcodesCommutative(op) =>
        if (th.name == vx || th.name == vy) {
          if (elidability == Elidability.Elidable && elidability2 == Elidability.Elidable) canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 2))
          else None
        } else {
          if (th.name == vz) None
          else canBeInlined(xCandidate, yCandidate, zCandidate, features, xs)
        }

      case (AssemblyLine(LDA, Absolute | ZeroPage, MemoryAddressConstant(th), elidability, _), _) :: (AssemblyLine(TAX, _, _, elidability2, _), _) :: xs
        if xCandidate.isDefined =>
        // a variable cannot be inlined if there is TAX not after LDA of that variable
        // but LDA-TAX can be simplified to TXA
        if (elidability == Elidability.Elidable && elidability2 == Elidability.Elidable && th.name == vx) {
          canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 3, cycles = 4))
        } else {
          None
        }

      case (AssemblyLine(LDA, Absolute | ZeroPage, MemoryAddressConstant(th), elidability, _), _) :: (AssemblyLine(TAY, _, _, elidability2, _), _) :: xs
        if yCandidate.isDefined =>
        // a variable cannot be inlined if there is TAY not after LDA of that variable
        // but LDA-TAY can be simplified to TYA
        if (elidability == Elidability.Elidable && elidability2 == Elidability.Elidable && th.name == vy) {
          canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 3, cycles = 4))
        } else {
          None
        }

      case (AssemblyLine(LDA, Absolute | ZeroPage, MemoryAddressConstant(th), elidability, _), _) :: (AssemblyLine(TAZ, _, _, elidability2, _), _) :: xs
        if zCandidate.isDefined =>
        // a variable cannot be inlined if there is TAZ not after LDA of that variable
        // but LDA-TAZ can be simplified to TZA
        if (elidability == Elidability.Elidable && elidability2 == Elidability.Elidable && th.name == vy) {
          canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 3, cycles = 4))
        } else {
          None
        }

      case (AssemblyLine(LDA | STA, Absolute | ZeroPage, MemoryAddressConstant(th), elidability, _), _) :: xs =>
        // changing LDA->TXA, STA->TAX, INC->INX, DEC->DEX saves 2 bytes
        if (th.name == vy || th.name == vx || th.name == vz) {
          if (elidability == Elidability.Elidable) {
            canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 2))
          } else {
            None
          }
        } else {
          canBeInlined(xCandidate, yCandidate, zCandidate, features, xs)
        }

      case (AssemblyLine(INC | DEC, Absolute | ZeroPage, MemoryAddressConstant(th), elidability, _), _) :: xs =>
        // changing LDA->TXA, STA->TAX, INC->INX, DEC->DEX saves 2 bytes
        if (th.name == vy || th.name == vx || th.name == vz) {
          if (elidability == Elidability.Elidable) {
            canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 4))
          } else {
            None
          }
        } else {
          canBeInlined(xCandidate, yCandidate, zCandidate, features, xs)
        }

      case (AssemblyLine(STZ, Absolute | ZeroPage, MemoryAddressConstant(th), elidability, _), _) :: xs =>
        // changing STZ->LDX saves 1 byte
        if (th.name == vy || th.name == vx) {
          if (elidability == Elidability.Elidable && features.izIsAlwaysZero) {
            canBeInlined(xCandidate, yCandidate, zCandidate, features, xs).map(_ + CyclesAndBytes(bytes = 1, cycles = 2))
          } else {
            None
          }
        } else {
          canBeInlined(xCandidate, yCandidate, zCandidate, features, xs)
        }

      case (AssemblyLine0(TAX, _, _), _) :: xs if xCandidate.isDefined =>
        // a variable cannot be inlined if there is TAX not after LDA of that variable
        None

      case (AssemblyLine0(TAY, _, _), _) :: xs if yCandidate.isDefined =>
        // a variable cannot be inlined if there is TAY not after LDA of that variable
        None

      case (AssemblyLine0(TAZ, _, _), _) :: xs if zCandidate.isDefined =>
        // a variable cannot be inlined if there is TAZ not after LDA of that variable
        None

      case (AssemblyLine0(LABEL, _, _), _) :: xs =>
        // labels always end the initial section
        canBeInlined(xCandidate, yCandidate, zCandidate, features, xs)

      case (AssemblyLine0(JSR, Absolute | LongAbsolute, MemoryAddressConstant(th)), _) :: xs =>
        if (
          xCandidate.isDefined && features.functionsSafeForX(th.name) ||
          yCandidate.isDefined && features.functionsSafeForY(th.name) ||
          zCandidate.isDefined && features.functionsSafeForZ(th.name)
        ) canBeInlined(xCandidate, yCandidate, zCandidate, features, xs)
        else None

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

  private def isReturn(code: List[(AssemblyLine, CpuImportance)], allowLdx: Boolean = true): Boolean = {
    import Opcode._
    code match {
      case (AssemblyLine0(LDX, Immediate, _), _) :: xs => allowLdx && isReturn(xs, allowLdx = false)
      case (AssemblyLine0(RTS | RTI | RTL, _, _), _) :: _ => true
      case (AssemblyLine0(op, _, _), _) :: xs if OpcodeClasses.NoopDiscardsFlags(op) => isReturn(xs, allowLdx = false)
      case _ => false
    }
  }

  def isAllVeryNiceForA(label: String, code: List[(AssemblyLine, CpuImportance)]): (Boolean, List[(AssemblyLine, CpuImportance)]) = {
    code match {
      case (AssemblyLine0(op, _, MemoryAddressConstant(Label(l))), _) :: xs if l == label => (true, xs)
      case (AssemblyLine0(op, _, _), _) :: xs if opcodesThatNeverPrecludeAAllocation(op) => isAllVeryNiceForA(label, xs)
      case _ => (false, Nil)
    }
  }

  def syncsSoon(candidate: String, code: List[(AssemblyLine, CpuImportance)]): Boolean = {
    code match {
      case (AssemblyLine0(op, _, _), _) :: xs if opcodesThatNeverPrecludeAAllocation(op) => syncsSoon(candidate, xs)
      case (AssemblyLine0(LDA, ZeroPage | Absolute | LongAbsolute, MemoryAddressConstant(th)), _) :: xs if th.name == candidate => true
      case _ => false
    }
  }

  def canBeInlinedToAccumulator(features: FeaturesForAccumulator, start: Boolean, synced: Boolean, candidate: String, lines: List[(AssemblyLine, CpuImportance)]): Option[CyclesAndBytes] = {
    lines match {

      case (AssemblyLine(STA, Absolute | ZeroPage, MemoryAddressConstant(th), Elidability.Elidable, _), _) :: xs
        if th.name == candidate && start || synced =>
        canBeInlinedToAccumulator(features, start = false, synced = true, candidate, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 4))

      case (AssemblyLine0(LABEL, _, MemoryAddressConstant(Label(l))), _) :: xs =>
        if ((synced || features.labelsSyncedAt(l)) && syncsSoon(candidate, xs)) {
          canBeInlinedToAccumulator(features, start = start, synced = synced, candidate, xs)
        } else {
          if (features.labelsUsedOnce(l)) {
            val (isNice, tail) = isAllVeryNiceForA(l, xs)
            if (isNice) {
              canBeInlinedToAccumulator(features, start = start, synced = synced, candidate, tail)
            } else {
              None
            }
          } else {
            None
          }
        }

      case (AssemblyLine0(op, Absolute | Relative | LongRelative, MemoryAddressConstant(Label(l))), _) :: xs if OpcodeClasses.AllDirectJumps(op) =>
        if (synced) {
          canBeInlinedToAccumulator(features.addSafeLabel(l), start = start, synced = synced, candidate, xs)
        } else {
          if (features.labelsUsedOnce(l)) {
            val (isNice, tail) = isAllVeryNiceForA(l, xs)
            if (isNice) {
              canBeInlinedToAccumulator(features, start = start, synced = synced, candidate, tail)
            } else {
              None
            }
          } else {
            None
          }
        }

      case (AssemblyLine0(_, TripleAbsolute | ZeroPageWithRelative, _), _) :: xs =>
        // TODO: maybe improve later?
        None

      case (AssemblyLine0(LABEL, _, _), _) :: xs =>
        None

      case (AssemblyLine0(op, _, _),_) :: xs if opcodesThatAlwaysPrecludeAAllocation(op) =>
        None

      case (AssemblyLine0(op, Absolute | ZeroPage, MemoryAddressConstant(th)), _) :: xs
        if th.name == candidate && opcodesThatCannotBeUsedWithAccumulatorAsParameter(op) =>
        // if a variable is used by some opcodes, then it cannot be assigned to a register
        None

      case (AssemblyLine0(_, Immediate, SubbyteConstant(MemoryAddressConstant(th), _)), _) :: xs
        if th.name == candidate =>
        // if an address of a variable is used, then that variable cannot be assigned to a register
        None

      case (AssemblyLine0(_, AbsoluteX | AbsoluteY | ZeroPageX | ZeroPageY | IndexedY | IndexedX | IndexedZ | Indirect | AbsoluteIndexedX, MemoryAddressConstant(th)), _) :: xs
        if th.name == candidate =>
        // if a variable is used as an array or a pointer, then it cannot be assigned to a register
        None

      case (AssemblyLine0(SEP | REP, Immediate, NumericConstant(nn, _)), _) :: xs =>
        if ((nn & 0x20) == 0) canBeInlinedToAccumulator(features, start = false, synced = synced, candidate, xs)
        else None

      case (AssemblyLine0(SEP | REP, _, _), _) :: xs => None

      case (AssemblyLine(STA, _, MemoryAddressConstant(th), elidability, _), _) :: xs if th.name == candidate =>
        if (synced && elidability == Elidability.Elidable) {
          canBeInlinedToAccumulator(features, start = false, synced = true, candidate, xs).map(_ + CyclesAndBytes(bytes = 3, cycles = 4))
        } else {
          None
        }

      case (AssemblyLine0(DCP, Absolute | ZeroPage, MemoryAddressConstant(th)), _) :: xs if th.name == candidate =>
        if (synced) {
          canBeInlinedToAccumulator(features, start = false, synced = true, candidate, xs)
        } else {
          None
        }

      case (AssemblyLine(STA | SAX, _, MemoryAddressConstant(th), elidability, _), _) :: xs if th.name != candidate =>
        if (synced) {
          canBeInlinedToAccumulator(features, start = false, synced = true, candidate, xs)
        } else {
          None
        }

      case (AssemblyLine0(STA | SAX, _, NumericConstant(_, _)), _) :: xs =>
        if (synced) {
          canBeInlinedToAccumulator(features, start = false, synced = true, candidate, xs)
        } else {
          None
        }

      case (AssemblyLine0(SAX, _, MemoryAddressConstant(th)), _) :: xs if th.name == candidate =>
        // if XAA had stable magic $FF, then SAXv/LDAv would correspond to XAA#ff
        // but there's no point in even thinking about that
        None

      case (AssemblyLine0(TAX | TAY, _, _),_) :: xs =>
        if (synced) {
          canBeInlinedToAccumulator(features, start = false, synced = true, candidate, xs)
        } else {
          None
        }

      case (AssemblyLine0(LDA | TYA | TXA | TZA | CLA, _, _), _) :: xs if isReturn(xs) =>
        canBeInlinedToAccumulator(features, start = start, synced = synced, candidate, xs)

      case (AssemblyLine(LDA, Absolute | ZeroPage, MemoryAddressConstant(th), Elidability.Elidable, _), imp) :: xs
        if th.name == candidate =>
        // removing LDA saves 3 bytes
        if (imp.z == Unimportant && imp.n == Unimportant) {
          canBeInlinedToAccumulator(features, start = false, synced = true, candidate, xs).map(_ + CyclesAndBytes(bytes = 3, cycles = 4))
        } else {
          canBeInlinedToAccumulator(features, start = false, synced = true, candidate, xs).map(_ + CyclesAndBytes(bytes = 1, cycles = 2))
        }

      case (AssemblyLine(LDA, _, _, elidability, _), _) :: (AssemblyLine(op, Absolute | ZeroPage, MemoryAddressConstant(th), elidability2, _), _) :: xs
        if opcodesCommutative(op) && synced =>
        if (th.name == candidate) {
          if (elidability == Elidability.Elidable && elidability2 == Elidability.Elidable) canBeInlinedToAccumulator(features, start = false, synced = true, candidate, xs).map(_ + CyclesAndBytes(bytes = 3, cycles = 4))
          else None
        } else canBeInlinedToAccumulator(features, start = false, synced = synced, candidate, xs)

      case (AssemblyLine(LDA, _, _, elidability, _), _) :: (AssemblyLine0(CLC, _, _),_) :: (AssemblyLine(op, Absolute | ZeroPage, MemoryAddressConstant(th), elidability2, _), _) :: xs
        if opcodesCommutative(op) && synced =>
        if (th.name == candidate) {
          if (elidability == Elidability.Elidable && elidability2 == Elidability.Elidable) canBeInlinedToAccumulator(features, start = false, synced = true, candidate, xs).map(_ + CyclesAndBytes(bytes = 3, cycles = 4))
          else None
        } else canBeInlinedToAccumulator(features, start = false, synced = synced, candidate, xs)

      case (AssemblyLine(LDX | LDY | LAX, Absolute | ZeroPage, MemoryAddressConstant(th), elidability, _), _) :: xs
        if th.name == candidate && synced =>
        // converting a load into a transfer saves 2 bytes
        if (elidability == Elidability.Elidable) {
          canBeInlinedToAccumulator(features, start = false, synced = true, candidate, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 2))
        } else {
          None
        }

      case (AssemblyLine0(LDA | LAX, _, _),_) :: xs =>
        // if a register is populated with something else than a variable, then no variable cannot be assigned to that register
        None

      case (AssemblyLine(ASL | LSR | ROR | ROL, Absolute | ZeroPage, MemoryAddressConstant(th), elidability, _), _) :: xs
        if th.name == candidate =>
        if (elidability == Elidability.Elidable) {
          canBeInlinedToAccumulator(features, start = false, synced = false, candidate, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 4))
        } else {
          None
        }

      case (AssemblyLine(INC | DEC, Absolute | ZeroPage, MemoryAddressConstant(th), elidability, _), _) :: xs
        if th.name == candidate =>
        if (features.cmos && elidability == Elidability.Elidable) {
          canBeInlinedToAccumulator(features, start = false, synced = false, candidate, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 4))
        } else {
          None
        }

      case (AssemblyLine(TXA | TYA, _, _, elidability, _), imp) :: xs =>
        if (imp.a == Unimportant && imp.c == Unimportant && imp.v == Unimportant && elidability == Elidability.Elidable) {
          // TYA/TXA has to be converted to CPY#0/CPX#0
          canBeInlinedToAccumulator(features, start = false, synced = false, candidate, xs).map(_ + CyclesAndBytes(bytes = -1, cycles = 0))
        } else {
          None
        }

      case (AssemblyLine0(JSR, Absolute | LongAbsolute, MemoryAddressConstant(th)), _) :: xs =>
        if (features.safeFunctions(th.name)) canBeInlinedToAccumulator(features, start = false, synced = synced, candidate, xs)
        else None

      case (x, _) :: xs => canBeInlinedToAccumulator(features, start = false, synced = synced && OpcodeClasses.AllLinear(x.opcode), candidate, xs)

      case Nil => Some(CyclesAndBytes.Zero)
    }
  }

  def isNot(v: String, param: Constant): Boolean = param match {
    case MemoryAddressConstant(th) => th.name != v
    case CompoundConstant(_, MemoryAddressConstant(th), _) =>  th.name != v
    case _ => true
  }

  def inlineVars(xCandidate: Option[String],
                 yCandidate: Option[String],
                 zCandidate: Option[String],
                 aCandidate: Option[String],
                 features: FeaturesForIndexRegisters,
                 lines: List[(AssemblyLine, CpuImportance)]): TailRec[List[AssemblyLine]] = {
    val vx = xCandidate.getOrElse("-")
    val vy = yCandidate.getOrElse("-")
    val vz = zCandidate.getOrElse("-")
    val va = aCandidate.getOrElse("-")
    lines match {
      case (AssemblyLine(INC, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == vx =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map( AssemblyLine.implied(INX).pos(s) :: _)

      case (AssemblyLine(INC, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == vy =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(INY).pos(s) :: _)

      case (AssemblyLine(INC, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == vz =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(INZ).pos(s) :: _)

      case (AssemblyLine(DEC, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == vx =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(DEX).pos(s) :: _)

      case (AssemblyLine(DEC, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == vy =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(DEY).pos(s) :: _)

      case (AssemblyLine(DEC, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == vz =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(DEZ).pos(s) :: _)

      case (AssemblyLine(opcode@(DEC | INC | ROL | ROR | ASL | LSR), Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == va =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(opcode).pos(s) :: _)

      case (AssemblyLine(LDX, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), imp) :: xs
        if th.name == vx =>
        if (imp.z == Unimportant && imp.n == Unimportant) {
          tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs))
        } else {
          tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.immediate(CPX, 0).pos(s) :: _)
        }

      case (AssemblyLine(LAX, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == vx =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(TXA).pos(s) ::  _)

      case (l@AssemblyLine0(op, Absolute | ZeroPage, MemoryAddressConstant(th)), _) :: xs
        if opcodesIdentityTable(op) && th.name == vx =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(l.copy(addrMode = AbsoluteX, parameter = features.identityArray) ::  _)

      case (l@AssemblyLine0(op, Absolute | ZeroPage, MemoryAddressConstant(th)), _) :: xs
        if opcodesIdentityTable(op) && th.name == vy =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(l.copy(addrMode = AbsoluteY, parameter = features.identityArray) ::  _)

      case (l@AssemblyLine0(LDA | TYA | TXA | TZA | CLA, _, _), _) :: xs if va != "" && isReturn(xs) =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(l ::  _)

      case (l@AssemblyLine0(LDA, _, _), _) ::  (AssemblyLine0(op, Absolute | ZeroPage, MemoryAddressConstant(th)), _) :: xs
        if opcodesCommutative(op) && th.name == va =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(l.copy(opcode = op) ::  _)

      case (l@AssemblyLine0(LDA, _, _), _) :: (clc@AssemblyLine0(CLC, _, _), _) :: (AssemblyLine0(op, Absolute | ZeroPage, MemoryAddressConstant(th)), _) :: xs
        if opcodesCommutative(op) && th.name == va =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(clc :: l.copy(opcode = op) ::  _)

      case (l@AssemblyLine(LDA, _, _, _, s), _) ::  (AssemblyLine0(op, Absolute | ZeroPage, MemoryAddressConstant(th)), _) :: xs
        if opcodesCommutative(op) && th.name == vx =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(TXA).pos(s) :: l.copy(opcode = op) ::  _)

      case (l@AssemblyLine(LDA, _, _, _, s), _) :: (clc@AssemblyLine0(CLC, _, _), _) :: (AssemblyLine0(op, Absolute | ZeroPage, MemoryAddressConstant(th)), _) :: xs
        if opcodesCommutative(op) && th.name == vx =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(TXA).pos(s) :: clc :: l.copy(opcode = op) ::  _)

      case (l@AssemblyLine(LDA, _, _, _, s), _) ::  (AssemblyLine0(op, Absolute | ZeroPage, MemoryAddressConstant(th)), _) :: xs
        if opcodesCommutative(op) && th.name == vy =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(TYA).pos(s) :: l.copy(opcode = op) ::  _)

      case (l@AssemblyLine(LDA, _, _, _, s), _) :: (clc@AssemblyLine0(CLC, _, _), _) :: (AssemblyLine0(op, Absolute | ZeroPage, MemoryAddressConstant(th)), _) :: xs
        if opcodesCommutative(op) && th.name == vy =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(TYA).pos(s) :: clc :: l.copy(opcode = op) ::  _)

      case (AssemblyLine(LDA | STA, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), imp) :: xs
        if th.name == va =>
        if (imp.z == Unimportant && imp.n == Unimportant) {
          inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)
        } else {
          tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.immediate(CMP, 0).pos(s) ::  _)
        }

      case (AssemblyLine(LAX, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == va =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(TAX).pos(s) ::  _)

      case (AssemblyLine(LDY, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), imp) :: xs
        if th.name == vy =>
        if (imp.z == Unimportant && imp.n == Unimportant) {
          inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)
        } else {
          tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.immediate(CPY, 0).pos(s) ::  _)
        }

      case (AssemblyLine(LDZ, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), imp) :: xs
        if th.name == vz =>
        if (imp.z == Unimportant && imp.n == Unimportant) {
          inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)
        } else {
          tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.immediate(CPZ, 0).pos(s) ::  _)
        }

      case (AssemblyLine(LDA, Absolute | ZeroPage, MemoryAddressConstant(th), Elidability.Elidable, s), _) :: (AssemblyLine(TAX, _, _, Elidability.Elidable, _), _) :: xs
        if th.name == vx =>
        // these TXA's may get optimized away by a different optimization
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(TXA).pos(s) ::  _)

      case (AssemblyLine(LDA, Absolute | ZeroPage, MemoryAddressConstant(th), Elidability.Elidable, s), _) :: (AssemblyLine(TAY, _, _, Elidability.Elidable, _), _) :: xs
        if th.name == vy =>
        // these TYA's may get optimized away by a different optimization
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(TYA).pos(s) ::  _)

      case (AssemblyLine(LDA, Absolute | ZeroPage, MemoryAddressConstant(th), Elidability.Elidable, s), _) :: (AssemblyLine(TAZ, _, _, Elidability.Elidable, _), _) :: xs
        if th.name == vz =>
        // these TZA's may get optimized away by a different optimization
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(TZA).pos(s) ::  _)

      case (AssemblyLine(LDX, Absolute | ZeroPage, MemoryAddressConstant(th), Elidability.Elidable, s), _) :: (AssemblyLine(TXA, _, _, Elidability.Elidable, _), _) :: xs
        if th.name == va =>
        // these TAX's may get optimized away by a different optimization
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(TAX).pos(s) ::  _)

      case (AssemblyLine(LDY, Absolute | ZeroPage, MemoryAddressConstant(th), Elidability.Elidable, _), _) :: (AssemblyLine(TYA, _, _, Elidability.Elidable, _), _) :: xs
        if th.name == va =>
        // these TAY's may get optimized away by a different optimization
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(TAY) ::  _)

      case (AssemblyLine(LDA, Absolute | ZeroPage, MemoryAddressConstant(th), _, s1), _) :: (AssemblyLine(CMP, am, param, Elidability.Elidable, s2), _) :: xs
        if th.name == vx && CpxyzAddrModes(am) && isNot(vx, param) =>
        // ditto
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(TXA).pos(s1) :: AssemblyLine(CPX, am, param).pos(s2) ::  _)

      case (AssemblyLine(LDA, Absolute | ZeroPage, MemoryAddressConstant(th), _, s1), _) :: (AssemblyLine(CMP, am, param, Elidability.Elidable, s2), _) :: xs
        if th.name == vy && CpxyzAddrModes(am) && isNot(vx, param) =>
        // ditto
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(TYA).pos(s1) :: AssemblyLine(CPY, am, param).pos(s2) ::  _)

      case (AssemblyLine(LDA, Absolute | ZeroPage, MemoryAddressConstant(th), _, s1), _) :: (AssemblyLine(CMP, am, param, Elidability.Elidable, s2), _) :: xs
        if th.name == vy && CpxyzAddrModes(am) && isNot(vx, param) =>
        // ditto
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(TZA).pos(s1) :: AssemblyLine(CPZ, am, param).pos(s2) ::  _)

      case (AssemblyLine(LDA, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == vx =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(TXA).pos(s) ::  _)

      case (AssemblyLine(LDA, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == vy =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(TYA).pos(s) ::  _)

      case (AssemblyLine(LDY, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == vx =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(TXY).pos(s) ::  _)

      case (AssemblyLine(LDX, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == vy =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(TYX).pos(s) ::  _)

      case (AssemblyLine(LDA, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == vz =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(TZA).pos(s) ::  _)

      case (AssemblyLine(LDX, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == va =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(TAX).pos(s) ::  _)

      case (AssemblyLine(LDY, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == va =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(TAY).pos(s) ::  _)

      case (AssemblyLine(LDA, am, param, Elidability.Elidable, s1), _) :: (AssemblyLine(STA, Absolute | ZeroPage, MemoryAddressConstant(th), Elidability.Elidable, s2), _) :: xs
        if th.name == vx && LdxAddrModes(am) =>
        // these TXA's may get optimized away by a different optimization
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine(LDX, am, param).pos(s1, s2) :: AssemblyLine.implied(TXA).pos(s1, s2) ::  _)

      case (AssemblyLine(LDA, am, param, Elidability.Elidable, s1), _) :: (AssemblyLine(STA, Absolute | ZeroPage, MemoryAddressConstant(th), Elidability.Elidable, s2), _) :: xs
        if th.name == vy && LdyAddrModes(am) =>
        // these TYA's may get optimized away by a different optimization
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine(LDY, am, param).pos(s1, s2) :: AssemblyLine.implied(TYA).pos(s1, s2) ::  _)

      case (AssemblyLine(LDA, am, param, Elidability.Elidable, s1), _) :: (AssemblyLine(STA, Absolute | ZeroPage, MemoryAddressConstant(th), Elidability.Elidable, s2), _) :: xs
        if th.name == vz && LdzAddrModes(am) =>
        // these TZA's may get optimized away by a different optimization
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine(LDZ, am, param).pos(s1, s2) :: AssemblyLine.implied(TZA).pos(s1, s2) :: _)

      case (AssemblyLine(STA, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == vx =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(TAX).pos(s) ::  _)

      case (AssemblyLine(STA, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == vy =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(TAY).pos(s) ::  _)

      case (AssemblyLine(STA, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == vz =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(TAZ).pos(s) ::  _)

      case (AssemblyLine(STX, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == va =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(TXA).pos(s) ::  _)

      case (AssemblyLine(STY, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == va =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(TYA).pos(s) ::  _)

      case (AssemblyLine(STX, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == vy =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(TXY).pos(s) ::  _)

      case (AssemblyLine(STY, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == vx =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(TYX).pos(s) ::  _)

      case (AssemblyLine(STZ, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == vx =>
        if (features.izIsAlwaysZero) tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.immediate(LDX, 0).pos(s) ::  _)
        else features.log.fatal("Unexpected STZ")

      case (AssemblyLine(STZ, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == vy =>
        if (features.izIsAlwaysZero) tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.immediate(LDY, 0).pos(s) ::  _)
        else features.log.fatal("Unexpected STZ")

      case (AssemblyLine(STZ, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == va =>
        if (features.izIsAlwaysZero) tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.immediate(LDA, 0).pos(s) ::  _)
        else tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.implied(TZA).pos(s) ::  _)

      case (AssemblyLine0(TAX, _, _), _) :: xs if xCandidate.isDefined =>
        features.log.fatal("Unexpected TAX")

      case (AssemblyLine0(TAY, _, _), _) :: xs if yCandidate.isDefined =>
        features.log.fatal("Unexpected TAY")

      case (AssemblyLine0(TAZ, _, _), _) :: xs if zCandidate.isDefined =>
        features.log.fatal("Unexpected TAZ")

      case (AssemblyLine(TXA, _, _, _, s), _) :: xs if aCandidate.isDefined =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.immediate(CPX, 0).pos(s) ::  _)

      case (AssemblyLine(TYA, _, _, _, s), _) :: xs if aCandidate.isDefined =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.immediate(CPY, 0).pos(s) ::  _)

      case (AssemblyLine(TZA, _, _, _, s), _) :: xs if aCandidate.isDefined =>
        tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(AssemblyLine.immediate(CPZ, 0).pos(s) ::  _)

      case (x, _) :: xs => tailcall(inlineVars(xCandidate, yCandidate, zCandidate, aCandidate, features, xs)).map(x ::  _)

      case Nil => done(Nil)
    }
  }

  def doesntUseXOrY(am: AddrMode.Value): Boolean = am match {
    case Immediate | WordImmediate | ZeroPage | Absolute | LongAbsolute | Relative | LongRelative | Indirect | LongIndirect | Stack | IndexedZ => true
    case _ => false
  }
}
