package millfork.assembly.mos.opt

import millfork.assembly.mos.AddrMode._
import millfork.assembly.mos.Opcode._
import millfork.assembly.mos._
import millfork.assembly.{AssemblyOptimization, Elidability, OptimizationContext}
import millfork.env._
import millfork.error.Logger
import millfork.node.MosNiceFunctionProperty
import millfork.{CompilationFlag, NonOverlappingIntervals}

import scala.collection.mutable.ListBuffer
import scala.util.control.TailCalls.{TailRec, done, tailcall}

/**
  * @author Karol Stasiak
  */
object TwoVariablesToIndexRegistersOptimization extends AssemblyOptimization[AssemblyLine] {

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


  // If any of these opcodes is present within a method,
  // then it's too hard to assign any variable to a register.
  private val opcodesThatAlwaysPrecludeAllocation = Set(
    JSR,
    STY, TYA, INY, DEY, CPY,
    STX, TXA, INX, DEX, CPX,
    LDX_W, STX_W, CPX_W, DEX_W, INX_W,
    LDY_W, STY_W, CPY_W, DEY_W, INY_W,
    PHX, PLX, PHY, PLY,
    PHX_W, PLX_W, PHY_W, PLY_W,
    SBX, SAX, LXA, XAA, AHX, SHX, SHY, LAS, TAS,
    HuSAX, SAY, SXY, TXY, TXY,
    BYTE,
  )

  private val hasBothIndexingModes = Set(
    LDA, STA, CMP, ADC, SBC, EOR, ORA, AND
  )

  override def name = "Allocating two variables to two index registers"

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
      case v@MemoryVariable(name, typ, VariableAllocationMethod.Auto | VariableAllocationMethod.Register) =>
        typ.size == 1 && !paramVariables(name) && stillUsedVariables(name) && !variablesWithAddressesTaken(name) && !v.isVolatile
      case _ => false
    }
    val variablesWithRegisterHint = f.environment.getAllLocalVariables.filter {
      case v@MemoryVariable(name, typ, VariableAllocationMethod.Register) =>
        typ.size == 1 && !paramVariables(name) && stillUsedVariables(name) && !variablesWithAddressesTaken(name) && !v.isVolatile
      case _ => false
    }.map(_.name).toSet

    val variablesWithLifetimes = localVariables.map(v =>
      v.name -> VariableLifetime.apply(v.name, code, expandToIncludeIndexing = true)
    ).toMap

    val removeVariablesForReal = !options.flag(CompilationFlag.InternalCurrentlyOptimizingForMeasurement)
    val costFunction: CyclesAndBytes => Int = if (options.flag(CompilationFlag.OptimizeForSpeed)) _.cycles else _.bytes
    val importances = ReverseFlowAnalyzer.analyze(f, code, optimizationContext)

    val candidatePairs = for {
      x <- variablesWithLifetimes.toSeq :+ ("--" -> 0.until(0))
      y <- variablesWithLifetimes.toSeq :+ ("--" -> 0.until(0))
      vx = x._1
      vy = y._1
      if vx != vy
      if importances(x._2.start).x != Important
      if importances(y._2.start).y != Important
      range = if (x._2.isEmpty) y._2 else if (y._2.isEmpty) x._2 else (x._2.start min y._2.start) until (x._2.end max y._2.end)
      c <- canBeInlined(vx, vy, "-", "-", code.zip(importances).slice(range.start, range.end))
      bonusX = if (variablesWithRegisterHint(vx)) CyclesAndBytes(16, 16) else CyclesAndBytes.Zero
      bonusY = if (variablesWithRegisterHint(vy)) CyclesAndBytes(16, 16) else CyclesAndBytes.Zero
    } yield (x._1, y._1, range, c + bonusX + bonusY)

    //println(s"candidates: $candidatePairs")

    if (candidatePairs.isEmpty) {
      return code
    }

    val (bestX, bestY, range, _) = candidatePairs.maxBy(t => costFunction(t._4))
    log.debug(s"Inlining $bestX to register X and $bestY to register Y")

    val before = code.take(range.start)
    val after = code.drop(range.end)
    val oldCode = code.zip(importances).slice(range.start, range.end)
    val newCode = inlineVars(bestX, bestY, "-", "-", oldCode).result

    if (log.traceEnabled) {
      oldCode.foreach(l => log.trace(l._1.toString))
      log.trace("     ↓")
      newCode.foreach(l => log.trace(l.toString))
    }

    if (removeVariablesForReal) {
      if (bestX != "--" && contains(range, variablesWithLifetimes(bestX))) {
        f.environment.removeVariable(bestX)
      }
      if (bestY != "--" && contains(range, variablesWithLifetimes(bestY))) {
        f.environment.removeVariable(bestY)
      }
    }

    // TODO
    before ++ newCode ++ after
  }

  def contains(outer: Range, inner: Range): Boolean = {
    outer.contains(inner.start) && outer.contains(inner.end - 1)
  }

  // TODO: STA has different flag behaviour than TAX, keep it in mind!
  def canBeInlined(vx: String, vy: String, loadedX: String, loadedY: String, lines: List[(AssemblyLine, CpuImportance)]): Option[CyclesAndBytes] = {
    @inline
    def fail(i: Int): Option[CyclesAndBytes] = {
//      println(s"$vx and $vy failed because of $i")
//      lines.take(5).map(_._1).foreach(println)
      None
    }
    lines match {

      case (AssemblyLine0(_, Immediate, SubbyteConstant(MemoryAddressConstant(th), _)), _) :: xs
        if th.name == vx || th.name == vy =>
        // if an address of a variable is used, then that variable cannot be assigned to a register
        fail(0)

      case (AssemblyLine0(_, AbsoluteX | AbsoluteY | LongAbsoluteX |
                             ZeroPageX | ZeroPageY |
                             IndexedY | IndexedX | IndexedZ |
                             LongIndexedY | LongIndexedZ |
                             Indirect | LongIndirect |
                             AbsoluteIndexedX, MemoryAddressConstant(th)), _) :: xs if th.name == vx || th.name == vy =>
        // if a variable is used as an array or a pointer, then it cannot be assigned to a register
        fail(1)

      case (AssemblyLine0(SEP | REP, Immediate, NumericConstant(nn, _)), _) :: xs =>
        if ((nn & 0x10) == 0) canBeInlined(vx, vy, loadedX, loadedY, xs)
        else fail(2)

      case (AssemblyLine0(SEP | REP, _, _), _) :: xs => fail(3)

      case (AssemblyLine0(LDY, Absolute | ZeroPage, MemoryAddressConstant(th)), _) :: xs if th.name == vx =>
        canBeInlined(vx, vy, loadedX, vx, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 2))

      case (AssemblyLine0(LDX, Absolute | ZeroPage, MemoryAddressConstant(th)), _) :: xs if th.name == vx =>
        canBeInlined(vx, vy, vx, loadedY, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 2))

      case (AssemblyLine0(LDY, Absolute | ZeroPage, MemoryAddressConstant(th)), _) :: xs if th.name == vy =>
        canBeInlined(vx, vy, loadedX, vy, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 2))

      case (AssemblyLine0(LDX, Absolute | ZeroPage, MemoryAddressConstant(th)), _) :: xs if th.name == vy =>
        canBeInlined(vx, vy, vy, loadedY, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 2))

      case (AssemblyLine0(LDX, _, _), _) :: xs if "--" == vx =>
        canBeInlined(vx, vy, "-", loadedY, xs)

      case (AssemblyLine0(LDY, _, _), _) :: xs if "--" == vy =>
        canBeInlined(vx, vy, loadedX, "-", xs)

      case (AssemblyLine0(_, AbsoluteX, _), _) :: xs if loadedX == vx || vx == "--" && loadedX == "-" =>
          canBeInlined(vx, vy, loadedX, loadedY, xs)

      case (AssemblyLine0(op, AbsoluteX, _), _) :: xs if loadedX == vy =>
        if (hasBothIndexingModes(op)) {
          canBeInlined(vx, vy, loadedX, loadedY, xs)
        } else fail(4)

      case (AssemblyLine0(_, AbsoluteY, _), _) :: xs if loadedY == vy || vy == "--" && loadedY == "-" =>
          canBeInlined(vx, vy, loadedX, loadedY, xs)

      case (AssemblyLine0(op, AbsoluteY, _), _) :: xs if loadedY == vx =>
        if (hasBothIndexingModes(op)) {
          canBeInlined(vx, vy, loadedX, loadedY, xs)
        } else fail(5)

      case (AssemblyLine0(op, IndexedY | IndexedSY | ZeroPageY, _), _) :: xs if loadedY == vy || vy == "--" && loadedY == "-" =>
          canBeInlined(vx, vy, loadedX, loadedY, xs)

      case (AssemblyLine0(op, IndexedX | ZeroPageX | LongAbsoluteX, _), _) :: xs if loadedX == vx || vx == "--" && loadedX == "-" =>
          canBeInlined(vx, vy, loadedX, loadedY, xs)

      case (AssemblyLine0(_, IndexedX | ZeroPageX | LongAbsoluteX | IndexedY | IndexedSY | ZeroPageY | AbsoluteX | AbsoluteY, _), _) :: xs =>
        fail(6)

      case (AssemblyLine(INC | DEC, Absolute | ZeroPage, MemoryAddressConstant(th), elidability, _), _) :: xs if th.name == vx =>
        // changing LDA->TXA, STA->TAX, INC->INX, DEC->DEX saves 2 bytes
          if (elidability == Elidability.Elidable) {
            canBeInlined(vx, vy, "-", loadedY, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 4))
          } else fail(7)

      case (AssemblyLine(INC | DEC, Absolute | ZeroPage, MemoryAddressConstant(th), elidability, _), _) :: xs if th.name == vy =>
        // changing LDA->TXA, STA->TAX, INC->INX, DEC->DEX saves 2 bytes
          if (elidability == Elidability.Elidable) {
            canBeInlined(vx, vy, loadedX, "-", xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 4))
          } else fail(8)

      case (AssemblyLine(LDA, Absolute | ZeroPage, MemoryAddressConstant(th), elidability, _), _) :: xs if th.name == vx || th.name == vy =>
        // changing LDA->TXA, STA->TAX, INC->INX, DEC->DEX saves 2 bytes
          if (elidability == Elidability.Elidable) {
            canBeInlined(vx, vy, loadedX, loadedY, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 2))
          } else fail(9)

      case (AssemblyLine(STA, Absolute | ZeroPage, MemoryAddressConstant(th), elidability, _), _) :: xs if th.name == vx =>
        // changing LDA->TXA, STA->TAX, INC->INX, DEC->DEX saves 2 bytes
          if (elidability == Elidability.Elidable) {
            canBeInlined(vx, vy, "-", loadedY, xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 2))
          } else fail(10)

      case (AssemblyLine(STA, Absolute | ZeroPage, MemoryAddressConstant(th), elidability, _), _) :: xs if th.name == vy =>
        // changing LDA->TXA, STA->TAX, INC->INX, DEC->DEX saves 2 bytes
          if (elidability == Elidability.Elidable) {
            canBeInlined(vx, vy, loadedX, "-", xs).map(_ + CyclesAndBytes(bytes = 2, cycles = 2))
          } else fail(11)

      case (AssemblyLine0(_, _, MemoryAddressConstant(th)), _) :: xs if th.name == vx || th.name == vy =>
        fail(12)

      case (AssemblyLine0(_, _, SubbyteConstant(MemoryAddressConstant(th), _)), _) :: xs if th.name == vx || th.name == vy =>
        fail(13)

      case (AssemblyLine0(op, _, _), _) :: xs if OpcodeClasses.ConcernsXAlways(op) =>
        fail(14)

      case (AssemblyLine0(op, _, _), _) :: xs if OpcodeClasses.ConcernsYAlways(op) =>
        fail(15)

      case (AssemblyLine0(op, _, _), _) :: xs if opcodesThatAlwaysPrecludeAllocation(op) =>
        fail(16)

      case (AssemblyLine0(LABEL | JSR, _, _), _) :: xs =>
        // labels always end the initial section
        canBeInlined(vx, vy, "-", "-", xs)

      case (AssemblyLine0(op, _, _), _) :: xs if OpcodeClasses.AllDirectJumps(op) =>
        // labels always end the initial section
        canBeInlined(vx, vy, "-", "-", xs)

      case _ :: xs => canBeInlined(vx, vy, loadedX, loadedY, xs)
      case Nil => Some(CyclesAndBytes.Zero)

    }
  }

  def inlineVars(vx: String,
                 vy: String,
                 loadedX: String,
                 loadedY: String,
                 lines: List[(AssemblyLine, CpuImportance)]): TailRec[List[AssemblyLine]] = {
    lines match {
      case (AssemblyLine(INC, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == vx =>
        tailcall(inlineVars(vx, vy, "-", loadedY, xs)).map( AssemblyLine.implied(INX).pos(s) :: _)

      case (AssemblyLine(INC, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == vy =>
        tailcall(inlineVars(vx, vy, loadedX, "-", xs)).map(AssemblyLine.implied(INY).pos(s) :: _)

      case (AssemblyLine(DEC, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == vx =>
        tailcall(inlineVars(vx, vy, "-", loadedY, xs)).map(AssemblyLine.implied(DEX).pos(s) :: _)

      case (AssemblyLine(DEC, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == vy =>
        tailcall(inlineVars(vx, vy, loadedX, "-", xs)).map(AssemblyLine.implied(DEY).pos(s) :: _)

      case (AssemblyLine(LDX, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), imp) :: xs
        if th.name == vx =>
        if (imp.z == Unimportant && imp.n == Unimportant) {
          tailcall(inlineVars(vx, vy, vx, loadedY, xs))
        } else {
          tailcall(inlineVars(vx, vy, vx, loadedY, xs)).map(AssemblyLine.immediate(CPX, 0).pos(s) :: _)
        }

      case (AssemblyLine(LDY, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), imp) :: xs
        if th.name == vx =>
        if (imp.z == Unimportant && imp.n == Unimportant) {
          tailcall(inlineVars(vx, vy, loadedX, vx, xs))
        } else {
          tailcall(inlineVars(vx, vy, loadedX, vx, xs)).map(AssemblyLine.immediate(CPX, 0).pos(s) :: _)
        }

      case (AssemblyLine(LDY, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), imp) :: xs
        if th.name == vy =>
        if (imp.z == Unimportant && imp.n == Unimportant) {
          inlineVars(vx, vy, loadedX, vy, xs)
        } else {
          tailcall(inlineVars(vx, vy, loadedX, vy, xs)).map(AssemblyLine.immediate(CPY, 0).pos(s) ::  _)
        }

      case (AssemblyLine(LDX, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), imp) :: xs
        if th.name == vy =>
        if (imp.z == Unimportant && imp.n == Unimportant) {
          inlineVars(vx, vy, vy, loadedY, xs)
        } else {
          tailcall(inlineVars(vx, vy, vy, loadedY, xs)).map(AssemblyLine.immediate(CPY, 0).pos(s) ::  _)
        }

      case (x@AssemblyLine(LDY, _, _, _, _), imp) :: xs =>
        inlineVars(vx, vy, loadedX, "-", xs).map(x ::  _)

      case (x@AssemblyLine(LDX, _, _, _, _), imp) :: xs =>
        inlineVars(vx, vy, "-", loadedY, xs).map(x ::  _)

      case (AssemblyLine(LDA, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == vx =>
        tailcall(inlineVars(vx, vy, vx, loadedY, xs)).map(AssemblyLine.implied(TXA).pos(s) ::  _)

      case (AssemblyLine(LDA, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == vy =>
        tailcall(inlineVars(vx, vy, loadedX, vy, xs)).map(AssemblyLine.implied(TYA).pos(s) ::  _)

      case (AssemblyLine(STA, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == vx =>
        tailcall(inlineVars(vx, vy, loadedX, loadedY, xs)).map(AssemblyLine.implied(TAX).pos(s) ::  _)

      case (AssemblyLine(STA, Absolute | ZeroPage, MemoryAddressConstant(th), _, s), _) :: xs
        if th.name == vy =>
        tailcall(inlineVars(vx, vy, loadedX, loadedY, xs)).map(AssemblyLine.implied(TAY).pos(s) :: _)

      case (l@AssemblyLine0(_, AbsoluteX, _), _) :: xs if loadedX == vy =>
        tailcall(inlineVars(vx, vy, loadedX, loadedY, xs)).map(l.copy(addrMode = AbsoluteY) :: _)

      case (l@AssemblyLine0(_, AbsoluteY, _), _) :: xs if loadedY == vx =>
        tailcall(inlineVars(vx, vy, loadedX, loadedY, xs)).map(l.copy(addrMode = AbsoluteX) :: _)

      case (x, _) :: xs => inlineVars(vx, vy, loadedX, loadedY, xs).map(x ::  _)

      case Nil => done(Nil)
    }
  }

}
