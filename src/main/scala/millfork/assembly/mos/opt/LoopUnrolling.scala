package millfork.assembly.mos.opt

import java.util.concurrent.atomic.AtomicInteger

import millfork.{CompilationFlag, CompilationOptions}
import millfork.assembly.mos.{AssemblyLine, State}
import millfork.assembly.mos.OpcodeClasses._
import millfork.assembly.mos.Opcode._
import millfork.assembly.mos.AddrMode._
import millfork.env.{Constant, Label, MemoryAddressConstant}

/**
  * @author Karol Stasiak
  */
object LoopUnrolling {

  object Unrolling extends Enumeration {
    val X, Y, Var = Value
  }

  val counter = new AtomicInteger(40000)

  def getNextLabel(prefix: String) = f".$prefix%s__${counter.getAndIncrement()}%05d"

  private val Initialization = 634
  private val Start = 453
  private val End = 312
  private val Skip = 1596
  private val Back = 5473
  private val Body = 6354
  private val Step = 63546
  private val BodyWithStep = 6355


  def isFeasible(ctx: AssemblyMatchingContext, branchingSize: Int, index: Unrolling.Value): Boolean = {
    if (!ctx.isExternallyLinearBlock(Body)) return false
    val bodyCode = ctx.get[List[AssemblyLine]](Body)
    val start = ctx.get[Int](Start)
    val end = ctx.getOrDefault[Int](End, 0)
    if (start == end) return false // 256 iterations, don't inline ever
    val increasing = isIncreasing(ctx)
    if (increasing != (start < end)) return false // overflow not supported
    val count = Math.abs(start - end)
    val onlyUsedForArrayIndexing = index match {
      case Unrolling.Var => false
      case Unrolling.X => bodyCode.forall(line => !ConcernsX(line) || line.addrMode == AbsoluteX)
      case Unrolling.Y => bodyCode.forall(line => !ConcernsY(line) || line.addrMode == AbsoluteY)
    }
    val stepSize = index match {
      case Unrolling.Var => 3
      case _ => 1
    }
    val cmpExists = ctx.getOrDefault[Int](End, -1) >= 0
    val bodySize = bodyCode.map(_.sizeInBytes).sum
    val sizeBefore = branchingSize + bodySize + stepSize + (if (cmpExists) 2 else 0)
    val sizeAfter = count * (bodySize + (if (onlyUsedForArrayIndexing) 0 else stepSize))
    if (sizeAfter <= sizeBefore) return true
    if (!ctx.compilationOptions.flag(CompilationFlag.OptimizeForSpeed)) return false
    if (ctx.compilationOptions.flag(CompilationFlag.OptimizeForSonicSpeed)) {
      (sizeAfter - sizeBefore < 128) && (sizeAfter < sizeBefore * 32)
    } else {
      (sizeAfter - sizeBefore < 64) && (sizeAfter < sizeBefore * 8)
    }

  }

  private def isIncreasing(ctx: AssemblyMatchingContext) = {
    val opcode = ctx.get[List[AssemblyLine]](Step).head.opcode
    opcode == INX || opcode == INY || opcode == INZ || opcode == INC || opcode == ISC
  }

  private def fixLabels(code: List[AssemblyLine]) = {
    val localLabels = code.flatMap {
      case AssemblyLine(LABEL, _, MemoryAddressConstant(Label(l)), _) => Some(l)
      case _ => None
    }.toSet
    val labelPrefix = getNextLabel("ur")
    code.map {
      case s@AssemblyLine(_, _, MemoryAddressConstant(Label(l)), _) if localLabels(l) =>
        s.copy(parameter = MemoryAddressConstant(Label(labelPrefix + l)))
      case s => s
    }
  }

  val LoopUnrolling = new RuleBasedAssemblyOptimization("Loop unrolling",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    (Elidable & HasOpcode(LDX) & MatchNumericImmediate(Start) & Not(HasImmediate(0))).capture(Initialization) ~
      (Elidable & HasOpcode(BEQ) & MatchParameter(Skip)) ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(Back)) ~
      ((Elidable & Not(HasOpcodeIn(Set(RTS, JSR, RTI, RTL))) & Not(ChangesX)).*.capture(Body) ~
        (Elidable & HasOpcodeIn(Set(DEX, INX))).capture(Step)
        ).capture(BodyWithStep) ~
      (Elidable & HasOpcode(CPX) & MatchNumericImmediate(End)).? ~
      (Elidable & HasOpcode(BNE) & MatchParameter(Back) & DoesntMatterWhatItDoesWith(State.C, State.N, State.Z)) ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(Skip)) ~
      Where(ctx => isFeasible(ctx, 4, Unrolling.X)) ~~> { (code, ctx) =>
      val start = ctx.get[Int](Start)
      val end = ctx.getOrDefault[Int](End, 0)
      val increasing = isIncreasing(ctx)
      ctx.get[List[AssemblyLine]](Initialization) ++ (0 until Math.abs(start - end)).flatMap(_ => fixLabels(ctx.get[List[AssemblyLine]](BodyWithStep)))
    },
    (Elidable & HasOpcode(LDX) & MatchNumericImmediate(Start)).capture(Initialization) ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(Back)) ~
      ((Elidable & Not(HasOpcodeIn(Set(RTS, JSR, RTI, RTL))) & Not(ChangesX)).*.capture(Body) ~
        (Elidable & HasOpcodeIn(Set(DEX, INX))).capture(Step)
        ).capture(BodyWithStep) ~
      (Elidable & HasOpcode(CPX) & MatchNumericImmediate(End)).? ~
      (Elidable & HasOpcode(BNE) & MatchParameter(Back) & DoesntMatterWhatItDoesWith(State.C, State.N, State.Z)) ~
      Where(ctx => isFeasible(ctx, 2, Unrolling.X)) ~~> { (code, ctx) =>
      val start = ctx.get[Int](Start)
      val end = ctx.getOrDefault[Int](End, 0)
      val increasing = isIncreasing(ctx)
      ctx.get[List[AssemblyLine]](Initialization) ++ (0 until Math.abs(start - end)).flatMap(_ => fixLabels(ctx.get[List[AssemblyLine]](BodyWithStep)))
    },
    (Elidable & HasOpcode(LDX) & MatchNumericImmediate(Start)).capture(Initialization) ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(Back)) ~
      ((Elidable & HasOpcodeIn(Set(DEX, INX))).capture(Step) ~
        (Elidable & Not(HasOpcodeIn(Set(RTS, JSR, RTI, RTL, BNE, CPX, TXA))) & Not(ChangesX)).*.capture(Body)
        ).capture(BodyWithStep) ~
      (Elidable & HasOpcode(CPX) & MatchNumericImmediate(End) | Elidable & HasOpcode(TXA)) ~
      (Elidable & HasOpcode(BNE) & MatchParameter(Back)) ~
      Where(ctx => isFeasible(ctx, 2, Unrolling.X)) ~~> { (code, ctx) =>
      val start = ctx.get[Int](Start)
      val end = ctx.getOrDefault[Int](End, 0)
      val increasing = isIncreasing(ctx)
      ctx.get[List[AssemblyLine]](Initialization) ++ (0 until Math.abs(start - end)).flatMap(_ => fixLabels(ctx.get[List[AssemblyLine]](BodyWithStep)))
    },
    (Elidable & HasOpcode(LDY) & MatchNumericImmediate(Start) & Not(HasImmediate(0))).capture(Initialization) ~
      (Elidable & HasOpcode(BEQ) & MatchParameter(Skip)) ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(Back)) ~
      ((Elidable & Not(HasOpcodeIn(Set(RTS, JSR, RTI, RTL))) & Not(ChangesY)).*.capture(Body) ~
        (Elidable & HasOpcodeIn(Set(DEY, INY))).capture(Step)
        ).capture(BodyWithStep) ~
      (Elidable & HasOpcode(CPY) & MatchNumericImmediate(End)).? ~
      (Elidable & HasOpcode(BNE) & MatchParameter(Back) & DoesntMatterWhatItDoesWith(State.C, State.N, State.Z)) ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(Skip)) ~
      Where(ctx => isFeasible(ctx, 4, Unrolling.Y)) ~~> { (code, ctx) =>
      val start = ctx.get[Int](Start)
      val end = ctx.getOrDefault[Int](End, 0)
      val increasing = isIncreasing(ctx)
      ctx.get[List[AssemblyLine]](Initialization) ++ (0 until Math.abs(start - end)).flatMap(_ => fixLabels(ctx.get[List[AssemblyLine]](BodyWithStep)))
    },
    (Elidable & HasOpcode(LDY) & MatchNumericImmediate(Start)).capture(Initialization) ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(Back)) ~
      ((Elidable & Not(HasOpcodeIn(Set(RTS, JSR, RTI, RTL))) & Not(ChangesY)).*.capture(Body) ~
        (Elidable & HasOpcodeIn(Set(DEY, INY))).capture(Step)
        ).capture(BodyWithStep) ~
      (Elidable & HasOpcode(CPY) & MatchNumericImmediate(End)).? ~
      (Elidable & HasOpcode(BNE) & MatchParameter(Back) & DoesntMatterWhatItDoesWith(State.C, State.N, State.Z)) ~
      Where(ctx => isFeasible(ctx, 2, Unrolling.Y)) ~~> { (code, ctx) =>
      val start = ctx.get[Int](Start)
      val end = ctx.getOrDefault[Int](End, 0)
      val increasing = isIncreasing(ctx)
      ctx.get[List[AssemblyLine]](Initialization) ++ (0 until Math.abs(start - end)).flatMap(_ => fixLabels(ctx.get[List[AssemblyLine]](BodyWithStep)))
    },
    (Elidable & HasOpcode(LDY) & MatchNumericImmediate(Start)).capture(Initialization) ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(Back)) ~
      ((Elidable & HasOpcodeIn(Set(DEY, INY))).capture(Step) ~
        (Elidable & Not(HasOpcodeIn(Set(RTS, JSR, RTI, RTL, BNE, CPY, TYA))) & Not(ChangesY)).*.capture(Body)
        ).capture(BodyWithStep) ~
      (Elidable & HasOpcode(CPY) & MatchNumericImmediate(End) | Elidable & HasOpcode(TYA)) ~
      (Elidable & HasOpcode(BNE) & MatchParameter(Back) & DoesntMatterWhatItDoesWith(State.C, State.N, State.Z)) ~
      Where(ctx => isFeasible(ctx, 2, Unrolling.Y)) ~~> { (code, ctx) =>
      val start = ctx.get[Int](Start)
      val end = ctx.getOrDefault[Int](End, 0)
      val increasing = isIncreasing(ctx)
      ctx.get[List[AssemblyLine]](Initialization) ++ (0 until Math.abs(start - end)).flatMap(_ => fixLabels(ctx.get[List[AssemblyLine]](BodyWithStep)))
    },
  )
}
