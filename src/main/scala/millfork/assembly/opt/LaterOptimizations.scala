package millfork.assembly.opt

import millfork.assembly.{AddrMode, AssemblyLine, Opcode, State}
import millfork.assembly.Opcode._
import millfork.assembly.AddrMode._
import millfork.assembly.OpcodeClasses._
import millfork.env.{Constant, NormalFunction, NumericConstant}

/**
  * These optimizations help on their own, but may prevent other optimizations from triggering.
  *
  * @author Karol Stasiak
  */
object LaterOptimizations {


  // This optimization tends to prevent later Variable To Register Optimization,
  // so run this only after it's pretty sure V2RO won't happen any more
  val DoubleLoadToDifferentRegisters = new RuleBasedAssemblyOptimization("Double load to different registers",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    TwoDifferentLoadsWithNoFlagChangeInBetween(LDA, Not(ChangesA), LDX, TAX),
    TwoDifferentLoadsWithNoFlagChangeInBetween(LDA, Not(ChangesA), LDY, TAY),
    TwoDifferentLoadsWithNoFlagChangeInBetween(LDX, Not(ChangesX), LDA, TXA),
    TwoDifferentLoadsWithNoFlagChangeInBetween(LDY, Not(ChangesY), LDA, TYA),
    TwoDifferentLoadsWhoseFlagsWillNotBeChecked(LDA, Not(ChangesA), LDX, TAX),
    TwoDifferentLoadsWhoseFlagsWillNotBeChecked(LDA, Not(ChangesA), LDY, TAY),
    TwoDifferentLoadsWhoseFlagsWillNotBeChecked(LDX, Not(ChangesX), LDA, TXA),
    TwoDifferentLoadsWhoseFlagsWillNotBeChecked(LDY, Not(ChangesY), LDA, TYA),
  )

  private def TwoDifferentLoadsWithNoFlagChangeInBetween(opcode1: Opcode.Value, middle: AssemblyLinePattern, opcode2: Opcode.Value, transferOpcode: Opcode.Value) = {
    (HasOpcode(opcode1) & MatchAddrMode(0) & MatchParameter(1)) ~
      (LinearOrLabel & Not(ChangesMemory) & middle & Not(HasOpcode(opcode2))).* ~
      (HasOpcode(opcode2) & Elidable & MatchAddrMode(0) & MatchParameter(1)) ~~> { c =>
      c.init :+ AssemblyLine.implied(transferOpcode)
    }
  }

  private def TwoDifferentLoadsWhoseFlagsWillNotBeChecked(opcode1: Opcode.Value, middle: AssemblyLinePattern, opcode2: Opcode.Value, transferOpcode: Opcode.Value) = {
    ((HasOpcode(opcode1) & MatchAddrMode(0) & MatchParameter(1)) ~
      (LinearOrLabel & Not(ChangesMemory) & middle & Not(HasOpcode(opcode2))).*).capture(2) ~
      (HasOpcode(opcode2) & Elidable & MatchAddrMode(0) & MatchParameter(1)) ~
      ((LinearOrLabel & Not(ReadsNOrZ) & Not(ChangesNAndZ)).* ~ ChangesNAndZ).capture(3) ~~> { (_, ctx) =>
      ctx.get[List[AssemblyLine]](2) ++ (AssemblyLine.implied(transferOpcode) :: ctx.get[List[AssemblyLine]](3))
    }
  }

  private def TwoIdenticalLoadsWithNoFlagChangeInBetween(opcode: Opcode.Value, middle: AssemblyLinePattern) = {
    (HasOpcode(opcode) & MatchAddrMode(0) & MatchParameter(1)) ~
      (LinearOrLabel & Not(ChangesMemory) & middle & Not(ChangesNAndZ)).* ~
      (HasOpcode(opcode) & Elidable & MatchAddrMode(0) & MatchParameter(1)) ~~> { c =>
      c.init
    }
  }

  private def TwoIdenticalImmediateLoadsWithNoFlagChangeInBetween(opcode: Opcode.Value, middle: AssemblyLinePattern) = {
    (HasOpcode(opcode) & HasAddrMode(Immediate) & MatchParameter(1)) ~
      (LinearOrLabel & middle & Not(ChangesNAndZ)).* ~
      (HasOpcode(opcode) & Elidable & HasAddrMode(Immediate) & MatchParameter(1)) ~~> { c =>
      c.init
    }
  }

  private def TwoIdenticalLoadsWhoseFlagsWillNotBeChecked(opcode: Opcode.Value, middle: AssemblyLinePattern) = {
    ((HasOpcode(opcode) & MatchAddrMode(0) & MatchParameter(1)) ~
      (LinearOrLabel & Not(ChangesMemory) & middle).*).capture(2) ~
      (HasOpcode(opcode) & Elidable & MatchAddrMode(0) & MatchParameter(1)) ~
      ((LinearOrLabel & Not(ReadsNOrZ) & Not(ChangesNAndZ)).* ~ ChangesNAndZ).capture(3) ~~> { (_, ctx) =>
      ctx.get[List[AssemblyLine]](2) ++ ctx.get[List[AssemblyLine]](3)
    }
  }

  //noinspection ZeroIndexToHead
  private def InterleavedImmediateLoads(load: Opcode.Value, store: Opcode.Value) = {
    (Elidable & HasOpcode(load) & MatchImmediate(0)) ~
      (Elidable & HasOpcode(store) & HasAddrMode(Absolute) & MatchParameter(8)) ~
      (Elidable & HasOpcode(load) & MatchImmediate(1)) ~
      (Elidable & HasOpcode(store) & HasAddrMode(Absolute) & MatchParameter(9) & DontMatchParameter(8)) ~
      (Elidable & HasOpcode(load) & MatchImmediate(0)) ~~> { c =>
      List(c(2), c(3), c(0), c(1))
    }
  }

  //noinspection ZeroIndexToHead
  private def InterleavedAbsoluteLoads(load: Opcode.Value, store: Opcode.Value) = {
    (Elidable & HasOpcode(load) & HasAddrMode(Absolute) & MatchParameter(0)) ~
      (Elidable & HasOpcode(store) & HasAddrMode(Absolute) & MatchParameter(8) & DontMatchParameter(0)) ~
      (Elidable & HasOpcode(load) & HasAddrMode(Absolute) & MatchParameter(1) & DontMatchParameter(8) & DontMatchParameter(0)) ~
      (Elidable & HasOpcode(store) & HasAddrMode(Absolute) & MatchParameter(9) & DontMatchParameter(8) & DontMatchParameter(1) & DontMatchParameter(0)) ~
      (Elidable & HasOpcode(load) & HasAddrMode(Absolute) & MatchParameter(0)) ~~> { c =>
      List(c(2), c(3), c(0), c(1))
    }
  }

  val DoubleLoadToTheSameRegister = new RuleBasedAssemblyOptimization("Double load to the same register",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    TwoIdenticalLoadsWithNoFlagChangeInBetween(LDA, Not(ChangesA)),
    TwoIdenticalLoadsWithNoFlagChangeInBetween(LDX, Not(ChangesX)),
    TwoIdenticalLoadsWithNoFlagChangeInBetween(LDY, Not(ChangesY)),
    TwoIdenticalLoadsWithNoFlagChangeInBetween(LAX, Not(ChangesA) & Not(ChangesX)),
    TwoIdenticalImmediateLoadsWithNoFlagChangeInBetween(LDA, Not(ChangesA)),
    TwoIdenticalImmediateLoadsWithNoFlagChangeInBetween(LDX, Not(ChangesX)),
    TwoIdenticalImmediateLoadsWithNoFlagChangeInBetween(LDY, Not(ChangesY)),
    TwoIdenticalLoadsWhoseFlagsWillNotBeChecked(LDA, Not(ChangesA)),
    TwoIdenticalLoadsWhoseFlagsWillNotBeChecked(LDX, Not(ChangesX)),
    TwoIdenticalLoadsWhoseFlagsWillNotBeChecked(LDY, Not(ChangesY)),
    TwoIdenticalLoadsWhoseFlagsWillNotBeChecked(LAX, Not(ChangesA) & Not(ChangesX)),
    InterleavedImmediateLoads(LDA, STA),
    InterleavedImmediateLoads(LDX, STX),
    InterleavedImmediateLoads(LDY, STY),
    InterleavedAbsoluteLoads(LDA, STA),
    InterleavedAbsoluteLoads(LDX, STX),
    InterleavedAbsoluteLoads(LDY, STY),
  )

  private def pointlessLoadAfterStore(store: Opcode.Value, load: Opcode.Value, addrMode: AddrMode.Value, meantime: AssemblyLinePattern = Anything) = {
    ((HasOpcode(store) & HasAddrMode(addrMode) & MatchParameter(1)) ~
      (LinearOrBranch & Not(ChangesA) & Not(ChangesMemory) & meantime).*).capture(2) ~
      (HasOpcode(load) & Elidable & HasAddrMode(addrMode) & MatchParameter(1)) ~
      ((LinearOrLabel & Not(ReadsNOrZ) & Not(ChangesNAndZ)).* ~ ChangesNAndZ).capture(3) ~~> { (_, ctx) =>
      ctx.get[List[AssemblyLine]](2) ++ ctx.get[List[AssemblyLine]](3)
    }
  }

  val PointlessLoadAfterStore = new RuleBasedAssemblyOptimization("Pointless load after store",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    pointlessLoadAfterStore(STA, LDA, Absolute),
    pointlessLoadAfterStore(STA, LDA, AbsoluteX, Not(ChangesX)),
    pointlessLoadAfterStore(STA, LDA, AbsoluteY, Not(ChangesY)),
    pointlessLoadAfterStore(STX, LDX, Absolute),
    pointlessLoadAfterStore(STY, LDY, Absolute),
  )


  private val ShiftAddrModes = Set(ZeroPage, ZeroPageX, Absolute, AbsoluteX)
  private val ShiftOpcodes = Set(ASL, ROL, ROR, LSR)

  // LDA-SHIFT-STA is slower than just SHIFT
  // LDA-SHIFT-SHIFT-STA is equally fast as SHIFT-SHIFT, but the latter doesn't use the accumulator
  val PointessLoadingForShifting = new RuleBasedAssemblyOptimization("Pointless loading for shifting",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (Elidable & HasOpcode(LDA) & HasAddrModeIn(ShiftAddrModes) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcodeIn(ShiftOpcodes) & HasAddrMode(Implied) & MatchOpcode(2)) ~
      (Elidable & HasOpcode(STA) & HasAddrModeIn(ShiftAddrModes) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Not(ReadsA) & Not(OverwritesA)).* ~ OverwritesA ~~> { (code, ctx) =>
      AssemblyLine(ctx.get[Opcode.Value](2), ctx.get[AddrMode.Value](0), ctx.get[Constant](1)) :: code.drop(3)
    },
    (Elidable & HasOpcode(LDA) & HasAddrModeIn(ShiftAddrModes) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcodeIn(ShiftOpcodes) & HasAddrMode(Implied) & MatchOpcode(2)) ~
      (Elidable & HasOpcodeIn(ShiftOpcodes) & HasAddrMode(Implied) & MatchOpcode(2)) ~
      (Elidable & HasOpcode(STA) & HasAddrModeIn(ShiftAddrModes) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Not(ReadsA) & Not(OverwritesA)).* ~ OverwritesA ~~> { (code, ctx) =>
      val shift = AssemblyLine(ctx.get[Opcode.Value](2), ctx.get[AddrMode.Value](0), ctx.get[Constant](1))
      shift :: shift :: code.drop(4)
    }
  )

  // SHIFT-LDA is equally fast as LDA-SHIFT-STA, but can enable further optimizations doesn't use the accumulator
  // LDA-SHIFT-SHIFT-STA is equally fast as SHIFT-SHIFT, but the latter doesn't use the accumulator
  val LoadingAfterShifting = new RuleBasedAssemblyOptimization("Loading after shifting",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (Elidable & HasOpcodeIn(ShiftOpcodes) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~~> { (code, ctx) =>
      AssemblyLine(LDA, ctx.get[AddrMode.Value](0), ctx.get[Constant](1)) ::
        AssemblyLine.implied(code.head.opcode) ::
        AssemblyLine(STA, ctx.get[AddrMode.Value](0), ctx.get[Constant](1)) ::
        code.drop(2)
    }
  )

  val UseZeropageAddressingMode = new RuleBasedAssemblyOptimization("Using zeropage addressing mode",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (Elidable & HasAddrMode(Absolute) & MatchParameter(0)) ~ Where(ctx => ctx.get[Constant](0).quickSimplify match {
      case NumericConstant(x, _) => (x & 0xff00) == 0
      case _ => false
    }) ~~> (code => code.head.copy(addrMode = ZeroPage) :: Nil)
  )

  val UseXInsteadOfStack = new RuleBasedAssemblyOptimization("Using X instead of stack",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    (Elidable & HasOpcode(PHA) & DoesntMatterWhatItDoesWith(State.X)) ~
      (Not(ConcernsStack) & Not(ConcernsX)).capture(1) ~
      Where(_.isExternallyLinearBlock(1)) ~
      (Elidable & HasOpcode(PLA)) ~~> (c =>
      AssemblyLine.implied(TAX) :: (c.tail.init :+ AssemblyLine.implied(TXA))
      )
  )

  val UseYInsteadOfStack = new RuleBasedAssemblyOptimization("Using Y instead of stack",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    (Elidable & HasOpcode(PHA) & DoesntMatterWhatItDoesWith(State.Y)) ~
      (Not(ConcernsStack) & Not(ConcernsY)).capture(1) ~
      Where(_.isExternallyLinearBlock(1)) ~
      (Elidable & HasOpcode(PLA)) ~~> (c =>
      AssemblyLine.implied(TAY) :: (c.tail.init :+ AssemblyLine.implied(TYA))
      )
  )

  // TODO: make it more generic
  val IndexSwitchingOptimization = new RuleBasedAssemblyOptimization("Index switching optimization",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    (Elidable & HasOpcode(LDY) & MatchAddrMode(2) & Not(ReadsX) & MatchParameter(0)) ~
      (Elidable & Linear & Not(ChangesY) & HasAddrMode(AbsoluteY) & SupportsAbsoluteX & Not(ConcernsX)) ~
      (HasOpcode(LDY) & Not(ConcernsX)) ~
      (Linear & Not(ChangesY) & Not(ConcernsX) & HasAddrModeIn(Set(AbsoluteY, IndexedY, ZeroPageY))) ~
      (Elidable & HasOpcode(LDY) & MatchAddrMode(2) & Not(ReadsX) & MatchParameter(0)) ~
      (Elidable & Linear & Not(ChangesY) & HasAddrMode(AbsoluteY) & SupportsAbsoluteX & Not(ConcernsX) & DoesntMatterWhatItDoesWith(State.X, State.N, State.Z)) ~~> { (code, ctx) =>
      List(
        code(0).copy(opcode = LDX),
        code(1).copy(addrMode = AbsoluteX),
        code(2),
        code(3),
        code(5).copy(addrMode = AbsoluteX))
    },
    (Elidable & HasOpcode(LDX) & MatchAddrMode(2) & Not(ReadsY) & MatchParameter(0)) ~
      (Elidable & Linear & Not(ChangesX) & HasAddrMode(AbsoluteX) & SupportsAbsoluteY & Not(ConcernsY)) ~
      (HasOpcode(LDX) & Not(ConcernsY)) ~
      (Linear & Not(ChangesX) & Not(ConcernsY) & HasAddrModeIn(Set(AbsoluteX, IndexedX, ZeroPageX, AbsoluteIndexedX))) ~
      (Elidable & HasOpcode(LDX) & MatchAddrMode(2) & Not(ReadsY) & MatchParameter(0)) ~
      (Elidable & Linear & Not(ChangesX) & HasAddrMode(AbsoluteX) & SupportsAbsoluteY & Not(ConcernsY) & DoesntMatterWhatItDoesWith(State.Y, State.N, State.Z)) ~~> { (code, ctx) =>
      List(
        code(0).copy(opcode = LDY),
        code(1).copy(addrMode = AbsoluteY),
        code(2),
        code(3),
        code(5).copy(addrMode = AbsoluteY))
    },

  )

  val All = List(
    DoubleLoadToDifferentRegisters,
    DoubleLoadToTheSameRegister,
    IndexSwitchingOptimization,
    PointlessLoadAfterStore,
    PointessLoadingForShifting,
    LoadingAfterShifting,
    UseXInsteadOfStack,
    UseYInsteadOfStack,
    UseZeropageAddressingMode)
}

