package millfork.assembly.opt

import java.util.UUID
import java.util.concurrent.atomic.AtomicInteger

import millfork.assembly.{opt, _}
import millfork.assembly.Opcode._
import millfork.assembly.AddrMode._
import millfork.assembly.OpcodeClasses._
import millfork.env._

/**
  * These optimizations should not remove opportunities for more complex optimizations to trigger.
  *
  * @author Karol Stasiak
  */
object AlwaysGoodOptimizations {

  val counter = new AtomicInteger(30000)

  def getNextLabel(prefix: String) = f".$prefix%s__${counter.getAndIncrement()}%05d"

  val PointlessMath = new RuleBasedAssemblyOptimization("Pointless math",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (HasOpcode(CLC) & Elidable) ~
      (HasOpcode(ADC) & Elidable & MatchParameter(0)) ~
      (HasOpcode(SEC) & Elidable) ~
      (HasOpcode(SBC) & Elidable & MatchParameter(0)) ~
      (LinearOrLabel & Not(ReadsNOrZ) & Not(ReadsV) & Not(ReadsC) & Not(NoopDiscardsFlags) & Not(Set(ADC, SBC))).* ~
      (NoopDiscardsFlags | Set(ADC, SBC)) ~~> (_.drop(4)),
    (HasOpcode(LDA) & HasImmediate(0) & Elidable) ~
      (HasOpcode(CLC) & Elidable) ~
      (HasOpcode(ADC) & Elidable) ~
      (LinearOrLabel & Not(ReadsV) & Not(NoopDiscardsFlags) & Not(ChangesNAndZ)).* ~
      (NoopDiscardsFlags | ChangesNAndZ) ~~> (code => code(2).copy(opcode = LDA) :: code.drop(3))
  )

  val PointlessMathFromFlow = new RuleBasedAssemblyOptimization("Pointless math from flow analysis",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    (Elidable & MatchA(0) &
      HasOpcode(ASL) & HasAddrMode(Implied) & DoesntMatterWhatItDoesWith(State.C)) ~~> { (code, ctx) =>
      AssemblyLine.immediate(LDA, ctx.get[Int](0) << 1) :: Nil
    },
    (Elidable & MatchA(0) &
      HasOpcode(LSR) & HasAddrMode(Implied) & DoesntMatterWhatItDoesWith(State.C)) ~~> { (code, ctx) =>
      AssemblyLine.immediate(LDA, (ctx.get[Int](0) & 0xff) >> 1) :: Nil
    },
    (Elidable & MatchA(0) &
      HasClear(State.C) & HasOpcode(ROL) & HasAddrMode(Implied) & DoesntMatterWhatItDoesWith(State.C)) ~~> { (code, ctx) =>
      AssemblyLine.immediate(LDA, ctx.get[Int](0) << 1) :: Nil
    },
    (Elidable & MatchA(0) &
      HasClear(State.C) & HasOpcode(ROR) & HasAddrMode(Implied) & DoesntMatterWhatItDoesWith(State.C)) ~~> { (code, ctx) =>
      AssemblyLine.immediate(LDA, (ctx.get[Int](0) & 0xff) >> 1) :: Nil
    },
    (Elidable & MatchA(0) &
      HasSet(State.C) & HasOpcode(ROL) & HasAddrMode(Implied) & DoesntMatterWhatItDoesWith(State.C)) ~~> { (code, ctx) =>
      AssemblyLine.immediate(LDA, ctx.get[Int](0) * 2 + 1) :: Nil
    },
    (Elidable & MatchA(0) &
      HasSet(State.C) & HasOpcode(ROR) & HasAddrMode(Implied) & DoesntMatterWhatItDoesWith(State.C)) ~~> { (code, ctx) =>
      AssemblyLine.immediate(LDA, 0x80 + (ctx.get[Int](0) & 0xff) / 2) :: Nil
    },
    (Elidable &
      MatchA(0) & MatchParameter(1) &
      HasOpcode(ADC) & HasAddrMode(Immediate) &
      HasClear(State.D) & HasClear(State.C) & DoesntMatterWhatItDoesWith(State.C, State.V)) ~~> { (code, ctx) =>
      AssemblyLine.immediate(LDA, ctx.get[Constant](1) + ctx.get[Int](0)) :: Nil
    },
    (Elidable &
      MatchA(0) & MatchParameter(1) &
      HasOpcode(ADC) & HasAddrMode(Immediate) &
      HasClear(State.D) & HasClear(State.C) & DoesntMatterWhatItDoesWith(State.V)) ~
      Where(ctx => (ctx.get[Constant](1) + ctx.get[Int](0)).quickSimplify match {
        case NumericConstant(x, _) => x == (x & 0xff)
        case _ => false
      }) ~~> { (code, ctx) =>
      AssemblyLine.immediate(LDA, ctx.get[Constant](1) + ctx.get[Int](0)) :: Nil
    },
    (Elidable &
      MatchA(0) & MatchParameter(1) &
      HasOpcode(ADC) & HasAddrMode(Immediate) &
      HasClear(State.D) & HasSet(State.C) & DoesntMatterWhatItDoesWith(State.C, State.V)) ~~> { (code, ctx) =>
      AssemblyLine.immediate(LDA, ctx.get[Constant](1) + ((ctx.get[Int](0) + 1) & 0xff)) :: Nil
    },
    (Elidable &
      MatchA(0) & MatchParameter(1) &
      HasOpcode(SBC) & HasAddrMode(Immediate) &
      HasClear(State.D) & HasSet(State.C) & DoesntMatterWhatItDoesWith(State.C, State.V)) ~~> { (code, ctx) =>
      AssemblyLine.immediate(LDA, CompoundConstant(MathOperator.Minus, NumericConstant(ctx.get[Int](0), 1), ctx.get[Constant](1)).quickSimplify) :: Nil
    },
    (Elidable &
      MatchA(0) & MatchParameter(1) &
      HasOpcode(EOR) & HasAddrMode(Immediate)) ~~> { (code, ctx) =>
      AssemblyLine.immediate(LDA, CompoundConstant(MathOperator.Exor, NumericConstant(ctx.get[Int](0), 1), ctx.get[Constant](1)).quickSimplify) :: Nil
    },
    (Elidable &
      MatchA(0) & MatchParameter(1) &
      HasOpcode(ORA) & HasAddrMode(Immediate)) ~~> { (code, ctx) =>
      AssemblyLine.immediate(LDA, CompoundConstant(MathOperator.Or, NumericConstant(ctx.get[Int](0), 1), ctx.get[Constant](1)).quickSimplify) :: Nil
    },
    (Elidable &
      MatchA(0) & MatchParameter(1) &
      HasOpcode(AND) & HasAddrMode(Immediate)) ~~> { (code, ctx) =>
      AssemblyLine.immediate(LDA, CompoundConstant(MathOperator.And, NumericConstant(ctx.get[Int](0), 1), ctx.get[Constant](1)).quickSimplify) :: Nil
    },
    (Elidable &
      MatchA(0) & MatchParameter(1) &
      HasOpcode(ANC) & HasAddrMode(Immediate) & DoesntMatterWhatItDoesWith(State.C)) ~~> { (code, ctx) =>
      AssemblyLine.immediate(LDA, CompoundConstant(MathOperator.And, NumericConstant(ctx.get[Int](0), 1), ctx.get[Constant](1)).quickSimplify) :: Nil
    },
  )

  val MathOperationOnTwoIdenticalMemoryOperands = new RuleBasedAssemblyOptimization("Math operation on two identical memory operands",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    (HasOpcodeIn(Set(STA, LDA, LAX)) & HasAddrModeIn(Set(ZeroPage, Absolute)) & MatchAddrMode(9) & MatchParameter(0)) ~
      (Linear & DoesntChangeMemoryAt(9, 0) & Not(ChangesA)).* ~
      (HasClear(State.D) & HasClear(State.C) & HasOpcode(ADC) & HasAddrModeIn(Set(ZeroPage, Absolute)) & MatchParameter(0) & Elidable) ~~> (code => code.init :+ AssemblyLine.implied(ASL)),

    (HasOpcodeIn(Set(STA, LDA)) & HasAddrMode(AbsoluteX) & MatchAddrMode(9) & MatchParameter(0)) ~
      (Linear & DoesntChangeMemoryAt(9, 0) & Not(ChangesA) & Not(ChangesX)).* ~
      (HasClear(State.D) & HasClear(State.C) & HasOpcode(ADC) & HasAddrMode(AbsoluteX) & MatchParameter(0) & Elidable) ~~> (code => code.init :+ AssemblyLine.implied(ASL)),

    (HasOpcodeIn(Set(STA, LDA, LAX)) & HasAddrMode(AbsoluteY) & MatchAddrMode(9) & MatchParameter(0)) ~
      (Linear & DoesntChangeMemoryAt(9, 0) & Not(ChangesA) & Not(ChangesY)).* ~
      (HasClear(State.D) & HasClear(State.C) & HasOpcode(ADC) & HasAddrMode(AbsoluteY) & MatchParameter(0) & Elidable) ~~> (code => code.init :+ AssemblyLine.implied(ASL)),

    (HasOpcodeIn(Set(STA, LDA, LAX)) & HasAddrModeIn(Set(ZeroPage, Absolute)) & MatchAddrMode(9) & MatchParameter(0)) ~
      (Linear & DoesntChangeMemoryAt(9, 0) & Not(ChangesA)).* ~
      (DoesntMatterWhatItDoesWith(State.N, State.Z) & HasOpcodeIn(Set(ORA, AND)) & HasAddrModeIn(Set(ZeroPage, Absolute)) & MatchParameter(0) & Elidable) ~~> (code => code.init),

    (HasOpcodeIn(Set(STA, LDA, LAX)) & HasAddrModeIn(Set(ZeroPage, Absolute)) & MatchAddrMode(9) & MatchParameter(0)) ~
      (Linear & DoesntChangeMemoryAt(9, 0) & Not(ChangesA)).* ~
      (DoesntMatterWhatItDoesWith(State.N, State.Z, State.C) & HasOpcode(ANC) & HasAddrModeIn(Set(ZeroPage, Absolute)) & MatchParameter(0) & Elidable) ~~> (code => code.init),

    (HasOpcodeIn(Set(STA, LDA, LAX)) & HasAddrModeIn(Set(ZeroPage, Absolute)) & MatchAddrMode(9) & MatchParameter(0)) ~
      (Linear & DoesntChangeMemoryAt(9, 0) & Not(ChangesA)).* ~
      (HasOpcode(EOR) & HasAddrModeIn(Set(ZeroPage, Absolute)) & MatchParameter(0) & Elidable) ~~> (code => code.init :+ AssemblyLine.immediate(LDA, 0)),
  )

  val PoinlessStoreBeforeStore = new RuleBasedAssemblyOptimization("Pointless store before store",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (Elidable & HasAddrModeIn(Set(Absolute, ZeroPage)) & MatchParameter(1) & MatchAddrMode(2) & Set(STA, SAX, STX, STY, STZ)) ~
      (LinearOrLabel & DoesNotConcernMemoryAt(2, 1)).* ~
      (MatchParameter(1) & MatchAddrMode(2) & Set(STA, SAX, STX, STY, STZ)) ~~> (_.tail),
    (Elidable & HasAddrModeIn(Set(AbsoluteX, ZeroPageX)) & MatchParameter(1) & MatchAddrMode(2) & Set(STA, STY, STZ)) ~
      (LinearOrLabel & DoesntChangeMemoryAt(2, 1) & Not(ReadsMemory) & Not(ChangesX)).* ~
      (MatchParameter(1) & MatchAddrMode(2) & Set(STA, STY, STZ)) ~~> (_.tail),
    (Elidable & HasAddrModeIn(Set(AbsoluteY, ZeroPageY)) & MatchParameter(1) & MatchAddrMode(2) & Set(STA, SAX, STX, STZ)) ~
      (LinearOrLabel & DoesntChangeMemoryAt(2, 1) & Not(ReadsMemory) & Not(ChangesY)).* ~
      (MatchParameter(1) & MatchAddrMode(2) & Set(STA, SAX, STX, STZ)) ~~> (_.tail),
  )

  val PointlessLoadBeforeReturn = new RuleBasedAssemblyOptimization("Pointless load before return",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (Set(LDA, TXA, TYA, EOR, AND, ORA, ANC) & Elidable) ~ (LinearOrLabel & Not(ConcernsA) & Not(ReadsNOrZ) & Not(HasOpcode(DISCARD_AF))).* ~ HasOpcode(DISCARD_AF) ~~> (_.tail),
    (Set(LDX, TAX, TSX, INX, DEX) & Elidable) ~ (LinearOrLabel & Not(ConcernsX) & Not(ReadsNOrZ) & Not(HasOpcode(DISCARD_XF))).* ~ HasOpcode(DISCARD_XF) ~~> (_.tail),
    (Set(LDY, TAY, INY, DEY) & Elidable) ~ (LinearOrLabel & Not(ConcernsY) & Not(ReadsNOrZ) & Not(HasOpcode(DISCARD_YF))).* ~ HasOpcode(DISCARD_YF) ~~> (_.tail),
    (HasOpcode(LDX) & Elidable & MatchAddrMode(3)) ~
      (LinearOrLabel & Not(ConcernsX) & Not(ReadsNOrZ) & DoesntChangeIndexingInAddrMode(3)).*.capture(1) ~
      (HasOpcode(TXA) & Elidable) ~
      ((LinearOrLabel & Not(ConcernsX) & Not(HasOpcode(DISCARD_XF))).* ~
        HasOpcode(DISCARD_XF)).capture(2) ~~> { (c, ctx) =>
      ctx.get[List[AssemblyLine]](1) ++ (c.head.copy(opcode = LDA) :: ctx.get[List[AssemblyLine]](2))
    },
    (HasOpcode(LDY) & Elidable & MatchAddrMode(3)) ~
      (LinearOrLabel & Not(ConcernsY) & Not(ReadsNOrZ) & DoesntChangeIndexingInAddrMode(3)).*.capture(1) ~
      (HasOpcode(TYA) & Elidable) ~
      ((LinearOrLabel & Not(ConcernsY) & Not(HasOpcode(DISCARD_YF))).* ~
        HasOpcode(DISCARD_YF)).capture(2) ~~> { (c, ctx) =>
      ctx.get[List[AssemblyLine]](1) ++ (c.head.copy(opcode = LDA) :: ctx.get[List[AssemblyLine]](2))
    },
  )

  private def operationPairBuilder(op1: Opcode.Value, op2: Opcode.Value, middle: AssemblyLinePattern) = {
    (HasOpcode(op1) & Elidable) ~
      (Linear & middle).*.capture(1) ~
      (HasOpcode(op2) & Elidable) ~
      ((LinearOrLabel & Not(ReadsNOrZ) & Not(ChangesNAndZ)).* ~ ChangesNAndZ).capture(2) ~~> { (_, ctx) =>
      ctx.get[List[AssemblyLine]](1) ++ ctx.get[List[AssemblyLine]](2)
    }
  }

  val PointlessOperationPairRemoval = new RuleBasedAssemblyOptimization("Pointless operation pair",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    operationPairBuilder(PHA, PLA, Not(ConcernsA) & Not(ConcernsStack)),
    operationPairBuilder(PHX, PLX, Not(ConcernsX) & Not(ConcernsStack)),
    operationPairBuilder(PHY, PLY, Not(ConcernsY) & Not(ConcernsStack)),
    operationPairBuilder(INX, DEX, Not(ConcernsX) & Not(ReadsNOrZ)),
    operationPairBuilder(DEX, INX, Not(ConcernsX) & Not(ReadsNOrZ)),
    operationPairBuilder(INY, DEY, Not(ConcernsX) & Not(ReadsNOrZ)),
    operationPairBuilder(DEY, INY, Not(ConcernsX) & Not(ReadsNOrZ)),
  )


  val BranchInPlaceRemoval = new RuleBasedAssemblyOptimization("Branch in place",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (AllDirectJumps & MatchParameter(0) & Elidable) ~
      HasOpcodeIn(NoopDiscardsFlags).* ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> (c => c.last :: Nil)
  )

  val ImpossibleBranchRemoval = new RuleBasedAssemblyOptimization("Impossible branch",
    needsFlowInfo = FlowInfoRequirement.ForwardFlow,
    (HasOpcode(BCC) & HasSet(State.C) & Elidable) ~~> (_ => Nil),
    (HasOpcode(BCS) & HasClear(State.C) & Elidable) ~~> (_ => Nil),
    (HasOpcode(BVC) & HasSet(State.V) & Elidable) ~~> (_ => Nil),
    (HasOpcode(BVS) & HasClear(State.V) & Elidable) ~~> (_ => Nil),
    (HasOpcode(BNE) & HasSet(State.Z) & Elidable) ~~> (_ => Nil),
    (HasOpcode(BEQ) & HasClear(State.Z) & Elidable) ~~> (_ => Nil),
    (HasOpcode(BPL) & HasSet(State.N) & Elidable) ~~> (_ => Nil),
    (HasOpcode(BMI) & HasClear(State.N) & Elidable) ~~> (_ => Nil),
  )

  val UnconditionalJumpRemoval = new RuleBasedAssemblyOptimization("Unconditional jump removal",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (Elidable & HasOpcode(JMP) & HasAddrMode(Absolute) & MatchParameter(0)) ~
      (Elidable & LinearOrBranch).* ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> (_ => Nil),
    (Elidable & HasOpcode(JMP) & HasAddrMode(Absolute) & MatchParameter(0)) ~
      (Not(HasOpcode(LABEL)) & Not(MatchParameter(0))).* ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~
      (HasOpcode(LABEL) | HasOpcodeIn(NoopDiscardsFlags)).* ~
      HasOpcode(RTS) ~~> (code => AssemblyLine.implied(RTS) :: code.tail),
    (Elidable & HasOpcodeIn(ShortBranching) & MatchParameter(0)) ~
      (HasOpcodeIn(NoopDiscardsFlags).* ~
        (Elidable & HasOpcode(RTS))).capture(1) ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~
      HasOpcodeIn(NoopDiscardsFlags).* ~
      (Elidable & HasOpcode(RTS)) ~~> ((code, ctx) => ctx.get[List[AssemblyLine]](1)),
  )

  val TailCallOptimization = new RuleBasedAssemblyOptimization("Tail call optimization",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (Elidable & HasOpcode(JSR)) ~ HasOpcodeIn(NoopDiscardsFlags).* ~ (Elidable & HasOpcode(RTS)) ~~> (c => c.tail.init :+ c.head.copy(opcode = JMP)),
    (Elidable & HasOpcode(JSR)) ~
      HasOpcode(LABEL).* ~
      HasOpcodeIn(NoopDiscardsFlags).*.capture(0) ~
      HasOpcode(RTS) ~~> ((code, ctx) => ctx.get[List[AssemblyLine]](0) ++ (code.head.copy(opcode = JMP) :: code.tail)),
  )

  val UnusedCodeRemoval = new RuleBasedAssemblyOptimization("Unreachable code removal",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    HasOpcode(JMP) ~ (Not(HasOpcode(LABEL)) & Elidable).+ ~~> (c => c.head :: Nil)
  )

  val PoinlessFlagChange = new RuleBasedAssemblyOptimization("Pointless flag change",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (HasOpcodeIn(Set(CMP, CPX, CPY)) & Elidable) ~ NoopDiscardsFlags ~~> (_.tail),
    (OverwritesC & Elidable) ~ (LinearOrLabel & Not(ReadsC) & Not(DiscardsC)).* ~ DiscardsC ~~> (_.tail),
    (OverwritesD & Elidable) ~ (LinearOrLabel & Not(ReadsD) & Not(DiscardsD)).* ~ DiscardsD ~~> (_.tail),
    (OverwritesV & Elidable) ~ (LinearOrLabel & Not(ReadsV) & Not(DiscardsV)).* ~ DiscardsV ~~> (_.tail)
  )

  // Optimizing Bxx to JMP is generally bad, but it may allow for better optimizations later
  // It's okay to undo it at a later stage, but it's not done yet
  val FlagFlowAnalysis = new RuleBasedAssemblyOptimization("Flag flow analysis",
    needsFlowInfo = FlowInfoRequirement.ForwardFlow,
    (HasSet(State.C) & HasOpcode(SEC) & Elidable) ~~> (_ => Nil),
    (HasSet(State.D) & HasOpcode(SED) & Elidable) ~~> (_ => Nil),
    (HasClear(State.C) & HasOpcode(CLC) & Elidable) ~~> (_ => Nil),
    (HasClear(State.D) & HasOpcode(CLD) & Elidable) ~~> (_ => Nil),
    (HasClear(State.V) & HasOpcode(CLV) & Elidable) ~~> (_ => Nil),
    (HasSet(State.C) & HasOpcode(BCS) & Elidable) ~~> (c => c.map(_.copy(opcode = JMP, addrMode = Absolute))),
    (HasClear(State.C) & HasOpcode(BCC) & Elidable) ~~> (c => c.map(_.copy(opcode = JMP, addrMode = Absolute))),
    (HasSet(State.N) & HasOpcode(BMI) & Elidable) ~~> (c => c.map(_.copy(opcode = JMP, addrMode = Absolute))),
    (HasClear(State.N) & HasOpcode(BPL) & Elidable) ~~> (c => c.map(_.copy(opcode = JMP, addrMode = Absolute))),
    (HasClear(State.V) & HasOpcode(BVC) & Elidable) ~~> (c => c.map(_.copy(opcode = JMP, addrMode = Absolute))),
    (HasSet(State.V) & HasOpcode(BVS) & Elidable) ~~> (c => c.map(_.copy(opcode = JMP, addrMode = Absolute))),
    (HasSet(State.Z) & HasOpcode(BEQ) & Elidable) ~~> (c => c.map(_.copy(opcode = JMP, addrMode = Absolute))),
    (HasClear(State.Z) & HasOpcode(BNE) & Elidable) ~~> (c => c.map(_.copy(opcode = JMP, addrMode = Absolute))),
  )

  val ReverseFlowAnalysis = new RuleBasedAssemblyOptimization("Reverse flow analysis",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    (Elidable & HasOpcodeIn(Set(TXA, TYA, LDA, EOR, ORA, AND)) & DoesntMatterWhatItDoesWith(State.A, State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcode(ANC) & DoesntMatterWhatItDoesWith(State.A, State.C, State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(Set(TAX, TSX, LDX, INX, DEX)) & DoesntMatterWhatItDoesWith(State.X, State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(Set(TAY, LDY, DEY, INY)) & DoesntMatterWhatItDoesWith(State.Y, State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(Set(LAX)) & DoesntMatterWhatItDoesWith(State.A, State.X, State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(Set(SEC, CLC)) & DoesntMatterWhatItDoesWith(State.C)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(Set(CLD, SED)) & DoesntMatterWhatItDoesWith(State.D)) ~~> (_ => Nil),
    (Elidable & HasOpcode(CLV) & DoesntMatterWhatItDoesWith(State.V)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(Set(CMP, CPX, CPY)) & DoesntMatterWhatItDoesWith(State.C, State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(Set(BIT)) & DoesntMatterWhatItDoesWith(State.C, State.N, State.Z, State.V)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(Set(ASL, LSR, ROL, ROR)) & HasAddrMode(Implied) & DoesntMatterWhatItDoesWith(State.A, State.C, State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(Set(ADC, SBC)) & DoesntMatterWhatItDoesWith(State.A, State.C, State.V, State.N, State.Z)) ~~> (_ => Nil),
  )

  private def modificationOfJustWrittenValue(store: Opcode.Value,
                                             addrMode: AddrMode.Value,
                                             initExtra: AssemblyLinePattern,
                                             modify: Opcode.Value,
                                             meantimeExtra: AssemblyLinePattern,
                                             atLeastTwo: Boolean,
                                             flagsToTrash: Seq[State.Value],
                                             fix: ((AssemblyMatchingContext, Int) => List[AssemblyLine]),
                                             alternateStore: Opcode.Value = LABEL) = {
    val actualFlagsToTrash = List(State.N, State.Z) ++ flagsToTrash
    val init = Elidable & HasOpcode(store) & HasAddrMode(addrMode) & MatchAddrMode(3) & MatchParameter(0) & DoesntMatterWhatItDoesWith(actualFlagsToTrash: _*) & initExtra
    val meantime = (Linear & Not(ConcernsMemory) & meantimeExtra).*
    val oneModification = Elidable & HasOpcode(modify) & HasAddrMode(addrMode) & MatchParameter(0) & DoesntMatterWhatItDoesWith(actualFlagsToTrash: _*)
    val modifications = (if (atLeastTwo) oneModification ~ oneModification.+ else oneModification.+).captureLength(1)
    if (alternateStore == LABEL) {
      ((init ~ meantime).capture(2) ~ modifications) ~~> ((code, ctx) => fix(ctx, ctx.get[Int](1)) ++ ctx.get[List[AssemblyLine]](2))
    } else {
      (init.capture(3) ~ meantime.capture(2) ~ modifications) ~~> { (code, ctx) =>
        fix(ctx, ctx.get[Int](1)) ++
          List(AssemblyLine(alternateStore, ctx.get[AddrMode.Value](3), ctx.get[Constant](0))) ++
          ctx.get[List[AssemblyLine]](2)
      }
    }
  }

  val ModificationOfJustWrittenValue = new RuleBasedAssemblyOptimization("Modification of Just written value",
    needsFlowInfo = FlowInfoRequirement.ForwardFlow,
    modificationOfJustWrittenValue(STA, Absolute, MatchA(5), INC, Anything, atLeastTwo = false, Seq(), (c, i) => List(
      AssemblyLine.immediate(LDA, (c.get[Int](5) + i) & 0xff)
    )),
    modificationOfJustWrittenValue(STA, Absolute, MatchA(5), DEC, Anything, atLeastTwo = false, Seq(), (c, i) => List(
      AssemblyLine.immediate(LDA, (c.get[Int](5) - i) & 0xff)
    )),
    modificationOfJustWrittenValue(STA, ZeroPage, MatchA(5), INC, Anything, atLeastTwo = false, Seq(), (c, i) => List(
      AssemblyLine.immediate(LDA, (c.get[Int](5) + i) & 0xff)
    )),
    modificationOfJustWrittenValue(STA, ZeroPage, MatchA(5), DEC, Anything, atLeastTwo = false, Seq(), (c, i) => List(
      AssemblyLine.immediate(LDA, (c.get[Int](5) - i) & 0xff)
    )),
    modificationOfJustWrittenValue(STA, AbsoluteX, MatchA(5), INC, Not(ChangesX), atLeastTwo = false, Seq(), (c, i) => List(
      AssemblyLine.immediate(LDA, (c.get[Int](5) + i) & 0xff)
    )),
    modificationOfJustWrittenValue(STA, AbsoluteX, MatchA(5), DEC, Not(ChangesX), atLeastTwo = false, Seq(), (c, i) => List(
      AssemblyLine.immediate(LDA, (c.get[Int](5) - i) & 0xff)
    )),
    modificationOfJustWrittenValue(STA, Absolute, Anything, INC, Anything, atLeastTwo = true, Seq(State.C, State.V), (_, i) => List(
      AssemblyLine.implied(CLC),
      AssemblyLine.immediate(ADC, i)
    )),
    modificationOfJustWrittenValue(STA, Absolute, Anything, DEC, Anything, atLeastTwo = true, Seq(State.C, State.V), (_, i) => List(
      AssemblyLine.implied(SEC),
      AssemblyLine.immediate(SBC, i)
    )),
    modificationOfJustWrittenValue(STA, ZeroPage, Anything, INC, Anything, atLeastTwo = true, Seq(State.C, State.V), (_, i) => List(
      AssemblyLine.implied(CLC),
      AssemblyLine.immediate(ADC, i)
    )),
    modificationOfJustWrittenValue(STA, ZeroPage, Anything, DEC, Anything, atLeastTwo = true, Seq(State.C, State.V), (_, i) => List(
      AssemblyLine.implied(SEC),
      AssemblyLine.immediate(SBC, i)
    )),
    modificationOfJustWrittenValue(STA, AbsoluteX, Anything, INC, Not(ChangesX), atLeastTwo = true, Seq(State.C, State.V), (_, i) => List(
      AssemblyLine.implied(CLC),
      AssemblyLine.immediate(ADC, i)
    )),
    modificationOfJustWrittenValue(STA, AbsoluteX, Anything, DEC, Not(ChangesX), atLeastTwo = true, Seq(State.C, State.V), (_, i) => List(
      AssemblyLine.implied(SEC),
      AssemblyLine.immediate(SBC, i)
    )),
    modificationOfJustWrittenValue(STA, Absolute, Anything, ASL, Anything, atLeastTwo = false, Seq(State.C), (_, i) => List.fill(i)(AssemblyLine.implied(ASL))),
    modificationOfJustWrittenValue(STA, Absolute, Anything, LSR, Anything, atLeastTwo = false, Seq(State.C), (_, i) => List.fill(i)(AssemblyLine.implied(LSR))),
    modificationOfJustWrittenValue(STA, ZeroPage, Anything, ASL, Anything, atLeastTwo = false, Seq(State.C), (_, i) => List.fill(i)(AssemblyLine.implied(ASL))),
    modificationOfJustWrittenValue(STA, ZeroPage, Anything, LSR, Anything, atLeastTwo = false, Seq(State.C), (_, i) => List.fill(i)(AssemblyLine.implied(LSR))),
    modificationOfJustWrittenValue(STA, AbsoluteX, Anything, ASL, Not(ChangesX), atLeastTwo = false, Seq(State.C), (_, i) => List.fill(i)(AssemblyLine.implied(ASL))),
    modificationOfJustWrittenValue(STA, AbsoluteX, Anything, LSR, Not(ChangesX), atLeastTwo = false, Seq(State.C), (_, i) => List.fill(i)(AssemblyLine.implied(LSR))),
    modificationOfJustWrittenValue(STX, Absolute, Anything, INC, Anything, atLeastTwo = false, Seq(), (_, i) => List.fill(i)(AssemblyLine.implied(INX))),
    modificationOfJustWrittenValue(STX, Absolute, Anything, DEC, Anything, atLeastTwo = false, Seq(), (_, i) => List.fill(i)(AssemblyLine.implied(DEX))),
    modificationOfJustWrittenValue(STY, Absolute, Anything, INC, Anything, atLeastTwo = false, Seq(), (_, i) => List.fill(i)(AssemblyLine.implied(INY))),
    modificationOfJustWrittenValue(STY, Absolute, Anything, DEC, Anything, atLeastTwo = false, Seq(), (_, i) => List.fill(i)(AssemblyLine.implied(DEY))),
    modificationOfJustWrittenValue(STZ, Absolute, Anything, ASL, Anything, atLeastTwo = false, Seq(), (_, i) => Nil),
    modificationOfJustWrittenValue(STZ, Absolute, Anything, LSR, Anything, atLeastTwo = false, Seq(), (_, i) => Nil),
    modificationOfJustWrittenValue(STZ, Absolute, Anything, INC, Anything, atLeastTwo = false, Seq(State.A), (_, i) => List(AssemblyLine.immediate(LDA, i)), STA),
    modificationOfJustWrittenValue(STZ, Absolute, Anything, DEC, Anything, atLeastTwo = false, Seq(State.A), (_, i) => List(AssemblyLine.immediate(LDA, 256 - i)), STA),
    modificationOfJustWrittenValue(STX, ZeroPage, Anything, INC, Anything, atLeastTwo = false, Seq(), (_, i) => List.fill(i)(AssemblyLine.implied(INX))),
    modificationOfJustWrittenValue(STX, ZeroPage, Anything, DEC, Anything, atLeastTwo = false, Seq(), (_, i) => List.fill(i)(AssemblyLine.implied(DEX))),
    modificationOfJustWrittenValue(STY, ZeroPage, Anything, INC, Anything, atLeastTwo = false, Seq(), (_, i) => List.fill(i)(AssemblyLine.implied(INY))),
    modificationOfJustWrittenValue(STY, ZeroPage, Anything, DEC, Anything, atLeastTwo = false, Seq(), (_, i) => List.fill(i)(AssemblyLine.implied(DEY))),
    modificationOfJustWrittenValue(STZ, ZeroPage, Anything, ASL, Anything, atLeastTwo = false, Seq(), (_, i) => Nil),
    modificationOfJustWrittenValue(STZ, ZeroPage, Anything, LSR, Anything, atLeastTwo = false, Seq(), (_, i) => Nil),
    modificationOfJustWrittenValue(STZ, ZeroPage, Anything, INC, Anything, atLeastTwo = false, Seq(State.A), (_, i) => List(AssemblyLine.immediate(LDA, i)), STA),
    modificationOfJustWrittenValue(STZ, ZeroPage, Anything, DEC, Anything, atLeastTwo = false, Seq(State.A), (_, i) => List(AssemblyLine.immediate(LDA, 256 - i)), STA),
  )

  val ConstantFlowAnalysis = new RuleBasedAssemblyOptimization("Constant flow analysis",
    needsFlowInfo = FlowInfoRequirement.ForwardFlow,
    (MatchX(0) & HasAddrMode(AbsoluteX) & SupportsAbsolute & Elidable) ~~> { (code, ctx) =>
      code.map(l => l.copy(addrMode = Absolute, parameter = l.parameter + ctx.get[Int](0)))
    },
    (MatchY(0) & HasAddrMode(AbsoluteY) & SupportsAbsolute & Elidable) ~~> { (code, ctx) =>
      code.map(l => l.copy(addrMode = Absolute, parameter = l.parameter + ctx.get[Int](0)))
    },
    (MatchX(0) & HasAddrMode(ZeroPageX) & Elidable) ~~> { (code, ctx) =>
      code.map(l => l.copy(addrMode = ZeroPage, parameter = l.parameter + ctx.get[Int](0)))
    },
    (MatchY(0) & HasAddrMode(ZeroPageY) & Elidable) ~~> { (code, ctx) =>
      code.map(l => l.copy(addrMode = ZeroPage, parameter = l.parameter + ctx.get[Int](0)))
    },
  )

  val IdempotentDuplicateRemoval = new RuleBasedAssemblyOptimization("Idempotent duplicate operation",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    HasOpcode(RTS) ~ HasOpcodeIn(NoopDiscardsFlags).* ~ (HasOpcode(RTS) ~ Elidable) ~~> (_.take(1)) ::
      HasOpcode(RTI) ~ HasOpcodeIn(NoopDiscardsFlags).* ~ (HasOpcode(RTI) ~ Elidable) ~~> (_.take(1)) ::
      HasOpcode(DISCARD_XF) ~ (Not(HasOpcode(DISCARD_XF)) & HasOpcodeIn(NoopDiscardsFlags + LABEL)).* ~ HasOpcode(DISCARD_XF) ~~> (_.tail) ::
      HasOpcode(DISCARD_AF) ~ (Not(HasOpcode(DISCARD_AF)) & HasOpcodeIn(NoopDiscardsFlags + LABEL)).* ~ HasOpcode(DISCARD_AF) ~~> (_.tail) ::
      HasOpcode(DISCARD_YF) ~ (Not(HasOpcode(DISCARD_YF)) & HasOpcodeIn(NoopDiscardsFlags + LABEL)).* ~ HasOpcode(DISCARD_YF) ~~> (_.tail) ::
      List(RTS, RTI, SEC, CLC, CLV, CLD, SED, SEI, CLI, TAX, TXA, TYA, TAY, TXS, TSX).flatMap { opcode =>
        Seq(
          (HasOpcode(opcode) & Elidable) ~ (HasOpcodeIn(NoopDiscardsFlags) | HasOpcode(LABEL)).* ~ HasOpcode(opcode) ~~> (_.tail),
          HasOpcode(opcode) ~ (HasOpcode(opcode) ~ Elidable) ~~> (_.init),
        )
      }: _*
  )

  val PointlessRegisterTransfers = new RuleBasedAssemblyOptimization("Pointless register transfers",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    HasOpcode(TYA) ~ (Elidable & Set(TYA, TAY)) ~~> (_.init),
    HasOpcode(TXA) ~ (Elidable & Set(TXA, TAX)) ~~> (_.init),
    HasOpcode(TAY) ~ (Elidable & Set(TYA, TAY)) ~~> (_.init),
    HasOpcode(TAX) ~ (Elidable & Set(TXA, TAX)) ~~> (_.init),
    HasOpcode(TSX) ~ (Elidable & Set(TXS, TSX)) ~~> (_.init),
    HasOpcode(TXS) ~ (Elidable & Set(TXS, TSX)) ~~> (_.init),
    HasOpcode(TSX) ~ (Not(ChangesX) & Not(ChangesS) & Linear).* ~ (Elidable & Set(TXS, TSX)) ~~> (_.init),
    HasOpcode(TXS) ~ (Not(ChangesX) & Not(ChangesS) & Linear).* ~ (Elidable & Set(TXS, TSX)) ~~> (_.init),
  )

  val PointlessRegisterTransfersBeforeStore = new RuleBasedAssemblyOptimization("Pointless register transfers before store",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    (Elidable & HasOpcode(TXA)) ~
      (Linear & Not(ConcernsA) & Not(ConcernsX)).* ~
      (Elidable & HasOpcode(STA) & HasAddrModeIn(Set(ZeroPage, ZeroPageY, Absolute)) & DoesntMatterWhatItDoesWith(State.A, State.N, State.Z)) ~~> (code => code.tail.init :+ code.last.copy(opcode = STX)),
    (Elidable & HasOpcode(TYA)) ~
      (Linear & Not(ConcernsA) & Not(ConcernsY)).* ~
      (Elidable & HasOpcode(STA) & HasAddrModeIn(Set(ZeroPage, ZeroPageX, Absolute)) & DoesntMatterWhatItDoesWith(State.A, State.N, State.Z)) ~~> (code => code.tail.init :+ code.last.copy(opcode = STY)),
  )


  val PointlessRegisterTransfersBeforeReturn = new RuleBasedAssemblyOptimization("Pointless register transfers before return",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (HasOpcode(TAX) & Elidable) ~
      HasOpcode(LABEL).* ~
      HasOpcode(TXA).? ~
      ManyWhereAtLeastOne(HasOpcodeIn(NoopDiscardsFlags), HasOpcode(DISCARD_XF)).capture(1) ~
      HasOpcode(RTS) ~~> ((code, ctx) => ctx.get[List[AssemblyLine]](1) ++ (AssemblyLine.implied(RTS) :: code.tail)),
    (HasOpcode(TSX) & Elidable) ~
      HasOpcode(LABEL).* ~
      HasOpcode(TSX).? ~
      ManyWhereAtLeastOne(HasOpcodeIn(NoopDiscardsFlags), HasOpcode(DISCARD_XF)).capture(1) ~
      HasOpcode(RTS) ~~> ((code, ctx) => ctx.get[List[AssemblyLine]](1) ++ (AssemblyLine.implied(RTS) :: code.tail)),
    (HasOpcode(TXA) & Elidable) ~
      HasOpcode(LABEL).* ~
      HasOpcode(TAX).? ~
      ManyWhereAtLeastOne(HasOpcodeIn(NoopDiscardsFlags), HasOpcode(DISCARD_AF)).capture(1) ~
      HasOpcode(RTS) ~~> ((code, ctx) => ctx.get[List[AssemblyLine]](1) ++ (AssemblyLine.implied(RTS) :: code.tail)),
    (HasOpcode(TAY) & Elidable) ~
      HasOpcode(LABEL).* ~
      HasOpcode(TYA).? ~
      ManyWhereAtLeastOne(HasOpcodeIn(NoopDiscardsFlags), HasOpcode(DISCARD_YF)).capture(1) ~
      HasOpcode(RTS) ~~> ((code, ctx) => ctx.get[List[AssemblyLine]](1) ++ (AssemblyLine.implied(RTS) :: code.tail)),
    (HasOpcode(TYA) & Elidable) ~
      HasOpcode(LABEL).* ~
      HasOpcode(TAY).? ~
      ManyWhereAtLeastOne(HasOpcodeIn(NoopDiscardsFlags), HasOpcode(DISCARD_AF)).capture(1) ~
      HasOpcode(RTS) ~~> ((code, ctx) => ctx.get[List[AssemblyLine]](1) ++ (AssemblyLine.implied(RTS) :: code.tail)),
  )

  val PointlessRegisterTransfersBeforeCompare = new RuleBasedAssemblyOptimization("Pointless register transfers before compare",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    HasOpcodeIn(Set(DEX, INX, LDX, LAX)) ~
      (HasOpcode(TXA) & Elidable & DoesntMatterWhatItDoesWith(State.A)) ~~> (code => code.init),
    HasOpcodeIn(Set(DEY, INY, LDY)) ~
      (HasOpcode(TYA) & Elidable & DoesntMatterWhatItDoesWith(State.A)) ~~> (code => code.init),
  )

  private def stashing(tai: Opcode.Value, tia: Opcode.Value, readsI: AssemblyLinePattern, concernsI: AssemblyLinePattern, discardIF: Opcode.Value, withRts: Boolean, withBeq: Boolean) = {
    val init: AssemblyPattern = if (withBeq) {
      (Linear & ChangesNAndZ & ChangesA) ~
        (HasOpcode(tai) & Elidable) ~
        (Linear & Not(concernsI) & Not(ChangesA) & Not(ReadsNOrZ)).* ~
        (ShortBranching & ReadsNOrZ & MatchParameter(0))
    } else {
      (HasOpcode(tai) & Elidable) ~
        (Linear & Not(concernsI) & Not(ChangesA) & Not(ReadsNOrZ)).* ~
        ((ShortBranching -- ReadsNOrZ) & MatchParameter(0))
    }
    val inner: AssemblyPattern = if (withRts) {
      (Linear & Not(readsI) & Not(ReadsNOrZ ++ NoopDiscardsFlags)).* ~
        ManyWhereAtLeastOne(HasOpcodeIn(NoopDiscardsFlags), HasOpcode(discardIF)) ~
        HasOpcodeIn(Set(RTS, RTI)) ~
        Not(HasOpcode(LABEL)).*
    } else {
      (Linear & Not(concernsI) & Not(ChangesA) & Not(ReadsNOrZ)).*
    }
    val end: AssemblyPattern =
      (HasOpcode(LABEL) & MatchParameter(0)) ~
        (Linear & Not(concernsI) & Not(ChangesA) & Not(ReadsNOrZ)).* ~
        (HasOpcode(tia) & Elidable)
    val total = init ~ inner ~ end
    if (withBeq) {
      total ~~> (code => code.head :: (code.tail.tail.init :+ AssemblyLine.implied(tai)))
    } else {
      total ~~> (code => code.tail.init :+ AssemblyLine.implied(tai))
    }
  }

  // Optimize the following patterns:
  // TAX - B__ .a - don't change A - .a - TXA
  // TAX - B__ .a - change A – discard X – RTS - .a - TXA
  // by removing the first transfer and flipping the second one
  val PointlessStashingToIndexOverShortSafeBranch = new RuleBasedAssemblyOptimization("Pointless stashing into index over short safe branch",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    //    stashing(TAX, TXA, ReadsX, ConcernsX, DISCARD_XF, withRts = false, withBeq = false),
    stashing(TAX, TXA, ReadsX, ConcernsX, DISCARD_XF, withRts = true, withBeq = false),
    //    stashing(TAX, TXA, ReadsX, ConcernsX, DISCARD_XF, withRts = false, withBeq = true),
    //    stashing(TAX, TXA, ReadsX, ConcernsX, DISCARD_XF, withRts = true, withBeq = true),
    //
    //    stashing(TAY, TYA, ReadsY, ConcernsY, DISCARD_YF, withRts = false, withBeq = false),
    //    stashing(TAY, TYA, ReadsY, ConcernsY, DISCARD_YF, withRts = true, withBeq = false),
    //    stashing(TAY, TYA, ReadsY, ConcernsY, DISCARD_YF, withRts = false, withBeq = true),
    //    stashing(TAY, TYA, ReadsY, ConcernsY, DISCARD_YF, withRts = true, withBeq = true),
  )

  private def loadBeforeTransfer(ld1: Opcode.Value, ld2: Opcode.Value, concerns1: AssemblyLinePattern, overwrites1: State.Value, t12: Opcode.Value, ams: Set[AddrMode.Value]) =
    (Elidable & HasOpcode(ld1) & MatchAddrMode(0) & MatchParameter(1) & HasAddrModeIn(ams)) ~
      (Linear & Not(ReadsNOrZ) & Not(concerns1) & DoesntChangeMemoryAt(0, 1) & DoesntChangeIndexingInAddrMode(0) & Not(HasOpcode(t12))).*.capture(2) ~
      (HasOpcode(t12) & Elidable & DoesntMatterWhatItDoesWith(overwrites1, State.N, State.Z)) ~~> { (code, ctx) =>
      ctx.get[List[AssemblyLine]](2) :+ code.head.copy(opcode = ld2)
    }

  val PointlessLoadBeforeTransfer = new RuleBasedAssemblyOptimization("Pointless load before transfer",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    loadBeforeTransfer(LDX, LDA, ConcernsX, State.X, TXA, Set(ZeroPage, Absolute, IndexedY, AbsoluteY)),
    loadBeforeTransfer(LDA, LDX, ConcernsA, State.A, TAX, Set(ZeroPage, Absolute, IndexedY, AbsoluteY)),
    loadBeforeTransfer(LDY, LDA, ConcernsY, State.Y, TYA, Set(ZeroPage, Absolute, ZeroPageX, IndexedX, AbsoluteX)),
    loadBeforeTransfer(LDA, LDY, ConcernsA, State.A, TAY, Set(ZeroPage, Absolute, ZeroPageX, IndexedX, AbsoluteX)),
  )

  private def immediateLoadBeforeTwoTransfers(ld1: Opcode.Value, ld2: Opcode.Value, concerns1: AssemblyLinePattern, overwrites1: State.Value, t12: Opcode.Value, t21: Opcode.Value) =
    (Elidable & HasOpcode(ld1) & HasAddrMode(Immediate)) ~
      (Linear & Not(ReadsNOrZ) & Not(concerns1) & Not(HasOpcode(t12))).*.capture(2) ~
      (HasOpcode(t12) & Elidable & DoesntMatterWhatItDoesWith(overwrites1, State.N, State.Z)) ~~> { (code, ctx) =>
      ctx.get[List[AssemblyLine]](2) :+ code.head.copy(opcode = ld2)
    }

  val YYY = new RuleBasedAssemblyOptimization("Pointless load before transfer",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    immediateLoadBeforeTwoTransfers(LDA, LDY, ConcernsA, State.A, TAY, TYA),
  )

  val ConstantIndexPropagation = new RuleBasedAssemblyOptimization("Constant index propagation",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (HasOpcode(LDX) & HasAddrMode(Immediate) & MatchParameter(0)) ~
      (Linear & Not(ChangesX) & Not(HasAddrMode(AbsoluteX))).* ~
      (Elidable & SupportsAbsolute & HasAddrMode(AbsoluteX)) ~~> { (lines, ctx) =>
      val last = lines.last
      val offset = ctx.get[Constant](0)
      lines.init :+ last.copy(addrMode = Absolute, parameter = last.parameter + offset)
    },
    (HasOpcode(LDY) & HasAddrMode(Immediate) & MatchParameter(0)) ~
      (Linear & Not(ChangesY) & Not(HasAddrMode(AbsoluteY))).* ~
      (Elidable & SupportsAbsolute & HasAddrMode(AbsoluteY)) ~~> { (lines, ctx) =>
      val last = lines.last
      val offset = ctx.get[Constant](0)
      lines.init :+ last.copy(addrMode = Absolute, parameter = last.parameter + offset)
    },
  )

  val PoinlessLoadBeforeAnotherLoad = new RuleBasedAssemblyOptimization("Pointless load before another load",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (Set(LDA, TXA, TYA) & Elidable) ~ (LinearOrLabel & Not(ConcernsA) & Not(ReadsNOrZ)).* ~ OverwritesA ~~> (_.tail),
    (Set(LDX, TAX, TSX) & Elidable) ~ (LinearOrLabel & Not(ConcernsX) & Not(ReadsNOrZ)).* ~ OverwritesX ~~> (_.tail),
    (Set(LDY, TAY) & Elidable) ~ (LinearOrLabel & Not(ConcernsY) & Not(ReadsNOrZ)).* ~ OverwritesY ~~> (_.tail),
  )

  // TODO: better proofs that memory doesn't change
  val PointlessLoadAfterLoadOrStore = new RuleBasedAssemblyOptimization("Pointless load after load or store",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,

    (HasOpcodeIn(Set(LDA, STA)) & HasAddrMode(Immediate) & MatchParameter(1)) ~
      (Linear & Not(ChangesA)).* ~
      (Elidable & HasOpcode(LDA) & HasAddrMode(Immediate) & MatchParameter(1)) ~~> (_.init),

    (HasOpcodeIn(Set(LDX, STX)) & HasAddrMode(Immediate) & MatchParameter(1)) ~
      (Linear & Not(ChangesX)).* ~
      (Elidable & HasOpcode(LDX) & HasAddrMode(Immediate) & MatchParameter(1)) ~~> (_.init),

    (HasOpcodeIn(Set(LDY, STY)) & HasAddrMode(Immediate) & MatchParameter(1)) ~
      (Linear & Not(ChangesY)).* ~
      (Elidable & HasOpcode(LDY) & HasAddrMode(Immediate) & MatchParameter(1)) ~~> (_.init),

    (HasOpcodeIn(Set(LDA, STA)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(ChangesA) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~~> (_.init),

    (HasOpcodeIn(Set(LDX, STX)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(ChangesX) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(LDX) & MatchAddrMode(0) & MatchParameter(1)) ~~> (_.init),

    (HasOpcodeIn(Set(LDY, STY)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(ChangesY) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(LDY) & MatchAddrMode(0) & MatchParameter(1)) ~~> (_.init),
  )

  val PointlessOperationAfterLoad = new RuleBasedAssemblyOptimization("Pointless operation after load",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (ChangesA & ChangesNAndZ) ~ (Elidable & HasOpcode(EOR) & HasImmediate(0)) ~~> (_.init),
    (ChangesA & ChangesNAndZ) ~ (Elidable & HasOpcode(ORA) & HasImmediate(0)) ~~> (_.init),
    (ChangesA & ChangesNAndZ) ~ (Elidable & HasOpcode(AND) & HasImmediate(0xff)) ~~> (_.init)
  )

  val SimplifiableBitOpsSequence = new RuleBasedAssemblyOptimization("Simplifiable sequence of bit operations",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (Elidable & HasOpcode(EOR) & MatchImmediate(0)) ~
      (Linear & Not(ChangesA) & Not(ReadsNOrZ) & Not(ReadsA)).* ~
      (Elidable & HasOpcode(EOR) & MatchImmediate(1)) ~~> { (lines, ctx) =>
      lines.init.tail :+ AssemblyLine.immediate(EOR, CompoundConstant(MathOperator.Exor, ctx.get[Constant](0), ctx.get[Constant](1)))
    },
    (Elidable & HasOpcode(ORA) & MatchImmediate(0)) ~
      (Linear & Not(ChangesA) & Not(ReadsNOrZ) & Not(ReadsA)).* ~
      (Elidable & HasOpcode(ORA) & MatchImmediate(1)) ~~> { (lines, ctx) =>
      lines.init.tail :+ AssemblyLine.immediate(ORA, CompoundConstant(MathOperator.Or, ctx.get[Constant](0), ctx.get[Constant](1)))
    },
    (Elidable & HasOpcode(AND) & MatchImmediate(0)) ~
      (Linear & Not(ChangesA) & Not(ReadsNOrZ) & Not(ReadsA)).* ~
      (Elidable & HasOpcode(AND) & MatchImmediate(1)) ~~> { (lines, ctx) =>
      lines.init.tail :+ AssemblyLine.immediate(AND, CompoundConstant(MathOperator.And, ctx.get[Constant](0), ctx.get[Constant](1)))
    },
    (Elidable & HasOpcode(ANC) & MatchImmediate(0)) ~
      (Linear & Not(ChangesA) & Not(ReadsNOrZ) & Not(ReadsC) & Not(ReadsA)).* ~
      (Elidable & HasOpcode(ANC) & MatchImmediate(1)) ~~> { (lines, ctx) =>
      lines.init.tail :+ AssemblyLine.immediate(ANC, CompoundConstant(MathOperator.And, ctx.get[Constant](0), ctx.get[Constant](1)))
    },
  )

  val RemoveNops = new RuleBasedAssemblyOptimization("Removing NOP instructions",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (Elidable & HasOpcode(NOP)) ~~> (_ => Nil)
  )

  val RearrangeMath = new RuleBasedAssemblyOptimization("Rearranging math",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (Elidable & HasOpcode(LDA) & HasAddrMode(Immediate)) ~
      (Elidable & HasOpcodeIn(Set(CLC, SEC))) ~
      (Elidable & HasOpcode(ADC) & Not(HasAddrMode(Immediate))) ~~> { c =>
      c.last.copy(opcode = LDA) :: c(1) :: c.head.copy(opcode = ADC) :: Nil
    },
    (Elidable & HasOpcode(LDA) & HasAddrMode(Immediate)) ~
      (Elidable & HasOpcodeIn(Set(ADC, EOR, ORA, AND)) & Not(HasAddrMode(Immediate))) ~~> { c =>
      c.last.copy(opcode = LDA) :: c.head.copy(opcode = c.last.opcode) :: Nil
    },
  )

  private def wordShifting(i: Int, hiFirst: Boolean, hiFromX: Boolean) = {
    val ldax = if (hiFromX) LDX else LDA
    val stax = if (hiFromX) STX else STA
    val restriction = if (hiFromX) Not(ReadsX) else Anything
    val originalStart = if (hiFirst) {
      (Elidable & HasOpcode(LDA) & MatchParameter(0) & MatchAddrMode(1)) ~
        (Elidable & HasOpcode(STA) & MatchParameter(2) & MatchAddrMode(3) & restriction) ~
        (Elidable & HasOpcode(ldax) & HasImmediate(0)) ~
        (Elidable & HasOpcode(stax) & MatchParameter(4) & MatchAddrMode(5))
    } else {
      (Elidable & HasOpcode(ldax) & HasImmediate(0)) ~
        (Elidable & HasOpcode(stax) & MatchParameter(4) & MatchAddrMode(5)) ~
        (Elidable & HasOpcode(LDA) & MatchParameter(0) & MatchAddrMode(1)) ~
        (Elidable & HasOpcode(STA) & MatchParameter(2) & MatchAddrMode(3) & restriction)
    }
    val middle = (Linear & Not(ConcernsMemory) & DoesntChangeIndexingInAddrMode(3) & DoesntChangeIndexingInAddrMode(5)).*
    val singleOriginalShift =
      (Elidable & HasOpcode(ASL) & MatchParameter(2) & MatchAddrMode(3)) ~
        (Elidable & HasOpcode(ROL) & MatchParameter(4) & MatchAddrMode(5) & DoesntMatterWhatItDoesWith(State.C, State.N, State.V, State.Z))
    val originalShifting = (1 to i).map(_ => singleOriginalShift).reduce(_ ~ _)
    originalStart ~ middle.capture(6) ~ originalShifting ~~> { (code, ctx) =>
      val newStart = List(
        code(0),
        code(1).copy(addrMode = code(3).addrMode, parameter = code(3).parameter),
        code(2),
        code(3).copy(addrMode = code(1).addrMode, parameter = code(1).parameter))
      val middle = ctx.get[List[AssemblyLine]](6)
      val singleNewShift = List(
        AssemblyLine(LSR, ctx.get[AddrMode.Value](5), ctx.get[Constant](4)),
        AssemblyLine(ROR, ctx.get[AddrMode.Value](3), ctx.get[Constant](2)))
      newStart ++ middle ++ (i until 8).flatMap(_ => singleNewShift)
    }
  }

  val SmarterShiftingWords = new RuleBasedAssemblyOptimization("Smarter shifting of words",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    wordShifting(8, hiFirst = false, hiFromX = true),
    wordShifting(8, hiFirst = false, hiFromX = false),
    wordShifting(8, hiFirst = true, hiFromX = true),
    wordShifting(8, hiFirst = true, hiFromX = false),
    wordShifting(7, hiFirst = false, hiFromX = true),
    wordShifting(7, hiFirst = false, hiFromX = false),
    wordShifting(7, hiFirst = true, hiFromX = true),
    wordShifting(7, hiFirst = true, hiFromX = false),
    wordShifting(6, hiFirst = false, hiFromX = true),
    wordShifting(6, hiFirst = false, hiFromX = false),
    wordShifting(6, hiFirst = true, hiFromX = true),
    wordShifting(6, hiFirst = true, hiFromX = false),
    wordShifting(5, hiFirst = false, hiFromX = true),
    wordShifting(5, hiFirst = false, hiFromX = false),
    wordShifting(5, hiFirst = true, hiFromX = true),
    wordShifting(5, hiFirst = true, hiFromX = false),
  )

  private def carryFlagConversionCase(shift: Int, firstSet: Boolean, zeroIfSet: Boolean) = {
    val nonZero = 1 << shift
    val test = Elidable & HasOpcode(if (firstSet) BCC else BCS) & MatchParameter(0)
    val ifSet = Elidable & HasOpcode(LDA) & HasImmediate(if (zeroIfSet) 0 else nonZero)
    val ifClear = Elidable & HasOpcode(LDA) & HasImmediate(if (zeroIfSet) nonZero else 0)
    val jump = Elidable & HasOpcodeIn(Set(JMP, if (firstSet) BCS else BCC, if (zeroIfSet) BEQ else BNE)) & MatchParameter(1)
    val elseLabel = Elidable & HasOpcode(LABEL) & MatchParameter(0)
    val afterLabel = Elidable & HasOpcode(LABEL) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.C, State.N, State.V, State.Z)
    val store = Elidable & (Not(ReadsC) & Linear | HasOpcodeIn(Set(RTS, JSR, RTI)))
    val secondReturn = (Elidable & HasOpcodeIn(Set(RTS, RTI) | NoopDiscardsFlags)).*.capture(6)
    val where = Where { ctx =>
      ctx.get[List[AssemblyLine]](4) == ctx.get[List[AssemblyLine]](5) ||
        ctx.get[List[AssemblyLine]](4) == ctx.get[List[AssemblyLine]](5) ++ ctx.get[List[AssemblyLine]](6)
    }
    val pattern =
      if (firstSet) test ~ ifSet ~ store.*.capture(4) ~ jump ~ elseLabel ~ ifClear ~ store.*.capture(5) ~ afterLabel ~ secondReturn ~ where
      else test ~ ifClear ~ store.*.capture(4) ~ jump ~ elseLabel ~ ifSet ~ store.*.capture(5) ~ afterLabel ~ secondReturn ~ where
    pattern ~~> { (_, ctx) =>
      List(
        AssemblyLine.immediate(LDA, 0),
        AssemblyLine.implied(if (shift >= 4) ROR else ROL)) ++
        (if (shift >= 4) List.fill(7 - shift)(AssemblyLine.implied(LSR)) else List.fill(shift)(AssemblyLine.implied(ASL))) ++
        (if (zeroIfSet) List(AssemblyLine.immediate(EOR, nonZero)) else Nil) ++
        ctx.get[List[AssemblyLine]](5) ++
        ctx.get[List[AssemblyLine]](6)
    }
  }

  val CarryFlagConversion = new RuleBasedAssemblyOptimization("Carry flag conversion",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    // TODO: These yield 2 cycles more but 1–2 bytes less
    // TODO: Add an "optimize for size" compilation option?
    //    carryFlagConversionCase(2, firstSet = false, zeroIfSet = false),
    //    carryFlagConversionCase(2, firstSet = true, zeroIfSet = false),
    //    carryFlagConversionCase(1, firstSet = true, zeroIfSet = true),
    //    carryFlagConversionCase(1, firstSet = false, zeroIfSet = true),
    carryFlagConversionCase(1, firstSet = false, zeroIfSet = false),
    carryFlagConversionCase(1, firstSet = true, zeroIfSet = false),
    carryFlagConversionCase(0, firstSet = true, zeroIfSet = true),
    carryFlagConversionCase(0, firstSet = false, zeroIfSet = true),
    carryFlagConversionCase(0, firstSet = false, zeroIfSet = false),
    carryFlagConversionCase(0, firstSet = true, zeroIfSet = false),
    //    carryFlagConversionCase(5, firstSet = false, zeroIfSet = false),
    //    carryFlagConversionCase(5, firstSet = true, zeroIfSet = false),
    //    carryFlagConversionCase(6, firstSet = true, zeroIfSet = true),
    //    carryFlagConversionCase(6, firstSet = false, zeroIfSet = true),
    carryFlagConversionCase(6, firstSet = false, zeroIfSet = false),
    carryFlagConversionCase(6, firstSet = true, zeroIfSet = false),
    carryFlagConversionCase(7, firstSet = true, zeroIfSet = true),
    carryFlagConversionCase(7, firstSet = false, zeroIfSet = true),
    carryFlagConversionCase(7, firstSet = false, zeroIfSet = false),
    carryFlagConversionCase(7, firstSet = true, zeroIfSet = false),
  )

  val Adc0Optimization = new RuleBasedAssemblyOptimization("ADC #0/#1 optimization",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    (Elidable & HasOpcode(LDA) & HasImmediate(0) & HasClear(State.D)) ~
      (Elidable & HasOpcode(ADC) & MatchAddrMode(1) & MatchParameter(2) & HasAddrModeIn(Set(ZeroPage, ZeroPageX, Absolute, AbsoluteX))) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(1) & MatchParameter(2) & DoesntMatterWhatItDoesWith(State.A, State.C, State.Z, State.N, State.V)) ~~> { code =>
      val label = getNextLabel("ah")
      List(
        AssemblyLine.relative(BCC, label),
        code.last.copy(opcode = INC),
        AssemblyLine.label(label))
    },
    (Elidable & HasOpcode(LDA) & MatchAddrMode(1) & MatchParameter(2) & HasAddrModeIn(Set(ZeroPage, ZeroPageX, Absolute, AbsoluteX))) ~
      (Elidable & HasOpcode(ADC) & HasImmediate(0) & HasClear(State.D)) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(1) & MatchParameter(2) & DoesntMatterWhatItDoesWith(State.A, State.C, State.Z, State.N, State.V)) ~~> { code =>
      val label = getNextLabel("ah")
      List(
        AssemblyLine.relative(BCC, label),
        code.last.copy(opcode = INC),
        AssemblyLine.label(label))
    },
    (Elidable & HasOpcode(LDA) & HasImmediate(1) & HasClear(State.D) & HasClear(State.C)) ~
      (Elidable & HasOpcode(ADC) & MatchAddrMode(1) & MatchParameter(2) & HasAddrModeIn(Set(ZeroPage, ZeroPageX, Absolute, AbsoluteX))) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(1) & MatchParameter(2) & DoesntMatterWhatItDoesWith(State.A, State.C, State.Z, State.N, State.V)) ~~> { code =>
      List(code.last.copy(opcode = INC))
    },
    (Elidable & HasOpcode(LDA) & MatchAddrMode(1) & HasClear(State.C) & MatchParameter(2) & HasAddrModeIn(Set(ZeroPage, ZeroPageX, Absolute, AbsoluteX))) ~
      (Elidable & HasOpcode(ADC) & HasImmediate(1) & HasClear(State.D)) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(1) & MatchParameter(2) & DoesntMatterWhatItDoesWith(State.A, State.C, State.Z, State.N, State.V)) ~~> { code =>
      List(code.last.copy(opcode = INC))
    },
    (Elidable & HasOpcode(TXA) & HasClear(State.D)) ~
      (Elidable & HasOpcode(ADC) & HasImmediate(0)) ~
      (Elidable & HasOpcode(TAX) & DoesntMatterWhatItDoesWith(State.A, State.C, State.Z, State.N, State.V)) ~~> { code =>
      val label = getNextLabel("ah")
      List(
        AssemblyLine.relative(BCC, label),
        AssemblyLine.implied(INX),
        AssemblyLine.label(label))
    },
    (Elidable & HasOpcode(TYA) & HasClear(State.D)) ~
      (Elidable & HasOpcode(ADC) & HasImmediate(0)) ~
      (Elidable & HasOpcode(TAY) & DoesntMatterWhatItDoesWith(State.A, State.C, State.Z, State.N, State.V)) ~~> { code =>
      val label = getNextLabel("ah")
      List(
        AssemblyLine.relative(BCC, label),
        AssemblyLine.implied(INY),
        AssemblyLine.label(label))
    },
    (Elidable & HasOpcode(TXA) & HasClear(State.D) & HasClear(State.C)) ~
      (Elidable & HasOpcode(ADC) & HasImmediate(1)) ~
      (Elidable & HasOpcode(TAX) & DoesntMatterWhatItDoesWith(State.A, State.C, State.Z, State.N, State.V)) ~~> { code =>
      List(AssemblyLine.implied(INX))
    },
    (Elidable & HasOpcode(TYA) & HasClear(State.D) & HasClear(State.C)) ~
      (Elidable & HasOpcode(ADC) & HasImmediate(1)) ~
      (Elidable & HasOpcode(TAY) & DoesntMatterWhatItDoesWith(State.A, State.C, State.Z, State.N, State.V)) ~~> { code =>
      List(AssemblyLine.implied(INY))
    },
  )

  val IndexSequenceOptimization = new RuleBasedAssemblyOptimization("Index sequence optimization",
    needsFlowInfo = FlowInfoRequirement.ForwardFlow,
    (Elidable & HasOpcode(LDY) & MatchImmediate(1) & MatchY(0)) ~
      Where(ctx => ctx.get[Constant](1).quickSimplify.isLowestByteAlwaysEqual(ctx.get[Int](0))) ~~> (_ => Nil),
    (Elidable & HasOpcode(LDY) & MatchImmediate(1) & MatchY(0)) ~
      Where(ctx => ctx.get[Constant](1).quickSimplify.isLowestByteAlwaysEqual(ctx.get[Int](0)+1)) ~~> (_ => List(AssemblyLine.implied(INY))),
    (Elidable & HasOpcode(LDY) & MatchImmediate(1) & MatchY(0)) ~
      Where(ctx => ctx.get[Constant](1).quickSimplify.isLowestByteAlwaysEqual(ctx.get[Int](0)-1)) ~~> (_ => List(AssemblyLine.implied(DEY))),
    (Elidable & HasOpcode(LDX) & MatchImmediate(1) & MatchX(0)) ~
      Where(ctx => ctx.get[Constant](1).quickSimplify.isLowestByteAlwaysEqual(ctx.get[Int](0))) ~~> (_ => Nil),
    (Elidable & HasOpcode(LDX) & MatchImmediate(1) & MatchX(0)) ~
      Where(ctx => ctx.get[Constant](1).quickSimplify.isLowestByteAlwaysEqual(ctx.get[Int](0)+1)) ~~> (_ => List(AssemblyLine.implied(INX))),
    (Elidable & HasOpcode(LDX) & MatchImmediate(1) & MatchX(0)) ~
      Where(ctx => ctx.get[Constant](1).quickSimplify.isLowestByteAlwaysEqual(ctx.get[Int](0)-1)) ~~> (_ => List(AssemblyLine.implied(DEX))),
  )

  val CommonBranchBodyOptimization = new RuleBasedAssemblyOptimization("Common branch body optimization",
    needsFlowInfo = FlowInfoRequirement.JustLabels,
    (Elidable & Linear & MatchOpcode(3) & MatchAddrMode(4) & MatchParameter(5)).capture(1) ~
      (
        (HasOpcode(JMP) & MatchParameter(2)) ~ Not(MatchOpcode(3)).*
        ).capture(11) ~
      (Elidable & Linear & MatchOpcode(3) & MatchAddrMode(4) & MatchParameter(5)) ~
      (HasOpcode(LABEL) & MatchParameter(2) & HasCallerCount(1)).capture(12) ~~> { (code, ctx) =>
      ctx.get[List[AssemblyLine]](11) ++ ctx.get[List[AssemblyLine]](12) :+ code.head
    }
  )


  val UnusedLabelRemoval = new RuleBasedAssemblyOptimization("Unused label removal",
    needsFlowInfo = FlowInfoRequirement.JustLabels,
    (Elidable & HasOpcode(LABEL) & HasCallerCount(0)) ~~> (_ => Nil)
  )

}
