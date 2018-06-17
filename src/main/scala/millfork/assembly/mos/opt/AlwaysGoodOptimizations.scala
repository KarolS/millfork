package millfork.assembly.mos.opt

import java.util.concurrent.atomic.AtomicInteger

import millfork.assembly.mos.Opcode._
import millfork.assembly.mos.OpcodeClasses._
import millfork.assembly.mos.{AddrMode, opt, _}
import millfork.assembly.mos.AddrMode._
import millfork.env._

/**
  * These optimizations should not remove opportunities for more complex optimizations to trigger.
  *
  * @author Karol Stasiak
  */
//noinspection ZeroIndexToHead
object AlwaysGoodOptimizations {

  val counter = new AtomicInteger(30000)
  val LdxAddrModes = Set(Immediate, Absolute, ZeroPage, ZeroPageY, AbsoluteY)
  val LdyAddrModes = Set(Immediate, Absolute, ZeroPage, ZeroPageX, AbsoluteX)

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
      AssemblyLine.immediate(LDA, (ctx.get[Int](0) << 1) & 0xff) :: Nil
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
      AssemblyLine.immediate(LDA, (ctx.get[Int](0) * 2 + 1) & 0xff) :: Nil
    },
    (Elidable & MatchA(0) &
      HasSet(State.C) & HasOpcode(ROR) & HasAddrMode(Implied) & DoesntMatterWhatItDoesWith(State.C)) ~~> { (code, ctx) =>
      AssemblyLine.immediate(LDA, 0x80 + (ctx.get[Int](0) & 0xff) / 2) :: Nil
    },
    (Elidable &
      MatchA(0) & MatchParameter(1) &
      HasOpcode(ADC) & HasAddrMode(Immediate) &
      HasClear(State.D) & HasClear(State.C) & DoesntMatterWhatItDoesWith(State.C, State.V)) ~~> { (code, ctx) =>
      AssemblyLine.immediate(LDA, (ctx.get[Constant](1) + ctx.get[Int](0)).quickSimplify.loByte) :: Nil
    },
    (Elidable &
      MatchA(0) & MatchParameter(1) &
      HasOpcode(ADC) & HasAddrMode(Immediate) &
      HasClear(State.D) & HasClear(State.C) & DoesntMatterWhatItDoesWith(State.V)) ~
      Where(ctx => (ctx.get[Constant](1) + ctx.get[Int](0)).quickSimplify match {
        case NumericConstant(x, _) => x == (x & 0xff)
        case _ => false
      }) ~~> { (code, ctx) =>
      AssemblyLine.immediate(LDA, (ctx.get[Constant](1) + ctx.get[Int](0)).quickSimplify.loByte) :: Nil
    },
    (Elidable &
      MatchA(0) & MatchParameter(1) &
      HasOpcode(ADC) & HasAddrMode(Immediate) &
      HasClear(State.D) & HasSet(State.C) & DoesntMatterWhatItDoesWith(State.C, State.V)) ~~> { (code, ctx) =>
      AssemblyLine.immediate(LDA, (ctx.get[Constant](1) + (ctx.get[Int](0) + 1)).quickSimplify.loByte) :: Nil
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
    (Elidable & HasA(0) & HasOpcodeIn(Set(ORA, EOR))) ~~> (code => code.map(_.copy(opcode = LDA))),
    (Elidable & HasA(0) & HasOpcode(ADC) &
      HasClear(State.D) & HasClear(State.C) &
      // C stays cleared!
      DoesntMatterWhatItDoesWith(State.V)) ~~> (code => code.map(_.copy(opcode = LDA))),
    (Elidable & HasA(0) & HasOpcode(ADC) &
      HasClear(State.C) &
      // C stays cleared!
      DoesntMatterWhatItDoesWith(State.N, State.Z, State.V)) ~~> (code => code.map(_.copy(opcode = LDA))),
    (Elidable & HasA(0xff) & HasOpcode(AND)) ~~> (code => code.map(_.copy(opcode = LDA))),
    (Elidable & HasA(0) & HasOpcode(AND)) ~~> (code => List(AssemblyLine.immediate(LDA, 0))),
    (Elidable & HasX(0) & HasOpcode(XAA)) ~~> (code => List(AssemblyLine.immediate(LDA, 0))),
    (Elidable & HasImmediate(0) & HasOpcode(AND)) ~~> (code => List(AssemblyLine.immediate(LDA, 0))),
    (Elidable & HasImmediate(0) & HasOpcode(XAA)) ~~> (code => List(AssemblyLine.immediate(LDA, 0))),
    (Elidable & HasImmediate(0xff) & HasOpcode(ORA)) ~~> (code => List(AssemblyLine.immediate(LDA, 0xff))),
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

  private def pointlessNonimmediateStoreToTheSameVariable(ld1: Set[Opcode.Value], st1: Opcode.Value, ld2: Opcode.Value, st2: Opcode.Value) = {
    (HasOpcodeIn(ld1) & Not(HasAddrMode(Immediate)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (HasOpcode(st1) & MatchAddrMode(2) & MatchParameter(3)) ~
      (Linear & DoesntChangeIndexingInAddrMode(0) & DoesntChangeIndexingInAddrMode(2) &
        DoesNotConcernMemoryAt(0, 1) & DoesntChangeMemoryAt(2, 3)).* ~
      (Elidable & HasOpcode(ld2) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(st2) & MatchAddrMode(2) & MatchParameter(3)) ~~> (code => code.init)
  }

  private def pointlessImmediateStoreToTheSameVariable(match1: AssemblyLinePattern, st1: Opcode.Value, match2: AssemblyLinePattern, st2: Opcode.Value) = {
      (HasOpcode(st1) & MatchAddrMode(2) & MatchParameter(3) & match1) ~
      (Linear & DoesntChangeIndexingInAddrMode(2) & (
        DoesntChangeMemoryAt(2, 3) |
          HasAddrModeIn(Set(IndexedX, IndexedY, IndexedZ, LongIndexedY, LongIndexedZ, Indirect)) &
          MatchParameter(3) & HasParameterWhere({
            case MemoryAddressConstant(th) => th.name.startsWith("__")
            case _ => false
          })
        )).* ~
      (Elidable & match2 & HasOpcode(st2) & MatchAddrMode(2) & MatchParameter(3)) ~~> (code => code.init),
  }

  val PointlessStoreToTheSameVariable = new RuleBasedAssemblyOptimization("Pointless store to the same variable",
    needsFlowInfo = FlowInfoRequirement.ForwardFlow,

    pointlessNonimmediateStoreToTheSameVariable(Set(LDA, LAX), STA, LDA, STA),
    pointlessNonimmediateStoreToTheSameVariable(Set(LDX, LAX), STX, LDA, STA),
    pointlessNonimmediateStoreToTheSameVariable(Set(LDY), STY, LDA, STA),

    pointlessNonimmediateStoreToTheSameVariable(Set(LDA, LAX), STA, LDX, STX),
    pointlessNonimmediateStoreToTheSameVariable(Set(LDX, LAX), STX, LDX, STX),
    pointlessNonimmediateStoreToTheSameVariable(Set(LDY), STY, LDX, STX),

    pointlessNonimmediateStoreToTheSameVariable(Set(LDA, LAX), STA, LDY, STY),
    pointlessNonimmediateStoreToTheSameVariable(Set(LDX, LAX), STX, LDY, STY),
    pointlessNonimmediateStoreToTheSameVariable(Set(LDY), STY, LDY, STY),

    pointlessImmediateStoreToTheSameVariable(MatchA(0), STA, MatchA(0), STA),
    pointlessImmediateStoreToTheSameVariable(MatchX(0), STX, MatchA(0), STA),
    pointlessImmediateStoreToTheSameVariable(MatchY(0), STY, MatchA(0), STA),

    pointlessImmediateStoreToTheSameVariable(MatchA(0), STA, MatchX(0), STX),
    pointlessImmediateStoreToTheSameVariable(MatchX(0), STX, MatchX(0), STX),
    pointlessImmediateStoreToTheSameVariable(MatchY(0), STY, MatchX(0), STX),

    pointlessImmediateStoreToTheSameVariable(MatchA(0), STA, MatchY(0), STY),
    pointlessImmediateStoreToTheSameVariable(MatchX(0), STX, MatchY(0), STY),
    pointlessImmediateStoreToTheSameVariable(MatchY(0), STY, MatchY(0), STY),

  )

  val PointlessStashingForLaterStore = new RuleBasedAssemblyOptimization("Pointless stashing for later store",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    // LDA/TAX/TAY/TAZ will be cleaned by something else
    (HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & Linear & DoesNotConcernMemoryAt(0, 1) & DoesntChangeIndexingInAddrMode(0)).*.capture(5) ~
      (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(STA) & HasAddrModeIn(Set(ZeroPage, Absolute, LongAbsolute)) & DoesNotConcernMemoryAt(0, 1)).capture(4) ~
      Where(ctx => {
        val sta = ctx.get[List[AssemblyLine]](4).head
        val middleCode = ctx.get[List[AssemblyLine]](5)
        middleCode.forall(line => HelperCheckers.memoryAccessDoesntOverlap(line, sta))
      }) ~~> { code =>
      code.last :: code.init
    },
    (HasOpcode(TAX)) ~
      (Elidable & Linear & Not(ChangesX) & Not(HasOpcode(STX))).*.capture(5) ~
      (Elidable & HasOpcode(STX) & HasAddrModeIn(Set(ZeroPage, Absolute, LongAbsolute))).capture(4) ~
      Where(ctx => {
        val sta = ctx.get[List[AssemblyLine]](4).head
        val middleCode = ctx.get[List[AssemblyLine]](5)
        middleCode.forall(line => HelperCheckers.memoryAccessDoesntOverlap(line, sta))
      }) ~~> { code =>
      code.last.copy(opcode = STA) :: code.init
    },
    (HasOpcode(TAY)) ~
      (Elidable & Linear & Not(ChangesY) & Not(HasOpcode(STY))).*.capture(5) ~
      (Elidable & HasOpcode(STY) & HasAddrModeIn(Set(ZeroPage, Absolute, LongAbsolute))).capture(4) ~
      Where(ctx => {
        val sta = ctx.get[List[AssemblyLine]](4).head
        val middleCode = ctx.get[List[AssemblyLine]](5)
        middleCode.forall(line => HelperCheckers.memoryAccessDoesntOverlap(line, sta))
      }) ~~> { code =>
      code.last.copy(opcode = STA) :: code.init
    },
    (HasOpcode(TAZ)) ~
      (Elidable & Linear & Not(ChangesIZ) & Not(HasOpcode(STZ))).*.capture(5) ~
      (Elidable & HasOpcode(STZ) & HasAddrModeIn(Set(ZeroPage, Absolute, LongAbsolute))).capture(4) ~
      Where(ctx => {
        val sta = ctx.get[List[AssemblyLine]](4).head
        val middleCode = ctx.get[List[AssemblyLine]](5)
        middleCode.forall(line => HelperCheckers.memoryAccessDoesntOverlap(line, sta))
      }) ~~> { code =>
      code.last.copy(opcode = STA) :: code.init
    },
  )

  val PointlessStashingForLaterLoad = new RuleBasedAssemblyOptimization("Pointless stashing for later load",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~
    (Elidable & HasOpcode(STA) & MatchAddrMode(10) & MatchParameter(11) & DoesntMatterWhatItDoesWith(State.A, State.Z, State.N)) ~
      (Elidable & Linear & DoesNotConcernMemoryAt(0, 1) & DoesNotConcernMemoryAt(10, 11) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeIndexingInAddrMode(10)).* ~
      (Elidable & HasOpcode(LDA) & MatchAddrMode(10) & MatchParameter(11)) ~~> { code =>
      code.drop(2).init ++ code.take(2)
    },
    (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1)) ~
    (Elidable & HasOpcode(STA) & MatchAddrMode(10) & MatchParameter(11) & DoesntMatterWhatItDoesWith(State.A, State.Z, State.N)) ~
      (Elidable & Linear & DoesNotConcernMemoryAt(0, 1) & DoesNotConcernMemoryAt(10, 11) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeIndexingInAddrMode(10)).* ~
      (Elidable & HasOpcode(LDA) & MatchAddrMode(10) & MatchParameter(11)) ~~> { code =>
      code.head :: (code.drop(2).init ++ List(code.head.copy(opcode = LDA), code(1)))
    },

    (Elidable & HasOpcode(LDX) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~
      (Elidable & Linear & DoesNotConcernMemoryAt(0, 1) & DoesntChangeIndexingInAddrMode(0) & Not(ConcernsX)).* ~
      (Elidable & HasOpcode(STX)) ~
      (Elidable & HasOpcode(TXA)) ~~> { code =>
      code.tail.init.init ++ List(code.head.copy(opcode = LDA), code.init.last.copy(opcode = STA), AssemblyLine.implied(TAX))
    },
    (Elidable & HasOpcode(LDY) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~
      (Elidable & Linear & DoesNotConcernMemoryAt(0, 1) & DoesntChangeIndexingInAddrMode(0) & Not(ConcernsY)).* ~
      (Elidable & HasOpcode(STY)) ~
      (Elidable & HasOpcode(TYA)) ~~> { code =>
      code.tail.init.init ++ List(code.head.copy(opcode = LDA), code.init.last.copy(opcode = STA), AssemblyLine.implied(TAY))
    },
    (Elidable & HasOpcode(LDZ) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~
      (Elidable & Linear & DoesNotConcernMemoryAt(0, 1) & DoesntChangeIndexingInAddrMode(0) & Not(ConcernsIZ)).* ~
      (Elidable & HasOpcode(STZ)) ~
      (Elidable & HasOpcode(TZA)) ~~> { code =>
      code.tail.init.init ++ List(code.head.copy(opcode = LDA), code.init.last.copy(opcode = STA), AssemblyLine.implied(TAZ))
    },

    (Elidable & HasOpcode(LDX) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~
      (Elidable & Linear & DoesNotConcernMemoryAt(0, 1) & DoesntChangeIndexingInAddrMode(0) & Not(ConcernsX)).* ~
      (Elidable & HasOpcode(TXA)) ~~> { code =>
      code.tail.init ++ List(code.head.copy(opcode = LDA), AssemblyLine.implied(TAX))
    },
    (Elidable & HasOpcode(LDY) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~
      (Elidable & Linear & DoesNotConcernMemoryAt(0, 1) & DoesntChangeIndexingInAddrMode(0) & Not(ConcernsY)).* ~
      (Elidable & HasOpcode(TYA)) ~~> { code =>
      code.tail.init ++ List(code.head.copy(opcode = LDA), AssemblyLine.implied(TAY))
    },
    (Elidable & HasOpcode(LDZ) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~
      (Elidable & Linear & DoesNotConcernMemoryAt(0, 1) & DoesntChangeIndexingInAddrMode(0) & Not(ConcernsIZ)).* ~
      (Elidable & HasOpcode(TZA)) ~~> { code =>
      code.tail.init ++ List(code.head.copy(opcode = LDA), AssemblyLine.implied(TAZ))
    },
  )

  val PointlessLoadBeforeReturn = new RuleBasedAssemblyOptimization("Pointless load before return",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (Set(LDA, TXA, TYA, EOR, AND, ORA, ANC) & Elidable) ~ (LinearOrLabel & Not(ConcernsA) & Not(ReadsNOrZ) & Not(HasOpcode(DISCARD_AF))).* ~ HasOpcode(DISCARD_AF) ~~> (_.tail),
    (Set(LDX, TAX, TSX, INX, DEX) & Elidable) ~ (LinearOrLabel & Not(ConcernsX) & Not(ReadsNOrZ) & Not(HasOpcode(DISCARD_XF))).* ~ HasOpcode(DISCARD_XF) ~~> (_.tail),
    (Set(LDY, TAY, INY, DEY) & Elidable) ~ (LinearOrLabel & Not(ConcernsY) & Not(ReadsNOrZ) & Not(HasOpcode(DISCARD_YF))).* ~ HasOpcode(DISCARD_YF) ~~> (_.tail),
    (HasOpcode(LDX) & Elidable & MatchAddrMode(3) & MatchParameter(4)) ~
      (LinearOrLabel & Not(ConcernsX) & Not(ReadsNOrZ) & DoesntChangeMemoryAt(3, 4) & DoesntChangeIndexingInAddrMode(3)).*.capture(1) ~
      (HasOpcode(TXA) & Elidable) ~
      ((LinearOrLabel & Not(ConcernsX) & Not(HasOpcode(DISCARD_XF))).* ~
        HasOpcode(DISCARD_XF)).capture(2) ~~> { (c, ctx) =>
      ctx.get[List[AssemblyLine]](1) ++ (c.head.copy(opcode = LDA) :: ctx.get[List[AssemblyLine]](2))
    },
    (HasOpcode(LDY) & Elidable & MatchAddrMode(3) & MatchParameter(4)) ~
      (LinearOrLabel & Not(ConcernsY) & Not(ReadsNOrZ) & DoesntChangeMemoryAt(3, 4) & DoesntChangeIndexingInAddrMode(3)).*.capture(1) ~
      (HasOpcode(TYA) & Elidable) ~
      ((LinearOrLabel & Not(ConcernsY) & Not(HasOpcode(DISCARD_YF))).* ~
        HasOpcode(DISCARD_YF)).capture(2) ~~> { (c, ctx) =>
      ctx.get[List[AssemblyLine]](1) ++ (c.head.copy(opcode = LDA) :: ctx.get[List[AssemblyLine]](2))
    },
  )

  private def operationPairBuilder(op1: Opcode.Value, op2: Opcode.Value, middle: AssemblyLinePattern, discardToRemove: Option[Opcode.Value]) = {
    (HasOpcode(op1) & Elidable) ~
      (Linear & middle).*.capture(1) ~
      (HasOpcode(op2) & Elidable) ~
      ((LinearOrLabel & Not(ReadsNOrZ) & Not(ChangesNAndZ)).* ~ ChangesNAndZ).capture(2) ~~> { (_, ctx) =>
      ctx.get[List[AssemblyLine]](1).filter(l => !discardToRemove.contains(l.opcode)) ++ ctx.get[List[AssemblyLine]](2)
    }
  }

  val PointlessOperationPairRemoval = new RuleBasedAssemblyOptimization("Pointless operation pair",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    operationPairBuilder(PHA, PLA, Not(ChangesA) & Not(ConcernsStack), Some(DISCARD_AF)),
    operationPairBuilder(PHX, PLX, Not(ChangesX) & Not(ConcernsStack), Some(DISCARD_XF)),
    operationPairBuilder(PHY, PLY, Not(ChangesY) & Not(ConcernsStack), Some(DISCARD_YF)),
    operationPairBuilder(PHZ, PLZ, Not(ChangesIZ) & Not(ConcernsStack), Some(DISCARD_YF)),
    operationPairBuilder(INX, DEX, Not(ConcernsX) & Not(ReadsNOrZ), None),
    operationPairBuilder(DEX, INX, Not(ConcernsX) & Not(ReadsNOrZ), None),
    operationPairBuilder(INY, DEY, Not(ConcernsX) & Not(ReadsNOrZ), None),
    operationPairBuilder(DEY, INY, Not(ConcernsX) & Not(ReadsNOrZ), None),
  )


  private def operationPairBuilder2(op1: Opcode.Value, op1extra: AssemblyLinePattern, middle: AssemblyLinePattern,op2: Opcode.Value,  op2extra: AssemblyLinePattern, discardToRemove: Option[Opcode.Value]) = {
    (HasOpcode(op1) & Elidable & op1extra) ~
      (Linear & middle).*.capture(1) ~
      (HasOpcode(op2) & Elidable & op2extra) ~~> { (_, ctx) =>
      ctx.get[List[AssemblyLine]](1).filter(l => !discardToRemove.contains(l.opcode))
    }
  }

  private def operationPairBuilder3(op1: Opcode.Value, op1extra: AssemblyLinePattern, op2: Opcode.Value, middle: AssemblyLinePattern, discardToRemove: Option[Opcode.Value]) = {
    (HasOpcode(op1) & Elidable & op1extra) ~
      middle.*.capture(1) ~
      Where(_.isExternallyLinearBlock(1)) ~
      (HasOpcode(op2) & Elidable) ~~> { (_, ctx) =>
      ctx.get[List[AssemblyLine]](1).filter(l => !discardToRemove.contains(l.opcode))
    }
  }

  private def operationPairBuilder4(op1: Opcode.Value, op1extra: AssemblyLinePattern, middle: AssemblyLinePattern, op2: Opcode.Value, op2extra: AssemblyLinePattern) = {
    (HasOpcode(op1) & op1extra  & Elidable & HasAddrModeIn(Set(Absolute, ZeroPage, LongAbsolute)) & MatchParameter(3)) ~
      middle.*.capture(1) ~
      Where(_.isExternallyLinearBlock(1)) ~
      (HasOpcode(op2) & op2extra & Elidable & HasAddrModeIn(Set(Absolute, ZeroPage, LongAbsolute)) & MatchParameter(3)) ~~> { (_, ctx) =>
      ctx.get[List[AssemblyLine]](1)
    }
  }

  val PointlessOperationPairRemoval2 = new RuleBasedAssemblyOptimization("Pointless operation pair 2",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    operationPairBuilder2(
      PHA, Anything,
      Not(ConcernsStack),
      PLA, DoesntMatterWhatItDoesWith(State.A, State.N, State.Z), Some(DISCARD_AF)),
    operationPairBuilder2(
      PHX, Anything,
      Not(ConcernsStack),
      PLX, DoesntMatterWhatItDoesWith(State.X, State.N, State.Z), Some(DISCARD_XF)),
    operationPairBuilder2(
      PHY, Anything,
      Not(ConcernsStack),
      PLY, DoesntMatterWhatItDoesWith(State.Y, State.N, State.Z), Some(DISCARD_YF)),
    operationPairBuilder2(
      INX, DoesntMatterWhatItDoesWith(State.X, State.N, State.Z),
      Anything,
      DEX, DoesntMatterWhatItDoesWith(State.X, State.N, State.Z), None),
    operationPairBuilder2(
      DEX, DoesntMatterWhatItDoesWith(State.X, State.N, State.Z),
      Anything,
      INX, DoesntMatterWhatItDoesWith(State.X, State.N, State.Z), None),
    operationPairBuilder2(
      INY, DoesntMatterWhatItDoesWith(State.Y, State.N, State.Z),
      Anything,
      DEY, DoesntMatterWhatItDoesWith(State.Y, State.N, State.Z), None),
    operationPairBuilder2(
      DEY, DoesntMatterWhatItDoesWith(State.Y, State.N, State.Z),
      Anything,
      INY, DoesntMatterWhatItDoesWith(State.Y, State.N, State.Z), None),
    operationPairBuilder3(PHA, Anything, PLA, Not(ChangesA) & Not(ConcernsStack), Some(DISCARD_AF)),
    operationPairBuilder3(PHX, Anything, PLX, Not(ChangesX) & Not(ConcernsStack), Some(DISCARD_XF)),
    operationPairBuilder3(PHY, Anything, PLY, Not(ChangesY) & Not(ConcernsStack), Some(DISCARD_YF)),
    operationPairBuilder3(PHZ, Anything, PLZ, Not(ChangesIZ) & Not(ConcernsStack), Some(DISCARD_YF)),
    operationPairBuilder3(PHD, Anything, PLD, Not(ChangesDirectPageRegister), None),
    operationPairBuilder3(PHB, Anything, PLB, Not(ChangesDataBankRegister), None),
    operationPairBuilder3(INX, DoesntMatterWhatItDoesWith(State.N, State.Z), DEX, Not(ConcernsX) & Not(ReadsNOrZ), None),
    operationPairBuilder3(DEX, DoesntMatterWhatItDoesWith(State.N, State.Z), INX, Not(ConcernsX) & Not(ReadsNOrZ), None),
    operationPairBuilder3(INY, DoesntMatterWhatItDoesWith(State.N, State.Z), DEY, Not(ConcernsX) & Not(ReadsNOrZ), None),
    operationPairBuilder3(DEY, DoesntMatterWhatItDoesWith(State.N, State.Z), INY, Not(ConcernsX) & Not(ReadsNOrZ), None),
    operationPairBuilder4(
      LDA, DoesntMatterWhatItDoesWith(State.N, State.Z),
      Not(ConcernsA),
      STA, DoesntMatterWhatItDoesWith(State.A)),
    operationPairBuilder4(
      LDX, DoesntMatterWhatItDoesWith(State.N, State.Z),
      Not(ConcernsX),
      STX, DoesntMatterWhatItDoesWith(State.X)),
    operationPairBuilder4(
      LDY, DoesntMatterWhatItDoesWith(State.N, State.Z),
      Not(ConcernsY),
      STY, DoesntMatterWhatItDoesWith(State.Y)),
    operationPairBuilder4(
      LAX, DoesntMatterWhatItDoesWith(State.N, State.Z),
      Not(ConcernsX) & Not(ConcernsA),
      STA, DoesntMatterWhatItDoesWith(State.A, State.X)),
    operationPairBuilder4(
      LAX, DoesntMatterWhatItDoesWith(State.N, State.Z),
      Not(ConcernsX) & Not(ConcernsA),
      STX, DoesntMatterWhatItDoesWith(State.A, State.X)),
    operationPairBuilder4(
      LAX, DoesntMatterWhatItDoesWith(State.N, State.Z),
      Not(ConcernsX) & Not(ConcernsA),
      SAX, DoesntMatterWhatItDoesWith(State.A, State.X)),
  )

  val PointlessStackStashing = new RuleBasedAssemblyOptimization("Pointless stack stashing",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (Elidable & HasOpcode(LDA) & HasAddrMode(Immediate)) ~
      (Elidable & HasOpcode(PHA)) ~
      (Linear & Not(ConcernsStack) | HasOpcodeIn(Set(JSR, BSR))).* ~
      (Elidable & HasOpcode(PLA)) ~~> { code =>
      code.head :: (code.drop(2).init :+ code.head)
    },
    (Elidable & HasOpcode(LDX) & HasAddrMode(Immediate)) ~
      (Elidable & HasOpcode(PHX)) ~
      (Linear & Not(ConcernsStack) | HasOpcodeIn(Set(JSR, BSR))).* ~
      (Elidable & HasOpcode(PLX)) ~~> { code =>
      code.head :: (code.drop(2).init :+ code.head)
    },
    (Elidable & HasOpcode(LDY) & HasAddrMode(Immediate)) ~
      (Elidable & HasOpcode(PHY)) ~
      (Linear & Not(ConcernsStack) | HasOpcodeIn(Set(JSR, BSR))).* ~
      (Elidable & HasOpcode(PLY)) ~~> { code =>
      code.head :: (code.drop(2).init :+ code.head)
    },
    (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(PHA)) ~
      (Linear & Not(ConcernsStack) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(PLA)) ~~> { code =>
      code.head :: (code.drop(2).init :+ code.head)
    },
    (Elidable & HasOpcode(LDX) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(PHX)) ~
      (Linear & Not(ConcernsStack) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(PLX)) ~~> { code =>
      code.head :: (code.drop(2).init :+ code.head)
    },
    (Elidable & HasOpcode(LDY) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(PHY)) ~
      (Linear & Not(ConcernsStack) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(PLY)) ~~> { code =>
      code.head :: (code.drop(2).init :+ code.head)
    },
  )

  val IncrementingIndexRegistersAfterTransfer = new RuleBasedAssemblyOptimization("Incrementing index registers after transfer",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    (Elidable & HasOpcode(CLC) & HasClear(State.D)) ~
      (Elidable & HasOpcode(ADC) & HasImmediate(1)) ~
      (Elidable & HasOpcode(TAY) & DoesntMatterWhatItDoesWith(State.A, State.C)) ~~> { code =>
      List(AssemblyLine.implied(TAY), AssemblyLine.implied(INY))
    },
    (Elidable & HasOpcode(SEC) & HasClear(State.D)) ~
      (Elidable & HasOpcode(SBC) & HasImmediate(1)) ~
      (Elidable & HasOpcode(TAY) & DoesntMatterWhatItDoesWith(State.A, State.C)) ~~> { code =>
      List(AssemblyLine.implied(TAY), AssemblyLine.implied(DEY))
    },
    (Elidable & HasOpcode(CLC) & HasClear(State.D)) ~
      (Elidable & HasOpcode(ADC) & HasImmediate(1)) ~
      (Elidable & HasOpcode(TAX) & DoesntMatterWhatItDoesWith(State.A, State.C)) ~~> { code =>
      List(AssemblyLine.implied(TAX), AssemblyLine.implied(INX))
    },
    (Elidable & HasOpcode(SEC) & HasClear(State.D)) ~
      (Elidable & HasOpcode(SBC) & HasImmediate(1)) ~
      (Elidable & HasOpcode(TAX) & DoesntMatterWhatItDoesWith(State.A, State.C)) ~~> { code =>
      List(AssemblyLine.implied(TAX), AssemblyLine.implied(DEX))
    },
  )

  val BranchInPlaceRemoval = new RuleBasedAssemblyOptimization("Branch in place",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (AllDirectJumps & MatchParameter(0) & Elidable) ~
      HasOpcodeIn(NoopDiscardsFlags).* ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> (c => c.last :: Nil),
    (AllDirectJumps & MatchParameter(0) & Elidable) ~
      (HasOpcode(LABEL) & Not(MatchParameter(0))).* ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> (_.tail),
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
    (Elidable & HasOpcode(JSR)) ~ HasOpcodeIn(NoopDiscardsFlags).* ~ (Elidable & HasOpcode(RTS)) ~~> (c => c.head.copy(opcode = JMP) :: Nil),
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
    (OverwritesC & Elidable & Not(ChangesStack)) ~ (LinearOrLabel & Not(ReadsC) & Not(DiscardsC)).* ~ DiscardsC ~~> (_.tail),
    (OverwritesD & Elidable & Not(ChangesStack)) ~ (LinearOrLabel & Not(ReadsD) & Not(DiscardsD)).* ~ DiscardsD ~~> (_.tail),
    (OverwritesV & Elidable & Not(ChangesStack)) ~ (LinearOrLabel & Not(ReadsV) & Not(DiscardsV)).* ~ DiscardsV ~~> (_.tail)
  )

  // Optimizing Bxx to JMP is generally bad, but it may allow for better optimizations later
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
    val actualFlagsToTrashForStore = List(State.N, State.Z) ++ flagsToTrash ++ (store match{
      case STA => List(State.A)
      case STY => List(State.Y)
      case STX => List(State.X)
      case SAX => List(State.A, State.X)
      case STZ => Nil
    })
    val actualFlagsToTrashForModify = List(State.N, State.Z) ++ flagsToTrash
    val init = Elidable & HasOpcode(store) & HasAddrMode(addrMode) & MatchAddrMode(3) & MatchParameter(0) & DoesntMatterWhatItDoesWith(actualFlagsToTrashForStore: _*) & initExtra
    val meantime = (Linear & DoesNotConcernMemoryAt(3, 0) & meantimeExtra).*
    val oneModification = Elidable & HasOpcode(modify) & HasAddrMode(addrMode) & MatchParameter(0) & DoesntMatterWhatItDoesWith(actualFlagsToTrashForModify: _*)
    val modifications = (if (atLeastTwo) oneModification ~ oneModification.+ else oneModification.+).captureLength(1)
    if (alternateStore == LABEL) {
      ((init ~ meantime).capture(2) ~ modifications) ~~> ((code, ctx) => fix(ctx, ctx.get[Int](1)) ++ ctx.get[List[AssemblyLine]](2))
    } else {
      (init ~ meantime.capture(2) ~ modifications) ~~> { (code, ctx) =>
        fix(ctx, ctx.get[Int](1)) ++
          List(AssemblyLine(alternateStore, ctx.get[AddrMode.Value](3), ctx.get[Constant](0))) ++
          ctx.get[List[AssemblyLine]](2)
      }
    }
  }

  val ModificationOfJustWrittenValue = new RuleBasedAssemblyOptimization("Modification of just written value",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
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
    modificationOfJustWrittenValue(STA, Absolute, HasClear(State.D), INC, Anything, atLeastTwo = true, Seq(State.C, State.V), (_, i) => List(
      AssemblyLine.implied(CLC),
      AssemblyLine.immediate(ADC, i)
    )),
    modificationOfJustWrittenValue(STA, Absolute, HasClear(State.D), DEC, Anything, atLeastTwo = true, Seq(State.C, State.V), (_, i) => List(
      AssemblyLine.implied(SEC),
      AssemblyLine.immediate(SBC, i)
    )),
    modificationOfJustWrittenValue(STA, ZeroPage, HasClear(State.D), INC, Anything, atLeastTwo = true, Seq(State.C, State.V), (_, i) => List(
      AssemblyLine.implied(CLC),
      AssemblyLine.immediate(ADC, i)
    )),
    modificationOfJustWrittenValue(STA, ZeroPage, HasClear(State.D), DEC, Anything, atLeastTwo = true, Seq(State.C, State.V), (_, i) => List(
      AssemblyLine.implied(SEC),
      AssemblyLine.immediate(SBC, i)
    )),
    modificationOfJustWrittenValue(STA, AbsoluteX, HasClear(State.D), INC, Not(ChangesX), atLeastTwo = true, Seq(State.C, State.V), (_, i) => List(
      AssemblyLine.implied(CLC),
      AssemblyLine.immediate(ADC, i)
    )),
    modificationOfJustWrittenValue(STA, AbsoluteX, HasClear(State.D), DEC, Not(ChangesX), atLeastTwo = true, Seq(State.C, State.V), (_, i) => List(
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
    (MatchX(0) & HasAddrMode(AbsoluteX) & SupportsAbsolute & Elidable & HasParameterWhere({
      case MemoryAddressConstant(th) => th.name == "identity$"
      case _ => false
    })) ~~> { (code, ctx) =>
      code.map(l => l.copy(addrMode = Immediate, parameter = NumericConstant(ctx.get[Int](0), 1)))
    },
    (MatchY(0) & HasAddrMode(AbsoluteY) & SupportsAbsolute & Elidable & HasParameterWhere({
      case MemoryAddressConstant(th) => th.name == "identity$"
      case _ => false
    })) ~~> { (code, ctx) =>
      code.map(l => l.copy(addrMode = Immediate, parameter = NumericConstant(ctx.get[Int](0), 1)))
    },
    (MatchY(0) & HasAddrMode(AbsoluteY) & SupportsAbsolute & Elidable) ~~> { (code, ctx) =>
      code.map(l => l.copy(addrMode = Absolute, parameter = l.parameter + ctx.get[Int](0)))
    },
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
    HasOpcodeIn(Set(TXA, TAX)) ~
      (Linear & Not(ChangesNAndZ) & Not(ChangesA) & Not(ChangesX)).* ~
      (Elidable & HasOpcodeIn(Set(TXA, TAX))) ~~> (_.init),
    HasOpcodeIn(Set(TYA, TAY)) ~
      (Linear & Not(ChangesNAndZ) & Not(ChangesA) & Not(ChangesX)).* ~
      (Elidable & HasOpcodeIn(Set(TYA, TAY))) ~~> (_.init),
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

  val PointlessRegisterTransfersBeforeCompare = new RuleBasedAssemblyOptimization("Pointless register transfers and loads before compare",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    HasOpcodeIn(Set(DEX, INX, LDX, LAX)) ~
      (HasOpcode(TXA) & Elidable & DoesntMatterWhatItDoesWith(State.A)) ~~> (code => code.init),
    HasOpcodeIn(Set(DEY, INY, LDY)) ~
      (HasOpcode(TYA) & Elidable & DoesntMatterWhatItDoesWith(State.A)) ~~> (code => code.init),
    (HasOpcodeIn(Set(DEC, INC, ASL, ROL, ROR, LSR, SLO, SRE, RLA, RRA, ISC, DCP)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (HasOpcode(LDA) & Elidable & DoesntMatterWhatItDoesWith(State.A) & MatchAddrMode(0) & MatchParameter(1)) ~~> (code => code.init),
    (HasOpcodeIn(Set(DEC, INC, ASL, ROL, ROR, LSR, SLO, SRE, RLA, RRA, ISC, DCP)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (HasOpcode(LDX) & Elidable & DoesntMatterWhatItDoesWith(State.X) & MatchAddrMode(0) & MatchParameter(1)) ~~> (code => code.init),
    (HasOpcodeIn(Set(DEC, INC, ASL, ROL, ROR, LSR, SLO, SRE, RLA, RRA, ISC, DCP)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (HasOpcode(LDY) & Elidable & DoesntMatterWhatItDoesWith(State.Y) & MatchAddrMode(0) & MatchParameter(1)) ~~> (code => code.init),
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
    loadBeforeTransfer(LDX, LDA, ConcernsX, State.X, TXA, LdxAddrModes),
    loadBeforeTransfer(LDA, LDX, ConcernsA, State.A, TAX, LdxAddrModes),
    loadBeforeTransfer(LDY, LDA, ConcernsY, State.Y, TYA, LdyAddrModes),
    loadBeforeTransfer(LDA, LDY, ConcernsA, State.A, TAY, LdyAddrModes),
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
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,

    (HasOpcodeIn(Set(LDA, STA)) & HasAddrMode(Immediate) & MatchParameter(1)) ~
      (Linear & Not(ChangesA) & Not(HasOpcode(DISCARD_AF))).* ~
      (Elidable & HasOpcode(LDA) & HasAddrMode(Immediate) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init),

    (HasOpcodeIn(Set(LDX, STX)) & HasAddrMode(Immediate) & MatchParameter(1)) ~
      (Linear & Not(ChangesX) & Not(HasOpcode(DISCARD_XF))).* ~
      (Elidable & HasOpcode(LDX) & HasAddrMode(Immediate) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init),

    (HasOpcodeIn(Set(LDY, STY)) & HasAddrMode(Immediate) & MatchParameter(1)) ~
      (Linear & Not(ChangesY) & Not(HasOpcode(DISCARD_YF))).* ~
      (Elidable & HasOpcode(LDY) & HasAddrMode(Immediate) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init),

    (HasOpcodeIn(Set(LDA, STA)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(ChangesA) & Not(HasOpcode(DISCARD_AF)) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init),

    (HasOpcodeIn(Set(LDX, STX)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(ChangesX) & Not(HasOpcode(DISCARD_XF)) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(LDX) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init),

    (HasOpcodeIn(Set(LDY, STY)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(ChangesY) & Not(HasOpcode(DISCARD_YF)) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(LDY) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init),

    (HasOpcode(LDA) & HasAddrMode(Immediate) & MatchParameter(1)) ~
      (Linear & Not(ChangesA) & Not(ChangesNAndZ) & Not(HasOpcode(DISCARD_AF))).* ~
      (Elidable & HasOpcode(LDA) & HasAddrMode(Immediate) & MatchParameter(1)) ~~> (_.init),

    (HasOpcode(LDX) & HasAddrMode(Immediate) & MatchParameter(1)) ~
      (Linear & Not(ChangesX) & Not(ChangesNAndZ) & Not(HasOpcode(DISCARD_XF))).* ~
      (Elidable & HasOpcode(LDX) & HasAddrMode(Immediate) & MatchParameter(1)) ~~> (_.init),

    (HasOpcode(LDY) & HasAddrMode(Immediate) & MatchParameter(1)) ~
      (Linear & Not(ChangesY) & Not(ChangesNAndZ) & Not(HasOpcode(DISCARD_YF))).* ~
      (Elidable & HasOpcode(LDY) & HasAddrMode(Immediate) & MatchParameter(1)) ~~> (_.init),

    (HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(ChangesA) & Not(ChangesNAndZ) & Not(HasOpcode(DISCARD_AF)) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~~> (_.init),

    (HasOpcode(LDX) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(ChangesX) & Not(ChangesNAndZ) & Not(HasOpcode(DISCARD_XF)) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(LDX) & MatchAddrMode(0) & MatchParameter(1)) ~~> (_.init),

    (HasOpcode(LDY) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(ChangesY) & Not(ChangesNAndZ) & Not(HasOpcode(DISCARD_YF)) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(LDY) & MatchAddrMode(0) & MatchParameter(1)) ~~> (_.init),

    (HasOpcodeIn(Set(LDA, STA)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (ShortConditionalBranching & MatchParameter(2)) ~
      (Linear & Not(ChangesA) & Not(HasOpcode(DISCARD_AF)) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAt(0, 1)).* ~
      (HasOpcode(LABEL) & MatchParameter(2)) ~
      (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init),

    (HasOpcodeIn(Set(LDX, STX)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (ShortConditionalBranching & MatchParameter(2)) ~
      (Linear & Not(ChangesX) & Not(HasOpcode(DISCARD_XF)) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAt(0, 1)).* ~
      (HasOpcode(LABEL) & MatchParameter(2)) ~
      (Elidable & HasOpcode(LDX) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init),

    (HasOpcodeIn(Set(LDY, STY)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (ShortConditionalBranching & MatchParameter(2)) ~
      (Linear & Not(ChangesY) & Not(HasOpcode(DISCARD_YF)) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAt(0, 1)).* ~
      (HasOpcode(LABEL) & MatchParameter(2)) ~
      (Elidable & HasOpcode(LDY) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init),

    (HasOpcodeIn(Set(LDA, STA)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (HasOpcodeIn(ShortBranching) & MatchParameter(3)) ~
      (Linear & Not(ChangesA) & Not(HasOpcode(DISCARD_AF)) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAt(0, 1)).* ~
      (HasOpcode(LABEL) & MatchParameter(3) & HasCallerCount(1)) ~
      (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init),

    HasOpcodeIn(Set(TXA, TAX)) ~
      (Not(Set(TXA, TAX)) & Linear & Not(ChangesA) & Not(ChangesX)).* ~
      (Elidable & HasOpcodeIn(Set(TXA, TAX)) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init),

    HasOpcodeIn(Set(TYA, TAY)) ~
      (Not(Set(TYA, TAY)) & Linear & Not(ChangesA) & Not(ChangesY)).* ~
      (Elidable & HasOpcodeIn(Set(TYA, TAY)) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init),
  )

  val RearrangableLoadFromTheSameLocation = new RuleBasedAssemblyOptimization("Rearrangable load from the same location",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,

    (HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (HasOpcode(STA) & Not(ReadsX)) ~
      (Elidable & HasOpcode(LDX) & MatchAddrMode(0) & MatchParameter(1)) ~~> {code =>
      List(code.head, AssemblyLine.implied(TAX), code(1))
    },
    (HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (HasOpcode(STA) & Not(ReadsY)) ~
      (Elidable & HasOpcode(LDY) & MatchAddrMode(0) & MatchParameter(1)) ~~> {code =>
      List(code.head, AssemblyLine.implied(TAY), code(1))
    },
    (HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (HasOpcode(LDA) & Not(ReadsX)) ~
      (Elidable & HasOpcode(LDX) & MatchAddrMode(0) & MatchParameter(1)) ~~> {code =>
      List(code.head, AssemblyLine.implied(TAX), code(1))
    },
    (HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (HasOpcode(LDA) & Not(ReadsY)) ~
      (Elidable & HasOpcode(LDY) & MatchAddrMode(0) & MatchParameter(1)) ~~> {code =>
      List(code.head, AssemblyLine.implied(TAY), code(1))
    },
    (HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(ChangesA) & Not(ChangesNAndZ) & DoesntChangeMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~~> {code =>
      code.init
    },
    (HasOpcode(LDX) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(ChangesX) & Not(ChangesNAndZ) & DoesntChangeMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(LDX) & MatchAddrMode(0) & MatchParameter(1)) ~~> {code =>
      code.init
    },
    (HasOpcode(LDY) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(ChangesY) & Not(ChangesNAndZ) & DoesntChangeMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(LDY) & MatchAddrMode(0) & MatchParameter(1)) ~~> {code =>
      code.init
    },
  )

  val PointlessOperationAfterLoad = new RuleBasedAssemblyOptimization("Pointless operation after load",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (ChangesA & ChangesNAndZ) ~ (Elidable & HasOpcode(EOR) & HasImmediate(0)) ~~> (_.init),
    (ChangesA & ChangesNAndZ) ~ (Elidable & HasOpcode(ORA) & HasImmediate(0)) ~~> (_.init),
    (ChangesA & ChangesNAndZ) ~ (Elidable & HasOpcode(AND) & HasImmediate(0xff)) ~~> (_.init)
  )

  val PointlessOperationFromFlow = new RuleBasedAssemblyOptimization("Pointless operation from flow",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    (Elidable & HasOpcodeIn(Set(LDA, TXA, TYA, AND, EOR, ORA, XAA)) & DoesntMatterWhatItDoesWith(State.A, State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(Set(LDX, TSX, TAX, SBX, INX, DEX)) & DoesntMatterWhatItDoesWith(State.X, State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(Set(LDY, TAY, INY, DEY)) & DoesntMatterWhatItDoesWith(State.Y, State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(Set(LAX, LXA)) & DoesntMatterWhatItDoesWith(State.A, State.X, State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(Set(ANC, ALR)) & DoesntMatterWhatItDoesWith(State.A, State.N, State.Z, State.C)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(Set(ADC, SBC, ARR)) & DoesntMatterWhatItDoesWith(State.A, State.N, State.Z, State.C, State.V)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(Set(CMP, CPY, CPX, BIT)) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.C, State.V)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(Set(ROL, ROR, ASL, LSR)) & HasAddrMode(Implied) & DoesntMatterWhatItDoesWith(State.A, State.N, State.Z, State.C)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(Set(INC, DEC)) & HasAddrMode(Implied) & DoesntMatterWhatItDoesWith(State.A, State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(Set(CLC, SEC)) & DoesntMatterWhatItDoesWith(State.C)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(Set(CLD, SED)) & DoesntMatterWhatItDoesWith(State.D)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(Set(CLV)) & DoesntMatterWhatItDoesWith(State.V)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(Set(ORA, EOR)) & HasImmediate(0) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcode(AND) & HasImmediate(0xff) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcode(ANC) & HasImmediate(0xff) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.C)) ~~> (_ => Nil),
  )

  val SimplifiableStackOperation = new RuleBasedAssemblyOptimization("Simplifiable stack operation",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    (Elidable & HasOpcode(TSX)) ~
      (Elidable & HasOpcode(INX)) ~
      (Elidable & HasOpcode(TXS)) ~
      (Elidable & HasOpcode(PHA) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~~> (_ => List(AssemblyLine.implied(TSX), AssemblyLine.absoluteX(STA, 0x101))),
    (Elidable & HasOpcode(TSX)) ~
      (Elidable & HasOpcode(INX)) ~
      (Elidable & HasOpcode(TXS) & DoesntMatterWhatItDoesWith(State.Z, State.N, State.A)) ~~> (_ => List(AssemblyLine.implied(PLA))),
    (Elidable & HasOpcode(TSX)) ~
      (Elidable & HasOpcode(INX)) ~
      (Elidable & HasOpcode(TXS)) ~
      (ConcernsA & Not(ConcernsStack) & Linear & DoesntMatterWhatItDoesWith(State.Z, State.N, State.A)) ~~> (code => List(code.last, AssemblyLine.implied(PLA))),
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

  val SimplifiableIndexChanging = new RuleBasedAssemblyOptimization("Simplifiable index changing",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (Elidable & HasOpcode(LDX) & MatchImmediate(0)) ~
      (Linear & Not(ConcernsX) & Not(ReadsNOrZ)).* ~
      (Elidable & HasOpcode(DEX)) ~~> { (lines, ctx) =>
      lines.init.tail :+ AssemblyLine.immediate(LDX, (ctx.get[Constant](0) + -1).quickSimplify)
    },
    (Elidable & HasOpcode(LDX) & MatchImmediate(0)) ~
      (Linear & Not(ConcernsX) & Not(ReadsNOrZ)).* ~
      (Elidable & HasOpcode(INX)) ~~> { (lines, ctx) =>
      lines.init.tail :+ AssemblyLine.immediate(LDX, (ctx.get[Constant](0) + 1).quickSimplify)
    },
    (Elidable & HasOpcode(LDY) & MatchImmediate(0)) ~
      (Linear & Not(ConcernsY) & Not(ReadsNOrZ)).* ~
      (Elidable & HasOpcode(DEY)) ~~> { (lines, ctx) =>
      lines.init.tail :+ AssemblyLine.immediate(LDY, (ctx.get[Constant](0) + -1).quickSimplify)
    },
    (Elidable & HasOpcode(LDY) & MatchImmediate(0)) ~
      (Linear & Not(ConcernsY) & Not(ReadsNOrZ)).* ~
      (Elidable & HasOpcode(INY)) ~~> { (lines, ctx) =>
      lines.init.tail :+ AssemblyLine.immediate(LDY, (ctx.get[Constant](0) + 1).quickSimplify)
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
      (HasOpcode(ldax) & HasImmediate(0)) ~
        (Elidable & HasOpcode(stax) & MatchParameter(4) & MatchAddrMode(5)) ~
        (HasOpcode(LDA) & MatchParameter(0) & MatchAddrMode(1) & DoesNotConcernMemoryAt(5, 4)) ~
        (Elidable & HasOpcode(STA) & MatchParameter(2) & MatchAddrMode(3) & DoesntChangeMemoryAt(1, 0) & DoesntChangeMemoryAt(5, 4) & restriction)
    } else {
      (HasOpcode(LDA) & MatchParameter(0) & MatchAddrMode(1)) ~
        (Elidable & HasOpcode(STA) & MatchParameter(2) & MatchAddrMode(3) & DoesntChangeMemoryAt(1, 0) & restriction) ~
        (HasOpcode(ldax) & HasImmediate(0)) ~
        (Elidable & HasOpcode(stax) & MatchParameter(4) & MatchAddrMode(5) & DoesntChangeMemoryAt(1, 0) & DoesntChangeMemoryAt(3, 2))
    }
    val middle = (Linear
      & DoesntChangeIndexingInAddrMode(3)
      & DoesntChangeIndexingInAddrMode(5)
      & DoesNotConcernMemoryAt(3, 2)
      & DoesNotConcernMemoryAt(5, 4)).*
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

  val SmarterShiftingOfWords = new RuleBasedAssemblyOptimization("Smarter shifting of words",
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

  val ShiftingJustWrittenValue = new RuleBasedAssemblyOptimization("Shifting just written value",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.C, State.Z, State.N, State.A)) ~
      (Linear & DoesNotConcernMemoryAt(0, 1) & DoesntChangeIndexingInAddrMode(0)).* ~
      (Elidable & HasOpcodeIn(Set(ASL, LSR)) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.C, State.Z, State.N)) ~~> { code =>
      code.last.copy(addrMode = AddrMode.Implied, parameter = Constant.Zero) :: code.init
    },
    (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.C, State.Z, State.N, State.A)) ~
      (Linear & DoesNotConcernMemoryAt(0, 1) & DoesntChangeIndexingInAddrMode(0) & Not(ChangesC)).* ~
      (Elidable & HasOpcodeIn(Set(ASL, LSR)) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~~> { code =>
      code.last.copy(addrMode = AddrMode.Implied, parameter = Constant.Zero) :: code.init
    },
    (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.Z, State.N, State.A)) ~
      (Linear & DoesNotConcernMemoryAt(0, 1) & DoesntChangeIndexingInAddrMode(0) & Not(ChangesC)).* ~
      (Elidable & HasOpcodeIn(Set(ROL, ROR)) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~~> { code =>
      code.last.copy(addrMode = AddrMode.Implied, parameter = Constant.Zero) :: code.init
    },
  )

  val SmarterShiftingBytes = new RuleBasedAssemblyOptimization("Smarter shifting of bytes",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (Elidable & HasOpcode(ASL) & HasAddrMode(Implied)) ~
      (Elidable & HasOpcode(ASL) & HasAddrMode(Implied)) ~
      (Elidable & HasOpcode(ASL) & HasAddrMode(Implied)) ~
      (Elidable & HasOpcode(ASL) & HasAddrMode(Implied)) ~
      (Elidable & HasOpcode(ASL) & HasAddrMode(Implied)) ~
      (Elidable & HasOpcode(ASL) & HasAddrMode(Implied)) ~
      (Elidable & HasOpcode(ASL) & HasAddrMode(Implied)) ~~> { _ =>
      List(
        AssemblyLine.implied(ROR),
        AssemblyLine.implied(ROR),
        AssemblyLine.immediate(AND, 0x80)
      )
    },
    (Elidable & HasOpcode(ASL) & HasAddrMode(Implied)) ~
      (Elidable & HasOpcode(ASL) & HasAddrMode(Implied)) ~
      (Elidable & HasOpcode(ASL) & HasAddrMode(Implied)) ~
      (Elidable & HasOpcode(ASL) & HasAddrMode(Implied)) ~
      (Elidable & HasOpcode(ASL) & HasAddrMode(Implied)) ~
      (Elidable & HasOpcode(ASL) & HasAddrMode(Implied)) ~~> { _ =>
      List(
        AssemblyLine.implied(ROR),
        AssemblyLine.implied(ROR),
        AssemblyLine.implied(ROR),
        AssemblyLine.immediate(AND, 0xC0)
      )
    },
    (Elidable & HasOpcode(LSR) & HasAddrMode(Implied)) ~
      (Elidable & HasOpcode(LSR) & HasAddrMode(Implied)) ~
      (Elidable & HasOpcode(LSR) & HasAddrMode(Implied)) ~
      (Elidable & HasOpcode(LSR) & HasAddrMode(Implied)) ~
      (Elidable & HasOpcode(LSR) & HasAddrMode(Implied)) ~
      (Elidable & HasOpcode(LSR) & HasAddrMode(Implied)) ~
      (Elidable & HasOpcode(LSR) & HasAddrMode(Implied)) ~~> { _ =>
      List(
        AssemblyLine.implied(ROL),
        AssemblyLine.implied(ROL),
        AssemblyLine.immediate(AND, 0x1)
      )
    },
    (Elidable & HasOpcode(LSR) & HasAddrMode(Implied)) ~
      (Elidable & HasOpcode(LSR) & HasAddrMode(Implied)) ~
      (Elidable & HasOpcode(LSR) & HasAddrMode(Implied)) ~
      (Elidable & HasOpcode(LSR) & HasAddrMode(Implied)) ~
      (Elidable & HasOpcode(LSR) & HasAddrMode(Implied)) ~
      (Elidable & HasOpcode(LSR) & HasAddrMode(Implied)) ~~> { _ =>
      List(
        AssemblyLine.implied(ROL),
        AssemblyLine.implied(ROL),
        AssemblyLine.implied(ROL),
        AssemblyLine.immediate(AND, 0x3)
      )
    },
  )

  private def carryFlagConversionCase(shift: Int, firstSet: Boolean, zeroIfSet: Boolean) = {
    val nonZero = 1 << shift
    val test = Elidable & HasOpcode(if (firstSet) BCC else BCS) & MatchParameter(0)
    val ifSet = Elidable & HasOpcode(LDA) & HasImmediate(if (zeroIfSet) 0 else nonZero)
    val ifClear = Elidable & HasOpcode(LDA) & HasImmediate(if (zeroIfSet) nonZero else 0)
    val jump = Elidable & HasOpcodeIn(Set(JMP, if (firstSet) BCS else BCC, if (zeroIfSet) BEQ else BNE)) & MatchParameter(1)
    val elseLabel = Elidable & HasOpcode(LABEL) & MatchParameter(0)
    val afterLabel = Elidable & HasOpcode(LABEL) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.C, State.N, State.V, State.Z)
    val store = Elidable & (Not(ReadsC) & Linear | HasOpcodeIn(Set(RTS, JSR, RTI, RTL, BSR)))
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
    (Elidable & HasOpcode(ADC) & HasImmediate(0) & HasA(0) & DoesntMatterWhatItDoesWith(State.V)) ~~> (_ => List(AssemblyLine.implied(ROL))),
    (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(CLC)).? ~
      (Elidable & HasOpcode(ADC) & MatchAddrMode(0) & MatchParameter(1) & HasClear(State.C) & HasClear(State.D) & DoesntMatterWhatItDoesWith(State.V)) ~~> { code =>
      List(code.head, AssemblyLine.implied(ASL))
    },
    (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(ADC) & MatchAddrMode(0) & MatchParameter(1) & HasClear(State.D) & DoesntMatterWhatItDoesWith(State.V)) ~~> { code =>
      List(code.head, AssemblyLine.implied(ROL))
    },
    (Elidable & HasOpcode(ADC) & HasImmediate(0) & HasClear(State.C) & DoesntMatterWhatItDoesWith(State.V, State.C, State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcode(ROL) & HasClear(State.C)) ~~> (code => code.map(_.copy(opcode = ASL))),
    (Elidable & HasOpcode(ROR) & HasClear(State.C)) ~~> (code => code.map(_.copy(opcode = LSR))),
    (HasOpcode(AND) & HasImmediate(1)) ~
      (Linear & Not(ChangesNAndZ) & Not(HasOpcode(CLC)) & Not(ChangesA)).* ~
      (Elidable & HasOpcode(CLC) & HasClear(State.D)) ~
      (Elidable & HasOpcode(ADC) & MatchAddrMode(0) & MatchParameter(1) & HasAddrModeIn(Set(ZeroPage, ZeroPageX, Absolute, AbsoluteX))) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.C, State.V, State.Z, State.N, State.A)) ~~> { code =>
      val label = getNextLabel("in")
      code.take(code.length - 3) ++ List(
        AssemblyLine.relative(BEQ, label),
        code.last.copy(opcode = INC),
        AssemblyLine.label(label)
      )
    },
    (HasOpcode(ANC) & HasImmediate(1)) ~
      (Linear & Not(ChangesNAndZ) & Not(ChangesA) & (Not(ChangesC) | HasOpcode(CLC))).* ~
      (Elidable & HasOpcode(ADC) & MatchAddrMode(0) & MatchParameter(1) & HasClear(State.D) & HasAddrModeIn(Set(ZeroPage, ZeroPageX, Absolute, AbsoluteX))) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.C, State.V, State.Z, State.N, State.A)) ~~> { code =>
      val label = getNextLabel("in")
      code.head.copy(opcode = AND) :: code.take(code.length - 2).tail ++ List(
        AssemblyLine.relative(BEQ, label),
        code.last.copy(opcode = INC),
        AssemblyLine.label(label)
      )
    },
  )

  val IndexSequenceOptimization = new RuleBasedAssemblyOptimization("Index sequence optimization",
    needsFlowInfo = FlowInfoRequirement.ForwardFlow,
    (Elidable & HasOpcode(LDY) & MatchImmediate(1) & MatchY(0)) ~
      Where(ctx => ctx.get[Constant](1).quickSimplify.isLowestByteAlwaysEqual(ctx.get[Int](0))) ~~> (_ => Nil),
    (Elidable & HasOpcode(LDY) & MatchImmediate(1) & MatchY(0)) ~
      Where(ctx => ctx.get[Constant](1).quickSimplify.isLowestByteAlwaysEqual(ctx.get[Int](0) + 1)) ~~> (_ => List(AssemblyLine.implied(INY))),
    (Elidable & HasOpcode(LDY) & MatchImmediate(1) & MatchY(0)) ~
      Where(ctx => ctx.get[Constant](1).quickSimplify.isLowestByteAlwaysEqual(ctx.get[Int](0) - 1)) ~~> (_ => List(AssemblyLine.implied(DEY))),
    (Elidable & HasOpcode(LDX) & MatchImmediate(1) & MatchX(0)) ~
      Where(ctx => ctx.get[Constant](1).quickSimplify.isLowestByteAlwaysEqual(ctx.get[Int](0))) ~~> (_ => Nil),
    (Elidable & HasOpcode(LDX) & MatchImmediate(1) & MatchX(0)) ~
      Where(ctx => ctx.get[Constant](1).quickSimplify.isLowestByteAlwaysEqual(ctx.get[Int](0) + 1)) ~~> (_ => List(AssemblyLine.implied(INX))),
    (Elidable & HasOpcode(LDX) & MatchImmediate(1) & MatchX(0)) ~
      Where(ctx => ctx.get[Constant](1).quickSimplify.isLowestByteAlwaysEqual(ctx.get[Int](0) - 1)) ~~> (_ => List(AssemblyLine.implied(DEX))),
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

  val LoopInvariantRegister = new RuleBasedAssemblyOptimization("Loop invariant register",
    needsFlowInfo = FlowInfoRequirement.JustLabels,
    (HasOpcode(LABEL) & MatchParameter(2) & HasCallerCount(1)) ~
      (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1) & IsNonvolatile) ~
      (Linear & Not(ChangesA) & DoesntChangeMemoryAt(0, 1)).* ~
      (ShortConditionalBranching & MatchParameter(2)) ~~> { code =>
      code(1) :: code(0) :: code.drop(2)
    },
    (HasOpcode(LABEL) & MatchParameter(2) & HasCallerCount(1)) ~
      (Elidable & HasOpcode(LDX) & MatchAddrMode(0) & MatchParameter(1) & IsNonvolatile) ~
      (Linear & Not(ChangesX) & DoesntChangeMemoryAt(0, 1)).* ~
      (ShortConditionalBranching & MatchParameter(2)) ~~> { code =>
      code(1) :: code(0) :: code.drop(2)
    },
    (HasOpcode(LABEL) & MatchParameter(2) & HasCallerCount(1)) ~
      (Elidable & HasOpcode(LDY) & MatchAddrMode(0) & MatchParameter(1) & IsNonvolatile) ~
      (Linear & Not(ChangesY) & DoesntChangeMemoryAt(0, 1)).* ~
      (ShortConditionalBranching & MatchParameter(2)) ~~> { code =>
      code(1) :: code(0) :: code.drop(2)
    },
    (HasOpcode(LABEL) & MatchParameter(2) & HasCallerCount(1)) ~
      (Elidable & HasOpcode(LDZ) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(ChangesIZ) & DoesntChangeMemoryAt(0, 1)).* ~
      (ShortConditionalBranching & MatchParameter(2)) ~~> { code =>
      code(1) :: code(0) :: code.drop(2)
    },

  )


  val UnusedLabelRemoval = new RuleBasedAssemblyOptimization("Unused label removal",
    needsFlowInfo = FlowInfoRequirement.JustLabels,
    (Elidable & HasOpcode(LABEL) & HasCallerCount(0)) ~~> (_ => Nil)
  )

  val OperationsAroundShifting = new RuleBasedAssemblyOptimization("Operations around shifting",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    (Elidable & HasOpcode(CLC)).? ~
      (Elidable & HasOpcode(ADC) & HasClear(State.C) & HasClear(State.D) & MatchImmediate(1)) ~
      HasOpcode(ASL).+.capture(2) ~
      (Elidable & HasOpcode(CLC)) ~
      (Elidable & HasOpcode(ADC) & HasClear(State.D) & MatchImmediate(3) & DoesntMatterWhatItDoesWith(State.C, State.Z, State.N)) ~~> { (code, ctx) =>
      val shifts = ctx.get[List[AssemblyLine]](2)
      val const = ctx.get[Constant](1).asl(shifts.length) + ctx.get[Constant](3)
      shifts ++ List(AssemblyLine.implied(CLC), AssemblyLine.immediate(ADC, const))
    },
    (Elidable & HasOpcode(AND) & MatchImmediate(1)) ~
      HasOpcode(ASL).+.capture(2) ~
      (Elidable & HasOpcode(AND) & MatchImmediate(3) & DoesntMatterWhatItDoesWith(State.C, State.Z, State.N)) ~~> { (code, ctx) =>
      val shifts = ctx.get[List[AssemblyLine]](2)
      val const = CompoundConstant(MathOperator.And, ctx.get[Constant](1).asl(shifts.length), ctx.get[Constant](3)).quickSimplify
      shifts :+ AssemblyLine.immediate(AND, const)
    },
    (Elidable & HasOpcode(EOR) & MatchImmediate(1)) ~
      HasOpcode(ASL).+.capture(2) ~
      (Elidable & HasOpcode(EOR) & MatchImmediate(3) & DoesntMatterWhatItDoesWith(State.C, State.Z, State.N)) ~~> { (code, ctx) =>
      val shifts = ctx.get[List[AssemblyLine]](2)
      val const = CompoundConstant(MathOperator.Exor, ctx.get[Constant](1).asl(shifts.length), ctx.get[Constant](3)).quickSimplify
      shifts :+ AssemblyLine.immediate(EOR, const)
    },
    (Elidable & HasOpcode(ORA) & MatchImmediate(1)) ~
      HasOpcode(ASL).+.capture(2) ~
      (Elidable & HasOpcode(ORA) & MatchImmediate(3) & DoesntMatterWhatItDoesWith(State.C, State.Z, State.N)) ~~> { (code, ctx) =>
      val shifts = ctx.get[List[AssemblyLine]](2)
      val const = CompoundConstant(MathOperator.Or, ctx.get[Constant](1).asl(shifts.length), ctx.get[Constant](3)).quickSimplify
      shifts :+ AssemblyLine.immediate(ORA, const)
    },
  )

  val BitPackingUnpacking = new RuleBasedAssemblyOptimization("Bit packing/unpacking",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(AND) & HasImmediate(1)) ~
      ((Elidable & Linear & Not(ChangesMemory) & DoesNotConcernMemoryAt(0, 1) & Not(ChangesA)).* ~
        (Elidable & HasOpcode(STA) & DoesNotConcernMemoryAt(0, 1))).capture(3) ~
      ((Elidable & HasOpcodeIn(Set(LSR, ROR)) & Not(ChangesA) & MatchAddrMode(0) & Not(MatchParameter(1))).* ~
        (Elidable & HasOpcodeIn(Set(LSR, ROR)) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.Z, State.C, State.N))).capture(2) ~~> { (code, ctx) =>
      ctx.get[List[AssemblyLine]](2) ++
        List(AssemblyLine.immediate(LDA, 0), AssemblyLine.implied(ROL)) ++
        ctx.get[List[AssemblyLine]](3)
    },
    (Elidable & HasOpcode(LDA) & HasImmediate(1)) ~
      (Elidable & HasOpcode(AND) & MatchAddrMode(0) & MatchParameter(1)) ~
      ((Elidable & Linear & Not(ChangesMemory) & DoesNotConcernMemoryAt(0, 1) & Not(ChangesA)).* ~
        (Elidable & HasOpcode(STA) & DoesNotConcernMemoryAt(0, 1))).capture(3) ~
      ((Elidable & HasOpcodeIn(Set(LSR, ROR)) & Not(ChangesA) & MatchAddrMode(0) & Not(MatchParameter(1))).* ~
        (Elidable & HasOpcodeIn(Set(LSR, ROR)) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.Z, State.C, State.N))).capture(2) ~~> { (code, ctx) =>
      ctx.get[List[AssemblyLine]](2) ++
        List(AssemblyLine.immediate(LDA, 0), AssemblyLine.implied(ROL)) ++
        ctx.get[List[AssemblyLine]](3)
    },
    (Elidable & (HasOpcode(ASL) | HasOpcode(ROL) & HasClear(State.C)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(ROL) & Not(ChangesA) & MatchAddrMode(0) & Not(MatchParameter(1))).*.capture(2) ~
      (Elidable & HasOpcode(CLC)).? ~
      (Elidable & HasOpcodeIn(Set(LDA, TYA, TXA, PLA))).capture(3) ~
      (Elidable & HasOpcode(AND) & HasImmediate(1)) ~
      (Elidable & HasOpcode(CLC)).? ~
      (Elidable & (HasOpcode(ORA) | HasOpcode(ADC) & HasClear(State.C) & HasClear(State.D)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.C, State.N, State.Z, State.V, State.A)) ~~> { (code, ctx) =>
      ctx.get[List[AssemblyLine]](3) ++
        List(AssemblyLine.implied(LSR), code.head.copy(opcode = ROL)) ++
        ctx.get[List[AssemblyLine]](2)
    },
    (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcodeIn(Set(ROL, ASL)) & DoesntMatterWhatItDoesWith(State.A)) ~
      (Linear & DoesNotConcernMemoryAt(0, 1) & DoesntChangeIndexingInAddrMode(0)).* ~
      (Elidable & HasOpcodeIn(Set(ROL, ASL)) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.C) & MatchAddrMode(0) & MatchParameter(1)) ~~> { code =>
      code.last :: code.drop(2).init
    },
    (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcodeIn(Set(ROR, LSR)) & DoesntMatterWhatItDoesWith(State.A)) ~
      (Linear & DoesNotConcernMemoryAt(0, 1) & DoesntChangeIndexingInAddrMode(0)).* ~
      (Elidable & HasOpcode(LSR) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.C) & MatchAddrMode(0) & MatchParameter(1)) ~~> { code =>
      code.last :: code.drop(2).init
    },
    (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcodeIn(Set(ROR, LSR)) & DoesntMatterWhatItDoesWith(State.A)) ~
      (Linear & Not(HasOpcode(LSR)) & DoesNotConcernMemoryAt(0, 1) & DoesntChangeIndexingInAddrMode(0)).* ~
      (Elidable & HasOpcode(LSR) & DoesNotConcernMemoryAt(0, 1)) ~
      (Elidable & HasOpcode(ROR) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.C) & MatchAddrMode(0) & MatchParameter(1)) ~~> { code =>
      code.init.last :: code.last :: code.drop(2).init.init
    },
    (Elidable & HasOpcode(ASL) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(INC) & MatchAddrMode(0) & MatchParameter(1)) ~~> { code =>
      List(AssemblyLine.implied(SEC), code.head.copy(opcode = ROL))
    },
    (Elidable & HasOpcode(ASL) & HasAddrMode(AddrMode.Implied) & HasClear(State.D)) ~
      (Elidable & HasOpcode(CLC)) ~
      (Elidable & HasOpcode(ADC) & HasImmediate(1) & DoesntMatterWhatItDoesWith(State.C, State.V)) ~~> { code =>
      List(AssemblyLine.implied(SEC), code.head.copy(opcode = ROL))
    },
    (Elidable & HasOpcode(ASL) & HasAddrMode(AddrMode.Implied)) ~
      (Elidable & HasOpcodeIn(Set(ORA, EOR)) & HasImmediate(1)) ~~> { code =>
      List(AssemblyLine.implied(SEC), code.head.copy(opcode = ROL))
    },
  )

  private def blockIsIdempotentWhenItComesToIndexRegisters(i: Int) = Where(ctx => {
    val code = ctx.get[List[AssemblyLine]](i)
    val rx = code.indexWhere(ReadsX)
    val wx = code.indexWhere(l => ChangesX(l.opcode))
    val ry = code.indexWhere(ReadsY)
    val wy = code.indexWhere(l => ChangesY(l.opcode))
    val xOk = rx < 0 || wx < 0 || rx >= wx
    val yOk = ry < 0 || wy < 0 || ry >= wy
    xOk && yOk
  })

  val CommonExpressionInConditional = new RuleBasedAssemblyOptimization("Common expression in conditional",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    (
      (HasOpcodeIn(Set(LDA, LAX)) & MatchAddrMode(0) & MatchParameter(1)) ~
        HasOpcodeIn(Set(LDY, LDX, AND, ORA, EOR, ADC, SBC, CLC, SEC, CPY, CPX, CMP)).*
      ).capture(7) ~
      blockIsIdempotentWhenItComesToIndexRegisters(7) ~
      HasOpcodeIn(ShortConditionalBranching) ~
      MatchElidableCopyOf(7, Anything, DoesntMatterWhatItDoesWith(State.C, State.Z, State.N, State.V)) ~~> { code =>
      code.take(code.length / 2 + 1)
    },

    (Elidable & HasOpcodeIn(Set(LDA, LAX)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(AND) & HasAddrModeIn(Set(Absolute, ZeroPage)) & DoesntMatterWhatItDoesWith(State.C, State.V, State.A)) ~
      HasOpcodeIn(Set(BEQ, BNE)) ~
      (HasOpcodeIn(Set(LDA, LAX)) & MatchAddrMode(0) & MatchParameter(1)) ~~> { code =>
      List(code(0), code(1).copy(opcode = BIT), code(2))
    },

    (Elidable & HasOpcode(LDA) & HasAddrModeIn(Set(Absolute, ZeroPage))) ~
      (Elidable & HasOpcode(AND) & MatchAddrMode(0) & MatchParameter(1)) ~
      HasOpcodeIn(Set(BEQ, BNE)) ~
      (HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~~> { code =>
      List(code(1).copy(opcode = LDA), code(0).copy(opcode = BIT), code(2))
    },

  )

  private def negate(branch: AssemblyLine) = branch.opcode match {
    case BEQ => branch.copy(opcode = BNE)
    case BNE => branch.copy(opcode = BEQ)
    case BCC => branch.copy(opcode = BCS)
    case BCS => branch.copy(opcode = BCC)
    case BVC => branch.copy(opcode = BVS)
    case BVS => branch.copy(opcode = BVC)
    case BMI => branch.copy(opcode = BPL)
    case BPL => branch.copy(opcode = BMI)
  }

  val DoubleJumpSimplification = new RuleBasedAssemblyOptimization("Double jump simplification",
    needsFlowInfo = FlowInfoRequirement.JustLabels,
    ((Elidable & HasOpcode(LABEL) & MatchParameter(1)) ~ LinearOrLabel.*).capture(10) ~
      Where(ctx => ctx.get[List[AssemblyLine]](10).map(_.sizeInBytes).sum < 100) ~
      (Elidable & HasOpcodeIn(ShortConditionalBranching) & MatchParameter(0)) ~
      (Elidable & HasOpcode(JMP) & MatchParameter(1)) ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(0)) ~~> { (code, ctx) =>
      ctx.get[List[AssemblyLine]](10) ++ List(negate(code(code.length - 3).copy(parameter = ctx.get[Constant](1))), code.last)
    },
    ((Elidable & HasOpcode(LABEL) & MatchParameter(1)) ~ LinearOrLabel.*).capture(10) ~
      Where(ctx => ctx.get[List[AssemblyLine]](10).map(_.sizeInBytes).sum < 100) ~
      (Elidable & HasOpcodeIn(ShortConditionalBranching) & MatchParameter(0)).capture(13) ~
      ((Elidable & Not(MatchParameter(0))).* ~
        (Elidable & HasOpcode(LABEL) & MatchParameter(0)) ~
        (Elidable & HasOpcode(JMP) & MatchParameter(1))).capture(11) ~~> { (code, ctx) =>
        ctx.get[List[AssemblyLine]](10) ++ ctx.get[List[AssemblyLine]](13).map(_.copy(parameter = ctx.get[Constant](1))) ++ ctx.get[List[AssemblyLine]](11)
    },
    (Elidable & HasOpcode(LABEL) & MatchParameter(0)) ~
      (Elidable & HasOpcode(JMP) & MatchParameter(1)) ~
      Not(MatchParameter(1)).* ~
      (HasOpcode(LABEL) & MatchParameter(1)) ~
      (HasOpcode(JMP) & MatchParameter(1)) ~~> { (code, ctx) =>
      code.head :: code.drop(2)
    },
  )

  val IndexComparisonOptimization = new RuleBasedAssemblyOptimization("Index comparison optimization",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    (Elidable & HasOpcodeIn(Set(DEX, INX)) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~
      (Linear & Not(ConcernsX)).* ~
      (Elidable & (HasOpcode(TXA) & DoesntMatterWhatItDoesWith(State.A) | HasOpcode(CPX) & HasImmediate(0) & DoesntMatterWhatItDoesWith(State.C, State.V))) ~~> { code =>
      code.tail.init :+ code.head
    },
    (Elidable & HasOpcodeIn(Set(DEY, INY)) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~
      (Linear & Not(ConcernsY)).* ~
      (Elidable & (HasOpcode(TYA) & DoesntMatterWhatItDoesWith(State.A) | HasOpcode(CPY) & HasImmediate(0) & DoesntMatterWhatItDoesWith(State.C, State.V))) ~~> { code =>
      code.tail.init :+ code.head
    },
    (Elidable & HasAddrMode(Implied) & HasOpcodeIn(Set(DEC, INC)) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~
      (Linear & Not(ConcernsA)).* ~
      (Elidable & (
        HasOpcode(TAY) & DoesntMatterWhatItDoesWith(State.Y)
          | HasOpcode(TAX) & DoesntMatterWhatItDoesWith(State.X)
          | HasOpcode(EOR) & HasImmediate(0)
          | HasOpcode(ORA) & HasImmediate(0)
          | HasOpcode(AND) & HasImmediate(0xff)
          | HasOpcode(ANC) & HasImmediate(0xff) & DoesntMatterWhatItDoesWith(State.C, State.V)
          | HasOpcode(CMP) & HasImmediate(0) & DoesntMatterWhatItDoesWith(State.C, State.V))) ~~> { code =>
      code.tail.init :+ code.head
    },
  )

  val OptimizeZeroComparisons = new RuleBasedAssemblyOptimization("Optimizing zero comparisons",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    (Elidable & HasSourceOfNZ(State.A) & HasOpcode(CMP) & HasImmediate(0) & DoesntMatterWhatItDoesWith(State.C)) ~~> (_.init),
    (Elidable & HasSourceOfNZ(State.X) & HasOpcode(CPX) & HasImmediate(0) & DoesntMatterWhatItDoesWith(State.C)) ~~> (_.init),
    (Elidable & HasSourceOfNZ(State.Y) & HasOpcode(CPY) & HasImmediate(0) & DoesntMatterWhatItDoesWith(State.C)) ~~> (_.init),
    (Elidable & HasSourceOfNZ(State.IZ) & HasOpcode(CPZ) & HasImmediate(0) & DoesntMatterWhatItDoesWith(State.C)) ~~> (_.init),
  )

  private def remapZ2N(line: AssemblyLine) = line.opcode match {
    case BNE => line.copy(opcode = BMI)
    case BEQ => line.copy(opcode = BPL)
  }
  private def remapZ2V(line: AssemblyLine) = line.opcode match {
    case BNE => line.copy(opcode = BVS)
    case BEQ => line.copy(opcode = BVC)
  }

  val SimplifiableCondition = new RuleBasedAssemblyOptimization("Simplifiable condition",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    HasOpcode(LDA) ~
      (Elidable & HasOpcode(AND) & HasImmediate(0x80)) ~
      (Elidable & HasOpcodeIn(Set(BNE, BEQ)) & DoesntMatterWhatItDoesWith(State.A, State.N, State.Z)) ~~> {code =>
      List(code(0), remapZ2N(code(2)))
    },
    (Elidable & HasOpcode(LDA) & HasImmediate(0x80)) ~
      (Elidable & HasOpcode(AND)) ~
      (Elidable & HasOpcodeIn(Set(BNE, BEQ)) & DoesntMatterWhatItDoesWith(State.A, State.N, State.Z)) ~~> {code =>
      List(code(1).copy(opcode = LDA), remapZ2N(code(2)))
    },
  )

  val PointlessSignCheck: RuleBasedAssemblyOptimization = {
    def loadOldSignedVariable: AssemblyPattern = (
      (HasOpcodeIn(Set(AND, ANC)) & HasImmediateWhere(i => (i & 0x80) == 0)) ~
      (HasOpcode(STA) & HasAddrModeIn(Set(Absolute, ZeroPage)) & MatchAddrMode(0) & MatchParameter(1)) ~
      DoesNotConcernMemoryAt(0, 1).* ~
      (HasOpcode(LDA) & HasAddrModeIn(Set(Absolute, ZeroPage)) & MatchParameter(1))
      ).capture(10) ~ Where(_.isExternallyLinearBlock(10))

    val isNonnegative: Int => Boolean = i => (i & 0x80) == 0

    new RuleBasedAssemblyOptimization("Pointless sign check",
      needsFlowInfo = FlowInfoRequirement.NoRequirement,
      (HasOpcode(AND) & HasImmediateWhere(isNonnegative)) ~
        (Elidable & HasOpcode(BMI)) ~~> (_.take(1)),
      loadOldSignedVariable ~
        (Elidable & HasOpcode(BMI)) ~~> { (code, ctx) => ctx.get[List[AssemblyLine]](10) },
      loadOldSignedVariable ~
        (Elidable & HasOpcodeIn(Set(ORA, EOR)) & HasImmediateWhere(isNonnegative)) ~
        (Elidable & HasOpcode(BMI)) ~
        OverwritesA ~~> { (code, ctx) => ctx.get[List[AssemblyLine]](10) :+ code.last },
      loadOldSignedVariable ~
        (Elidable & HasOpcodeIn(Set(ORA, EOR)) & HasImmediateWhere(isNonnegative)) ~
        (Elidable & HasOpcode(BMI)) ~~> { code => code.init },
      loadOldSignedVariable ~
        (Elidable & HasOpcodeIn(Set(ORA, EOR)) & HasImmediateWhere(isNonnegative)).? ~
        (Elidable & HasOpcode(BPL)) ~~> { code => code.init :+ code.last.copy(opcode = JMP, addrMode = Absolute) },
      loadOldSignedVariable ~
        ((Linear & Not(ConcernsX) & Not(ChangesA)).* ~
          HasOpcode(TAX) ~
          (Linear & Not(ConcernsX)).*).capture(11) ~
        (Elidable & HasOpcode(TXA)) ~
        (Elidable & HasOpcodeIn(Set(ORA, EOR)) & HasImmediateWhere(isNonnegative)).? ~
        (Elidable & HasOpcode(BMI)) ~
        OverwritesA ~~> { (code, ctx) => ctx.get[List[AssemblyLine]](10) ++ ctx.get[List[AssemblyLine]](11) :+ code.last },
      loadOldSignedVariable ~
        (Linear & Not(ConcernsX) & Not(ChangesA)).* ~
        HasOpcode(TAX) ~
        (Linear & Not(ConcernsX)).* ~
        HasOpcode(TXA) ~
        (HasOpcodeIn(Set(ORA, EOR)) & HasImmediateWhere(isNonnegative)).? ~
        (Elidable & HasOpcode(BMI)) ~~> { code => code.init },
      loadOldSignedVariable ~
        (Linear & Not(ConcernsX) & Not(ChangesA)).* ~
        HasOpcode(TAX) ~
        (Linear & Not(ConcernsX)).* ~
        HasOpcode(TXA) ~
        (HasOpcodeIn(Set(ORA, EOR)) & HasImmediateWhere(isNonnegative)).? ~
        (Elidable & HasOpcode(BPL)) ~~> { code => code.init :+ code.last.copy(opcode = JMP, addrMode = Absolute) },
    )
  }

  val NonetAddition = new RuleBasedAssemblyOptimization("Nonet addition",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    (Elidable & HasOpcode(LDX) & HasImmediate(0) & HasClear(State.D)) ~
      (Elidable & HasOpcode(BCC) & MatchParameter(14)) ~
      (Elidable & HasOpcode(INX)) ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(14) & HasCallerCount(1)) ~
      (Elidable & HasOpcode(CLC)) ~
      (Elidable & HasOpcode(ADC) & MatchAddrMode(0) & MatchParameter(1) & Not(ConcernsX)) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1) & Not(ConcernsX)) ~
      (Elidable & HasOpcode(TXA)) ~
      (Elidable & HasOpcode(ADC) & MatchAddrMode(2) & MatchParameter(3) & Not(ConcernsX) & DoesNotConcernMemoryAt(0, 1)) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(2) & MatchParameter(3) & Not(ConcernsX) & DoesntMatterWhatItDoesWith(State.C, State.N, State.V, State.Z)) ~~> { (code, ctx) =>
      val label = getNextLabel("in")
      List(
        code(1), // BCC
        code(8).copy(opcode = INC),
        code(3), //LABEL
        code(4), //CLC
        code(5), //ADC
        code(6), //STA
        AssemblyLine.relative(BCC, label),
        code(8).copy(opcode = INC),
        AssemblyLine.label(label))
    },
    (Elidable & HasOpcode(LDX) & HasImmediate(0) & HasClear(State.D)) ~
      (Elidable & HasOpcode(BCC) & MatchParameter(14)) ~
      (Elidable & HasOpcode(INX)) ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(14) & HasCallerCount(1)) ~
      (Elidable & HasOpcode(CLC)) ~
      (Elidable & HasOpcode(ADC) & MatchAddrMode(0) & MatchParameter(1) & Not(ConcernsX)) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1) & Not(ConcernsX)) ~
      (Elidable & HasOpcode(TXA)) ~
      (Elidable & HasOpcode(ADC) & MatchAddrMode(2) & MatchParameter(3) & Not(ConcernsX) & DoesNotConcernMemoryAt(0, 1)) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(2) & MatchParameter(3) & Not(ConcernsX) & DoesntMatterWhatItDoesWith(State.C, State.N, State.V, State.Z)) ~~> { (code, ctx) =>
      val label = getNextLabel("in")
      List(
        code(1), // BCC
        code(8).copy(opcode = INC),
        code(3), //LABEL
        code(4), //CLC
        code(5), //ADC
        code(6), //STA
        AssemblyLine.relative(BCC, label),
        code(8).copy(opcode = INC),
        AssemblyLine.label(label))
    },
    (Elidable & HasOpcode(LDX) & HasAddrMode(Immediate) & HasClear(State.D)) ~
      (Elidable & HasOpcode(BCC) & MatchParameter(14)) ~
      (Elidable & HasOpcode(INX)) ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(14) & HasCallerCount(1)) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1) & Not(ConcernsX)) ~
      (Elidable & HasOpcode(STX) & MatchAddrMode(2) & MatchParameter(3) & DoesNotConcernMemoryAt(0, 1) & Not(HasAddrMode(ZeroPageY)) &
        DoesntMatterWhatItDoesWith(State.X, State.A, State.C, State.V)) ~~> { (code, ctx) =>
      val label = getNextLabel("in")
      List(
        code(4), // STA
        code.head.copy(opcode = LDA), // LDX
        AssemblyLine.immediate(ADC, 0),
        code(5).copy(opcode = STA)) //STX
    },
    (Elidable & HasOpcode(TAX) & HasClear(State.D)) ~
      (Elidable & HasOpcode(BCC) & MatchParameter(14)) ~
      (Elidable & HasOpcode(INX)) ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(14) & HasCallerCount(1)) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1) & Not(ConcernsX)) ~
      (Elidable & HasOpcode(STX) & MatchAddrMode(2) & MatchParameter(3) & DoesNotConcernMemoryAt(0, 1) & Not(HasAddrMode(ZeroPageY)) &
        DoesntMatterWhatItDoesWith(State.X, State.A, State.C, State.V)) ~~> { (code, ctx) =>
      val label = getNextLabel("in")
      List(
        code(4), // STA
        AssemblyLine.immediate(ADC, 0),
        code(5).copy(opcode = STA)) //STX
    },
  )

  val NonetBitOp = new RuleBasedAssemblyOptimization("Nonet bit operation",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    (Elidable & HasOpcode(LDX) & HasImmediate(0)) ~
      (Elidable & HasOpcode(BCC) & MatchParameter(14)) ~
      (Elidable & HasOpcode(INX)) ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(14) & HasCallerCount(1)) ~
      (Elidable & HasOpcodeIn(Set(ORA, EOR)) & MatchAddrMode(0) & MatchParameter(1) & Not(ConcernsX)) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1) & Not(ConcernsX)) ~
      (Elidable & HasOpcode(TXA)) ~
      (Elidable & HasOpcodeIn(Set(ORA, EOR)) & MatchAddrMode(2) & MatchParameter(3) & Not(ConcernsX) & DoesNotConcernMemoryAt(0, 1)) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(2) & MatchParameter(3) & Not(ConcernsX) & DoesntMatterWhatItDoesWith(State.C, State.N, State.V, State.Z)) ~~>{ (code, ctx) =>
        val label = getNextLabel("in")
            List(
              code(4), //EOR/ORA
              code(5), //STA
              code(1), // BCC
              AssemblyLine.immediate(LDA, 1),
              code(7), //EOR/ORA
              code(8), //STA
              code(3)) // LABEL
          }
  )

  val CommonIndexSubexpressionElimination: RuleBasedAssemblyOptimization = {
    def eliminate(firstLda: Boolean, targetY: Boolean): AssemblyRule = {
      val firstLoad = HasClear(State.D) & (
        if (firstLda) HasOpcodeIn(Set(LDA, STA, LAX)) & HasAddrModeIn(Set(Absolute, ZeroPage, Immediate)) & MatchParameter(1)
        else if (targetY) HasOpcodeIn(Set(TXA, TAX))
        else HasOpcodeIn(Set(TAY, TYA))
        )
      val secondLoad = Elidable & (
        if (firstLda) HasOpcode(LDA) & HasAddrModeIn(Set(Absolute, ZeroPage, Immediate)) & MatchParameter(1)
        else if (targetY) HasOpcode(TXA)
        else HasOpcode(TYA)
        )
      val firstTransfer = if (targetY) HasOpcode(TAY) else HasOpcode(TAX)
      val secondTransfer = Elidable & (
        if (targetY) HasOpcode(TAY)
        else HasOpcode(TAX)
        ) & DoesntMatterWhatItDoesWith(State.A, State.Z, State.N, State.C)
      val fillerLine =
        HasAddrMode(Implied) & HasOpcodeIn(Set(ASL, CLC, CLD, SEC, SED, LSR, INC, DEC)) |
          HasOpcodeIn(Set(ADC, ORA, EOR, AND, SBC)) & HasAddrModeIn(Set(Absolute, ZeroPage, Immediate))
      val firstFiller = fillerLine.*
      val secondFiller = (Elidable & fillerLine).*
      val betweenLines = (Linear & Not(secondLoad) & Not(if (targetY) ChangesY else ChangesX)).+
      (firstLoad ~ firstFiller ~ firstTransfer).capture(91) ~ betweenLines.capture(95) ~ (secondLoad ~ secondFiller ~ secondTransfer).capture(92) ~ Where(ctx => {
        val first = ctx.get[List[AssemblyLine]](91)
        val second = ctx.get[List[AssemblyLine]](92)
        val between = ctx.get[List[AssemblyLine]](95)
        var currentD = false
        var currentCDefined = false
        first.length == second.length &&
          first.head.parameter == second.head.parameter &&
          (first.head.addrMode == Immediate) == (second.head.addrMode == Immediate) && first.tail.zip(second.tail).forall(p => {
          p._1.opcode == p._2.opcode && p._1.parameter.quickSimplify == p._2.parameter.quickSimplify && (p._1.addrMode == Immediate) == (p._2.addrMode == Immediate)
        }) && (for (s1 <- first; s2 <- between) yield HelperCheckers.memoryAccessDoesntOverlap(s1, s2)).forall(identity) && {
          var currentD = false
          var currentCDefined = false
          var noAdditionDependency = true
          first.tail.map(_.opcode).foreach{
            case SED => currentD = true
            case CLD => currentD = false
            case CLC | SEC => currentCDefined = true
            case ADC | SBC => noAdditionDependency = currentCDefined
            case _ => ()
          }
          noAdditionDependency && !currentD
        }

      }) ~~> { (code, ctx) =>
        val first = ctx.get[List[AssemblyLine]](91)
        val between = ctx.get[List[AssemblyLine]](95)
        first ++ between
      }
    }
    new RuleBasedAssemblyOptimization("Common index subexpression elimination",
      needsFlowInfo = FlowInfoRequirement.BothFlows,
      eliminate(firstLda = true, targetY = false),
      eliminate(firstLda = false, targetY = false),
      eliminate(firstLda = false, targetY = true),
      eliminate(firstLda = true, targetY = true),
    )
  }

  // this grows the code by one byte, but shaves 1 cycle and allows for optimizing some stores away
  val ConstantPointer = new RuleBasedAssemblyOptimization("Constant pointer optimization",
    needsFlowInfo = FlowInfoRequirement.ForwardFlow,

    (HasOpcode(STA) & MatchA(0) & HasAddrModeIn(Set(Absolute, ZeroPage)) & MatchParameter(4)) ~
      Where(ctx => {
        val lo = ctx.get[Constant](4)
        ctx.addObject(5, lo + 1)
        ctx.addObject(3, ZeroPage)
        true
      }) ~
      (Linear & DoesNotConcernMemoryAt(3,4) & DoesNotConcernMemoryAt(3,5)).* ~
      (HasOpcode(STA) & MatchA(1) & HasAddrModeIn(Set(Absolute, ZeroPage)) & MatchParameter(5)) ~
      Where(ctx => {
        val lo = ctx.get[Int](0) & 0xff
        val hi = ctx.get[Int](1) & 0xff
        ctx.addObject(2, hi * 256 + lo)
        true
      }) ~
      (Linear & DoesNotConcernMemoryAt(3,4) & DoesNotConcernMemoryAt(3,5)).* ~
      (Elidable & MatchParameter(6) & HasAddrModeIn(Set(IndexedZ, IndexedY))) ~~> { (code, ctx) =>
      val addr = ctx.get[Int](2)
      val last = code.last
      code.init :+ last.copy(parameter = NumericConstant(addr, 2), addrMode = if (last.addrMode == IndexedZ) Absolute else AbsoluteY)
    },

    (HasOpcode(STA) & MatchA(0) & HasAddrModeIn(Set(Absolute, ZeroPage)) & MatchParameter(4)) ~
      Where(ctx => {
        val lo = ctx.get[Constant](4)
        ctx.addObject(5, lo + 1)
        ctx.addObject(3, ZeroPage)
        true
      }) ~
      (Linear & DoesNotConcernMemoryAt(3,4) & DoesNotConcernMemoryAt(3,5)).* ~
      (HasOpcode(STX) & MatchX(1) & HasAddrModeIn(Set(Absolute, ZeroPage)) & MatchParameter(5)) ~
      Where(ctx => {
        val lo = ctx.get[Int](0) & 0xff
        val hi = ctx.get[Int](1) & 0xff
        ctx.addObject(2, hi * 256 + lo)
        true
      }) ~
      (Linear & DoesNotConcernMemoryAt(3,4) & DoesNotConcernMemoryAt(3,5)).* ~
      (Elidable & MatchParameter(6) & HasAddrModeIn(Set(IndexedZ, IndexedY))) ~~> { (code, ctx) =>
      val addr = ctx.get[Int](2)
      val last = code.last
      code.init :+ last.copy(parameter = NumericConstant(addr, 2), addrMode = if (last.addrMode == IndexedZ) Absolute else AbsoluteY)
    },
  )

}
