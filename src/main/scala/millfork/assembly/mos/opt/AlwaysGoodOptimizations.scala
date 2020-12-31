package millfork.assembly.mos.opt

import java.util.concurrent.atomic.AtomicInteger

import millfork.CompilationFlag
import millfork.assembly.mos.Opcode._
import millfork.assembly.mos.OpcodeClasses._
import millfork.assembly.mos.{AddrMode, opt, _}
import millfork.assembly.mos.AddrMode._
import millfork.env._
import millfork.error.FatalErrorReporting
import millfork.node.LiteralExpression
import millfork.node.MosNiceFunctionProperty.DoesntChangeA

/**
  * These optimizations should not remove opportunities for more complex optimizations to trigger.
  *
  * @author Karol Stasiak
  */
//noinspection ZeroIndexToHead
object AlwaysGoodOptimizations {

  val LdxAddrModes = Set(Immediate, Absolute, ZeroPage, ZeroPageY, AbsoluteY)
  val LdyAddrModes = Set(Immediate, Absolute, ZeroPage, ZeroPageX, AbsoluteX)

  private def jvmFix(r: => RuleBasedAssemblyOptimization): RuleBasedAssemblyOptimization = r

  val PointlessMath = jvmFix(new RuleBasedAssemblyOptimization("Pointless math",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    (HasOpcode(CLC) & Elidable) ~
      (HasOpcode(ADC) & Elidable & MatchParameter(0)) ~
      (HasOpcode(SEC) & Elidable) ~
      (HasOpcode(SBC) & Elidable & MatchParameter(0) & DoesntMatterWhatItDoesWith(State.C, State.Z, State.V, State.N)) ~~> (_.drop(4)),
    (HasOpcode(LDA) & HasImmediate(0) & Elidable) ~
      (HasOpcode(CLC) & Elidable) ~
      (HasOpcode(ADC) & Elidable & DoesntMatterWhatItDoesWith(State.C, State.Z, State.V, State.N)) ~~> (code => code(2).copy(opcode = LDA) :: code.drop(3)),
    (HasOpcode(CLC) & Elidable) ~
      (HasOpcode(ADC) & MatchImmediate(0) & Elidable) ~
      (HasOpcode(CLC) & Elidable) ~
      (HasOpcode(ADC) & MatchImmediate(1) & Elidable & DoesntMatterWhatItDoesWith(State.C, State.V)) ~~> ((code, ctx) => List(
      AssemblyLine.implied(CLC),
      AssemblyLine.immediate(ADC, (ctx.get[Constant](0) + ctx.get[Constant](1)).quickSimplify),
    )),
    (HasOpcode(AND) & HasImmediate(0x7F) & Elidable & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~
      (Linear & Not(ConcernsA)).* ~
      (HasOpcodeIn(ASL, ROL) & HasAddrMode(Implied) & DoesntMatterWhatItDoesWith(State.C)) ~~> (_.tail),
    (HasOpcodeIn(ORA, EOR) & HasImmediate(0x80) & Elidable & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~
      (Linear & Not(ConcernsA)).* ~
      (HasOpcodeIn(ASL, ROL) & HasAddrMode(Implied) & DoesntMatterWhatItDoesWith(State.C)) ~~> (_.tail),
    (HasOpcode(AND) & HasImmediate(0xFE) & Elidable & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~
      (Linear & Not(ConcernsA)).* ~
      (HasOpcodeIn(LSR, ROR) & HasAddrMode(Implied) & DoesntMatterWhatItDoesWith(State.C)) ~~> (_.tail),
    (HasOpcodeIn(ORA, EOR) & HasImmediate(1) & Elidable & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~
      (Linear & Not(ConcernsA)).* ~
      (HasOpcodeIn(LSR, ROR) & HasAddrMode(Implied) & DoesntMatterWhatItDoesWith(State.C)) ~~> (_.tail),
    (HasOpcode(LDA) & Elidable & MatchParameter(0)) ~
      (HasOpcode(SEC) & Elidable) ~
      (HasOpcode(SBC) & Elidable & MatchParameter(0) & DoesntMatterWhatItDoesWith(State.C, State.V)) ~~> { code =>
      List(AssemblyLine.immediate(LDA, 0).mergePos(code.map(_.source)))
    },
  ))

  val PointlessAccumulatorShifting = new RuleBasedAssemblyOptimization("Pointless accumulator shifting",
      needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    // before:    A=ZXXX_XXXX C=Y
    // after ROL: A=XXXX_XXXY C=Z
    // after LSR: A=0XXX_XXXX C=Y
      (HasOpcode(ROL) & HasAddrMode(Implied) & Elidable) ~
        (HasOpcode(LSR) & HasAddrMode(Implied) & Elidable) ~~> (_ => List(AssemblyLine.immediate(AND, 0x7f))),
    // before:    A=XXXX_XXXZ C=Y
    // after ROR: A=YXXX_XXXX C=Z
    // after ASL: A=XXXX_XXX0 C=Y
      (HasOpcode(ROR) & HasAddrMode(Implied) & Elidable) ~
        (HasOpcode(ASL) & HasAddrMode(Implied) & Elidable) ~~> (_ => List(AssemblyLine.immediate(AND, 0xfe))),
    // before:    A=XXXX_XXXZ C=Y
    // after LSR: A=0XXX_XXXX C=Z
    // after ROL: A=XXXX_XXXZ C=0
      (HasOpcode(LSR) & HasAddrMode(Implied) & Elidable) ~
        (HasOpcode(ROL) & HasAddrMode(Implied) & Elidable & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~~> (_ => List(AssemblyLine.implied(CLC))),
    // before:    A=ZXXX_XXXX C=Y
    // after ASL: A=XXXX_XXX0 C=Z
    // after ROR: A=ZXXX_XXXX C=0
      (HasOpcode(ASL) & HasAddrMode(Implied) & Elidable) ~
        (HasOpcode(ROR) & HasAddrMode(Implied) & Elidable & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~~> (_ => List(AssemblyLine.implied(CLC))),
    // before:    A=XXXX_XXXZ C=Y
    // after ROR: A=YXXX_XXXX C=Z
    // after ROL: A=XXXX_XXXZ C=Y
      (HasOpcode(ROR) & HasAddrMode(Implied) & Elidable) ~
        (HasOpcode(ROL) & HasAddrMode(Implied) & Elidable & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~~> (_ => Nil),
    // before:    A=ZXXX_XXXX C=Y
    // after ROL: A=XXXX_XXXY C=Z
    // after ROR: A=ZXXX_XXXX C=Y
      (HasOpcode(ROL) & HasAddrMode(Implied) & Elidable) ~
        (HasOpcode(ROR) & HasAddrMode(Implied) & Elidable & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~~> (_ => Nil),
    // before:    A=ZXXX_XXXX C=Y
    // after ASL: A=XXXX_XXX0 C=Z
    // after LSR: A=0XXX_XXXX C=0
      (HasOpcode(ASL) & HasAddrMode(Implied) & Elidable) ~
        (HasOpcode(LSR) & HasAddrMode(Implied) & Elidable) ~~> (_ => List(AssemblyLine.immediate(AND, 0x7f), AssemblyLine.implied(CLC))),
    // before:    A=XXXX_XXXZ C=Y
    // after LSR: A=0XXX_XXXX C=Z
    // after ASL: A=XXXX_XXX0 C=0
      (HasOpcode(LSR) & HasAddrMode(Implied) & Elidable) ~
        (HasOpcode(ASL) & HasAddrMode(Implied) & Elidable) ~~> (_ => List(AssemblyLine.immediate(AND, 0xfe), AssemblyLine.implied(CLC))),
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
      HasOpcode(ASL) & HasAddrMode(Implied)) ~~> { (code, ctx) =>
      val v = ctx.get[Int](0)
      AssemblyLine.immediate(LDA, (v << 1) & 0xff) :: AssemblyLine.implied(if(v.&(0x80) != 0) SEC else CLC) :: Nil
    },
    (Elidable & MatchA(0) &
      HasOpcode(LSR) & HasAddrMode(Implied)) ~~> { (code, ctx) =>
      val v = ctx.get[Int](0)
      AssemblyLine.immediate(LDA, (v & 0xff) >> 1) :: AssemblyLine.implied(if(v.&(1) != 0) SEC else CLC) :: Nil
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

    (Elidable & MatchA(0) &
      HasClear(State.C) & HasOpcode(ROL) & HasAddrMode(Implied)) ~~> { (code, ctx) =>
      val v = ctx.get[Int](0)
      AssemblyLine.immediate(LDA, v << 1) :: AssemblyLine.implied(if(v.&(0x80) != 0) SEC else CLC) :: Nil
    },
    (Elidable & MatchA(0) &
      HasClear(State.C) & HasOpcode(ROR) & HasAddrMode(Implied)) ~~> { (code, ctx) =>
      val v = ctx.get[Int](0)
      AssemblyLine.immediate(LDA, (ctx.get[Int](0) & 0xff) >> 1) :: AssemblyLine.implied(if(v.&(1) != 0) SEC else CLC) :: Nil
    },
    (Elidable & MatchA(0) &
      HasSet(State.C) & HasOpcode(ROL) & HasAddrMode(Implied)) ~~> { (code, ctx) =>
      val v = ctx.get[Int](0)
      AssemblyLine.immediate(LDA, (v * 2 + 1) & 0xff) :: AssemblyLine.implied(if(v.&(0x80) != 0) SEC else CLC) :: Nil
    },
    (Elidable & MatchA(0) &
      HasSet(State.C) & HasOpcode(ROR) & HasAddrMode(Implied)) ~~> { (code, ctx) =>
      val v = ctx.get[Int](0)
      AssemblyLine.immediate(LDA, 0x80 + (v & 0xff) / 2) :: AssemblyLine.implied(if(v.&(1) != 0) SEC else CLC) :: Nil
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
      HasClear(State.D) & HasClear(State.C) & DoesntMatterWhatItDoesWith(State.V, State.C)) ~
      Where(ctx => (ctx.get[Constant](1).loByte.quickSimplify + ctx.get[Int](0).&(0xff)).quickSimplify match {
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
      HasOpcode(ADC) & HasAddrMode(Immediate) &
      HasSet(State.D) & HasClear(State.C) & DoesntMatterWhatItDoesWith(State.C, State.V)) ~~> { (code, ctx) =>
      AssemblyLine.immediate(LDA, CompoundConstant(MathOperator.DecimalPlus, ctx.get[Constant](1), NumericConstant(ctx.get[Int](0), 1)).quickSimplify.loByte) :: Nil
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
    (NotFixed & HasA(0) & HasOpcodeIn(ORA, EOR)) ~~> (code => code.map(_.copy(opcode = LDA))),
    (NotFixed & HasA(0) & HasOpcode(ADC) &
      HasClear(State.D) & HasClear(State.C) &
      // C stays cleared!
      DoesntMatterWhatItDoesWith(State.V)) ~~> (code => code.map(_.copy(opcode = LDA))),
    (NotFixed & HasA(0) & HasOpcode(ADC) &
      HasClear(State.C) &
      // C stays cleared!
      DoesntMatterWhatItDoesWith(State.N, State.Z, State.V)) ~~> (code => code.map(_.copy(opcode = LDA))),
    (NotFixed & HasA(0xff) & HasOpcode(AND)) ~~> (code => code.map(_.copy(opcode = LDA))),
    (Elidable & HasA(0) & HasOpcode(AND)) ~~> (code => List(AssemblyLine.immediate(LDA, 0))),
    (Elidable & HasX(0) & HasOpcode(XAA)) ~~> (code => List(AssemblyLine.immediate(LDA, 0))),
    (Elidable & HasImmediate(0) & HasOpcode(AND)) ~~> (code => List(AssemblyLine.immediate(LDA, 0))),
    (Elidable & HasImmediate(0) & HasOpcode(XAA)) ~~> (code => List(AssemblyLine.immediate(LDA, 0))),
    (Elidable & HasImmediate(0xff) & HasOpcode(ORA)) ~~> (code => List(AssemblyLine.immediate(LDA, 0xff))),
  )

  val MathOperationOnTwoIdenticalMemoryOperands = new RuleBasedAssemblyOptimization("Math operation on two identical memory operands",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    (HasOpcodeIn(STA, LDA, LAX) & HasAddrModeIn(ZeroPage, Absolute) & MatchAddrMode(9) & MatchParameter(0)) ~
      (Linear & DoesntChangeMemoryAt(9, 0) & Not(ChangesA)).* ~
      (HasClear(State.D) & HasClear(State.C) & HasOpcode(ADC) & HasAddrModeIn(ZeroPage, Absolute) & MatchParameter(0) & Elidable) ~~> (code => code.init :+ AssemblyLine.implied(ASL).pos(code.last.source)),

    (HasOpcodeIn(STA, LDA) & HasAddrMode(AbsoluteX) & MatchAddrMode(9) & MatchParameter(0)) ~
      (Linear & DoesntChangeMemoryAt(9, 0) & Not(ChangesA) & Not(ChangesX)).* ~
      (HasClear(State.D) & HasClear(State.C) & HasOpcode(ADC) & HasAddrMode(AbsoluteX) & MatchParameter(0) & Elidable) ~~> (code => code.init :+ AssemblyLine.implied(ASL).pos(code.last.source)),

    (HasOpcodeIn(STA, LDA, LAX) & HasAddrMode(AbsoluteY) & MatchAddrMode(9) & MatchParameter(0)) ~
      (Linear & DoesntChangeMemoryAt(9, 0) & Not(ChangesA) & Not(ChangesY)).* ~
      (HasClear(State.D) & HasClear(State.C) & HasOpcode(ADC) & HasAddrMode(AbsoluteY) & MatchParameter(0) & Elidable) ~~> (code => code.init :+ AssemblyLine.implied(ASL).pos(code.last.source)),

    (HasOpcodeIn(STA, LDA, LAX) & HasAddrModeIn(ZeroPage, Absolute) & MatchAddrMode(9) & MatchParameter(0)) ~
      (Linear & DoesntChangeMemoryAt(9, 0) & Not(ChangesA)).* ~
      (DoesntMatterWhatItDoesWith(State.N, State.Z) & HasOpcodeIn(ORA, AND) & HasAddrModeIn(ZeroPage, Absolute) & MatchParameter(0) & Elidable) ~~> (code => code.init),

    (HasOpcodeIn(STA, LDA, LAX) & HasAddrModeIn(ZeroPage, Absolute) & MatchAddrMode(9) & MatchParameter(0)) ~
      (Linear & DoesntChangeMemoryAt(9, 0) & Not(ChangesA)).* ~
      (DoesntMatterWhatItDoesWith(State.N, State.Z, State.C) & HasOpcode(ANC) & HasAddrModeIn(ZeroPage, Absolute) & MatchParameter(0) & Elidable) ~~> (code => code.init),

    (HasOpcodeIn(STA, LDA, LAX) & HasAddrModeIn(ZeroPage, Absolute) & MatchAddrMode(9) & MatchParameter(0)) ~
      (Linear & DoesntChangeMemoryAt(9, 0) & Not(ChangesA)).* ~
      (HasOpcode(EOR) & HasAddrModeIn(ZeroPage, Absolute) & MatchParameter(0) & Elidable) ~~> (code => code.init :+ AssemblyLine.immediate(LDA, 0).pos(code.last.source)),
  )

  val PoinlessStoreBeforeStore = new RuleBasedAssemblyOptimization("Pointless store before store",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (Elidable & HasAddrModeIn(Absolute, ZeroPage) & MatchParameter(1) & MatchAddrMode(2) & HasOpcodeIn(STA, SAX, STX, STY, STZ)) ~
      (LinearOrLabel & DoesNotConcernMemoryAt(2, 1)).* ~
      (MatchParameter(1) & MatchAddrMode(2) & HasOpcodeIn(STA, SAX, STX, STY, STZ)) ~~> (_.tail),
    (Elidable & HasAddrModeIn(AbsoluteX, ZeroPageX) & MatchParameter(1) & MatchAddrMode(2) & HasOpcodeIn(STA, STY, STZ)) ~
      (LinearOrLabel & DoesntChangeMemoryAt(2, 1) & Not(ReadsMemory) & Not(ChangesX)).* ~
      (MatchParameter(1) & MatchAddrMode(2) & HasOpcodeIn(STA, STY, STZ)) ~~> (_.tail),
    (Elidable & HasAddrModeIn(AbsoluteY, ZeroPageY) & MatchParameter(1) & MatchAddrMode(2) & HasOpcodeIn(STA, SAX, STX, STZ)) ~
      (LinearOrLabel & DoesntChangeMemoryAt(2, 1) & Not(ReadsMemory) & Not(ChangesY)).* ~
      (MatchParameter(1) & MatchAddrMode(2) & HasOpcodeIn(STA, SAX, STX, STZ)) ~~> (_.tail),
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
          HasAddrModeIn(IndexedX, IndexedY, IndexedZ, LongIndexedY, LongIndexedZ, Indirect) &
          MatchParameter(3) & HasParameterWhere({
            case MemoryAddressConstant(th) => th.name.startsWith("__")
            case _ => false
          })
        )).* ~
      (Elidable & match2 & HasOpcode(st2) & MatchAddrMode(2) & MatchParameter(3)) ~~> (code => code.init)
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

    (HasOpcode(PHA)) ~
      (Linear & Not(ChangesA) & Not(ChangesStack) & Not(ChangesMemory)).* ~
      (Elidable & XContainsHardwareStackPointer & HasOpcode(STA) & HasAddrMode(AbsoluteX) & HasParameterWhere(p => p.quickSimplify match {
        case NumericConstant(n, _) => n == 0x101
        case _ => false
      })) ~~> (_.init),

    (HasOpcode(PHA)) ~
      (Linear & Not(ChangesA) & Not(ChangesStack) & Not(ChangesMemory)).* ~
      (Elidable & HasOpcode(STA) & HasAddrMode(Stack) & HasParameterWhere(p => p.quickSimplify match {
        case NumericConstant(n, _) => n == 1
        case _ => false
      })) ~~> (_.init)

  )

  val PointlessStashingForLaterStore = new RuleBasedAssemblyOptimization("Pointless stashing for later store",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    // LDA/TAX/TAY/TAZ will be cleaned by something else
    (HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & Linear & DoesNotConcernMemoryAt(0, 1) & DoesntChangeIndexingInAddrMode(0)).*.capture(5) ~
      (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(STA) & HasAddrModeIn(ZeroPage, Absolute, LongAbsolute) & DoesNotConcernMemoryAt(0, 1)).capture(4) ~
      Where(ctx => {
        val sta = ctx.get[List[AssemblyLine]](4).head
        val middleCode = ctx.get[List[AssemblyLine]](5)
        middleCode.forall(line => HelperCheckers.memoryAccessDoesntOverlap(line, sta))
      }) ~~> { code =>
      code.last :: code.init
    },
    (HasOpcode(TAX)) ~
      (Elidable & Linear & Not(ChangesX) & Not(HasOpcode(STX))).*.capture(5) ~
      (Elidable & HasOpcode(STX) & HasAddrModeIn(ZeroPage, Absolute, LongAbsolute)).capture(4) ~
      Where(ctx => {
        val sta = ctx.get[List[AssemblyLine]](4).head
        val middleCode = ctx.get[List[AssemblyLine]](5)
        middleCode.forall(line => HelperCheckers.memoryAccessDoesntOverlap(line, sta))
      }) ~~> { code =>
      code.last.copy(opcode = STA) :: code.init
    },
    (HasOpcode(TAY)) ~
      (Elidable & Linear & Not(ChangesY) & Not(HasOpcode(STY))).*.capture(5) ~
      (Elidable & HasOpcode(STY) & HasAddrModeIn(ZeroPage, Absolute, LongAbsolute)).capture(4) ~
      Where(ctx => {
        val sta = ctx.get[List[AssemblyLine]](4).head
        val middleCode = ctx.get[List[AssemblyLine]](5)
        middleCode.forall(line => HelperCheckers.memoryAccessDoesntOverlap(line, sta))
      }) ~~> { code =>
      code.last.copy(opcode = STA) :: code.init
    },
    (HasOpcode(TAZ)) ~
      (Elidable & Linear & Not(ChangesIZ) & Not(HasOpcode(STZ))).*.capture(5) ~
      (Elidable & HasOpcode(STZ) & HasAddrModeIn(ZeroPage, Absolute, LongAbsolute)).capture(4) ~
      Where(ctx => {
        val sta = ctx.get[List[AssemblyLine]](4).head
        val middleCode = ctx.get[List[AssemblyLine]](5)
        middleCode.forall(line => HelperCheckers.memoryAccessDoesntOverlap(line, sta))
      }) ~~> { code =>
      code.last.copy(opcode = STA) :: code.init
    },
  )

  val PointlessStashingForLaterLoad = jvmFix(new RuleBasedAssemblyOptimization("Pointless stashing for later load",
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
  ))

  val PointlessLoadBeforeReturn = new RuleBasedAssemblyOptimization("Pointless load before return",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (HasOpcodeIn(LDA, TXA, TYA, EOR, AND, ORA, ANC) & Elidable) ~ (LinearOrLabel & Not(ConcernsA) & Not(ReadsNOrZ) & Not(HasOpcode(DISCARD_AF))).* ~ HasOpcode(DISCARD_AF) ~~> (_.tail),
    (HasOpcodeIn(LDX, TAX, TSX, INX, DEX) & Elidable) ~ (LinearOrLabel & Not(ConcernsX) & Not(ReadsNOrZ) & Not(HasOpcode(DISCARD_XF))).* ~ HasOpcode(DISCARD_XF) ~~> (_.tail),
    (HasOpcodeIn(LDY, TAY, INY, DEY) & Elidable) ~ (LinearOrLabel & Not(ConcernsY) & Not(ReadsNOrZ) & Not(HasOpcode(DISCARD_YF))).* ~ HasOpcode(DISCARD_YF) ~~> (_.tail),
    (HasOpcode(LDX) & Elidable & MatchAddrMode(3) & MatchParameter(4)) ~
      (Linear & Not(ConcernsX) & Not(ReadsNOrZ) & DoesntChangeMemoryAtAssumingNonchangingIndices(3, 4) & DoesntChangeIndexingInAddrMode(3)).*.capture(1) ~
      (HasOpcode(TXA) & Elidable) ~
      ((Linear & Not(ConcernsX) & Not(HasOpcode(DISCARD_XF))).* ~
        HasOpcode(DISCARD_XF)).capture(2) ~~> { (c, ctx) =>
      ctx.get[List[AssemblyLine]](1) ++ (c.head.copy(opcode = LDA) :: ctx.get[List[AssemblyLine]](2))
    },
    (HasOpcode(LDY) & Elidable & MatchAddrMode(3) & MatchParameter(4)) ~
      (Linear & Not(ConcernsY) & Not(ReadsNOrZ) & DoesntChangeMemoryAtAssumingNonchangingIndices(3, 4) & DoesntChangeIndexingInAddrMode(3)).*.capture(1) ~
      (HasOpcode(TYA) & Elidable) ~
      ((Linear & Not(ConcernsY) & Not(HasOpcode(DISCARD_YF))).* ~
        HasOpcode(DISCARD_YF)).capture(2) ~~> { (c, ctx) =>
      ctx.get[List[AssemblyLine]](1) ++ (c.head.copy(opcode = LDA) :: ctx.get[List[AssemblyLine]](2))
    },
  )

  val InefficientStashingToRegister = new RuleBasedAssemblyOptimization("Inefficient stashing to register",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,

    (Elidable & HasOpcodeIn(LDA, STA) & IsZeroPage & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(TAX) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~
      (Linear & Not(ConcernsX) & DoesntChangeMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(TXA) & DoesntMatterWhatItDoesWith(State.X)) ~~> (code => code.head :: (code.drop(2).init :+ code.head.copy(opcode = LDA))),

    (Elidable & HasOpcodeIn(LDA, STA) & IsZeroPage & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(TAY) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~
      (Linear & Not(ConcernsY) & DoesntChangeMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(TYA) & DoesntMatterWhatItDoesWith(State.Y)) ~~> (code => code.head :: (code.drop(2).init :+ code.head.copy(opcode = LDA))),

    (Elidable & HasOpcodeIn(LDA, STA) & IsZeroPage & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(TAZ) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~
      (Linear & Not(ConcernsIZ) & DoesntChangeMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(TZA) & DoesntMatterWhatItDoesWith(State.IZ)) ~~> (code => code.head :: (code.drop(2).init :+ code.head.copy(opcode = LDA))),

    (NotFixed & HasOpcodeIn(LDA, STA) & IsZeroPage & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(TAX) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~
      (Elidable & Linear & Not(ConcernsX) & DoesNotConcernMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(TXA) & DoesntMatterWhatItDoesWith(State.X)) ~~> (code => code.head :: (code.drop(2).init :+ code.head.copy(opcode = LDA))),

    (NotFixed & HasOpcodeIn(LDA, STA) & IsZeroPage & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(TAY) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~
      (Elidable & Linear & Not(ConcernsY) & DoesNotConcernMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(TYA) & DoesntMatterWhatItDoesWith(State.Y)) ~~> (code => code.head :: (code.drop(2).init :+ code.head.copy(opcode = LDA))),

    (NotFixed & HasOpcodeIn(LDA, STA) & IsZeroPage & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(TAZ) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~
      (Elidable & Linear & Not(ConcernsIZ) & DoesNotConcernMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(TZA) & DoesntMatterWhatItDoesWith(State.IZ)) ~~> (code => code.head :: (code.drop(2).init :+ code.head.copy(opcode = LDA))),
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
    operationPairBuilder(INY, DEY, Not(ConcernsY) & Not(ReadsNOrZ), None),
    operationPairBuilder(DEY, INY, Not(ConcernsY) & Not(ReadsNOrZ), None),
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
      (middle & IsNotALabelUsedManyTimes).*.capture(1) ~
      Where(_.isExternallyLinearBlock(1)) ~
      (HasOpcode(op2) & Elidable) ~~> { (_, ctx) =>
      ctx.get[List[AssemblyLine]](1).filter(l => !discardToRemove.contains(l.opcode))
    }
  }

  private def operationPairBuilder4(op1: Opcode.Value, op1extra: AssemblyLinePattern, middle: AssemblyLinePattern, op2: Opcode.Value, op2extra: AssemblyLinePattern) = {
    (HasOpcode(op1) & op1extra  & Elidable & HasAddrModeIn(Absolute, ZeroPage, LongAbsolute) & MatchParameter(3)) ~
      (middle & IsNotALabelUsedManyTimes).*.capture(1) ~
      Where(_.isExternallyLinearBlock(1)) ~
      (HasOpcode(op2) & op2extra & Elidable & HasAddrModeIn(Absolute, ZeroPage, LongAbsolute) & MatchParameter(3)) ~~> { (_, ctx) =>
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
    operationPairBuilder3(PHD, Anything, PLD, Not(HasOpcodeIn(ChangesDirectPageRegister)), None),
    operationPairBuilder3(PHB, Anything, PLB, Not(HasOpcodeIn(ChangesDataBankRegister)), None),
    operationPairBuilder3(INX, DoesntMatterWhatItDoesWith(State.N, State.Z), DEX, Not(ConcernsX) & Not(ReadsNOrZ), None),
    operationPairBuilder3(DEX, DoesntMatterWhatItDoesWith(State.N, State.Z), INX, Not(ConcernsX) & Not(ReadsNOrZ), None),
    operationPairBuilder3(INY, DoesntMatterWhatItDoesWith(State.N, State.Z), DEY, Not(ConcernsY) & Not(ReadsNOrZ), None),
    operationPairBuilder3(DEY, DoesntMatterWhatItDoesWith(State.N, State.Z), INY, Not(ConcernsY) & Not(ReadsNOrZ), None),
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

    (Elidable & HasOpcode(ASL) & HasAddrMode(Implied)) ~
      (Linear & Not(ConcernsA) & Not(ConcernsC) & Not(ReadsNOrZ)).* ~
      (Elidable & HasOpcode(ROR) & HasAddrMode(Implied) & DoesntMatterWhatItDoesWith(State.C, State.N, State.Z)) ~~> (_.init.tail),
    (Elidable & HasOpcode(LSR) & HasAddrMode(Implied)) ~
      (Linear & Not(ConcernsA) & Not(ConcernsC) & Not(ReadsNOrZ)).* ~
      (Elidable & HasOpcode(ROL) & HasAddrMode(Implied) & DoesntMatterWhatItDoesWith(State.C, State.N, State.Z)) ~~> (_.init.tail),
    (Elidable & HasOpcode(ROL) & HasAddrMode(Implied)) ~
      (Linear & Not(ConcernsA) & Not(ConcernsC) & Not(ReadsNOrZ)).* ~
      (Elidable & HasOpcode(ROR) & HasAddrMode(Implied) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init.tail),
    (Elidable & HasOpcode(ROR) & HasAddrMode(Implied)) ~
      (Linear & Not(ConcernsA) & Not(ConcernsC) & Not(ReadsNOrZ)).* ~
      (Elidable & HasOpcode(ROL) & HasAddrMode(Implied) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init.tail),
  )

  val PointlessStackStashing = new RuleBasedAssemblyOptimization("Pointless stack stashing",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (Elidable & HasOpcode(LDA) & HasAddrMode(Immediate)) ~
      (Elidable & HasOpcode(PHA)) ~
      (Linear & Not(ConcernsStack) | HasOpcodeIn(JSR, BSR)).* ~
      (Elidable & HasOpcode(PLA)) ~~> { code =>
      code.head :: (code.drop(2).init :+ code.head)
    },
    (Elidable & HasOpcode(LDX) & HasAddrMode(Immediate)) ~
      (Elidable & HasOpcode(PHX)) ~
      (Linear & Not(ConcernsStack) | HasOpcodeIn(JSR, BSR)).* ~
      (Elidable & HasOpcode(PLX)) ~~> { code =>
      code.head :: (code.drop(2).init :+ code.head)
    },
    (Elidable & HasOpcode(LDY) & HasAddrMode(Immediate)) ~
      (Elidable & HasOpcode(PHY)) ~
      (Linear & Not(ConcernsStack) | HasOpcodeIn(JSR, BSR)).* ~
      (Elidable & HasOpcode(PLY)) ~~> { code =>
      code.head :: (code.drop(2).init :+ code.head)
    },
    (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(PHA)) ~
      (Linear & Not(ConcernsStack) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAtAssumingNonchangingIndices(0, 1)).* ~
      (Elidable & HasOpcode(PLA)) ~~> { code =>
      code.head :: (code.drop(2).init :+ code.head)
    },
    (Elidable & HasOpcode(LDX) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(PHX)) ~
      (Linear & Not(ConcernsStack) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAtAssumingNonchangingIndices(0, 1)).* ~
      (Elidable & HasOpcode(PLX)) ~~> { code =>
      code.head :: (code.drop(2).init :+ code.head)
    },
    (Elidable & HasOpcode(LDY) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(PHY)) ~
      (Linear & Not(ConcernsStack) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAtAssumingNonchangingIndices(0, 1)).* ~
      (Elidable & HasOpcode(PLY)) ~~> { code =>
      code.head :: (code.drop(2).init :+ code.head)
    },
    (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(PHA)) ~
      (IsNotALabelUsedManyTimes & Not(ConcernsStack) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAt(0, 1)).*.capture(2) ~
      Where(ctx => ctx.isExternallyLinearBlock(2))~
      (Elidable & HasOpcode(PLA)) ~~> { code =>
      code.head :: (code.drop(2).init :+ code.head)
    },
  )

  val IncrementingIndexRegistersAfterTransfer = new RuleBasedAssemblyOptimization("Incrementing index registers after transfer",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    (Elidable & HasOpcode(CLC) & HasClear(State.D)) ~
      (Elidable & HasOpcode(ADC) & HasImmediate(1)) ~
      (Elidable & HasOpcode(TAY) & DoesntMatterWhatItDoesWith(State.A, State.C, State.V)) ~~> { code =>
      List(AssemblyLine.implied(TAY), AssemblyLine.implied(INY))
    },
    (Elidable & HasOpcode(ADC) & HasImmediate(1) & HasClear(State.D) & HasClear(State.C)) ~
      (Elidable & HasOpcode(TAY) & DoesntMatterWhatItDoesWith(State.A, State.C, State.V)) ~~> { code =>
      List(AssemblyLine.implied(TAY), AssemblyLine.implied(INY))
    },
    (Elidable & HasOpcode(SEC) & HasClear(State.D)) ~
      (Elidable & HasOpcode(SBC) & HasImmediate(1)) ~
      (Elidable & HasOpcode(TAY) & DoesntMatterWhatItDoesWith(State.A, State.C, State.V)) ~~> { code =>
      List(AssemblyLine.implied(TAY), AssemblyLine.implied(DEY))
    },
    (Elidable & HasOpcode(SBC) & HasImmediate(1) & HasClear(State.D) & HasSet(State.C)) ~
      (Elidable & HasOpcode(TAY) & DoesntMatterWhatItDoesWith(State.A, State.C, State.V)) ~~> { code =>
      List(AssemblyLine.implied(TAY), AssemblyLine.implied(DEY))
    },
    (Elidable & HasOpcode(CLC) & HasClear(State.D)) ~
      (Elidable & HasOpcode(ADC) & HasImmediate(1)) ~
      (Elidable & HasOpcode(TAX) & DoesntMatterWhatItDoesWith(State.A, State.C, State.V)) ~~> { code =>
      List(AssemblyLine.implied(TAX), AssemblyLine.implied(INX))
    },
    (Elidable & HasOpcode(ADC) & HasImmediate(1) & HasClear(State.D) & HasClear(State.C)) ~
      (Elidable & HasOpcode(TAX) & DoesntMatterWhatItDoesWith(State.A, State.C, State.V)) ~~> { code =>
      List(AssemblyLine.implied(TAX), AssemblyLine.implied(INX))
    },
    (Elidable & HasOpcode(SEC) & HasClear(State.D)) ~
      (Elidable & HasOpcode(SBC) & HasImmediate(1)) ~
      (Elidable & HasOpcode(TAX) & DoesntMatterWhatItDoesWith(State.A, State.C, State.V)) ~~> { code =>
      List(AssemblyLine.implied(TAX), AssemblyLine.implied(DEX))
    },
    (Elidable & HasOpcode(SBC) & HasImmediate(1) & HasClear(State.D) & HasSet(State.C)) ~
      (Elidable & HasOpcode(TAX) & DoesntMatterWhatItDoesWith(State.A, State.C, State.V)) ~~> { code =>
      List(AssemblyLine.implied(TAX), AssemblyLine.implied(DEX))
    },
    (Elidable & HasOpcode(TXA)) ~
      (Elidable & HasOpcode(CLC) & HasClear(State.D)) ~
      (Elidable & HasOpcode(ADC) & HasImmediate(1)) ~
      (Elidable & HasOpcode(TAX) & DoesntMatterWhatItDoesWith(State.C, State.V)) ~~> { code =>
      List(AssemblyLine.implied(INX), AssemblyLine.implied(TXA))
    },
    (Elidable & HasOpcode(TXA)) ~
      (Elidable & HasOpcode(CLC) & HasClear(State.D)) ~
      (Elidable & HasOpcode(ADC) & HasImmediate(2)) ~
      (Elidable & HasOpcode(TAX) & DoesntMatterWhatItDoesWith(State.C, State.V)) ~~> { code =>
      List(AssemblyLine.implied(INX), AssemblyLine.implied(INX), AssemblyLine.implied(TXA))
    },
    (Elidable & HasOpcode(TYA)) ~
      (Elidable & HasOpcode(CLC) & HasClear(State.D)) ~
      (Elidable & HasOpcode(ADC) & HasImmediate(1)) ~
      (Elidable & HasOpcode(TAY) & DoesntMatterWhatItDoesWith(State.C, State.V)) ~~> { code =>
      List(AssemblyLine.implied(INY), AssemblyLine.implied(TYA))
    },
    (Elidable & HasOpcode(TYA)) ~
      (Elidable & HasOpcode(CLC) & HasClear(State.D)) ~
      (Elidable & HasOpcode(ADC) & HasImmediate(2)) ~
      (Elidable & HasOpcode(TAY) & DoesntMatterWhatItDoesWith(State.C, State.V)) ~~> { code =>
      List(AssemblyLine.implied(INY), AssemblyLine.implied(INY), AssemblyLine.implied(TYA))
    },
  )

  val BranchInPlaceRemoval = new RuleBasedAssemblyOptimization("Branch in place",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (HasOpcodeIn(AllDirectJumps) & HasAddrModeIn(Absolute, Relative, LongAbsolute, LongRelative) & MatchParameter(0) & Elidable) ~
      NoopDiscardsFlags.* ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> (c => c.last :: Nil),
    (HasOpcodeIn(AllDirectJumps) & HasAddrModeIn(Absolute, Relative, LongAbsolute, LongRelative) & MatchParameter(0) & Elidable) ~
      (HasOpcode(LABEL) & Not(MatchParameter(0))).* ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> (_.tail),
    (HasOpcode(BEQ) & MatchParameter(0) & Elidable) ~ HasOpcode(BNE) ~
      (HasOpcode(LABEL) & Not(MatchParameter(0))).* ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> (_.tail),
    (HasOpcode(BNE) & MatchParameter(0) & Elidable) ~ HasOpcode(BEQ) ~
      (HasOpcode(LABEL) & Not(MatchParameter(0))).* ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> (_.tail),
    (HasOpcode(BCC) & MatchParameter(0) & Elidable) ~ HasOpcode(BCS) ~
      (HasOpcode(LABEL) & Not(MatchParameter(0))).* ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> (_.tail),
    (HasOpcode(BCS) & MatchParameter(0) & Elidable) ~ HasOpcode(BCC) ~
      (HasOpcode(LABEL) & Not(MatchParameter(0))).* ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> (_.tail),
    (HasOpcode(BMI) & MatchParameter(0) & Elidable) ~ HasOpcode(BPL) ~
      (HasOpcode(LABEL) & Not(MatchParameter(0))).* ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> (_.tail),
    (HasOpcode(BPL) & MatchParameter(0) & Elidable) ~ HasOpcode(BMI) ~
      (HasOpcode(LABEL) & Not(MatchParameter(0))).* ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> (_.tail),
    (HasOpcode(BVS) & MatchParameter(0) & Elidable) ~ HasOpcode(BVC) ~
      (HasOpcode(LABEL) & Not(MatchParameter(0))).* ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> (_.tail),
    (HasOpcode(BVC) & MatchParameter(0) & Elidable) ~ HasOpcode(BVS) ~
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
    (Elidable & HasOpcode(JMP) & HasAddrModeIn(Absolute, Relative, LongAbsolute, LongRelative) & MatchParameter(0)) ~
      (Elidable & LinearOrBranch).* ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> (code => List(code.last)),
    (Elidable & HasOpcode(BRA) & MatchParameter(0)) ~
      (Elidable & LinearOrBranch).* ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> (code => List(code.last)),
    (Elidable & HasOpcode(JMP) & HasAddrMode(Absolute) & MatchParameter(0)) ~
      (Not(HasOpcode(LABEL)) & Not(MatchParameter(0))).* ~
      ((HasOpcode(LABEL) & MatchParameter(0)) ~
      (HasOpcode(LABEL) | NoopDiscardsFlags).* ~
      HasOpcode(RTS)).capture(1) ~~> ((code, ctx) => ctx.get[List[AssemblyLine]](1) ++ code.tail),
    (Elidable & ShortBranching & MatchParameter(0)) ~
      (NoopDiscardsFlags.* ~
        (Elidable & HasOpcode(RTS)) ~
      (HasOpcode(LABEL) & MatchParameter(0))).capture(1) ~
      NoopDiscardsFlags.* ~
      (Elidable & HasOpcode(RTS)) ~~> ((code, ctx) => ctx.get[List[AssemblyLine]](1)),
  )

  val AlwaysTakenJumpRemoval = new RuleBasedAssemblyOptimization("Always taken jump removal",
    needsFlowInfo = FlowInfoRequirement.ForwardFlow,
    (Elidable & HasOpcode(BEQ) & HasSet(State.Z) & MatchParameter(0)) ~
      (Elidable & LinearOrBranch).* ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> (_ => Nil),
    (Elidable & HasOpcode(BNE) & HasClear(State.Z) & MatchParameter(0)) ~
      (Elidable & LinearOrBranch).* ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> (_ => Nil),
    (Elidable & HasOpcode(BCC) & HasClear(State.C) & MatchParameter(0)) ~
      (Elidable & LinearOrBranch).* ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> (_ => Nil),
    (Elidable & HasOpcode(BCS) & HasSet(State.C) & MatchParameter(0)) ~
      (Elidable & LinearOrBranch).* ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> (_ => Nil),
    (Elidable & HasOpcode(BMI) & HasSet(State.N) & MatchParameter(0)) ~
      (Elidable & LinearOrBranch).* ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> (_ => Nil),
    (Elidable & HasOpcode(BPL) & HasClear(State.N) & MatchParameter(0)) ~
      (Elidable & LinearOrBranch).* ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> (_ => Nil),
    (Elidable & HasOpcode(BVC) & HasClear(State.V) & MatchParameter(0)) ~
      (Elidable & LinearOrBranch).* ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> (_ => Nil),
    (Elidable & HasOpcode(BVS) & HasSet(State.V) & MatchParameter(0)) ~
      (Elidable & LinearOrBranch).* ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> (_ => Nil),
  )

  val TailCallOptimization = new RuleBasedAssemblyOptimization("Tail call optimization",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (Elidable & HasOpcode(JSR)) ~ NoopDiscardsFlags.* ~ (Elidable & HasOpcode(RTS)) ~~> (c => c.head.copy(opcode = JMP) :: Nil),
    (Elidable & HasOpcode(JSR)) ~
      HasOpcode(LABEL).* ~
      NoopDiscardsFlags.*.capture(0) ~
      HasOpcode(RTS) ~~> ((code, ctx) => code.head.copy(opcode = JMP) :: code.tail),
  )

  val UnusedCodeRemoval = new RuleBasedAssemblyOptimization("Unreachable code removal",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    HasOpcode(JMP) ~ (Not(HasOpcode(LABEL)) & Elidable).+ ~~> (c => c.head :: Nil)
  )

  val PoinlessFlagChange = new RuleBasedAssemblyOptimization("Pointless flag change",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (HasOpcodeIn(CMP, CPX, CPY) & Elidable) ~ NoopDiscardsFlags ~~> (_.tail),
    (HasOpcodeIn(OverwritesC) & Elidable & Not(ChangesStack)) ~ (LinearOrLabel & Not(ReadsC) & Not(HasOpcodeIn(DiscardsC))).* ~ HasOpcodeIn(DiscardsC) ~~> (_.tail),
    (HasOpcodeIn(OverwritesD) & Elidable & Not(ChangesStack)) ~ (LinearOrLabel & Not(ReadsD) & Not(HasOpcodeIn(DiscardsD))).* ~ HasOpcodeIn(DiscardsD) ~~> (_.tail),
    (HasOpcodeIn(OverwritesV) & Elidable & Not(ChangesStack)) ~ (LinearOrLabel & Not(ReadsV) & Not(HasOpcodeIn(DiscardsV))).* ~ HasOpcodeIn(DiscardsV) ~~> (_.tail)
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
    (Elidable & HasOpcodeIn(TXA, TYA, LDA, EOR, ORA, AND) & DoesntMatterWhatItDoesWith(State.A, State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcode(ANC) & DoesntMatterWhatItDoesWith(State.A, State.C, State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(TAX, TSX, LDX, INX, DEX) & DoesntMatterWhatItDoesWith(State.X, State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(TAY, LDY, DEY, INY) & DoesntMatterWhatItDoesWith(State.Y, State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(LAX) & DoesntMatterWhatItDoesWith(State.A, State.X, State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(SEC, CLC) & DoesntMatterWhatItDoesWith(State.C)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(CLD, SED) & DoesntMatterWhatItDoesWith(State.D)) ~~> (_ => Nil),
    (Elidable & HasOpcode(CLV) & DoesntMatterWhatItDoesWith(State.V)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(CMP, CPX, CPY) & DoesntMatterWhatItDoesWith(State.C, State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(BIT) & DoesntMatterWhatItDoesWith(State.C, State.N, State.Z, State.V)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(ASL, LSR, ROL, ROR) & HasAddrMode(Implied) & DoesntMatterWhatItDoesWith(State.A, State.C, State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(ADC, SBC) & DoesntMatterWhatItDoesWith(State.A, State.C, State.V, State.N, State.Z)) ~~> (_ => Nil),
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

  val ModificationOfJustWrittenValue = jvmFix(new RuleBasedAssemblyOptimization("Modification of just written value",
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
    (Elidable & HasOpcode(ROL) & Not(HasAddrMode(Implied)) & MatchAddrMode(3) & MatchParameter(0) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~
      (Linear & DoesNotConcernMemoryAt(3, 0) & Not(ConcernsC) & DoesntChangeIndexingInAddrMode(3)).* ~
      (Elidable & HasOpcode(ROR) & Not(HasAddrMode(Implied)) & MatchAddrMode(3) & MatchParameter(0) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init.tail),
    (Elidable & HasOpcode(ROR) & Not(HasAddrMode(Implied)) & MatchAddrMode(3) & MatchParameter(0) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~
      (Linear & DoesNotConcernMemoryAt(3, 0) & Not(ConcernsC) & DoesntChangeIndexingInAddrMode(3)).* ~
      (Elidable & HasOpcode(ROL) & Not(HasAddrMode(Implied)) & MatchAddrMode(3) & MatchParameter(0) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init.tail),
    (Elidable & HasOpcode(INC) & Not(HasAddrMode(Implied)) & MatchAddrMode(3) & MatchParameter(0) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~
      (Linear & DoesNotConcernMemoryAt(3, 0) & DoesntChangeIndexingInAddrMode(3)).* ~
      (Elidable & HasOpcode(DEC) & Not(HasAddrMode(Implied)) & MatchAddrMode(3) & MatchParameter(0) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init.tail),
    (Elidable & HasOpcode(DEC) & Not(HasAddrMode(Implied)) & MatchAddrMode(3) & MatchParameter(0) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~
      (Linear & DoesNotConcernMemoryAt(3, 0) & DoesntChangeIndexingInAddrMode(3)).* ~
      (Elidable & HasOpcode(INC) & Not(HasAddrMode(Implied)) & MatchAddrMode(3) & MatchParameter(0) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init.tail),
  ))

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

  val LoadingOfJustWrittenValue = jvmFix(new RuleBasedAssemblyOptimization("Loading of just written value",
    needsFlowInfo = FlowInfoRequirement.BothFlows,

    (HasOpcode(STA) & XContainsHardwareStackPointer & HasAddrMode(AbsoluteX) & MatchAddrMode(0) & MatchParameter(1) & MatchA(2) & HasParameterWhere(_ match {
      case NumericConstant(addr, _) => addr >= 0x100 && addr <= 0x1ff
      case _ => false
    })) ~
      (HasOpcode(JSR) |
        XContainsHardwareStackPointer & HasOpcodeIn(INC, DEC, ASL, LSR) & MatchAddrMode(0) & MatchParameter(1) |
        Linear & XContainsHardwareStackPointer & HasAddrMode(AbsoluteX) & Not(MatchParameter(1)) & Not(HasOpcodeIn(OpcodeClasses.AccessesWordInMemory)) |
        Linear & HasAddrMode(AbsoluteX) & Not(MatchParameter(1) & XContainsHardwareStackPointer) & Not(ChangesMemory) |
        Linear & Not(ChangesS) & DoesntChangeMemoryAt(0, 1) & Not(HasAddrMode(AbsoluteX)) & Not(HasOpcodeIn(TXA, TXY, HuSAX, SXY, STX, STX_W, PHX, PHX_W, SAX, XAA, SAX, AHX, SHX, TAS))).* ~
      (Elidable & XContainsHardwareStackPointer & HasOpcodeIn(LDA, LDX, LDY, ADC, SBC, ORA, EOR, AND, CMP) & MatchAddrMode(0) & MatchParameter(1)) ~~> { (code, ctx) =>
      val oldA = ctx.get[Int](2)
      val ADDR = ctx.get[Constant](1)
      val value = code.foldLeft(oldA) { (prev, line) =>
        line match {
          case AssemblyLine0(INC, AbsoluteX, ADDR) => (prev + 1) & 0xff
          case AssemblyLine0(DEC, AbsoluteX, ADDR) => (prev - 1) & 0xff
          case AssemblyLine0(ASL, AbsoluteX, ADDR) => (prev << 1) & 0xff
          case AssemblyLine0(LSR, AbsoluteX, ADDR) => (prev >> 1) & 0xff
          case _ => prev
        }
      }
      code.init :+ code.last.copy(addrMode = AddrMode.Immediate, parameter = NumericConstant(value, 1))
    },

    (HasOpcode(STA) & HasAddrMode(Stack) & MatchAddrMode(0) & MatchParameter(1) & MatchA(2)) ~
      (HasOpcode(JSR) |
        Linear & HasAddrMode(Stack) & Not(MatchParameter(1)) & Not(HasOpcodeIn(OpcodeClasses.AccessesWordInMemory)) |
        Linear & Not(ChangesS) & DoesntChangeMemoryAt(0, 1) & Not(HasAddrMode(Stack)) & Not(MatchAddrMode(0) & MatchParameter(1))).* ~
      (Elidable & HasOpcodeIn(LDA, LDX, LDY, ADC, SBC, ORA, EOR, AND, CMP) & MatchAddrMode(0) & MatchParameter(1)) ~~> { (code, ctx) =>
      code.init :+ code.last.copy(addrMode = AddrMode.Immediate, parameter = NumericConstant(ctx.get[Int](2), 1))
    },

    (HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1) & MatchA(2)) ~
      (LinearOrBranch & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAtAssumingNonchangingIndices(0, 1) & Not(MatchAddrMode(0) & MatchParameter(1))).* ~
      (Elidable & HasOpcodeIn(LDA, LDX, LDY, ADC, SBC, ORA, EOR, AND, CMP) & MatchAddrMode(0) & MatchParameter(1)) ~~> { (code, ctx) =>
      code.init :+ code.last.copy(addrMode = AddrMode.Immediate, parameter = NumericConstant(ctx.get[Int](2), 1))
    },

    (HasOpcode(PHA)) ~
      (Linear & Not(ChangesA) & Not(ChangesStack) & Not(ChangesMemory)).* ~
      (Elidable & XContainsHardwareStackPointer & HasOpcode(LDA) & DoesntMatterWhatItDoesWith(State.N, State.Z) & HasAddrMode(AbsoluteX) & HasParameterWhere(p => p.quickSimplify match {
        case NumericConstant(n, _) => n == 0x101
        case _ => false
      })) ~~> (_.init),

    (HasOpcode(PHA)) ~
      (Linear & Not(ChangesA) & Not(ChangesStack) & Not(ChangesMemory)).* ~
      (Elidable & HasOpcode(LDA) & HasAddrMode(Stack) & DoesntMatterWhatItDoesWith(State.N, State.Z) & HasParameterWhere(p => p.quickSimplify match {
        case NumericConstant(n, _) => n == 1
        case _ => false
      })) ~~> (_.init),

    (HasOpcode(PHA)) ~
      (Linear & Not(ChangesA) & Not(ChangesStack) & Not(ChangesMemory)).* ~
      (Elidable & XContainsHardwareStackPointer & HasOpcode(LDA) & HasAddrMode(AbsoluteX) & HasParameterWhere(p => p.quickSimplify match {
        case NumericConstant(n, _) => n == 0x101
        case _ => false
      })) ~~> (code => code.init :+ AssemblyLine.immediate(ORA, 0)),

    (HasOpcode(PHA)) ~
      (Linear & Not(ChangesA) & Not(ChangesStack) & Not(ChangesMemory)).* ~
      (Elidable & HasOpcode(LDA) & HasAddrMode(Stack) & HasParameterWhere(p => p.quickSimplify match {
        case NumericConstant(n, _) => n == 1
        case _ => false
      })) ~~> (code => code.init :+ AssemblyLine.immediate(ORA, 0)),

      (HasOpcode(LDX) & RefersTo("__sp", 0)
        & XContainsSoftwareStackPointer & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_ => Nil),

      (HasOpcodeIn(LAX, LDA) & RefersTo("__sp", 0)
        & XContainsSoftwareStackPointer) ~~> (_ => List(AssemblyLine.implied(TXA))),

    (Elidable & HasOpcodeIn(LDA, LDX, LDY, ADC, SBC, EOR, AND, ORA) & HasAddrModeIn(Absolute, ZeroPage, LongAbsolute) & HasParameterWhere{
      case MemoryAddressConstant(i: InitializedArray) => i.readOnly
      case CompoundConstant(MathOperator.Plus, MemoryAddressConstant(i: InitializedArray), NumericConstant(offset, _)) =>
        i.readOnly && offset >= 0 && offset < i.sizeInBytes
      case CompoundConstant(MathOperator.Plus, NumericConstant(offset, _), MemoryAddressConstant(i: InitializedArray)) =>
        i.readOnly && offset >= 0 && offset < i.sizeInBytes
      case _ => false
    }) ~~> { code =>
      val p = code.head.parameter match {
        case MemoryAddressConstant(i: InitializedArray) =>
          i.contents.head
        case CompoundConstant(MathOperator.Plus, MemoryAddressConstant(i: InitializedArray), NumericConstant(offset, _)) =>
          i.contents(offset.toInt)
        case CompoundConstant(MathOperator.Plus, NumericConstant(offset, _), MemoryAddressConstant(i: InitializedArray)) =>
          i.contents(offset.toInt)
      }
      p match {
        case LiteralExpression(n, s) =>
          code.head.copy(addrMode = Immediate, parameter = NumericConstant(n, s)) :: Nil
        case _ =>
          code
      }
    },
  ))

  val PointlessStackStore = new RuleBasedAssemblyOptimization("Pointless stack store",
    needsFlowInfo = FlowInfoRequirement.BothFlows,

    // TODO: check if JSR is OK

    (Elidable & HasOpcode(STA) & HasAddrMode(AbsoluteX) & XContainsHardwareStackPointer & HasParameterWhere(_ match {
      case NumericConstant(addr, _) => addr >= 0x100 && addr <= 0x1ff
      case _ => false
    }) & MatchParameter(1)) ~
      (HasOpcode(JSR) |
        Not(ChangesS) & Not(HasOpcodeIn(RTS, RTI)) & Linear & Not(HasAddrMode(AbsoluteX)) |
        HasAddrMode(AbsoluteX) & XContainsHardwareStackPointer & Not(MatchParameter(1)) & Not(HasOpcodeIn(OpcodeClasses.AccessesWordInMemory))).* ~
      HasOpcodeIn(RTS, RTI) ~~> (_.tail),

    (Elidable & HasOpcode(STA) & HasAddrMode(AbsoluteX) & HasParameterWhere(_ match {
      case NumericConstant(addr, _) => addr >= 0x100 && addr <= 0x1ff
      case _ => false
    })) ~
      (HasOpcode(JSR) | Not(HasOpcodeIn(RTS, RTI)) & Linear & Not(HasAddrMode(AbsoluteX))).* ~
      HasOpcodeIn(RTS, RTI) ~~> (_.tail),

    (Elidable & HasOpcodeIn(INC, DEC, ASL, LSR, ROR, ROL) & HasAddrMode(AbsoluteX) & HasParameterWhere(_ match {
      case NumericConstant(addr, _) => addr >= 0x100 && addr <= 0x1ff
      case _ => false
    }) & DoesntMatterWhatItDoesWith(State.Z, State.N, State.C)) ~
      (HasOpcode(JSR) | Not(HasOpcodeIn(RTS, RTI)) & Linear & Not(HasAddrMode(AbsoluteX))).* ~
      HasOpcodeIn(RTS, RTI) ~~> (_.tail),

    (Elidable & HasOpcodeIn(STA, STA_W) & HasAddrMode(Stack)) ~
      (HasOpcode(JSR) | Linear & Not(HasAddrMode(Stack)) & Not(HasOpcodeIn(RTS, RTI))).* ~
      HasOpcodeIn(RTS, RTI) ~~> (_.tail),

    (HasOpcode(STA) & XContainsHardwareStackPointer & HasAddrMode(AbsoluteX) & MatchAddrMode(0) & MatchParameter(1) & HasParameterWhere(_ match {
      case NumericConstant(addr, _) => addr >= 0x100 && addr <= 0x1ff
      case _ => false
    })) ~
      (HasOpcode(JSR) |
        Linear & XContainsHardwareStackPointer & HasAddrMode(AbsoluteX) & Not(MatchParameter(1)) & Not(HasOpcodeIn(OpcodeClasses.AccessesWordInMemory)) |
        Linear & Not(ChangesS) & Not(HasAddrMode(AbsoluteX)) & DoesNotConcernMemoryAt(0, 1)).* ~
    (Elidable & XContainsHardwareStackPointer & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1)) ~~> (_.tail),
  )

  val IdempotentDuplicateRemoval = new RuleBasedAssemblyOptimization("Idempotent duplicate operation",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    HasOpcode(RTS) ~ NoopDiscardsFlags.* ~ (HasOpcode(RTS) ~ Elidable) ~~> (_.take(1)) ::
      HasOpcode(RTI) ~ NoopDiscardsFlags.* ~ (HasOpcode(RTI) ~ Elidable) ~~> (_.take(1)) ::
      HasOpcode(DISCARD_XF) ~ (Not(HasOpcode(DISCARD_XF)) & NoopDiscardsFlagsOrLabel).* ~ HasOpcode(DISCARD_XF) ~~> (_.tail) ::
      HasOpcode(DISCARD_AF) ~ (Not(HasOpcode(DISCARD_AF)) & NoopDiscardsFlagsOrLabel).* ~ HasOpcode(DISCARD_AF) ~~> (_.tail) ::
      HasOpcode(DISCARD_YF) ~ (Not(HasOpcode(DISCARD_YF)) & NoopDiscardsFlagsOrLabel).* ~ HasOpcode(DISCARD_YF) ~~> (_.tail) ::
      List(RTS, RTI, SEC, CLC, CLV, CLD, SED, SEI, CLI, TAX, TXA, TYA, TAY, TXS, TSX).flatMap { opcode =>
        Seq(
          (HasOpcode(opcode) & Elidable) ~ (NoopDiscardsFlags | HasOpcode(LABEL)).* ~ HasOpcode(opcode) ~~> (_.tail),
          HasOpcode(opcode) ~ (HasOpcode(opcode) ~ Elidable) ~~> (_.init),
        )
      }: _*
  )

  lazy val PointlessRegisterTransfers = new RuleBasedAssemblyOptimization("Pointless register transfers",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    HasOpcode(TYA) ~ (Elidable & HasOpcodeIn(TYA, TAY)) ~~> (_.init),
    HasOpcode(TXA) ~ (Elidable & HasOpcodeIn(TXA, TAX)) ~~> (_.init),
    HasOpcode(TAY) ~ (Elidable & HasOpcodeIn(TYA, TAY)) ~~> (_.init),
    HasOpcode(TAX) ~ (Elidable & HasOpcodeIn(TXA, TAX)) ~~> (_.init),
    HasOpcode(TSX) ~ (Elidable & HasOpcodeIn(TXS, TSX)) ~~> (_.init),
    HasOpcode(TXS) ~ (Elidable & HasOpcodeIn(TXS, TSX)) ~~> (_.init),
    HasOpcodeIn(TXA, TAX, LAX, LXA) ~
      (Linear & Not(ChangesNAndZ) & Not(ChangesA) & Not(ChangesX)).* ~
      (Elidable & HasOpcodeIn(TXA, TAX)) ~~> (_.init),
    HasOpcodeIn(TYA, TAY) ~
      (Linear & Not(ChangesNAndZ) & Not(ChangesA) & Not(ChangesX)).* ~
      (Elidable & HasOpcodeIn(TYA, TAY)) ~~> (_.init),
    HasOpcode(TSX) ~ (Not(ChangesX) & Not(ChangesS) & Linear).* ~ (Elidable & HasOpcodeIn(TXS, TSX)) ~~> (_.init),
    HasOpcode(TXS) ~ (Not(ChangesX) & Not(ChangesS) & Linear).* ~ (Elidable & HasOpcodeIn(TXS, TSX)) ~~> (_.init),
    HasOpcodeIn(TXA, TAX) ~ (Not(ChangesA) & Not(ChangesX) & Linear).* ~ (Elidable & HasOpcode(TXA) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~~> (_.init),
    HasOpcodeIn(TXA, TAX) ~ (Not(ChangesA) & Not(ChangesX) & Linear).* ~ (Elidable & HasOpcode(TAX) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~~> (_.init),
    HasOpcodeIn(TYA, TAY) ~ (Not(ChangesA) & Not(ChangesY) & Linear).* ~ (Elidable & HasOpcode(TYA) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~~> (_.init),
    HasOpcodeIn(TYA, TAY) ~ (Not(ChangesA) & Not(ChangesY) & Linear).* ~ (Elidable & HasOpcode(TAY) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~~> (_.init),
  )

  lazy val PointlessRegisterTransfersBeforeStore = new RuleBasedAssemblyOptimization("Pointless register transfers from flow",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    (Elidable & HasOpcode(TXA)) ~
      (Linear & Not(ConcernsA) & Not(ConcernsX)).* ~
      (Elidable & HasOpcode(STA) & HasAddrModeIn(ZeroPage, ZeroPageY, Absolute) & DoesntMatterWhatItDoesWith(State.A, State.N, State.Z)) ~~> (code => code.tail.init :+ code.last.copy(opcode = STX)),
    (Elidable & HasOpcode(TYA)) ~
      (Linear & Not(ConcernsA) & Not(ConcernsY)).* ~
      (Elidable & HasOpcode(STA) & HasAddrModeIn(ZeroPage, ZeroPageX, Absolute) & DoesntMatterWhatItDoesWith(State.A, State.N, State.Z)) ~~> (code => code.tail.init :+ code.last.copy(opcode = STY)),

    (Elidable & HasOpcode(TYA)) ~
      (HasOpcodeIn(OpcodeClasses.ShortBranching) & MatchParameter(0)) ~
      (Elidable & HasOpcode(LDY)) ~
      (HasOpcode(LABEL) & MatchParameter(0) & HasCallerCount(1)) ~
      (Elidable & HasOpcode(TYA) & DoesntMatterWhatItDoesWith(State.Y, State.N, State.Z)) ~~> (code => List(code.head, code(1), code(2).copy(opcode = LDA), code(3))),

    (Elidable & HasOpcode(TXA)) ~
      (HasOpcodeIn(OpcodeClasses.ShortBranching) & MatchParameter(0)) ~
      (Elidable & HasOpcode(LDX) & Not(HasAddrMode(ZeroPageY))) ~
      (HasOpcode(LABEL) & MatchParameter(0) & HasCallerCount(1)) ~
      (Elidable & HasOpcode(TXA) & DoesntMatterWhatItDoesWith(State.X, State.N, State.Z)) ~~> (code => List(code.head, code(1), code(2).copy(opcode = LDA), code(3))),

    (Elidable & HasOpcode(LDA) & HasAddrModeIn(LdyAddrModes)) ~
      (Elidable & HasOpcode(TAY) & DoesntMatterWhatItDoesWith(State.A)) ~~> (code => List(code.head.copy(opcode = LDY))),
    (Elidable & HasOpcode(LDA) & HasAddrModeIn(LdxAddrModes)) ~
      (Elidable & HasOpcode(TAX) & DoesntMatterWhatItDoesWith(State.A)) ~~> (code => List(code.head.copy(opcode = LDX))),
  )


  val PointlessRegisterTransfersBeforeReturn = new RuleBasedAssemblyOptimization("Pointless register transfers before return",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (HasOpcode(TAX) & Elidable) ~
      HasOpcode(LABEL).* ~
      HasOpcode(TXA).? ~
      ManyWhereAtLeastOne(NoopDiscardsFlags, HasOpcode(DISCARD_XF)).capture(1) ~
      HasOpcode(RTS) ~~> ((code, ctx) => ctx.get[List[AssemblyLine]](1) ++ (AssemblyLine.implied(RTS) :: code.tail)),
    (HasOpcode(TSX) & Elidable) ~
      HasOpcode(LABEL).* ~
      HasOpcode(TSX).? ~
      ManyWhereAtLeastOne(NoopDiscardsFlags, HasOpcode(DISCARD_XF)).capture(1) ~
      HasOpcode(RTS) ~~> ((code, ctx) => ctx.get[List[AssemblyLine]](1) ++ (AssemblyLine.implied(RTS) :: code.tail)),
    (HasOpcode(TXA) & Elidable) ~
      HasOpcode(LABEL).* ~
      HasOpcode(TAX).? ~
      ManyWhereAtLeastOne(NoopDiscardsFlags, HasOpcode(DISCARD_AF)).capture(1) ~
      HasOpcode(RTS) ~~> ((code, ctx) => ctx.get[List[AssemblyLine]](1) ++ (AssemblyLine.implied(RTS) :: code.tail)),
    (HasOpcode(TAY) & Elidable) ~
      HasOpcode(LABEL).* ~
      HasOpcode(TYA).? ~
      ManyWhereAtLeastOne(NoopDiscardsFlags, HasOpcode(DISCARD_YF)).capture(1) ~
      HasOpcode(RTS) ~~> ((code, ctx) => ctx.get[List[AssemblyLine]](1) ++ (AssemblyLine.implied(RTS) :: code.tail)),
    (HasOpcode(TYA) & Elidable) ~
      HasOpcode(LABEL).* ~
      HasOpcode(TAY).? ~
      ManyWhereAtLeastOne(NoopDiscardsFlags, HasOpcode(DISCARD_AF)).capture(1) ~
      HasOpcode(RTS) ~~> ((code, ctx) => ctx.get[List[AssemblyLine]](1) ++ (AssemblyLine.implied(RTS) :: code.tail)),
  )

  val PointlessRegisterTransfersBeforeCompare = new RuleBasedAssemblyOptimization("Pointless register transfers and loads before compare",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    HasOpcodeIn(DEX, INX, LDX, LAX) ~
      (HasOpcode(TXA) & Elidable & DoesntMatterWhatItDoesWith(State.A)) ~~> (code => code.init),
    HasOpcodeIn(DEY, INY, LDY) ~
      (HasOpcode(TYA) & Elidable & DoesntMatterWhatItDoesWith(State.A)) ~~> (code => code.init),
    (HasOpcodeIn(DEC, INC, ASL, ROL, ROR, LSR, SLO, SRE, RLA, RRA, ISC, DCP) & MatchAddrMode(0) & MatchParameter(1)) ~
      (HasOpcode(LDA) & Elidable & DoesntMatterWhatItDoesWith(State.A) & MatchAddrMode(0) & MatchParameter(1)) ~~> (code => code.init),
    (HasOpcodeIn(DEC, INC, ASL, ROL, ROR, LSR, SLO, SRE, RLA, RRA, ISC, DCP) & MatchAddrMode(0) & MatchParameter(1)) ~
      (HasOpcode(LDX) & Elidable & DoesntMatterWhatItDoesWith(State.X) & MatchAddrMode(0) & MatchParameter(1)) ~~> (code => code.init),
    (HasOpcodeIn(DEC, INC, ASL, ROL, ROR, LSR, SLO, SRE, RLA, RRA, ISC, DCP) & MatchAddrMode(0) & MatchParameter(1)) ~
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
      (Linear & Not(readsI) & Not(ReadsNOrZ | NoopDiscardsFlags)).* ~
        ManyWhereAtLeastOne(NoopDiscardsFlags, HasOpcode(discardIF)) ~
        HasOpcodeIn(RTS, RTI) ~
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
    (HasOpcodeIn(LDA, TXA, TYA) & Elidable) ~ (LinearOrLabel & Not(ConcernsA) & Not(ReadsNOrZ)).* ~ OverwritesA ~~> (_.tail),
    (HasOpcodeIn(LDX, TAX, TSX) & Elidable) ~ (LinearOrLabel & Not(ConcernsX) & Not(ReadsNOrZ)).* ~ OverwritesX ~~> (_.tail),
    (HasOpcodeIn(LDY, TAY) & Elidable) ~ (LinearOrLabel & Not(ConcernsY) & Not(ReadsNOrZ)).* ~ OverwritesY ~~> (_.tail),
  )

  // TODO: better proofs that memory doesn't change
  val PointlessLoadAfterLoadOrStore = new RuleBasedAssemblyOptimization("Pointless load after load or store",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,

    (HasOpcodeIn(LDA, STA) & HasAddrMode(Immediate) & MatchParameter(1)) ~
      (Linear & Not(ChangesA) & Not(HasOpcode(DISCARD_AF)) | HasOpcodeIn(OpcodeClasses.ShortConditionalBranching)).* ~
      (Elidable & HasOpcode(LDA) & HasAddrMode(Immediate) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init),

    (HasOpcodeIn(LDX, STX) & HasAddrMode(Immediate) & MatchParameter(1)) ~
      (Linear & Not(ChangesX) & Not(HasOpcode(DISCARD_XF)) | HasOpcodeIn(OpcodeClasses.ShortConditionalBranching)).* ~
      (Elidable & HasOpcode(LDX) & HasAddrMode(Immediate) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init),

    (HasOpcodeIn(LDY, STY) & HasAddrMode(Immediate) & MatchParameter(1)) ~
      (Linear & Not(ChangesY) & Not(HasOpcode(DISCARD_YF)) | HasOpcodeIn(OpcodeClasses.ShortConditionalBranching)).* ~
      (Elidable & HasOpcode(LDY) & HasAddrMode(Immediate) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init),

    (HasOpcodeIn(LDA, STA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(ChangesA) & Not(HasOpcode(DISCARD_AF)) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAt(0, 1) | HasOpcodeIn(OpcodeClasses.ShortConditionalBranching)).* ~
      (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init),

    (HasOpcodeIn(LDX, STX) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(ChangesX) & Not(HasOpcode(DISCARD_XF)) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAt(0, 1) | HasOpcodeIn(OpcodeClasses.ShortConditionalBranching)).* ~
      (Elidable & HasOpcode(LDX) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init),

    (HasOpcodeIn(LDY, STY) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(ChangesY) & Not(HasOpcode(DISCARD_YF)) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAt(0, 1) | HasOpcodeIn(OpcodeClasses.ShortConditionalBranching)).* ~
      (Elidable & HasOpcode(LDY) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init),

    (HasOpcode(LDA) & HasAddrMode(Immediate) & MatchParameter(1)) ~
      (Linear & Not(ChangesA) & Not(ChangesNAndZ) & Not(HasOpcode(DISCARD_AF)) | HasOpcodeIn(OpcodeClasses.ShortConditionalBranching)).* ~
      (Elidable & HasOpcode(LDA) & HasAddrMode(Immediate) & MatchParameter(1)) ~~> (_.init),

    (HasOpcode(LDX) & HasAddrMode(Immediate) & MatchParameter(1)) ~
      (Linear & Not(ChangesX) & Not(ChangesNAndZ) & Not(HasOpcode(DISCARD_XF)) | HasOpcodeIn(OpcodeClasses.ShortConditionalBranching)).* ~
      (Elidable & HasOpcode(LDX) & HasAddrMode(Immediate) & MatchParameter(1)) ~~> (_.init),

    (HasOpcode(LDY) & HasAddrMode(Immediate) & MatchParameter(1)) ~
      (Linear & Not(ChangesY) & Not(ChangesNAndZ) & Not(HasOpcode(DISCARD_YF)) | HasOpcodeIn(OpcodeClasses.ShortConditionalBranching)).* ~
      (Elidable & HasOpcode(LDY) & HasAddrMode(Immediate) & MatchParameter(1)) ~~> (_.init),

    (HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(ChangesA) & Not(ChangesNAndZ) & Not(HasOpcode(DISCARD_AF)) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAt(0, 1) | HasOpcodeIn(OpcodeClasses.ShortConditionalBranching)).* ~
      (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~~> (_.init),

    (HasOpcode(LDX) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(ChangesX) & Not(ChangesNAndZ) & Not(HasOpcode(DISCARD_XF)) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAt(0, 1) | HasOpcodeIn(OpcodeClasses.ShortConditionalBranching)).* ~
      (Elidable & HasOpcode(LDX) & MatchAddrMode(0) & MatchParameter(1)) ~~> (_.init),

    (HasOpcode(LDY) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(ChangesY) & Not(ChangesNAndZ) & Not(HasOpcode(DISCARD_YF)) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAt(0, 1) | HasOpcodeIn(OpcodeClasses.ShortConditionalBranching)).* ~
      (Elidable & HasOpcode(LDY) & MatchAddrMode(0) & MatchParameter(1)) ~~> (_.init),

    (HasOpcodeIn(LDA, STA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (ShortConditionalBranching & MatchParameter(2)) ~
      (Linear & Not(ChangesA) & Not(HasOpcode(DISCARD_AF)) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAt(0, 1) | HasOpcodeIn(OpcodeClasses.ShortConditionalBranching)).* ~
      (HasOpcode(LABEL) & MatchParameter(2)) ~
      (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init),

    (HasOpcodeIn(LDX, STX) & MatchAddrMode(0) & MatchParameter(1)) ~
      (ShortConditionalBranching & MatchParameter(2)) ~
      (Linear & Not(ChangesX) & Not(HasOpcode(DISCARD_XF)) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAt(0, 1) | HasOpcodeIn(OpcodeClasses.ShortConditionalBranching)).* ~
      (HasOpcode(LABEL) & MatchParameter(2)) ~
      (Elidable & HasOpcode(LDX) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init),

    (HasOpcodeIn(LDY, STY) & MatchAddrMode(0) & MatchParameter(1)) ~
      (ShortConditionalBranching & MatchParameter(2)) ~
      (Linear & Not(ChangesY) & Not(HasOpcode(DISCARD_YF)) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAt(0, 1) | HasOpcodeIn(OpcodeClasses.ShortConditionalBranching)).* ~
      (HasOpcode(LABEL) & MatchParameter(2)) ~
      (Elidable & HasOpcode(LDY) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init),

    (HasOpcodeIn(LDA, STA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (ShortBranching & MatchParameter(3)) ~
      (Linear & Not(ChangesA) & Not(HasOpcode(DISCARD_AF)) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAt(0, 1) | HasOpcodeIn(OpcodeClasses.ShortConditionalBranching)).* ~
      (HasOpcode(LABEL) & MatchParameter(3) & HasCallerCount(1)) ~
      (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init),

    HasOpcodeIn(TXA, TAX, LAX, LXA) ~
      (Not(HasOpcodeIn(TXA, TAX)) & Linear & Not(ChangesA) & Not(ChangesX) | HasOpcodeIn(OpcodeClasses.ShortConditionalBranching)).* ~
      (Elidable & HasOpcodeIn(TXA, TAX) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init),

    HasOpcodeIn(TYA, TAY) ~
      (Not(HasOpcodeIn(TYA, TAY)) & Linear & Not(ChangesA) & Not(ChangesY) | HasOpcodeIn(OpcodeClasses.ShortConditionalBranching)).* ~
      (Elidable & HasOpcodeIn(TYA, TAY) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init),

    (HasOpcodeIn(STA, LDA) & HasAddrModeIn(ZeroPage, Absolute) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(HasOpcode(TAX)) & Not(ChangesA) & DoesntChangeMemoryAt(0, 1) | HasOpcodeIn(OpcodeClasses.ShortConditionalBranching)).* ~
      HasOpcode(TAX) ~
      (LinearOrBranch & Not(ChangesX) & DoesntChangeMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(LDX) & HasAddrModeIn(ZeroPage, Absolute) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init),

    (HasOpcodeIn(STA, LDA) & HasAddrModeIn(ZeroPage, Absolute) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(HasOpcode(TAY)) & Not(ChangesA) & DoesntChangeMemoryAt(0, 1) | HasOpcodeIn(OpcodeClasses.ShortConditionalBranching)).* ~
      HasOpcode(TAY) ~
      (LinearOrBranch & Not(ChangesY) & DoesntChangeMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(LDY) & HasAddrModeIn(ZeroPage, Absolute) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_.init),

    (HasOpcodeIn(LDA, STA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(ChangesA) & Not(HasOpcode(DISCARD_AF)) & DoesntChangeIndexingInAddrMode(0) & DoesNotConcernMemoryAt(0, 1) | HasOpcodeIn(OpcodeClasses.ShortConditionalBranching)).* ~
      (Elidable & HasOpcode(LDX) & MatchAddrMode(0) & MatchParameter(1)) ~~> (code => code.init :+ AssemblyLine.implied(TAX)),

    (HasOpcodeIn(LDA, STA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(ChangesA) & Not(HasOpcode(DISCARD_AF)) & DoesntChangeIndexingInAddrMode(0) & DoesNotConcernMemoryAt(0, 1) | HasOpcodeIn(OpcodeClasses.ShortConditionalBranching)).* ~
      (Elidable & HasOpcode(LDY) & MatchAddrMode(0) & MatchParameter(1)) ~~> (code => code.init :+ AssemblyLine.implied(TAY)),
  )

  val RearrangableLoadFromTheSameLocation = new RuleBasedAssemblyOptimization("Rearrangable load from the same location",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,

    (HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (HasOpcode(STA) & Not(ReadsX)) ~
      (Elidable & HasOpcode(LDX) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> { code =>
      List(code.head, AssemblyLine.implied(TAX), code(1))
    },
    (HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (HasOpcode(STA) & Not(ReadsY)) ~
      (Elidable & HasOpcode(LDY) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> {code =>
      List(code.head, AssemblyLine.implied(TAY), code(1))
    },
    (HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (HasOpcode(LDA) & Not(ReadsX)) ~
      (Elidable & HasOpcode(LDX) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> {code =>
      List(code.head, AssemblyLine.implied(TAX), code(1))
    },
    (HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (HasOpcode(LDA) & Not(ReadsY)) ~
      (Elidable & HasOpcode(LDY) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> {code =>
      List(code.head, AssemblyLine.implied(TAY), code(1))
    },
    (HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(ChangesA) & Not(ChangesNAndZ) & DoesntChangeMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> {code =>
      code.init
    },
    (HasOpcode(LDX) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(ChangesX) & Not(ChangesNAndZ) & DoesntChangeMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(LDX) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> {code =>
      code.init
    },
    (HasOpcode(LDY) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(ChangesY) & Not(ChangesNAndZ) & DoesntChangeMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(LDY) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> {code =>
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
    (Elidable & HasOpcodeIn(LDA, TXA, TYA, AND, EOR, ORA, XAA) & DoesntMatterWhatItDoesWith(State.A, State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(LDX, TSX, TAX, SBX, INX, DEX) & DoesntMatterWhatItDoesWith(State.X, State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(LDY, TAY, INY, DEY) & DoesntMatterWhatItDoesWith(State.Y, State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(LAX, LXA) & DoesntMatterWhatItDoesWith(State.A, State.X, State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(ANC, ALR) & DoesntMatterWhatItDoesWith(State.A, State.N, State.Z, State.C)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(ADC, SBC, ARR) & DoesntMatterWhatItDoesWith(State.A, State.N, State.Z, State.C, State.V)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(CMP, CPY, CPX, BIT) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.C, State.V)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(ROL, ROR, ASL, LSR) & HasAddrMode(Implied) & DoesntMatterWhatItDoesWith(State.A, State.N, State.Z, State.C)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(INC, DEC) & HasAddrMode(Implied) & DoesntMatterWhatItDoesWith(State.A, State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(CLC, SEC) & DoesntMatterWhatItDoesWith(State.C)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(CLD, SED) & DoesntMatterWhatItDoesWith(State.D)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(CLV) & DoesntMatterWhatItDoesWith(State.V)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(ORA, EOR) & HasImmediate(0) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_ => Nil),
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

    (Elidable & HasOpcodeIn(PHA, PHX, PHY)).*.captureLength(0) ~
      (HasOpcode(TSX) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~
      (Linear & Not(ConcernsS) & Not(ConcernsX) & Not(ChangesStack)).* ~
      (Elidable & HasOpcode(INX)).*.captureLength(1) ~
      Where(ctx => ctx.get[Int](0) >= ctx.get[Int](1)) ~
      (HasOpcode(TXS) & DoesntMatterWhatItDoesWith(State.Z, State.N, State.X)) ~~> {(code, ctx) =>
      val pushCount = ctx.get[Int](0)
      val inxCount = ctx.get[Int](1)
      code.take(pushCount - inxCount) ++ code.drop(pushCount + 1).dropRight(inxCount + 1)
    },
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
    (Elidable & HasOpcode(ROR) & HasAddrMode(Implied)) ~
      (Linear & Not(ChangesA) & Not(ReadsNOrZ) & Not(ReadsC) & Not(ReadsA)).* ~
      (HasOpcodeIn(AND, ANC) & MatchNumericImmediate(1)) ~
      Where(ctx => ctx.get[Int](1).&(0x80) == 0)~~> { (lines, ctx) =>
      lines.head.copy(opcode = LSR) :: lines.tail
    },
    (Elidable & HasOpcode(ROL) & HasAddrMode(Implied)) ~
      (Linear & Not(ChangesA) & Not(ReadsNOrZ) & Not(ReadsC) & Not(ReadsA)).* ~
      (HasOpcodeIn(AND, ANC) & MatchNumericImmediate(1)) ~
      Where(ctx => ctx.get[Int](1).&(1) == 0)~~> { (lines, ctx) =>
      lines.head.copy(opcode = ASL) :: lines.tail
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
      (Elidable & HasOpcodeIn(CLC, SEC)) ~
      (Elidable & HasOpcode(ADC) & Not(HasAddrMode(Immediate))) ~~> { c =>
      c.last.copy(opcode = LDA) :: c(1) :: c.head.copy(opcode = ADC) :: Nil
    },
    (Elidable & HasOpcode(LDA) & HasAddrMode(Immediate)) ~
      (Elidable & HasOpcodeIn(ADC, EOR, ORA, AND) & Not(HasAddrMode(Immediate))) ~~> { c =>
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

  private def byteToWordThenShiftLeft(shiftAmountAfterStore: Int, shiftBeforeStore: Boolean) = {

    val originalStart = if (shiftBeforeStore) {
      (Elidable & HasOpcode(ASL) & HasAddrMode(Implied)) ~
        (Elidable & HasOpcode(STA) & HasAddrModeIn(Absolute, ZeroPage) & MatchParameter(0)) ~
        (Elidable & HasOpcode(LDA) & HasImmediate(0)) ~
        (Elidable & HasOpcode(ROL) & HasAddrMode(Implied)) ~
        (Elidable & HasOpcode(STA) & HasAddrModeIn(Absolute, ZeroPage) & MatchParameter(1))
    } else {
      (Elidable & HasOpcode(STA) & HasAddrModeIn(Absolute, ZeroPage) & MatchParameter(0)) ~
        (Elidable & HasOpcode(LDA) & HasImmediate(0)) ~
        (Elidable & HasOpcode(STA) & HasAddrModeIn(Absolute, ZeroPage) & MatchParameter(1))
    }
    val shifting = (0 until shiftAmountAfterStore).map(_ =>
      (Elidable & HasOpcode(ASL) & HasAddrModeIn(Absolute, ZeroPage) & MatchParameter(0)) ~
        (Elidable & HasOpcode(ROL) & HasAddrModeIn(Absolute, ZeroPage) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.A))
    ).reduce(_ ~ _)

    val rightShiftCount = 8 - (if (shiftBeforeStore) shiftAmountAfterStore + 1 else shiftAmountAfterStore)
    assert(rightShiftCount >= 0)
    originalStart ~ shifting ~~> { (code, ctx) =>
      if (rightShiftCount == 0) {
        List(
          AssemblyLine.absolute(STA, ctx.get[Constant](1)),
          AssemblyLine.immediate(LDA, 0),
          AssemblyLine.absolute(STA, ctx.get[Constant](0)),
          AssemblyLine.implied(CLC))
      } else {
        val improvedStart = List(
          AssemblyLine.implied(LSR),
          AssemblyLine.absolute(STA, ctx.get[Constant](1)),
          AssemblyLine.immediate(LDA, 0),
          AssemblyLine.implied(ROR),
          AssemblyLine.absolute(STA, ctx.get[Constant](0)))
        val improvedShifting = if (rightShiftCount == 1) List(AssemblyLine.implied(CLC)) else (1 until rightShiftCount).flatMap(_ => List(
          AssemblyLine.absolute(LSR, ctx.get[Constant](1)),
          AssemblyLine.absolute(ROR, ctx.get[Constant](0))))
        improvedStart ++ improvedShifting
      }
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
    byteToWordThenShiftLeft(8, false),
    byteToWordThenShiftLeft(7, false),
    byteToWordThenShiftLeft(7, true),
    byteToWordThenShiftLeft(6, false),
    byteToWordThenShiftLeft(6, true),
    byteToWordThenShiftLeft(5, false),
    byteToWordThenShiftLeft(5, true),
    byteToWordThenShiftLeft(4, true),
  )

  val ShiftingJustWrittenValue = new RuleBasedAssemblyOptimization("Shifting just written value",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.C, State.Z, State.N, State.A)) ~
      (Linear & DoesNotConcernMemoryAt(0, 1) & DoesntChangeIndexingInAddrMode(0)).* ~
      (Elidable & HasOpcodeIn(ASL, LSR) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.C, State.Z, State.N)) ~~> { code =>
      code.last.copy(addrMode = AddrMode.Implied, parameter = Constant.Zero) :: code.init
    },
    (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.C, State.Z, State.N, State.A)) ~
      (Linear & DoesNotConcernMemoryAt(0, 1) & DoesntChangeIndexingInAddrMode(0) & Not(ChangesC)).* ~
      (Elidable & HasOpcodeIn(ASL, LSR) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~~> { code =>
      code.last.copy(addrMode = AddrMode.Implied, parameter = Constant.Zero) :: code.init
    },
    (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.Z, State.N, State.A)) ~
      (Linear & DoesNotConcernMemoryAt(0, 1) & DoesntChangeIndexingInAddrMode(0) & Not(ChangesC)).* ~
      (Elidable & HasOpcodeIn(ROL, ROR) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~~> { code =>
      code.last.copy(addrMode = AddrMode.Implied, parameter = Constant.Zero) :: code.init
    },
    (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & DoesNotConcernMemoryAt(0, 1) & DoesntChangeIndexingInAddrMode(0) & Not(ConcernsA)).* ~
      (Elidable & HasOpcodeIn(ROL, ROR, LSR, ASL) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.A)) ~~> { code =>
      code.tail.init ++ List(code.last.copy(addrMode = AddrMode.Implied, parameter = Constant.Zero), code.head)
    },
  )

  val SmarterShiftingBytes = new RuleBasedAssemblyOptimization("Smarter shifting of bytes",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    // TODO: Check flags:
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
    val elseLabel = (Elidable & HasOpcode(LABEL) & MatchParameter(0) & IsNotALabelUsedManyTimes).capture(10)
    val afterLabel = Elidable & HasOpcode(LABEL) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.C, State.N, State.V, State.Z) & IsNotALabelUsedManyTimes
    val store = Elidable & (Not(ReadsC) & Linear | HasOpcodeIn(RTS, JSR, RTI, RTL, BSR))
    val secondReturn = (Elidable & (HasOpcodeIn(RTS, RTI) | NoopDiscardsFlags)).*.capture(6)
    val where = Where { ctx =>
      ctx.get[List[AssemblyLine]](4) == ctx.get[List[AssemblyLine]](5) ||
        ctx.get[List[AssemblyLine]](4) == ctx.get[List[AssemblyLine]](5) ++ ctx.get[List[AssemblyLine]](6)
    }
    val pattern =
      if (firstSet) test ~ ifSet ~ store.*.capture(4) ~ jump ~ elseLabel ~ ifClear ~ store.*.capture(5) ~ afterLabel ~ secondReturn ~ where
      else test ~ ifClear ~ store.*.capture(4) ~ jump ~ elseLabel ~ ifSet ~ store.*.capture(5) ~ afterLabel ~ secondReturn ~ where
    pattern ~~> { (_, ctx) =>
      val elseLabelUseCount = ctx.get[Constant](0) match {
        case MemoryAddressConstant(Label(label)) => ctx.labelUseCount(label)
        case _ => 9999
      }
      List(
        AssemblyLine.immediate(LDA, 0),
        AssemblyLine.implied(if (shift >= 4) ROR else ROL)) ++
        (if (shift >= 4) List.fill(7 - shift)(AssemblyLine.implied(LSR)) else List.fill(shift)(AssemblyLine.implied(ASL))) ++
        (if (zeroIfSet) List(AssemblyLine.immediate(EOR, nonZero)) else Nil) ++
        ctx.get[List[AssemblyLine]](5) ++
        ctx.get[List[AssemblyLine]](6) ++ (
          if (elseLabelUseCount == 1) Nil
          else {
            val storeAndSecondReturn = ctx.get[List[AssemblyLine]](5) ++ ctx.get[List[AssemblyLine]](6)
            val set = Set(RTS, JSR, RTI, RTL, BSR)
            if (storeAndSecondReturn.exists(p => set(p.opcode))) {
              ctx.get[List[AssemblyLine]](10) ++ List(
                AssemblyLine.immediate(LDA, if (firstSet) (if (zeroIfSet) nonZero else 0) else (if (zeroIfSet) 0 else nonZero))
              ) ++ ctx.get[List[AssemblyLine]](5) ++ ctx.get[List[AssemblyLine]](6)
            } else {
              ctx.get[List[AssemblyLine]](10)
            }
          }
        )
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
      (Elidable & HasOpcode(ADC) & MatchAddrMode(1) & MatchParameter(2) & HasAddrModeIn(ZeroPage, ZeroPageX, Absolute, AbsoluteX)) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(1) & MatchParameter(2) & DoesntMatterWhatItDoesWith(State.A, State.C, State.Z, State.N, State.V)) ~~> { (code, ctx) =>
      val label = ctx.nextLabel("ah")
      List(
        AssemblyLine.relative(BCC, label),
        code.last.copy(opcode = INC),
        AssemblyLine.label(label))
    },
    (Elidable & HasOpcode(LDA) & MatchAddrMode(1) & MatchParameter(2) & HasAddrModeIn(ZeroPage, ZeroPageX, Absolute, AbsoluteX)) ~
      (Elidable & HasOpcode(ADC) & HasImmediate(0) & HasClear(State.D)) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(1) & MatchParameter(2) & DoesntMatterWhatItDoesWith(State.A, State.C, State.Z, State.N, State.V)) ~~> { (code, ctx) =>
      val label = ctx.nextLabel("ah")
      List(
        AssemblyLine.relative(BCC, label),
        code.last.copy(opcode = INC),
        AssemblyLine.label(label))
    },
    (Elidable & HasOpcode(LDA) & HasImmediate(1) & HasClear(State.D) & HasClear(State.C)) ~
      (Elidable & HasOpcode(ADC) & MatchAddrMode(1) & MatchParameter(2) & HasAddrModeIn(ZeroPage, ZeroPageX, Absolute, AbsoluteX)) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(1) & MatchParameter(2) & DoesntMatterWhatItDoesWith(State.A, State.C, State.Z, State.N, State.V)) ~~> { code =>
      List(code.last.copy(opcode = INC))
    },
    (Elidable & HasOpcode(LDA) & MatchAddrMode(1) & HasClear(State.C) & MatchParameter(2) & HasAddrModeIn(ZeroPage, ZeroPageX, Absolute, AbsoluteX)) ~
      (Elidable & HasOpcode(ADC) & HasImmediate(1) & HasClear(State.D)) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(1) & MatchParameter(2) & DoesntMatterWhatItDoesWith(State.A, State.C, State.Z, State.N, State.V)) ~~> { code =>
      List(code.last.copy(opcode = INC))
    },
    (Elidable & HasOpcode(TXA) & HasClear(State.D)) ~
      (Elidable & HasOpcode(ADC) & HasImmediate(0)) ~
      (Elidable & HasOpcode(TAX) & DoesntMatterWhatItDoesWith(State.A, State.C, State.Z, State.N, State.V)) ~~> { (code, ctx) =>
      val label = ctx.nextLabel("ah")
      List(
        AssemblyLine.relative(BCC, label),
        AssemblyLine.implied(INX),
        AssemblyLine.label(label))
    },
    (Elidable & HasOpcode(TYA) & HasClear(State.D)) ~
      (Elidable & HasOpcode(ADC) & HasImmediate(0)) ~
      (Elidable & HasOpcode(TAY) & DoesntMatterWhatItDoesWith(State.A, State.C, State.Z, State.N, State.V)) ~~> { (code, ctx) =>
      val label = ctx.nextLabel("ah")
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
      (Elidable & HasOpcode(ADC) & MatchAddrMode(0) & MatchParameter(1) & HasAddrModeIn(ZeroPage, ZeroPageX, Absolute, AbsoluteX)) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.C, State.V, State.Z, State.N, State.A)) ~~> { (code, ctx) =>
      val label = ctx.nextLabel("in")
      code.take(code.length - 3) ++ List(
        AssemblyLine.relative(BEQ, label),
        code.last.copy(opcode = INC),
        AssemblyLine.label(label)
      )
    },
    (HasOpcode(ANC) & HasImmediate(1)) ~
      (Linear & Not(ChangesNAndZ) & Not(ChangesA) & (Not(ChangesC) | HasOpcode(CLC))).* ~
      (Elidable & HasOpcode(ADC) & MatchAddrMode(0) & MatchParameter(1) & HasClear(State.D) & HasAddrModeIn(ZeroPage, ZeroPageX, Absolute, AbsoluteX)) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.C, State.V, State.Z, State.N, State.A)) ~~> { (code, ctx) =>
      val label = ctx.nextLabel("in")
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
      (Linear & Not(ChangesA) & DoesntChangeMemoryAt(0, 1) & DoesntChangeIndexingInAddrMode(0)).* ~
      (ShortConditionalBranching & MatchParameter(2)) ~~> { code =>
      code(1) :: code(0) :: code.drop(2)
    },
    (HasOpcode(LABEL) & MatchParameter(2) & HasCallerCount(1)) ~
      (Elidable & HasOpcode(LDX) & MatchAddrMode(0) & MatchParameter(1) & IsNonvolatile) ~
      (Linear & Not(ChangesX) & DoesntChangeMemoryAt(0, 1) & DoesntChangeIndexingInAddrMode(0)).* ~
      (ShortConditionalBranching & MatchParameter(2)) ~~> { code =>
      code(1) :: code(0) :: code.drop(2)
    },
    (HasOpcode(LABEL) & MatchParameter(2) & HasCallerCount(1)) ~
      (Elidable & HasOpcode(LDY) & MatchAddrMode(0) & MatchParameter(1) & IsNonvolatile) ~
      (Linear & Not(ChangesY) & DoesntChangeMemoryAt(0, 1) & DoesntChangeIndexingInAddrMode(0)).* ~
      (ShortConditionalBranching & MatchParameter(2)) ~~> { code =>
      code(1) :: code(0) :: code.drop(2)
    },
    (HasOpcode(LABEL) & MatchParameter(2) & HasCallerCount(1)) ~
      (Elidable & HasOpcode(LDZ) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(ChangesIZ) & DoesntChangeMemoryAt(0, 1) & DoesntChangeIndexingInAddrMode(0)).* ~
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
      ((Elidable & HasOpcodeIn(LSR, ROR) & Not(ChangesA) & MatchAddrMode(0) & Not(MatchParameter(1))).* ~
        (Elidable & HasOpcodeIn(LSR, ROR) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.Z, State.C, State.N))).capture(2) ~~> { (code, ctx) =>
      ctx.get[List[AssemblyLine]](2) ++
        List(AssemblyLine.immediate(LDA, 0), AssemblyLine.implied(ROL)) ++
        ctx.get[List[AssemblyLine]](3)
    },
    (Elidable & HasOpcode(LDA) & HasImmediate(1)) ~
      (Elidable & HasOpcode(AND) & MatchAddrMode(0) & MatchParameter(1)) ~
      ((Elidable & Linear & Not(ChangesMemory) & DoesNotConcernMemoryAt(0, 1) & Not(ChangesA)).* ~
        (Elidable & HasOpcode(STA) & DoesNotConcernMemoryAt(0, 1))).capture(3) ~
      ((Elidable & HasOpcodeIn(LSR, ROR) & Not(ChangesA) & MatchAddrMode(0) & Not(MatchParameter(1))).* ~
        (Elidable & HasOpcodeIn(LSR, ROR) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.Z, State.C, State.N))).capture(2) ~~> { (code, ctx) =>
      ctx.get[List[AssemblyLine]](2) ++
        List(AssemblyLine.immediate(LDA, 0), AssemblyLine.implied(ROL)) ++
        ctx.get[List[AssemblyLine]](3)
    },
    (Elidable & (HasOpcode(ASL) | HasOpcode(ROL) & HasClear(State.C)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(ROL) & Not(ChangesA) & MatchAddrMode(0) & Not(MatchParameter(1))).*.capture(2) ~
      (Elidable & HasOpcode(CLC)).? ~
      (Elidable & HasOpcodeIn(LDA, TYA, TXA, PLA)).capture(3) ~
      (Elidable & HasOpcode(AND) & HasImmediate(1)) ~
      (Elidable & HasOpcode(CLC)).? ~
      (Elidable & (HasOpcode(ORA) | HasOpcode(ADC) & HasClear(State.C) & HasClear(State.D)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.C, State.N, State.Z, State.V, State.A)) ~~> { (code, ctx) =>
      ctx.get[List[AssemblyLine]](3) ++
        List(AssemblyLine.implied(LSR), code.head.copy(opcode = ROL)) ++
        ctx.get[List[AssemblyLine]](2)
    },
    (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcodeIn(ROL, ASL) & DoesntMatterWhatItDoesWith(State.A)) ~
      (Linear & DoesNotConcernMemoryAt(0, 1) & DoesntChangeIndexingInAddrMode(0)).* ~
      (Elidable & HasOpcodeIn(ROL, ASL) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.C) & MatchAddrMode(0) & MatchParameter(1)) ~~> { code =>
      code.last :: code.drop(2).init
    },
    (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcodeIn(ROR, LSR) & DoesntMatterWhatItDoesWith(State.A)) ~
      (Linear & DoesNotConcernMemoryAt(0, 1) & DoesntChangeIndexingInAddrMode(0)).* ~
      (Elidable & HasOpcode(LSR) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.C) & MatchAddrMode(0) & MatchParameter(1)) ~~> { code =>
      code.last :: code.drop(2).init
    },
    (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcodeIn(ROR, LSR) & DoesntMatterWhatItDoesWith(State.A)) ~
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
      (Elidable & HasOpcodeIn(ORA, EOR) & HasImmediate(1)) ~~> { code =>
      List(AssemblyLine.implied(SEC), code.head.copy(opcode = ROL))
    },

    (Elidable & HasOpcode(BIT) & DoesntMatterWhatItDoesWith(State.V, State.C, State.Z) & HasAddrModeIn(Absolute, ZeroPage) & MatchAddrMode(2) & MatchParameter(3)) ~
      (Elidable & HasOpcodeIn(BPL, BMI) & MatchParameter(1)) ~
      (Linear & DoesNotConcernMemoryAt(2, 3)).* ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(1) & HasCallerCount(1)) ~
      (Elidable & HasOpcode(ASL) & MatchAddrMode(2) & MatchParameter(3)) ~~> { code =>
      List(code.last, remapN2C(code(1))) ++ (code.drop(2).init)
    },

    (Elidable & HasOpcode(LDA) & DoesntMatterWhatItDoesWith(State.A, State.C, State.Z) & HasAddrModeIn(Absolute, ZeroPage) & MatchAddrMode(2) & MatchParameter(3)) ~
      (Elidable & HasOpcodeIn(BPL, BMI) & MatchParameter(1)) ~
      (Linear & DoesNotConcernMemoryAt(2, 3) & Not(ConcernsA)).* ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(1) & HasCallerCount(1)) ~
      (Elidable & HasOpcode(ASL) & MatchAddrMode(2) & MatchParameter(3)) ~~> { code =>
      List(code.last, remapN2C(code(1))) ++ (code.drop(2) :+ code.init.last)
    },
  )

  private def blockIsIdempotentWhenItComesToIndexRegisters(i: Int) = Where(ctx => {
    val code = ctx.get[List[AssemblyLine]](i)
    val rx = code.indexWhere(ReadsX)
    val wx = code.indexWhere(l => OpcodeClasses.ChangesX(l.opcode))
    val ry = code.indexWhere(ReadsY)
    val wy = code.indexWhere(l => OpcodeClasses.ChangesY(l.opcode))
    val xOk = rx < 0 || wx < 0 || rx >= wx
    val yOk = ry < 0 || wy < 0 || ry >= wy
    xOk && yOk
  })

  val CommonExpressionInConditional = new RuleBasedAssemblyOptimization("Common expression in conditional",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    (
      (HasOpcodeIn(LDA, LAX) & MatchAddrMode(0) & MatchParameter(1)) ~
        HasOpcodeIn(LDY, LDX, AND, ORA, EOR, ADC, SBC, CLC, SEC, CPY, CPX, CMP).*
      ).capture(7) ~
      blockIsIdempotentWhenItComesToIndexRegisters(7) ~
      ShortConditionalBranching ~
      MatchElidableCopyOf(7, Anything, DoesntMatterWhatItDoesWith(State.C, State.Z, State.N, State.V)) ~~> { code =>
      code.take(code.length / 2 + 1)
    },

    (Elidable & HasOpcodeIn(LDA, LAX) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(AND) & HasAddrModeIn(Absolute, ZeroPage) & DoesntMatterWhatItDoesWith(State.C, State.V, State.A)) ~
      HasOpcodeIn(BEQ, BNE) ~
      (HasOpcodeIn(LDA, LAX) & MatchAddrMode(0) & MatchParameter(1)) ~~> { code =>
      List(code(0), code(1).copy(opcode = BIT), code(2))
    },

    (Elidable & HasOpcode(LDA) & HasAddrModeIn(Absolute, ZeroPage)) ~
      (Elidable & HasOpcode(AND) & MatchAddrMode(0) & MatchParameter(1)) ~
      HasOpcodeIn(BEQ, BNE) ~
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
      (Elidable & ShortConditionalBranching & MatchParameter(0)) ~
      (Elidable & HasOpcode(JMP) & MatchParameter(1)) ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(0)) ~~> { (code, ctx) =>
      ctx.get[List[AssemblyLine]](10) ++ List(negate(code(code.length - 3).copy(parameter = ctx.get[Constant](1))), code.last)
    },
    ((Elidable & HasOpcode(LABEL) & MatchParameter(1)) ~ LinearOrLabel.*).capture(10) ~
      Where(ctx => ctx.get[List[AssemblyLine]](10).map(_.sizeInBytes).sum < 100) ~
      (Elidable & ShortConditionalBranching & MatchParameter(0)).capture(13) ~
      ((Elidable & Not(MatchParameter(0))).* ~
        (Elidable & HasOpcode(LABEL) & MatchParameter(0)) ~
        (Elidable & HasOpcode(JMP) & MatchParameter(1))).capture(11) ~~> { (code, ctx) =>
        ctx.get[List[AssemblyLine]](10) ++ ctx.get[List[AssemblyLine]](13).map(_.copy(parameter = ctx.get[Constant](1))) ++ ctx.get[List[AssemblyLine]](11)
    },
    (Elidable & HasOpcode(LABEL) & MatchParameter(0)) ~
      (Elidable & HasOpcode(JMP) & MatchParameter(1)) ~
      Not(MatchParameter(1)).* ~
      (HasOpcode(LABEL) & MatchParameter(1)) ~
      (HasOpcode(JMP) & MatchParameter(0)) ~~> { (code, ctx) =>
      code.head :: code.drop(2)
    },

    (Elidable & HasOpcode(BEQ) & MatchParameter(0)) ~
      (Elidable & HasOpcodeIn(JMP, BNE) & MatchParameter(1)) ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> { (code, ctx) =>
      code(1).copy(opcode = BNE, addrMode = Relative) :: code.drop(2)
    },
    (Elidable & HasOpcode(BNE) & MatchParameter(0)) ~
      (Elidable & HasOpcodeIn(JMP, BEQ) & MatchParameter(1)) ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> { (code, ctx) =>
      code(1).copy(opcode = BEQ, addrMode = Relative) :: code.drop(2)
    },
    (Elidable & HasOpcode(BCC) & MatchParameter(0)) ~
      (Elidable & HasOpcodeIn(JMP, BCS) & MatchParameter(1)) ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> { (code, ctx) =>
      code(1).copy(opcode = BCS, addrMode = Relative) :: code.drop(2)
    },
    (Elidable & HasOpcode(BCS) & MatchParameter(0)) ~
      (Elidable & HasOpcodeIn(JMP, BCC) & MatchParameter(1)) ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> { (code, ctx) =>
      code(1).copy(opcode = BCC, addrMode = Relative) :: code.drop(2)
    },
    (Elidable & HasOpcode(BMI) & MatchParameter(0)) ~
      (Elidable & HasOpcodeIn(JMP, BPL) & MatchParameter(1)) ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> { (code, ctx) =>
      code(1).copy(opcode = BPL, addrMode = Relative) :: code.drop(2)
    },
    (Elidable & HasOpcode(BPL) & MatchParameter(0)) ~
      (Elidable & HasOpcodeIn(JMP, BMI) & MatchParameter(1)) ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> { (code, ctx) =>
      code(1).copy(opcode = BMI, addrMode = Relative) :: code.drop(2)
    },
  )

  val IndexComparisonOptimization = new RuleBasedAssemblyOptimization("Index comparison optimization",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    (Elidable & HasOpcodeIn(DEX, INX) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~
      (Linear & Not(ConcernsX)).* ~
      (Elidable & (HasOpcode(TXA) & DoesntMatterWhatItDoesWith(State.A) | HasOpcode(CPX) & HasImmediate(0) & DoesntMatterWhatItDoesWith(State.C, State.V))) ~~> { code =>
      code.tail.init :+ code.head
    },
    (Elidable & HasOpcodeIn(DEY, INY) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~
      (Linear & Not(ConcernsY)).* ~
      (Elidable & (HasOpcode(TYA) & DoesntMatterWhatItDoesWith(State.A) | HasOpcode(CPY) & HasImmediate(0) & DoesntMatterWhatItDoesWith(State.C, State.V))) ~~> { code =>
      code.tail.init :+ code.head
    },
    (Elidable & HasOpcodeIn(DEX, INX) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~
      (IsNotALabelUsedManyTimes & Not(ConcernsX)).*.capture(1) ~
      Where(ctx => ctx.isExternallyLinearBlock(1)) ~
      (Elidable & (HasOpcode(TXA) & DoesntMatterWhatItDoesWith(State.A) | HasOpcode(CPX) & HasImmediate(0) & DoesntMatterWhatItDoesWith(State.C, State.V))) ~~> { code =>
      code.tail.init :+ code.head
    },
    (Elidable & HasOpcodeIn(DEY, INY) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~
      (IsNotALabelUsedManyTimes & Not(ConcernsY)).*.capture(1) ~
      Where(ctx => ctx.isExternallyLinearBlock(1)) ~
      (Elidable & (HasOpcode(TYA) & DoesntMatterWhatItDoesWith(State.A) | HasOpcode(CPY) & HasImmediate(0) & DoesntMatterWhatItDoesWith(State.C, State.V))) ~~> { code =>
      code.tail.init :+ code.head
    },
    (Elidable & HasAddrMode(Implied) & HasOpcodeIn(DEC, INC) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~
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

  val OptimizeZeroComparisons = jvmFix(new RuleBasedAssemblyOptimization("Optimizing zero comparisons",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    (Elidable & HasSourceOfNZ(State.A) & HasOpcode(CMP) & HasImmediate(0) & DoesntMatterWhatItDoesWith(State.C)) ~~> (_.init),
    (Elidable & HasSourceOfNZ(State.X) & HasOpcode(CPX) & HasImmediate(0) & DoesntMatterWhatItDoesWith(State.C)) ~~> (_.init),
    (Elidable & HasSourceOfNZ(State.Y) & HasOpcode(CPY) & HasImmediate(0) & DoesntMatterWhatItDoesWith(State.C)) ~~> (_.init),
    (Elidable & HasSourceOfNZ(State.IZ) & HasOpcode(CPZ) & HasImmediate(0) & DoesntMatterWhatItDoesWith(State.C)) ~~> (_.init),

    (Elidable & HasA(0) & HasOpcode(CMP) & DoesntMatterWhatItDoesWith(State.N, State.A)) ~
      (HasOpcode(BCC) & DoesntMatterWhatItDoesWith(State.C)) ~~> {code =>
      List(code.head.copy(opcode = LDA), code(1).copy(opcode = BNE))
    },
    (Elidable & HasA(0) & HasOpcode(CMP) & DoesntMatterWhatItDoesWith(State.N, State.A)) ~
      (HasOpcode(BCS) & DoesntMatterWhatItDoesWith(State.C)) ~~> {code =>
      List(code.head.copy(opcode = LDA), code(1).copy(opcode = BEQ))
    },

    (Elidable & HasX(0) & HasOpcode(CPX) & DoesntMatterWhatItDoesWith(State.X, State.N)) ~
      (HasOpcode(BCC) & DoesntMatterWhatItDoesWith(State.C)) ~~> {code =>
      List(code.head.copy(opcode = LDX), code(1).copy(opcode = BNE))
    },
    (Elidable & HasX(0) & HasOpcode(CPX) & DoesntMatterWhatItDoesWith(State.X, State.N)) ~
      (HasOpcode(BCS) & DoesntMatterWhatItDoesWith(State.C)) ~~> {code =>
      List(code.head.copy(opcode = LDX), code(1).copy(opcode = BEQ))
    },

    (Elidable & HasY(0) & HasOpcode(CPY) & DoesntMatterWhatItDoesWith(State.Y, State.N)) ~
      (HasOpcode(BCC) & DoesntMatterWhatItDoesWith(State.C)) ~~> {code =>
      List(code.head.copy(opcode = LDY), code(1).copy(opcode = BNE))
    },
    (Elidable & HasY(0) & HasOpcode(CPY) & DoesntMatterWhatItDoesWith(State.Y, State.N)) ~
      (HasOpcode(BCS) & DoesntMatterWhatItDoesWith(State.C)) ~~> {code =>
      List(code.head.copy(opcode = LDY), code(1).copy(opcode = BEQ))
    },

    (Elidable & HasA(0) & HasOpcode(CMP) & DoesntMatterWhatItDoesWith(State.N, State.X) & HasAddrModeIn(LdxAddrModes)) ~
      (HasOpcode(BCC) & DoesntMatterWhatItDoesWith(State.C)) ~~> {code =>
      List(code.head.copy(opcode = LDX), code(1).copy(opcode = BNE))
    },
    (Elidable & HasA(0) & HasOpcode(CMP) & DoesntMatterWhatItDoesWith(State.N, State.X) & HasAddrModeIn(LdxAddrModes)) ~
      (HasOpcode(BCS) & DoesntMatterWhatItDoesWith(State.C)) ~~> {code =>
      List(code.head.copy(opcode = LDX), code(1).copy(opcode = BEQ))
    },

    (Elidable & HasA(0) & HasOpcode(CMP) & DoesntMatterWhatItDoesWith(State.N, State.Y) & HasAddrModeIn(LdyAddrModes)) ~
      (HasOpcode(BCC) & DoesntMatterWhatItDoesWith(State.C)) ~~> {code =>
      List(code.head.copy(opcode = LDY), code(1).copy(opcode = BNE))
    },
    (Elidable & HasA(0) & HasOpcode(CMP) & DoesntMatterWhatItDoesWith(State.N, State.Y) & HasAddrModeIn(LdyAddrModes)) ~
      (HasOpcode(BCS) & DoesntMatterWhatItDoesWith(State.C)) ~~> {code =>
      List(code.head.copy(opcode = LDY), code(1).copy(opcode = BEQ))
    },

    (Elidable & HasX(0) & HasOpcode(CPX) & DoesntMatterWhatItDoesWith(State.N, State.A)) ~
      (HasOpcode(BCC) & DoesntMatterWhatItDoesWith(State.C)) ~~> {code =>
      List(code.head.copy(opcode = LDA), code(1).copy(opcode = BNE))
    },
    (Elidable & HasX(0) & HasOpcode(CPX) & DoesntMatterWhatItDoesWith(State.N, State.A)) ~
      (HasOpcode(BCS) & DoesntMatterWhatItDoesWith(State.C)) ~~> {code =>
      List(code.head.copy(opcode = LDA), code(1).copy(opcode = BEQ))
    },

    (Elidable & HasY(0) & HasOpcode(CPY) & DoesntMatterWhatItDoesWith(State.N, State.A)) ~
      (HasOpcode(BCC) & DoesntMatterWhatItDoesWith(State.C)) ~~> {code =>
      List(code.head.copy(opcode = LDA), code(1).copy(opcode = BNE))
    },
    (Elidable & HasY(0) & HasOpcode(CPY) & DoesntMatterWhatItDoesWith(State.N, State.A)) ~
      (HasOpcode(BCS) & DoesntMatterWhatItDoesWith(State.C)) ~~> {code =>
      List(code.head.copy(opcode = LDA), code(1).copy(opcode = BEQ))
    },

    (Elidable & HasZ(0) & HasOpcode(CPZ) & DoesntMatterWhatItDoesWith(State.N, State.IZ)) ~
      (HasOpcode(BCC) & DoesntMatterWhatItDoesWith(State.C)) ~~> {code =>
      List(code.head.copy(opcode = LDZ), code(1).copy(opcode = BNE))
    },
    (Elidable & HasZ(0) & HasOpcode(CPZ) & DoesntMatterWhatItDoesWith(State.N, State.IZ)) ~
      (HasOpcode(BCS) & DoesntMatterWhatItDoesWith(State.C)) ~~> {code =>
      List(code.head.copy(opcode = LDZ), code(1).copy(opcode = BEQ))
    },
  ))

  private def remapZ2N(line: AssemblyLine): AssemblyLine = line.opcode match {
    case BNE => line.copy(opcode = BMI)
    case BEQ => line.copy(opcode = BPL)
    case _ => FatalErrorReporting.reportFlyingPig(s"Tried to treat ${line.opcode} as a branch on Z")
  }

  private def remapC2N(line: AssemblyLine): AssemblyLine = line.opcode match {
    case BCS => line.copy(opcode = BMI)
    case BCC => line.copy(opcode = BPL)
    case _ => FatalErrorReporting.reportFlyingPig(s"Tried to treat ${line.opcode} as a branch on C")
  }

  private def remapN2C(line: AssemblyLine): AssemblyLine = line.opcode match {
    case BMI => line.copy(opcode = BCS)
    case BPL => line.copy(opcode = BCC)
    case _ => FatalErrorReporting.reportFlyingPig(s"Tried to treat ${line.opcode} as a branch on N")
  }

  private def remapZ2CInverse(line: AssemblyLine): AssemblyLine = line.opcode match {
    case BNE => line.copy(opcode = BCS)
    case BEQ => line.copy(opcode = BCC)
    case _ => FatalErrorReporting.reportFlyingPig(s"Tried to treat ${line.opcode} as a branch on C")
  }

  private def remapZ2V(line: AssemblyLine): AssemblyLine = line.opcode match {
    case BNE => line.copy(opcode = BVS)
    case BEQ => line.copy(opcode = BVC)
    case _ => FatalErrorReporting.reportFlyingPig(s"Tried to treat ${line.opcode} as a branch on Z")
  }

  val SimplifiableCondition = new RuleBasedAssemblyOptimization("Simplifiable condition",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    HasOpcodeIn(LDA, TXA, TYA, TZA, ADC, SBC, AND, ORA, EOR) ~
      (Elidable & HasOpcode(AND) & HasImmediate(0x80)) ~
      (Elidable & HasOpcodeIn(BNE, BEQ) & DoesntMatterWhatItDoesWith(State.A, State.N, State.Z)) ~~> {code =>
      List(code(0), remapZ2N(code(2)))
    },
    (Elidable & HasOpcode(LDA) & HasImmediate(0x80)) ~
      (Elidable & HasOpcode(AND)) ~
      (Elidable & HasOpcodeIn(BNE, BEQ) & DoesntMatterWhatItDoesWith(State.A, State.N, State.Z)) ~~> {code =>
      List(code(1).copy(opcode = LDA), remapZ2N(code(2)))
    },
    HasOpcodeIn(LDA, TXA, TYA, TZA, ADC, SBC, AND, ORA, EOR) ~
      (Elidable & HasOpcode(ASL)) ~
      (Elidable & HasOpcodeIn(BCS, BCC) & DoesntMatterWhatItDoesWith(State.A, State.N, State.C, State.Z)) ~~> {code =>
      List(code(0), remapC2N(code(2)))
    },
    (HasOpcodeIn(LDA, AND) & HasImmediate(0)) ~
      (Elidable & HasOpcode(ROL)) ~
      (Elidable & HasOpcodeIn(BEQ, BNE) & DoesntMatterWhatItDoesWith(State.A, State.N, State.C, State.Z)) ~~> {code =>
      List(code(0), remapZ2CInverse(code(2)))
    },
  )

  val PointlessSignCheck: RuleBasedAssemblyOptimization = {
    def loadOldSignedVariable: AssemblyPattern = (
      (HasOpcodeIn(AND, ANC) & HasImmediateWhere(i => (i & 0x80) == 0)) ~
        (HasOpcode(STA) & HasAddrModeIn(Absolute, ZeroPage) & MatchAddrMode(0) & MatchParameter(1)) ~
        (IsNotALabelUsedManyTimes & DoesNotConcernMemoryAt(0, 1)).* ~
        (HasOpcode(LDA) & HasAddrModeIn(Absolute, ZeroPage) & MatchParameter(1))
      ).capture(10) ~ Where(_.isExternallyLinearBlock(10))

    val isNonnegative: Int => Boolean = i => (i & 0x80) == 0

    new RuleBasedAssemblyOptimization("Pointless sign check",
      needsFlowInfo = FlowInfoRequirement.NoRequirement,
      (HasOpcode(AND) & HasImmediateWhere(isNonnegative)) ~
        (Elidable & HasOpcode(BMI)) ~~> (_.take(1)),
      loadOldSignedVariable ~
        (Elidable & HasOpcode(BMI)) ~~> { (code, ctx) => ctx.get[List[AssemblyLine]](10) },
      loadOldSignedVariable ~
        (Elidable & HasOpcodeIn(ORA, EOR) & HasImmediateWhere(isNonnegative)) ~
        (Elidable & HasOpcode(BMI)) ~
        OverwritesA ~~> { (code, ctx) => ctx.get[List[AssemblyLine]](10) :+ code.last },
      loadOldSignedVariable ~
        (Elidable & HasOpcodeIn(ORA, EOR) & HasImmediateWhere(isNonnegative)) ~
        (Elidable & HasOpcode(BMI)) ~~> { code => code.init },
      loadOldSignedVariable ~
        (Elidable & HasOpcodeIn(ORA, EOR) & HasImmediateWhere(isNonnegative)).? ~
        (Elidable & HasOpcode(BPL)) ~~> { code => code.init :+ code.last.copy(opcode = JMP, addrMode = Absolute) },
      loadOldSignedVariable ~
        ((Linear & Not(ConcernsX) & Not(ChangesA)).* ~
          HasOpcode(TAX) ~
          (Linear & Not(ConcernsX)).*).capture(11) ~
        (Elidable & HasOpcode(TXA)) ~
        (Elidable & HasOpcodeIn(ORA, EOR) & HasImmediateWhere(isNonnegative)).? ~
        (Elidable & HasOpcode(BMI)) ~
        OverwritesA ~~> { (code, ctx) => ctx.get[List[AssemblyLine]](10) ++ ctx.get[List[AssemblyLine]](11) :+ code.last },
      loadOldSignedVariable ~
        (Linear & Not(ConcernsX) & Not(ChangesA)).* ~
        HasOpcode(TAX) ~
        (Linear & Not(ConcernsX)).* ~
        HasOpcode(TXA) ~
        (HasOpcodeIn(ORA, EOR) & HasImmediateWhere(isNonnegative)).? ~
        (Elidable & HasOpcode(BMI)) ~~> { code => code.init },
      loadOldSignedVariable ~
        (Linear & Not(ConcernsX) & Not(ChangesA)).* ~
        HasOpcode(TAX) ~
        (Linear & Not(ConcernsX)).* ~
        HasOpcode(TXA) ~
        (HasOpcodeIn(ORA, EOR) & HasImmediateWhere(isNonnegative)).? ~
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
      val label = ctx.nextLabel("in")
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
      val label = ctx.nextLabel("in")
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
      (Elidable & HasOpcode(STA) & Not(ConcernsX)) ~
      (Elidable & HasOpcode(TXA)) ~
      (Elidable & HasOpcode(CLC)) ~
      (Elidable & HasOpcode(ADC)) ~
      (Elidable & HasOpcode(STA) & DoesntMatterWhatItDoesWith(State.C, State.N, State.V, State.Z)) ~~> { (code, ctx) =>
      val label = ctx.nextLabel("in")
      List(
        code(4), // STA
        AssemblyLine.implied(TAX),
        AssemblyLine.immediate(LDA, 0),
        code(7), // ADC
        code(8)) // STA
    },
    (Elidable & HasOpcode(LDX) & HasAddrMode(Immediate) & HasClear(State.D)) ~
      (Elidable & HasOpcode(BCC) & MatchParameter(14)) ~
      (Elidable & HasOpcode(INX)) ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(14) & HasCallerCount(1)) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1) & Not(ConcernsX)) ~
      (Elidable & HasOpcode(STX) & MatchAddrMode(2) & MatchParameter(3) & DoesNotConcernMemoryAt(0, 1) & Not(HasAddrMode(ZeroPageY)) &
        DoesntMatterWhatItDoesWith(State.X, State.A, State.C, State.V)) ~~> { (code, ctx) =>
      val label = ctx.nextLabel("in")
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
      val label = ctx.nextLabel("in")
      List(
        code(4), // STA
        AssemblyLine.immediate(ADC, 0),
        code(5).copy(opcode = STA)) //STX
    },

    (Elidable & HasOpcode(LDX) & HasImmediate(0)) ~
      (Elidable & HasOpcode(BCC) & MatchParameter(1)) ~
      (Elidable & HasOpcode(INX)) ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(1) & IsNotALabelUsedManyTimes) ~
      (Elidable & HasOpcode(TXA) & DoesntMatterWhatItDoesWith(State.C, State.X)) ~~> { (code, ctx) =>
      List(AssemblyLine.immediate(LDA, 0), AssemblyLine.implied(ROL))
    },

    (Elidable & HasOpcode(LDX) & HasImmediate(0)) ~
    (Elidable & HasOpcode(BCC) & MatchParameter(1)) ~
    (Elidable & HasOpcode(INX)) ~
    (Elidable & HasOpcode(LABEL) & MatchParameter(1) & IsNotALabelUsedManyTimes) ~
    (Elidable & HasOpcode(TXA) & DoesntMatterWhatItDoesWith(State.C)) ~~> { (code, ctx) =>
      List(AssemblyLine.immediate(LDA, 0), AssemblyLine.implied(ROL), AssemblyLine.implied(TAX))
    },
  )

  val NonetBitOp = new RuleBasedAssemblyOptimization("Nonet bit operation",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    (Elidable & HasOpcode(LDX) & HasImmediate(0)) ~
      (Elidable & HasOpcode(BCC) & MatchParameter(14)) ~
      (Elidable & HasOpcode(INX)) ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(14) & HasCallerCount(1)) ~
      (Elidable & HasOpcodeIn(ORA, EOR) & MatchAddrMode(0) & MatchParameter(1) & Not(ConcernsX)) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1) & Not(ConcernsX)) ~
      (Elidable & HasOpcode(TXA)) ~
      (Elidable & HasOpcodeIn(ORA, EOR) & MatchAddrMode(2) & MatchParameter(3) & Not(ConcernsX) & DoesNotConcernMemoryAt(0, 1)) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(2) & MatchParameter(3) & Not(ConcernsX) & DoesntMatterWhatItDoesWith(State.C, State.N, State.V, State.Z)) ~~>{ (code, ctx) =>
        val label = ctx.nextLabel("in")
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
        if (firstLda) HasOpcodeIn(LDA, STA, LAX) & HasAddrModeIn(Absolute, ZeroPage, Immediate) & MatchParameter(1)
        else if (targetY) HasOpcodeIn(TXA, TAX)
        else HasOpcodeIn(TAY, TYA)
        )
      val secondLoad = Elidable & (
        if (firstLda) HasOpcode(LDA) & HasAddrModeIn(Absolute, ZeroPage, Immediate) & MatchParameter(1)
        else if (targetY) HasOpcode(TXA)
        else HasOpcode(TYA)
        )
      val firstTransfer = if (targetY) HasOpcode(TAY) else HasOpcode(TAX)
      val secondTransfer = Elidable & (
        if (targetY) HasOpcode(TAY)
        else HasOpcode(TAX)
        ) & DoesntMatterWhatItDoesWith(State.A, State.Z, State.N, State.C)
      val fillerLine =
        HasAddrMode(Implied) & HasOpcodeIn(ASL, CLC, CLD, SEC, SED, LSR, INC, DEC) |
          HasOpcodeIn(ADC, ORA, EOR, AND, SBC) & HasAddrModeIn(Absolute, ZeroPage, Immediate)
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

    (HasOpcode(STA) & MatchA(0) & HasAddrModeIn(Absolute, ZeroPage) & MatchParameter(4)) ~
      Where(ctx => {
        val lo = ctx.get[Constant](4)
        ctx.addObject(5, lo + 1)
        ctx.addObject(3, ZeroPage)
        true
      }) ~
      (Linear & DoesNotConcernMemoryAt(3, 4) & DoesNotConcernMemoryAt(3, 5)).* ~
      (HasOpcode(STA) & HasAddrModeIn(Absolute, ZeroPage) & MatchA(1) & MatchParameter(5)) ~
      Where(ctx => {
        val lo = ctx.get[Int](0) & 0xff
        val hi = ctx.get[Int](1) & 0xff
        ctx.addObject(2, hi * 256 + lo)
        true
      }) ~
      (Linear & DoesNotConcernMemoryAt(3,4) & DoesNotConcernMemoryAt(3,5)).* ~
      (Elidable & MatchParameter(4) & HasAddrModeIn(IndexedZ, IndexedY) & MatchAddrMode(9)) ~
      Where(ctx => {
        ctx.get[AddrMode.Value](9) == IndexedY || !ctx.compilationOptions.flag(CompilationFlag.Emit65CE02Opcodes)
      }) ~~> { (code, ctx) =>
      val addr = ctx.get[Int](2)
      val last = code.last
      code.init :+ last.copy(parameter = NumericConstant(addr, 2), addrMode = if (last.addrMode == IndexedZ) Absolute else AbsoluteY)
    },

    (HasOpcode(STA) & MatchA(0) & HasAddrModeIn(Absolute, ZeroPage) & MatchParameter(4)) ~
      Where(ctx => {
        val lo = ctx.get[Constant](4)
        ctx.addObject(5, lo + 1)
        ctx.addObject(3, ZeroPage)
        true
      }) ~
      (Linear & DoesNotConcernMemoryAt(3,4) & DoesNotConcernMemoryAt(3,5)).* ~
      (HasOpcode(STX) & MatchX(1) & HasAddrModeIn(Absolute, ZeroPage) & MatchParameter(5)) ~
      Where(ctx => {
        val lo = ctx.get[Int](0) & 0xff
        val hi = ctx.get[Int](1) & 0xff
        ctx.addObject(2, hi * 256 + lo)
        true
      }) ~
      (Linear & DoesNotConcernMemoryAt(3, 4) & DoesNotConcernMemoryAt(3, 5)).* ~
      (Elidable & MatchParameter(4) & HasAddrModeIn(IndexedZ, IndexedY) & MatchAddrMode(9)) ~
      Where(ctx => {
        ctx.get[AddrMode.Value](9) == IndexedY || !ctx.compilationOptions.flag(CompilationFlag.Emit65CE02Opcodes)
      }) ~~> { (code, ctx) =>
      val addr = ctx.get[Int](2)
      val last = code.last
      code.init :+ last.copy(parameter = NumericConstant(addr, 2), addrMode = if (last.addrMode == IndexedZ) Absolute else AbsoluteY)
    },

    (HasOpcode(STX) & MatchX(1) & HasAddrModeIn(Absolute, ZeroPage) & MatchParameter(5)) ~
    (HasOpcode(STA) & MatchA(0) & HasAddrModeIn(Absolute, ZeroPage) & MatchParameter(4)) ~
      Where(ctx => {
        ctx.addObject(3, ZeroPage)
        (ctx.get[Constant](4) + 1).quickSimplify == ctx.get[Constant](5)
      }) ~
      (Linear & DoesNotConcernMemoryAt(3, 4) & DoesNotConcernMemoryAt(3, 5)).* ~
      Where(ctx => {
        val lo = ctx.get[Int](0) & 0xff
        val hi = ctx.get[Int](1) & 0xff
        ctx.addObject(2, hi * 256 + lo)
        true
      }) ~
      (Elidable & MatchParameter(4) & HasAddrModeIn(IndexedZ, IndexedY) & MatchAddrMode(9)) ~
      Where(ctx => {
        ctx.get[AddrMode.Value](9) == IndexedY || !ctx.compilationOptions.flag(CompilationFlag.Emit65CE02Opcodes)
      }) ~~> { (code, ctx) =>
      val addr = ctx.get[Int](2)
      val last = code.last
      code.init :+ last.copy(parameter = NumericConstant(addr, 2), addrMode = if (last.addrMode == IndexedZ) Absolute else AbsoluteY)
    },

    (HasOpcode(LDA) & MatchImmediate(0)) ~
      (HasOpcode(LDX) & MatchImmediate(1)) ~
      (HasOpcode(STX) & HasAddrModeIn(Absolute, ZeroPage) & MatchParameter(5)) ~
      (HasOpcode(STA) & HasAddrModeIn(Absolute, ZeroPage) & MatchParameter(4)) ~
      Where(ctx => {
        ctx.addObject(3, ZeroPage)
        (ctx.get[Constant](4) + 1).quickSimplify == ctx.get[Constant](5)
      }) ~
      (Linear & DoesNotConcernMemoryAt(3, 4) & DoesNotConcernMemoryAt(3, 5)).* ~
      Where(ctx => {
        val lo = ctx.get[Constant](0)
        val hi = ctx.get[Constant](1)
        ctx.addObject(2, (hi.asl(8) + lo).quickSimplify)
        true
      }) ~
      (Elidable & MatchParameter(4) & HasAddrModeIn(IndexedZ, IndexedY) & MatchAddrMode(9)) ~
      Where(ctx => {
        ctx.get[AddrMode.Value](9) == IndexedY || !ctx.compilationOptions.flag(CompilationFlag.Emit65CE02Opcodes)
      }) ~~> { (code, ctx) =>
      val addr = ctx.get[Constant](2)
      val last = code.last
      code.init :+ last.copy(parameter = addr, addrMode = if (last.addrMode == IndexedZ) Absolute else AbsoluteY)
    },

    (HasOpcode(LDA) & MatchImmediate(0)) ~
      (HasOpcode(LDX) & MatchImmediate(1)) ~
      (HasOpcode(STA) & HasAddrModeIn(Absolute, ZeroPage) & MatchParameter(4)) ~
      (HasOpcode(STX) & HasAddrModeIn(Absolute, ZeroPage) & MatchParameter(5)) ~
      Where(ctx => {
        ctx.addObject(3, ZeroPage)
        (ctx.get[Constant](4) + 1).quickSimplify == ctx.get[Constant](5)
      }) ~
      (Linear & DoesNotConcernMemoryAt(3, 4) & DoesNotConcernMemoryAt(3, 5)).* ~
      Where(ctx => {
        val lo = ctx.get[Constant](0)
        val hi = ctx.get[Constant](1)
        ctx.addObject(2, (hi.asl(8) + lo).quickSimplify)
        true
      }) ~
      (Elidable & MatchParameter(4) & HasAddrModeIn(IndexedZ, IndexedY) & MatchAddrMode(9)) ~
      Where(ctx => {
        ctx.get[AddrMode.Value](9) == IndexedY || !ctx.compilationOptions.flag(CompilationFlag.Emit65CE02Opcodes)
      }) ~~> { (code, ctx) =>
      val addr = ctx.get[Constant](2)
      val last = code.last
      code.init :+ last.copy(parameter = addr, addrMode = if (last.addrMode == IndexedZ) Absolute else AbsoluteY)
    },

    (HasOpcode(LDA) & MatchImmediate(0)) ~
      (HasOpcode(STA) & HasAddrModeIn(Absolute, ZeroPage) & MatchParameter(4)) ~
      (HasOpcode(LDA) & MatchImmediate(1)) ~
      (HasOpcode(STA) & HasAddrModeIn(Absolute, ZeroPage) & MatchParameter(5)) ~
      Where(ctx => {
        ctx.addObject(3, ZeroPage)
        (ctx.get[Constant](4) + 1).quickSimplify == ctx.get[Constant](5)
      }) ~
      (Linear & DoesNotConcernMemoryAt(3, 4) & DoesNotConcernMemoryAt(3, 5)).* ~
      Where(ctx => {
        val lo = ctx.get[Constant](0)
        val hi = ctx.get[Constant](1)
        ctx.addObject(2, (hi.asl(8) + lo).quickSimplify)
        true
      }) ~
      (Elidable & MatchParameter(4) & HasAddrModeIn(IndexedZ, IndexedY) & MatchAddrMode(9)) ~
      Where(ctx => {
        ctx.get[AddrMode.Value](9) == IndexedY || !ctx.compilationOptions.flag(CompilationFlag.Emit65CE02Opcodes)
      }) ~~> { (code, ctx) =>
      val addr = ctx.get[Constant](2)
      val last = code.last
      code.init :+ last.copy(parameter = addr, addrMode = if (last.addrMode == IndexedZ) Absolute else AbsoluteY)
    },

    (HasOpcode(LDA) & MatchImmediate(1)) ~
      (HasOpcode(STA) & HasAddrModeIn(Absolute, ZeroPage) & MatchParameter(5)) ~
      (HasOpcode(LDA) & MatchImmediate(0)) ~
      (HasOpcode(STA) & HasAddrModeIn(Absolute, ZeroPage) & MatchParameter(4)) ~
      Where(ctx => {
        ctx.addObject(3, ZeroPage)
        (ctx.get[Constant](4) + 1).quickSimplify == ctx.get[Constant](5)
      }) ~
      (Linear & DoesNotConcernMemoryAt(3, 4) & DoesNotConcernMemoryAt(3, 5)).* ~
      Where(ctx => {
        val lo = ctx.get[Constant](0)
        val hi = ctx.get[Constant](1)
        ctx.addObject(2, (hi.asl(8) + lo).quickSimplify)
        true
      }) ~
      (Elidable & MatchParameter(4) & HasAddrModeIn(IndexedZ, IndexedY) & MatchAddrMode(9)) ~
      Where(ctx => {
        ctx.get[AddrMode.Value](9) == IndexedY || !ctx.compilationOptions.flag(CompilationFlag.Emit65CE02Opcodes)
      }) ~~> { (code, ctx) =>
      val addr = ctx.get[Constant](2)
      val last = code.last
      code.init :+ last.copy(parameter = addr, addrMode = if (last.addrMode == IndexedZ) Absolute else AbsoluteY)
    },

  )

  val ReplacingArithmeticsWithBitOps = new RuleBasedAssemblyOptimization("Replacing arithmetics with bit ops",
    needsFlowInfo = FlowInfoRequirement.BothFlows,

    (Elidable & HasOpcode(CLC)).? ~
      (Elidable & HasOpcode(ADC) & HasImmediate(0x80) & HasClear(State.C) & HasClear(State.D) & DoesntMatterWhatItDoesWith(State.C, State.V)) ~~>
      (_ => List(AssemblyLine.immediate(EOR, 0x80))),

    (Elidable & HasOpcode(SEC)).? ~
      (Elidable & HasOpcode(SBC) & HasImmediate(0x80) & HasSet(State.C) & HasClear(State.D) & DoesntMatterWhatItDoesWith(State.C, State.V)) ~~>
      (_ => List(AssemblyLine.immediate(EOR, 0x80))),

    (Elidable & HasOpcode(CLC)).? ~
      (Elidable & HasOpcode(ADC) & HasImmediate(1) & HasClearBitA0 & HasClear(State.C) & HasClear(State.D) & DoesntMatterWhatItDoesWith(State.C, State.V)) ~~>
      (_ => List(AssemblyLine.immediate(ORA, 1))),

    (HasOpcode(AND) & MatchImmediate(1)) ~
      (Elidable & HasOpcode(CLC)).? ~
      (Elidable & HasOpcode(ADC) & MatchImmediate(2) & HasClear(State.C) & HasClear(State.D) & DoesntMatterWhatItDoesWith(State.C, State.V)) ~
      Where(ctx => (ctx.get[Constant](1), ctx.get[Constant](2)) match {
        case (NumericConstant(a1, _), NumericConstant(a2, _)) => (a1 & a2) == 0
        case _ => false
      }) ~~> ((code,ctx) => List(code.head, AssemblyLine.immediate(ORA, ctx.get[Constant](2)))),

    (Elidable & HasOpcode(ANC) & MatchImmediate(1)) ~
      (Elidable & HasOpcode(ADC) & MatchImmediate(2) & HasClear(State.C) & HasClear(State.D) & DoesntMatterWhatItDoesWith(State.C, State.V)) ~
      Where(ctx => (ctx.get[Constant](1), ctx.get[Constant](2)) match {
        case (NumericConstant(a1, _), NumericConstant(a2, _)) => (a1 & 0x80) == 0 && (a1 & a2) == 0 && (a2 & 0xff) > 1
        case _ => false
      }) ~~> ((code,ctx) => List(code.head.copy(opcode = AND), AssemblyLine.immediate(ORA, ctx.get[Constant](2)))),


    (Elidable & HasOpcode(STA) & HasClearBitA0 & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.Z, State.N, State.A)) ~
      (Linear & DoesNotConcernMemoryAt(0, 1) & DoesntChangeIndexingInAddrMode(0)).* ~
      (Elidable & HasOpcode(INC) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~~> { code =>
      AssemblyLine.immediate(ORA, 1) :: code.init
    },
  )

  lazy val ConstantInlinedShifting = new RuleBasedAssemblyOptimization("Constant inlined shifting",
    needsFlowInfo = FlowInfoRequirement.BothFlows,

    // TODO: set limits on the loop iteration to avoid huge unrolled code

    (Elidable & HasOpcode(LABEL) & MatchX(1) & MatchParameter(2)) ~
      (Elidable & HasOpcodeIn(ASL, LSR, ROL, ROR, DEC, INC) & Not(ConcernsX)).*.capture(5) ~
      (Elidable & HasOpcode(DEX)) ~
      (Elidable & HasOpcode(BNE) & MatchParameter(2)) ~~> { (code, ctx) =>
      val iters = ctx.get[Int](1)
      val shift = ctx.get[List[AssemblyLine]](5)
      List.fill(iters)(shift).flatten :+ AssemblyLine.immediate(LDX, 0)
    },

    (Elidable & HasOpcode(LABEL) & MatchY(1) & MatchParameter(2))~
      (Elidable & HasOpcodeIn(ASL, LSR, ROL, ROR, DEC, INC) & Not(ConcernsY)).*.capture(5) ~
      (Elidable & HasOpcode(DEY)) ~
      (Elidable & HasOpcode(BNE) & MatchParameter(2)) ~~> { (code, ctx) =>
      val iters = ctx.get[Int](1)
      val shift = ctx.get[List[AssemblyLine]](5)
      List.fill(iters)(shift).flatten :+ AssemblyLine.immediate(LDY, 0)
    },

  )

  lazy val SimplifiableComparison = new RuleBasedAssemblyOptimization("Simplifiable comparison",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,

    (Elidable & HasOpcode(LDA)) ~
      (Elidable & HasOpcode(CMP) & DoesntMatterWhatItDoesWith(State.A, State.N)) ~
      (Elidable & HasOpcode(BCC) & DoesntMatterWhatItDoesWith(State.C) & MatchParameter(1)) ~
      (Elidable & HasOpcode(BEQ) & DoesntMatterWhatItDoesWith(State.C) & MatchParameter(1)) ~~> { code =>
      List(
        code(1).copy(opcode = LDA),
        code(0).copy(opcode = CMP),
        code(2).copy(opcode = BCS))
    },

    (Elidable & HasOpcode(LDA)) ~
      (Elidable & HasOpcode(CMP) & DoesntMatterWhatItDoesWith(State.A, State.N)) ~
      (Elidable & HasOpcode(BEQ) & MatchParameter(2)) ~
      (Elidable & HasOpcode(BCS) & DoesntMatterWhatItDoesWith(State.C) & MatchParameter(1)) ~
      (Elidable & HasCallerCount(1) & MatchParameter(2)) ~~> { code =>
      List(
        code(1).copy(opcode = LDA),
        code(0).copy(opcode = CMP),
        code(3).copy(opcode = BCC))
    },

    (Elidable & HasOpcode(TXA)) ~
      (Elidable & HasOpcode(CMP) & DoesntMatterWhatItDoesWith(State.A) & HasAddrModeIn(Absolute, ZeroPage, Immediate)) ~~> { code =>
      List(code.last.copy(opcode = CPX))
    },

    (Elidable & HasOpcode(TYA)) ~
      (Elidable & HasOpcode(CMP) & DoesntMatterWhatItDoesWith(State.A) & HasAddrModeIn(Absolute, ZeroPage, Immediate)) ~~> { code =>
      List(code.last.copy(opcode = CPY))
    },

    (Elidable & HasOpcode(EOR) & MatchImmediate(0)) ~
      (Elidable & HasOpcode(CMP) & MatchImmediate(1) & DoesntMatterWhatItDoesWith(State.A, State.N, State.C)) ~~> { (code, ctx) =>
      List(code.last.copy(parameter = CompoundConstant(MathOperator.Exor, ctx.get[Constant](0), ctx.get[Constant](1))))
    },

    MultipleAssemblyRules(for {
      cmpZero <- Seq(false, true)
      rts <- Seq(false, true)
      beq <- Seq(false, true)
      nonzeroFirst <- Seq(false, true)
    } yield {
      (Elidable & HasOpcode(AND) & MatchNumericImmediate(0)) ~
        Where(ctx => {
          val mask = ctx.get[Int](0)
          (mask == 1 || mask == 2 || mask == 4)
        }) ~
        (if (cmpZero) Elidable & HasOpcode(CMP) & HasImmediate(0) else Elidable & HasOpcode(CMP) & MatchNumericImmediate(0)) ~
        (Elidable & HasOpcode(if (beq) BEQ else BNE) & MatchParameter(5)) ~
        (Elidable & HasOpcode(LDA) & (if (nonzeroFirst) MatchNumericImmediate(1) else HasImmediate(0))) ~
        (if (rts) Elidable & HasOpcode(RTS) else Elidable & HasOpcodeIn(JMP, BRA, if (nonzeroFirst) BNE else BEQ) & MatchParameter(6)) ~
        (Elidable & HasOpcode(LABEL) & MatchParameter(5) & IsNotALabelUsedManyTimes) ~
        (Elidable & HasOpcode(LDA) & (if (nonzeroFirst) HasImmediate(0) else MatchNumericImmediate(1))) ~
        Where(ctx => {
          val mask = ctx.get[Int](1)
          (mask == 1 || mask == 2 || mask == 4)
        }) ~
        (Elidable & (if (rts) HasOpcode(RTS) else HasOpcode(LABEL) & MatchParameter(6) & IsNotALabelUsedManyTimes) & DoesntMatterWhatItDoesWith(State.C, State.Z, State.N)) ~~> { (code, ctx) =>
        val inverted = cmpZero ^ beq ^ nonzeroFirst
        val andMask = ctx.get[Int](0)
        val positiveResult = ctx.get[Int](1)
        val prepare = if (inverted) {
          List(code.head, AssemblyLine.immediate(EOR, andMask))
        } else {
          List(code.head)
        }
        val shifts = (andMask, positiveResult) match {
          case (1, 1) | (2, 2) | (4, 4) => Nil
          case (1, 2) | (2, 4) => List(AssemblyLine.implied(ASL))
          case (2, 1) | (4, 2) => List(AssemblyLine.implied(LSR))
          case (1, 4) => List(AssemblyLine.implied(ASL), AssemblyLine.implied(ASL))
          case (4, 1) => List(AssemblyLine.implied(LSR), AssemblyLine.implied(LSR))
          case _ => ???
        }
        if (rts) {
          prepare ++ shifts ++ List(code(4))
        } else {
          prepare ++ shifts
        }
      }
    }),

    (Elidable & HasOpcode(LDA) & HasImmediate(1)) ~
      (Elidable & HasOpcode(BIT)) ~
      (Elidable & HasOpcode(BEQ) & MatchParameter(1)) ~
      (Elidable & HasOpcode(LDA) & HasImmediate(0)) ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(1) & IsNotALabelUsedManyTimes & DoesntMatterWhatItDoesWith(State.N, State.Z, State.C, State.V)) ~~> { code =>
      List(code(1).copy(opcode = LDA), code.head.copy(opcode = AND), code.head.copy(opcode = EOR))
    },

    (Elidable & HasOpcode(LDA) & HasImmediate(1)) ~
      (Elidable & HasOpcode(BIT)) ~
      (Elidable & HasOpcode(BNE) & MatchParameter(1)) ~
      (Elidable & HasOpcode(LDA) & HasImmediate(0)) ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(1) & IsNotALabelUsedManyTimes & DoesntMatterWhatItDoesWith(State.N, State.Z, State.C, State.V)) ~~> { code =>
      List(code(1).copy(opcode = LDA), code.head.copy(opcode = AND))
    },

    (Elidable & HasOpcode(CMP) & HasAddrMode(Immediate)) ~
      (Elidable & HasOpcodeIn(BEQ, BNE) & MatchParameter(1)) ~
      (Elidable & HasAddrMode(Implied) & HasOpcodeIn(INC, DEC) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.C, State.V)) ~
      (Elidable & HasOpcode(JMP) & HasAddrModeIn(Absolute, LongAbsolute)) ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.C, State.V, State.A)) ~~> { code =>
      val delta = code(2).opcode match {
        case INC => +1
        case DEC => -1
      }
      val branch = code(1).opcode match {
        case BEQ => BNE
        case BNE => BEQ
      }
      List(
        code(2),
        code.head.copy(parameter = (code.head.parameter + delta).quickSimplify),
        code(1).copy(opcode = branch, parameter = code(3).parameter),
        code(4))
    },

    (Elidable & HasOpcode(CPX) & HasAddrMode(Immediate)) ~
      (Elidable & HasOpcodeIn(BEQ, BNE) & MatchParameter(1)) ~
      (Elidable & HasAddrMode(Implied) & HasOpcodeIn(INX, DEX) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.C, State.V)) ~
      (Elidable & HasOpcode(JMP) & HasAddrModeIn(Absolute, LongAbsolute)) ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.C, State.V, State.X)) ~~> { code =>
      val delta = code(2).opcode match {
        case INX => +1
        case DEX => -1
      }
      val branch = code(1).opcode match {
        case BEQ => BNE
        case BNE => BEQ
      }
      List(
        code(2),
        code.head.copy(parameter = (code.head.parameter + delta).quickSimplify),
        code(1).copy(opcode = branch, parameter = code(3).parameter),
        code(4))
    },

    (Elidable & HasOpcode(CPY) & HasAddrMode(Immediate)) ~
      (Elidable & HasOpcodeIn(BEQ, BNE) & MatchParameter(1)) ~
      (Elidable & HasAddrMode(Implied) & HasOpcodeIn(INY, DEY) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.C, State.V)) ~
      (Elidable & HasOpcode(JMP) & HasAddrModeIn(Absolute, LongAbsolute)) ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.C, State.V, State.Y)) ~~> { code =>
      val delta = code(2).opcode match {
        case INY => +1
        case DEY => -1
      }
      val branch = code(1).opcode match {
        case BEQ => BNE
        case BNE => BEQ
      }
      List(
        code(2),
        code.head.copy(parameter = (code.head.parameter + delta).quickSimplify),
        code(1).copy(opcode = branch, parameter = code(3).parameter),
        code(4))
    },
  )

  private val powersOf2: List[(Int, Int)] = List(
    1 -> 0,
    2 -> 1,
    4 -> 2,
    8 -> 3,
    16 -> 4,
    32 -> 5,
    64 -> 6
  )

  lazy val OptimizableMasking = new RuleBasedAssemblyOptimization("Simplifiable masking",
      needsFlowInfo = FlowInfoRequirement.BackwardFlow,

    MultipleAssemblyRules((for{
        (sourceMask, sourceShift) <- powersOf2
        (targetMask, targetShift) <- powersOf2
        shiftOp = if (sourceShift > targetShift) LSR else ASL
        shift = if (sourceShift > targetShift) sourceShift - targetShift else targetShift - sourceShift
        if shift < 2
    } yield {
      List(

        // LDA:

        (HasOpcode(AND) & HasImmediate(sourceMask)) ~
          (Elidable & HasOpcode(BEQ) & MatchParameter(10)) ~
          (Elidable & HasOpcode(LDA) & HasImmediate(targetMask)) ~
          (Elidable & HasOpcodeIn(JMP, BNE, BRA, BPL) & MatchParameter(11)) ~
          (Elidable & HasOpcode(LABEL) & IsNotALabelUsedManyTimes & MatchParameter(10)) ~
          (Elidable & HasOpcode(LDA) & HasImmediate(0)) ~
          (Elidable & HasOpcode(LABEL) & IsNotALabelUsedManyTimes & MatchParameter(11) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.C)) ~~>
          (c => c.head :: List.fill(shift)(AssemblyLine.implied(shiftOp).pos(c(2).source))),

        (HasOpcode(AND) & HasImmediate(sourceMask)) ~
          (Elidable & HasOpcode(BNE) & MatchParameter(10)) ~
          (Elidable & HasOpcode(LDA) & HasImmediate(targetMask)) ~
          (Elidable & HasOpcodeIn(JMP, BEQ, BRA, BPL) & MatchParameter(11)) ~
          (Elidable & HasOpcode(LABEL) & IsNotALabelUsedManyTimes & MatchParameter(10)) ~
          (Elidable & HasOpcode(LDA) & HasImmediate(0)) ~
          (Elidable & HasOpcode(LABEL) & IsNotALabelUsedManyTimes & MatchParameter(11) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.C)) ~~>
          (c => List(c.head, AssemblyLine.immediate(EOR, sourceMask).pos(c(1).source)) ++ List.fill(shift)(AssemblyLine.implied(shiftOp).pos(c(2).source))),

        (HasOpcode(AND) & HasImmediate(sourceMask)) ~
          (Elidable & HasOpcode(BEQ) & MatchParameter(10)) ~
          (Elidable & HasOpcode(LDA) & HasImmediate(0)) ~
          (Elidable & HasOpcodeIn(JMP, BNE, BRA, BPL) & MatchParameter(11)) ~
          (Elidable & HasOpcode(LABEL) & IsNotALabelUsedManyTimes & MatchParameter(10)) ~
          (Elidable & HasOpcode(LDA) & HasImmediate(targetMask)) ~
          (Elidable & HasOpcode(LABEL) & IsNotALabelUsedManyTimes & MatchParameter(11) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.C)) ~~>
          (c => List(c.head, AssemblyLine.immediate(EOR, sourceMask).pos(c(1).source)) ++ List.fill(shift)(AssemblyLine.implied(shiftOp).pos(c(2).source))),

        (HasOpcode(AND) & HasImmediate(sourceMask)) ~
          (Elidable & HasOpcode(BNE) & MatchParameter(10)) ~
          (Elidable & HasOpcode(LDA) & HasImmediate(0)) ~
          (Elidable & HasOpcodeIn(JMP, BEQ, BRA, BPL) & MatchParameter(11)) ~
          (Elidable & HasOpcode(LABEL) & IsNotALabelUsedManyTimes & MatchParameter(10)) ~
          (Elidable & HasOpcode(LDA) & HasImmediate(targetMask)) ~
          (Elidable & HasOpcode(LABEL) & IsNotALabelUsedManyTimes & MatchParameter(11) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.C)) ~~>
          (c => c.head ::  List.fill(shift)(AssemblyLine.implied(shiftOp).pos(c(2).source))),

        // LDY

        (HasOpcode(AND) & HasImmediate(sourceMask)) ~
          (Elidable & HasOpcode(BEQ) & MatchParameter(10)) ~
          (Elidable & HasOpcode(LDY) & HasImmediate(targetMask)) ~
          (Elidable & HasOpcodeIn(JMP, BNE, BRA, BPL) & MatchParameter(11)) ~
          (Elidable & HasOpcode(LABEL) & IsNotALabelUsedManyTimes & MatchParameter(10)) ~
          (Elidable & (HasOpcode(LDY) & HasImmediate(0) | HasOpcode(TAY))) ~
          (Elidable & HasOpcode(LABEL) & IsNotALabelUsedManyTimes & MatchParameter(11) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.C, State.A)) ~~>
          (c => c.head :: List.fill(shift)(AssemblyLine.implied(shiftOp).pos(c(2).source)) ++ List(AssemblyLine.implied(TAY))),

        (HasOpcode(AND) & HasImmediate(sourceMask)) ~
          (Elidable & HasOpcode(BNE) & MatchParameter(10)) ~
          (Elidable & HasOpcode(LDY) & HasImmediate(targetMask)) ~
          (Elidable & HasOpcodeIn(JMP, BEQ, BRA, BPL) & MatchParameter(11)) ~
          (Elidable & HasOpcode(LABEL) & IsNotALabelUsedManyTimes & MatchParameter(10)) ~
          (Elidable & HasOpcode(LDY) & HasImmediate(0)) ~
          (Elidable & HasOpcode(LABEL) & IsNotALabelUsedManyTimes & MatchParameter(11) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.C, State.A)) ~~>
          (c => List(c.head, AssemblyLine.immediate(EOR, sourceMask).pos(c(1).source)) ++ List.fill(shift)(AssemblyLine.implied(shiftOp).pos(c(2).source)) ++ List(AssemblyLine.implied(TAY))),

        (HasOpcode(AND) & HasImmediate(sourceMask)) ~
          (Elidable & HasOpcode(BEQ) & MatchParameter(10)) ~
          (Elidable & HasOpcode(LDY) & HasImmediate(0)) ~
          (Elidable & HasOpcodeIn(JMP, BNE, BRA, BPL) & MatchParameter(11)) ~
          (Elidable & HasOpcode(LABEL) & IsNotALabelUsedManyTimes & MatchParameter(10)) ~
          (Elidable & HasOpcode(LDY) & HasImmediate(targetMask)) ~
          (Elidable & HasOpcode(LABEL) & IsNotALabelUsedManyTimes & MatchParameter(11) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.C, State.A)) ~~>
          (c => List(c.head, AssemblyLine.immediate(EOR, sourceMask).pos(c(1).source)) ++ List.fill(shift)(AssemblyLine.implied(shiftOp).pos(c(2).source)) ++ List(AssemblyLine.implied(TAY))),

        (HasOpcode(AND) & HasImmediate(sourceMask)) ~
          (Elidable & HasOpcode(BNE) & MatchParameter(10)) ~
          (Elidable & (HasOpcode(LDY) & HasImmediate(0) | HasOpcode(TAY))) ~
          (Elidable & HasOpcodeIn(JMP, BEQ, BRA, BPL) & MatchParameter(11)) ~
          (Elidable & HasOpcode(LABEL) & IsNotALabelUsedManyTimes & MatchParameter(10)) ~
          (Elidable & HasOpcode(LDY) & HasImmediate(targetMask)) ~
          (Elidable & HasOpcode(LABEL) & IsNotALabelUsedManyTimes & MatchParameter(11) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.C, State.A)) ~~>
          (c => c.head ::  List.fill(shift)(AssemblyLine.implied(shiftOp).pos(c(2).source)) ++ List(AssemblyLine.implied(TAY))),


        // LDX

        (HasOpcode(AND) & HasImmediate(sourceMask)) ~
          (Elidable & HasOpcode(BEQ) & MatchParameter(10)) ~
          (Elidable & HasOpcode(LDX) & HasImmediate(targetMask)) ~
          (Elidable & HasOpcodeIn(JMP, BNE, BRA, BPL) & MatchParameter(11)) ~
          (Elidable & HasOpcode(LABEL) & IsNotALabelUsedManyTimes & MatchParameter(10)) ~
          (Elidable & (HasOpcode(LDX) & HasImmediate(0) | HasOpcode(TAX))) ~
          (Elidable & HasOpcode(LABEL) & IsNotALabelUsedManyTimes & MatchParameter(11) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.C, State.A)) ~~>
          (c => c.head :: List.fill(shift)(AssemblyLine.implied(shiftOp).pos(c(2).source)) ++ List(AssemblyLine.implied(TAX))),

        (HasOpcode(AND) & HasImmediate(sourceMask)) ~
          (Elidable & HasOpcode(BNE) & MatchParameter(10)) ~
          (Elidable & HasOpcode(LDX) & HasImmediate(targetMask)) ~
          (Elidable & HasOpcodeIn(JMP, BEQ, BRA, BPL) & MatchParameter(11)) ~
          (Elidable & HasOpcode(LABEL) & IsNotALabelUsedManyTimes & MatchParameter(10)) ~
          (Elidable & HasOpcode(LDX) & HasImmediate(0)) ~
          (Elidable & HasOpcode(LABEL) & IsNotALabelUsedManyTimes & MatchParameter(11) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.C, State.A)) ~~>
          (c => List(c.head, AssemblyLine.immediate(EOR, sourceMask).pos(c(1).source)) ++ List.fill(shift)(AssemblyLine.implied(shiftOp).pos(c(2).source)) ++ List(AssemblyLine.implied(TAX))),

        (HasOpcode(AND) & HasImmediate(sourceMask)) ~
          (Elidable & HasOpcode(BEQ) & MatchParameter(10)) ~
          (Elidable & HasOpcode(LDX) & HasImmediate(0)) ~
          (Elidable & HasOpcodeIn(JMP, BNE, BRA, BPL) & MatchParameter(11)) ~
          (Elidable & HasOpcode(LABEL) & IsNotALabelUsedManyTimes & MatchParameter(10)) ~
          (Elidable & HasOpcode(LDX) & HasImmediate(targetMask)) ~
          (Elidable & HasOpcode(LABEL) & IsNotALabelUsedManyTimes & MatchParameter(11) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.C, State.A)) ~~>
          (c => List(c.head, AssemblyLine.immediate(EOR, sourceMask).pos(c(1).source)) ++ List.fill(shift)(AssemblyLine.implied(shiftOp).pos(c(2).source)) ++ List(AssemblyLine.implied(TAX))),

        (HasOpcode(AND) & HasImmediate(sourceMask)) ~
          (Elidable & HasOpcode(BNE) & MatchParameter(10)) ~
          (Elidable & (HasOpcode(LDX) & HasImmediate(0) | HasOpcode(TAX))) ~
          (Elidable & HasOpcodeIn(JMP, BEQ, BRA, BPL) & MatchParameter(11)) ~
          (Elidable & HasOpcode(LABEL) & IsNotALabelUsedManyTimes & MatchParameter(10)) ~
          (Elidable & HasOpcode(LDX) & HasImmediate(targetMask)) ~
          (Elidable & HasOpcode(LABEL) & IsNotALabelUsedManyTimes & MatchParameter(11) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.C, State.A)) ~~>
          (c => c.head ::  List.fill(shift)(AssemblyLine.implied(shiftOp).pos(c(2).source)) ++ List(AssemblyLine.implied(TAX)))

        
        )
      }).flatten)

  )

  lazy val ReuseIndex = new RuleBasedAssemblyOptimization("Reuse right index register",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,

    (Elidable & HasOpcode(LDX) & MatchParameter(0) & HasAddrModeIn(Absolute, ZeroPage)) ~
      (Elidable & Linear & Not(ConcernsY) &
        (Not(ConcernsX) | HasOpcodeIn(LDA, STA, LDA_W, STA_W, ADC, ADC_W, SBC, SBC_W, ORA, ORA_W, AND, AND_W, EOR, EOR_W, CMP, CMP_W) & HasAddrMode(AbsoluteX))).*.capture(1) ~
      ((Elidable & HasOpcode(LDY) & MatchParameter(0) & HasAddrModeIn(Absolute, ZeroPage))).capture(2) ~~> { (code, ctx) =>
      code.head.copy(opcode = LDY) :: (ctx.get[List[AssemblyLine]](1).map(l => if (l.addrMode == AbsoluteX) l.copy(addrMode = AbsoluteY) else l) ++ ctx.get[List[AssemblyLine]](2))
    },

    (Elidable & HasOpcode(LDY) & MatchParameter(0) & HasAddrModeIn(Absolute, ZeroPage)) ~
      (Elidable & Linear & Not(ConcernsX) &
        (Not(ConcernsY) | HasOpcodeIn(LDA, STA, LDA_W, STA_W, ADC, ADC_W, SBC, SBC_W, ORA, ORA_W, AND, AND_W, EOR, EOR_W, CMP, CMP_W) & HasAddrMode(AbsoluteY))).*.capture(1) ~
      ((Elidable & HasOpcode(LDX) & MatchParameter(0) & HasAddrModeIn(Absolute, ZeroPage))).capture(2) ~~> { (code, ctx) =>
      code.head.copy(opcode = LDX) :: (ctx.get[List[AssemblyLine]](1).map(l => if (l.addrMode == AbsoluteY) l.copy(addrMode = AbsoluteX) else l) ++ ctx.get[List[AssemblyLine]](2))
    },

    (Elidable & HasOpcode(LDX) & MatchParameter(0) & HasAddrModeIn(Absolute, ZeroPage)) ~
      (Elidable & Linear & Not(ConcernsY) &
        (Not(ConcernsX) | HasOpcodeIn(LDA, STA, LDA_W, STA_W, ADC, ADC_W, SBC, SBC_W, ORA, ORA_W, AND, AND_W, EOR, EOR_W, CMP, CMP_W) & HasAddrMode(AbsoluteX))).*.capture(1) ~
      ((HasOpcode(LDX) & Not(MatchParameter(0))) ~
        (Linear & Not(ConcernsY)).* ~
        (Elidable & HasOpcode(LDY) & MatchParameter(0) & HasAddrModeIn(Absolute, ZeroPage))).capture(2) ~~> { (code, ctx) =>
      code.head.copy(opcode = LDY) :: (ctx.get[List[AssemblyLine]](1).map(l => if (l.addrMode == AbsoluteX) l.copy(addrMode = AbsoluteY) else l) ++ ctx.get[List[AssemblyLine]](2))
    },

    (Elidable & HasOpcode(LDY) & MatchParameter(0) & HasAddrModeIn(Absolute, ZeroPage)) ~
      (Elidable & Linear & Not(ConcernsX) &
        (Not(ConcernsY) | HasOpcodeIn(LDA, STA, LDA_W, STA_W, ADC, ADC_W, SBC, SBC_W, ORA, ORA_W, AND, AND_W, EOR, EOR_W, CMP, CMP_W) & HasAddrMode(AbsoluteY))).*.capture(1) ~
      ((HasOpcode(LDY) & Not(MatchParameter(0))) ~
        (Linear & Not(ConcernsY)).* ~
        (Elidable & HasOpcode(LDX) & MatchParameter(0) & HasAddrModeIn(Absolute, ZeroPage))).capture(2) ~~> { (code, ctx) =>
      code.head.copy(opcode = LDX) :: (ctx.get[List[AssemblyLine]](1).map(l => if (l.addrMode == AbsoluteY) l.copy(addrMode = AbsoluteX) else l) ++ ctx.get[List[AssemblyLine]](2))
    },


  )
}
