package millfork.assembly.mos.opt

import millfork.assembly.mos.Opcode._
import millfork.assembly.mos.AddrMode._
import millfork.assembly.mos.OpcodeClasses._
import millfork.assembly.AssemblyOptimization
import millfork.assembly.mos.{AssemblyLine, OpcodeClasses, State}
import millfork.env.{Constant, NumericConstant}
/**
  * @author Karol Stasiak
  */
object SixteenOptimizations {

  val AccumulatorSwapping = new RuleBasedAssemblyOptimization("Accumulator swapping",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    (Elidable & HasOpcode(PHA) & HasAccu8 & DoesntMatterWhatItDoesWith(State.AH, State.A, State.N, State.Z)) ~
      (Linear & Not(ConcernsStack)).* ~
      (Elidable & HasOpcode(PLA) & DoesntMatterWhatItDoesWith(State.AH)) ~~> { code =>
      AssemblyLine.implied(XBA) :: (code.tail.init :+ AssemblyLine.implied(XBA))
    },
    (Elidable & HasOpcode(TAX) & HasAccu8 & HasIndex8 & DoesntMatterWhatItDoesWith(State.AH, State.A, State.N, State.Z)) ~
      (Linear & Not(ConcernsX)).* ~
      (Elidable & HasOpcode(TXA) & DoesntMatterWhatItDoesWith(State.AH, State.X)) ~~> { code =>
      AssemblyLine.implied(XBA) :: (code.tail.init :+ AssemblyLine.implied(XBA))
    },
    (Elidable & HasOpcode(TAY) & HasAccu8 & HasIndex8 & DoesntMatterWhatItDoesWith(State.AH, State.A, State.N, State.Z)) ~
      (Linear & Not(ConcernsY)).* ~
      (Elidable & HasOpcode(TYA) & DoesntMatterWhatItDoesWith(State.AH, State.Y)) ~~> { code =>
      AssemblyLine.implied(XBA) :: (code.tail.init :+ AssemblyLine.implied(XBA))
    },
    (Elidable & HasOpcode(XBA) & DoesntMatterWhatItDoesWith(State.A, State.AH, State.N, State.Z)) ~~> { code =>
      Nil
    },
    (Elidable & HasOpcode(XBA) & DoesntMatterWhatItDoesWith(State.N, State.Z) & HasAccu8 & HasIndex8) ~
    (Elidable & HasOpcode(TAX)) ~
    (Elidable & HasOpcode(XBA)) ~
    (Elidable & HasOpcode(TXA) & DoesntMatterWhatItDoesWith(State.X, State.AH)) ~~> { code =>
      List(code.head)
    },
  )

  val RepSepWeakening = new RuleBasedAssemblyOptimization("REP/SEP weakening",
    needsFlowInfo = FlowInfoRequirement.BothFlows,

    (Elidable & HasOpcodeIn(SEP, REP) & HasImmediate(0)) ~~> (_ => Nil),

    (HasOpcode(SEP) & HasImmediate(0x20)) ~
      (Linear & Not(HasOpcodeIn(SEP, REP, PLP))).* ~
      (Elidable & HasOpcode(SEP) & HasImmediate(0x20)) ~~> (_.init),
    (HasOpcode(REP) & HasImmediate(0x20)) ~
      (Linear & Not(HasOpcodeIn(SEP, REP, PLP))).* ~
      (Elidable & HasOpcode(REP) & HasImmediate(0x20)) ~~> (_.init),
    (HasOpcode(SEP) & HasImmediate(0x10)) ~
      (Linear & Not(HasOpcodeIn(SEP, REP, PLP))).* ~
      (Elidable & HasOpcode(SEP) & HasImmediate(0x10)) ~~> (_.init),
    (HasOpcode(REP) & HasImmediate(0x10)) ~
      (Linear & Not(HasOpcodeIn(SEP, REP, PLP))).* ~
      (Elidable & HasOpcode(REP) & HasImmediate(0x10)) ~~> (_.init),

    (Elidable & HasOpcodeIn(SEP, REP) & MatchNumericImmediate(0) & DoesntMatterWhatItDoesWith(State.C)) ~
          Where(c => c.get[Int](0).&(0x1).!=(0)) ~~> { (code, ctx) =>
      val i = ctx.get[Int](0) & 0xFE
      if (i == 0) Nil else List(AssemblyLine.immediate(code.head.opcode, i))
    },
    (Elidable & HasOpcodeIn(SEP, REP) & MatchNumericImmediate(0) & DoesntMatterWhatItDoesWith(State.Z)) ~
          Where(c => c.get[Int](0).&(0x2).!=(0)) ~~> { (code, ctx) =>
      val i = ctx.get[Int](0) & 0xFD
      if (i == 0) Nil else List(AssemblyLine.immediate(code.head.opcode, i))
    },
    (Elidable & HasOpcodeIn(SEP, REP) & MatchNumericImmediate(0) & DoesntMatterWhatItDoesWith(State.D)) ~
          Where(c => c.get[Int](0).&(0x8).!=(0)) ~~> { (code, ctx) =>
      val i = ctx.get[Int](0) & 0xF7
      if (i == 0) Nil else List(AssemblyLine.immediate(code.head.opcode, i))
    },
    (Elidable & HasOpcodeIn(SEP, REP) & MatchNumericImmediate(0) & DoesntMatterWhatItDoesWith(State.W)) ~
          Where(c => c.get[Int](0).&(0x10).!=(0)) ~~> { (code, ctx) =>
      val i = ctx.get[Int](0) & 0xEF
      if (i == 0) Nil else List(AssemblyLine.immediate(code.head.opcode, i))
    },
    (Elidable & HasOpcodeIn(SEP, REP) & MatchNumericImmediate(0) & DoesntMatterWhatItDoesWith(State.M)) ~
          Where(c => c.get[Int](0).&(0x20).!=(0)) ~~> { (code, ctx) =>
      val i = ctx.get[Int](0) & 0xDF
      if (i == 0) Nil else List(AssemblyLine.immediate(code.head.opcode, i))
    },
    (Elidable & HasOpcodeIn(SEP, REP) & MatchNumericImmediate(0) & DoesntMatterWhatItDoesWith(State.V)) ~
          Where(c => c.get[Int](0).&(0x40).!=(0)) ~~> { (code, ctx) =>
      val i = ctx.get[Int](0) & 0xBF
      if (i == 0) Nil else List(AssemblyLine.immediate(code.head.opcode, i))
    },
    (Elidable & HasOpcodeIn(SEP, REP) & MatchNumericImmediate(0) & DoesntMatterWhatItDoesWith(State.N)) ~
          Where(c => c.get[Int](0).&(0x80).!=(0)) ~~> { (code, ctx) =>
      val i = ctx.get[Int](0) & 0x7F
      if (i == 0) Nil else List(AssemblyLine.immediate(code.head.opcode, i))
    },

    (Elidable & HasOpcode(SEP) & HasSet(State.C) & MatchNumericImmediate(0)) ~
      Where(c => c.get[Int](0).&(0x1).!=(0)) ~~> { (code, ctx) =>
      val i = ctx.get[Int](0) & 0xFE
      if (i == 0) Nil else List(AssemblyLine.immediate(SEP, i))
    },
    (Elidable & HasOpcode(REP) & HasClear(State.C) & MatchNumericImmediate(0)) ~
      Where(c => c.get[Int](0).&(0x1).!=(0)) ~~> { (code, ctx) =>
      val i = ctx.get[Int](0) & 0xFE
      if (i == 0) Nil else List(AssemblyLine.immediate(REP, i))
    },

    (Elidable & HasOpcode(SEP) & HasSet(State.W) & MatchNumericImmediate(0)) ~
      Where(c => c.get[Int](0).&(0x10).!=(0)) ~~> { (code, ctx) =>
      val i = ctx.get[Int](0) & 0xEF
      if (i == 0) Nil else List(AssemblyLine.immediate(SEP, i))
    },
    (Elidable & HasOpcode(REP) & HasClear(State.W) & MatchNumericImmediate(0)) ~
      Where(c => c.get[Int](0).&(0x10).!=(0)) ~~> { (code, ctx) =>
      val i = ctx.get[Int](0) & 0xEF
      if (i == 0) Nil else List(AssemblyLine.immediate(REP, i))
    },

    (Elidable & HasOpcode(SEP) & HasSet(State.M) & MatchNumericImmediate(0)) ~
      Where(c => c.get[Int](0).&(0x20).!=(0)) ~~> { (code, ctx) =>
      val i = ctx.get[Int](0) & 0xDF
      if (i == 0) Nil else List(AssemblyLine.immediate(SEP, i))
    },
    (Elidable & HasOpcode(REP) & HasClear(State.M) & MatchNumericImmediate(0)) ~
      Where(c => c.get[Int](0).&(0x20).!=(0)) ~~> { (code, ctx) =>
      val i = ctx.get[Int](0) & 0xDF
      if (i == 0) Nil else List(AssemblyLine.immediate(REP, i))
    },
  )


  val PointlessLoadAfterLoadOrStore = new RuleBasedAssemblyOptimization("Pointless 16-bit load after load or store",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    // TODO: flags!

    (HasOpcodeIn(LDA_W, STA_W) & HasAddrMode(WordImmediate) & MatchParameter(1)) ~
      (Linear & Not(ChangesA) & Not(ChangesAH)).* ~
      (Elidable & HasOpcode(LDA_W) & HasAddrMode(WordImmediate) & MatchParameter(1)) ~~> (_.init),

    (HasOpcodeIn(LDA_W, STA_W) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(ChangesA) & Not(ChangesAH) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAt(0, 1, LDA_W)).* ~
      (Elidable & HasOpcode(LDA_W) & MatchAddrMode(0) & MatchParameter(1)) ~~> (_.init),
  )

  val OptimizeZeroIndex = new RuleBasedAssemblyOptimization("Optimizing zero index for far pointers",
    needsFlowInfo = FlowInfoRequirement.ForwardFlow,
    (Elidable & HasY(0) /*& HasZ(0)*/ & HasIndex8 & HasAddrMode(LongIndexedY) & HasOpcodeIn(SupportsLongIndexedZ)) ~~> (code => code.map(_.copy(addrMode = LongIndexedZ))),
  )

  private val SupportsStackAddressing = HasOpcodeIn(
    ADC, AND, EOR, ORA, LDA, STA, SBC, CMP,
  )

  val OptimizeStackRelative = new RuleBasedAssemblyOptimization("Optimizing stack variables",
    needsFlowInfo = FlowInfoRequirement.BothFlows,

    ((Elidable & HasOpcode(TSX)) ~
      (Elidable & HasOpcode(INX)).*.captureLength(60) ~
      (Elidable & HasOpcode(DEX) & DoesntMatterWhatItDoesWith(State.N, State.Z)).*.captureLength(61)).capture(3) ~
      (Not(ConcernsX) & Not(ChangesS)).*.capture(2) ~
      (Elidable & SupportsStackAddressing & HasAddrMode(AbsoluteX) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.X)) ~
      Where(ctx => ctx.get[Constant](1) match {
        case NumericConstant(x, _) => x >= 0x100 && x <= 0x1ff
        case _ => false
      }) ~~> { (code, ctx) =>
      ctx.get[List[AssemblyLine]](2) ++ List(
        code.last.copy(addrMode = Stack, parameter = (ctx.get[Constant](1) + ctx.get[Int](60) - ctx.get[Int](61)-0x100).quickSimplify)
      )
    },

    ((Elidable & HasOpcode(TSX)) ~
      (Elidable & HasOpcode(INX)).*.captureLength(60) ~
      (Elidable & HasOpcode(DEX) & DoesntMatterWhatItDoesWith(State.N, State.Z)).*.captureLength(61)).capture(3) ~
      (Not(ConcernsX) & Not(ChangesS)).*.capture(2) ~
      (Elidable & SupportsStackAddressing & HasAddrMode(AbsoluteX) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~
      Where(ctx => ctx.get[Constant](1) match {
        case NumericConstant(x, _) => x >= 0x100 && x <= 0x1ff
        case _ => false
      }) ~~> { (code, ctx) =>
      ctx.get[List[AssemblyLine]](2) ++ List(
        code.last.copy(addrMode = Stack, parameter = (ctx.get[Constant](1) + ctx.get[Int](60) - ctx.get[Int](61)-0x100).quickSimplify)
      ) ++ ctx.get[List[AssemblyLine]](3)
    },

    ((Elidable & HasOpcode(TSX)) ~
      (Elidable & HasOpcode(INX)).*.captureLength(60) ~
      (Elidable & HasOpcode(DEX) & DoesntMatterWhatItDoesWith(State.N, State.Z)).*.captureLength(61)).capture(3) ~
      (Not(ConcernsX) & Not(ChangesS)).*.capture(2) ~
      (Elidable & HasOpcode(LDY) & HasAddrMode(AbsoluteX) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.X, State.A)) ~
      Where(ctx => ctx.get[Constant](1) match {
        case NumericConstant(x, _) => x >= 0x100 && x <= 0x1ff
        case _ => false
      }) ~~> { (code, ctx) =>
      ctx.get[List[AssemblyLine]](2) ++ List(
        AssemblyLine(LDA, Stack,(ctx.get[Constant](1) + ctx.get[Int](60) - ctx.get[Int](61)-0x100).quickSimplify),
        AssemblyLine.implied(TAY)
      )
    },

    ((Elidable & HasOpcode(TSX)) ~
      (Elidable & HasOpcode(INX)).*.captureLength(60) ~
      (Elidable & HasOpcode(DEX) & DoesntMatterWhatItDoesWith(State.N, State.Z)).*.captureLength(61)).capture(3) ~
      (Not(ConcernsX) & Not(ChangesS)).*.capture(2) ~
      (Elidable & HasOpcode(LDY) & HasAddrMode(AbsoluteX) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.A)) ~
      Where(ctx => ctx.get[Constant](1) match {
        case NumericConstant(x, _) => x >= 0x100 && x <= 0x1ff
        case _ => false
      }) ~~> { (code, ctx) =>
      ctx.get[List[AssemblyLine]](2) ++ List(
        AssemblyLine(LDA, Stack,(ctx.get[Constant](1) + ctx.get[Int](60) - ctx.get[Int](61)-0x100).quickSimplify),
        AssemblyLine.implied(TAY)
      ) ++ ctx.get[List[AssemblyLine]](3)
    },
  )

  val PointlessIndexTransfers = new RuleBasedAssemblyOptimization("Pointless index transfer",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    (Elidable & HasOpcode(TXY) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~
      (Not(ChangesX) & Not(ChangesY) & Linear & (Not(ConcernsY) | Elidable & HasAddrMode(AbsoluteY) & SupportsAbsoluteX)).* ~
      (Not(ReadsY) & DoesntMatterWhatItDoesWith(State.Y)) ~~> (_.tail.map { l =>
      if (l.addrMode == AbsoluteY) l.copy(addrMode = AbsoluteX) else l
    }),
    (Elidable & HasOpcode(TYX) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~
      (Not(ChangesX) & Not(ChangesY) & Linear & (Not(ConcernsX) | Elidable & HasAddrMode(AbsoluteX) & SupportsAbsoluteY)).* ~
      (Not(ReadsX) & DoesntMatterWhatItDoesWith(State.X)) ~~> (_.tail.map { l =>
      if (l.addrMode == AbsoluteX) l.copy(addrMode = AbsoluteY) else l
    }),
  )

  // TODO: rewrite most 8-bit optimizations that are applicable to 16-bit code

  val AllForEmulation: List[AssemblyOptimization[AssemblyLine]] = List(
    AccumulatorSwapping,
    OptimizeStackRelative,
    OptimizeZeroIndex,
    PointlessIndexTransfers,
    RepSepWeakening,
  )

  val AllForNative: List[AssemblyOptimization[AssemblyLine]] = List(
    PointlessLoadAfterLoadOrStore
  )

  val All: List[AssemblyOptimization[AssemblyLine]] = AllForEmulation ++ AllForNative
}
