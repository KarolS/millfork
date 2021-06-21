package millfork.assembly.m6809.opt


import millfork.assembly.AssemblyOptimization
import millfork.assembly.m6809.MOpcode._
import millfork.assembly.m6809.{Absolute, DAccumulatorIndexed, Immediate, Indexed, InherentA, InherentB, LongRelative, MAddrMode, MLine, MState, PostIncremented, RegisterSet}
import millfork.env.{CompoundConstant, Constant, MathOperator}
import millfork.node.M6809Register

/**
  * @author Karol Stasiak
  */
object AlwaysGoodMOptimizations {

  val PointlessLoad = new RuleBasedAssemblyOptimization("Pointless load",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    (Elidable & HasOpcodeIn(LDA, ANDA, ORA, EORA) & DoesntMatterWhatItDoesWith(MState.A, MState.NF, MState.ZF, MState.VF)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(LDB, ANDB, ORB, EORB) & DoesntMatterWhatItDoesWith(MState.B, MState.NF, MState.ZF, MState.VF)) ~~> (_ => Nil),
    (Elidable & HasOpcode(LDD) & DoesntMatterWhatItDoesWith(MState.A, MState.B, MState.NF, MState.ZF, MState.VF)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(LDX, LEAX) & DoesntMatterWhatItDoesWith(MState.X, MState.NF, MState.ZF, MState.VF)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(LDY, LEAY) & DoesntMatterWhatItDoesWith(MState.Y, MState.NF, MState.ZF, MState.VF)) ~~> (_ => Nil),
    (Elidable & HasOpcode(STB) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(ChangesB) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(LDB) & MatchAddrMode(0) & MatchParameter(1)
        & DoesntMatterWhatItDoesWith(MState.NF, MState.ZF, MState.VF)) ~~> (_.init),
    (Elidable & HasOpcode(STD) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(ChangesB) & Not(ChangesA) & DoesntChangeIndexingInAddrMode(0) & DoesntChangeMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(LDD) & MatchAddrMode(0) & MatchParameter(1)
        & DoesntMatterWhatItDoesWith(MState.NF, MState.ZF, MState.VF)) ~~> (_.init),
  )

  val PointlessCompare = new RuleBasedAssemblyOptimization("Pointless compare",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    (HasOpcodeIn(LDA, ANDA, ORA, EORA, ADDA, ADCA, SUBA, SBCA)) ~
      (Elidable & HasOpcode(CMPA) & HasImmediate(0) & DoesntMatterWhatItDoesWith(MState.VF, MState.CF, MState.HF))  ~~> {code => code.init},
    (HasOpcodeIn(LDB, ANDB, ORB, EORB, ADDB, ADCB, SUBB, SBCB)) ~
      (Elidable & HasOpcode(CMPB) & HasImmediate(0) & DoesntMatterWhatItDoesWith(MState.VF, MState.CF, MState.HF)) ~~> {code => code.init},

    (Elidable & HasOpcode(EORA) & HasAddrMode(Immediate) & MatchParameter(0)) ~
      (Elidable & HasOpcode(CMPA) & HasAddrMode(Immediate) & MatchParameter(1)
        & DoesntMatterWhatItDoesWith(MState.A, MState.NF, MState.VF, MState.CF, MState.HF)) ~~> { (code, ctx) =>
      val c0 = ctx.get[Constant](0)
      val c1 = ctx.get[Constant](1)
      List(code.last.copy(parameter = CompoundConstant(MathOperator.Exor, c0, c1).quickSimplify))
    },
    (Elidable & HasOpcode(EORB) & HasAddrMode(Immediate) & MatchParameter(0)) ~
      (Elidable & HasOpcode(CMPB) & HasAddrMode(Immediate) & MatchParameter(1)
        & DoesntMatterWhatItDoesWith(MState.B, MState.NF, MState.VF, MState.CF, MState.HF)) ~~> { (code, ctx) =>
      val c0 = ctx.get[Constant](0)
      val c1 = ctx.get[Constant](1)
      List(code.last.copy(parameter = CompoundConstant(MathOperator.Exor, c0, c1).quickSimplify))
    },
  )

  val SimplifiableZeroStore = new RuleBasedAssemblyOptimization("Simplifiable zero store",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    (Elidable & HasOpcode(LDA) & HasImmediate(0) & DoesntMatterWhatItDoesWith(MState.CF)) ~~> {
      _ => List(MLine.inherentA(CLR))
    },
    (Elidable & HasOpcode(LDB) & HasImmediate(0) & DoesntMatterWhatItDoesWith(MState.CF)) ~~> {
      _ => List(MLine.inherentB(CLR))
    },
    (Elidable & HasOpcode(LDD) & HasImmediate(0) & DoesntMatterWhatItDoesWith(MState.A, MState.CF)) ~~> {
      _ => List(MLine.inherentB(CLR))
    },
    (Elidable & HasOpcode(LDD) & HasImmediate(0) & DoesntMatterWhatItDoesWith(MState.B, MState.CF)) ~~> {
      _ => List(MLine.inherentA(CLR))
    },
    (Elidable & HasOpcode(STA) & HasA(0) & DoesntMatterWhatItDoesWith(MState.CF)) ~~> {
      code => code.map(_.copy(opcode = CLR))
    },
    (Elidable & HasOpcode(STB) & HasB(0) & DoesntMatterWhatItDoesWith(MState.CF)) ~~> {
      code => code.map(_.copy(opcode = CLR))
    },
  )

  val SimplifiableComparison = new RuleBasedAssemblyOptimization("Simplifiable comparison",
    needsFlowInfo = FlowInfoRequirement.BothFlows,

    (Elidable & HasOpcode(EORB) & HasAddrMode(Immediate) & MatchParameter(0)) ~
      (Elidable & HasOpcode(CMPB) & HasAddrMode(Immediate) & MatchParameter(1)
        & DoesntMatterWhatItDoesWith(MState.CF, MState.B, MState.VF, MState.HF)) ~~> { (code, ctx) =>
      val c0 = ctx.get[Constant](0)
      val c1 = ctx.get[Constant](1)
      List(code.last.copy(parameter = CompoundConstant(MathOperator.Exor, c0, c1).quickSimplify))
    },
    (Elidable & HasOpcode(ADDB) & HasImmediate(1)) ~
      (Elidable & HasOpcode(CMPB) & HasAddrMode(Immediate) & MatchParameter(1)
        & DoesntMatterWhatItDoesWith(MState.CF, MState.B, MState.VF, MState.HF)) ~~> { (code, ctx) =>
      val c1 = ctx.get[Constant](1)
      List(code.last.copy(parameter = (c1 - 1).quickSimplify))
    },
    (Elidable & HasOpcode(SUBB) & HasImmediate(1)) ~
      (Elidable & HasOpcode(CMPB) & HasAddrMode(Immediate) & MatchParameter(1)
        & DoesntMatterWhatItDoesWith(MState.CF, MState.B, MState.VF, MState.HF)) ~~> { (code, ctx) =>
      val c1 = ctx.get[Constant](1)
      List(code.last.copy(parameter = (c1 + 1).quickSimplify))
    },
    (Elidable & HasOpcode(INC) & HasAddrMode(InherentB)) ~
      (Elidable & HasOpcode(CMPB) & HasAddrMode(Immediate) & MatchParameter(1)
        & DoesntMatterWhatItDoesWith(MState.CF, MState.B, MState.VF, MState.HF)) ~~> { (code, ctx) =>
      val c1 = ctx.get[Constant](1)
      List(code.last.copy(parameter = (c1 - 1).quickSimplify))
    },
    (Elidable & HasOpcode(DEC) & HasAddrMode(InherentB)) ~
      (Elidable & HasOpcode(CMPB) & HasAddrMode(Immediate) & MatchParameter(1)
        & DoesntMatterWhatItDoesWith(MState.CF, MState.B, MState.VF, MState.HF)) ~~> { (code, ctx) =>
      val c1 = ctx.get[Constant](1)
      List(code.last.copy(parameter = (c1 + 1).quickSimplify))
    },

    (Elidable & HasOpcode(EORA) & HasAddrMode(Immediate) & MatchParameter(0)) ~
      (Elidable & HasOpcode(CMPA) & HasAddrMode(Immediate) & MatchParameter(1)
        & DoesntMatterWhatItDoesWith(MState.CF, MState.A, MState.VF, MState.HF)) ~~> { (code, ctx) =>
      val c0 = ctx.get[Constant](0)
      val c1 = ctx.get[Constant](1)
      List(code.last.copy(parameter = CompoundConstant(MathOperator.Exor, c0, c1).quickSimplify))
    },
    (Elidable & HasOpcode(ADDA) & HasImmediate(1)) ~
      (Elidable & HasOpcode(CMPA) & HasAddrMode(Immediate) & MatchParameter(1)
        & DoesntMatterWhatItDoesWith(MState.CF, MState.A, MState.VF, MState.HF)) ~~> { (code, ctx) =>
      val c1 = ctx.get[Constant](1)
      List(code.last.copy(parameter = (c1 - 1).quickSimplify))
    },
    (Elidable & HasOpcode(SUBA) & HasImmediate(1)) ~
      (Elidable & HasOpcode(CMPA) & HasAddrMode(Immediate) & MatchParameter(1)
        & DoesntMatterWhatItDoesWith(MState.CF, MState.A, MState.VF, MState.HF)) ~~> { (code, ctx) =>
      val c1 = ctx.get[Constant](1)
      List(code.last.copy(parameter = (c1 + 1).quickSimplify))
    },
    (Elidable & HasOpcode(INC) & HasAddrMode(InherentA)) ~
      (Elidable & HasOpcode(CMPA) & HasAddrMode(Immediate) & MatchParameter(1)
        & DoesntMatterWhatItDoesWith(MState.CF, MState.A, MState.VF, MState.HF)) ~~> { (code, ctx) =>
      val c1 = ctx.get[Constant](1)
      List(code.last.copy(parameter = (c1 - 1).quickSimplify))
    },
    (Elidable & HasOpcode(DEC) & HasAddrMode(InherentA)) ~
      (Elidable & HasOpcode(CMPA) & HasAddrMode(Immediate) & MatchParameter(1)
        & DoesntMatterWhatItDoesWith(MState.CF, MState.A, MState.VF, MState.HF)) ~~> { (code, ctx) =>
      val c1 = ctx.get[Constant](1)
      List(code.last.copy(parameter = (c1 + 1).quickSimplify))
    },

  )

  val PointlessRegisterTransfers = new RuleBasedAssemblyOptimization("Pointless register transfers",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    (Elidable & IsTfr(M6809Register.D, M6809Register.X)) ~
      (Elidable & IsTfr(M6809Register.X, M6809Register.D)) ~~> (_.init),
    (Elidable & IsTfr(M6809Register.A, M6809Register.B)) ~
      (Elidable & IsTfr(M6809Register.B, M6809Register.A)) ~~> (_.init),
    (Elidable & IsTfrTo(M6809Register.B) & DoesntMatterWhatItDoesWith(MState.B)) ~~> (_ => Nil),
    (Elidable & IsTfrTo(M6809Register.A) & DoesntMatterWhatItDoesWith(MState.A)) ~~> (_ => Nil),
    (Elidable & IsTfrTo(M6809Register.D) & DoesntMatterWhatItDoesWith(MState.A, MState.B)) ~~> (_ => Nil),
    (Elidable & IsTfrTo(M6809Register.X) & DoesntMatterWhatItDoesWith(MState.X)) ~~> (_ => Nil),
  )

  private val PullByte: MAddrMode = PostIncremented(M6809Register.S, 1, indirect = false)
  private val PullWord: MAddrMode = PostIncremented(M6809Register.S, 2, indirect = false)

  import M6809Register._

  val PointlessStashing = new RuleBasedAssemblyOptimization("Pointless stashing",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,

    (Elidable & HasOpcode(LDB) & HasAddrMode(Immediate)) ~
      (Elidable & HasOpcode(PSHS) & HasAddrMode(RegisterSet(Set(B))) & DoesntMatterWhatItDoesWith(MState.NF, MState.ZF, MState.VF, MState.B)) ~
      (Linear & Not(ConcernsS)).* ~
      (Elidable & HasOpcodeIn(CanHaveImmediateAndIndexedByte) & HasAddrMode(PullByte)) ~~> { code =>
      code.drop(2).init :+ code.last.copy(addrMode = Immediate, parameter = code.head.parameter)
    },
    (Elidable & HasOpcode(LDD) & HasAddrMode(Immediate)) ~
      (Elidable & HasOpcode(PSHS) & HasAddrMode(RegisterSet(Set(D))) & DoesntMatterWhatItDoesWith(MState.NF, MState.ZF, MState.VF, MState.B, MState.A)) ~
      (Linear & Not(ConcernsS)).* ~
      (Elidable & HasOpcodeIn(CanHaveImmediateAndIndexedWord) & HasAddrMode(PullWord)) ~~> { code =>
      code.drop(2).init :+ code.last.copy(addrMode = Immediate, parameter = code.head.parameter)
    },

    (Elidable & HasOpcode(LDB) & HasAddrMode(Immediate)) ~
      (Elidable & HasOpcode(PSHS) & HasAddrMode(RegisterSet(Set(B))) & DoesntMatterWhatItDoesWith(MState.NF, MState.ZF, MState.VF, MState.B)) ~
      (Linear & Not(ConcernsS)).* ~
      (Elidable & HasOpcode(PULS) & HasAddrMode(RegisterSet(Set(B))) & DoesntMatterWhatItDoesWith(MState.NF, MState.ZF, MState.VF)) ~~> { code =>
      code.drop(2).init :+ code.head
    },
    (Elidable & HasOpcode(LDB) & HasAddrMode(Immediate)) ~
      (Elidable & HasOpcode(PSHS) & HasAddrMode(RegisterSet(Set(B))) & DoesntMatterWhatItDoesWith(MState.NF, MState.ZF, MState.VF, MState.B)) ~
      (Linear & Not(ConcernsS)).* ~
      (Elidable & HasOpcode(PULS) & HasAddrMode(RegisterSet(Set(A))) & DoesntMatterWhatItDoesWith(MState.NF, MState.ZF, MState.VF)) ~~> { code =>
      code.drop(2).init :+ code.head.copy(opcode = LDA)
    },
    (Elidable & HasOpcode(LDD) & HasAddrMode(Immediate)) ~
      (Elidable & HasOpcode(PSHS) & HasAddrMode(RegisterSet(Set(D))) & DoesntMatterWhatItDoesWith(MState.NF, MState.ZF, MState.VF, MState.B, MState.A)) ~
      (Linear & Not(ConcernsS)).* ~
      (Elidable & HasOpcode(PULS) & HasAddrMode(RegisterSet(Set(D))) & DoesntMatterWhatItDoesWith(MState.NF, MState.ZF, MState.VF)) ~~> { code =>
      code.drop(2).init :+ code.head
    },
    (Elidable & HasOpcode(LDD) & HasAddrMode(Immediate)) ~
      (Elidable & HasOpcode(PSHS) & HasAddrMode(RegisterSet(Set(D))) & DoesntMatterWhatItDoesWith(MState.NF, MState.ZF, MState.VF, MState.B, MState.A)) ~
      (Linear & Not(ConcernsS)).* ~
      (Elidable & HasOpcode(PULS) & HasAddrMode(RegisterSet(Set(X))) & DoesntMatterWhatItDoesWith(MState.NF, MState.ZF, MState.VF)) ~~> { code =>
      code.drop(2).init :+ code.head.copy(opcode = LDX)
    },
  )

  val SimplifiableJumps = new RuleBasedAssemblyOptimization("Simplifiable jumps",
    needsFlowInfo = FlowInfoRequirement.JustLabels,
    (Elidable & HasOpcodeIn(ConditionalBranching) & MatchParameter(0)) ~
    (Elidable & HasOpcodeIn(BRA, JMP)) ~
    (Elidable & HasOpcodeIn(LABEL) & MatchParameter(0) & IsNotALabelUsedManyTimes) ~~> {code =>
      List(code(1).copy(opcode = invertBranching(code.head.opcode), addrMode = LongRelative))
    },
    (Elidable & HasOpcodeIn(ConditionalBranching) & MatchParameter(0)) ~
    (Elidable & HasOpcodeIn(BRA, JMP)) ~
    (Elidable & HasOpcodeIn(LABEL) & MatchParameter(0)) ~~> {code =>
      List(code(1).copy(opcode = invertBranching(code.head.opcode), addrMode = LongRelative), code(2))
    }
  )

  val SimplifiableArithmetics = new RuleBasedAssemblyOptimization("Simplifiable arithmetics",
    needsFlowInfo = FlowInfoRequirement.BothFlows,

    (Elidable & HasOpcode(LEAX)
      & HasAddrMode(DAccumulatorIndexed(X, indirect = false))
      & HasA(0)
      & DoesntMatterWhatItDoesWith(MState.VF, MState.ZF, MState.NF)) ~~>
      (_ => List(MLine.inherent(ABX))),

    (NotFixed & HasB(0) & HasOpcodeIn(ORB, EORB, ADDB) & DoesntMatterWhatItDoesWith(MState.VF, MState.ZF, MState.NF, MState.CF, MState.HF)) ~~>
      (_.map(_.copy(opcode = LDB))),
    (NotFixed & HasB(0xff) & HasOpcode(ANDB) & DoesntMatterWhatItDoesWith(MState.VF, MState.ZF, MState.NF, MState.CF, MState.HF)) ~~>
      (_.map(_.copy(opcode = LDB))),
    (NotFixed & HasA(0) & HasOpcodeIn(ORA, EORA, ADDA) & DoesntMatterWhatItDoesWith(MState.VF, MState.ZF, MState.NF, MState.CF, MState.HF)) ~~>
      (_.map(_.copy(opcode = LDA))),
    (NotFixed & HasA(0xff) & HasOpcode(ANDA) & DoesntMatterWhatItDoesWith(MState.VF, MState.ZF, MState.NF, MState.CF, MState.HF)) ~~>
      (_.map(_.copy(opcode = LDA))),
    (NotFixed & HasA(0) & HasB(0) & HasOpcodeIn(ADDD) & DoesntMatterWhatItDoesWith(MState.VF, MState.ZF, MState.NF, MState.CF, MState.HF)) ~~>
      (_.map(_.copy(opcode = LDD))),
    (Elidable & HasImmediate(0) & HasOpcodeIn(ORB, EORB, ADDB, ORA, EORA, ADDA, ADDD) & DoesntMatterWhatItDoesWith(MState.VF, MState.ZF, MState.NF, MState.CF, MState.HF)) ~~>
      (_ => Nil),
    (HasOpcode(LDB)) ~
      (Elidable & HasOpcode(ANDB) & HasAddrMode(Immediate) & MatchParameter(0)) ~
      (Elidable & HasOpcode(BNE) & MatchParameter(1)) ~
      (Elidable & HasOpcode(LDB)) ~
      (HasOpcode(ANDB) & HasAddrMode(Immediate) & MatchParameter(0)) ~
      (HasOpcode(BEQ)) ~
      (HasOpcode(LABEL) & MatchParameter(1)) ~~> { code =>
      List(code.head, code(3).copy(opcode = ORB)) ++ code.drop(4)
    },
    (Elidable & HasOpcode(ADDB) & HasImmediate(1) & DoesntMatterWhatItDoesWith(MState.VF, MState.CF, MState.HF)) ~~> { code => List(MLine.inherentB(INC)) },
    (Elidable & HasOpcode(ADDA) & HasImmediate(1) & DoesntMatterWhatItDoesWith(MState.VF, MState.CF, MState.HF)) ~~> { code => List(MLine.inherentA(INC)) },
    (Elidable & HasOpcode(SUBB) & HasImmediate(1) & DoesntMatterWhatItDoesWith(MState.VF, MState.CF, MState.HF)) ~~> { code => List(MLine.inherentB(DEC)) },
    (Elidable & HasOpcode(SUBA) & HasImmediate(1) & DoesntMatterWhatItDoesWith(MState.VF, MState.CF, MState.HF)) ~~> { code => List(MLine.inherentA(DEC)) },

  )

  val SimplifiableAddressing = new RuleBasedAssemblyOptimization("Simplifiable addressing",
    needsFlowInfo = FlowInfoRequirement.ForwardFlow,
    (NotFixed & MatchX(0) & HasAddrMode(Indexed(M6809Register.X, indirect = false)) & Not(HasOpcodeIn(LEAX, LEAY, LEAS, LEAU))) ~~> { (code, ctx) =>
      val x = ctx.get[Constant](0)
      code.map(l => l.copy(addrMode = Absolute(indirect = false), parameter = (x + l.parameter).quickSimplify))
    },
    (NotFixed & MatchY(0) & HasAddrMode(Indexed(M6809Register.Y, indirect = false)) & Not(HasOpcodeIn(LEAX, LEAY, LEAS, LEAU))) ~~> { (code, ctx) =>
      val y = ctx.get[Constant](0)
      code.map(l => l.copy(addrMode = Absolute(indirect = false), parameter = (y + l.parameter).quickSimplify))
    },
    (NotFixed & MatchX(0) & HasAddrMode(Indexed(M6809Register.X, indirect = false)) & HasOpcodeIn(LEAX, LEAY, LEAS, LEAU)) ~~> { (code, ctx) =>
      val x = ctx.get[Constant](0)
      code.map(l => l.copy(opcode = l.opcode match {
        case LEAX => LDX
        case LEAY => LDY
        case LEAU => LDU
        case LEAS => LDS
      }, addrMode = Immediate, parameter = (x + l.parameter).quickSimplify))
    },
    (NotFixed & MatchY(0) & HasAddrMode(Indexed(M6809Register.X, indirect = false)) & HasOpcodeIn(LEAX, LEAY, LEAS, LEAU)) ~~> { (code, ctx) =>
      val y = ctx.get[Constant](0)
      code.map(l => l.copy(opcode = l.opcode match {
        case LEAX => LDX
        case LEAY => LDY
        case LEAU => LDU
        case LEAS => LDS
      }, addrMode = Immediate, parameter = (y + l.parameter).quickSimplify))
    },
  )

  val UnusedLabelRemoval = new RuleBasedAssemblyOptimization("Unused label removal",
    needsFlowInfo = FlowInfoRequirement.JustLabels,
    (Elidable & HasOpcode(LABEL) & HasCallerCount(0) & ParameterIsLocalLabel) ~~> (_ => Nil)
  )


  val All: Seq[AssemblyOptimization[MLine]] = Seq(
    PointlessLoad,
    PointlessCompare,
    PointlessRegisterTransfers,
    SimplifiableAddressing,
    SimplifiableArithmetics,
    SimplifiableComparison,
    SimplifiableJumps,
    SimplifiableZeroStore,
    UnusedLabelRemoval
  )
}
