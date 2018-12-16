package millfork.assembly.mos.opt

import millfork.assembly.AssemblyOptimization
import millfork.assembly.mos.{AddrMode, AssemblyLine, Opcode, State}
import millfork.assembly.mos.Opcode._
import millfork.assembly.mos.AddrMode._
import millfork.assembly.mos.OpcodeClasses._
import millfork.env._

/**
  * @author Karol Stasiak
  */
object CmosOptimizations {

  val StzAddrModes = Set(ZeroPage, ZeroPageX, Absolute, AbsoluteX)

  val ZeroStoreAsStz = new RuleBasedAssemblyOptimization("Zero store",
    needsFlowInfo = FlowInfoRequirement.ForwardFlow,
    (HasA(0) & HasZ(0) & HasOpcode(STA) & NotFixed & HasAddrModeIn(StzAddrModes)) ~~> {code =>
      code.head.copy(opcode = STZ) :: Nil
    },
    (HasX(0) & HasZ(0) & HasOpcode(STX) & NotFixed & HasAddrModeIn(StzAddrModes)) ~~> {code =>
      code.head.copy(opcode = STZ) :: Nil
    },
    (HasY(0) & HasZ(0) & HasOpcode(STY) & NotFixed & HasAddrModeIn(StzAddrModes)) ~~> {code =>
      code.head.copy(opcode = STZ) :: Nil
    },
  )

  val SimplerBitFlipping = new RuleBasedAssemblyOptimization("Simpler bit flipping",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    (Elidable & HasOpcode(LDA) & HasAddrModeIn(Set(Absolute, ZeroPage)) & MatchParameter(0)) ~
      (Elidable & HasOpcode(ORA) & MatchImmediate(1)) ~
      (Elidable & HasOpcode(STA) & HasAddrModeIn(Set(Absolute, ZeroPage)) & MatchParameter(0) & DoesntMatterWhatItDoesWith(State.A, State.Z, State.N)) ~~> { code =>
      List(code(1).copy(opcode = Opcode.LDA), code.head.copy(opcode = TSB))
    },
    (Elidable & HasOpcode(LDA) & HasAddrModeIn(Set(Absolute, ZeroPage)) & MatchParameter(0)) ~
      (Elidable & HasOpcode(AND) & MatchImmediate(1)) ~
      (Elidable & HasOpcode(STA) & HasAddrModeIn(Set(Absolute, ZeroPage)) & MatchParameter(0) & DoesntMatterWhatItDoesWith(State.A, State.Z, State.N)) ~~> { code =>
      List(
        code(1).copy(opcode = Opcode.LDA, parameter = CompoundConstant(MathOperator.Exor, NumericConstant(255, 1), code(1).parameter).quickSimplify),
        code.head.copy(opcode = TRB))
    },
  )

  val OptimizeZeroIndex = new RuleBasedAssemblyOptimization("Optimizing zero index",
    needsFlowInfo = FlowInfoRequirement.ForwardFlow,
    (Elidable & HasY(0) & HasZ(0) & HasAddrMode(IndexedY) & HasOpcodeIn(SupportsIndexedZ)) ~~> (code => code.map(_.copy(addrMode = IndexedZ))),
    (Elidable & HasX(0) & HasZ(0) & HasAddrMode(IndexedX) & HasOpcodeIn(SupportsIndexedZ)) ~~> (code => code.map(_.copy(addrMode = IndexedZ))),
    (Elidable & HasX(0) & HasZ(0) & HasAddrMode(AbsoluteIndexedX) & HasOpcode(JMP)) ~~> (code => code.map(_.copy(addrMode = Indirect))),
  )

  val OptimizeIncrement = new RuleBasedAssemblyOptimization("Optimizing increment/decrement",
    needsFlowInfo = FlowInfoRequirement.BothFlows,

    (Elidable & HasOpcode(CLC)).? ~
      (Elidable & HasOpcode(ADC) & HasImmediate(1) & HasClear(State.C) & HasClear(State.D) & DoesntMatterWhatItDoesWith(State.C, State.V)) ~~> (code => List(AssemblyLine.implied(INC))),

    (Elidable & HasOpcode(SEC)).? ~
      (Elidable & HasOpcode(SBC) & HasImmediate(1) & HasSet(State.C) & HasClear(State.D) & DoesntMatterWhatItDoesWith(State.C, State.V)) ~~> (code => List(AssemblyLine.implied(DEC))),

    (Elidable & HasClearBitA0 & HasOpcodeIn(Set(ORA, EOR)) & HasImmediate(1)) ~~> (code => List(AssemblyLine.implied(INC))),

    (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.Z, State.N, State.A)) ~
      (Linear & DoesNotConcernMemoryAt(0, 1) & DoesntChangeIndexingInAddrMode(0)).* ~
      (Elidable & HasOpcodeIn(Set(INC, DEC)) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~~> { code =>
      code.last.copy(addrMode = Implied, parameter = Constant.Zero) :: code.init
    },
  )

  val All: List[AssemblyOptimization[AssemblyLine]] = List(OptimizeZeroIndex, SimplerBitFlipping, ZeroStoreAsStz, OptimizeIncrement)
}
