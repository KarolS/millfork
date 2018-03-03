package millfork.assembly.opt

import millfork.assembly.{AssemblyLine, Opcode, State}
import millfork.assembly.Opcode._
import millfork.assembly.AddrMode._
import millfork.assembly.OpcodeClasses._
import millfork.env._

/**
  * @author Karol Stasiak
  */
object CmosOptimizations {

  val StzAddrModes = Set(ZeroPage, ZeroPageX, Absolute, AbsoluteX)

  val ZeroStoreAsStz = new RuleBasedAssemblyOptimization("Zero store",
    needsFlowInfo = FlowInfoRequirement.ForwardFlow,
    (HasA(0) & HasZ(0) & HasOpcode(STA) & Elidable & HasAddrModeIn(StzAddrModes)) ~~> {code =>
      code.head.copy(opcode = STZ) :: Nil
    },
    (HasX(0) & HasZ(0) & HasOpcode(STX) & Elidable & HasAddrModeIn(StzAddrModes)) ~~> {code =>
      code.head.copy(opcode = STZ) :: Nil
    },
    (HasY(0) & HasZ(0) & HasOpcode(STY) & Elidable & HasAddrModeIn(StzAddrModes)) ~~> {code =>
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
        code(1).copy(opcode = Opcode.LDA, parameter = CompoundConstant(MathOperator.Exor, NumericConstant(255, 1), code(1).parameter)),
        code.head.copy(opcode = TRB))
    },
  )

  val OptimizeZeroIndex = new RuleBasedAssemblyOptimization("Optimizing zero index",
    needsFlowInfo = FlowInfoRequirement.ForwardFlow,
    (Elidable & HasY(0) & HasZ(0) & HasAddrMode(IndexedY) & HasOpcodeIn(SupportsIndexedZ)) ~~> (code => code.map(_.copy(addrMode = IndexedZ))),
    (Elidable & HasX(0) & HasZ(0) & HasAddrMode(IndexedX) & HasOpcodeIn(SupportsIndexedZ)) ~~> (code => code.map(_.copy(addrMode = IndexedZ))),
    (Elidable & HasX(0) & HasZ(0) & HasAddrMode(AbsoluteIndexedX) & HasOpcode(JMP)) ~~> (code => code.map(_.copy(addrMode = Indirect))),
  )

  val All: List[AssemblyOptimization] = List(OptimizeZeroIndex, SimplerBitFlipping, ZeroStoreAsStz)
}
