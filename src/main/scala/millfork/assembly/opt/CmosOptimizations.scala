package millfork.assembly.opt

import millfork.assembly.{AssemblyLine, Opcode}
import millfork.assembly.Opcode._
import millfork.assembly.AddrMode._
import millfork.assembly.OpcodeClasses._
import millfork.env.{Constant, NormalFunction}

/**
  * @author Karol Stasiak
  */
object CmosOptimizations {

  val StzAddrModes = Set(ZeroPage, ZeroPageX, Absolute, AbsoluteX)

  val ZeroStoreAsStz = new RuleBasedAssemblyOptimization("Zero store",
    needsFlowInfo = FlowInfoRequirement.ForwardFlow,
    (HasA(0) & HasOpcode(STA) & Elidable & HasAddrModeIn(StzAddrModes)) ~~> {code =>
      code.head.copy(opcode = STZ) :: Nil
    },
    (HasX(0) & HasOpcode(STX) & Elidable & HasAddrModeIn(StzAddrModes)) ~~> {code =>
      code.head.copy(opcode = STZ) :: Nil
    },
    (HasY(0) & HasOpcode(STY) & Elidable & HasAddrModeIn(StzAddrModes)) ~~> {code =>
      code.head.copy(opcode = STZ) :: Nil
    },
  )

  val OptimizeZeroIndex = new RuleBasedAssemblyOptimization("Optimizing zero index",
    needsFlowInfo = FlowInfoRequirement.ForwardFlow,
    (Elidable & HasY(0) & HasAddrMode(IndexedY) & HasOpcodeIn(SupportsZeroPageIndirect)) ~~> (code => code.map(_.copy(addrMode = ZeroPageIndirect))),
    (Elidable & HasX(0) & HasAddrMode(IndexedX) & HasOpcodeIn(SupportsZeroPageIndirect)) ~~> (code => code.map(_.copy(addrMode = ZeroPageIndirect))),
  )

  val All: List[AssemblyOptimization] = List(ZeroStoreAsStz)
}
