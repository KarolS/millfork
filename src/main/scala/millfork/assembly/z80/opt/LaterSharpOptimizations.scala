package millfork.assembly.z80.opt

import millfork.assembly.AssemblyOptimization
import millfork.assembly.z80.ZLine
import millfork.assembly.z80.ZOpcode._
import millfork.node.ZRegister._

/**
  * @author Karol Stasiak
  */
object LaterSharpOptimizations {
  val UseSwap = new RuleBasedAssemblyOptimization("Use SWAP",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,

    (Elidable & HasOpcode(RRA)) ~
      (Elidable & HasOpcode(RRA)) ~
      (Elidable & HasOpcode(RRA)) ~
      (Elidable & HasOpcode(RRA)) ~
      (HasOpcode(AND) & Has8BitImmediate(0xf)) ~~> { code =>
      List(ZLine.register(SWAP, A), code.last)
    },

    (Elidable & HasOpcode(RR) & HasRegisterParam(A)) ~
      (Elidable & HasOpcode(RR) & HasRegisterParam(A)) ~
      (Elidable & HasOpcode(RR) & HasRegisterParam(A)) ~
      (Elidable & HasOpcode(RR) & HasRegisterParam(A)) ~
      (HasOpcode(AND) & Has8BitImmediate(0xf)) ~~> { code =>
      List(ZLine.register(SWAP, A), code.last)
    },

    (Elidable & HasOpcode(RRCA)) ~
      (Elidable & HasOpcode(RRCA)) ~
      (Elidable & HasOpcode(RRCA)) ~
      (Elidable & HasOpcode(RRCA)) ~
      (HasOpcode(AND) & Has8BitImmediate(0xf)) ~~> { code =>
      List(ZLine.register(SWAP, A), code.last)
    },

    (Elidable & HasOpcode(RRC) & HasRegisterParam(A)) ~
      (Elidable & HasOpcode(RRC) & HasRegisterParam(A)) ~
      (Elidable & HasOpcode(RRC) & HasRegisterParam(A)) ~
      (Elidable & HasOpcode(RRC) & HasRegisterParam(A)) ~
      (HasOpcode(AND) & Has8BitImmediate(0xf)) ~~> { code =>
      List(ZLine.register(SWAP, A), code.last)
    },

    (Elidable & HasOpcode(SRL) & HasRegisterParam(A)) ~
      (Elidable & HasOpcode(SRL) & HasRegisterParam(A)) ~
      (Elidable & HasOpcode(SRL) & HasRegisterParam(A)) ~
      (Elidable & HasOpcode(SRL) & HasRegisterParam(A)) ~
      (HasOpcode(AND) & Has8BitImmediate(0xf)) ~~> { code =>
      List(ZLine.register(SWAP, A), code.last)
    },
  )

  val UseAutoincrementOperations = new RuleBasedAssemblyOptimization("Use autoincrement operations",
     needsFlowInfo = FlowInfoRequirement.BackwardFlow,

    (Elidable & Is8BitLoad(MEM_HL, A)) ~
      (Linear & Not(Concerns(HL))).* ~
      (Elidable & HasOpcode(INC_16) & HasRegisterParam(HL)) ~~> { code =>
      ZLine.implied(LD_HLIA) :: code.tail.init
    },
    (Elidable & Is8BitLoad(A, MEM_HL)) ~
      (Linear & Not(Concerns(HL))).* ~
      (Elidable & HasOpcode(INC_16) & HasRegisterParam(HL)) ~~> { code =>
      ZLine.implied(LD_AHLI) :: code.tail.init
    },
    (Elidable & Is8BitLoad(MEM_HL, A)) ~
      (Linear & Not(Concerns(HL))).* ~
      (Elidable & HasOpcode(DEC_16) & HasRegisterParam(HL)) ~~> { code =>
      ZLine.implied(LD_HLDA) :: code.tail.init
    },
    (Elidable & Is8BitLoad(A, MEM_HL)) ~
      (Linear & Not(Concerns(HL))).* ~
      (Elidable & HasOpcode(DEC_16) & HasRegisterParam(HL)) ~~> { code =>
      ZLine.implied(LD_AHLD) :: code.tail.init
    },

  )

  val All: List[AssemblyOptimization[ZLine]] = List(
    UseSwap, UseAutoincrementOperations
  )
}
