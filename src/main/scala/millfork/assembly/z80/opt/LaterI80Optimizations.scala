package millfork.assembly.z80.opt

import millfork.assembly.AssemblyOptimization
import millfork.assembly.z80.{ZLine, ZOpcode}
import millfork.node.ZRegister
import ZOpcode._
import ZRegister._


/**
  * @author Karol Stasiak
  */
object LaterI80Optimizations {
  val VariousSmallOptimizations = new RuleBasedAssemblyOptimization("Various small optimizations",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,

    (Elidable & Is8BitLoadTo(A) & Has8BitImmediate(0) & DoesntMatterWhatItDoesWithFlags) ~~> { _ =>
      List(ZLine.register(XOR, A))
    },
    (Elidable & HasOpcode(CP) & DoesntMatterWhatItDoesWithFlags) ~~> { _ =>
      Nil
    },
    (Elidable & HasOpcode(CP) & Has8BitImmediate(0) & DoesntMatterWhatItDoesWithFlagsOtherThanSZ) ~~> { _ =>
      List(ZLine.register(OR, A))
    },
  )

  val All: List[AssemblyOptimization[ZLine]] = List(
    VariousSmallOptimizations
  )
}
