package millfork.assembly.z80.opt

import millfork.assembly.AssemblyOptimization
import millfork.assembly.z80.{ZLine, ZOpcode}
import millfork.node.ZRegister
import ZOpcode._
import ZRegister._

/**
  * @author Karol Stasiak
  */
object LaterIntel8080Optimizations {
  val UseExDeHl = new RuleBasedAssemblyOptimization("Use EX DE,HL",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,

    (Elidable & Is8BitLoad(D, H)) ~
      (Elidable & Is8BitLoad(E, L) & DoesntMatterWhatItDoesWith(HL)) ~~> { _ =>
      List(ZLine.implied(EX_DE_HL))
    },

    (Elidable & Is8BitLoad(H, D)) ~
      (Elidable & Is8BitLoad(L, E) & DoesntMatterWhatItDoesWith(DE)) ~~> { _ =>
      List(ZLine.implied(EX_DE_HL))
    },
  )

  val All: List[AssemblyOptimization[ZLine]] = List(
    UseExDeHl
  )

}
