package millfork.assembly.z80.opt

import millfork.assembly.AssemblyOptimization
import millfork.assembly.z80.ZLine
import millfork.assembly.z80.ZOpcode.MULUB
import millfork.assembly.z80.ZOpcode.MULUW
import millfork.node.ZRegister

/**
  * Optimizations valid for R800
  * @author Karol Stasiak
  */
object AlwaysGoodR800Optimizations {

  val UnusedR800Instructions = new RuleBasedAssemblyOptimization("Simplifiable maths (R800)",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    (Elidable & HasOpcode(MULUB) & DoesntMatterWhatItDoesWith(ZRegister.H, ZRegister.L) & DoesntMatterWhatItDoesWithFlags) ~~> (_ => Nil),
    (Elidable & HasOpcode(MULUW) & DoesntMatterWhatItDoesWith(ZRegister.H, ZRegister.L, ZRegister.D, ZRegister.E) & DoesntMatterWhatItDoesWithFlags) ~~> (_ => Nil),
  )

  val All: List[AssemblyOptimization[ZLine]] = List[AssemblyOptimization[ZLine]](
    UnusedR800Instructions,
  )
}
