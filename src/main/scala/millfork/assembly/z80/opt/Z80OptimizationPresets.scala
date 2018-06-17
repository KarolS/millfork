package millfork.assembly.z80.opt

import millfork.assembly.AssemblyOptimization
import millfork.assembly.z80.ZLine

/**
  * @author Karol Stasiak
  */
object Z80OptimizationPresets {
  val Good: List[AssemblyOptimization[ZLine]] = List.tabulate(15)(_ => AlwaysGoodZ80Optimizations.All).flatten

}
