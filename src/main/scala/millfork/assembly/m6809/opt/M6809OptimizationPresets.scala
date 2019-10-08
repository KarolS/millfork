package millfork.assembly.m6809.opt

import millfork.assembly.AssemblyOptimization
import millfork.assembly.m6809.MLine

/**
  * @author Karol Stasiak
  */
object M6809OptimizationPresets {

  val Default: List[AssemblyOptimization[MLine]] = forLevel(2)

  def forLevel(level: Int): List[AssemblyOptimization[MLine]] = List.fill(level)(AlwaysGoodMOptimizations.All).flatten
}
