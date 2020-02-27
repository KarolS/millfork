package millfork.assembly.m6809.opt

import millfork.CompilationOptions
import millfork.assembly.AssemblyOptimization
import millfork.assembly.m6809.MLine
import millfork.node.NiceFunctionProperty

/**
  * @author Karol Stasiak
  */
object VeryLateM6809AssemblyOptimizations {

  def All(nice: Set[NiceFunctionProperty], options: CompilationOptions): Seq[AssemblyOptimization[MLine]] = {
    val result = Seq.newBuilder[AssemblyOptimization[MLine]]
    // TODO: add stuff here
    result.result()
  }
}
