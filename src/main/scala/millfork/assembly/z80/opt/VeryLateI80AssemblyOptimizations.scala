package millfork.assembly.z80.opt

import millfork.CompilationOptions
import millfork.assembly.AssemblyOptimization
import millfork.assembly.z80.ZLine
import millfork.node.NiceFunctionProperty

/**
  * @author Karol Stasiak
  */
object VeryLateI80AssemblyOptimizations {

  def All(nice: Set[NiceFunctionProperty], options: CompilationOptions): Seq[AssemblyOptimization[ZLine]] = {
    val result = Seq.newBuilder[AssemblyOptimization[ZLine]]
    // TODO: add stuff here
    result.result()
  }

}
