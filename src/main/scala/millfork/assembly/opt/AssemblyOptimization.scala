package millfork.assembly.opt

import millfork.CompilationOptions
import millfork.assembly.AssemblyLine
import millfork.env.NormalFunction

/**
  * @author Karol Stasiak
  */
trait AssemblyOptimization {
  def name: String

  def optimize(f: NormalFunction, code: List[AssemblyLine], options: CompilationOptions): List[AssemblyLine]
}
