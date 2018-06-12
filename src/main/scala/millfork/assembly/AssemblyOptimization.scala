package millfork.assembly

import millfork.CompilationOptions
import millfork.env.NormalFunction

/**
  * @author Karol Stasiak
  */
trait AssemblyOptimization[T <: AbstractCode] {
  def name: String

  def optimize(f: NormalFunction, code: List[T], options: CompilationOptions): List[T]
}
