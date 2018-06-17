package millfork.compiler

import millfork.assembly.AbstractCode

/**
  * @author Karol Stasiak
  */
abstract class AbstractCompiler[T <: AbstractCode] {
  def nextLabel(prefix: String): String
  def compile(ctx: CompilationContext): List[T]
}
