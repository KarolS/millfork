package millfork.compiler

import millfork.assembly.AbstractCode

/**
  * @author Karol Stasiak
  */
abstract class AbstractCompiler[T <: AbstractCode] {
  def compile(ctx: CompilationContext): List[T]
  def packHalves(tuple: (List[T], List[T])): List[T] = tuple._1 ++ tuple._2
}
