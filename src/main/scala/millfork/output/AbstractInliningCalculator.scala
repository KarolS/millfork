package millfork.output

import millfork.assembly.AbstractCode
import millfork.compiler.AbstractCompiler

/**
  * @author Karol Stasiak
  */
abstract class AbstractInliningCalculator[T <: AbstractCode] {
  def codeForInlining(fname: String, functionsAlreadyKnownToBeNonInlineable: Set[String], code: List[T]): Option[List[T]]
  def inline(code: List[T], inlinedFunctions: Map[String, List[T]], compiler: AbstractCompiler[T]): List[T]
}
