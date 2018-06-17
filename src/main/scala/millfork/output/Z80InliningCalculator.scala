package millfork.output

import millfork.assembly.z80.ZLine
import millfork.compiler.AbstractCompiler

/**
  * @author Karol Stasiak
  */
object Z80InliningCalculator extends AbstractInliningCalculator[ZLine] {

  // TODO

  override def codeForInlining(fname: String, functionsAlreadyKnownToBeNonInlineable: Set[String], code: List[ZLine]): Option[List[ZLine]] = None

  override def inline(code: List[ZLine], inlinedFunctions: Map[String, List[ZLine]], compiler: AbstractCompiler[ZLine]): List[ZLine] = code
}
