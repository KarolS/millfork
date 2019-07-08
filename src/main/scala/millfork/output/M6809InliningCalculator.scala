package millfork.output

import millfork.JobContext
import millfork.assembly.m6809.MLine

/**
  * @author Karol Stasiak
  */
object M6809InliningCalculator extends AbstractInliningCalculator[MLine] {
  override def codeForInlining(fname: String, functionsThatCanBeCalledFromInlinedFunctions: Set[String], code: List[MLine]): Option[List[MLine]] = None

  override def inline(code: List[MLine], inlinedFunctions: Map[String, List[MLine]], jobContext: JobContext): List[MLine] = {
    if (inlinedFunctions.isEmpty) code
    else ???
  }
}
