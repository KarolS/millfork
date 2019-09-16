package millfork.output

import millfork.{CompilationOptions, JobContext}
import millfork.assembly.m6809.MLine
import millfork.env.ParamSignature

/**
  * @author Karol Stasiak
  */
object M6809InliningCalculator extends AbstractInliningCalculator[MLine] {
  override def codeForInlining(fname: String, functionsThatCanBeCalledFromInlinedFunctions: Set[String], code: List[MLine]): Option[List[MLine]] = None

  override def calculateExpectedSizeAfterInlining(options: CompilationOptions, params: ParamSignature, code: List[MLine]): Int = {
    var sum = 0
    for (c <- code) {
      sum += c.sizeInBytes
    }
    sum
  }

  override def inline(code: List[MLine], inlinedFunctions: Map[String, List[MLine]], jobContext: JobContext): List[MLine] = {
    if (inlinedFunctions.isEmpty) code
    else ???
  }
}
