package millfork.assembly.mos.opt

import millfork.assembly.mos.AssemblyLine
import millfork.env._
import millfork.error.ErrorReporting

/**
  * @author Karol Stasiak
  */
object VariableLifetime {

  // This only works for non-stack variables.
  def apply(variableName: String, code: List[AssemblyLine]): Range = {
    val flags = code.map(_.parameter match {
      case MemoryAddressConstant(MemoryVariable(n, _, _)) if n == variableName => true
      case CompoundConstant(MathOperator.Plus, MemoryAddressConstant(MemoryVariable(n, _, _)), NumericConstant(_, 1)) if n == variableName => true
      case _ => false
    })
    if (flags.forall(!_)) return Range(0, 0)
    var min = flags.indexOf(true)
    var max = flags.lastIndexOf(true) + 1
    var changed = true
    val labelMap = code.zipWithIndex.flatMap(a => a._1.parameter match {
      case MemoryAddressConstant(Label(l)) => List(l -> a._2)
      case _ => Nil
    }).groupBy(_._1).mapValues(_.map(_._2).toSet)

    while (changed) {
      changed = false
      for ((label, indices) <- labelMap) {
        if (indices.exists(i => i >= min && i < max)) {
          indices.foreach { i =>
            val before = max - min
            min = min min i
            max = max max (i + 1)
            if (max - min != before) {
              changed = true
            }
          }
        }
      }
    }

//    ErrorReporting.trace("Lifetime for " + variableName)
//    code.zipWithIndex.foreach {
//      case (line, index) =>
//        if (index >= min && index < max) {
//          ErrorReporting.trace(f"$line%-30s  <")
//        } else {
//          ErrorReporting.trace(line.toString)
//        }
//    }

    Range(min, max)
  }
}
