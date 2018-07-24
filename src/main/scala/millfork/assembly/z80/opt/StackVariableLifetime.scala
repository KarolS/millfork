package millfork.assembly.z80.opt

import millfork.assembly.opt.SingleStatus
import millfork.assembly.z80._
import millfork.env._
import millfork.node.ZRegister

/**
  * @author Karol Stasiak
  */
object StackVariableLifetime {

  // This only works for non-stack variables.
  // TODO: this is also probably very wrong
  def apply(variableOffset: Int, codeWithFlow: List[(FlowInfo, ZLine)]): Range = {
    val flags = codeWithFlow.map {
      case (_, ZLine(_, OneRegisterOffset(ZRegister.MEM_IX_D, i), _, _)) => i == variableOffset
      case (_, ZLine(_, TwoRegistersOffset(ZRegister.MEM_IX_D, _, i), _, _)) => i == variableOffset
      case (_, ZLine(_, TwoRegistersOffset(_, ZRegister.MEM_IX_D, i), _, _)) => i == variableOffset
      case _ => false
    }
    if (flags.forall(!_)) return Range(0, 0)
    var min = flags.indexOf(true)
    var max = flags.lastIndexOf(true) + 1
    var changed = true
    val labelMap = codeWithFlow.zipWithIndex.flatMap(a => a._1._2.parameter match {
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

//    ErrorReporting.trace("Lifetime for IX+" + variableOffset)
//    codeWithFlow.zipWithIndex.foreach {
//      case ((_, line), index) =>
//        if (index >= min && index < max) {
//          ErrorReporting.trace(f"$line%-30s  <")
//        } else {
//          ErrorReporting.trace(line.toString)
//        }
//    }

    Range(min, max)
  }
}
