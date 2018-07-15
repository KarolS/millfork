package millfork.assembly.z80.opt

import millfork.assembly.opt.SingleStatus
import millfork.assembly.z80.{OneRegister, TwoRegisters, ZLine}
import millfork.env._
import millfork.error.ErrorReporting
import millfork.node.ZRegister

/**
  * @author Karol Stasiak
  */
object VariableLifetime {

  // This only works for non-stack variables.
  // TODO: this is also probably very wrong
  def apply(variableName: String, codeWithFlow: List[(FlowInfo, ZLine)]): Range = {
    val flags = codeWithFlow.map {
      case (_, ZLine(_, _, MemoryAddressConstant(MemoryVariable(n, _, _)), _)) => n == variableName
      case (_, ZLine(_, _, CompoundConstant(MathOperator.Plus, MemoryAddressConstant(MemoryVariable(n, _, _)), NumericConstant(_, 1)), _)) => n == variableName
      case (i, ZLine(_, TwoRegisters(ZRegister.MEM_HL, _) | TwoRegisters(_, ZRegister.MEM_HL) | OneRegister(ZRegister.MEM_HL), _, _)) =>
        i.statusBefore.hl match {
          case SingleStatus(MemoryAddressConstant(MemoryVariable(n, _, _))) => n == variableName
          case SingleStatus(CompoundConstant(MathOperator.Plus, MemoryAddressConstant(MemoryVariable(n, _, _)), NumericConstant(_, 1))) => n == variableName
          case _ => false
        }
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

//    ErrorReporting.trace("Lifetime for " + variableName)
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
