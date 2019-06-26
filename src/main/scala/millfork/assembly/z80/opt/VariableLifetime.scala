package millfork.assembly.z80.opt

import millfork.assembly.opt.SingleStatus
import millfork.assembly.z80.{OneRegister, TwoRegisters, ZLine, ZLine0, ZOpcode}
import millfork.env._
import millfork.error.ConsoleLogger
import millfork.node.ZRegister

/**
  * @author Karol Stasiak
  */
object VariableLifetime {

  // This only works for non-stack variables.
  // TODO: this is also probably very wrong
  def apply(variableName: String, codeWithFlow: List[(FlowInfo, ZLine)]): Range = {
    import ZRegister._
    import ZOpcode._

    val pointerLoadedAt = codeWithFlow.zipWithIndex.filter{
      case ((_, ZLine0(ZOpcode.LD_16, TwoRegisters(_, IMM_16), MemoryAddressConstant(MemoryVariable(n, _, _)))), _) => n == variableName
      case _ => false
    }.map(_._2)
    val pointerReadAt = codeWithFlow.zipWithIndex.filter{
      case ((_, ZLine0(_, TwoRegisters(MEM_HL | MEM_DE | MEM_BC, _), _)), _) => true
      case ((_, ZLine0(_, TwoRegisters(_, MEM_HL | MEM_DE | MEM_BC), _)), _) => true
      case ((_, ZLine0(_, OneRegister(MEM_HL | MEM_DE | MEM_BC), _)), _) => true
      case ((_, ZLine0(CALL, _, _)), _) => true
      case _ => false
    }.map(_._2)


    val flags = codeWithFlow.map {
      case (_, ZLine0(_, _, MemoryAddressConstant(MemoryVariable(n, _, _)))) => n == variableName
      case (_, ZLine0(_, _, CompoundConstant(MathOperator.Plus, MemoryAddressConstant(MemoryVariable(n, _, _)), NumericConstant(_, 1)))) => n == variableName
      case (i, ZLine0(_, TwoRegisters(MEM_HL, _) | TwoRegisters(_, MEM_HL) | OneRegister(MEM_HL), _)) =>
        i.statusBefore.hl match {
          case SingleStatus(MemoryAddressConstant(MemoryVariable(n, _, _))) => n == variableName
          case SingleStatus(CompoundConstant(MathOperator.Plus, MemoryAddressConstant(MemoryVariable(n, _, _)), NumericConstant(_, 1))) => n == variableName
          case _ => false
        }
      case _ => false
    }.toArray

    if(pointerLoadedAt.nonEmpty) {
      pointerReadAt.foreach(i => flags(i) = true)
    }

    val code = codeWithFlow.map(_._2)
    expandRangeToCoverLoops(code, flags)
  }

  def expandRangeToCoverLoops(code: List[ZLine], flags: Array[Boolean]): Range = {
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

//    val log = new ConsoleLogger
//    log.verbosity = 3
//    log.trace("Lifetime for " + variableName)
//    codeWithFlow.zipWithIndex.foreach {
//      case ((_, line), index) =>
//        if (index >= min && index < max) {
//          log.trace(f"$line%-30s  <")
//        } else {
//          log.trace(line.toString)
//        }
//    }

    Range(min, max)
  }
}
