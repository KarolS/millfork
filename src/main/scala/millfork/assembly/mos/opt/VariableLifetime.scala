package millfork.assembly.mos.opt

import millfork.assembly.mos.{AssemblyLine, OpcodeClasses}
import millfork.env._
import millfork.error.ConsoleLogger

/**
  * @author Karol Stasiak
  */
object VariableLifetime {

  // This only works for non-stack variables.
  def apply(variableName: String, code: List[AssemblyLine], expandToIncludeIndexing: Boolean = false): Range = {
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

    if (expandToIncludeIndexing) {
      import millfork.assembly.mos.Opcode._
      import millfork.assembly.mos.AddrMode._
      val linearChuckAfter = code.drop(max).takeWhile{ line => line.opcode match {
        case LABEL | JSR | BSR | RTS | RTI => false
        case op if OpcodeClasses.AllDirectJumps(op) => false
        case _ => true
      }}
      val lastIndexing = linearChuckAfter.lastIndexWhere(line => line.addrMode match {
        case IndexedY | IndexedX | IndexedSY => true
        case AbsoluteY | AbsoluteX | LongAbsoluteX => true
        case ZeroPageX | ZeroPageY => true
        case _ => false
      })
      if (lastIndexing >= 0) {
        max += lastIndexing + 1
      }
    }

//    val log = new ConsoleLogger
//    log.verbosity = 3
//    log.trace("Lifetime for " + variableName)
//    code.zipWithIndex.foreach {
//      case (line, index) =>
//        if (index >= min && index < max) {
//          log.trace(f"$line%-30s  <")
//        } else {
//          log.trace(line.toString)
//        }
//    }

    Range(min, max)
  }
}
