package millfork.assembly.mos.opt

import millfork.assembly.mos.{AssemblyLine, AssemblyLine0}
import millfork.assembly.mos.Opcode._
import millfork.assembly.{AssemblyOptimization, OptimizationContext}
import millfork.env._
import millfork.error.ConsoleLogger

/**
  * @author Karol Stasiak
  */
object UnusedLabelRemoval extends AssemblyOptimization[AssemblyLine] {

  override def optimize(f: NormalFunction, code: List[AssemblyLine], optimizationContext: OptimizationContext): List[AssemblyLine] = {
    val usedLabels = code.flatMap {
      case AssemblyLine0(LABEL, _, _) => None
      case AssemblyLine0(_, _, MemoryAddressConstant(Label(l))) => Some(l)
      case AssemblyLine0(_, _, StructureConstant(_, List(_, MemoryAddressConstant(Label(l))))) => Some(l)
      case _ => None
    }.toSet
    val definedLabels = code.flatMap {
      case AssemblyLine0(LABEL, _, MemoryAddressConstant(Label(l))) => Some(l).filter(_.startsWith("."))
      case _ => None
    }.toSet
    val toRemove = definedLabels -- usedLabels
    if (toRemove.nonEmpty) {
      optimizationContext.log.debug("Removing labels: " + toRemove.mkString(", "))
      code.filterNot {
        case AssemblyLine0(LABEL, _, MemoryAddressConstant(Label(l))) => toRemove(l)
        case _ => false
      }
    } else {
      code
    }
  }

  override def name = "Unused label removal"
}
