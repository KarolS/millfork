package millfork.assembly.mos.opt

import millfork.CompilationOptions
import millfork.assembly.mos.AssemblyLine
import millfork.assembly.mos.Opcode._
import millfork.assembly.AssemblyOptimization
import millfork.env._
import millfork.error.ErrorReporting

/**
  * @author Karol Stasiak
  */
object UnusedLabelRemoval extends AssemblyOptimization[AssemblyLine] {

  override def optimize(f: NormalFunction, code: List[AssemblyLine], options: CompilationOptions): List[AssemblyLine] = {
    val usedLabels = code.flatMap {
      case AssemblyLine(LABEL, _, _, _) => None
      case AssemblyLine(_, _, MemoryAddressConstant(Label(l)), _) => Some(l)
      case _ => None
    }.toSet
    val definedLabels = code.flatMap {
      case AssemblyLine(LABEL, _, MemoryAddressConstant(Label(l)), _) => Some(l).filter(_.startsWith("."))
      case _ => None
    }.toSet
    val toRemove = definedLabels -- usedLabels
    if (toRemove.nonEmpty) {
      ErrorReporting.debug("Removing labels: " + toRemove.mkString(", "))
      code.filterNot {
        case AssemblyLine(LABEL, _, MemoryAddressConstant(Label(l)), _) => toRemove(l)
        case _ => false
      }
    } else {
      code
    }
  }

  override def name = "Unused label removal"
}
