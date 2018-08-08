package millfork.assembly.mos.opt

import millfork.CompilationOptions
import millfork.assembly.mos.{AssemblyLine, OpcodeClasses}
import millfork.env.{Label, MemoryAddressConstant}
import millfork.assembly.mos.Opcode._
import millfork.assembly.mos.AddrMode._

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
object JumpFollowing {

  def apply(options: CompilationOptions, code: List[AssemblyLine]): List[AssemblyLine] = {
    val labelsToRts = mutable.Set[String]()
    val labelsToRti = mutable.Set[String]()
    val labelsToRtl = mutable.Set[String]()
    val labelsToJumps = mutable.Map[String, String]()
    val currentLabels = mutable.Set[String]()
    for (line <- code) {
      line match {
        case AssemblyLine(LABEL, _, MemoryAddressConstant(Label(label)), _) =>
          currentLabels += label
        case AssemblyLine(op, _, _, _) if OpcodeClasses.NoopDiscardsFlags(op) =>
        case AssemblyLine(LABEL, _, _, _) =>
        case AssemblyLine(RTS, _, _, _) =>
          labelsToRts ++= currentLabels
          currentLabels.clear()
        case AssemblyLine(RTI, _, _, _) =>
          labelsToRti ++= currentLabels
          currentLabels.clear()
        case AssemblyLine(RTL, _, _, _) =>
          labelsToRtl ++= currentLabels
          currentLabels.clear()
        case AssemblyLine(JMP | BRA, Absolute | Relative, MemoryAddressConstant(Label(label)), _) =>
          labelsToJumps ++= currentLabels.map(_ -> label)
          currentLabels.clear()
        case _ =>
          currentLabels.clear()
      }
    }
    code.map {
      case jump@AssemblyLine(JMP | BRA, Absolute | Relative | LongRelative | LongAbsolute, MemoryAddressConstant(Label(label)), true) =>
        if (labelsToRts(label)) {
          options.log.debug(s"Optimizing ${jump.opcode} straight into RTS")
          AssemblyLine.implied(RTS)
        } else if (labelsToRti(label)) {
          options.log.debug(s"Optimizing ${jump.opcode} straight into RTI")
          AssemblyLine.implied(RTI)
        } else if (labelsToRtl(label)) {
          options.log.debug(s"Optimizing ${jump.opcode} straight into RTL")
          AssemblyLine.implied(RTL)
        } else if (labelsToJumps.contains(label)) {
          options.log.debug(s"Optimizing ${jump.opcode} straight into a jump")
          AssemblyLine.absolute(JMP, Label(labelsToJumps(label)))
        } else jump
      case x => x
    }
  }
}
