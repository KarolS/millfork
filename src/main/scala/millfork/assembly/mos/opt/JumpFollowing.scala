package millfork.assembly.mos.opt

import millfork.CompilationOptions
import millfork.assembly.Elidability
import millfork.assembly.mos.{AssemblyLine, AssemblyLine0, OpcodeClasses}
import millfork.env.{Label, MemoryAddressConstant}
import millfork.assembly.mos.Opcode._
import millfork.assembly.mos.AddrMode._

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
object JumpFollowing {

  def apply(options: CompilationOptions, code: List[AssemblyLine]): List[AssemblyLine] = {
    val labelsToRts = new mutable.HashSet[String]()
    val labelsToRti = new mutable.HashSet[String]()
    val labelsToRtl = new mutable.HashSet[String]()
    val labelsToJumps = new mutable.HashMap[String, String]()
    val currentLabels = new mutable.HashSet[String]()
    for (line <- code) {
      line match {
        case AssemblyLine0(LABEL, _, MemoryAddressConstant(Label(label))) =>
          currentLabels += label
        case AssemblyLine0(op, _, _) if OpcodeClasses.NoopDiscardsFlags(op) =>
        case AssemblyLine0(LABEL, _, _) =>
        case AssemblyLine0(RTS, _, _) =>
          labelsToRts ++= currentLabels
          currentLabels.clear()
        case AssemblyLine0(RTI, _, _) =>
          labelsToRti ++= currentLabels
          currentLabels.clear()
        case AssemblyLine0(RTL, _, _) =>
          labelsToRtl ++= currentLabels
          currentLabels.clear()
        case AssemblyLine0(JMP | BRA, Absolute | Relative, MemoryAddressConstant(Label(label))) =>
          labelsToJumps ++= currentLabels.map(_ -> label)
          currentLabels.clear()
        case _ =>
          currentLabels.clear()
      }
    }
    code.map {
      case jump@AssemblyLine(JMP | BRA, Absolute | Relative | LongRelative | LongAbsolute, MemoryAddressConstant(Label(label)), Elidability.Elidable, s) =>
        if (labelsToRts(label)) {
          options.log.debug(s"Optimizing ${jump.opcode} straight into RTS")
          AssemblyLine.implied(RTS).pos(s)
        } else if (labelsToRti(label)) {
          options.log.debug(s"Optimizing ${jump.opcode} straight into RTI")
          AssemblyLine.implied(RTI).pos(s)
        } else if (labelsToRtl(label)) {
          options.log.debug(s"Optimizing ${jump.opcode} straight into RTL")
          AssemblyLine.implied(RTL).pos(s)
        } else if (labelsToJumps.contains(label)) {
          options.log.debug(s"Optimizing ${jump.opcode} straight into a jump")
          AssemblyLine.absolute(JMP, Label(labelsToJumps(label))).pos(s)
        } else jump
      case x => x
    }
  }
}
