package millfork.assembly.z80.opt

import millfork.assembly.z80.ZOpcode._
import millfork.assembly.z80._
import millfork.env.{Constant, Label, MemoryAddressConstant}
import millfork.CompilationOptions
import millfork.assembly.Elidability

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
object JumpFollowing {

  def apply(options: CompilationOptions, code: List[ZLine]): List[ZLine] = {
    val labelsToRet = new mutable.HashSet[String]()
    val labelsToJumps = new mutable.HashMap[String, String]()
    val currentLabels = new mutable.HashSet[String]()
    for (line <- code) {
      line match {
        case ZLine0(LABEL, _, MemoryAddressConstant(Label(label))) =>
          currentLabels += label
        case ZLine0(op, _, _) if ZOpcodeClasses.NoopDiscards(op) =>
        case ZLine0(LABEL, _, _) =>
        case ZLine0(RET, NoRegisters, _) =>
          labelsToRet ++= currentLabels
          currentLabels.clear()
        case ZLine0(JP | JR, NoRegisters, MemoryAddressConstant(Label(label))) =>
          labelsToJumps ++= currentLabels.map(_ -> label)
          currentLabels.clear()
        case _ =>
          currentLabels.clear()
      }
    }
    code.map {
      case jump@ZLine(JR | JP, cond, MemoryAddressConstant(Label(label)), Elidability.Elidable, _) =>
        if (labelsToRet(label)) {
          options.log.debug(s"Optimizing ${jump.opcode} straight into RET")
          ZLine(RET, cond, Constant.Zero)
        } else if (labelsToJumps.contains(label)) {
          options.log.debug(s"Optimizing ${jump.opcode} straight into a jump")
          ZLine.jump(labelsToJumps(label))
        } else jump
      case x => x
    }
  }
}
