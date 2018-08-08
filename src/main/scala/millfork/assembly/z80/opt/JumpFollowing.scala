package millfork.assembly.z80.opt

import millfork.assembly.z80.ZOpcode._
import millfork.assembly.z80._
import millfork.env.{Constant, Label, MemoryAddressConstant}
import millfork.{CompilationOptions}

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
object JumpFollowing {

  def apply(options: CompilationOptions, code: List[ZLine]): List[ZLine] = {
    val labelsToRet = mutable.Set[String]()
    val labelsToJumps = mutable.Map[String, String]()
    val currentLabels = mutable.Set[String]()
    for (line <- code) {
      line match {
        case ZLine(LABEL, _, MemoryAddressConstant(Label(label)), _) =>
          currentLabels += label
        case ZLine(op, _, _, _) if ZOpcodeClasses.NoopDiscards(op) =>
        case ZLine(LABEL, _, _, _) =>
        case ZLine(RET, NoRegisters, _, _) =>
          labelsToRet ++= currentLabels
          currentLabels.clear()
        case ZLine(JP | JR, NoRegisters, MemoryAddressConstant(Label(label)), _) =>
          labelsToJumps ++= currentLabels.map(_ -> label)
          currentLabels.clear()
        case _ =>
          currentLabels.clear()
      }
    }
    code.map {
      case jump@ZLine(JR | JP, cond, MemoryAddressConstant(Label(label)), true) =>
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
