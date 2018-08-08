package millfork.assembly.z80.opt

import millfork.assembly.z80.ZOpcode._
import millfork.assembly.z80._
import millfork.env.{Constant, Label, MemoryAddressConstant, NormalFunction}
import millfork.parser.Preprocessor.IfContext
import millfork.{CompilationFlag, CompilationOptions}

/**
  * @author Karol Stasiak
  */
object ConditionalInstructions {

  def apply(options: CompilationOptions, code: List[ZLine]): List[ZLine] = code match {
    case (jump@ZLine(JR | JP, IfFlagSet(_) | IfFlagClear(_), MemoryAddressConstant(Label(label)), true)) ::
      (call@ZLine(CALL, NoRegisters, _, _)) ::
      xs =>
      if (startsWithLabel(label, xs)) {
        val condCall = call.copy(registers = jump.registers.negate)
        options.log.debug(s"Replacing ${jump.toString.trim} ${call.toString.trim} with ${condCall.toString.trim}")
        condCall :: apply(options, xs)
      }else jump :: call :: apply(options, xs)
    case (jump@ZLine(JR | JP, IfFlagSet(_) | IfFlagClear(_), MemoryAddressConstant(Label(label)), true)) :: xs =>
      retPrecededByDiscards(xs) match {
        case Some(rest) if startsWithLabel(label, rest) =>
          val condRet = ZLine(RET, jump.registers.negate, Constant.Zero)
          options.log.debug(s"Replacing ${jump.toString.trim} RET with ${condRet.toString.trim}")
          condRet :: apply(options, rest)

        case _ => jump :: apply(options, xs)
      }
    case x :: xs => x :: apply(options, xs)
    case Nil => Nil
  }

  private def retPrecededByDiscards(code: List[ZLine]): Option[List[ZLine]] = {
    code match {
      case ZLine(op, _, _, _) :: xs if ZOpcodeClasses.NoopDiscards(op) => retPrecededByDiscards(xs)
      case ZLine(RET, NoRegisters, _, _) :: xs => Some(xs)
      case _ => None
    }
  }

  private def startsWithLabel(LabelName: String, code: List[ZLine]): Boolean = {
    code match {
      case ZLine(LABEL, _, MemoryAddressConstant(Label(LabelName)), _) :: xs => true
      case ZLine(LABEL, _, _, _) :: xs => startsWithLabel(LabelName, xs)
      case ZLine(op, _, _, _) :: xs if ZOpcodeClasses.NoopDiscards(op) => startsWithLabel(LabelName, xs)
      case _ => false
    }
  }
}
