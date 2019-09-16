package millfork.output

import millfork.{CompilationOptions, JobContext}
import millfork.assembly.Elidability
import millfork.assembly.z80._
import millfork.env._

import scala.collection.GenTraversableOnce

/**
  * @author Karol Stasiak
  */
object Z80InliningCalculator extends AbstractInliningCalculator[ZLine] {

  import ZOpcode._

  private val badOpcodes = Set(RET, RETI, RETN, CALL, BYTE, POP, PUSH)
  private val jumpingRelatedOpcodes = Set(LABEL, JP, JR)

  override def calculateExpectedSizeAfterInlining(options: CompilationOptions, params: ParamSignature, code: List[ZLine]): Int = {
    var sum = 0
    for (c <- code) {
      sum += c.sizeInBytes
    }
    sum
  }

  override def codeForInlining(fname: String, functionsThatCanBeCalledFromInlinedFunctions: Set[String], code: List[ZLine]): Option[List[ZLine]] = {
    if (code.isEmpty) return None
    code.last match {
      case ZLine0(RET, NoRegisters, _) =>
      case _ => return None
    }
    var result = code.init
    while (result.nonEmpty && ZOpcodeClasses.NoopDiscards(result.last.opcode)) {
      result = result.init
    }
    if (result.head.opcode == LABEL && result.head.parameter == Label(fname).toAddress) result = result.tail
    if (result.exists {
      case ZLine0(op, _, MemoryAddressConstant(Label(l))) if jumpingRelatedOpcodes(op) =>
        !l.startsWith(".")
      case ZLine0(CALL, _, MemoryAddressConstant(th: ExternFunction)) => false
      case ZLine0(CALL, _, NumericConstant(_, _)) => false
      case ZLine0(JP, OneRegister(_), _) => false
      case ZLine0(CALL, _, MemoryAddressConstant(th: NormalFunction)) =>
        !functionsThatCanBeCalledFromInlinedFunctions(th.name)
      case ZLine0(op, _, _) if jumpingRelatedOpcodes(op) || badOpcodes(op) => true
      case _ => false
    }) return None
    Some(result)
  }

  def wrap(registers: ZRegisters, jobContext: JobContext, lines: List[ZLine]): List[ZLine] = registers match {
    case NoRegisters => lines
    case IfFlagClear(flag) =>
      val label = jobContext.nextLabel("ai")
      ZLine.jump(label, IfFlagSet(flag)) :: (lines :+ ZLine.label(label))
    case IfFlagSet(flag) =>
      val label = jobContext.nextLabel("ai")
      ZLine.jump(label, IfFlagClear(flag)) :: (lines :+ ZLine.label(label))
    case _ => throw new IllegalArgumentException("registers")
  }

  override def inline(code: List[ZLine], inlinedFunctions: Map[String, List[ZLine]], jobContext: JobContext): List[ZLine] = {
    code.flatMap {
      case callInstr@ZLine(CALL, registers, p, Elidability.Elidable, _) if inlinedFunctions.contains(p.toString) =>
        val labelPrefix = jobContext.nextLabel("ai")
        wrap(registers, jobContext, {
          var inlinedCode = inlinedFunctions(p.toString)
          if (inlinedCode.forall(_.source.isEmpty)) {
            inlinedCode = inlinedCode.map(_.copy(source = callInstr.source))
          }
          inlinedCode.map {
            case line@ZLine0(_, _, MemoryAddressConstant(Label(label))) =>
              val newLabel = MemoryAddressConstant(Label(labelPrefix + label))
              line.copy(parameter = newLabel)
            case l => l
          }
        })
      case callInstr@ZLine(JP | JR, registers, p, Elidability.Elidable, _) if inlinedFunctions.contains(p.toString) =>
        val labelPrefix = jobContext.nextLabel("ai")
        wrap(registers, jobContext, {
          var inlinedCode = inlinedFunctions(p.toString)
          if (inlinedCode.forall(_.source.isEmpty)) {
            inlinedCode = inlinedCode.map(_.copy(source = callInstr.source))
          }
          inlinedCode.map {
            case line@ZLine0(_, _, MemoryAddressConstant(Label(label))) =>
              val newLabel = MemoryAddressConstant(Label(labelPrefix + label))
              line.copy(parameter = newLabel)
            case l => l
          } :+ ZLine.implied(RET)
        })
      case x => List(x)
    }
  }
}
