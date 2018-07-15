package millfork.output

import millfork.assembly.z80._
import millfork.compiler.AbstractCompiler
import millfork.env.{ExternFunction, Label, MemoryAddressConstant, NormalFunction}

import scala.collection.GenTraversableOnce

/**
  * @author Karol Stasiak
  */
object Z80InliningCalculator extends AbstractInliningCalculator[ZLine] {

  import ZOpcode._

  private val badOpcodes = Set(RET, RETI, RETN, CALL, BYTE, POP, PUSH)
  private val jumpingRelatedOpcodes = Set(LABEL, JP, JR)

  override def codeForInlining(fname: String, functionsAlreadyKnownToBeNonInlineable: Set[String], code: List[ZLine]): Option[List[ZLine]] = {
    if (code.isEmpty) return None
    code.last match {
      case ZLine(RET, NoRegisters, _, _) =>
      case _ => return None
    }
    var result = code.init
    while (result.nonEmpty && ZOpcodeClasses.NoopDiscards(result.last.opcode)) {
      result = result.init
    }
    if (result.head.opcode == LABEL && result.head.parameter == Label(fname).toAddress) result = result.tail
    if (result.exists {
      case ZLine(op, _, MemoryAddressConstant(Label(l)), _) if jumpingRelatedOpcodes(op) =>
        !l.startsWith(".")
      case ZLine(CALL, _, MemoryAddressConstant(th: ExternFunction), _) => false
      case ZLine(CALL, _, MemoryAddressConstant(th: NormalFunction), _) =>
        !functionsAlreadyKnownToBeNonInlineable(th.name)
      case ZLine(op, _, _, _) if jumpingRelatedOpcodes(op) || badOpcodes(op) => true
      case _ => false
    }) return None
    Some(result)
  }

  def wrap(registers: ZRegisters, compiler: AbstractCompiler[ZLine], lines: List[ZLine]): List[ZLine] = registers match {
    case NoRegisters => lines
    case IfFlagClear(flag) =>
      val label = compiler.nextLabel("ai")
      ZLine.jump(label, IfFlagSet(flag)) :: (lines :+ ZLine.label(label))
    case IfFlagSet(flag) =>
      val label = compiler.nextLabel("ai")
      ZLine.jump(label, IfFlagClear(flag)) :: (lines :+ ZLine.label(label))
    case _ => throw new IllegalArgumentException("registers")
  }

  override def inline(code: List[ZLine], inlinedFunctions: Map[String, List[ZLine]], compiler: AbstractCompiler[ZLine]): List[ZLine] = {
    code.flatMap {
      case ZLine(CALL, registers, p, true) if inlinedFunctions.contains(p.toString) =>
        val labelPrefix = compiler.nextLabel("ai")
        wrap(registers, compiler,
          inlinedFunctions(p.toString).map {
            case line@ZLine(_, _, MemoryAddressConstant(Label(label)), _) =>
              val newLabel = MemoryAddressConstant(Label(labelPrefix + label))
              line.copy(parameter = newLabel)
            case l => l
          })
      case ZLine(JP | JR, registers, p, true) if inlinedFunctions.contains(p.toString) =>
        val labelPrefix = compiler.nextLabel("ai")
        wrap(registers, compiler,
          inlinedFunctions(p.toString).map {
            case line@ZLine(_, _, MemoryAddressConstant(Label(label)), _) =>
              val newLabel = MemoryAddressConstant(Label(labelPrefix + label))
              line.copy(parameter = newLabel)
            case l => l
          } :+ ZLine.implied(RET))
      case x => List(x)
    }
  }
}
