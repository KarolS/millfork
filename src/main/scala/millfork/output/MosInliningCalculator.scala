package millfork.output

import millfork.JobContext
import millfork.assembly.mos.Opcode._
import millfork.assembly.mos.{AddrMode, _}
import millfork.compiler.AbstractCompiler
import millfork.env._
import millfork.node._

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */

case class InliningResult(potentiallyInlineableFunctions: Map[String, Int], nonInlineableFunctions: Set[String])

object MosInliningCalculator extends AbstractInliningCalculator[AssemblyLine] {

  private val sizes = Seq(64, 64, 8, 6, 5, 5, 4)

  private val badOpcodes = Set(RTI, RTS, JSR, BRK, RTL, BSR, BYTE) ++ OpcodeClasses.ChangesStack
  private val jumpingRelatedOpcodes = Set(LABEL, JMP) ++ OpcodeClasses.ShortBranching

  def codeForInlining(fname: String, functionsAlreadyKnownToBeNonInlineable: Set[String], code: List[AssemblyLine]): Option[List[AssemblyLine]] = {
    if (code.isEmpty) return None
    val lastOpcode = code.last.opcode
    if (lastOpcode != RTS && lastOpcode != RTL) return None
    var result = code.init
    while (result.nonEmpty && OpcodeClasses.NoopDiscardsFlags(result.last.opcode)) {
      result = result.init
    }
    if (result.head.opcode == LABEL && result.head.parameter == Label(fname).toAddress) result = result.tail
    if (result.exists{
      case AssemblyLine(op, AddrMode.Absolute | AddrMode.Relative | AddrMode.DoesNotExist, MemoryAddressConstant(Label(l)), _) if jumpingRelatedOpcodes(op) =>
        !l.startsWith(".")
      case AssemblyLine(JSR, AddrMode.Absolute, MemoryAddressConstant(th:ExternFunction), _) => false
      case AssemblyLine(JSR, AddrMode.Absolute, MemoryAddressConstant(th:NormalFunction), _) =>
        !functionsAlreadyKnownToBeNonInlineable(th.name)
      case AssemblyLine(op, _, _, _) if jumpingRelatedOpcodes(op) || badOpcodes(op) => true
      case _ => false
    }) return None
    Some(result)
  }

  def inline(code: List[AssemblyLine], inlinedFunctions: Map[String, List[AssemblyLine]], jobContext: JobContext): List[AssemblyLine] = {
    code.flatMap {
      case AssemblyLine(Opcode.JSR, AddrMode.Absolute | AddrMode.LongAbsolute, p, true) if inlinedFunctions.contains(p.toString) =>
        val labelPrefix = jobContext.nextLabel("ai")
        inlinedFunctions(p.toString).map {
          case line@AssemblyLine(_, _, MemoryAddressConstant(Label(label)), _) =>
            val newLabel = MemoryAddressConstant(Label(labelPrefix + label))
            line.copy(parameter = newLabel)
          case l => l
        }
      case AssemblyLine(Opcode.JMP, AddrMode.Absolute, p, true) if inlinedFunctions.contains(p.toString) =>
        val labelPrefix = jobContext.nextLabel("ai")
        inlinedFunctions(p.toString).map {
          case line@AssemblyLine(_, _, MemoryAddressConstant(Label(label)), _) =>
            val newLabel = MemoryAddressConstant(Label(labelPrefix + label))
            line.copy(parameter = newLabel)
          case l => l
        } :+ AssemblyLine.implied(Opcode.RTS)
      case x => List(x)
    }
  }
}
