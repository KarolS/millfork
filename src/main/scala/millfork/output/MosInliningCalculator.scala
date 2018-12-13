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

object MosInliningCalculator extends AbstractInliningCalculator[AssemblyLine] {

  private val sizes = Seq(64, 64, 8, 6, 5, 5, 4)

  private val badOpcodes = Set(RTI, RTS, JSR, BRK, RTL, BSR, BYTE) ++ OpcodeClasses.ChangesStack
  private val jumpingRelatedOpcodes = Set(LABEL, JMP) ++ OpcodeClasses.ShortBranching

  def codeForInlining(fname: String, functionsThatCanBeCalledFromInlinedFunctions: Set[String], code: List[AssemblyLine]): Option[List[AssemblyLine]] = {
    if (code.isEmpty) return None
    val localLabels = code.flatMap{
      case AssemblyLine(LABEL, _, MemoryAddressConstant(Label(l)), _) => Some(l)
      case _ => None
    }
    val lastLineOfCode = code.last
    lastLineOfCode match {
      case AssemblyLine(RTS | RTL, _, _, _) =>
      case AssemblyLine(JMP, AddrMode.Absolute, _, _) =>
      case _ => return None
    }
    var result = code.init
    if (lastLineOfCode.opcode == JMP) {
      result = result :+ lastLineOfCode.copy(opcode = JSR)
    }
    while (result.nonEmpty && OpcodeClasses.NoopDiscardsFlags(result.last.opcode)) {
      result = result.init
    }
    if (result.head.opcode == LABEL && result.head.parameter == Label(fname).toAddress) result = result.tail
    if (result.exists{
      case AssemblyLine(op, AddrMode.Absolute | AddrMode.Relative | AddrMode.DoesNotExist, MemoryAddressConstant(Label(l)), _) if jumpingRelatedOpcodes(op) =>
        if (!localLabels.contains(l) && !l.startsWith(".")) {
          println("Bad jump " + l)
          true
        } else false
      case AssemblyLine(JSR, AddrMode.Absolute, MemoryAddressConstant(th:ExternFunction), _) =>
        false
      case AssemblyLine(JSR, AddrMode.Absolute, MemoryAddressConstant(th:NormalFunction), _) =>
        if(!functionsThatCanBeCalledFromInlinedFunctions(th.name)){
          println("Bad call " + th)
          true
        } else false
      case AssemblyLine(op, _, _, _) if jumpingRelatedOpcodes(op) || badOpcodes(op) =>
        println("Bad opcode " + op)
        true
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
