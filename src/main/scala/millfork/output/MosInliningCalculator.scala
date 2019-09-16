package millfork.output

import millfork.{CompilationOptions, JobContext}
import millfork.assembly.Elidability
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

  private val badOpcodes = Set(RTI, RTS, JSR, BRK, RTL, BSR, BYTE) ++ OpcodeClasses.ChangesStack
  private val jumpingRelatedOpcodes = Set(LABEL, JMP) ++ OpcodeClasses.ShortBranching
  def calculateExpectedSizeAfterInlining(options: CompilationOptions, params: ParamSignature, code: List[AssemblyLine]): Int =  {
      var sum = 0
      var beforeParams = true
      val paramNames = params.paramThingNames
      for ((c, ix) <- code.zipWithIndex) {
        import Opcode._
        import millfork.assembly.mos.AddrMode._
        sum += (c match {
          case AssemblyLine0(LABEL, _, _) if ix == 0  => 0

          case AssemblyLine0(
          LDA | LDX | LDY | LDZ,
          Absolute | ZeroPage,
          MemoryAddressConstant(thing))
            if beforeParams && paramNames(extractThingName(thing.name))
          => 1 // a guess of how likely parameter copying can be avoided

          case AssemblyLine0(
          STA | STX | STY | STZ,
          Absolute | ZeroPage | LongAbsolute,
          _)
          => c.sizeInBytes

          case _ =>
            beforeParams = false
            c.sizeInBytes
        })
      }
    sum
    }

  def codeForInlining(fname: String, functionsThatCanBeCalledFromInlinedFunctions: Set[String], code: List[AssemblyLine]): Option[List[AssemblyLine]] = {
    if (code.isEmpty) return None
    val localLabels = code.flatMap{
      case AssemblyLine0(LABEL, _, MemoryAddressConstant(Label(l))) => Some(l)
      case _ => None
    }
    val lastLineOfCode = code.last
    lastLineOfCode match {
      case AssemblyLine0(RTS | RTL, _, _) =>
      case AssemblyLine0(JMP, AddrMode.Absolute, _) =>
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
      case AssemblyLine0(op, AddrMode.Absolute | AddrMode.Relative | AddrMode.DoesNotExist, MemoryAddressConstant(Label(l))) if jumpingRelatedOpcodes(op) =>
        if (!localLabels.contains(l) && !l.startsWith(".")) {
//          println("Bad jump " + l)
          true
        } else false
      case AssemblyLine0(JSR, AddrMode.Absolute, MemoryAddressConstant(th:ExternFunction)) =>
        false
      case AssemblyLine0(JSR, AddrMode.Absolute, MemoryAddressConstant(th:NormalFunction)) =>
        if(!functionsThatCanBeCalledFromInlinedFunctions(th.name)){
//          println("Bad call " + th)
          true
        } else false
      case AssemblyLine0(op, _, _) if jumpingRelatedOpcodes(op) || badOpcodes(op) =>
//        println("Bad opcode " + op)
        true
      case _ => false
    }) return None
    Some(result)
  }

  def inline(code: List[AssemblyLine], inlinedFunctions: Map[String, List[AssemblyLine]], jobContext: JobContext): List[AssemblyLine] = {
    code.flatMap {
      case callInstr@AssemblyLine(Opcode.JSR, AddrMode.Absolute | AddrMode.LongAbsolute, p, Elidability.Elidable, _) if inlinedFunctions.contains(p.toString) =>
        val labelPrefix = jobContext.nextLabel("ai")
        var inlinedCode = inlinedFunctions(p.toString)
        if (inlinedCode.forall(_.source.isEmpty)) {
          inlinedCode = inlinedCode.map(_.copy(source = callInstr.source))
        }
        inlinedCode.map {
          case line@AssemblyLine0(_, _, MemoryAddressConstant(Label(label))) =>
            val newLabel = MemoryAddressConstant(Label(labelPrefix + label))
            line.copy(parameter = newLabel)
          case l => l
        }
      case callInstr@AssemblyLine(Opcode.JMP, AddrMode.Absolute, p, Elidability.Elidable, _) if inlinedFunctions.contains(p.toString) =>
        val labelPrefix = jobContext.nextLabel("ai")
        var inlinedCode = inlinedFunctions(p.toString)
        if (inlinedCode.forall(_.source.isEmpty)) {
          inlinedCode = inlinedCode.map(_.copy(source = callInstr.source))
        }
        inlinedCode.map {
          case line@AssemblyLine0(_, _, MemoryAddressConstant(Label(label))) =>
            val newLabel = MemoryAddressConstant(Label(labelPrefix + label))
            line.copy(parameter = newLabel)
          case l => l
        } :+ AssemblyLine.implied(Opcode.RTS)
      case x => List(x)
    }
  }
}
