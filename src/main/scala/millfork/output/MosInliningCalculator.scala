package millfork.output

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

  def calculate(program: Program,
                inlineByDefault: Boolean,
                aggressivenessForNormal: Double,
                aggressivenessForRecommended: Double): InliningResult = {
    val callCount = mutable.Map[String, Int]().withDefaultValue(0)
    val allFunctions = mutable.Set[String]()
    val badFunctions = mutable.Set[String]()
    val recommendedFunctions = mutable.Set[String]()
    getAllCalledFunctions(program.declarations).foreach{
      case (name, true) => badFunctions += name
      case (name, false) => callCount(name) += 1
    }
    program.declarations.foreach{
      case f:FunctionDeclarationStatement =>
        allFunctions += f.name
        if (f.inlinable.contains(true)) {
          recommendedFunctions += f.name
        }
        if (f.isMacro
          || f.inlinable.contains(false)
          || f.address.isDefined
          || f.interrupt
          || f.reentrant
          || f.name == "main"
          || containsReturnDispatch(f.statements.getOrElse(Nil))) badFunctions += f.name
      case _ =>
    }
    allFunctions --= badFunctions
    recommendedFunctions --= badFunctions
    val map = (if (inlineByDefault) allFunctions else recommendedFunctions).map(f => f -> {
      val size = sizes(callCount(f) min (sizes.size - 1))
      val aggressiveness = if (recommendedFunctions(f)) aggressivenessForRecommended else aggressivenessForNormal
      (size * aggressiveness).floor.toInt
    }).toMap
    InliningResult(map, badFunctions.toSet)
  }

  private def containsReturnDispatch(statements: Seq[Statement]): Boolean = statements.exists {
    case _: ReturnDispatchStatement => true
    case c: CompoundStatement => containsReturnDispatch(c.getChildStatements)
    case _ => false
  }

  private def getAllCalledFunctions(expressions: List[Node]): List[(String, Boolean)] = expressions.flatMap {
    case s: VariableDeclarationStatement => getAllCalledFunctions(s.address.toList) ++ getAllCalledFunctions(s.initialValue.toList)
    case ReturnDispatchStatement(index, params, branches) =>
      getAllCalledFunctions(List(index)) ++ getAllCalledFunctions(params) ++ getAllCalledFunctions(branches.map(b => b.function))
    case s: ArrayDeclarationStatement => getAllCalledFunctions(s.address.toList) ++ getAllCalledFunctions(s.elements.toList)
    case s: ArrayContents => getAllCalledFunctions(s.getAllExpressions)
    case s: FunctionDeclarationStatement => getAllCalledFunctions(s.address.toList) ++ getAllCalledFunctions(s.statements.getOrElse(Nil))
    case Assignment(VariableExpression(_), expr) => getAllCalledFunctions(expr :: Nil)
    case MosAssemblyStatement(JSR, _, VariableExpression(name), true) => (name -> false) :: Nil
    case s: Statement => getAllCalledFunctions(s.getAllExpressions)
    case s: VariableExpression => Set(
          s.name,
          s.name.stripSuffix(".addr"),
          s.name.stripSuffix(".hi"),
          s.name.stripSuffix(".lo"),
          s.name.stripSuffix(".addr.lo"),
          s.name.stripSuffix(".addr.hi")).toList.map(_ -> true)
    case s: LiteralExpression => Nil
    case HalfWordExpression(param, _) => getAllCalledFunctions(param :: Nil)
    case SumExpression(xs, _) => getAllCalledFunctions(xs.map(_._2))
    case FunctionCallExpression(name, xs) => (name -> false) :: getAllCalledFunctions(xs)
    case IndexedExpression(arr, index) => (arr -> true) :: getAllCalledFunctions(List(index))
    case SeparateBytesExpression(h, l) => getAllCalledFunctions(List(h, l))
    case _ => Nil
  }

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

  def inline(code: List[AssemblyLine], inlinedFunctions: Map[String, List[AssemblyLine]], compiler: AbstractCompiler[AssemblyLine]): List[AssemblyLine] = {
    code.flatMap {
      case AssemblyLine(Opcode.JSR, AddrMode.Absolute | AddrMode.LongAbsolute, p, true) if inlinedFunctions.contains(p.toString) =>
        val labelPrefix = compiler.nextLabel("ai")
        inlinedFunctions(p.toString).map {
          case line@AssemblyLine(_, _, MemoryAddressConstant(Label(label)), _) =>
            val newLabel = MemoryAddressConstant(Label(labelPrefix + label))
            line.copy(parameter = newLabel)
          case l => l
        }
      case AssemblyLine(Opcode.JMP, AddrMode.Absolute, p, true) if inlinedFunctions.contains(p.toString) =>
        val labelPrefix = compiler.nextLabel("ai")
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
