package millfork.output

import millfork.assembly.{AssemblyLine, Opcode, OpcodeClasses}
import millfork.assembly.Opcode._
import millfork.env._
import millfork.node._

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
object InliningCalculator {

  private val sizes = Seq(64, 64, 8, 6, 5, 5, 4)

  def getPotentiallyInlineableFunctions(program: Program): Map[String, Int] = {
    val callCount = mutable.Map[String, Int]().withDefaultValue(0)
    val allFunctions = mutable.Set[String]()
    val badFunctions = mutable.Set[String]()
    getAllCalledFunctions(program.declarations).foreach{
      case (name, true) => badFunctions += name
      case (name, false) => callCount(name) += 1
    }
    program.declarations.foreach{
      case f:FunctionDeclarationStatement =>
        allFunctions += f.name
        if (f.inlined) badFunctions += f.name
        if (f.address.isDefined) badFunctions += f.name
        if (f.interrupt) badFunctions += f.name
        if (f.reentrant) badFunctions += f.name
        if (f.name == "main") badFunctions += f.name
      case _ =>
    }
    allFunctions --= badFunctions
    allFunctions.map(f => f -> sizes(callCount(f) min (sizes.size - 1))).toMap
  }

  private def getAllCalledFunctions(expressions: List[Node]): List[(String, Boolean)] = expressions.flatMap {
    case s: VariableDeclarationStatement => getAllCalledFunctions(s.address.toList) ++ getAllCalledFunctions(s.initialValue.toList)
    case s: ArrayDeclarationStatement => getAllCalledFunctions(s.address.toList) ++ getAllCalledFunctions(s.elements.getOrElse(Nil))
    case s: FunctionDeclarationStatement => getAllCalledFunctions(s.address.toList) ++ getAllCalledFunctions(s.statements.getOrElse(Nil))
    case Assignment(VariableExpression(_), expr) => getAllCalledFunctions(expr :: Nil)
    case AssemblyStatement(JSR, _, VariableExpression(name), true) => (name -> false) :: Nil
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

  private val badOpcodes =
    Set(RTI, RTS, JSR, JMP, LABEL, BRK) ++
      OpcodeClasses.ShortBranching ++
      OpcodeClasses.ChangesStack

  def codeForInlining(fname: String, code: List[AssemblyLine]): Option[List[AssemblyLine]] = {
    if (code.isEmpty) return None
    if (code.last.opcode != RTS) return None
    var result = code.init
    if (result.head.opcode == LABEL && result.head.parameter == Label(fname).toAddress) result = result.tail
    if (result.exists(l => badOpcodes(l.opcode))) return None
    Some(result)
  }
}
