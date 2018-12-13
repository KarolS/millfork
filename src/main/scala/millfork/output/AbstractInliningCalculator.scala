package millfork.output

import millfork.JobContext
import millfork.assembly.AbstractCode
import millfork.assembly.mos.Opcode
import millfork.assembly.z80.ZOpcode
import millfork.compiler.AbstractCompiler
import millfork.node._

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */

case class InliningResult(potentiallyInlineableFunctions: Map[String, Int], nonInlineableFunctions: Set[String])

abstract class AbstractInliningCalculator[T <: AbstractCode] {
  def codeForInlining(fname: String, functionsThatCanBeCalledFromInlinedFunctions: Set[String], code: List[T]): Option[List[T]]
  def inline(code: List[T], inlinedFunctions: Map[String, List[T]], jobContext: JobContext): List[T]

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

  protected def containsReturnDispatch(statements: Seq[Statement]): Boolean = statements.exists {
    case _: ReturnDispatchStatement => true
    case c: CompoundStatement => containsReturnDispatch(c.getChildStatements)
    case _ => false
  }

  protected def getAllCalledFunctions(expressions: List[Node]): List[(String, Boolean)] = expressions.flatMap {
    case s: VariableDeclarationStatement => getAllCalledFunctions(s.address.toList) ++ getAllCalledFunctions(s.initialValue.toList)
    case ReturnDispatchStatement(index, params, branches) =>
      getAllCalledFunctions(List(index)) ++ getAllCalledFunctions(params) ++ getAllCalledFunctions(branches.map(b => b.function))
    case s: ArrayDeclarationStatement => getAllCalledFunctions(s.address.toList) ++ getAllCalledFunctions(s.elements.toList)
    case s: ArrayContents => getAllCalledFunctions(s.getAllExpressions)
    case s: FunctionDeclarationStatement => getAllCalledFunctions(s.address.toList) ++ getAllCalledFunctions(s.statements.getOrElse(Nil))
    case Assignment(VariableExpression(_), expr) => getAllCalledFunctions(expr :: Nil)
    case MosAssemblyStatement(Opcode.JSR, _, VariableExpression(name), true) => (name -> false) :: Nil
    case Z80AssemblyStatement(ZOpcode.CALL, _, _, VariableExpression(name), true) => (name -> false) :: Nil
    case s: Statement => getAllCalledFunctions(s.getAllExpressions)
    case s: VariableExpression => Set(
          s.name,
          s.name.stripSuffix(".addr"),
          s.name.stripSuffix(".hi"),
          s.name.stripSuffix(".lo"),
          s.name.stripSuffix(".addr.lo"),
          s.name.stripSuffix(".addr.hi")).toList.map(_ -> true)
    case s: LiteralExpression => Nil
    case s: GeneratedConstantExpression => Nil
    case HalfWordExpression(param, _) => getAllCalledFunctions(param :: Nil)
    case SumExpression(xs, _) => getAllCalledFunctions(xs.map(_._2))
    case FunctionCallExpression(name, xs) => (name -> false) :: getAllCalledFunctions(xs)
    case IndexedExpression(arr, index) => (arr -> true) :: getAllCalledFunctions(List(index))
    case SeparateBytesExpression(h, l) => getAllCalledFunctions(List(h, l))
    case _ => Nil
  }
}
