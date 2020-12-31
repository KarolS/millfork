package millfork.output

import millfork.assembly.m6809.MOpcode
import millfork.assembly.mos.Opcode
import millfork.assembly.z80.ZOpcode
import millfork.{CompilationOptions, JobContext}
import millfork.assembly.{AbstractCode, Elidability}
import millfork.env.ParamSignature
import millfork.node._

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */

case class InliningResult(potentiallyInlineableFunctions: Map[String, Int], nonInlineableFunctions: Set[String])

abstract class AbstractInliningCalculator[T <: AbstractCode] {

  def codeForInlining(fname: String, functionsThatCanBeCalledFromInlinedFunctions: Set[String], code: List[T]): Option[List[T]]
  def inline(code: List[T], inlinedFunctions: Map[String, List[T]], jobContext: JobContext): List[T]
  def calculateExpectedSizeAfterInlining(options: CompilationOptions, params: ParamSignature, code: List[T]): Int

  private val sizes = Seq(64, 64, 8, 6, 5, 5, 4)

  def calculate(program: Program,
                bankLayouts: Map[String, Seq[String]],
                inlineByDefault: Boolean,
                aggressivenessForNormal: Double,
                aggressivenessForRecommended: Double): InliningResult = {
    val callCount = mutable.Map[String, Int]().withDefaultValue(0)
    val allFunctions = mutable.Map[String, Double]()
    val badFunctions = mutable.Set[String]()
    val recommendedFunctions = mutable.Set[String]()
    getAllCalledFunctions(program.declarations).foreach{
      case (name, true) => badFunctions += name
      case (name, false) => callCount(name) += 1
    }
    program.declarations.foreach{
      case f:FunctionDeclarationStatement =>
        if (f.inlinable.contains(true)) {
          recommendedFunctions += f.name
        }
        val aggressiveness =
          if (f.inlinable.contains(true) || f.optimizationHints("inline")) aggressivenessForRecommended
          else aggressivenessForNormal
        allFunctions(f.name) = aggressiveness
        if (f.isMacro
          || f.inlinable.contains(false)
          || f.address.isDefined
          || f.interrupt
          || f.reentrant
          || f.name == "main"
          || bankLayouts.values.exists(_.contains(f.name))
          || containsReturnDispatch(f.statements.getOrElse(Nil))) badFunctions += f.name
      case _ =>
    }
    allFunctions --= badFunctions
    recommendedFunctions --= badFunctions
    val map = (if (inlineByDefault) allFunctions.keySet else recommendedFunctions).map(f => f -> {
      val size = sizes(callCount(f) min (sizes.size - 1))
      val aggressiveness = allFunctions.getOrElse(f, aggressivenessForNormal)
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
    case s: ArrayContents => getAllCalledFunctions(s.getAllExpressions(false)) // endianness doesn't matter here at all
    case s: FunctionDeclarationStatement => getAllCalledFunctions(s.address.toList) ++ getAllCalledFunctions(s.statements.getOrElse(Nil))
    case Assignment(VariableExpression(_), expr) => getAllCalledFunctions(expr :: Nil)
    case MosAssemblyStatement(Opcode.JSR, _, VariableExpression(name), Elidability.Elidable) => (name -> false) :: Nil
    case Z80AssemblyStatement(ZOpcode.CALL, _, _, VariableExpression(name), Elidability.Elidable) => (name -> false) :: Nil
    case M6809AssemblyStatement(MOpcode.JSR, _, VariableExpression(name), Elidability.Elidable) => (name -> false) :: Nil
    case s: Statement => getAllCalledFunctions(s.getAllExpressions)
    case s: VariableExpression => Set(
          s.name,
          s.name.stripSuffix(".raw"),
          s.name.stripSuffix(".raw.lo"),
          s.name.stripSuffix(".raw.hi"),
          s.name.stripSuffix(".pointer"),
          s.name.stripSuffix(".pointer.lo"),
          s.name.stripSuffix(".pointer.hi"),
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

  protected def extractThingName(fullName: String): String = {
    var result = fullName.takeWhile(_ != '.')
    if (result.length == fullName.length) return result
    val suffix = fullName.drop(result.length)
    if (suffix == ".return" || suffix.startsWith(".return.")) {
      result += ".return"
    }
    result
  }
}
