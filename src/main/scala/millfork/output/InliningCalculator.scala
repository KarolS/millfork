package millfork.output

import millfork.assembly.{AddrMode, AssemblyLine, Opcode, OpcodeClasses}
import millfork.assembly.Opcode._
import millfork.compiler.{ExpressionCompiler, MfCompiler}
import millfork.env._
import millfork.node._

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
object InliningCalculator {

  private val sizes = Seq(64, 64, 8, 6, 5, 5, 4)

  def getPotentiallyInlineableFunctions(program: Program,
                                        inlineByDefault: Boolean,
                                        aggressivenessForNormal: Double,
                                        aggressivenessForRecommended: Double): Map[String, Int] = {
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
          || f.statements.exists(_.lastOption.exists(_.isInstanceOf[ReturnDispatchStatement]))) badFunctions += f.name
      case _ =>
    }
    allFunctions --= badFunctions
    recommendedFunctions --= badFunctions
    (if (inlineByDefault) allFunctions else recommendedFunctions).map(f => f -> {
      val size = sizes(callCount(f) min (sizes.size - 1))
      val aggressiveness = if (recommendedFunctions(f)) aggressivenessForRecommended else aggressivenessForNormal
      (size * aggressiveness).floor.toInt
    }).toMap
  }

  private def getAllCalledFunctions(expressions: List[Node]): List[(String, Boolean)] = expressions.flatMap {
    case s: VariableDeclarationStatement => getAllCalledFunctions(s.address.toList) ++ getAllCalledFunctions(s.initialValue.toList)
    case ReturnDispatchStatement(index, params, branches) =>
      getAllCalledFunctions(List(index)) ++ getAllCalledFunctions(params) ++ getAllCalledFunctions(branches.map(b => b.function))
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

  private val badOpcodes = Set(RTI, RTS, JSR, BRK) ++ OpcodeClasses.ChangesStack
  private val jumpingRelatedOpcodes = Set(LABEL, JMP) ++ OpcodeClasses.ShortBranching

  def codeForInlining(fname: String, code: List[AssemblyLine]): Option[List[AssemblyLine]] = {
    if (code.isEmpty) return None
    if (code.last.opcode != RTS) return None
    var result = code.init
    while (result.nonEmpty && OpcodeClasses.NoopDiscardsFlags(result.last.opcode)) {
      result = result.init
    }
    if (result.head.opcode == LABEL && result.head.parameter == Label(fname).toAddress) result = result.tail
    if (result.exists{
      case AssemblyLine(op, AddrMode.Absolute | AddrMode.Relative, MemoryAddressConstant(Label(l)), _) if jumpingRelatedOpcodes(op) =>
        !l.startsWith(".")
      case AssemblyLine(op, _, _, _) if jumpingRelatedOpcodes(op) || badOpcodes(op) => true
      case _ => false
    }) return None
    Some(result)
  }
}
