package millfork.node.opt

import millfork.{CompilationFlag, CompilationOptions, CpuFamily}
import millfork.env._
import millfork.error.ConsoleLogger
import millfork.node._

/**
  * @author Karol Stasiak
  */
object UnusedFunctions extends NodeOptimization {

  private val operatorImplementations: List[(String, Int, String)] = List(
    ("*", 2, "__mul_u8u8u8"),
    ("*", 3, "__mul_u16u8u16"),
    ("*=", 2, "__mul_u8u8u8"),
    ("*=", 2, "__mul_u16u8u16"),
    ("+'", 4, "__adc_decimal"),
    ("+'=", 4, "__adc_decimal"),
    ("-'", 4, "__sub_decimal"),
    ("-'=", 4, "__sub_decimal"),
    ("-'", 4, "__sbc_decimal"),
    ("-'=", 4, "__sbc_decimal"),
    ("<<'", 4, "__adc_decimal"),
    ("<<'=", 4, "__adc_decimal"),
    ("*'", 4, "__adc_decimal"),
    ("*'=", 4, "__adc_decimal"),
  )

  override def optimize(nodes: List[Node], options: CompilationOptions): List[Node] = {
    val aliases = nodes.flatMap{
      case AliasDefinitionStatement(source, target) => Some(source -> target)
      case _ => None
    }.toMap
    val panicRequired = options.flags(CompilationFlag.CheckIndexOutOfBounds)
    val allNormalFunctions = nodes.flatMap {
      case v: FunctionDeclarationStatement => if (v.address.isDefined && v.statements.isDefined || v.interrupt || v.name == "main" || panicRequired && v.name == "_panic") Nil else List(v.name)
      case _ => Nil
    }.toSet
    val allCalledFunctions = resolveAliases(aliases, getAllCalledFunctions(nodes).toSet)
    var unusedFunctions = allNormalFunctions -- allCalledFunctions
    val effectiveZpSize = options.platform.cpuFamily match {
      case CpuFamily.M6502 => options.zpRegisterSize
      case _ => 999999
    }
    for((op, zp, fun) <- operatorImplementations) {
      if (allCalledFunctions.contains(op) && effectiveZpSize >= zp) {
        unusedFunctions -= fun
      }
    }
    if (unusedFunctions.nonEmpty) {
      options.log.debug("Removing unused functions: " + unusedFunctions.mkString(", "))
      optimize(removeFunctionsFromProgram(nodes, unusedFunctions), options)
    } else {
      nodes
    }
  }

  private def removeFunctionsFromProgram(nodes: List[Node], unusedFunctions: Set[String]): List[Node] = {
    nodes match {
      case (x: FunctionDeclarationStatement) :: xs if unusedFunctions(x.name) =>
        removeFunctionsFromProgram(xs, unusedFunctions)
      case x :: xs =>
        x :: removeFunctionsFromProgram(xs, unusedFunctions)
      case Nil =>
        Nil
    }
  }

  def getAllCalledFunctions(c: Constant): List[String] = c match {
    case SubbyteConstant(cc, _) => getAllCalledFunctions(cc)
    case CompoundConstant(_, l, r) => getAllCalledFunctions(l) ++ getAllCalledFunctions(r)
    case MemoryAddressConstant(th) => List(
      th.name,
      th.name.stripSuffix(".addr"),
      th.name.stripSuffix(".hi"),
      th.name.stripSuffix(".lo"),
      th.name.stripSuffix(".addr.lo"),
      th.name.stripSuffix(".addr.hi"))
    case _ => Nil
  }

  def getAllCalledFunctions(expressions: List[Node]): List[String] = expressions.flatMap {
    case s: VariableDeclarationStatement => getAllCalledFunctions(s.address.toList) ++ getAllCalledFunctions(s.initialValue.toList)
    case s: ArrayDeclarationStatement => getAllCalledFunctions(s.address.toList) ++ getAllCalledFunctions(s.elements.toList)
    case s: ArrayContents => getAllCalledFunctions(s.getAllExpressions)
    case s: FunctionDeclarationStatement => getAllCalledFunctions(s.address.toList) ++ getAllCalledFunctions(s.statements.getOrElse(Nil))
    case Assignment(VariableExpression(_), expr) => getAllCalledFunctions(expr :: Nil)
    case s: ReturnDispatchStatement =>
      getAllCalledFunctions(s.getAllExpressions) ++ getAllCalledFunctions(s.branches.map(_.function))
    case s: Statement => getAllCalledFunctions(s.getAllExpressions)
    case s: VariableExpression => List(
          s.name,
          s.name.stripSuffix(".addr"),
          s.name.stripSuffix(".hi"),
          s.name.stripSuffix(".lo"),
          s.name.stripSuffix(".addr.lo"),
          s.name.stripSuffix(".addr.hi"))
    case s: LiteralExpression => Nil
    case HalfWordExpression(param, _) => getAllCalledFunctions(param :: Nil)
    case SumExpression(xs, decimal) =>
      var set = List[String]()
      if (decimal) {
        if (xs.tail.exists(_._1)) set ::= "-'"
        if (xs.tail.exists(p => !p._1)) set ::= "+'"
      }
      set ++ getAllCalledFunctions(xs.map(_._2))
    case FunctionCallExpression(name, xs) => name :: getAllCalledFunctions(xs)
    case IndexedExpression(arr, index) => arr :: getAllCalledFunctions(List(index))
    case SeparateBytesExpression(h, l) => getAllCalledFunctions(List(h, l))
    case _ => Nil
  }

}
