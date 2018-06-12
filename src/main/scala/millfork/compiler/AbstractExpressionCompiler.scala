package millfork.compiler

import millfork.env._
import millfork.node._
import millfork.error.ErrorReporting
import millfork.assembly.AbstractCode
import millfork.compiler.mos.CompilationContext

/**
  * @author Karol Stasiak
  */
class AbstractExpressionCompiler[T <: AbstractCode] {

  def getExpressionType(ctx: CompilationContext, expr: Expression): Type = {
    val env = ctx.env
    val b = env.get[Type]("byte")
    val bool = env.get[Type]("bool$")
    val v = env.get[Type]("void")
    val w = env.get[Type]("word")
    expr match {
      case LiteralExpression(value, size) =>
        size match {
          case 1 => b
          case 2 => w
          case 3 => env.get[Type]("farword")
          case 4 => env.get[Type]("long")
        }
      case VariableExpression(name) =>
        env.get[TypedThing](name, expr.position).typ
      case HalfWordExpression(param, _) =>
        getExpressionType(ctx, param)
        b
      case IndexedExpression(_, _) => b
      case SeparateBytesExpression(hi, lo) =>
        if (getExpressionType(ctx, hi).size > 1) ErrorReporting.error("Hi byte too large", hi.position)
        if (getExpressionType(ctx, lo).size > 1) ErrorReporting.error("Lo byte too large", lo.position)
        w
      case SumExpression(params, _) => params.map { case (_, e) => getExpressionType(ctx, e).size }.max match {
        case 1 => b
        case 2 => w
        case _ => ErrorReporting.error("Adding values bigger than words", expr.position); w
      }
      case FunctionCallExpression("nonet", params) => w
      case FunctionCallExpression("not", params) => bool
      case FunctionCallExpression("hi", params) => b
      case FunctionCallExpression("lo", params) => b
      case FunctionCallExpression("*", params) => b
      case FunctionCallExpression("|" | "&" | "^", params) => params.map { e => getExpressionType(ctx, e).size }.max match {
        case 1 => b
        case 2 => w
        case _ => ErrorReporting.error("Adding values bigger than words", expr.position); w
      }
      case FunctionCallExpression("<<", List(a1, a2)) =>
        if (getExpressionType(ctx, a2).size > 1) ErrorReporting.error("Shift amount too large", a2.position)
        getExpressionType(ctx, a1)
      case FunctionCallExpression(">>", List(a1, a2)) =>
        if (getExpressionType(ctx, a2).size > 1) ErrorReporting.error("Shift amount too large", a2.position)
        getExpressionType(ctx, a1)
      case FunctionCallExpression("<<'", params) => b
      case FunctionCallExpression(">>'", params) => b
      case FunctionCallExpression(">>>>", params) => b
      case FunctionCallExpression("&&", params) => bool
      case FunctionCallExpression("||", params) => bool
      case FunctionCallExpression("^^", params) => bool
      case FunctionCallExpression("==", params) => bool
      case FunctionCallExpression("!=", params) => bool
      case FunctionCallExpression("<", params) => bool
      case FunctionCallExpression(">", params) => bool
      case FunctionCallExpression("<=", params) => bool
      case FunctionCallExpression(">=", params) => bool
      case FunctionCallExpression("+=", params) => v
      case FunctionCallExpression("-=", params) => v
      case FunctionCallExpression("*=", params) => v
      case FunctionCallExpression("+'=", params) => v
      case FunctionCallExpression("-'=", params) => v
      case FunctionCallExpression("*'=", params) => v
      case FunctionCallExpression("|=", params) => v
      case FunctionCallExpression("&=", params) => v
      case FunctionCallExpression("^=", params) => v
      case FunctionCallExpression("<<=", params) => v
      case FunctionCallExpression(">>=", params) => v
      case FunctionCallExpression("<<'=", params) => v
      case FunctionCallExpression(">>'=", params) => v
      case f@FunctionCallExpression(name, params) =>
        ctx.env.maybeGet[Type](name) match {
          case Some(typ) =>
            typ
          case None =>
            lookupFunction(ctx, f).returnType
        }
    }
  }

  def lookupFunction(ctx: CompilationContext, f: FunctionCallExpression): MangledFunction = {
    val paramsWithTypes = f.expressions.map(x => getExpressionType(ctx, x) -> x)
    ctx.env.lookupFunction(f.functionName, paramsWithTypes).getOrElse(
      ErrorReporting.fatal(s"Cannot find function `${f.functionName}` with given params `${paramsWithTypes.map(_._1).mkString("(", ",", ")")}`", f.position))
  }
}
