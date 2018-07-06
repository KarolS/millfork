package millfork.compiler

import millfork.env._
import millfork.node._
import millfork.error.ErrorReporting
import millfork.assembly.AbstractCode

/**
  * @author Karol Stasiak
  */
class AbstractExpressionCompiler[T <: AbstractCode] {

  def getExpressionType(ctx: CompilationContext, expr: Expression): Type = AbstractExpressionCompiler.getExpressionType(ctx, expr)

  def lookupFunction(ctx: CompilationContext, f: FunctionCallExpression): MangledFunction = AbstractExpressionCompiler.lookupFunction(ctx, f)

  def assertCompatible(exprType: Type, variableType: Type): Unit = {
    // TODO
  }

  def callingContext(ctx: CompilationContext, v: MemoryVariable): CompilationContext = {
    val result = new Environment(Some(ctx.env), "", ctx.options.platform.cpuFamily)
    result.registerVariable(VariableDeclarationStatement(v.name, v.typ.name, stack = false, global = false, constant = false, volatile = false, register = false, initialValue = None, address = None, bank = v.declaredBank), ctx.options)
    ctx.copy(env = result)
  }

  def getParamMaxSize(ctx: CompilationContext, params: List[Expression]): Int = {
    params.map(expr => getExpressionType(ctx, expr).size).max
  }

  def getSumSize(ctx: CompilationContext, params: List[(Boolean, Expression)]): Int = {
    params.map { case (_, expr) => getExpressionType(ctx, expr).size}.max
  }

  def assertAllBytes(msg: String, ctx: CompilationContext, params: List[Expression]): Unit = {
    if (params.exists { expr => getExpressionType(ctx, expr).size != 1 }) {
      ErrorReporting.fatal(msg, params.head.position)
    }
  }

  def assertBinary(ctx: CompilationContext, params: List[Expression]): (Expression, Expression, Int) = {
    if (params.length != 2) {
      ErrorReporting.fatal("sfgdgfsd", None)
    }
    (params.head, params(1)) match {
      case (l: Expression, r: Expression) => (l, r, getExpressionType(ctx, l).size max getExpressionType(ctx, r).size)
    }
  }

  def assertComparison(ctx: CompilationContext, params: List[Expression]): (Int, Boolean) = {
    (params.head, params(1)) match {
      case (l: Expression, r: Expression) =>
        val lt = getExpressionType(ctx, l)
        val rt = getExpressionType(ctx, r)
        (lt.size max rt.size, lt.isSigned || rt.isSigned)
    }
  }

  def assertBool(ctx: CompilationContext, fname: String, params: List[Expression], expectedParamCount: Int): Unit = {
    if (params.length != expectedParamCount) {
      ErrorReporting.error("Invalid number of parameters for " + fname, params.headOption.flatMap(_.position))
      return
    }
    params.foreach { param =>
      if (!getExpressionType(ctx, param).isInstanceOf[BooleanType])
        ErrorReporting.fatal("Parameter should be boolean", param.position)
    }
  }

  def assertBool(ctx: CompilationContext, fname: String, params: List[Expression]): Unit = {
    if (params.length < 2) {
      ErrorReporting.error("Invalid number of parameters for " + fname, params.headOption.flatMap(_.position))
      return
    }
    params.foreach { param =>
      if (!getExpressionType(ctx, param).isInstanceOf[BooleanType])
        ErrorReporting.fatal("Parameter should be boolean", param.position)
    }
  }

  def assertAssignmentLike(ctx: CompilationContext, params: List[Expression]): (LhsExpression, Expression, Int) = {
    if (params.length != 2) {
      ErrorReporting.fatal("sfgdgfsd", None)
    }
    (params.head, params(1)) match {
      case (l: LhsExpression, r: Expression) =>
        val lsize = getExpressionType(ctx, l).size
        val rsize = getExpressionType(ctx, r).size
        if (lsize < rsize) {
          ErrorReporting.error("Left-hand-side expression is of smaller type than the right-hand-side expression", l.position)
        }
        (l, r, lsize)
      case (err: Expression, _) => ErrorReporting.fatal("Invalid left-hand-side expression", err.position)
    }
  }
}

object AbstractExpressionCompiler {
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
