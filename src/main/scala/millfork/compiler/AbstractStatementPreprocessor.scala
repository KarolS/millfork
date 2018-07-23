package millfork.compiler

import millfork.CompilationFlag
import millfork.env._
import millfork.error.ErrorReporting
import millfork.node._
import AbstractExpressionCompiler.getExpressionType
import millfork.compiler.AbstractStatementPreprocessor.hiddenEffectFreeFunctions

import scala.collection.mutable.ListBuffer

/**
  * @author Karol Stasiak
  */
abstract class AbstractStatementPreprocessor(ctx: CompilationContext, statements: List[ExecutableStatement]) {
  type VV = Map[String, Constant]
  protected val optimize = true // TODO
  protected val env: Environment = ctx.env
  protected val localPrefix = ctx.function.name + "$"
  protected val usedIdentifiers = if (optimize) statements.flatMap(_.getAllExpressions).flatMap(_.getAllIdentifiers) else Set()
  protected val trackableVars: Set[String] = if (optimize) {
    env.getAllLocalVariables.map(_.name.stripPrefix(localPrefix))
      .filterNot(_.contains("."))
      .filterNot(_.contains("$"))
      .filterNot { vname =>
        val prefix = vname + "."
        usedIdentifiers.exists(_.startsWith(prefix))
      }.toSet
  } else Set() // TODO
  if (ErrorReporting.traceEnabled && trackableVars.nonEmpty) {
    ErrorReporting.trace("Tracking local variables: " + trackableVars.mkString(", "))
  }
  protected val reentrantVars: Set[String] = trackableVars.filter(v => env.get[Variable](v) match {
    case _: StackVariable => true
    case UninitializedMemoryVariable(_, _, VariableAllocationMethod.Auto, _) => ctx.options.flag(CompilationFlag.DangerousOptimizations)
    case _ => false
  })
  protected val nonreentrantVars: Set[String] = trackableVars -- reentrantVars


  def apply(): List[ExecutableStatement] = {
    optimizeStmts(statements, Map())._1
  }

  def optimizeStmts(stmts: Seq[ExecutableStatement], currentVarValues: VV): (List[ExecutableStatement], VV) = {
    val result = ListBuffer[ExecutableStatement]()
    var cv = currentVarValues
    for(stmt <- stmts){
      val p = optimizeStmt(stmt, cv)
      result += p._1
      cv = p._2
    }
    result.toList -> cv
  }

  def maybeOptimizeForStatement(f: ForStatement): Option[(ExecutableStatement, VV)]

  def optimizeStmt(stmt: ExecutableStatement, currentVarValues: VV): (ExecutableStatement, VV) = {
    var cv = currentVarValues
    val pos = stmt.position
    stmt match {
      case Assignment(ve@VariableExpression(v), arg) if trackableVars(v) =>
        cv = search(arg, cv)

        Assignment(ve, optimizeExpr(arg, cv)).pos(pos) -> (env.eval(arg, currentVarValues) match {
          case Some(c) => cv + (v -> c)
          case None => cv - v
        })
      case ExpressionStatement(expr@FunctionCallExpression("+=", List(VariableExpression(v), arg)))
        if currentVarValues.contains(v) =>
        cv = search(arg, cv)
        ExpressionStatement(optimizeExpr(expr, cv)).pos(pos) -> (env.eval(expr, currentVarValues) match {
          case Some(c) => if (cv.contains(v)) cv + (v -> (cv(v) + c)) else cv
          case None => cv - v
        })
      case ExpressionStatement(expr@FunctionCallExpression(op, List(VariableExpression(v), arg)))
        if op.endsWith("=") && op != ">=" && op != "<=" && op != ":=" =>
        cv = search(arg, cv)
        ExpressionStatement(optimizeExpr(expr, cv)).pos(pos) -> (cv - v)
      case ExpressionStatement(expr) =>
        cv = search(expr, cv)
        ExpressionStatement(optimizeExpr(expr, cv)).pos(pos) -> cv
      case IfStatement(cond, th, el) =>
        cv = search(cond, cv)
        val c = optimizeExpr(cond, cv)
        val (t, vt) = optimizeStmts(th, cv)
        val (e, ve) = optimizeStmts(el, cv)
        IfStatement(c, t, e).pos(pos) -> commonVV(vt, ve)
      case WhileStatement(cond, body, inc, labels) =>
        cv = search(cond, cv)
        val c = optimizeExpr(cond, cv)
        val (b, _) = optimizeStmts(body, Map())
        val (i, _) = optimizeStmts(inc, Map())
        WhileStatement(c, b, i, labels).pos(pos) -> Map()
      case DoWhileStatement(body, inc, cond, labels) =>
        val c = optimizeExpr(cond, Map())
        val (b, _) = optimizeStmts(body, Map())
        val (i, _) = optimizeStmts(inc, Map())
        DoWhileStatement(b, i, c, labels).pos(pos) -> Map()
      case f@ForStatement(v, st, en, dir, body) =>
        maybeOptimizeForStatement(f) match {
          case Some(x) => x
          case None =>
            val s = optimizeExpr(st, Map())
            val e = optimizeExpr(en, Map())
            val (b, _) = optimizeStmts(body, Map())
            ForStatement(v, s, e, dir, b).pos(pos) -> Map()
        }
      case _ => stmt -> Map()
    }
  }

  def search(expr: Expression, cv: VV): VV = {
    expr match {
      case FunctionCallExpression(op, List(VariableExpression(v), arg)) if op.endsWith("=") && op != "<=" && op != ">=" =>
        search(arg, cv - v)
      case FunctionCallExpression(name, params)
        if hiddenEffectFreeFunctions(name) || env.maybeGet[Type](name).isDefined =>
        params.map(p => search(p, cv)).reduce(commonVV)
      case FunctionCallExpression(_, _) => cv -- nonreentrantVars
      case SumExpression(params, _) => params.map(p => search(p._2, cv)).reduce(commonVV)
      case HalfWordExpression(arg, _) => search(arg, cv)
      case IndexedExpression(_, arg) => search(arg, cv)
      case _ => cv // TODO
    }
  }

  def commonVV(a: VV, b: VV): VV = {
    if (a.isEmpty) return a
    if (b.isEmpty) return b
    val keys = a.keySet & b.keySet
    if (keys.isEmpty) return Map()
    keys.flatMap{ k =>
      val aa = a(k)
      val bb = b(k)
      if (aa == bb) Some(k -> aa) else None
    }.toMap
  }

  def isHiddenEffectFree(expr: Expression): Boolean = {
    expr match {
      case _: VariableExpression => true
      case _: LiteralExpression => true
      case _: ConstantArrayElementExpression => true
      case _: GeneratedConstantExpression => true
      case FunctionCallExpression(name, params) =>
        hiddenEffectFreeFunctions(name) && params.forall(isHiddenEffectFree)
      case _ => false // TODO
    }
  }

  def optimizeExpr(expr: Expression, currentVarValues: VV): Expression = {
    val pos = expr.position
    expr match {
      case FunctionCallExpression("->", List(handle, VariableExpression(field))) =>
        expr
      case FunctionCallExpression("->", List(handle, FunctionCallExpression(method, params))) =>
        expr
      case VariableExpression(v) if currentVarValues.contains(v) =>
        val constant = currentVarValues(v)
        ErrorReporting.debug(s"Using node flow to replace $v with $constant", pos)
        GeneratedConstantExpression(constant, getExpressionType(ctx, expr)).pos(pos)
      case FunctionCallExpression(t1, List(FunctionCallExpression(t2, List(arg))))
        if optimize && pointlessDoubleCast(t1, t2, expr) =>
        ErrorReporting.debug(s"Pointless double cast $t1($t2(...))", pos)
        optimizeExpr(FunctionCallExpression(t1, List(arg)), currentVarValues)
      case FunctionCallExpression(t1, List(arg))
        if optimize && pointlessCast(t1, expr) =>
        ErrorReporting.debug(s"Pointless cast $t1(...)", pos)
        optimizeExpr(arg, currentVarValues)
      case _ => expr // TODO
    }
  }

  def pointlessCast(t1: String, expr: Expression): Boolean = {
    val typ1 = env.maybeGet[Type](t1).getOrElse(return false)
    val typ2 = getExpressionType(ctx, expr)
    typ1.name == typ2.name
  }

  def pointlessDoubleCast(t1: String, t2: String, expr: Expression): Boolean = {
    val s1 = env.maybeGet[Type](t1).getOrElse(return false).size
    val s2 = env.maybeGet[Type](t2).getOrElse(return false).size
    if (s1 != s2) return false
    val s3 = AbstractExpressionCompiler.getExpressionType(ctx, expr).size
    s1 == s3
  }
}

object AbstractStatementPreprocessor {
  val hiddenEffectFreeFunctions = Set(
    "+", "+'", "-", "-'",
    "*", "*'",
    "<<", "<<'", ">>", ">>'", ">>>>",
    "&", "&&", "||", "|", "^",
    "==", "!=", "<", ">", ">=", "<=",
    "not", "hi", "lo", "nonet"
  )
}