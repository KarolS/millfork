package millfork.compiler

import millfork.{CompilationFlag, CpuFamily, node}
import millfork.env._
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
    env.getAllLocalVariables
      .filterNot(_.typ.isSigned) // sadly, tracking loses signedness
      .map(_.name.stripPrefix(localPrefix))
      .filterNot(_.contains("."))
      .filterNot(_.contains("$"))
      .filterNot { vname =>
        val prefix = vname + "."
        usedIdentifiers.exists(_.startsWith(prefix))
      }.toSet
  } else Set() // TODO
  if (ctx.log.traceEnabled && trackableVars.nonEmpty) {
    ctx.log.trace("Tracking local variables: " + trackableVars.mkString(", "))
  }
  protected val reentrantVars: Set[String] = trackableVars.filter(v => env.get[Variable](v) match {
    case _: StackVariable => true
    case v:UninitializedMemoryVariable if v.alloc == VariableAllocationMethod.Auto => ctx.options.flag(CompilationFlag.DangerousOptimizations)
    case _ => false
  })
  protected val nonreentrantVars: Set[String] = trackableVars -- reentrantVars

  protected val optimizeStdlib: Boolean = ctx.options.flag(CompilationFlag.OptimizeStdlib)

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
    // stdlib:
    if (optimizeStdlib) {
      stmt match {
        case ExpressionStatement(FunctionCallExpression("putstrz", List(TextLiteralExpression(text)))) =>
          text.lastOption match {
            case Some(LiteralExpression(0, _)) =>
              text.size match {
                case 1 =>
                  ctx.log.debug("Removing putstrz with empty argument", stmt.position)
                  return EmptyStatement(Nil) -> currentVarValues
                case 2 =>
                  ctx.log.debug("Replacing putstrz with putchar", stmt.position)
                  return ExpressionStatement(FunctionCallExpression("putchar", List(text.head))) -> currentVarValues
                case 3 =>
                  if (ctx.options.platform.cpuFamily == CpuFamily.M6502) {
                    ctx.log.debug("Replacing putstrz with putchar", stmt.position)
                    return IfStatement(FunctionCallExpression("==", List(LiteralExpression(1, 1), LiteralExpression(1, 1))), List(
                      ExpressionStatement(FunctionCallExpression("putchar", List(text.head))),
                      ExpressionStatement(FunctionCallExpression("putchar", List(text(1))))
                    ), Nil) -> currentVarValues
                  }
                case _ =>
              }
          }
        case _ =>
      }
    }
    // generic warnings:
    stmt match {
      case ExpressionStatement(expr@FunctionCallExpression("strzlen" | "putstrz" | "strzcmp" | "strzcopy", params)) =>
        for (param <- params) checkIfNullTerminated(stmt, param)
      case ExpressionStatement(VariableExpression(v)) =>
        val volatile = ctx.env.maybeGet[ThingInMemory](v).fold(false)(_.isVolatile)
        if (!volatile) ctx.log.warn("Pointless expression.", stmt.position)
      case ExpressionStatement(LiteralExpression(_, _)) =>
        ctx.log.warn("Pointless expression.", stmt.position)
      case _ =>
    }
    stmt match {
      case Assignment(ve@VariableExpression(v), arg) if trackableVars(v) =>
        cv = search(arg, cv)
        Assignment(ve, optimizeExpr(arg, cv)).pos(pos) -> (env.eval(arg, currentVarValues) match {
          case Some(c) => cv + (v -> c)
          case None => cv - v
        })
      case Assignment(ve, arg) =>
        cv = search(arg, cv)
        cv = search(ve, cv)
        Assignment(ve, optimizeExpr(arg, cv)).pos(pos) -> cv
      case ExpressionStatement(expr@FunctionCallExpression("+=", List(VariableExpression(v), arg)))
        if currentVarValues.contains(v) =>
        cv = search(arg, cv)
        ExpressionStatement(optimizeExpr(expr, cv - v)).pos(pos) -> (env.eval(expr, currentVarValues) match {
          case Some(c) => if (cv.contains(v)) cv + (v -> (cv(v) + c)) else cv
          case None => cv - v
        })
      case ExpressionStatement(expr@FunctionCallExpression(op, List(VariableExpression(v), arg)))
        if op.endsWith("=") && op != ">=" && op != "<=" && op != ":=" =>
        cv = search(arg, cv)
        ExpressionStatement(optimizeExpr(expr, cv - v)).pos(pos) -> (cv - v)
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
        val c = optimizeExpr(cond, Map())
        val (b, _) = optimizeStmts(body, Map())
        val (i, _) = optimizeStmts(inc, Map())
        WhileStatement(c, b, i, labels).pos(pos) -> Map()
      case DoWhileStatement(body, inc, cond, labels) =>
        val c = optimizeExpr(cond, Map())
        val (b, _) = optimizeStmts(body, Map())
        val (i, _) = optimizeStmts(inc, Map())
        DoWhileStatement(b, i, c, labels).pos(pos) -> Map()
      case f@ForEachStatement(v, arr, body) =>
        for (a <- arr.right.getOrElse(Nil)) cv = search(a, cv)
        val a = arr.map(_.map(optimizeExpr(_, Map())))
        val (b, _) = optimizeStmts(body, Map())
        ForEachStatement(v, a, b).pos(pos) -> Map()
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

  private def checkIfNullTerminated(stmt: ExecutableStatement, param: Expression): Unit = {
    param match {
      case TextLiteralExpression(ch) =>
        ch.last match {
          case LiteralExpression(0, _) => //ok
          case _ => ctx.log.warn("Passing a non-null-terminated string to a function that expects a null-terminated string.", stmt.position)
        }
      case _ =>
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

  def genName(characters: List[Expression]): String = {
    "textliteral$" ++ characters.flatMap{
      case LiteralExpression(n, _) =>
        f"$n%02x"
      case _ => ???
    }
  }

  def optimizeExpr(expr: Expression, currentVarValues: VV): Expression = {
    val pos = expr.position
    // stdlib:
    if (optimizeStdlib) {
      expr match {
        case FunctionCallExpression("strzlen", List(TextLiteralExpression(text))) =>
          text.lastOption match {
            case Some(LiteralExpression(0, _)) if text.size <= 256 =>
              ctx.log.debug("Replacing strzlen with constant argument", expr.position)
              return LiteralExpression(text.size - 1, 1)
            case _ =>
          }
        case _ =>
      }
    }
    // generic warnings:
    expr match {
      case FunctionCallExpression("*" | "*=", params) =>
        if (params.exists {
          case LiteralExpression(0, _) => true
          case _ => false
        }) ctx.log.warn("Multiplication by zero.", params.head.position)
      case FunctionCallExpression("<<" | ">>" | "<<'" | "<<=" | ">>=" | "<<'=" | ">>>>", List(lhs@_, LiteralExpression(0, _))) =>
        ctx.log.warn("Shift by zero.", lhs.position)
      case _ =>
    }
    expr match {
      case FunctionCallExpression("->", List(handle, VariableExpression(field))) =>
        expr
      case FunctionCallExpression("->", List(handle, FunctionCallExpression(method, params))) =>
        expr
      case TextLiteralExpression(characters) =>
        val name = genName(characters)
        if (ctx.env.maybeGet[Thing](name).isEmpty) {
          ctx.env.root.registerArray(ArrayDeclarationStatement(name, None, None, "byte", None, Some(LiteralContents(characters)), None).pos(pos), ctx.options)
        }
        VariableExpression(name).pos(pos)
      case VariableExpression(v) if currentVarValues.contains(v) =>
        val constant = currentVarValues(v)
        ctx.log.debug(s"Using node flow to replace $v with $constant", pos)
        GeneratedConstantExpression(constant, getExpressionType(ctx, expr)).pos(pos)
      case FunctionCallExpression(t1, List(FunctionCallExpression(t2, List(arg))))
        if optimize && pointlessDoubleCast(t1, t2, arg) =>
        ctx.log.debug(s"Pointless double cast $t1($t2(...))", pos)
        optimizeExpr(FunctionCallExpression(t1, List(arg)).pos(pos), currentVarValues)
      case FunctionCallExpression(t1, List(arg))
        if optimize && pointlessCast(t1, arg) =>
        ctx.log.debug(s"Pointless cast $t1(...)", pos)
        optimizeExpr(arg, currentVarValues)
      case FunctionCallExpression("nonet", args) =>
        // Eliminating variables may eliminate carry
        FunctionCallExpression("nonet", args.map(arg => optimizeExpr(arg, Map()))).pos(pos)
      case FunctionCallExpression(name, args) =>
        FunctionCallExpression(name, args.map(arg => optimizeExpr(arg, currentVarValues))).pos(pos)
      case SumExpression(expressions, decimal) =>
        // don't collapse additions, let the later stages deal with it
        // expecially important when inside a nonet operation
        SumExpression(expressions.map{case (minus, arg) => minus -> optimizeExpr(arg, currentVarValues)}, decimal)
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
    "not", "hi", "lo", "nonet", "sizeof"
  )
}