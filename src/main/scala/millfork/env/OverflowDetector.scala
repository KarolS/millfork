package millfork.env

import millfork.{CompilationFlag, CompilationOptions}
import millfork.compiler.{AbstractExpressionCompiler, CompilationContext}
import millfork.error.Logger
import millfork.node._

/**
  * @author Karol Stasiak
  */
class OverflowDetector(env: Environment, options: CompilationOptions) {

  def this(ctx: CompilationContext) {
    this(ctx.env, ctx.options)
  }

  private def log: Logger = options.log

  private def isWord(e: Expression): Boolean =
    AbstractExpressionCompiler.getExpressionType(env, log, e) match {
      case t: PlainType => t.size == 2
      case _ => false
    }

  private def isWord(typeName: String): Boolean =
    env.maybeGet[Thing](typeName) match {
      case Some(t: PlainType) => t.size == 2
      case _ => false
    }

  private def isWord(typ: Type): Boolean =
    typ match {
      case t: PlainType => t.size == 2
      case _ => false
    }

  private def isByte(e: Expression): Boolean =
    AbstractExpressionCompiler.getExpressionType(env, log, e) match {
      case t: PlainType => t.size == 1
      case _ => false
    }

  def warnConstantOverflow(e: Expression, op: String): Unit = {
    if (options.flag(CompilationFlag.ByteOverflowWarning)) {
      log.warn(s"Constant byte overflow. Consider wrapping one of the arguments of $op with word( )", e.position)
    }
  }

  def warnDynamicOverflow(e: Expression, op: String): Unit = {
    if (options.flag(CompilationFlag.ByteOverflowWarning)) {
      log.warn(s"Potential byte overflow. Consider wrapping one of the arguments of $op with word( )", e.position)
    }
  }

  def scanExpression(e: Expression, willBeAssignedToWord: Boolean): Unit = {
    if (willBeAssignedToWord) {
      e match {
        case FunctionCallExpression("<<", List(l, r)) =>
          if (isByte(l) && isByte(r)) {
            (env.eval(l), env.eval(r)) match {
              case (Some(NumericConstant(lc, 1)), Some(NumericConstant(rc, 1))) =>
                if (lc >= 0 && rc >= 0 && (lc << rc) > 255) {
                  warnConstantOverflow(e, "<<")
                }
              case (_, Some(NumericConstant(0, _))) =>
              case _ =>
                warnDynamicOverflow(e, "<<")
            }
          }
        case FunctionCallExpression("*", List(l, r)) =>
          if (isByte(l) && isByte(r)) {
            (env.eval(l), env.eval(r)) match {
              case (Some(NumericConstant(lc, 1)), Some(NumericConstant(rc, 1))) =>
                if (lc >= 0 && rc >= 0 && (lc * rc) > 255) {
                  warnConstantOverflow(e, "*")
                }
              case (_, Some(NumericConstant(0, _))) =>
              case (_, Some(NumericConstant(1, _))) =>
              case (Some(NumericConstant(0, _)), _) =>
              case (Some(NumericConstant(1, _)), _) =>
              case _ =>
                warnDynamicOverflow(e, "*")
            }
          }
        case FunctionCallExpression("word" | "unsigned16" | "signed16" | "pointer", List(SumExpression(expressions, _))) =>
          if (expressions.map(_._2).forall(isByte)) {

          }
        case _ =>
      }
    }
    e match {
      case SumExpression(expressions, decimal) =>
        if (willBeAssignedToWord && !decimal && isByte(e)) env.eval(e) match {
          case Some(NumericConstant(n, _)) if n < -128 || n > 255 =>
            warnConstantOverflow(e, "+")
          case _ =>
        }
        for ((_, e) <- expressions) {
          scanExpression(e, willBeAssignedToWord = willBeAssignedToWord)
        }
      case FunctionCallExpression("word" | "unsigned16" | "signed16" | "pointer", expressions) =>
        expressions.foreach(x => scanExpression(x, willBeAssignedToWord = true))
      case FunctionCallExpression("|" | "^" | "&" | "not", expressions) =>
        expressions.foreach(x => scanExpression(x, willBeAssignedToWord = false))
      case FunctionCallExpression(fname, expressions) =>
        env.maybeGet[Thing](fname) match {
          case Some(f: FunctionInMemory) if f.params.length == expressions.length =>
            for ((e, t) <- expressions zip f.params.types) {
              scanExpression(e, willBeAssignedToWord = isWord(t))
            }
          case _ =>
            for (e <- expressions) {
              scanExpression(e, willBeAssignedToWord = false)
            }
        }
      case _ =>
    }
  }

  def detectOverflow(stmt: Statement): Unit = {
    stmt match {
      case Assignment(lhs, rhs) =>
        if (isWord(lhs)) scanExpression(rhs, willBeAssignedToWord = true)
      case v: VariableDeclarationStatement =>
        v.initialValue match {
          case Some(e) => scanExpression(e, willBeAssignedToWord = isWord(v.typ))
          case _ =>
        }
      case s =>
        s.getAllExpressions.foreach(e => scanExpression(e, willBeAssignedToWord = false))
    }
  }
}
