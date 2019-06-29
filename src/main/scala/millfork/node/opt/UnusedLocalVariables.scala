package millfork.node.opt

import millfork.CompilationOptions
import millfork.env._
import millfork.error.{ConsoleLogger, Logger}
import millfork.node._

/**
  * @author Karol Stasiak
  */
object UnusedLocalVariables extends NodeOptimization {

  override def optimize(nodes: List[Node], options: CompilationOptions): List[Node] = nodes match {
    case (x: FunctionDeclarationStatement) :: xs =>
      x.copy(statements = x.statements.map(stmt => optimizeVariables(options.log, stmt))) :: optimize(xs, options)
    case x :: xs =>
      x :: optimize(xs, options)
    case Nil =>
      Nil
  }

  def getAllLocalVariables(statements: List[Statement]): List[String] = statements.flatMap {
    case v: VariableDeclarationStatement => List(v.name)
    case v: ArrayDeclarationStatement => List(v.name)
    case x: IfStatement => getAllLocalVariables(x.thenBranch) ++ getAllLocalVariables(x.elseBranch)
    case x: WhileStatement => getAllLocalVariables(x.body)
    case x: DoWhileStatement => getAllLocalVariables(x.body)
    case _ => Nil
  }

  def getAllReadVariables(c: Constant): List[String] = c match {
    case SubbyteConstant(cc, _) => getAllReadVariables(cc)
    case CompoundConstant(_, l, r) => getAllReadVariables(l) ++ getAllReadVariables(r)
    case MemoryAddressConstant(th) => List(extractThingName(th.name))
    case _ => Nil
  }

  def getAllReadVariables(expressions: List[Node]): List[String] = expressions.flatMap {
    case s: VariableExpression => List(extractThingName(s.name))
    case s: LiteralExpression => Nil
    case HalfWordExpression(param, _) => getAllReadVariables(param :: Nil)
    case SumExpression(xs, _) => getAllReadVariables(xs.map(_._2))
    case FunctionCallExpression(_, xs) => getAllReadVariables(xs)
    case IndexedExpression(arr, index) => arr :: getAllReadVariables(List(index))
    case DerefExpression(inner, _, _) => getAllReadVariables(List(inner))
    case DerefDebuggingExpression(inner, _) => getAllReadVariables(List(inner))
    case IndirectFieldExpression(inner, firstIndices, fields) => getAllReadVariables(List(inner) ++ firstIndices ++ fields.flatMap(_._2))
    case SeparateBytesExpression(h, l) => getAllReadVariables(List(h, l))
    case _ => Nil
  }

  def getAllReadVariables(expression: (String, Expression)): List[String] = {
    if (expression._2.isPure) getAllReadVariables(List(expression._2)).filter(_ != expression._1)
    else getAllReadVariables(List(expression._2))
  }

  def optimizeVariables(log: Logger, statements: List[Statement]): List[Statement] = {
    val allLocals = getAllLocalVariables(statements)
    val allRead = statements.flatMap {
          case Assignment(VariableExpression(v), expression) => List(extractThingName(v) -> expression)
          case ExpressionStatement(FunctionCallExpression(op, VariableExpression(_) :: params)) if op.endsWith("=") => params.map("```" -> _)
          case x => x.getAllExpressions.map("```" -> _)
        }.flatMap(getAllReadVariables(_)).toSet
    val localsToRemove = allLocals.filterNot(allRead).toSet
    if (localsToRemove.nonEmpty) {
      log.debug("Removing unused local variables: " + localsToRemove.mkString(", "))
    }
    removeVariables(statements, localsToRemove)
  }

  def removeVariables(statements: List[Statement], localsToRemove: Set[String]): List[Statement] = if (localsToRemove.isEmpty) statements else statements.flatMap {
    case s: VariableDeclarationStatement =>
      if (localsToRemove(s.name)) None else Some(s)
    case s: ArrayDeclarationStatement =>
      if (localsToRemove(s.name)) None else Some(s)
    case s@ExpressionStatement(FunctionCallExpression(op, VariableExpression(n) :: params)) if op.endsWith("=") =>
      if (localsToRemove(n)) {
        params.flatMap {
          case VariableExpression(_) => None
          case LiteralExpression(_, _) => None
          case x => Some(ExpressionStatement(x).pos(s.position))
        }
      } else Some(s)
    case s@Assignment(VariableExpression(n), VariableExpression(_)) =>
      if (localsToRemove(n)) Nil else Some(s)
    case s@Assignment(VariableExpression(n), LiteralExpression(_, _)) =>
      if (localsToRemove(n)) Nil else Some(s)
    case s@Assignment(VariableExpression(n), expr) =>
      if (localsToRemove(n)) Some(ExpressionStatement(expr).pos(s.position)) else Some(s)
    case s@Assignment(SeparateBytesExpression(he@VariableExpression(h), le@VariableExpression(l)), expr) =>
      if (localsToRemove(h)) {
        if (localsToRemove(l))
          Some(ExpressionStatement(expr).pos(s.position))
        else
          Some(Assignment(
            SeparateBytesExpression(
              BlackHoleExpression,
              VariableExpression(l).pos(le.position)
            ).pos(he.position),
            expr
          ).pos(s.position))
      } else {
        if (localsToRemove(l))
          Some(Assignment(
            SeparateBytesExpression(
              VariableExpression(h).pos(he.position),
              BlackHoleExpression
            ).pos(he.position),
            expr
          ).pos(s.position))
        else
          Some(s)
      }
    case s@Assignment(SeparateBytesExpression(h, VariableExpression(l)), expr) =>
      if (localsToRemove(l)) Some(Assignment(
        SeparateBytesExpression(h, BlackHoleExpression).pos(h.position),
        expr
      ).pos(s.position))
      else Some(s)
    case s@Assignment(SeparateBytesExpression(he@VariableExpression(h), l), expr) =>
      if (localsToRemove(h)) Some(Assignment(
        SeparateBytesExpression(BlackHoleExpression, l).pos(he.position), expr
      ).pos(s.position))
      else Some(s)
    case s: IfStatement =>
      Some(s.copy(
        thenBranch = removeVariables(s.thenBranch, localsToRemove).asInstanceOf[List[ExecutableStatement]],
        elseBranch = removeVariables(s.elseBranch, localsToRemove).asInstanceOf[List[ExecutableStatement]]).pos(s.position))
    case s: WhileStatement =>
      Some(s.copy(
        body = removeVariables(s.body, localsToRemove).asInstanceOf[List[ExecutableStatement]]).pos(s.position))
    case s: DoWhileStatement =>
      Some(s.copy(
        body = removeVariables(s.body, localsToRemove).asInstanceOf[List[ExecutableStatement]]).pos(s.position))
    case s => Some(s)
  }

}
