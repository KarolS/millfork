package millfork.node.opt

import millfork.CompilationOptions
import millfork.assembly.AssemblyLine
import millfork.env._
import millfork.error.ErrorReporting
import millfork.node._

/**
  * @author Karol Stasiak
  */
object UnusedLocalVariables extends NodeOptimization {

  override def optimize(nodes: List[Node], options: CompilationOptions): List[Node] = nodes match {
    case (x: FunctionDeclarationStatement) :: xs =>
      x.copy(statements = x.statements.map(optimizeVariables)) :: optimize(xs, options)
    case x :: xs =>
      x :: optimize(xs, options)
    case Nil =>
      Nil
  }

  def getAllLocalVariables(statements: List[Statement]): List[String] = statements.flatMap {
    case v: VariableDeclarationStatement => List(v.name)
    case x: IfStatement => getAllLocalVariables(x.thenBranch) ++ getAllLocalVariables(x.elseBranch)
    case x: WhileStatement => getAllLocalVariables(x.body)
    case x: DoWhileStatement => getAllLocalVariables(x.body)
    case _ => Nil
  }

  def getAllReadVariables(c: Constant): List[String] = c match {
    case SubbyteConstant(cc, _) => getAllReadVariables(cc)
    case CompoundConstant(_, l, r) => getAllReadVariables(l) ++ getAllReadVariables(r)
    case MemoryAddressConstant(th) => List(
      th.name,
      th.name.stripSuffix(".addr"),
      th.name.stripSuffix(".hi"),
      th.name.stripSuffix(".lo"),
      th.name.stripSuffix(".addr.lo"),
      th.name.stripSuffix(".addr.hi"))
    case _ => Nil
  }

  def getAllReadVariables(expressions: List[Node]): List[String] = expressions.flatMap {
    case s: VariableExpression => List(
          s.name,
          s.name.stripSuffix(".addr"),
          s.name.stripSuffix(".hi"),
          s.name.stripSuffix(".lo"),
          s.name.stripSuffix(".addr.lo"),
          s.name.stripSuffix(".addr.hi"))
    case s: LiteralExpression => Nil
    case HalfWordExpression(param, _) => getAllReadVariables(param :: Nil)
    case SumExpression(xs, _) => getAllReadVariables(xs.map(_._2))
    case FunctionCallExpression(_, xs) => getAllReadVariables(xs)
    case IndexedExpression(arr, index) => arr :: getAllReadVariables(List(index))
    case SeparateBytesExpression(h, l) => getAllReadVariables(List(h, l))
    case _ => Nil
  }


  def optimizeVariables(statements: List[Statement]): List[Statement] = {
    val allLocals = getAllLocalVariables(statements)
    val allRead = getAllReadVariables(statements.flatMap {
      case Assignment(VariableExpression(_), expression) => List(expression)
      case ExpressionStatement(FunctionCallExpression(op, VariableExpression(_) :: params)) if op.endsWith("=") => params
      case x => x.getAllExpressions
    }).toSet
    val localsToRemove = allLocals.filterNot(allRead).toSet
    if (localsToRemove.nonEmpty) {
      ErrorReporting.debug("Removing unused local variables: " + localsToRemove.mkString(", "))
    }
    removeVariables(statements, localsToRemove)
  }

  def removeVariables(statements: List[Statement], localsToRemove: Set[String]): List[Statement] = if (localsToRemove.isEmpty) statements else statements.flatMap {
    case s: VariableDeclarationStatement =>
      if (localsToRemove(s.name)) None else Some(s)
    case s@ExpressionStatement(FunctionCallExpression(op, VariableExpression(n) :: params)) if op.endsWith("=") =>
      if (localsToRemove(n)) {
        params.flatMap {
          case VariableExpression(_) => None
          case LiteralExpression(_, _) => None
          case x => Some(ExpressionStatement(x))
        }
      } else Some(s)
    case s@Assignment(VariableExpression(n), VariableExpression(_)) =>
      if (localsToRemove(n)) Nil else Some(s)
    case s@Assignment(VariableExpression(n), LiteralExpression(_, _)) =>
      if (localsToRemove(n)) Nil else Some(s)
    case s@Assignment(VariableExpression(n), expr) =>
      if (localsToRemove(n)) Some(ExpressionStatement(expr)) else Some(s)
    case s@Assignment(SeparateBytesExpression(VariableExpression(h), VariableExpression(l)), expr) =>
      if (localsToRemove(h)) {
        if (localsToRemove(l))
          Some(ExpressionStatement(expr))
        else
          Some(Assignment(SeparateBytesExpression(BlackHoleExpression, VariableExpression(l)), expr))
      } else {
        if (localsToRemove(l))
          Some(Assignment(SeparateBytesExpression(VariableExpression(h), BlackHoleExpression), expr))
        else
          Some(s)
      }
    case s@Assignment(SeparateBytesExpression(h, VariableExpression(l)), expr) =>
      if (localsToRemove(l)) Some(Assignment(SeparateBytesExpression(h, BlackHoleExpression), expr))
      else Some(s)
    case s@Assignment(SeparateBytesExpression(VariableExpression(h), l), expr) =>
      if (localsToRemove(h)) Some(Assignment(SeparateBytesExpression(BlackHoleExpression, l), expr))
      else Some(s)
    case s: IfStatement =>
      Some(s.copy(
        thenBranch = removeVariables(s.thenBranch, localsToRemove).asInstanceOf[List[ExecutableStatement]],
        elseBranch = removeVariables(s.elseBranch, localsToRemove).asInstanceOf[List[ExecutableStatement]]))
    case s: WhileStatement =>
      Some(s.copy(
        body = removeVariables(s.body, localsToRemove).asInstanceOf[List[ExecutableStatement]]))
    case s: DoWhileStatement =>
      Some(s.copy(
        body = removeVariables(s.body, localsToRemove).asInstanceOf[List[ExecutableStatement]]))
    case s => Some(s)
  }

}
