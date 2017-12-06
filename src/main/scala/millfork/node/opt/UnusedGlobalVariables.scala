package millfork.node.opt

import millfork.env._
import millfork.error.ErrorReporting
import millfork.node._

/**
  * @author Karol Stasiak
  */
object UnusedGlobalVariables extends NodeOptimization {

  override def optimize(nodes: List[Node]): List[Node] = {

    // TODO: volatile
    val allNonvolatileGlobalVariables = nodes.flatMap {
      case v: VariableDeclarationStatement => if (v.address.isDefined) Nil else List(v.name)
      case v: ArrayDeclarationStatement => if (v.address.isDefined) Nil else List(v.name)
      case _ => Nil
    }.toSet
    val allReadVariables = getAllReadVariables(nodes).toSet
    val unusedVariables = allNonvolatileGlobalVariables -- allReadVariables
    if (unusedVariables.nonEmpty) {
      ErrorReporting.debug("Removing unused global variables: " + unusedVariables.mkString(", "))
    }
    removeVariablesFromProgram(nodes, unusedVariables.flatMap(v => Set(v, v + ".hi", v + ".lo")))
  }

  private def removeVariablesFromProgram(nodes: List[Node], unusedVariables: Set[String]): List[Node] = {
    nodes match {
      case (x: ArrayDeclarationStatement) :: xs if unusedVariables(x.name) => removeVariablesFromProgram(xs, unusedVariables)
      case (x: VariableDeclarationStatement) :: xs if unusedVariables(x.name) => removeVariablesFromProgram(xs, unusedVariables)
      case (x: FunctionDeclarationStatement) :: xs =>
        x.copy(statements = x.statements.map(s => removeVariablesFromStatement(s, unusedVariables))) :: removeVariablesFromProgram(xs, unusedVariables)
      case x :: xs =>
        x :: removeVariablesFromProgram(xs, unusedVariables)
      case Nil =>
        Nil
    }
  }

  def getAllReadVariables(c: Constant): List[String] = c match {
    case HalfWordConstant(cc, _) => getAllReadVariables(cc)
    case SubbyteConstant(cc, _) => getAllReadVariables(cc)
    case CompoundConstant(_, l, r) => getAllReadVariables(l) ++ getAllReadVariables(r)
    case MemoryAddressConstant(th) => List(th.name.takeWhile(_ != '.'))
    case _ => Nil
  }

  def getAllReadVariables(expressions: List[Node]): List[String] = expressions.flatMap {
    case s: VariableDeclarationStatement => getAllReadVariables(s.address.toList) ++ getAllReadVariables(s.initialValue.toList)
    case s: ArrayDeclarationStatement => getAllReadVariables(s.address.toList) ++ getAllReadVariables(s.elements.getOrElse(Nil))
    case s: FunctionDeclarationStatement => getAllReadVariables(s.address.toList) ++ getAllReadVariables(s.statements.getOrElse(Nil))
    case Assignment(VariableExpression(_), expr) => getAllReadVariables(expr :: Nil)
    case ExpressionStatement(FunctionCallExpression(op, VariableExpression(_) :: params)) if op.endsWith("=") => getAllReadVariables(params)
    case s: Statement => getAllReadVariables(s.getAllExpressions)
    case s: VariableExpression => List(s.name.takeWhile(_ != '.'))
    case s: LiteralExpression => Nil
    case HalfWordExpression(param, _) => getAllReadVariables(param :: Nil)
    case SumExpression(xs, _) => getAllReadVariables(xs.map(_._2))
    case FunctionCallExpression(name, xs) => name :: getAllReadVariables(xs)
    case IndexedExpression(arr, index) => arr :: getAllReadVariables(List(index))
    case SeparateBytesExpression(h, l) => getAllReadVariables(List(h, l))
    case _ => Nil
  }

  def removeVariablesFromStatement(statements: List[Statement], globalsToRemove: Set[String]): List[Statement] = statements.flatMap {
    case s: VariableDeclarationStatement =>
      if (globalsToRemove(s.name)) None else Some(s)
    case s@ExpressionStatement(FunctionCallExpression(op, VariableExpression(n) :: params)) if op.endsWith("=") =>
      if (globalsToRemove(n)) params.map(ExpressionStatement) else Some(s)
    case s@Assignment(VariableExpression(n), expr) =>
      if (globalsToRemove(n)) Some(ExpressionStatement(expr)) else Some(s)
    case s@Assignment(SeparateBytesExpression(VariableExpression(h), VariableExpression(l)), expr) =>
      if (globalsToRemove(h)) {
        if (globalsToRemove(l))
          Some(ExpressionStatement(expr))
        else
          Some(Assignment(SeparateBytesExpression(BlackHoleExpression, VariableExpression(l)), expr))
      } else {
        if (globalsToRemove(l))
          Some(Assignment(SeparateBytesExpression(VariableExpression(h), BlackHoleExpression), expr))
        else
          Some(s)
      }
    case s@Assignment(SeparateBytesExpression(h, VariableExpression(l)), expr) =>
      if (globalsToRemove(l)) Some(Assignment(SeparateBytesExpression(h, BlackHoleExpression), expr))
      else Some(s)
    case s@Assignment(SeparateBytesExpression(VariableExpression(h), l), expr) =>
      if (globalsToRemove(h)) Some(Assignment(SeparateBytesExpression(BlackHoleExpression, l), expr))
      else Some(s)
    case s: IfStatement =>
      Some(s.copy(
        thenBranch = removeVariablesFromStatement(s.thenBranch, globalsToRemove).asInstanceOf[List[ExecutableStatement]],
        elseBranch = removeVariablesFromStatement(s.elseBranch, globalsToRemove).asInstanceOf[List[ExecutableStatement]]))
    case s: WhileStatement =>
      Some(s.copy(
        body = removeVariablesFromStatement(s.body, globalsToRemove).asInstanceOf[List[ExecutableStatement]]))
    case s: DoWhileStatement =>
      Some(s.copy(
        body = removeVariablesFromStatement(s.body, globalsToRemove).asInstanceOf[List[ExecutableStatement]]))
    case s => Some(s)
  }

}
