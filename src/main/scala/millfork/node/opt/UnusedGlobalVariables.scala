package millfork.node.opt

import millfork.CompilationOptions
import millfork.env._
import millfork.error.ConsoleLogger
import millfork.node._

/**
  * @author Karol Stasiak
  */
object UnusedGlobalVariables extends NodeOptimization {

  override def optimize(nodes: List[Node], options: CompilationOptions): List[Node] = {
    val aliases = nodes.flatMap{
      case AliasDefinitionStatement(source, target, _) => Some(source -> target)
      case _ => None
    }.toMap
    // TODO: volatile
    val allNonvolatileGlobalVariables = nodes.flatMap {
      case v: VariableDeclarationStatement => if (v.address.isDefined) Nil else List(v.name)
      case v: ArrayDeclarationStatement => if (v.address.isDefined) Nil else List(v.name)
      case _ => Nil
    }.toSet
    val allReadVariables = resolveAliases(aliases, getAllReadVariables(nodes).toSet)
    val unusedVariables = allNonvolatileGlobalVariables -- allReadVariables
    if (unusedVariables.nonEmpty) {
      options.log.debug("Removing unused global variables: " + unusedVariables.mkString(", "))
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
    case SubbyteConstant(cc, _) => getAllReadVariables(cc)
    case CompoundConstant(_, l, r) => getAllReadVariables(l) ++ getAllReadVariables(r)
    case MemoryAddressConstant(th) => List(extractThingName(th.name))
    case _ => Nil
  }

  def getAllReadVariables(expressions: List[Node]): List[String] = expressions.flatMap {
    case s: VariableDeclarationStatement => getAllReadVariables(s.address.toList) ++ getAllReadVariables(s.initialValue.toList) ++ (if (s.stack) List("__sp", "__stack") else Nil)
    case s: ArrayDeclarationStatement => getAllReadVariables(s.address.toList) ++ getAllReadVariables(s.elements.toList)
    case s: ArrayContents => getAllReadVariables(s.getAllExpressions(false)) // endianness doesn't matter here at all
    case s: FunctionDeclarationStatement => getAllReadVariables(s.address.toList) ++ getAllReadVariables(s.statements.getOrElse(Nil))
    case Assignment(VariableExpression(_), expr) => getAllReadVariables(expr :: Nil)
    case ExpressionStatement(FunctionCallExpression(op, VariableExpression(_) :: params)) if op.endsWith("=") => getAllReadVariables(params)
    case s: Statement => getAllReadVariables(s.getAllExpressions)
    case s: VariableExpression => List(extractThingName(s.name))
    case s: LiteralExpression => Nil
    case HalfWordExpression(param, _) => getAllReadVariables(param :: Nil)
    case SumExpression(xs, _) => getAllReadVariables(xs.map(_._2))
    case FunctionCallExpression(name, xs) => name :: getAllReadVariables(xs)
    case IndexedExpression(arr, index) => arr :: getAllReadVariables(List(index))
    case SeparateBytesExpression(h, l) => getAllReadVariables(List(h, l))
    case IndirectFieldExpression(root, firstIndices, fields) => getAllReadVariables(List(root) ++ firstIndices ++ fields.flatMap(_._3))
    case _ => Nil
  }

  def removeVariablesFromStatement(statements: List[Statement], globalsToRemove: Set[String]): List[Statement] = statements.flatMap {
    case s: VariableDeclarationStatement =>
      if (globalsToRemove(s.name)) None else Some(s)
    case s@ExpressionStatement(FunctionCallExpression(op, VariableExpression(n) :: params)) if op.endsWith("=") =>
      if (globalsToRemove(n)) {
        params.flatMap {
          case VariableExpression(_) => None
          case LiteralExpression(_, _) => None
          case x => Some(ExpressionStatement(x).pos(s.position))
        }
      } else Some(s)
    case s@Assignment(VariableExpression(n), expr@VariableExpression(n2)) =>
      if (globalsToRemove(extractThingName(n))) {
        if (globalsToRemove(extractThingName(n))) None else Some(Assignment(BlackHoleExpression, expr).pos(s.position))
      } else Some(s)
    case s@Assignment(VariableExpression(n), LiteralExpression(_, _)) =>
      if (globalsToRemove(extractThingName(n))) Nil else Some(s)
    case s@Assignment(VariableExpression(n), expr) =>
      if (globalsToRemove(extractThingName(n))) Some(ExpressionStatement(expr).pos(s.position)) else Some(s)
    case s@Assignment(SeparateBytesExpression(he@VariableExpression(h), le@VariableExpression(l)), expr) =>
      if (globalsToRemove(extractThingName(h))) {
        if (globalsToRemove(extractThingName(l)))
          Some(ExpressionStatement(expr).pos(s.position))
        else
          Some(Assignment(SeparateBytesExpression(BlackHoleExpression, le).pos(he.position), expr).pos(s.position))
      } else {
        if (globalsToRemove(extractThingName(l)))
          Some(Assignment(SeparateBytesExpression(he, BlackHoleExpression).pos(he.position), expr).pos(s.position))
        else
          Some(s)
      }
    case s@Assignment(SeparateBytesExpression(h, le@VariableExpression(l)), expr) =>
      if (globalsToRemove(extractThingName(l))) Some(Assignment(SeparateBytesExpression(h, BlackHoleExpression).pos(h.position), expr).pos(s.position))
      else Some(s)
    case s@Assignment(SeparateBytesExpression(he@VariableExpression(h), l), expr) =>
      if (globalsToRemove(extractThingName(h))) Some(Assignment(SeparateBytesExpression(BlackHoleExpression, l).pos(he.position), expr).pos(s.position))
      else Some(s)
    case s: IfStatement =>
      Some(s.copy(
        thenBranch = removeVariablesFromStatement(s.thenBranch, globalsToRemove).asInstanceOf[List[ExecutableStatement]],
        elseBranch = removeVariablesFromStatement(s.elseBranch, globalsToRemove).asInstanceOf[List[ExecutableStatement]]).pos(s.position))
    case s: WhileStatement =>
      Some(s.copy(
        body = removeVariablesFromStatement(s.body, globalsToRemove).asInstanceOf[List[ExecutableStatement]]).pos(s.position))
    case s: DoWhileStatement =>
      Some(s.copy(
        body = removeVariablesFromStatement(s.body, globalsToRemove).asInstanceOf[List[ExecutableStatement]]).pos(s.position))
    case s => Some(s)
  }

}
