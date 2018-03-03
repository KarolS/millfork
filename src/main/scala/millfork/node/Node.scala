package millfork.node

import millfork.assembly.{AddrMode, Opcode}
import millfork.env.{Label, ParamPassingConvention}

case class Position(filename: String, line: Int, column: Int, cursor: Int)

sealed trait Node {
  var position: Option[Position] = None
}

object Node {
  implicit class NodeOps[N<:Node](val node: N) extends AnyVal {
    def pos(position: Position): N = {
      node.position = Some(position)
      node
    }
  }
}

sealed trait Expression extends Node {
  def replaceVariable(variable: String, actualParam: Expression): Expression
}

case class LiteralExpression(value: Long, requiredSize: Int) extends Expression {
  override def replaceVariable(variable: String, actualParam: Expression): Expression = this
}

case class BooleanLiteralExpression(value: Boolean) extends Expression {
  override def replaceVariable(variable: String, actualParam: Expression): Expression = this
}

sealed trait LhsExpression extends Expression

case object BlackHoleExpression extends LhsExpression {
  override def replaceVariable(variable: String, actualParam: Expression): LhsExpression = this
}

case class SeparateBytesExpression(hi: Expression, lo: Expression) extends LhsExpression {
  def replaceVariable(variable: String, actualParam: Expression): Expression =
    SeparateBytesExpression(
      hi.replaceVariable(variable, actualParam),
      lo.replaceVariable(variable, actualParam))
}

case class SumExpression(expressions: List[(Boolean, Expression)], decimal: Boolean) extends Expression {
  override def replaceVariable(variable: String, actualParam: Expression): Expression =
    SumExpression(expressions.map { case (n, e) => n -> e.replaceVariable(variable, actualParam) }, decimal)
}

case class FunctionCallExpression(functionName: String, expressions: List[Expression]) extends Expression {
  override def replaceVariable(variable: String, actualParam: Expression): Expression =
    FunctionCallExpression(functionName, expressions.map {
      _.replaceVariable(variable, actualParam)
    })
}

case class HalfWordExpression(expression: Expression, hiByte: Boolean) extends Expression {
  override def replaceVariable(variable: String, actualParam: Expression): Expression =
    HalfWordExpression(expression.replaceVariable(variable, actualParam), hiByte)
}

object Register extends Enumeration {
  val A, X, Y, AX, AY, YA, XA, XY, YX, AW = Value
}

//case class Indexing(child: Expression, register: Register.Value) extends Expression

case class VariableExpression(name: String) extends LhsExpression {
  override def replaceVariable(variable: String, actualParam: Expression): Expression =
    if (name == variable) actualParam else this
}

case class IndexedExpression(name: String, index: Expression) extends LhsExpression {
  override def replaceVariable(variable: String, actualParam: Expression): Expression =
    if (name == variable) {
      actualParam match {
        case VariableExpression(actualVariable) => IndexedExpression(actualVariable, index.replaceVariable(variable, actualParam))
        case _ => ??? // TODO
      }
    } else IndexedExpression(name, index.replaceVariable(variable, actualParam))
}

sealed trait Statement extends Node {
  def getAllExpressions: List[Expression]
}

sealed trait DeclarationStatement extends Statement

case class TypeDefinitionStatement(name: String, parent: String) extends DeclarationStatement {
  override def getAllExpressions: List[Expression] = Nil
}

case class VariableDeclarationStatement(name: String,
                                        typ: String,
                                        global: Boolean,
                                        stack: Boolean,
                                        constant: Boolean,
                                        volatile: Boolean,
                                        register: Boolean,
                                        initialValue: Option[Expression],
                                        address: Option[Expression]) extends DeclarationStatement {
  override def getAllExpressions: List[Expression] = List(initialValue, address).flatten
}

case class ArrayDeclarationStatement(name: String,
                                     length: Option[Expression],
                                     address: Option[Expression],
                                     elements: Option[List[Expression]]) extends DeclarationStatement {
  override def getAllExpressions: List[Expression] = List(length, address).flatten ++ elements.getOrElse(Nil)
}

case class ParameterDeclaration(typ: String,
                                assemblyParamPassingConvention: ParamPassingConvention) extends Node

case class ImportStatement(filename: String) extends DeclarationStatement {
  override def getAllExpressions: List[Expression] = Nil
}

case class FunctionDeclarationStatement(name: String,
                                        resultType: String,
                                        params: List[ParameterDeclaration],
                                        address: Option[Expression],
                                        statements: Option[List[Statement]],
                                        isMacro: Boolean,
                                        inlinable: Option[Boolean],
                                        assembly: Boolean,
                                        interrupt: Boolean,
                                        reentrant: Boolean) extends DeclarationStatement {
  override def getAllExpressions: List[Expression] = address.toList ++ statements.getOrElse(Nil).flatMap(_.getAllExpressions)
}

sealed trait ExecutableStatement extends Statement

case class ExpressionStatement(expression: Expression) extends ExecutableStatement {
  override def getAllExpressions: List[Expression] = List(expression)
}

case class ReturnStatement(value: Option[Expression]) extends ExecutableStatement {
  override def getAllExpressions: List[Expression] = value.toList
}

trait ReturnDispatchLabel extends Node {
  def getAllExpressions: List[Expression]
}

case class DefaultReturnDispatchLabel(start: Option[Expression], end: Option[Expression]) extends ReturnDispatchLabel {
  def getAllExpressions: List[Expression] = List(start, end).flatten
}

case class StandardReturnDispatchLabel(labels:List[Expression]) extends ReturnDispatchLabel {
  def getAllExpressions: List[Expression] = labels
}

case class ReturnDispatchBranch(label: ReturnDispatchLabel, function: Expression, params: List[Expression]) extends Node {
  def getAllExpressions: List[Expression] = label.getAllExpressions ++ params
}

case class ReturnDispatchStatement(indexer: Expression, params: List[LhsExpression], branches: List[ReturnDispatchBranch]) extends ExecutableStatement {
  override def getAllExpressions: List[Expression] = indexer :: params ++ branches.flatMap(_.getAllExpressions)
}

case class Assignment(destination: LhsExpression, source: Expression) extends ExecutableStatement {
  override def getAllExpressions: List[Expression] = List(destination, source)
}

case class LabelStatement(label: Label) extends ExecutableStatement {
  override def getAllExpressions: List[Expression] = Nil
}

case class AssemblyStatement(opcode: Opcode.Value, addrMode: AddrMode.Value, expression: Expression, elidable: Boolean) extends ExecutableStatement {
  override def getAllExpressions: List[Expression] = List(expression)
}

case class IfStatement(condition: Expression, thenBranch: List[ExecutableStatement], elseBranch: List[ExecutableStatement]) extends ExecutableStatement {
  override def getAllExpressions: List[Expression] = condition :: (thenBranch ++ elseBranch).flatMap(_.getAllExpressions)
}

case class WhileStatement(condition: Expression, body: List[ExecutableStatement], increment: List[ExecutableStatement], labels: Set[String] = Set("", "while")) extends ExecutableStatement {
  override def getAllExpressions: List[Expression] = condition :: body.flatMap(_.getAllExpressions)
}

object ForDirection extends Enumeration {
  val To, Until, DownTo, ParallelTo, ParallelUntil = Value
}

case class ForStatement(variable: String, start: Expression, end: Expression, direction: ForDirection.Value, body: List[ExecutableStatement]) extends ExecutableStatement {
  override def getAllExpressions: List[Expression] = VariableExpression(variable) :: start :: end :: body.flatMap(_.getAllExpressions)
}

case class DoWhileStatement(body: List[ExecutableStatement], increment: List[ExecutableStatement], condition: Expression, labels: Set[String] = Set("", "do")) extends ExecutableStatement {
  override def getAllExpressions: List[Expression] = condition :: body.flatMap(_.getAllExpressions)
}

case class BlockStatement(body: List[ExecutableStatement]) extends ExecutableStatement {
  override def getAllExpressions: List[Expression] = body.flatMap(_.getAllExpressions)
}

case class BreakStatement(label: String) extends ExecutableStatement {
  override def getAllExpressions: List[Expression] = Nil
}

case class ContinueStatement(label: String) extends ExecutableStatement {
  override def getAllExpressions: List[Expression] = Nil
}

object AssemblyStatement {
  def implied(opcode: Opcode.Value, elidable: Boolean) = AssemblyStatement(opcode, AddrMode.Implied, LiteralExpression(0, 1), elidable)

  def nonexistent(opcode: Opcode.Value) = AssemblyStatement(opcode, AddrMode.DoesNotExist, LiteralExpression(0, 1), elidable = true)
}