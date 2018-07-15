package millfork.node

import millfork.assembly.mos.{AddrMode, Opcode}
import millfork.assembly.z80.{ZOpcode, ZRegisters}
import millfork.env.{Constant, ParamPassingConvention}

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
  def containsVariable(variable: String): Boolean
  def isPure: Boolean
}

case class ConstantArrayElementExpression(constant: Constant) extends Expression {
  override def replaceVariable(variable: String, actualParam: Expression): Expression = this
  override def containsVariable(variable: String): Boolean = false
  override def isPure: Boolean = true
}

case class LiteralExpression(value: Long, requiredSize: Int) extends Expression {
  override def replaceVariable(variable: String, actualParam: Expression): Expression = this
  override def containsVariable(variable: String): Boolean = false
  override def isPure: Boolean = true
}

case class BooleanLiteralExpression(value: Boolean) extends Expression {
  override def replaceVariable(variable: String, actualParam: Expression): Expression = this
  override def containsVariable(variable: String): Boolean = false
  override def isPure: Boolean = true
}

sealed trait LhsExpression extends Expression

case object BlackHoleExpression extends LhsExpression {
  override def replaceVariable(variable: String, actualParam: Expression): LhsExpression = this
  override def containsVariable(variable: String): Boolean = false
  override def isPure: Boolean = true
}

case class SeparateBytesExpression(hi: Expression, lo: Expression) extends LhsExpression {
  def replaceVariable(variable: String, actualParam: Expression): Expression =
    SeparateBytesExpression(
      hi.replaceVariable(variable, actualParam),
      lo.replaceVariable(variable, actualParam))
  override def containsVariable(variable: String): Boolean = hi.containsVariable(variable) || lo.containsVariable(variable)
  override def isPure: Boolean = hi.isPure && lo.isPure
}

case class SumExpression(expressions: List[(Boolean, Expression)], decimal: Boolean) extends Expression {
  override def replaceVariable(variable: String, actualParam: Expression): Expression =
    SumExpression(expressions.map { case (n, e) => n -> e.replaceVariable(variable, actualParam) }, decimal)
  override def containsVariable(variable: String): Boolean = expressions.exists(_._2.containsVariable(variable))
  override def isPure: Boolean = expressions.forall(_._2.isPure)
}

case class FunctionCallExpression(functionName: String, expressions: List[Expression]) extends Expression {
  override def replaceVariable(variable: String, actualParam: Expression): Expression =
    FunctionCallExpression(functionName, expressions.map {
      _.replaceVariable(variable, actualParam)
    })
  override def containsVariable(variable: String): Boolean = expressions.exists(_.containsVariable(variable))
  override def isPure: Boolean = false // TODO
}

case class HalfWordExpression(expression: Expression, hiByte: Boolean) extends Expression {
  override def replaceVariable(variable: String, actualParam: Expression): Expression =
    HalfWordExpression(expression.replaceVariable(variable, actualParam), hiByte)
  override def containsVariable(variable: String): Boolean = expression.containsVariable(variable)
  override def isPure: Boolean = expression.isPure
}

sealed class NiceFunctionProperty(override val toString: String)

object NiceFunctionProperty {
  case object DoesntReadMemory extends NiceFunctionProperty("MR")
  case object DoesntWriteMemory extends NiceFunctionProperty("MW")
}

object MosNiceFunctionProperty {
  case object DoesntChangeA extends NiceFunctionProperty("A")
  case object DoesntChangeX extends NiceFunctionProperty("X")
  case object DoesntChangeY extends NiceFunctionProperty("Y")
  case object DoesntChangeIZ extends NiceFunctionProperty("Z")
  case object DoesntChangeAH extends NiceFunctionProperty("AH")
  case object DoesntChangeC extends NiceFunctionProperty("C")
  case object DoesntConcernD extends NiceFunctionProperty("D")
  case object DoesntChangeZpRegister extends NiceFunctionProperty("reg")
}

object Z80NiceFunctionProperty {
  case object DoesntChangeBC extends NiceFunctionProperty("BC")
  case object DoesntChangeDE extends NiceFunctionProperty("DE")
  case object DoesntChangeHL extends NiceFunctionProperty("HL")
  case object DoesntChangeIY extends NiceFunctionProperty("IY")
}

object MosRegister extends Enumeration {
  val A, X, Y, AX, AY, YA, XA, XY, YX, AW = Value
}

object ZRegister extends Enumeration {

  val A, B, C, D, E, H, L, AF, BC, HL, DE, SP, IXH, IXL, IYH, IYL, IX, IY, R, I, MEM_HL, MEM_BC, MEM_DE, MEM_IX_D, MEM_IY_D, MEM_ABS_8, MEM_ABS_16, IMM_8, IMM_16 = Value

  def size(reg: Value): Int = reg match {
    case AF | BC | DE | HL | IX | IY | IMM_16 => 2
    case A | B | C | D | E | H | L | IXH | IXL | IYH | IYL | R | I | IMM_8 => 1
  }

  def matchingImm(target: ZRegister.Value): ZRegister.Value = size(target) match {
    case 1 => IMM_8
    case 2 => IMM_16
  }

  def matchingMemAbs(target: ZRegister.Value): ZRegister.Value = size(target) match {
    case 1 => MEM_ABS_8
    case 2 => MEM_ABS_16
  }
}

//case class Indexing(child: Expression, register: Register.Value) extends Expression

case class VariableExpression(name: String) extends LhsExpression {
  override def replaceVariable(variable: String, actualParam: Expression): Expression =
    if (name == variable) actualParam else this
  override def containsVariable(variable: String): Boolean = name == variable
  override def isPure: Boolean = true
}

case class IndexedExpression(name: String, index: Expression) extends LhsExpression {
  override def replaceVariable(variable: String, actualParam: Expression): Expression =
    if (name == variable) {
      actualParam match {
        case VariableExpression(actualVariable) => IndexedExpression(actualVariable, index.replaceVariable(variable, actualParam))
        case _ => ??? // TODO
      }
    } else IndexedExpression(name, index.replaceVariable(variable, actualParam))
  override def containsVariable(variable: String): Boolean = name == variable || index.containsVariable(variable)
  override def isPure: Boolean = index.isPure
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
                                        bank: Option[String],
                                        global: Boolean,
                                        stack: Boolean,
                                        constant: Boolean,
                                        volatile: Boolean,
                                        register: Boolean,
                                        initialValue: Option[Expression],
                                        address: Option[Expression]) extends DeclarationStatement {
  override def getAllExpressions: List[Expression] = List(initialValue, address).flatten
}

trait ArrayContents extends Node {
  def getAllExpressions: List[Expression]
  def replaceVariable(variableToReplace: String, expression: Expression): ArrayContents
}

case class LiteralContents(contents: List[Expression]) extends ArrayContents {
  override def getAllExpressions: List[Expression] = contents

  override def replaceVariable(variable: String, expression: Expression): ArrayContents =
    LiteralContents(contents.map(_.replaceVariable(variable, expression)))
}

case class ForLoopContents(variable: String, start: Expression, end: Expression, direction: ForDirection.Value, body: ArrayContents) extends ArrayContents {
  override def getAllExpressions: List[Expression] = start :: end :: body.getAllExpressions.map(_.replaceVariable(variable, LiteralExpression(0, 1)))

  override def replaceVariable(variableToReplace: String, expression: Expression): ArrayContents =
    if (variableToReplace == variable) this else ForLoopContents(
      variable,
      start.replaceVariable(variableToReplace, expression),
      end.replaceVariable(variableToReplace, expression),
      direction,
      body.replaceVariable(variableToReplace, expression))
}

case class CombinedContents(contents: List[ArrayContents]) extends ArrayContents {
  override def getAllExpressions: List[Expression] = contents.flatMap(_.getAllExpressions)

  override def replaceVariable(variableToReplace: String, expression: Expression): ArrayContents =
    CombinedContents(contents.map(_.replaceVariable(variableToReplace, expression)))
}

case class ProcessedContents(processor: String, values: ArrayContents) extends ArrayContents {
  override def getAllExpressions: List[Expression] = processor match {
    case "word" | "word_le" =>
      values.getAllExpressions.flatMap(expr => List(
        FunctionCallExpression("lo", List(expr)),
        FunctionCallExpression("hi", List(expr))
      ))
    case "word_be" =>
      values.getAllExpressions.flatMap(expr => List(
        FunctionCallExpression("hi", List(expr)),
        FunctionCallExpression("lo", List(expr))
      ))
  }

  override def replaceVariable(variableToReplace: String, expression: Expression): ArrayContents =
    ProcessedContents(processor, values.replaceVariable(variableToReplace, expression))
}

case class AliasDefinitionStatement(name: String, target: String) extends DeclarationStatement {
  override def getAllExpressions: List[Expression] = Nil
}

case class ArrayDeclarationStatement(name: String,
                                     bank: Option[String],
                                     length: Option[Expression],
                                     address: Option[Expression],
                                     elements: Option[ArrayContents]) extends DeclarationStatement {
  override def getAllExpressions: List[Expression] = List(length, address).flatten ++ elements.fold(List[Expression]())(_.getAllExpressions)
}

case class ParameterDeclaration(typ: String,
                                assemblyParamPassingConvention: ParamPassingConvention) extends Node

case class ImportStatement(filename: String) extends DeclarationStatement {
  override def getAllExpressions: List[Expression] = Nil
}

case class FunctionDeclarationStatement(name: String,
                                        resultType: String,
                                        params: List[ParameterDeclaration],
                                        bank: Option[String],
                                        address: Option[Expression],
                                        statements: Option[List[Statement]],
                                        isMacro: Boolean,
                                        inlinable: Option[Boolean],
                                        assembly: Boolean,
                                        interrupt: Boolean,
                                        kernalInterrupt: Boolean,
                                        reentrant: Boolean) extends DeclarationStatement {
  override def getAllExpressions: List[Expression] = address.toList ++ statements.getOrElse(Nil).flatMap(_.getAllExpressions)
}

sealed trait ExecutableStatement extends Statement

case class RawBytesStatement(contents: ArrayContents) extends ExecutableStatement {
  override def getAllExpressions: List[Expression] = contents.getAllExpressions
}

sealed trait CompoundStatement extends ExecutableStatement {
  def getChildStatements: Seq[Statement]
}

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

case class MosAssemblyStatement(opcode: Opcode.Value, addrMode: AddrMode.Value, expression: Expression, elidable: Boolean) extends ExecutableStatement {
  override def getAllExpressions: List[Expression] = List(expression)
}

case class Z80AssemblyStatement(opcode: ZOpcode.Value, registers: ZRegisters, offsetExpression: Option[Expression], expression: Expression, elidable: Boolean) extends ExecutableStatement {
  override def getAllExpressions: List[Expression] = List(expression)
}

case class IfStatement(condition: Expression, thenBranch: List[ExecutableStatement], elseBranch: List[ExecutableStatement]) extends CompoundStatement {
  override def getAllExpressions: List[Expression] = condition :: (thenBranch ++ elseBranch).flatMap(_.getAllExpressions)

  override def getChildStatements: Seq[Statement] = thenBranch ++ elseBranch
}

case class WhileStatement(condition: Expression, body: List[ExecutableStatement], increment: List[ExecutableStatement], labels: Set[String] = Set("", "while")) extends CompoundStatement {
  override def getAllExpressions: List[Expression] = condition :: body.flatMap(_.getAllExpressions)

  override def getChildStatements: Seq[Statement] = body ++ increment
}

object ForDirection extends Enumeration {
  val To, Until, DownTo, ParallelTo, ParallelUntil = Value
}

case class ForStatement(variable: String, start: Expression, end: Expression, direction: ForDirection.Value, body: List[ExecutableStatement]) extends CompoundStatement {
  override def getAllExpressions: List[Expression] = VariableExpression(variable) :: start :: end :: body.flatMap(_.getAllExpressions)

  override def getChildStatements: Seq[Statement] = body
}

case class DoWhileStatement(body: List[ExecutableStatement], increment: List[ExecutableStatement], condition: Expression, labels: Set[String] = Set("", "do")) extends CompoundStatement {
  override def getAllExpressions: List[Expression] = condition :: body.flatMap(_.getAllExpressions)

  override def getChildStatements: Seq[Statement] = body ++ increment
}

case class BreakStatement(label: String) extends ExecutableStatement {
  override def getAllExpressions: List[Expression] = Nil
}

case class ContinueStatement(label: String) extends ExecutableStatement {
  override def getAllExpressions: List[Expression] = Nil
}

object MosAssemblyStatement {
  def implied(opcode: Opcode.Value, elidable: Boolean) = MosAssemblyStatement(opcode, AddrMode.Implied, LiteralExpression(0, 1), elidable)

  def nonexistent(opcode: Opcode.Value) = MosAssemblyStatement(opcode, AddrMode.DoesNotExist, LiteralExpression(0, 1), elidable = true)
}