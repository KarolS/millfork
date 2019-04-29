package millfork.node

import millfork.assembly.Elidability
import millfork.assembly.mos.{AddrMode, Opcode}
import millfork.assembly.z80.{ZOpcode, ZRegisters}
import millfork.env.{Constant, ParamPassingConvention, Type}
import millfork.output.MemoryAlignment

case class Position(moduleName: String, line: Int, column: Int, cursor: Int)

sealed trait Node {
  var position: Option[Position] = None
}

object Node {
  implicit class NodeOps[N<:Node](val node: N) extends AnyVal {
    def pos(position: Position): N = {
      node.position = Some(position)
      node
    }
    def pos(position: Option[Position]): N = {
      if (position.isDefined) {
        node.position = position
      }
      node
    }
  }
}

sealed trait Expression extends Node {
  def replaceVariable(variable: String, actualParam: Expression): Expression
  def containsVariable(variable: String): Boolean
  def getPointies: Seq[String]
  def isPure: Boolean
  def getAllIdentifiers: Set[String]
  @transient var typeCache: Type = _
}

case class ConstantArrayElementExpression(constant: Constant) extends Expression {
  override def replaceVariable(variable: String, actualParam: Expression): Expression = this
  override def containsVariable(variable: String): Boolean = false
  override def getPointies: Seq[String] = Seq.empty
  override def isPure: Boolean = true
  override def getAllIdentifiers: Set[String] = Set.empty
}

case class LiteralExpression(value: Long, requiredSize: Int) extends Expression {
  override def replaceVariable(variable: String, actualParam: Expression): Expression = this
  override def containsVariable(variable: String): Boolean = false
  override def getPointies: Seq[String] = Seq.empty
  override def isPure: Boolean = true
  override def getAllIdentifiers: Set[String] = Set.empty
}

case class TextLiteralExpression(characters: List[Expression]) extends Expression {
  override def replaceVariable(variable: String, actualParam: Expression): Expression = this
  override def containsVariable(variable: String): Boolean = false
  override def getPointies: Seq[String] = Seq.empty
  override def isPure: Boolean = true
  override def getAllIdentifiers: Set[String] = Set.empty
}

case class GeneratedConstantExpression(value: Constant, typ: Type) extends Expression {
  override def replaceVariable(variable: String, actualParam: Expression): Expression = this
  override def containsVariable(variable: String): Boolean = false
  override def getPointies: Seq[String] = Seq.empty
  override def isPure: Boolean = true
  override def getAllIdentifiers: Set[String] = Set.empty
}

case class BooleanLiteralExpression(value: Boolean) extends Expression {
  override def replaceVariable(variable: String, actualParam: Expression): Expression = this
  override def containsVariable(variable: String): Boolean = false
  override def getPointies: Seq[String] = Seq.empty
  override def isPure: Boolean = true
  override def getAllIdentifiers: Set[String] = Set.empty
}

sealed trait LhsExpression extends Expression

case object BlackHoleExpression extends LhsExpression {
  override def replaceVariable(variable: String, actualParam: Expression): LhsExpression = this
  override def containsVariable(variable: String): Boolean = false
  override def getPointies: Seq[String] = Seq.empty
  override def isPure: Boolean = true
  override def getAllIdentifiers: Set[String] = Set.empty
}

case class SeparateBytesExpression(hi: Expression, lo: Expression) extends LhsExpression {
  def replaceVariable(variable: String, actualParam: Expression): Expression =
    SeparateBytesExpression(
      hi.replaceVariable(variable, actualParam),
      lo.replaceVariable(variable, actualParam)).pos(position)
  override def containsVariable(variable: String): Boolean = hi.containsVariable(variable) || lo.containsVariable(variable)
  override def getPointies: Seq[String] = hi.getPointies ++ lo.getPointies
  override def isPure: Boolean = hi.isPure && lo.isPure
  override def getAllIdentifiers: Set[String] = hi.getAllIdentifiers ++ lo.getAllIdentifiers
}

case class SumExpression(expressions: List[(Boolean, Expression)], decimal: Boolean) extends Expression {
  override def replaceVariable(variable: String, actualParam: Expression): Expression =
    SumExpression(expressions.map { case (n, e) => n -> e.replaceVariable(variable, actualParam) }, decimal).pos(position)
  override def containsVariable(variable: String): Boolean = expressions.exists(_._2.containsVariable(variable))
  override def getPointies: Seq[String] = expressions.flatMap(_._2.getPointies)
  override def isPure: Boolean = expressions.forall(_._2.isPure)
  override def getAllIdentifiers: Set[String] = expressions.map(_._2.getAllIdentifiers).fold(Set[String]())(_ ++ _)
}

case class FunctionCallExpression(functionName: String, expressions: List[Expression]) extends Expression {
  override def replaceVariable(variable: String, actualParam: Expression): Expression =
    FunctionCallExpression(functionName, expressions.map {
      _.replaceVariable(variable, actualParam)
    }).pos(position)
  override def containsVariable(variable: String): Boolean = expressions.exists(_.containsVariable(variable))
  override def getPointies: Seq[String] = expressions.flatMap(_.getPointies)
  override def isPure: Boolean = false // TODO
  override def getAllIdentifiers: Set[String] = expressions.map(_.getAllIdentifiers).fold(Set[String]())(_ ++ _) + functionName
}

case class HalfWordExpression(expression: Expression, hiByte: Boolean) extends Expression {
  override def replaceVariable(variable: String, actualParam: Expression): Expression =
    HalfWordExpression(expression.replaceVariable(variable, actualParam), hiByte).pos(position)
  override def containsVariable(variable: String): Boolean = expression.containsVariable(variable)
  override def getPointies: Seq[String] = expression.getPointies
  override def isPure: Boolean = expression.isPure
  override def getAllIdentifiers: Set[String] = expression.getAllIdentifiers
}

sealed class NiceFunctionProperty(override val toString: String)

object NiceFunctionProperty {
  case object DoesntReadMemory extends NiceFunctionProperty("MR")
  case object DoesntWriteMemory extends NiceFunctionProperty("MW")
  case object IsLeaf extends NiceFunctionProperty("LEAF")
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

  def registerSize(reg: Value): Int = reg match {
    case AF | BC | DE | HL | IX | IY | IMM_16 => 2
    case A | B | C | D | E | H | L | IXH | IXL | IYH | IYL | R | I | IMM_8 => 1
  }

  def matchingImm(target: ZRegister.Value): ZRegister.Value = registerSize(target) match {
    case 1 => IMM_8
    case 2 => IMM_16
  }

  def matchingMemAbs(target: ZRegister.Value): ZRegister.Value = registerSize(target) match {
    case 1 => MEM_ABS_8
    case 2 => MEM_ABS_16
  }
}

//case class Indexing(child: Expression, register: Register.Value) extends Expression

case class VariableExpression(name: String) extends LhsExpression {
  override def replaceVariable(variable: String, actualParam: Expression): Expression =
    if (name == variable) actualParam else this
  override def containsVariable(variable: String): Boolean = name == variable
  override def getPointies: Seq[String] = if (name.endsWith(".addr.lo")) Seq(name.stripSuffix(".addr.lo")) else Seq.empty
  override def isPure: Boolean = true
  override def getAllIdentifiers: Set[String] = Set(name)
}

case class IndexedExpression(name: String, index: Expression) extends LhsExpression {
  override def replaceVariable(variable: String, actualParam: Expression): Expression =
    if (name == variable) {
      actualParam match {
        case VariableExpression(actualVariable) =>
          IndexedExpression(actualVariable, index.replaceVariable(variable, actualParam)).pos(position)
        case _ => ??? // TODO
      }
    } else IndexedExpression(name, index.replaceVariable(variable, actualParam)).pos(position)
  override def containsVariable(variable: String): Boolean = name == variable || index.containsVariable(variable)
  override def getPointies: Seq[String] = Seq(name)
  override def isPure: Boolean = index.isPure
  override def getAllIdentifiers: Set[String] = index.getAllIdentifiers + name
}

case class IndirectFieldExpression(root: Expression, firstIndices: Seq[Expression], fields: Seq[(String, Seq[Expression])]) extends LhsExpression {
  override def replaceVariable(variable: String, actualParam: Expression): Expression =
    IndirectFieldExpression(
      root.replaceVariable(variable, actualParam),
      firstIndices.map(_.replaceVariable(variable, actualParam)),
      fields.map{case (f, i) => f -> i.map(_.replaceVariable(variable, actualParam))})

  override def containsVariable(variable: String): Boolean =
    root.containsVariable(variable) ||
      firstIndices.exists(_.containsVariable(variable)) ||
      fields.exists(_._2.exists(_.containsVariable(variable)))

  override def getPointies: Seq[String] = (root match {
    case VariableExpression(v) => List(v)
    case _ => root.getPointies
  }) ++ firstIndices.flatMap(_.getPointies) ++ fields.flatMap(_._2.flatMap(_.getPointies))

  override def isPure: Boolean = root.isPure && firstIndices.forall(_.isPure) && fields.forall(_._2.forall(_.isPure))

  override def getAllIdentifiers: Set[String] = root.getAllIdentifiers ++ firstIndices.flatMap(_.getAllIdentifiers) ++ fields.flatMap(_._2.flatMap(_.getAllIdentifiers))
}

case class DerefDebuggingExpression(inner: Expression, preferredSize: Int) extends LhsExpression {
  override def replaceVariable(variable: String, actualParam: Expression): Expression = DerefDebuggingExpression(inner.replaceVariable(variable, actualParam), preferredSize)

  override def containsVariable(variable: String): Boolean = inner.containsVariable(variable)

  override def getPointies: Seq[String] = inner match {
    case VariableExpression(v) => List(v)
    case _ => inner.getPointies
  }

  override def isPure: Boolean = inner.isPure

  override def getAllIdentifiers: Set[String] = inner.getAllIdentifiers
}

case class DerefExpression(inner: Expression, offset: Int, targetType: Type) extends LhsExpression {
  override def replaceVariable(variable: String, actualParam: Expression): Expression = DerefExpression(inner.replaceVariable(variable, actualParam), offset, targetType)

  override def containsVariable(variable: String): Boolean = inner.containsVariable(variable)

  override def getPointies: Seq[String] = inner match {
    case VariableExpression(v) => List(v)
    case _ => inner.getPointies
  }

  override def isPure: Boolean = inner.isPure

  override def getAllIdentifiers: Set[String] = inner.getAllIdentifiers
}

sealed trait Statement extends Node {
  def getAllExpressions: List[Expression]

  def getAllPointies: Seq[String] = getAllExpressions.flatMap(_.getPointies)
}

sealed trait DeclarationStatement extends Statement {
  def name: String
}

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
                                        address: Option[Expression],
                                        alignment: Option[MemoryAlignment]) extends DeclarationStatement {
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
        FunctionCallExpression("lo", List(expr)).pos(expr.position),
        FunctionCallExpression("hi", List(expr)).pos(expr.position)
      ))
    case "word_be" =>
      values.getAllExpressions.flatMap(expr => List(
        FunctionCallExpression("hi", List(expr)).pos(expr.position),
        FunctionCallExpression("lo", List(expr)).pos(expr.position)
      ))
    case "long" | "long_le" =>
      values.getAllExpressions.flatMap(expr => List(
        FunctionCallExpression("lo", List(expr)).pos(expr.position),
        FunctionCallExpression("lo", List(FunctionCallExpression(">>", List(expr, LiteralExpression(8, 1))).pos(expr.position))).pos(expr.position),
        FunctionCallExpression("lo", List(FunctionCallExpression(">>", List(expr, LiteralExpression(16, 1))).pos(expr.position))).pos(expr.position),
        FunctionCallExpression("lo", List(FunctionCallExpression(">>", List(expr, LiteralExpression(24, 1))).pos(expr.position))).pos(expr.position)
      ))
    case "long_be" =>
      values.getAllExpressions.flatMap(expr => List(
        FunctionCallExpression("lo", List(FunctionCallExpression(">>", List(expr, LiteralExpression(24, 1))).pos(expr.position))).pos(expr.position),
        FunctionCallExpression("lo", List(FunctionCallExpression(">>", List(expr, LiteralExpression(16, 1))).pos(expr.position))).pos(expr.position),
        FunctionCallExpression("lo", List(FunctionCallExpression(">>", List(expr, LiteralExpression(8, 1))).pos(expr.position))).pos(expr.position),
        FunctionCallExpression("lo", List(expr)).pos(expr.position)
      ))
    case "struct" => values.getAllExpressions // not used for emitting actual arrays
  }

  override def replaceVariable(variableToReplace: String, expression: Expression): ArrayContents =
    ProcessedContents(processor, values.replaceVariable(variableToReplace, expression))
}

case class AliasDefinitionStatement(name: String, target: String, important: Boolean) extends DeclarationStatement {
  override def getAllExpressions: List[Expression] = Nil
}

case class EnumDefinitionStatement(name: String, variants: List[(String, Option[Expression])]) extends DeclarationStatement {
  override def getAllExpressions: List[Expression] = variants.flatMap(_._2)
}

case class StructDefinitionStatement(name: String, fields: List[(String, String)]) extends DeclarationStatement {
  override def getAllExpressions: List[Expression] = Nil
}

case class UnionDefinitionStatement(name: String, fields: List[(String, String)]) extends DeclarationStatement {
  override def getAllExpressions: List[Expression] = Nil
}

case class ArrayDeclarationStatement(name: String,
                                     bank: Option[String],
                                     length: Option[Expression],
                                     elementType: String,
                                     address: Option[Expression],
                                     const: Boolean,
                                     elements: Option[ArrayContents],
                                     alignment: Option[MemoryAlignment]) extends DeclarationStatement {
  override def getAllExpressions: List[Expression] = List(length, address).flatten ++ elements.fold(List[Expression]())(_.getAllExpressions)
}

case class ParameterDeclaration(typ: String,
                                assemblyParamPassingConvention: ParamPassingConvention) extends Node

case class ImportStatement(filename: String) extends DeclarationStatement {
  override def getAllExpressions: List[Expression] = Nil

  override def name: String = ""
}

case class FunctionDeclarationStatement(name: String,
                                        resultType: String,
                                        params: List[ParameterDeclaration],
                                        bank: Option[String],
                                        address: Option[Expression],
                                        alignment: Option[MemoryAlignment],
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

  def flatMap(f: ExecutableStatement => Option[ExecutableStatement]): Option[ExecutableStatement]

  def loopVariable: String
}

case class ExpressionStatement(expression: Expression) extends ExecutableStatement {
  override def getAllExpressions: List[Expression] = List(expression)
}

case class ReturnStatement(value: Option[Expression]) extends ExecutableStatement {
  override def getAllExpressions: List[Expression] = value.toList
}

case class EmptyStatement(toTypecheck: List[ExecutableStatement]) extends ExecutableStatement {
  override def getAllExpressions: List[Expression] = toTypecheck.flatMap(_.getAllExpressions)
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

case class MosAssemblyStatement(opcode: Opcode.Value, addrMode: AddrMode.Value, expression: Expression, elidability: Elidability.Value) extends ExecutableStatement {
  override def getAllExpressions: List[Expression] = List(expression)
  override def getAllPointies: Seq[String] = addrMode match {
    case AddrMode.IndexedY | AddrMode.IndexedX | AddrMode.LongIndexedY | AddrMode.IndexedZ | AddrMode.LongIndexedZ |
         AddrMode.ZeroPage | AddrMode.ZeroPageX | AddrMode.ZeroPageY =>
      expression.getAllIdentifiers.toSeq.map(_.takeWhile(_ != '.'))
    case AddrMode.Immediate =>
      expression.getAllIdentifiers.toSeq.filter(i => !i.contains('.') || i.endsWith(".addr") || i.endsWith(".addr.lo")).map(_.takeWhile(_ != '.'))
    case _ => Seq.empty
  }
}

case class Z80AssemblyStatement(opcode: ZOpcode.Value, registers: ZRegisters, offsetExpression: Option[Expression], expression: Expression, elidability: Elidability.Value) extends ExecutableStatement {
  override def getAllExpressions: List[Expression] = List(expression)
}

case class IfStatement(condition: Expression, thenBranch: List[ExecutableStatement], elseBranch: List[ExecutableStatement]) extends CompoundStatement {
  override def getAllExpressions: List[Expression] = condition :: (thenBranch ++ elseBranch).flatMap(_.getAllExpressions)

  override def getChildStatements: Seq[Statement] = thenBranch ++ elseBranch

  override def flatMap(f: ExecutableStatement => Option[ExecutableStatement]): Option[ExecutableStatement] = {
    val t = thenBranch.map(f)
    val e = elseBranch.map(f)
    if (t.forall(_.isDefined) && e.forall(_.isDefined)) Some(IfStatement(condition, t.map(_.get), e.map(_.get)).pos(this.position))
    else None
  }

  override def loopVariable: String = "-none-"
}

case class WhileStatement(condition: Expression, body: List[ExecutableStatement], increment: List[ExecutableStatement], labels: Set[String] = Set("", "while")) extends CompoundStatement {
  override def getAllExpressions: List[Expression] = condition :: body.flatMap(_.getAllExpressions)

  override def getChildStatements: Seq[Statement] = body ++ increment

  override def flatMap(f: ExecutableStatement => Option[ExecutableStatement]): Option[ExecutableStatement] = {
    val b = body.map(f)
    val i = increment.map(f)
    if (b.forall(_.isDefined) && i.forall(_.isDefined)) Some(WhileStatement(condition, b.map(_.get), i.map(_.get), labels).pos(this.position))
    else None
  }

  override def loopVariable: String = "-none-"
}

object ForDirection extends Enumeration {
  val To, Until, DownTo, ParallelTo, ParallelUntil = Value
}

case class ForStatement(variable: String, start: Expression, end: Expression, direction: ForDirection.Value, body: List[ExecutableStatement]) extends CompoundStatement {
  override def getAllExpressions: List[Expression] = VariableExpression(variable) :: start :: end :: body.flatMap(_.getAllExpressions)

  override def getChildStatements: Seq[Statement] = body

  override def flatMap(f: ExecutableStatement => Option[ExecutableStatement]): Option[ExecutableStatement] = {
    val b = body.map(f)
    if (b.forall(_.isDefined)) Some(ForStatement(variable, start, end, direction, b.map(_.get)).pos(this.position))
    else None
  }

  override def loopVariable: String = variable
}

case class ForEachStatement(variable: String, values: Either[Expression, List[Expression]], body: List[ExecutableStatement]) extends CompoundStatement {
  override def getAllExpressions: List[Expression] = VariableExpression(variable) :: body.flatMap(_.getAllExpressions)

  override def getChildStatements: Seq[Statement] = body
  override def flatMap(f: ExecutableStatement => Option[ExecutableStatement]): Option[ExecutableStatement] = {
    val b = body.map(f)
    if (b.forall(_.isDefined)) Some(ForEachStatement(variable,values, b.map(_.get)).pos(this.position))
    else None
  }

  override def loopVariable: String = variable
}

case class DoWhileStatement(body: List[ExecutableStatement], increment: List[ExecutableStatement], condition: Expression, labels: Set[String] = Set("", "do")) extends CompoundStatement {
  override def getAllExpressions: List[Expression] = condition :: body.flatMap(_.getAllExpressions)

  override def getChildStatements: Seq[Statement] = body ++ increment

  override def flatMap(f: ExecutableStatement => Option[ExecutableStatement]): Option[ExecutableStatement] = {
    val b = body.map(f)
    val i = increment.map(f)
    if (b.forall(_.isDefined) && i.forall(_.isDefined)) Some(DoWhileStatement(b.map(_.get), i.map(_.get), condition, labels).pos(this.position))
    else None
  }

  override def loopVariable: String = "-none-"
}

case class BreakStatement(label: String) extends ExecutableStatement {
  override def getAllExpressions: List[Expression] = Nil
}

case class ContinueStatement(label: String) extends ExecutableStatement {
  override def getAllExpressions: List[Expression] = Nil
}

object MosAssemblyStatement {
  def implied(opcode: Opcode.Value, elidability: Elidability.Value) = MosAssemblyStatement(opcode, AddrMode.Implied, LiteralExpression(0, 1), elidability)

  def nonexistent(opcode: Opcode.Value) = MosAssemblyStatement(opcode, AddrMode.DoesNotExist, LiteralExpression(0, 1), elidability = Elidability.Elidable)
}