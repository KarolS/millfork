package millfork.node

import millfork.assembly.Elidability
import millfork.assembly.m6809.{MAddrMode, MOpcode}
import millfork.assembly.mos.opt.SourceOfNZ
import millfork.assembly.mos.{AddrMode, Opcode}
import millfork.assembly.z80.{NoRegisters, OneRegister, ZOpcode, ZRegisters}
import millfork.env.{Constant, ParamPassingConvention, Type, VariableType}
import millfork.output.MemoryAlignment

case class Position(moduleName: String, line: Int, column: Int, cursor: Int)

case class FieldDesc(typeName:String, fieldName: String, arraySize: Option[Expression])

case class ResolvedFieldDesc(typ:VariableType, fieldName: String, arraySize: Option[Int])

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
  def renameVariable(variable: String, newVariable: String): Expression
  def replaceVariable(variable: String, actualParam: Expression): Expression
  def replaceIndexedExpression(predicate: IndexedExpression => Boolean, replacement: IndexedExpression => Expression): Expression
  def containsVariable(variable: String): Boolean
  def getPointies: Seq[String]
  def isPure: Boolean
  def getAllIdentifiers: Set[String]
  def prettyPrint: String

  def #+#(smallInt: Int): Expression = if (smallInt == 0) this else (this #+# LiteralExpression(smallInt, 1).pos(this.position)).pos(this.position)
  def #+#(that: Expression): Expression = that match {
    case SumExpression(params, false) => SumExpression((false -> this) :: params, decimal = false)
    case _ => SumExpression(List(false -> this, false -> that), decimal = false)
  }
  def #*#(smallInt: Int): Expression =
    if (smallInt == 1) this else FunctionCallExpression("*", List(this, LiteralExpression(smallInt, 1).pos(this.position))).pos(this.position)
  def #-#(smallInt: Int): Expression = if (smallInt == 0) this else (this #-# LiteralExpression(smallInt, 1).pos(this.position)).pos(this.position)
  def #-#(that: Expression): Expression = SumExpression(List(false -> this, true -> that), decimal = false)

  @transient var typeCache: Type = _
  @transient var constantValueCache: Option[Constant] = _
}

case class ConstantArrayElementExpression(constant: Constant) extends Expression {
  override def renameVariable(variable: String, newVariable: String): Expression = this
  override def replaceVariable(variable: String, actualParam: Expression): Expression = this
  override def replaceIndexedExpression(predicate: IndexedExpression => Boolean, replacement: IndexedExpression => Expression): Expression = this
  override def containsVariable(variable: String): Boolean = false
  override def getPointies: Seq[String] = Seq.empty
  override def isPure: Boolean = true
  override def getAllIdentifiers: Set[String] = Set.empty
  override def prettyPrint: String = constant.toString
}

case class LiteralExpression(value: Long, requiredSize: Int) extends Expression {
  override def renameVariable(variable: String, newVariable: String): Expression = this
  override def replaceVariable(variable: String, actualParam: Expression): Expression = this
  override def replaceIndexedExpression(predicate: IndexedExpression => Boolean, replacement: IndexedExpression => Expression): Expression = this
  override def containsVariable(variable: String): Boolean = false
  override def getPointies: Seq[String] = Seq.empty
  override def isPure: Boolean = true
  override def getAllIdentifiers: Set[String] = Set.empty
  override def prettyPrint: String = "$" + value.toHexString
}

case class TextLiteralExpression(characters: List[Expression]) extends Expression {
  override def renameVariable(variable: String, newVariable: String): Expression = this
  override def replaceVariable(variable: String, actualParam: Expression): Expression = this
  override def replaceIndexedExpression(predicate: IndexedExpression => Boolean, replacement: IndexedExpression => Expression): Expression = this
  override def containsVariable(variable: String): Boolean = false
  override def getPointies: Seq[String] = Seq.empty
  override def isPure: Boolean = true
  override def getAllIdentifiers: Set[String] = Set.empty
  override def prettyPrint: String = characters.map(_.prettyPrint).mkString("[", ",", "]")
}

case class GeneratedConstantExpression(value: Constant, typ: Type) extends Expression {
  override def renameVariable(variable: String, newVariable: String): Expression = this
  override def replaceVariable(variable: String, actualParam: Expression): Expression = this
  override def replaceIndexedExpression(predicate: IndexedExpression => Boolean, replacement: IndexedExpression => Expression): Expression = this
  override def containsVariable(variable: String): Boolean = false
  override def getPointies: Seq[String] = Seq.empty
  override def isPure: Boolean = true
  override def getAllIdentifiers: Set[String] = Set.empty
  override def prettyPrint: String = value.toString
}

case class BooleanLiteralExpression(value: Boolean) extends Expression {
  override def renameVariable(variable: String, newVariable: String): Expression = this
  override def replaceVariable(variable: String, actualParam: Expression): Expression = this
  override def replaceIndexedExpression(predicate: IndexedExpression => Boolean, replacement: IndexedExpression => Expression): Expression = this
  override def containsVariable(variable: String): Boolean = false
  override def getPointies: Seq[String] = Seq.empty
  override def isPure: Boolean = true
  override def getAllIdentifiers: Set[String] = Set.empty
  override def prettyPrint: String = value.toString
}

sealed trait LhsExpression extends Expression

case object BlackHoleExpression extends LhsExpression {
  override def renameVariable(variable: String, newVariable: String): Expression = this
  override def replaceVariable(variable: String, actualParam: Expression): LhsExpression = this
  override def replaceIndexedExpression(predicate: IndexedExpression => Boolean, replacement: IndexedExpression => Expression): Expression = this
  override def containsVariable(variable: String): Boolean = false
  override def getPointies: Seq[String] = Seq.empty
  override def isPure: Boolean = true
  override def getAllIdentifiers: Set[String] = Set.empty
  override def prettyPrint: String = "(_|_)"
}

case class SeparateBytesExpression(hi: Expression, lo: Expression) extends LhsExpression {
  override def renameVariable(variable: String, newVariable: String): Expression =
    SeparateBytesExpression(
      hi.renameVariable(variable, newVariable),
      lo.renameVariable(variable, newVariable)).pos(position)
  def replaceVariable(variable: String, actualParam: Expression): Expression =
    SeparateBytesExpression(
      hi.replaceVariable(variable, actualParam),
      lo.replaceVariable(variable, actualParam)).pos(position)
  override def replaceIndexedExpression(predicate: IndexedExpression => Boolean, replacement: IndexedExpression => Expression): Expression =
    SeparateBytesExpression(
          hi.replaceIndexedExpression(predicate, replacement),
          lo.replaceIndexedExpression(predicate, replacement)).pos(position)
  override def containsVariable(variable: String): Boolean = hi.containsVariable(variable) || lo.containsVariable(variable)
  override def getPointies: Seq[String] = hi.getPointies ++ lo.getPointies
  override def isPure: Boolean = hi.isPure && lo.isPure
  override def getAllIdentifiers: Set[String] = hi.getAllIdentifiers ++ lo.getAllIdentifiers
  override def prettyPrint: String = s"($hi:$lo)"
}

case class SumExpression(expressions: List[(Boolean, Expression)], decimal: Boolean) extends Expression {
  override def renameVariable(variable: String, newVariable: String): Expression =
    SumExpression(expressions.map { case (n, e) => n -> e.renameVariable(variable, newVariable) }, decimal).pos(position)
  override def replaceVariable(variable: String, actualParam: Expression): Expression =
    SumExpression(expressions.map { case (n, e) => n -> e.replaceVariable(variable, actualParam) }, decimal).pos(position)
  override def replaceIndexedExpression(predicate: IndexedExpression => Boolean, replacement: IndexedExpression => Expression): Expression =
    SumExpression(expressions.map { case (n, e) => n -> e.replaceIndexedExpression(predicate, replacement) }, decimal).pos(position)
  override def containsVariable(variable: String): Boolean = expressions.exists(_._2.containsVariable(variable))
  override def getPointies: Seq[String] = expressions.flatMap(_._2.getPointies)
  override def isPure: Boolean = expressions.forall(_._2.isPure)
  override def getAllIdentifiers: Set[String] = expressions.map(_._2.getAllIdentifiers).fold(Set[String]())(_ ++ _)

  override def #+#(that: Expression): Expression =
    if (decimal) super.#+#(that)
    else that match {
      case SumExpression(params, false) => SumExpression(expressions ++ params, decimal = false)
      case _ => SumExpression(expressions :+ (false -> that), decimal = false)
    }
  override def #-#(that: Expression): Expression =
    if (decimal) super.#-#(that)
    else SumExpression(expressions :+ (true -> that), decimal = false)
  override def prettyPrint: String = '(' + expressions.map{case(neg, e) => (if (neg) "- " else "+ ") + e.prettyPrint}.mkString("", " ", ")").stripPrefix("+ ")
}

case class FunctionCallExpression(functionName: String, expressions: List[Expression]) extends Expression {
  override def renameVariable(variable: String, newVariable: String): Expression =
    FunctionCallExpression(functionName, expressions.map {
      _.renameVariable(variable, newVariable)
    }).pos(position)
  override def replaceVariable(variable: String, actualParam: Expression): Expression =
    FunctionCallExpression(functionName, expressions.map {
      _.replaceVariable(variable, actualParam)
    }).pos(position)
  override def replaceIndexedExpression(predicate: IndexedExpression => Boolean, replacement: IndexedExpression => Expression): Expression = 
    FunctionCallExpression(functionName, expressions.map {
      _.replaceIndexedExpression(predicate, replacement)
    }).pos(position)
  override def containsVariable(variable: String): Boolean = expressions.exists(_.containsVariable(variable))
  override def getPointies: Seq[String] = expressions.flatMap(_.getPointies)
  override def isPure: Boolean = false // TODO
  override def getAllIdentifiers: Set[String] = expressions.map(_.getAllIdentifiers).fold(Set[String]())(_ ++ _) + functionName
  override def prettyPrint: String =
    if (expressions.size != 2 || functionName.exists(Character.isAlphabetic(_)))
      functionName + expressions.mkString("(", ", ", ")")
    else s"(${expressions.head.prettyPrint} $functionName ${expressions(1).prettyPrint}"
}

case class HalfWordExpression(expression: Expression, hiByte: Boolean) extends Expression {
  override def renameVariable(variable: String, newVariable: String): Expression =
    HalfWordExpression(expression.renameVariable(variable, newVariable), hiByte).pos(position)
  override def replaceVariable(variable: String, actualParam: Expression): Expression =
    HalfWordExpression(expression.replaceVariable(variable, actualParam), hiByte).pos(position)
  override def replaceIndexedExpression(predicate: IndexedExpression => Boolean, replacement: IndexedExpression => Expression): Expression = 
    HalfWordExpression(expression.replaceIndexedExpression(predicate, replacement), hiByte).pos(position)
  override def containsVariable(variable: String): Boolean = expression.containsVariable(variable)
  override def getPointies: Seq[String] = expression.getPointies
  override def isPure: Boolean = expression.isPure
  override def getAllIdentifiers: Set[String] = expression.getAllIdentifiers
  override def prettyPrint: String = '(' + expression.prettyPrint + (if (hiByte) ").hi" else ").lo")
}

sealed class NiceFunctionProperty(override val toString: String)

object NiceFunctionProperty {
  case object DoesntReadMemory extends NiceFunctionProperty("MR")
  case object DoesntWriteMemory extends NiceFunctionProperty("MW")
  case object Idempotent extends NiceFunctionProperty("Idem")
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
  case class SetsSourceOfNZ(sourceOfNZ: SourceOfNZ) extends NiceFunctionProperty(sourceOfNZ + "NZ")
  case class SetsXTo(value: Int) extends NiceFunctionProperty("Y=" + value)
  case class SetsYTo(value: Int) extends NiceFunctionProperty("Z=" + value)
  case class SetsATo(value: Int) extends NiceFunctionProperty("A=" + value)
  case class Bit0OfA(value: Boolean) extends NiceFunctionProperty("A0=" + value)
  case class Bit7OfA(value: Boolean) extends NiceFunctionProperty("A7=" + value)
}

object Z80NiceFunctionProperty {
  case object DoesntChangeBC extends NiceFunctionProperty("BC")
  case object DoesntChangeDE extends NiceFunctionProperty("DE")
  case object DoesntChangeHL extends NiceFunctionProperty("HL")
  case object DoesntChangeIY extends NiceFunctionProperty("IY")
  case class SetsATo(value: Int) extends NiceFunctionProperty("A=" + value)
}

object M6809NiceFunctionProperty {
  case object DoesntChangeA extends NiceFunctionProperty("A")
  case object DoesntChangeB extends NiceFunctionProperty("B")
  case object DoesntChangeX extends NiceFunctionProperty("X")
  case object DoesntChangeY extends NiceFunctionProperty("Y")
  case object DoesntChangeU extends NiceFunctionProperty("U")
  case object DoesntChangeDP extends NiceFunctionProperty("DP")
  case object DoesntChangeCF extends NiceFunctionProperty("C")
  case class SetsBTo(value: Int) extends NiceFunctionProperty("B=" + value)
}

object MosRegister extends Enumeration {
  val A, X, Y, AX, AY, YA, XA, XY, YX, AW = Value

  private val registerStringToValue = Map[String, MosRegister.Value](
    "xy" -> MosRegister.XY,
    "yx" -> MosRegister.YX,
    "ax" -> MosRegister.AX,
    "ay" -> MosRegister.AY,
    "xa" -> MosRegister.XA,
    "ya" -> MosRegister.YA,
    "a" -> MosRegister.A,
    "x" -> MosRegister.X,
    "y" -> MosRegister.Y,
  )
  private val registerValueToString = registerStringToValue.map { case (key, value) => (value, key)}.toMap

  def fromString(name: String): Option[MosRegister.Value] = registerStringToValue.get(name)
  def toString(value: MosRegister.Value): Option[String] = registerValueToString.get(value)
}

object ZRegister extends Enumeration {

  val A, B, C, D, E, H, L, AF, BC, HL, DE, SP, IXH, IXL, IYH, IYL, IX, IY, R, I, MEM_HL, MEM_BC, MEM_DE, MEM_IX_D, MEM_IY_D, MEM_ABS_8, MEM_ABS_16, IMM_8, IMM_16 = Value

  val registerStringToValue = Map[String, ZRegister.Value](
    "hl" -> ZRegister.HL,
    "bc" -> ZRegister.BC,
    "de" -> ZRegister.DE,
    "a" -> ZRegister.A,
    "b" -> ZRegister.B,
    "c" -> ZRegister.C,
    "d" -> ZRegister.D,
    "e" -> ZRegister.E,
    "h" -> ZRegister.H,
    "l" -> ZRegister.L,
  )

  private val registerValueToString = registerStringToValue.map { case (key, value) => (value, key)}.toMap

  def fromString(name: String): Option[ZRegister.Value] = registerStringToValue.get(name)
  def toString(value: ZRegister.Value): Option[String] = registerValueToString.get(value)

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

  val main7Registers: Set[ZRegister.Value] = Set[ZRegister.Value](ZRegister.A, ZRegister.B, ZRegister.C, ZRegister.D, ZRegister.D, ZRegister.E, ZRegister.H, ZRegister.L)
}

object M6809Register extends Enumeration {
  val A, B, D, DP, X, Y, U, S, PC, CC = Value

    val registerStringToValue = Map[String, M6809Register.Value](
    "x" -> M6809Register.X,
    "y" -> M6809Register.Y,
    "s" -> M6809Register.S,
    "u" -> M6809Register.U,
    "a" -> M6809Register.A,
    "b" -> M6809Register.B,
    "d" -> M6809Register.D,
    "dp" -> M6809Register.DP,
    "pc" -> M6809Register.PC,
    "cc" -> M6809Register.CC,
  )

  private val registerValueToString = registerStringToValue.map { case (key, value) => (value, key)}.toMap

  def fromString(name: String): Option[M6809Register.Value] = registerStringToValue.get(name)
  def toString(value: M6809Register.Value): Option[String] = registerValueToString.get(value)

  def registerSize(reg: Value): Int = reg match {
    case D | X | Y | U | S | PC => 2
    case A | B | DP | CC => 1
  }
  def registerPushMask(register: M6809Register.Value): Int = {
    register match {
      case CC => 1
      case A => 2
      case B => 4
      case D => 6
      case X => 0x10
      case Y => 0x20
      case U => 0x40
      case S => 0x40
      case PC => 0x80
      case DP => 0
    }
  }
}

//case class Indexing(child: Expression, register: Register.Value) extends Expression

case class VariableExpression(name: String) extends LhsExpression {
  override def renameVariable(variable: String, newVariable: String): Expression =
    if (name == variable)
      VariableExpression(newVariable).pos(position)
    else if (name.startsWith(variable) && name(variable.length) == '.')
      VariableExpression(newVariable + name.stripPrefix(variable)).pos(position)
    else this
  override def replaceVariable(variable: String, actualParam: Expression): Expression =
    if (name == variable) actualParam
    else if (name.startsWith(variable) && name(variable.length) == '.') {
      actualParam match {
        case VariableExpression(newVariable) => this.renameVariable(variable, newVariable)
        case _ =>
          name.stripPrefix(variable) match {
            case ".lo" => FunctionCallExpression("lo", List(this)).pos(position)
            case ".hi" => FunctionCallExpression("hi", List(this)).pos(position)
            case _ => ??? // TODO
          }
      }
    } else this
  override def replaceIndexedExpression(predicate: IndexedExpression => Boolean, replacement: IndexedExpression => Expression): Expression = this
  override def containsVariable(variable: String): Boolean = name == variable
  override def getPointies: Seq[String] = if (name.endsWith(".addr.lo")) Seq(name.stripSuffix(".addr.lo")) else Seq.empty
  override def isPure: Boolean = true
  override def getAllIdentifiers: Set[String] = Set(name)
  override def prettyPrint: String = name
}

case class IndexedExpression(name: String, index: Expression) extends LhsExpression {
  override def renameVariable(variable: String, newVariable: String): Expression = {
    val newIndex = index.renameVariable(variable, newVariable)
    if (name == variable)
      IndexedExpression(newVariable, newIndex).pos(position)
    else if (name.startsWith(variable) && name(variable.length) == '.')
      IndexedExpression(newVariable + name.stripPrefix(variable), newIndex).pos(position)
    else
      IndexedExpression(name, newIndex).pos(position)
  }

  override def replaceVariable(variable: String, actualParam: Expression): Expression =
    if (name == variable) {
      actualParam match {
        case VariableExpression(actualVariable) =>
          IndexedExpression(actualVariable, index.replaceVariable(variable, actualParam)).pos(position)
        case _ => ??? // TODO
      }
    } else IndexedExpression(name, index.replaceVariable(variable, actualParam)).pos(position)
  override def replaceIndexedExpression(predicate: IndexedExpression => Boolean, replacement: IndexedExpression => Expression): Expression =
    if (predicate(this)) replacement(this).pos(position = position) 
    else IndexedExpression(name, index.replaceIndexedExpression(predicate, replacement))
  override def containsVariable(variable: String): Boolean = name == variable || index.containsVariable(variable)
  override def getPointies: Seq[String] = Seq(name)
  override def isPure: Boolean = index.isPure
  override def getAllIdentifiers: Set[String] = index.getAllIdentifiers + name
  override def prettyPrint: String = s"$name[${index.prettyPrint}]"
}

case class IndirectFieldExpression(root: Expression, firstIndices: Seq[Expression], fields: Seq[(Boolean, String, Seq[Expression])]) extends LhsExpression {
  override def renameVariable(variable: String, newVariable: String): Expression =
    IndirectFieldExpression(
      root.renameVariable(variable, newVariable),
      firstIndices.map(_.renameVariable(variable, newVariable)),
      fields.map{case (dot, f, i) => (dot, f, i.map(_.renameVariable(variable, newVariable)))})
  override def replaceVariable(variable: String, actualParam: Expression): Expression =
    IndirectFieldExpression(
      root.replaceVariable(variable, actualParam),
      firstIndices.map(_.replaceVariable(variable, actualParam)),
      fields.map{case (dot, f, i) => (dot, f, i.map(_.replaceVariable(variable, actualParam)))})
  
  override def replaceIndexedExpression(predicate: IndexedExpression => Boolean, replacement: IndexedExpression => Expression): Expression =
    IndirectFieldExpression(
      root.replaceIndexedExpression(predicate, replacement),
      firstIndices.map(_.replaceIndexedExpression(predicate, replacement)),
      fields.map{case (dot, f, i) => (dot, f, i.map(_.replaceIndexedExpression(predicate, replacement)))})

  override def containsVariable(variable: String): Boolean =
    root.containsVariable(variable) ||
      firstIndices.exists(_.containsVariable(variable)) ||
      fields.exists(_._3.exists(_.containsVariable(variable)))

  override def getPointies: Seq[String] = (root match {
    case VariableExpression(v) => List(v)
    case _ => root.getPointies
  }) ++ firstIndices.flatMap(_.getPointies) ++ fields.flatMap(_._3.flatMap(_.getPointies))

  override def isPure: Boolean = root.isPure && firstIndices.forall(_.isPure) && fields.forall(_._3.forall(_.isPure))

  override def getAllIdentifiers: Set[String] = root.getAllIdentifiers ++ firstIndices.flatMap(_.getAllIdentifiers) ++ fields.flatMap(_._3.flatMap(_.getAllIdentifiers))

  override def prettyPrint: String = root.prettyPrint +
    firstIndices.map(i => '[' + i.prettyPrint + ']').mkString("") +
    fields.map{case (dot, f, ixs) => (if (dot) "." else "->") + f + ixs.map(i => '[' + i.prettyPrint + ']').mkString("")}
}

case class DerefDebuggingExpression(inner: Expression, preferredSize: Int) extends LhsExpression {
  override def renameVariable(variable: String, newVariable: String): Expression = DerefDebuggingExpression(inner.renameVariable(variable, newVariable), preferredSize)

  override def replaceVariable(variable: String, actualParam: Expression): Expression = DerefDebuggingExpression(inner.replaceVariable(variable, actualParam), preferredSize)

  override def replaceIndexedExpression(predicate: IndexedExpression => Boolean, replacement: IndexedExpression => Expression): Expression =
    DerefDebuggingExpression(inner.replaceIndexedExpression(predicate, replacement), preferredSize)

  override def containsVariable(variable: String): Boolean = inner.containsVariable(variable)

  override def getPointies: Seq[String] = inner match {
    case VariableExpression(v) => List(v)
    case _ => inner.getPointies
  }

  override def isPure: Boolean = inner.isPure

  override def getAllIdentifiers: Set[String] = inner.getAllIdentifiers
  override def prettyPrint: String = s"¥deref(${inner.prettyPrint})"
}

case class DerefExpression(inner: Expression, offset: Int, targetType: Type) extends LhsExpression {
  override def renameVariable(variable: String, newVariable: String): Expression = DerefExpression(inner.renameVariable(variable, newVariable), offset, targetType)
  override def replaceVariable(variable: String, actualParam: Expression): Expression = DerefExpression(inner.replaceVariable(variable, actualParam), offset, targetType)

  override def replaceIndexedExpression(predicate: IndexedExpression => Boolean, replacement: IndexedExpression => Expression): Expression =
    DerefExpression(inner.replaceIndexedExpression(predicate, replacement), offset, targetType)

  override def containsVariable(variable: String): Boolean = inner.containsVariable(variable)

  override def getPointies: Seq[String] = inner match {
    case VariableExpression(v) => List(v)
    case _ => inner.getPointies
  }

  override def isPure: Boolean = inner.isPure

  override def getAllIdentifiers: Set[String] = inner.getAllIdentifiers
  override def prettyPrint: String = s"¥deref(${inner.prettyPrint})"
}

sealed trait Statement extends Node {
  def getAllExpressions: List[Expression]

  def getAllPointies: Seq[String] = getAllExpressions.flatMap(_.getPointies)
}

sealed trait DeclarationStatement extends Statement {
  def name: String
  var docComment: Option[DocComment] = None
}

object DeclarationStatement {
  implicit class DeclarationStatementOps[D<:DeclarationStatement](val declaration: D) extends AnyVal {
    def docComment(comment: Option[DocComment]): D = {
      if (comment.isDefined) {
        declaration.docComment = comment
      }
      declaration
    }
  }
}

sealed trait BankedDeclarationStatement extends DeclarationStatement {
  def bank: Option[String]
  def name: String
  def optimizationHints: Set[String]
  def withChangedBank(bank: String): BankedDeclarationStatement
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
                                        optimizationHints: Set[String],
                                        alignment: Option[MemoryAlignment]) extends BankedDeclarationStatement {
  override def getAllExpressions: List[Expression] = List(initialValue, address).flatten

  override def withChangedBank(bank: String): BankedDeclarationStatement = copy(bank = Some(bank))
}

trait ArrayContents extends Node {
  def getAllExpressions(bigEndian: Boolean): List[Expression]
  def renameVariable(variableToRename: String, newVariable: String): ArrayContents
  def replaceVariable(variableToReplace: String, expression: Expression): ArrayContents
}

case class LiteralContents(contents: List[Expression]) extends ArrayContents {
  override def getAllExpressions(bigEndian: Boolean): List[Expression] = contents

  override def renameVariable(variableToRename: String, newVariable: String): ArrayContents =
    LiteralContents(contents.map(_.renameVariable(variableToRename, newVariable)))

  override def replaceVariable(variable: String, expression: Expression): ArrayContents =
    LiteralContents(contents.map(_.replaceVariable(variable, expression)))
}

case class ForLoopContents(variable: String, start: Expression, end: Expression, direction: ForDirection.Value, body: ArrayContents) extends ArrayContents {
  override def getAllExpressions(bigEndian: Boolean): List[Expression] = start :: end :: body.getAllExpressions(bigEndian).map(_.replaceVariable(variable, LiteralExpression(0, 1)))

  override def renameVariable(variableToRename: String, newVariable: String): ArrayContents =
    if (variableToRename == variable) this else ForLoopContents(
      variable,
      start.renameVariable(variableToRename, newVariable),
      end.renameVariable(variableToRename, newVariable),
      direction,
      body.renameVariable(variableToRename, newVariable))

  override def replaceVariable(variableToReplace: String, expression: Expression): ArrayContents =
    if (variableToReplace == variable) this else ForLoopContents(
      variable,
      start.replaceVariable(variableToReplace, expression),
      end.replaceVariable(variableToReplace, expression),
      direction,
      body.replaceVariable(variableToReplace, expression))
}

case class CombinedContents(contents: List[ArrayContents]) extends ArrayContents {
  override def getAllExpressions(bigEndian: Boolean): List[Expression] = contents.flatMap(_.getAllExpressions(bigEndian))

  override def renameVariable(variableToRename: String, newVariable: String): ArrayContents =
    CombinedContents(contents.map(_.renameVariable(variableToRename, newVariable)))

  override def replaceVariable(variableToReplace: String, expression: Expression): ArrayContents =
    CombinedContents(contents.map(_.replaceVariable(variableToReplace, expression)))
}

case class ProcessedContents(processor: String, values: ArrayContents) extends ArrayContents {
  private def normalizeProcessor(bigEndian: Boolean): String = processor match {
    case "word" =>  if (bigEndian) "word_be" else "word_le"
    case "long" =>  if (bigEndian) "long_be" else "long_le"
    case x => x
  }
  override def getAllExpressions(bigEndian: Boolean): List[Expression] = normalizeProcessor(bigEndian) match {
    case "word_le" =>
      values.getAllExpressions(bigEndian).flatMap(expr => List(
        FunctionCallExpression("lo", List(expr)).pos(expr.position),
        FunctionCallExpression("hi", List(expr)).pos(expr.position)
      ))
    case "word_be" =>
      values.getAllExpressions(bigEndian).flatMap(expr => List(
        FunctionCallExpression("hi", List(expr)).pos(expr.position),
        FunctionCallExpression("lo", List(expr)).pos(expr.position)
      ))
    case "long_le" =>
      values.getAllExpressions(bigEndian).flatMap(expr => List(
        FunctionCallExpression("lo", List(expr)).pos(expr.position),
        FunctionCallExpression("lo", List(FunctionCallExpression(">>", List(expr, LiteralExpression(8, 1))).pos(expr.position))).pos(expr.position),
        FunctionCallExpression("lo", List(FunctionCallExpression(">>", List(expr, LiteralExpression(16, 1))).pos(expr.position))).pos(expr.position),
        FunctionCallExpression("lo", List(FunctionCallExpression(">>", List(expr, LiteralExpression(24, 1))).pos(expr.position))).pos(expr.position)
      ))
    case "long_be" =>
      values.getAllExpressions(bigEndian).flatMap(expr => List(
        FunctionCallExpression("lo", List(FunctionCallExpression(">>", List(expr, LiteralExpression(24, 1))).pos(expr.position))).pos(expr.position),
        FunctionCallExpression("lo", List(FunctionCallExpression(">>", List(expr, LiteralExpression(16, 1))).pos(expr.position))).pos(expr.position),
        FunctionCallExpression("lo", List(FunctionCallExpression(">>", List(expr, LiteralExpression(8, 1))).pos(expr.position))).pos(expr.position),
        FunctionCallExpression("lo", List(expr)).pos(expr.position)
      ))
    case "struct" => values.getAllExpressions(bigEndian) // not used for emitting actual arrays
  }

  override def renameVariable(variableToRename: String, newVariable: String): ArrayContents =
    ProcessedContents(processor, values.renameVariable(variableToRename, newVariable))

  override def replaceVariable(variableToReplace: String, expression: Expression): ArrayContents =
    ProcessedContents(processor, values.replaceVariable(variableToReplace, expression))
}

case class AliasDefinitionStatement(name: String, target: String, important: Boolean) extends DeclarationStatement {
  override def getAllExpressions: List[Expression] = Nil
}

case class EnumDefinitionStatement(name: String, variants: List[(String, Option[Expression])]) extends DeclarationStatement {
  override def getAllExpressions: List[Expression] = variants.flatMap(_._2)
}

case class StructDefinitionStatement(name: String, fields: List[FieldDesc], alignment: Option[MemoryAlignment]) extends DeclarationStatement {
  override def getAllExpressions: List[Expression] = Nil
}

case class UnionDefinitionStatement(name: String, fields: List[FieldDesc], alignment: Option[MemoryAlignment]) extends DeclarationStatement {
  override def getAllExpressions: List[Expression] = Nil
}

case class ArrayDeclarationStatement(name: String,
                                     bank: Option[String],
                                     length: Option[Expression],
                                     elementType: String,
                                     address: Option[Expression],
                                     const: Boolean,
                                     elements: Option[ArrayContents],
                                     optimizationHints: Set[String],
                                     alignment: Option[MemoryAlignment],
                                     bigEndian: Boolean) extends BankedDeclarationStatement {
  override def getAllExpressions: List[Expression] = List(length, address).flatten ++ elements.fold(List[Expression]())(_.getAllExpressions(bigEndian))

  override def withChangedBank(bank: String): BankedDeclarationStatement = copy(bank = Some(bank))
}

case class ParameterDeclaration(typ: String,
                                assemblyParamPassingConvention: ParamPassingConvention) extends Node

case class ImportStatement(filename: String, templateParams: List[String]) extends DeclarationStatement {
  override def getAllExpressions: List[Expression] = Nil

  override def name: String = ""
}

case class FunctionDeclarationStatement(name: String,
                                        resultType: String,
                                        params: List[ParameterDeclaration],
                                        bank: Option[String],
                                        address: Option[Expression],
                                        optimizationHints: Set[String],
                                        alignment: Option[MemoryAlignment],
                                        statements: Option[List[Statement]],
                                        isMacro: Boolean,
                                        inlinable: Option[Boolean],
                                        assembly: Boolean,
                                        interrupt: Boolean,
                                        kernalInterrupt: Boolean,
                                        constPure: Boolean,
                                        reentrant: Boolean) extends BankedDeclarationStatement {
  override def getAllExpressions: List[Expression] = address.toList ++ statements.getOrElse(Nil).flatMap(_.getAllExpressions)

  override def withChangedBank(bank: String): BankedDeclarationStatement = copy(bank = Some(bank))

  override def getAllPointies: Seq[String] = statements match {
    case None => Seq.empty
    case Some(stmts) =>
      val locals = stmts.flatMap{
        case s:VariableDeclarationStatement => Some(s.name)
        case s:ArrayDeclarationStatement => Some(s.name)
        case _ => None
      }.toSet
      val pointies = stmts.flatMap(_.getAllPointies).toSet
      (pointies -- locals).toSeq
  }
}

sealed trait ExecutableStatement extends Statement {
  def isValidFunctionEnd: Boolean = false
}

case class RawBytesStatement(contents: ArrayContents, bigEndian: Boolean) extends ExecutableStatement {
  override def getAllExpressions: List[Expression] = contents.getAllExpressions(bigEndian)
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
  override def isValidFunctionEnd: Boolean = true
}

case class GotoStatement(target: Expression) extends ExecutableStatement {
  override def getAllExpressions: List[Expression] = List(target)
  override def isValidFunctionEnd: Boolean = true
}

case class LabelStatement(name: String) extends ExecutableStatement {
  override def getAllExpressions: List[Expression] = Nil
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
  override def isValidFunctionEnd: Boolean = opcode == Opcode.RTS || opcode == Opcode.RTI || opcode == Opcode.JMP || opcode == Opcode.BRA
}

case class Z80AssemblyStatement(opcode: ZOpcode.Value, registers: ZRegisters, offsetExpression: Option[Expression], expression: Expression, elidability: Elidability.Value) extends ExecutableStatement {
  override def getAllExpressions: List[Expression] = List(expression)

  override def isValidFunctionEnd: Boolean = registers match {
    case NoRegisters | OneRegister(_) =>
      opcode == ZOpcode.RETN || opcode == ZOpcode.RETI || opcode == ZOpcode.JP
    case _ =>
      false
  }
}

case class M6809AssemblyStatement(opcode: MOpcode.Value, addrMode: MAddrMode, expression: Expression, elidability: Elidability.Value) extends ExecutableStatement {
  override def getAllExpressions: List[Expression] = List(expression)

  override def isValidFunctionEnd: Boolean = opcode == MOpcode.RTS || opcode == MOpcode.JMP || opcode == MOpcode.BRA || opcode == MOpcode.RTI
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

  override def isValidFunctionEnd: Boolean = thenBranch.lastOption.fold(false)(_.isValidFunctionEnd) && elseBranch.lastOption.fold(false)(_.isValidFunctionEnd)
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

case class ForStatement(variable: String, start: Expression, end: Expression, direction: ForDirection.Value, body: List[ExecutableStatement], extraIncrement: List[ExecutableStatement] = Nil) extends CompoundStatement {
  override def getAllExpressions: List[Expression] = VariableExpression(variable) :: start :: end :: (body.flatMap(_.getAllExpressions) ++ extraIncrement.flatMap(_.getAllExpressions))

  override def getChildStatements: Seq[Statement] = body ++ extraIncrement

  override def flatMap(f: ExecutableStatement => Option[ExecutableStatement]): Option[ExecutableStatement] = {
    val b = body.map(f)
    val i = extraIncrement.map(f)
    if (b.forall(_.isDefined) && i.forall(_.isDefined)) Some(ForStatement(variable, start, end, direction, b.map(_.get), i.map(_.get)).pos(this.position))
    else None
  }

  override def loopVariable: String = variable
}

case class MemsetStatement(start: Expression, size: Constant, value: Expression, direction: ForDirection.Value, original: Option[ForStatement]) extends CompoundStatement {
  if (original.isEmpty && direction != ForDirection.ParallelUntil) {
    throw new IllegalArgumentException
  }
  override def getAllExpressions: List[Expression] = List(start, value) ++ original.toList.flatMap(_.getAllExpressions)

  override def getChildStatements: Seq[Statement] = original.toList.flatMap(_.getChildStatements)

  override def flatMap(f: ExecutableStatement => Option[ExecutableStatement]): Option[ExecutableStatement] =
    // shouldn't ever yield None, as this is possible only in case of control-flow changing statements:
    Some(copy(original = original.flatMap(_.flatMap(f).asInstanceOf[Option[ForStatement]])))


  override def loopVariable: String = original.fold("_none")(_.loopVariable)
}

case class ForEachStatement(variable: String, pointerVariable: Option[String], values: Either[Expression, List[Expression]], body: List[ExecutableStatement]) extends CompoundStatement {
  override def getAllExpressions: List[Expression] = VariableExpression(variable) :: (values.fold[List[Expression]](_ => Nil, identity) ++ body.flatMap(_.getAllExpressions))

  override def getChildStatements: Seq[Statement] = body
  override def flatMap(f: ExecutableStatement => Option[ExecutableStatement]): Option[ExecutableStatement] = {
    val b = body.map(f)
    if (b.forall(_.isDefined)) Some(ForEachStatement(variable, pointerVariable, values, b.map(_.get)).pos(this.position))
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

case class DocComment(text: String) extends Node {}