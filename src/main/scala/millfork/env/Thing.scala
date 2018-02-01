package millfork.env

import millfork.assembly.Opcode
import millfork.error.ErrorReporting
import millfork.node._

sealed trait Thing {
  def name: String
}

sealed trait CallableThing extends Thing

sealed trait VariableLikeThing extends Thing

sealed trait IndexableThing extends Thing

sealed trait Type extends CallableThing {

  def size: Int

  def isSigned: Boolean

  def isSubtypeOf(other: Type): Boolean = this == other

  def isCompatible(other: Type): Boolean = this == other

  override def toString(): String = name

  def isAssignableTo(targetType: Type): Boolean = isCompatible(targetType)
}

case object VoidType extends Type {
  def size = 0

  def isSigned = false

  override def name = "void"
}

sealed trait PlainType extends Type {
  override def isCompatible(other: Type): Boolean = this == other || this.isSubtypeOf(other) || other.isSubtypeOf(this)

  override def isAssignableTo(targetType: Type): Boolean = isCompatible(targetType) || (targetType match {
    case BasicPlainType(_, size) => size > this.size // TODO
    case _ => false
  })
}

case class BasicPlainType(name: String, size: Int) extends PlainType {
  def isSigned = false

  override def isSubtypeOf(other: Type): Boolean = this == other
}

case class DerivedPlainType(name: String, parent: PlainType, isSigned: Boolean) extends PlainType {
  def size: Int = parent.size

  override def isSubtypeOf(other: Type): Boolean = parent == other || parent.isSubtypeOf(other)
}

sealed trait BooleanType extends Type {
  def size = 0

  def isSigned = false
}

case class ConstantBooleanType(name: String, value: Boolean) extends BooleanType

case class FlagBooleanType(name: String, jumpIfTrue: Opcode.Value, jumpIfFalse: Opcode.Value) extends BooleanType

case object BuiltInBooleanType extends BooleanType {
  override def name = "bool$"
}

sealed trait TypedThing extends Thing {
  def typ: Type
}


sealed trait ThingInMemory extends Thing {
  def toAddress: Constant
}

sealed trait PreallocableThing extends ThingInMemory {
  def shouldGenerate: Boolean

  def address: Option[Constant]

  def toAddress: Constant = address.getOrElse(MemoryAddressConstant(this))
}

case class Label(name: String) extends ThingInMemory {
  override def toAddress: MemoryAddressConstant = MemoryAddressConstant(this)
}

sealed trait Variable extends TypedThing with VariableLikeThing

case class BlackHole(typ: Type) extends Variable {
  override def name = "<black hole>"
}

sealed trait VariableInMemory extends Variable with ThingInMemory with IndexableThing {

  def zeropage: Boolean
}

case class RegisterVariable(register: Register.Value, typ: Type) extends Variable {
  def name: String = register.toString
}

case class Placeholder(name: String, typ: Type) extends Variable

sealed trait UninitializedMemory extends ThingInMemory {
  def sizeInBytes: Int

  def alloc: VariableAllocationMethod.Value
}

object VariableAllocationMethod extends Enumeration {
  val Auto, Register, Static, Zeropage, None = Value
}

case class StackVariable(name: String, typ: Type, baseOffset: Int) extends Variable {
  def sizeInBytes: Int = typ.size
}

object MemoryVariable {
  def unapply(v: MemoryVariable) = Some((v.name, v.typ, v.alloc))
}

abstract class MemoryVariable extends VariableInMemory {
  def alloc: VariableAllocationMethod.Value
}

case class UninitializedMemoryVariable(name: String, typ: Type, alloc: VariableAllocationMethod.Value) extends MemoryVariable with UninitializedMemory {
  override def sizeInBytes: Int = typ.size

  override def zeropage: Boolean = alloc == VariableAllocationMethod.Zeropage

  override def toAddress: MemoryAddressConstant = MemoryAddressConstant(this)
}

case class InitializedMemoryVariable(name: String, address: Option[Constant], typ: Type, initialValue: Constant) extends MemoryVariable with PreallocableThing {
  override def zeropage: Boolean = false

  override def toAddress: MemoryAddressConstant = MemoryAddressConstant(this)

  override def shouldGenerate: Boolean = true

  override def alloc: VariableAllocationMethod.Value = VariableAllocationMethod.Static
}

trait MfArray extends ThingInMemory with IndexableThing

case class UninitializedArray(name: String, sizeInBytes: Int) extends MfArray with UninitializedMemory {
  override def toAddress: MemoryAddressConstant = MemoryAddressConstant(this)

  override def alloc = VariableAllocationMethod.Static
}

case class RelativeArray(name: String, address: Constant, sizeInBytes: Int) extends MfArray {
  override def toAddress: Constant = address
}

case class InitializedArray(name: String, address: Option[Constant], contents: List[Constant]) extends MfArray with PreallocableThing {
  override def shouldGenerate = true
}

case class RelativeVariable(name: String, address: Constant, typ: Type, zeropage: Boolean) extends VariableInMemory {
  override def toAddress: Constant = address
}

sealed trait MangledFunction extends CallableThing {
  def name: String

  def returnType: Type

  def params: ParamSignature

  def interrupt: Boolean
}

case class EmptyFunction(name: String,
                         returnType: Type,
                         paramType: Type) extends MangledFunction {
  override def params = EmptyFunctionParamSignature(paramType)

  override def interrupt = false
}

case class MacroFunction(name: String,
                         returnType: Type,
                         params: ParamSignature,
                         environment: Environment,
                         code: List[ExecutableStatement]) extends MangledFunction {
  override def interrupt = false
}

sealed trait FunctionInMemory extends MangledFunction with ThingInMemory {
  def environment: Environment
}

case class ExternFunction(name: String,
                          returnType: Type,
                          params: ParamSignature,
                          address: Constant,
                          environment: Environment) extends FunctionInMemory {
  override def toAddress: Constant = address

  override def interrupt = false
}

case class NormalFunction(name: String,
                          returnType: Type,
                          params: ParamSignature,
                          environment: Environment,
                          stackVariablesSize: Int,
                          address: Option[Constant],
                          code: List[ExecutableStatement],
                          interrupt: Boolean,
                          reentrant: Boolean,
                          position: Option[Position]) extends FunctionInMemory with PreallocableThing {
  override def shouldGenerate = true
}

case class ConstantThing(name: String, value: Constant, typ: Type) extends TypedThing with VariableLikeThing with IndexableThing {
  def map(f: Constant => Constant) = ConstantThing("", f(value), typ)
}

trait ParamSignature {
  def types: List[Type]

  def length: Int
}

case class NormalParamSignature(params: List[MemoryVariable]) extends ParamSignature {
  override def length: Int = params.length

  override def types: List[Type] = params.map(_.typ)
}

sealed trait ParamPassingConvention {
  def inInlinedOnly: Boolean

  def inNonInlinedOnly: Boolean
}

case class ByRegister(register: Register.Value) extends ParamPassingConvention {
  override def inInlinedOnly = false

  override def inNonInlinedOnly = false
}

case class ByVariable(name: String) extends ParamPassingConvention {
  override def inInlinedOnly = false

  override def inNonInlinedOnly = true
}

case class ByConstant(name: String) extends ParamPassingConvention {
  override def inInlinedOnly = true

  override def inNonInlinedOnly = false
}

case class ByReference(name: String) extends ParamPassingConvention {
  override def inInlinedOnly = true

  override def inNonInlinedOnly = false
}

object AssemblyParameterPassingBehaviour extends Enumeration {
  val Copy, ByReference, ByConstant = Value
}

case class AssemblyParam(typ: Type, variable: TypedThing, behaviour: AssemblyParameterPassingBehaviour.Value)


case class AssemblyParamSignature(params: List[AssemblyParam]) extends ParamSignature {
  override def length: Int = params.length

  override def types: List[Type] = params.map(_.typ)
}

case class EmptyFunctionParamSignature(paramType: Type) extends ParamSignature {
  override def length: Int = 1

  override def types: List[Type] = List(paramType)
}