package millfork.env

import millfork.output.MemoryAlignment

trait Pointy {
  def name: Option[String]
  def indexType: VariableType
  def elementType: VariableType
  def readOnly: Boolean
  def isArray: Boolean
}

case class StackVariablePointy(offset: Int, indexType: VariableType, elementType: VariableType) extends Pointy {
  override def name: Option[String] = None
  override def readOnly: Boolean = false
  override def isArray: Boolean = false
}

case class VariablePointy(addr: Constant, indexType: VariableType, elementType: VariableType, zeropage: Boolean) extends Pointy {
  override def name: Option[String] = None
  override def readOnly: Boolean = false
  override def isArray: Boolean = false
}

case class ConstantPointy(value: Constant,
                          name: Option[String],
                          sizeInBytes: Option[Int],
                          elementCount: Option[Int],
                          indexType: VariableType,
                          elementType: VariableType,
                          alignment: MemoryAlignment,
                          override val readOnly: Boolean) extends Pointy {
  override def isArray: Boolean = elementCount.isDefined
}
