package millfork.env

trait Pointy {
  def name: Option[String]
  def indexType: VariableType
  def elementType: VariableType
}

case class VariablePointy(addr: Constant, indexType: VariableType, elementType: VariableType) extends Pointy {
  override def name: Option[String] = None
}

case class ConstantPointy(value: Constant, name: Option[String], size: Option[Int], indexType: VariableType, elementType: VariableType) extends Pointy
