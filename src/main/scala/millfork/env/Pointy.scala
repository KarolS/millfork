package millfork.env

trait Pointy

case class VariablePointy(addr: Constant) extends Pointy
case class ConstantPointy(value: Constant, size: Option[Int]) extends Pointy
