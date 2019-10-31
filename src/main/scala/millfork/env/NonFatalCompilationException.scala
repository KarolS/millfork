package millfork.env

import millfork.node.Position

/**
  * @author Karol Stasiak
  */
abstract class NonFatalCompilationException(msg: String) extends RuntimeException(msg) {
  def position: Option[Position]
}

case class ConstantOverflowException(value: Long, expectedSize: Int) extends NonFatalCompilationException (
  if (expectedSize == 1) {
    s"The constant $value is too big to fit in a byte"
  } else {
    s"The constant $value is too big to fit in $expectedSize bytes"
  }
) {
  override def position: Option[Position] = None
}
case class UndefinedIdentifierException(thingClass: Class[_], name: String, val position: Option[Position])
  extends NonFatalCompilationException(s"${thingClass.getSimpleName} `$name` is not defined") {

}
case class IdentifierHasWrongTypeOfThingException(thingClass: Class[_], name: String, val position: Option[Position])
  extends NonFatalCompilationException(s"`$name` is not a ${thingClass.getSimpleName}") {

}