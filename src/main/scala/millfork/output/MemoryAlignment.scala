package millfork.output

/**
  * @author Karol Stasiak
  */
sealed trait MemoryAlignment

case object NoAlignment extends MemoryAlignment
case object WithinPageAlignment extends MemoryAlignment
case class DivisibleAlignment(divisor: Int) extends MemoryAlignment
