package millfork.output

/**
  * @author Karol Stasiak
  */
sealed trait MemoryAlignment {
  def isMultiplePages: Boolean
}

case object NoAlignment extends MemoryAlignment {
  override def isMultiplePages: Boolean = false
}

case object WithinPageAlignment extends MemoryAlignment {
  override def isMultiplePages: Boolean = false
}

case class DivisibleAlignment(divisor: Int) extends MemoryAlignment {
  override def isMultiplePages: Boolean = divisor > 256
}
