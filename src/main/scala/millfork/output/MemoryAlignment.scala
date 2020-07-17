package millfork.output

import millfork.MathUtils

/**
  * @author Karol Stasiak
  */
sealed trait MemoryAlignment {
  def isMultiplePages: Boolean
  def roundSizeUp(size: Int): Int
  def &(other: MemoryAlignment): MemoryAlignment
}

case object NoAlignment extends MemoryAlignment {
  override def isMultiplePages: Boolean = false
  override def roundSizeUp(size: Int): Int = size
  override def &(other: MemoryAlignment): MemoryAlignment = other
}

case object WithinPageAlignment extends MemoryAlignment {
  override def isMultiplePages: Boolean = false
  override def roundSizeUp(size: Int): Int = size
  override def &(other: MemoryAlignment): MemoryAlignment = other match {
    case NoAlignment | WithinPageAlignment => this
    case _ => throw new IllegalArgumentException(s"Cannot use incompatible alignments $this and $other simultaneously")
  }
}

case class DivisibleAlignment(divisor: Int) extends MemoryAlignment {
  override def isMultiplePages: Boolean = divisor > 256
  override def roundSizeUp(size: Int): Int =
    if (size % divisor == 0) size else size + divisor - size % divisor
  override def &(other: MemoryAlignment): MemoryAlignment = other match {
    case NoAlignment => this
    case DivisibleAlignment(d) => DivisibleAlignment(MathUtils.lcm(divisor, d))
    case _ => throw new IllegalArgumentException(s"Cannot use incompatible alignments $this and $other simultaneously")
  }
}
