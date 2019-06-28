package millfork

/**
  * @author Karol Stasiak
  */
class Prehashed[T](override val hashCode: Int, val value: T) {
  override def equals(obj: Any): Boolean = {
    if (!obj.isInstanceOf[Prehashed[_]]) return false
    if (obj.hashCode() != this.hashCode) return false
    obj.asInstanceOf[Prehashed[_]].value == value
  }
}

object Prehashed {
  def apply[T](value: T): Prehashed[T] = new Prehashed(value.hashCode(), value)
}
