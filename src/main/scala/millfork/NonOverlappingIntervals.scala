package millfork

import scala.collection.mutable.ListBuffer

/**
  * @author Karol Stasiak
  */
object NonOverlappingIntervals {
  def apply[T](intervals: Iterable[T], start: T => Int, end: T => Int): Seq[Set[T]] = {
    def nonEmpty(interval: T): Boolean = start(interval) != end(interval)

    val builder = ListBuffer[Set[T]]()

    def scan(set: Set[T], lastEnd: Int): Unit = {
      builder += set
      for (interval <- intervals if nonEmpty(interval)) {
        if (start(interval) >= lastEnd) {
          scan(set + interval, end(interval))
        }
      }
    }

    scan(Set(), 0)
    builder.toIndexedSeq
  }
}
