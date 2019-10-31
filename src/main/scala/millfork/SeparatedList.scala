package millfork

/**
  * @author Karol Stasiak
  */
case class SeparatedList[T, S](head: T, tail: List[(S, T)]) {

  def exists(predicate: T => Boolean): Boolean = {
     predicate(head) || tail.exists(x => predicate(x._2))
  }

  def count(predicate: T => Boolean): Int = {
    val headCount = if (predicate(head)) 1 else 0
    headCount + tail.count(x => predicate(x._2))
  }

  def map[R](f: T => R): SeparatedList[R, S] = {
    SeparatedList(f(head), tail.map(x => x._1 -> f(x._2)))
  }

  def toPairList(initialSeparator: S): List[(S, T)] = (initialSeparator -> head) :: tail

  def size: Int = tail.size + 1

  def items: List[T] = head :: tail.map(_._2)

  def separators: List[S] = tail.map(_._1)

  def drop(i: Int): SeparatedList[T, S] = if (i == 0) this else SeparatedList(tail(i - 1)._2, tail.drop(i))

  def take(i: Int): SeparatedList[T, S] = if (i <= 0) ??? else SeparatedList(head, tail.take(i - 1))

  def splitAt(i: Int): (SeparatedList[T, S], S, SeparatedList[T, S]) = {
    val (a, b) = tail.splitAt(i - 1)
    (SeparatedList(head, a), b.head._1, SeparatedList(b.head._2, b.tail))
  }

  def indexOfSeparator(p: S => Boolean): Int = 1 + tail.indexWhere(x => p(x._1))

  def ::(pair: (T, S)) = SeparatedList(pair._1, (pair._2 -> head) :: tail)

  def split(p: S => Boolean): SeparatedList[SeparatedList[T, S], S] = {
    val i = indexOfSeparator(p)
    if (i <= 0) SeparatedList(this, Nil)
    else {
      val (a, b, c) = splitAt(i)
      (a, b) :: c.split(p)
    }
  }
}

object SeparatedList {
  def of[T, S](t0: T): SeparatedList[T, S] = SeparatedList[T, S](t0, Nil)

  def of[T, S](t0: T, s1: S, t1: T): SeparatedList[T, S] =
    SeparatedList(t0, List(s1 -> t1))

  def of[T, S](t0: T, s1: S, t1: T, s2: S, t2: T): SeparatedList[T, S] =
    SeparatedList(t0, List(s1 -> t1, s2 -> t2))

  def of[T, S](t0: T, s1: S, t1: T, s2: S, t2: T, s3: S, t3: T): SeparatedList[T, S] =
    SeparatedList(t0, List(s1 -> t1, s2 -> t2, s3 -> t3))
}