package millfork

import scala.collection.{immutable, mutable}

/**
  * @author Karol Stasiak
  */
object Tarjan {
  def sort[T](vertices: Iterable[T], edges: Iterable[(T,T)]): List[T] = {
    var index = 0
    val s = mutable.Stack[T]()
    val indices = mutable.Map[T, Int]()
    val lowlinks = mutable.Map[T, Int]()
    val onStack = mutable.Set[T]()
    var result = List[T]()

    def strongConnect(v: T): Unit = {
      indices(v) = index
      lowlinks(v) = index
      index += 1
      s.push(v)
      onStack += v
      edges.filter(_._1 == v).foreach {
        case (_, w) =>
          if (!indices.contains(w)) {
            strongConnect(w)
            lowlinks(v) = lowlinks(v) min lowlinks(w)
          } else if (onStack(w)) {
            lowlinks(v) = lowlinks(v) min indices(w)
          }
      }
      if (lowlinks(v) == indices(v)) {
        var w: T = v
        do {
          w = s.pop()
          onStack -= w
          result ::= w
        } while (w != v)
      }
    }

    vertices.foreach{ v =>
      if (!indices.contains(v)) strongConnect(v)
    }
    result.reverse
  }
}
