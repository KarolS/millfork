package millfork

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
class MutableMultimap[K, V]() {
  val inner: mutable.HashMap[K, mutable.HashSet[V]] = mutable.HashMap()

  def addBinding(k: K, v: V): Unit = {
    inner.get(k) match {
      case None =>
        val s = mutable.HashSet[V]()
        s += v
        inner(k) = s
      case Some(s) =>
        s += v
    }
  }

  def get(k: K): Set[V] = {
    inner.get(k) match {
      case None =>
        Set()
      case Some(s) =>
        s.toSet
    }
  }
}
