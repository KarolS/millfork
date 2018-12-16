package millfork.assembly.opt

/**
  * @author Karol Stasiak
  */
class FlowCache[L, F](val name: String) {
  private val lastL = new Array[List[L]](1)
  private val lastF = new Array[List[F]](1)
  private var hits = 0L
  private var misses = 0L

  private def index(lines: List[L]): Int = 0

  private def dumpStats(): Unit = println(s"Cache for $name: $hits hits, $misses misses")

  def get(lines: List[L]): Option[List[F]] = synchronized {
    val i = index(lines)
    if (lastL.indices.contains(i) && (lastL(i) eq lines)) {
      hits += 1
      //      dumpStats()
      Some(lastF(i))
    } else {
      misses += 1
      //      dumpStats()
      None
    }
  }

  def put(lines: List[L], flow: List[F]): List[F] = synchronized {
    val i = index(lines)
    if (lastL.indices.contains(i)) {
      lastL(i) = lines
      lastF(i) = flow
    }
    flow
  }
}
