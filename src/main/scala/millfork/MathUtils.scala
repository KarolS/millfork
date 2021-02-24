package millfork

/**
  * @author Karol Stasiak
  */
object MathUtils {

  @scala.annotation.tailrec
  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  def lcm(a: Int, b: Int): Int =
    (a.toLong * b.toLong / gcd(a, b)).toInt


  def collapseConsecutiveInts(nums :Seq[Int], format: Int=>String): String = {
    val result = Seq.newBuilder[String]
    var lastIntervalStart = -2
    var lastIntervalEnd = -2
    def flushInterval(): Unit = {
      if (lastIntervalEnd >= 0) {
        if (lastIntervalStart == lastIntervalEnd) {
          result += format(lastIntervalEnd)
        } else {
          result += format(lastIntervalStart) + "-" + format(lastIntervalEnd)
        }
      }
    }
    for(n <- nums) {
      if (n != lastIntervalEnd + 1) {
        flushInterval()
        lastIntervalStart = n
      }
      lastIntervalEnd = n
    }
    flushInterval()
    result.result().mkString(",")
  }
}
