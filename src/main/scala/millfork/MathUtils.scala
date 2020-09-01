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

}
