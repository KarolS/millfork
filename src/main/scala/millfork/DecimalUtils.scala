package millfork

/**
  * @author Karol Stasiak
  */
object DecimalUtils {

  private def parseNormalToDecimalValue(a: Long): Long = {
    if (a < 0) -parseNormalToDecimalValue(-a)
    var x = a
    var result = 0L
    var multiplier = 1L
    while (x > 0) {
      result += multiplier * (x % 16L)
      x /= 16L
      multiplier *= 10L
    }
    result
  }

  private def storeDecimalValueInNormalRespresentation(a: Long): Long = {
    if (a < 0) -storeDecimalValueInNormalRespresentation(-a)
    var x = a
    var result = 0L
    var multiplier = 1L
    while (x > 0) {
      result += multiplier * (x % 10L)
      x /= 10L
      multiplier *= 16L
    }
    result
  }

  def asDecimal(a: Long, b: Long, f: (Long, Long) => Long): Long =
    storeDecimalValueInNormalRespresentation(f(parseNormalToDecimalValue(a), parseNormalToDecimalValue(b)))
}
