package millfork.test

import millfork.MathUtils
import millfork.MathUtils.gcd
import org.scalatest.{AppendedClues, FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class MathUtilsSuite extends FunSuite with Matchers with AppendedClues {

  test("GCD") {
    gcd(2, 4) should equal(2)
    gcd(4, 2) should equal(2)
    gcd(5, 1) should equal(1)
    gcd(5, 5) should equal(5)
    gcd(0, 5) should equal(5)
    gcd(5, 0) should equal(5)
    gcd(9, 12) should equal(3)
    gcd(12, 9) should equal(3)
  }

}
