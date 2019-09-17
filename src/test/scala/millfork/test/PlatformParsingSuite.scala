package millfork.test

import millfork.error.Logger
import millfork.{NullLogger, Platform}
import org.scalatest.{AppendedClues, FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class PlatformParsingSuite extends FunSuite with Matchers with AppendedClues {
  test("ranges in zp_bytes should be parsed correctly") {
    implicit val logger: Logger = new NullLogger()
    val b6to9 = Platform.parseNumberOrRange("6-9", 1)
    b6to9 shouldNot contain(5)
    b6to9 should contain(6)
    b6to9 should contain(7)
    b6to9 should contain(8)
    b6to9 should contain(9)
    b6to9 shouldNot contain(10)
    val p6to9 = Platform.parseNumberOrRange("6-9", 2)
    p6to9 shouldNot contain(5)
    p6to9 should contain(6)
    p6to9 shouldNot contain(7)
    p6to9 should contain(8)
    p6to9 shouldNot contain(9)
    p6to9 shouldNot contain(10)
  }

}
