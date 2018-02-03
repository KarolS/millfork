package millfork.test

import millfork.test.emu.EmuBenchmarkRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class MacroSuite extends FunSuite with Matchers {

  test("Most basic test") {
    EmuBenchmarkRun(
      """
        | macro void run(byte x) {
        |    output = x
        | }
        |
        | byte output @$c000
        |
        | void main () {
        |   byte a
        |   a = 7
        |   run(a)
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(7)
    }
  }
}
