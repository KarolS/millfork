package millfork.test

import millfork.test.emu.EmuCmosBenchmarkRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class CmosSuite extends FunSuite with Matchers {

  test("Zero store 1") {
    EmuCmosBenchmarkRun("""
        | word output @$c000
        | void main () {
        |  output = 1
        |  output = 0
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(0))
  }
  test("Zero store 2") {
    EmuCmosBenchmarkRun("""
        | byte output @$c000
        | void main () {
        |  output = 1
        |  output = 0
        |  output += 1
        |  output <<= 1
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(2))
  }
}
