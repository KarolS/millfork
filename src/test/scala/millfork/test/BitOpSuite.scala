package millfork.test

import millfork.test.emu.EmuBenchmarkRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class BitOpSuite extends FunSuite with Matchers {

  test("Word AND") {
    EmuBenchmarkRun("""
        | word output @$c000
        | void main () {
        |   byte b
        |   output = $5555
        |   b = $4E
        |   output &= b
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(0x44))
  }
  test("Long AND and EOR") {
    EmuBenchmarkRun("""
        | long output @$c000
        | void main () {
        |   byte b
        |   word w
        |   output = $55555555
        |   w = $505
        |   output ^= w
        |   b = $4E
        |   output &= b
        | }
      """.stripMargin)(_.readLong(0xc000) should equal(0x40))
  }
}
