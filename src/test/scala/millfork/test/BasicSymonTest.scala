package millfork.test

import millfork.test.emu.EmuUnoptimizedRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class BasicSymonTest extends FunSuite with Matchers {
  test("Empty test") {
    EmuUnoptimizedRun(
      """
        | void main () {
        |
        | }
      """.stripMargin)
  }

  test("Byte assignment") {
    EmuUnoptimizedRun(
      """
        | byte output @$c000
        | void main () {
        |  output = (1)
        | }
      """.stripMargin).readByte(0xc000) should equal(1)
  }
}
