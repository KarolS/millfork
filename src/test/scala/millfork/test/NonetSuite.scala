package millfork.test

import millfork.test.emu.EmuUnoptimizedRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class NonetSuite extends FunSuite with Matchers {

  test("Nonet operations") {
    val m = EmuUnoptimizedRun(
      """
        | array output [3] @$c000
        | array input = [5,6,7]
        | void main () {
        |   word a
        |   a = $110
        |   output[1] = a >>>> 1
        |   output[2] = a >>>> 2
        | }
        | void copyEntry(byte index) {
        |   output[index] = input[index]
        | }
      """.stripMargin)
      m.readByte(0xc001) should equal(0x88)
      m.readByte(0xc002) should equal(0x44)
  }
}
