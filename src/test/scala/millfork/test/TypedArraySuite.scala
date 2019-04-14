package millfork.test

import millfork.test.emu.EmuUnoptimizedCmosRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class TypedArraySuite extends FunSuite with Matchers {

  test("Trivial assignments") {
    val src =
      """
        | enum e {}
        | array(e) output [3] @$c000
        | void main () {
        |   output[0] = e(1)
        |   byte b
        |   b = byte(output[0])
        | }
      """.stripMargin
    val m = EmuUnoptimizedCmosRun(src)
    m.readByte(0xc000) should equal(1)
  }
}
