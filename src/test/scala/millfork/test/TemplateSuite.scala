package millfork.test

import millfork.test.emu.EmuUnoptimizedCmosRun
import org.scalatest.{FunSuite, Matchers}
/**
  * @author Karol Stasiak
  */
class TemplateSuite extends FunSuite with Matchers {

  test("Template test") {
    val src =
      """
        | import silly_template<byte, a, 1>
        | import silly_template<byte, b, 2>
        | import silly_template<byte, b, $2>
        | array output [30] @$c000
        | void main() {
        |   output[0] = a
        |   output[1] = b
        | }
      """.stripMargin
    val m = EmuUnoptimizedCmosRun(src)
    m.readByte(0xc000) should equal(1)
    m.readByte(0xc001) should equal(2)
  }

  test("same() test") {
    val src =
      """
        |
        | byte output @$c000
        | void main() {
        | #if same(a,a)
        |   output = 1
        | #endif
        | #if same(a,b)
        |   output = 2
        | #endif
        | }
      """.stripMargin
    val m = EmuUnoptimizedCmosRun(src)
    m.readByte(0xc000) should equal(1)
  }
}

