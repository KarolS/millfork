package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuCrossPlatformBenchmarkRun, EmuOptimizedInlinedRun, EmuSizeOptimizedCrossPlatformRun}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class InliningSuite extends FunSuite with Matchers {

  test("Should inline square") {
    EmuSizeOptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80)(
      """
        | import zp_reg
        | byte output @$c000
        | inline byte square(byte x) {
        |   return x * x
        | }
        | void main () {
        |   output = square(6)
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(36))
  }

  test("Should inline <<") {
    EmuSizeOptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80)(
      """
        | byte output @$c000
        | word output2 @$c006
        | inline byte thing(byte x) {
        |   return x << x
        | }
        | inline word sh(word x, byte y) {
        |   return x << y
        | }
        | void main () {
        |   output = thing(6)
        |   output2 = sh(84, 4)
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(6.<<(6).&(0xff))
      m.readWord(0xc006) should equal(84.<<(4).&(0xffff))
    }

  }
}