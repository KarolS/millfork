package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuBenchmarkRun, EmuCrossPlatformBenchmarkRun}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class MacroSuite extends FunSuite with Matchers {

  test("Most basic test") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80)(
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

  test("Macros in assembly") {
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
        |   asm {
        |     + run(a)
        |   }
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(7)
    }
  }
}
