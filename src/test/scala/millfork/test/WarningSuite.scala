package millfork.test

import millfork.Cpu
import millfork.test.emu.EmuUnoptimizedCrossPlatformRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class WarningSuite extends FunSuite with Matchers {

  test("Various warnings") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80)(
      """
        | void putstrz(pointer p) {}
        | byte output@0xc000
        | byte main (byte x) {
        |  putstrz("a")
        |  byte a
        |  a
        |  5
        |  a *= 0
        |  a <<= 0
        |  output = 4
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(4)
    }
  }
}
