package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuCrossPlatformBenchmarkRun, EmuUnoptimizedCrossPlatformRun}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class GotoSuite extends FunSuite with Matchers {

  test("Goto 1") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Cmos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Motorola6809)(
      """
        |
        | byte output @$c000
        |
        | void main() {
        |   byte x
        |   x = 0
        |   if x == 0 {
        |     label a
        |   }
        |   x += 1
        |   if x < 100 {
        |     goto a
        |   }
        |   output = x
        | }
        |
      """.stripMargin){m =>
      m.readByte(0xc000) should equal(100)
    }
  }

  test("Cross-loop goto") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        |
        | byte output @$c000
        |
        | void main() {
        |   byte x
        |   label a
        |   for x : [1,2,3] {
        |     if x == 0 { goto a }
        |     goto b
        |     label b
        |     output = x
        |   }
        | }
        |
      """.stripMargin){m =>
      m.readByte(0xc000) should equal(3)
    }
  }
}
