package millfork.test

import millfork.Cpu
import millfork.test.emu.EmuUnoptimizedCrossPlatformRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class AliasSuite extends FunSuite with Matchers {

  test("Test aliases to subvariables") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        |
        |
        |word output @$c000
        |alias o = output
        |
        |void main() {
        | o.hi = 1
        | o.lo = 2
        |}
        |
        |""".stripMargin){ m =>
      m.readWord(0xc000) should equal(0x102)
    }
  }

  test("Test aliases to pointers") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        |
        |
        |pointer output @$c000
        |alias o = output
        |
        |void main() {
        | o = o.addr
        |}
        |
        |""".stripMargin){ m =>
      m.readWord(0xc000) should equal(0xc000)
    }
  }
}
