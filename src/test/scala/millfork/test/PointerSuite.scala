package millfork.test

import millfork.Cpu
import millfork.test.emu.EmuCrossPlatformBenchmarkRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class PointerSuite extends FunSuite with Matchers {

  test("Pointers outside zeropage") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Sixteen, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | pointer p @$c004
        | array output[2] @$c000
        | void main() {
        |   p = output.addr
        |   output[0] = 45
        |   p[1] = p[0]
        | }
      """.stripMargin) { m =>
      m.readByte(0xc001) should equal(45)
    }
  }

  test("Pointers on stack") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Sixteen, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | array output[2] @$c000
        | void main() {
        |   stack pointer p
        |   p = output.addr
        |   output[0] = 45
        |   p[1] = p[0]
        | }
      """.stripMargin) { m =>
      m.readByte(0xc001) should equal(45)
    }
  }

}
