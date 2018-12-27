package millfork.test

import millfork.Cpu
import millfork.test.emu.EmuCrossPlatformBenchmarkRun
import org.scalatest.{AppendedClues, FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class MemsetSuite extends FunSuite with Matchers with AppendedClues {

  test("memset $1000") {
    memsetCase(0x1000)
  }
  test("memset $40") {
    memsetCase(0x40)
  }
  test("memset $80") {
    memsetCase(0x80)
  }
  test("memset $100") {
    memsetCase(0x100)
  }
  test("memset $200") {
    memsetCase(0x200)
  }
  test("memset $fff") {
    memsetCase(0xfff)
  }

  def memsetCase(size: Int): Unit = {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Intel8080, Cpu.Z80)(
      "const word SIZE = " + size + """
        | array output [SIZE] @$c000
        | void main () {
        |   pointer p
        |   for p,output.addr,paralleluntil,output.addr+SIZE {
        |     p[0] = 42
        |   }
        | }
      """.stripMargin) { m =>
      for(addr <- 0 until 0x1000) {
        if (addr < size) {
          m.readByte(addr + 0xc000) should equal(42) withClue f"$$$addr%04x"
        } else {
          m.readByte(addr + 0xc000) should equal(0) withClue f"$$$addr%04x"
        }
      }
    }
  }
}