package millfork.test

import millfork.Cpu
import millfork.test.emu.EmuCrossPlatformBenchmarkRun
import org.scalatest.{AppendedClues, FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class MemsetSuite extends FunSuite with Matchers with AppendedClues {

  test("memset pointer $1000") {
    memsetPointerCase(0x1000)
  }
  test("memset pointer $40") {
    memsetPointerCase(0x40)
  }
  test("memset pointer $80") {
    memsetPointerCase(0x80)
  }
  test("memset pointer $100") {
    memsetPointerCase(0x100)
  }
  test("memset pointer $200") {
    memsetPointerCase(0x200)
  }
  test("memset pointer $fff") {
    memsetPointerCase(0xfff)
  }

  test("memset array $1000") {
    memsetArrayCase(0x1000)
  }
  test("memset array $40") {
    memsetArrayCase(0x40)
  }
  test("memset array $80") {
    memsetArrayCase(0x80)
  }
  test("memset array $100") {
    memsetArrayCase(0x100)
  }
  test("memset array $200") {
    memsetArrayCase(0x200)
  }
  test("memset array $fff") {
    memsetArrayCase(0xfff)
  }

  def memsetPointerCase(size: Int): Unit = {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Intel8080, Cpu.Z80, Cpu.Intel8086)(
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

  def memsetArrayCase(size: Int): Unit = {
    val t = if (size < 256) "byte" else "word"
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Intel8080, Cpu.Z80, Cpu.Intel8086)(s"""
        | const $t SIZE = $size
        | array output [SIZE] @$$c000
        | void main () {
        |   $t i
        |   for i,0,paralleluntil,SIZE {
        |     output[i] = 42
        |   }
        | }
      """.stripMargin) { m =>
      for (addr <- 0 until 0x1000) {
        if (addr < size) {
          m.readByte(addr + 0xc000) should equal(42) withClue f"$$$addr%04x"
        } else {
          m.readByte(addr + 0xc000) should equal(0) withClue f"$$$addr%04x"
        }
      }
    }
  }

  test ("Tricky memset") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Intel8080, Cpu.Z80, Cpu.Intel8086)("""
        | const word SIZE = $800
        | array output [SIZE] @$c000
        | void main () {
        |   pointer p
        |   for p,output.addr,paralleluntil,output.addr+SIZE {
        |     p[0] = p.lo
        |   }
        | }
      """.stripMargin) { m =>
      for (addr <- 0 until 0x1000) {
        if (addr < 0x800) {
          m.readByte(addr + 0xc000) should equal(addr & 0xff) withClue f"$$$addr%04x"
        } else {
          m.readByte(addr + 0xc000) should equal(0) withClue f"$$$addr%04x"
        }
      }
    }
  }
}