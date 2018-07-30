package millfork.test

import millfork.Cpu
import millfork.error.ConsoleLogger
import millfork.test.emu.{EmuBenchmarkRun, EmuCrossPlatformBenchmarkRun}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class BitPackingSuite extends FunSuite with Matchers {

  test("Unpack bits from a byte") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)("""
        | array output[8]
        | word output_addr @$c000
        | void main () {
        |   byte b
        |   output_addr = output.addr
        |   b = $56
        |   barrier()
        |   byte i
        |   for i,0,until,8 {
        |     output[i] = b & 1
        |     b >>= 1
        |   }
        | }
        | void barrier() {}
      """.stripMargin){m =>
      val addr = m.readWord(0xc000)
      m.readByte(addr) should equal(0)
      m.readByte(addr + 1) should equal(1)
      m.readByte(addr + 2) should equal(1)
      m.readByte(addr + 3) should equal(0)
      m.readByte(addr + 4) should equal(1)
      m.readByte(addr + 5) should equal(0)
      m.readByte(addr + 6) should equal(1)
      m.readByte(addr + 7) should equal(0)
    }
  }

  test("Unpack bits from a word") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)("""
        | array output[16]
        | word output_addr @$c000
        | void main () {
        |   word w
        |   output_addr = output.addr
        |   w = $CC56
        |   barrier()
        |   byte i
        |   for i,0,until,16 {
        |     output[i] = w.lo & 1
        |     w >>= 1
        |   }
        | }
        | void barrier() {}
      """.stripMargin){m =>
      val addr = m.readWord(0xc000)
      m.readByte(addr) should equal(0)
      m.readByte(addr + 1) should equal(1)
      m.readByte(addr + 2) should equal(1)
      m.readByte(addr + 3) should equal(0)
      m.readByte(addr + 4) should equal(1)
      m.readByte(addr + 5) should equal(0)
      m.readByte(addr + 6) should equal(1)
      m.readByte(addr + 7) should equal(0)
      m.readByte(addr + 8) should equal(0)
      m.readByte(addr + 9) should equal(0)
      m.readByte(addr + 10) should equal(1)
      m.readByte(addr + 11) should equal(1)
      m.readByte(addr + 12) should equal(0)
      m.readByte(addr + 13) should equal(0)
      m.readByte(addr + 14) should equal(1)
      m.readByte(addr + 15) should equal(1)
    }
  }

  test("Pack bits into byte") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)("""
        | byte output @$C000
        | array input = [$F0, 1, 0, $41, $10, 1, $61, 0]
        | void main () {
        |   byte i
        |   output = 0
        |   for i,0,until,8 {
        |     output <<= 1
        |     output |= input[i] & 1
        |   }
        | }
      """.stripMargin){m =>
      m.readByte(0xc000) should equal(0x56)
    }
  }

  test("Pack bits into word") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)("""
        | word output @$C000
        | array input = [$F0, 1, 0, $41, $10, 1, $61, 0,
        |                1, 1, 0, 0, 0, 0, 1, 1]
        | void main () {
        |   byte i
        |   output = 0
        |   for i,0,until,16 {
        |     output <<= 1
        |     output |= input[i] & 1
        |   }
        | }
      """.stripMargin){m =>
      m.readWord(0xc000) should equal(0x56C3)
    }
  }

  test("Pack bits into byte using plus") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)("""
        | byte output @$C000
        | array input = [$F0, 1, 0, $41, $10, 1, $61, 0]
        | void main () {
        |   byte i
        |   output = 0
        |   for i,0,until,8 {
        |     output <<= 1
        |     output += (input[i] & 1)
        |   }
        | }
      """.stripMargin){m =>
      m.readByte(0xc000) should equal(0x56)
    }
  }

  test("Reverse byte") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)("""
        | word output_addr @$C000
        | void main () {
        |   byte i
        |   byte input
        |   byte output
        |   output_addr = output.addr
        |   input = $5A
        |   output = 0
        |   for i,0,paralleluntil,8 {
        |     output <<= 1
        |     output |= input & 1
        |     input >>= 1
        |   }
        | }
      """.stripMargin){m =>
      val addr = m.readWord(0xc000)
      m.readByte(addr) should equal(0x5A)
    }
  }

  test("Reverse byte 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)("""
        | byte output_real @$C000
        | void main () {
        |   byte i
        |   byte input
        |   byte output
        |   input = $5A
        |   output = 0
        |   for i,0,paralleluntil,8 {
        |     output <<= 1
        |     output |= input & 1
        |     input >>= 1
        |   }
        |   output_real = output
        | }
      """.stripMargin){m =>
      m.readByte(0xc000) should equal(0x5A)
    }
  }

  test("Reverse word") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)("""
        | word output_addr @$C000
        | void main () {
        |   byte i
        |   word input
        |   word output
        |   output_addr = output.addr
        |   input = $5AC1
        |   output = 0
        |   for i,0,paralleluntil,16 {
        |     output <<= 1
        |     output.lo |= input.lo & 1
        |     input >>= 1
        |   }
        | }
      """.stripMargin){m =>
      val addr = m.readWord(0xc000)
      m.readWord(addr) should equal(0x835A)
    }
  }
}
