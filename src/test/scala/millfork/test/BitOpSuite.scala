package millfork.test

import millfork.Cpu
import millfork.test.emu.EmuCrossPlatformBenchmarkRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class BitOpSuite extends FunSuite with Matchers {

  test("Word AND") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)("""
        | word output @$c000
        | void main () {
        |   byte b
        |   output = $5555
        |   b = $4E
        |   output &= b
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(0x44))
  }
  test("Long AND and EOR") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)("""
        | long output @$c000
        | void main () {
        |   byte b
        |   word w
        |   output = $55555555
        |   w = $505
        |   output ^= w
        |   b = $4E
        |   output &= b
        | }
      """.stripMargin)(_.readLong(0xc000) should equal(0x40))
  }

  test("Smart bit set/reset") {
    EmuCrossPlatformBenchmarkRun(Cpu.Cmos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)("""
        | byte output @$c000
        | void main () {
        |   output = 5
        |   barrier()
        |   output &= 0xfe
        |   barrier()
        |   output |= 2
        | }
        | noinline void barrier() { }
      """.stripMargin)(_.readLong(0xc000) should equal(6))
  }

  test("Smart bit set/reset 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Cmos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)("""
        | byte output @$c000
        | void main () {
        |   output = 5
        |   output &= 0xfe
        |   output |= 2
        | }
      """.stripMargin)(_.readLong(0xc000) should equal(6))
  }
}
