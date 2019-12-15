package millfork.test

import millfork.Cpu
import millfork.test.emu.EmuCrossPlatformBenchmarkRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class BitOpSuite extends FunSuite with Matchers {

  test("Word AND") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
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
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)("""
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
    EmuCrossPlatformBenchmarkRun(Cpu.Cmos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
        | byte output @$c000
        | void main () {
        |   output = 5
        |   barrier()
        |   output &= 0xfe
        |   barrier()
        |   output |= 2
        | }
        | noinline void barrier() { }
      """.stripMargin)(_.readByte(0xc000) should equal(6))
  }

  test("Smart bit set/reset 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Cmos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
        | byte output @$c000
        | void main () {
        |   output = 5
        |   output &= 0xfe
        |   output |= 2
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(6))
  }
  test("Word AND 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
        | word output @$c000
        | void main () {
        |   word v, w
        |   v = $ee0e
        |   w = $d0dd
        |   barrier()
        |   output = v & w & v & w
        | }
        | noinline void barrier(){}
      """.stripMargin)(_.readWord(0xc000) should equal(0xc00c))
  }

  test("Bit testing optimizations") {
    val code ="""
      | byte output @$c000
      | noinline byte f() = 5
      | noinline bool g(byte x) = x & 1 == 0
      | void main () {
      |   byte x
      |   x = f()
      |
      |   if x & 4 == 0 { x = 0 } else { x = 2 }
      |   output = x
      | }
      |
      """.stripMargin
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(code)(_.readByte(0xc000) should equal(2))
  }

  test("Multiple words") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
        | word output @$c000
        | void main () {
        |   word v, w
        |   v = $ee0e
        |   barrier()
        |   output = (v & $F0F0) | $0303
        | }
        | noinline void barrier(){}
      """.stripMargin)(_.readWord(0xc000) should equal(0xE303))
  }
}
