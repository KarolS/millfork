package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuBenchmarkRun, EmuCrossPlatformBenchmarkRun}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class LongTest extends FunSuite with Matchers {

  test("Long assignment") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | long output4 @$c000
        | long output2 @$c004
        | long output1 @$c008
        | void main () {
        |   output4 = $11223344
        |   output2 = $11223344
        |   output1 = $11223344
        |   output2 = $7788
        |   output1 = $55
        | }
      """.stripMargin) { m =>
      m.readLong(0xc000) should equal(0x11223344)
      m.readLong(0xc004) should equal(0x7788)
      m.readLong(0xc008) should equal(0x55)
    }
  }
  test("Long assignment 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | long output4 @$c000
        | long output2 @$c004
        | word output1 @$c008
        | void main () {
        |   word w
        |   byte b
        |   w = $7788
        |   b = $55
        |   output4 = $11223344
        |   output2 = $11223344
        |   output1 = $3344
        |   output2 = w
        |   output1 = b
        | }
      """.stripMargin) { m =>
      m.readLong(0xc000) should equal(0x11223344)
      m.readLong(0xc004) should equal(0x7788)
      m.readLong(0xc008) should equal(0x55)
    }
  }
  test("Long addition") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | long output @$c000
        | void main () {
        |   word w
        |   long l
        |   byte b
        |   w = $8000
        |   b = $8
        |   l = $50000
        |   output = 0
        |   output += l
        |   output += w
        |   output += b
        | }
      """.stripMargin) { m =>
      m.readLong(0xc000) should equal(0x58008)
    }
  }
  test("Long addition 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | long output @$c000
        | void main () {
        |   output = 0
        |   output += $50000
        |   output += $8000
        |   output += $8
        | }
      """.stripMargin) { m =>
      m.readLong(0xc000) should equal(0x58008)
    }
  }
  test("Long subtraction") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | long output @$c000
        | void main () {
        |   word w
        |   long l
        |   byte b
        |   w = $8000
        |   b = $8
        |   l = $50000
        |   output = $58008
        |   output -= l
        |   output -= w
        |   output -= b
        | }
      """.stripMargin) { m =>
      m.readLong(0xc000) should equal(0)
    }
  }
  test("Long subtraction 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | long output @$c000
        | void main () {
        |   output = $58008
        |   output -= $50000
        |   output -= $8000
        |   output -= $8
        | }
      """.stripMargin) { m =>
      m.readLong(0xc000) should equal(0)
    }
  }
  test("Long subtraction 3") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | long output @$c000
        | void main () {
        |   output = $58008
        |   output -= w()
        |   output -= b()
        | }
        | byte b() {
        |   return $8
        | }
        | word w() {
        |   return $8000
        | }
      """.stripMargin) { m =>
      m.readLong(0xc000) should equal(0x50000)
    }
  }

  test("Long AND") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | long output @$c000
        | void main () {
        |   output = $FFFFFF
        |   output &= w()
        |   output &= b()
        | }
        | byte b() {
        |   return $77
        | }
        | word w() {
        |   return $CCCC
        | }
      """.stripMargin) { m =>
      m.readLong(0xc000) should equal(0x44)
    }
  }

  test("Long INC/DEC") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | long output0 @$c000
        | long output1 @$c004
        | long output2 @$c008
        | long output3 @$c00c
        | long output4 @$c010
        | long output5 @$c014
        | long output6 @$c018
        | void main () {
        |   output0 = 0
        |   output1 = $FF
        |   output2 = $FFFF
        |   output3 = $FF00
        |   output4 = $FF00
        |   output5 = $10000
        |   output6 = 0
        |   barrier()
        |   output0 += 1
        |   output1 += 1
        |   output2 += 1
        |   output3 += 1
        |   output4 -= 1
        |   output5 -= 1
        |   output6 -= 1
        | }
        | void barrier() {
        | }
      """.stripMargin) { m =>
      m.readLong(0xc000) should equal(1)
      m.readLong(0xc004) should equal(0x100)
      m.readLong(0xc008) should equal(0x10000)
      m.readLong(0xc00c) should equal(0xff01)
      m.readLong(0xc010) should equal(0xfeff)
      m.readLong(0xc014) should equal(0xffff)
      m.readLong(0xc018) should equal(0xffffffff)
    }
  }
}
