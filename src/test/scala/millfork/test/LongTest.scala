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
  test("Long addition 3") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | long output @$c000
        | void main () {
        |   output = 0
        |   output += f()
        |   output += $8000
        |   output += $8
        | }
        | long f() {
        |   return $50000
        | }
      """.stripMargin) { m =>
      m.readLong(0xc000) should equal(0x58008)
    }
  }
  test("Extralong addition") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | int128 output @$c000
        | void main () {
        |   output = 0
        |   output += f()
        |   output += $8000
        |   output += $8
        | }
        | int128 f() {
        |   return $50000
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

  test("Returning long") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | long output @$c000
        | void main () {
        |   output = l($91929394)
        | }
        | long l(long param) {
        |   return param
        | }
      """.stripMargin) { m =>
      m.readLong(0xc000) should equal(0x91929394)
    }
  }

  test("Various combinations involving promotions") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | long output0 @$c000
        | long output1 @$c004
        | long output2 @$c008
        | long output3 @$c00c
        | long output4 @$c010
        | long output5 @$c014
        | long output6 @$c018
        |
        | farword f0 @$c020
        | farword f1 @$c023
        | farword f2 @$c026
        | void main () {
        |   output0 = ll($91929394)
        |   output1 = lf($929394)
        |   output2 = ff($929394)
        |   output3 = lw($9394)
        |   output4 = fw($9394)
        |   output5 = lb($94)
        |   output6 = fb($94)
        |
        |   f0 = ff($929394)
        |   f1 = fw($9394)
        |   f2 = fb($94)
        | }
        | long ll(long param) {
        |   return param
        | }
        | long lf(farword param) {
        |   return param
        | }
        | long lw(word param) {
        |   return param
        | }
        | long lb(byte param) {
        |   return param
        | }
        | farword ff(farword param) {
        |   return param
        | }
        | farword fw(word param) {
        |   return param
        | }
        | farword fb(byte param) {
        |   return param
        | }
      """.stripMargin) { m =>
      m.readLong(0xc000) should equal(0x91929394)
      m.readLong(0xc004) should equal(0x929394)
      m.readLong(0xc008) should equal(0x929394)
      m.readLong(0xc00c) should equal(0x9394)
      m.readLong(0xc010) should equal(0x9394)
      m.readLong(0xc014) should equal(0x94)
      m.readLong(0xc018) should equal(0x94)
      m.readMedium(0xc020) should equal(0x929394)
      m.readMedium(0xc023) should equal(0x9394)
      m.readMedium(0xc026) should equal(0x94)
    }
  }

  test("Larger than long") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | int64 output0 @$c000
        | int64 output1 @$c008
        | int64 output2 @$c010
        | int64 output3 @$c018
        | int64 output4 @$c020
        |
        | void main () {
        |   output0 = xl($91929394)
        |   output1 = xf($929394)
        |   output2 = xw($9394)
        |   output3 = xb($94)
        |   output4 = xx($91929394)
        | }
        | int64 xl(long param) {
        |   return param
        | }
        | int64 xf(farword param) {
        |   return param
        | }
        | int64 xw(word param) {
        |   return param
        | }
        | int64 xb(byte param) {
        |   return param
        | }
        | int64 xx(int64 param) {
        |   param.b4 += 1
        |   param.b5 += 1
        |   param.b6 += 1
        |   param.b7 += 1
        |   return param
        | }
      """.stripMargin) { m =>
      m.readLong(0xc000) should equal(0x91929394)
      m.readLong(0xc008) should equal(0x929394)
      m.readLong(0xc010) should equal(0x9394)
      m.readLong(0xc018) should equal(0x94)

      m.readLong(0xc004) should equal(0)
      m.readLong(0xc00c) should equal(0)
      m.readLong(0xc014) should equal(0)
      m.readLong(0xc01c) should equal(0)

      m.readLong(0xc020) should equal(0x91929394)
      m.readLong(0xc024) should equal(0x01010101)
    }
  }
}
