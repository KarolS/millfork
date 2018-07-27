package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuBenchmarkRun, EmuCrossPlatformBenchmarkRun, EmuUnoptimizedCrossPlatformRun}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class NonetSuite extends FunSuite with Matchers {

  test("Nonet operations") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | array output [5] @$c000
        | void main () {
        |   word a
        |   a = $110
        |   output[1] = a >>>> 1
        |   output[2] = a >>>> 2
        |   output[3] = a >>>> 3
        |   output[4] = a >>>> 4
        | }
      """.stripMargin) { m =>
      m.readByte(0xc001) should equal(0x88)
      m.readByte(0xc002) should equal(0x44)
      m.readByte(0xc003) should equal(0x22)
      m.readByte(0xc004) should equal(0x11)
    }
  }

  test("Nonet left shift") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | word output0 @$c000
        | word output1 @$c002
        | word output2 @$c004
        | word output3 @$c006
        | void main () {
        |   byte a
        |   a = 3
        |   output0 = nonet(a << 1)
        |   output1 = nonet(a << 2)
        |   output2 = nonet(a << 6)
        |   output3 = nonet(a << 7)
        | }
      """.stripMargin) { m =>
      m.readWord(0xc000) should equal(0x06)
      m.readWord(0xc002) should equal(0x0C)
      m.readWord(0xc004) should equal(0xC0)
      m.readWord(0xc006) should equal(0x180)
    }
  }

  test("Nonet left shift 2") {
    EmuBenchmarkRun(
      """
        | word output0 @$c000
        | word output1 @$c002
        | word output2 @$c004
        | word output3 @$c006
        | void main () {
        |   byte a
        |   a = 3
        |   output0 = nonet(a << 1)
        |   output1 = nonet(a << 2)
        |   output2 = nonet(a << 6)
        |   output3 = nonet(a << 7)
        | }
      """.stripMargin) { m =>
      m.readWord(0xc000) should equal(0x06)
      m.readWord(0xc002) should equal(0x0C)
      m.readWord(0xc004) should equal(0xC0)
      m.readWord(0xc006) should equal(0x180)
    }
  }

  test("Nonet OR/EXOR") {
    EmuBenchmarkRun(
      """
        | word output0 @$c000
        | word output1 @$c002
        | word output2 @$c004
        | word output3 @$c006
        | void main () {
        |   byte a
        |   output0 = 0
        |   output1 = 0
        |   output2 = $8100
        |   output3 = $8100
        |   a = three()
        |   output0 |= nonet(a << 1)
        |   output1 ^= nonet(a << 2)
        |   output2 |= nonet(a << 6)
        |   output3 ^= nonet(a << 7)
        | }
        | noinline byte three() { return 3 }
      """.stripMargin) { m =>
      m.readWord(0xc000) should equal(0x06)
      m.readWord(0xc002) should equal(0x0C)
      m.readWord(0xc004) should equal(0x81C0)
      m.readWord(0xc006) should equal(0x8080)
    }
  }
}
