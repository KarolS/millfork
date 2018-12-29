package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuBenchmarkRun, EmuCrossPlatformBenchmarkRun}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class SecondAssemblyOptimizationSuite extends FunSuite with Matchers {

  test("Add-shift-add") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Cmos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | byte output @$c000
        | void main () {
        |   byte a
        |   a = two()
        |   output = ((a + 3) << 2) + 9
        | }
        | byte two() { return 2 }
      """.stripMargin) { m => m.readByte(0xc000) should equal(29) }
  }

  test("And-shift-and") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Cmos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | byte output @$c000
        | void main () {
        |   byte a
        |   a = ee()
        |   output = ((a & $dd) << 1) & $55
        | }
        | byte ee() { return $ee }
      """.stripMargin) { m => m.readByte(0xc000) should equal(0x10) }
  }

  test("Add with limit") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Cmos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | byte output @$c000
        | const byte start = 5
        | const byte limit = 234
        | void main () {
        |   output += 1
        |   if output == limit {
        |     output = start
        |   }
        | }
      """.stripMargin) { m => m.readByte(0xc000) should equal(1) }
  }

  test("User register instead of stack") {
    EmuBenchmarkRun(
      """
        | array output [4] @$c000
        | void main () {
        |   output[0] = double(2)
        | }
        | asm byte double(byte a) {
        |   ? asl
        |   ? pha
        |     lda output
        |   ? pla
        |   ? rts
        | }
      """.stripMargin) { m => m.readByte(0xc000) should equal(4) }
  }

  test("Index register usage") {
    EmuBenchmarkRun(
      """
        | array palette = [0, 2, 8, 7, 1]
        | array reverse_palette [16] @$c000
        | array __screen[1000]
        | array c64_color_ram[1000]
        | void main () {
        |    byte i
        |    for i,0,paralleluntil,palette.length {
        |        reverse_palette[palette[i]] = i
        |    }
        |    for i,0,paralleluntil,250 {
        |        __screen[000+i] = 160
        |        __screen[250+i] = 160
        |        __screen[500+i] = 160
        |        __screen[750+i] = 160
        |    }
        |    for i,0,paralleluntil,250 {
        |        c64_color_ram[000+i] = 0
        |        c64_color_ram[250+i] = 0
        |        c64_color_ram[500+i] = 0
        |        c64_color_ram[750+i] = 0
        |    }
        |    for i,0,paralleluntil,40 {
        |        c64_color_ram[960+i] = 1
        |    }
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(0)
      m.readByte(0xc002) should equal(1)
      m.readByte(0xc008) should equal(2)
      m.readByte(0xc007) should equal(3)
      m.readByte(0xc001) should equal(4)
    }
  }
}
