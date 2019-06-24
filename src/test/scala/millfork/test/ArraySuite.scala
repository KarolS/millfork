package millfork.test

import millfork.{Cpu, CpuFamily, OptimizationPresets}
import millfork.assembly.mos.opt.{AlwaysGoodOptimizations, DangerousOptimizations}
import millfork.test.emu._
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class ArraySuite extends FunSuite with Matchers {

  test("Array assignment") {
    val src =
      """
        | array output [3] @$c000
        | array input = [5,6,7]
        | void main () {
        |   copyEntry(0)
        |   copyEntry(1)
        |   copyEntry(2)
        | }
        | void copyEntry(byte index) {
        |   output[index] = input[index]
        | }
      """.stripMargin
    val m = EmuSuperOptimizedRun(src)
    m.readByte(0xc000) should equal(5)
    m.readByte(0xc001) should equal(6)
    m.readByte(0xc002) should equal(7)
    EmuCrossPlatformBenchmarkRun(Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)(src) { m =>
      m.readByte(0xc000) should equal(5)
      m.readByte(0xc001) should equal(6)
      m.readByte(0xc002) should equal(7)
    }

  }
  test("Array assignment with offset") {
    val src =
      """
        | array output [8] @$c000
        | void main () {
        |   byte i
        |   i = 0
        |   while i != 6 {
        |     output[i + 2] = i + 1
        |     output[i] = output[i]
        |     i += 1
        |   }
        | }
      """.stripMargin
    EmuUltraBenchmarkRun(src) { m =>
      m.readByte(0xc002) should equal(1)
      m.readByte(0xc007) should equal(6)
    }
    EmuCrossPlatformBenchmarkRun(Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)(src) { m =>
      m.readByte(0xc002) should equal(1)
      m.readByte(0xc007) should equal(6)
    }
  }

  test("Array assignment with offset 1") {
    val m = new EmuRun(Cpu.StrictMos, Nil, DangerousOptimizations.All ++ OptimizationPresets.Good)(
      """
        | array output [8] @$c000
        | void main () {
        |   byte i
        |   i = 0
        |   while i != 6 {
        |     output[i + 2] = i + 1
        |     output[i] = output[i]
        |     i += 1
        |   }
        | }
      """.stripMargin)
      m.readByte(0xc002) should equal(1)
      m.readByte(0xc007) should equal(6)
  }

  test("Array assignment through a pointer") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)(
      """
        | array output [3] @$c000
        | pointer p
        | void main () {
        |   p = output.addr
        |   byte i
        |   byte ignored
        |   i = 1
        |   word w
        |   w = $105
        |   p[i]:ignored = w
        | }
      """.stripMargin) { m =>
      m.readByte(0xc001) should equal(1)
    }

  }

  test("Array in place math") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)(
      """
        | array output [4] @$c000
        | void main () {
        |   byte i
        |   i = 3
        |   output[i] = 3
        |   output[i + 1 - 1] *= 4
        |   output[3] *= 5
        | }
      """.stripMargin)(_.readByte(0xc003) should equal(60))
  }

  test("Array simple read") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)(
      """
        | byte output @$c000
        | array a[7]
        | void main () {
        |   byte i
        |   i = 6
        |   a[i] = 6
        |   output = a[i]
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(6))
  }

  test("Array simple read 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)(
      """
        | word output @$c000
        | array a[7]
        | void main () {
        |   output = 777
        |   byte i
        |   i = 6
        |   a[i] = 6
        |   output = a[i]
        | }
      """.stripMargin){m =>
      m.readByte(0xc000) should equal(6)
      m.readByte(0xc001) should equal(0)
    }
  }

  test("Pointers") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)(
      """
        | byte output
        |   pointer a
        |   pointer b
        |   pointer c
        | void main () {
        |   setup()
        |   reset()
        | }
        | void setup() {
        |   a = output.addr
        |   b = output.addr
        |   c = output.addr
        | }
        | void reset() {
        |   a[0] = 0
        |   b[0] = 0
        |   c[0] = 0
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(0))

  }

  test("Pointer indexing test") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)(
      """
        | array output [4] @$c000
        |   pointer a
        |   byte i
        | void main () {
        |   setup()
        |   a[i + 1] = 55
        | }
        | void setup() {
        |   a = output.addr
        |   i = 2
        | }
      """.stripMargin)(_.readByte(0xc003) should equal(55))
  }

  test("Syntax") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)(
      """
        | array a = [1, 2, 3]
        | array b = "text" ascii
        | array c = ["text" ascii, 5]
        | void main () {
        | }
      """.stripMargin){m => ()}

  }

  test("Negative subindex") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)(
      """
        |
        | array output [$fff] @$c000
        | void main () {
        |   byte i
        |   output[$100] = 55
        |   i = one()
        |   output[1 - i] = 5
        | }
        | noinline byte one() {return 1}
      """.stripMargin) { m =>
      m.dump(0xbf00, 0x200)(println(_))
      m.readByte(0xc100) should equal(55)
      m.readByte(0xc000) should equal(5)
    }
  }

  test("Word subindex 1") {
    EmuBenchmarkRun(
      """
        |
        | array output [$fff] @$c000
        | void main () {
        |   word i
        |   i = big()
        |   output[$64] = 55
        |   output[i] = 6
        |   output[i] = 5
        | }
        | noinline word big() {return $564}
      """.stripMargin) {m =>
      m.readByte(0xc064) should equal(55)
      m.readByte(0xc564) should equal(5)
    }

  }

  test("Word subindex 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)(
      """
        |
        | array output [$fff] @$c000
        | void main () {
        |   word i
        |   pointer o
        |   o = p()
        |   i = big()
        |   o[$164] = 55
        |   o[i] = 5
        | }
        | noinline word big() {return $564}
        | noinline word p() {return output.addr}
      """.stripMargin) {m =>
      m.readByte(0xc164) should equal(55)
      m.readByte(0xc564) should equal(5)
    }

  }

  test("Array filters") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)(
      """
        | array x = @word [$1144]
        | byte output @$c000
        | void main () {
        |   output = x[0]
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(0x44)
    }
  }

  test("Array filters 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)(
      """
        | array x = @long [$1144]
        | byte output @$c000
        | void main () {
        |   output = x[0]
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(0x44)
    }
  }

  test("Const arrays") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)(
      """
        | const array square = [0, 1, 4, 9, 16, 25, 36, 49, 64]
        | byte five() = 5
        | byte output0 @$c000
        | byte output1 @$c001
        | void main () {
        |   output0 = square[3]
        |   output1 = square[five()]
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(9)
      m.readByte(0xc001) should equal(25)
    }
  }

  test("Writing to const arrays should not compile") {
    ShouldNotCompile(
      """
        | const array a = [0]
        | void main () {
        |   a[0] = 5
        | }
      """.stripMargin)
  }

  test("Struct array initializers") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086)(
      """
        | struct p { byte x, byte y }
        | struct line { p from, p to }
        | struct ratio { int32 num, int32 den }
        | const array data1 @$c000 = @struct [p(2,3), p(4,5)]
        | const array data2 @$c010 = @struct [line(p(6,7), p(8,9))]
        | const array data3 @$c020 = @struct [ratio($666, $777)]
        | void main () { }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(2)
      m.readByte(0xc001) should equal(3)
      m.readByte(0xc002) should equal(4)
      m.readByte(0xc003) should equal(5)
      m.readByte(0xc010) should equal(6)
      m.readByte(0xc011) should equal(7)
      m.readByte(0xc012) should equal(8)
      m.readByte(0xc013) should equal(9)
      m.readByte(0xc020) should equal(0x66)
      m.readByte(0xc021) should equal(6)
      m.readByte(0xc022) should equal(0)
      m.readByte(0xc023) should equal(0)
      m.readByte(0xc024) should equal(0x77)
      m.readByte(0xc025) should equal(7)
      m.readByte(0xc026) should equal(0)
      m.readByte(0xc027) should equal(0)
    }
  }

  test("Local arrays") {
    EmuUnoptimizedRun(
      """
        | byte output @$c000
        | void main () {
        |   array square[5]
        |   square[0] = 1
        |   output = square[0]
        | }
      """.stripMargin).readByte(0xc000) should equal(1)
    ShouldNotCompile(
      """
        | void main () {
        |   array square = [0]
        | }
      """.stripMargin)
    ShouldNotCompile(
      """
        | void f() {
        |   square[1] = 1
        | }
        | void main () {
        |   array square = [0]
        | }
      """.stripMargin)
    EmuUnoptimizedRun(
      """
        | byte output @$c000
        | void main () {
        |   const array square = [1]
        |   output = square[0]
        | }
      """.stripMargin).readByte(0xc000) should equal(1)
  }
}
