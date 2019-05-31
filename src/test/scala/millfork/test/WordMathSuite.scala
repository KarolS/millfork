package millfork.test
import millfork.Cpu
import millfork.test.emu.{EmuBenchmarkRun, EmuCmosBenchmarkRun, EmuCrossPlatformBenchmarkRun}
import org.scalatest.{AppendedClues, FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class WordMathSuite extends FunSuite with Matchers with AppendedClues {

  test("Word addition") {
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)("""
        | word output @$c000
        | word a
        | void main () {
        |  a = 640
        |  output = a
        |  output += a
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(1280))
  }

  test("Cast word addition") {
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)("""
        | byte output @$c000
        | word a
        | void main () {
        |  output = add(155, 166)
        | }
        | byte add(byte a, byte b) {
        |   return hi(word(a) + word(b))
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(1))
  }

  test("Word subtraction") {
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)("""
        | word output @$c000
        | word a
        | void main () {
        |  a = 640
        |  output = 740
        |  output -= a
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(100))
  }

  test("Word subtraction 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Cmos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)("""
        | word output @$c000
        | word a
        | void main () {
        |  a = 640
        |  output = a
        |  output -= 400
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(240))
  }

  test("Word subtraction 3") {
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Cmos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)("""
        | word output @$c000
        | word a
        | void main () {
        |  a = 640
        |  f()
        |  output = a - f()
        | }
        | word f() {
        |   return 400
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(240))
  }

  test("Byte-to-word addition") {
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)("""
        | word output @$c000
        | word pair
        | void main () {
        |  pair = $A5A5
        |  pair.lo = 1
        |  output = 640
        |  output += pair.lo
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(641))
  }

  test("Literal addition") {
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)("""
        | word output @$c000
        | void main () {
        |  output = 640
        |  output += -0050
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(590))
  }

  test("Array element addition") {
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)("""
        | word output @$c000
        | word pair
        | array b[2]
        | void main () {
        |  byte i
        |  i = 1
        |  b[1] = 5
        |  pair = $A5A5
        |  pair.lo = 1
        |  output = 640
        |  output += b[i]
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(645))
  }

  test("nesdev.com example") {
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)("""
        | byte output @$c000
        | byte tile @$C3A6
        | array map [256] @$c300
        | array b[2]
        | void main () {
        | tile = 77
        |   output = get(5, 6)
        | }
        | byte get(byte mx, byte my) {
        |   pointer p
        |   p = mx
        |   p <<= 5
        |   p += map
        |   return p[my]
        | }
      """.stripMargin) { m =>
      m.readByte(0xc3a6) should equal(77)
    }
  }

  test("nesdev.com example 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)("""
        | byte output @$c000
        | byte tile @$C3A6
        | array map [256] @$c300
        | array b[2]
        | void main () {
        |   tile = 77
        |   output = get(5, 6)
        | }
        | byte get(byte mx, byte my) {
        |   return map[(word(mx) << 5) + my]
        | }
      """.stripMargin){ m =>
      m.readByte(0xc3a6) should equal(77)
    }
  }

  test("hi()/lo()") {
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)("""
        | array output [7] @$c000
        | void main () {
        |   output[0] = lo(33)
        |   output[1] = hi(33)
        |   output[2] = hi(w($504))
        |   output[3] = lo(w($209))
        |   output[4] = hi(s(-2))
        |   output[5] = lo(s(-2))
        |   output[6] = hi(b(200) + b(200))
        | }
        | word w(word w) {
        |   return w
        | }
        | byte b(byte b) {
        |   return b
        | }
        | sbyte s(sbyte s) {
        |   return s
        | }
      """.stripMargin){ m =>
      m.readByte(0xc000) should equal(33)
      m.readByte(0xc001) should equal(0)
      m.readByte(0xc002) should equal(5)
      m.readByte(0xc003) should equal(9)
      m.readByte(0xc004) should equal(255)
      m.readByte(0xc005) should equal(254)
      m.readByte(0xc006) should equal(0)
    }
  }

  test("Word addition 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)("""
        | word output @$c000
        | void main () {
        |   word v
        |   v = w($482)
        |   output = v + w($482) - 3
        | }
        | noinline word w(word w) {
        |   return w
        | }
      """.stripMargin){ m =>
      m.readWord(0xc000) should equal(0x901)
    }
  }

  test("Word addition 3") {
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)("""
        | word output @$c000
        | void main () {
        |   byte c
        |   c = b($82)
        |   output = $482 + c
        | }
        | noinline byte b(byte b) {
        |   return b
        | }
      """.stripMargin){ m =>
      m.readWord(0xc000) should equal(0x504)
    }
  }

  test("Word addition 4") {
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)("""
        | word output @$c000
        | void main () {
        |   word v
        |   word u
        |   v = w($308)
        |   u = w($601)
        |   barrier()
        |   output = u + v
        | }
        | noinline void barrier() { }
        | noinline word w(word w) {
        |   return w
        | }
      """.stripMargin){ m =>
      m.readWord(0xc000) should equal(0x909)
    }
  }

  test("Word bit ops 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)("""
        | word output @$c000
        | void main () {
        |   word v
        |   v = w($692)
        |   output = (v & w($ca2)) | 3
        | }
        | noinline word w(word w) {
        |   return w
        | }
      """.stripMargin){ m =>
      m.readWord(0xc000) should equal(0x483)
    }
  }

  test("Word shift") {
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)("""
        | word output @$c000
        | void main () {
        |   word v
        |   v = w($123)
        |   barrier()
        |   output = v << 1
        | }
        | noinline void barrier() { }
        | noinline word w(word w) {
        |   return w
        | }
      """.stripMargin){ m =>
      m.readWord(0xc000) should equal(0x246)
    }
  }

  test("Word shift 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)("""
        | word output @$c000
        | void main () {
        |   output = five()
        |   output <<= 5
        | }
        | noinline byte five() {
        |   return 5
        | }
      """.stripMargin){ m =>
      m.readWord(0xc000) should equal(5 * 32)
    }
  }

  test("Word shift 3") {
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)("""
        | word output @$c000
        | void main () {
        |   output = five()
        |   output <<= 8
        | }
        | noinline byte five() {
        |   return 5
        | }
      """.stripMargin){ m =>
      m.readWord(0xc000) should equal(5 * 256)
    }
  }

  test("Word shift 4") {
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)("""
        | word output @$c000
        | void main () {
        |   output = five()
        |   output <<= 7
        | }
        | noinline byte five() {
        |   return 5
        | }
      """.stripMargin){ m =>
      m.readWord(0xc000) should equal(5 * 128)
    }
  }

  test("Word shift 5") {
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)("""
        | word output @$c000
        | void main () {
        |   output = five()
        |   output <<= 1
        |   output >>= 1
        | }
        | noinline byte five() {
        |   return 5
        | }
      """.stripMargin){ m =>
      m.readWord(0xc000) should equal(5)
    }
  }

  test("Word multiplication 5") {
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)("""
        | word output @$c000
        | void main () {
        |   output = alot()
        |   output *= five()
        | }
        | noinline word alot() {
        |   return 4532
        | }
        | noinline byte five() {
        |   return 5
        | }
        | import zp_reg
      """.stripMargin){ m =>
      m.readWord(0xc000) should equal(4532 * 5)
    }
  }

  test("Word multiplication optimization") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)("""
        | word output @$c000
        | void main () {
        |   output = alot()
        |   output *= two()
        |   output *= four()
        | }
        | noinline word alot() {
        |   return 4532
        | }
        | inline byte four() {
        |   return 4
        | }
        | inline byte two() {
        |   return 2
        | }
        | import zp_reg
      """.stripMargin){ m =>
      m.readWord(0xc000) should equal(4532 * 8)
    }
  }

  test("In-place word/byte multiplication") {
    multiplyCase1(0, 0)
    multiplyCase1(0, 1)
    multiplyCase1(0, 2)
    multiplyCase1(0, 5)
    multiplyCase1(1, 0)
    multiplyCase1(5, 0)
    multiplyCase1(7, 0)
    multiplyCase1(2, 5)
    multiplyCase1(7, 2)
    multiplyCase1(100, 2)
    multiplyCase1(1000, 2)
    multiplyCase1(54, 4)
    multiplyCase1(2, 100)
    multiplyCase1(500, 50)
    multiplyCase1(4, 54)
  }

  private def multiplyCase1(x: Int, y: Int): Unit = {
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8086)(
      s"""
         | import zp_reg
         | word output @$$c000
         | void main () {
         |  output = $x
         |  output *= $y
         | }
          """.
        stripMargin)(_.readWord(0xc000) should equal(x * y) withClue s"$x * $y")
  }

  test("Not-in-place word/byte multiplication") {
    multiplyCase2(0, 0)
    multiplyCase2(0, 1)
    multiplyCase2(0, 2)
    multiplyCase2(0, 5)
    multiplyCase2(1, 0)
    multiplyCase2(5, 0)
    multiplyCase2(7, 0)
    multiplyCase2(2, 5)
    multiplyCase2(7, 2)
    multiplyCase2(100, 2)
    multiplyCase2(1000, 2)
    multiplyCase2(54, 4)
    multiplyCase2(2, 100)
    multiplyCase2(500, 50)
    multiplyCase2(4, 54)
    multiplyCase2(1, 10)
    multiplyCase2(12, 10)
    multiplyCase2(123, 10)
    multiplyCase2(1234, 10)
  }

  private def multiplyCase2(x: Int, y: Int): Unit = {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086)(
      s"""
         | import zp_reg
         | word output1 @$$c000
         | word output2 @$$c004
         | word tmp
         | noinline void init() {
         |  tmp = $x
         | }
         | void main () {
         |  init()
         |  output1 = $y * tmp
         |  output2 = tmp * $y
         | }
          """.
        stripMargin) { m =>
      m.readWord(0xc000) should equal(x * y) withClue s"$y * $x"
      m.readWord(0xc004) should equal(x * y) withClue s"$x * $y"
    }
  }
}
