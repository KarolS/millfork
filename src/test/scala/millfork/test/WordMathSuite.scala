package millfork.test
import millfork.Cpu
import millfork.test.emu.{EmuBenchmarkRun, EmuCmosBenchmarkRun, EmuCrossPlatformBenchmarkRun, EmuUnoptimizedCrossPlatformRun}
import org.scalatest.{AppendedClues, FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class WordMathSuite extends FunSuite with Matchers with AppendedClues {

  test("Word addition") {
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Intel8085, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
        | word output @$c000
        | word a
        | void main () {
        |  a = 640
        |  output = a
        |  output += a
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(1280))
  }

  test("Addition that was buggy") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)("""
        | word output @$c000
        | array X [$1000] align($100)
        | void main () {
        |  output = f(1)
        | }
        | noinline word f(byte I) {
        |   word S
        |   S = X + nonet(I << 1)
        |   return S
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(0x302))
  }

  test("Cast word addition") {
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Intel8085, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
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
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Intel8085, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
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
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Cmos, Cpu.Z80, Cpu.Intel8080, Cpu.Intel8085, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
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
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Cmos, Cpu.Z80, Cpu.Intel8080, Cpu.Intel8085, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
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
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
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
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
        | word output @$c000
        | void main () {
        |  output = 640
        |  output += -0050
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(590))
  }

  test("Array element addition") {
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
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
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
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
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
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
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
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
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
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
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
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
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
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

  test("Word addition 5") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Sixteen, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
        | word output @$c000
        | void main () {
        |   word v
        |   word u
        |   v = $308
        |   output = $102
        |   barrier()
        |   output = (output.lo:output.hi) + v
        | }
        | noinline void barrier() { }
      """.stripMargin){ m =>
      m.readWord(0xc000) should equal(0x509)
    }
  }

  test("Word addition 6") {
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Intel8085, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
        | byte output @$c000
        | void main () {
        |  signed16 p
        |  p = f()
        |  output = lo(-p)
        | }
        | noinline signed16 f() = 6
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal((-6) & 0xff)
    }
  }

  test("Word bit ops 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
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
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
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
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
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
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
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
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
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
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
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
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
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
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
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
    EmuCrossPlatformBenchmarkRun(Cpu.Sixteen, Cpu.Mos, Cpu.Z80, Cpu.Intel8086, Cpu.Motorola6809)(
      s"""
         | import zp_reg
         | word output @$$c000
         | void main () {
         |  output = $x
         |  output *= $y
         | }
          """.
        stripMargin)(_.readWord(0xc000) should equal(x * y) withClue s"= $x * $y")
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
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086, Cpu.Motorola6809)(
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
      m.readWord(0xc000) should equal(x * y) withClue s"= $y * $x"
      m.readWord(0xc004) should equal(x * y) withClue s"= $x * $y"
    }
  }

  test("Word division 1") {
    divisionCase1(0, 1)
    divisionCase1(1, 1)
    divisionCase1(1, 5)
    divisionCase1(6, 5)
    divisionCase1(420, 11)
    divisionCase1(1210, 11)
    divisionCase1(35000, 45)
    divisionCase1(127, 127)
    divisionCase1(128, 127)
    divisionCase1(51462, 1)
  }

  test("Word division 1 – large divisor") {
    divisionCase1(127, 128)
    divisionCase1(128, 128)
    divisionCase1(45323, 250)
    divisionCase1(4530, 200)
    divisionCase1(9039, 247)
  }

  private def divisionCase1(x: Int, y: Int): Unit = {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      s"""
         | import zp_reg
         | word output_q1 @$$c000
         | word output_m1 @$$c002
         | word output_q2 @$$c004
         | word output_m2 @$$c006
         | void main () {
         |  word a
         |  a = f()
         |  output_q1 = a / $y
         |  output_m1 = a %% $y
         |  output_q2 = a
         |  output_m2 = a
         |  output_q2 /= $y
         |  output_m2 %%= $y
         | }
         | word f() = $x
          """.
        stripMargin) { m =>
      m.readWord(0xc000) should equal(x / y) withClue s"= $x / $y"
      m.readWord(0xc002) should equal(x % y) withClue s"= $x %% $y"
      m.readWord(0xc004) should equal(x / y) withClue s"= $x / $y"
      m.readWord(0xc006) should equal(x % y) withClue s"= $x %% $y"
    }
  }

  test("Word division 2") {
    divisionCase2(0, 1)
    divisionCase2(1, 1)
    divisionCase2(2, 1)
    divisionCase2(250, 1)
    divisionCase2(0, 3)
    divisionCase2(0, 5)
    divisionCase2(1, 5)
    divisionCase2(6, 5)
    divisionCase2(73, 5)
    divisionCase2(73, 8)
    divisionCase2(127, 127)
    divisionCase2(128, 127)
    divisionCase2(75, 5)
    divisionCase2(42, 11)
    divisionCase2(420, 11)
    divisionCase2(1210, 11)
    divisionCase2(35000, 45)
    divisionCase2(35000, 2)
    divisionCase2(51462, 3)
    divisionCase2(51462, 1)
  }

  test("Word division 2 – large divisor") {
    divisionCase2(127, 128)
    divisionCase2(128, 128)
    divisionCase2(45323, 250)
    divisionCase2(4530, 200)
    divisionCase2(9039, 247)
  }

  private def divisionCase2(x: Int, y: Int): Unit = {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      s"""
         | import zp_reg
         | word output_q1 @$$c000
         | word output_m1 @$$c002
         | word output_q2 @$$c004
         | word output_m2 @$$c006
         | void main () {
         |  word a
         |  byte b
         |  a = f()
         |  b = g()
         |  output_q1 = a / b
         |  output_m1 = a %% b
         |  output_q2 = a
         |  output_m2 = a
         |  output_q2 /= b
         |  output_m2 %%= b
         | }
         | word f() = $x
         | noinline byte g() = $y
          """.
        stripMargin) { m =>
      m.readWord(0xc000) should equal(x / y) withClue s"= $x / $y"
      m.readWord(0xc002) should equal(x % y) withClue s"= $x %% $y"
      m.readWord(0xc004) should equal(x / y) withClue s"= $x / $y"
      m.readWord(0xc006) should equal(x % y) withClue s"= $x %% $y"
    }
  }

  test("Word division 4") {
    divisionCase4(0, 2)
    divisionCase4(1, 2)
    divisionCase4(2, 2)
    divisionCase4(250, 128)
    divisionCase4(0, 4)
    divisionCase4(0, 8)
    divisionCase4(1, 4)
    divisionCase4(6, 8)
    divisionCase4(73, 16)
    divisionCase4(2534, 2)
    divisionCase4(2534, 32)
    divisionCase4(35000, 2)
    divisionCase4(51462, 4)
    divisionCase4(51462, 1)
  }

  test("Word division 4 – large divisor") {
    divisionCase4(75, 128)
    divisionCase4(42, 128)
    divisionCase4(142, 128)
    divisionCase4(9039, 247)
  }

  private def divisionCase4(x: Int, y: Int): Unit = {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      s"""
         | import zp_reg
         | word output_q1 @$$c000
         | byte output_m1 @$$c002
         | word output_q2 @$$c004
         | word output_m2 @$$c006
         | void main () {
         |  byte a
         |  output_q2 = g()
         |  output_m2 = g()
         |  a = f()
         |  output_q1 = $x / a
         |  output_m1 = $x %% a
         |  output_q2 /= a
         |  output_m2 %%= a
         | }
         | byte f() {return $y}
         | noinline word g() {return $x}
          """.
        stripMargin) { m =>
      m.readWord(0xc000) should equal(x / y) withClue s"= $x / $y"
      m.readByte(0xc002) should equal(x % y) withClue s"= $x %% $y"
      m.readWord(0xc004) should equal(x / y) withClue s"= $x / $y"
      m.readWord(0xc006) should equal(x % y) withClue s"= $x %% $y"
    }
  }

  test("Word/word multiplication 1") {
    multiplyCaseWW1(0, 0)
    multiplyCaseWW1(0, 5)
    multiplyCaseWW1(7, 0)
    multiplyCaseWW1(2, 5)
    multiplyCaseWW1(7, 2)
    multiplyCaseWW1(100, 2)
    multiplyCaseWW1(1000, 2)
    multiplyCaseWW1(2, 1000)
    multiplyCaseWW1(1522, 1000)
    multiplyCaseWW1(54, 4)
    multiplyCaseWW1(35000, 9)
    multiplyCaseWW1(43, 35000)
    multiplyCaseWW1(53459, 48233)
  }

  private def multiplyCaseWW1(x: Int, y: Int): Unit = {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Motorola6809)(
      s"""
         | import zp_reg
         | word output0 @$$c000
         | word output1 @$$c002
         | word output2 @$$c004
         | void main () {
         |  output0 = $x
         |  output0 *= word($y)
         |  output1 = id($x) * id($y)
         | }
         | noinline word id(word w) = w
          """.
        stripMargin){m =>
      m.readWord(0xc000) should equal((x * y) & 0xffff) withClue s"= $x * $y (c000)"
      m.readWord(0xc002) should equal((x * y) & 0xffff) withClue s"= $x * $y (c002)"
    }
  }

  test("Word/word division 1") {
    divisionCaseWW1(0, 1)
    divisionCaseWW1(0, 1000)
    divisionCaseWW1(1, 1000)
    divisionCaseWW1(1, 1)
    divisionCaseWW1(1000, 1)
    divisionCaseWW1(1000, 1000)
    divisionCaseWW1(1000, 33)
    divisionCaseWW1(33000, 999)
    divisionCaseWW1(33000, 256)
    divisionCaseWW1(33000, 16)
    divisionCaseWW1(33000, 1024)
  }

  private def divisionCaseWW1(x: Int, y: Int): Unit = {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Motorola6809)(
      s"""
         | import zp_reg
         | word output0 @$$c000
         | word output1 @$$c002
         | void main () {
         |  output0 = $x
         |  memory_barrier()
         |  output0 /= word($y)
         |  output1 = $x
         |  memory_barrier()
         |  output1 %%= word($y)
         | }
         | noinline word id(word w) = w
          """.
        stripMargin){m =>
      m.readWord(0xc000) should equal((x / y) & 0xffff) withClue s"= $x / $y (c000)"
      m.readWord(0xc002) should equal((x % y) & 0xffff) withClue s"= $x %% $y (c002)"
    }
  }

  test("Sign extension in subtraction") {
    for {
      i <- Seq(5324, 6453, 1500)
      j <- Seq(0, 1, -1, -3, -7, -128, 127)
//      i <- Seq(5324)
//      j <- Seq(-1)
    } {
      EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
        s"""
           | word output0 @$$c000
           | word output1 @$$c002
           | word output2 @$$c004
           | void main () {
           |  sbyte tmp
           |  output0 = $i
           |  output2 = $i
           |  tmp = $j
           |  memory_barrier()
           |  output1 = output0 - sbyte(${j&0xff})
           |  memory_barrier()
           |  output0 -= sbyte(${j&0xff})
           |  output2 -= tmp
           | }
           | noinline word id(word w) = w
            """.
          stripMargin){m =>
        m.readWord(0xc000) should equal((i - j) & 0xffff) withClue s"= $i - $j (c000)"
        m.readWord(0xc002) should equal((i - j) & 0xffff) withClue s"= $i - $j (c002)"
        m.readWord(0xc004) should equal((i - j) & 0xffff) withClue s"= $i - $j (c004)"
      }
    }
  }

  test("Sign extension in multiplication") {
    for {
      x <- Seq(0, -10, 10, 120, -120)
      y <- Seq(0, -10, 10, 120, -120)
      angle <- Seq(0, 156, 100, 67)
    } {
      EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
        s"""
           | import zp_reg
           | array(sbyte) sinTable @$$c100= for i,0,to,256+64-1 [sin(i,127)]
           | sbyte outputX @$$c000
           | sbyte outputY @$$c001
           | noinline sbyte rotatePointX(sbyte x,sbyte y,byte angle) {
           |  sbyte rx
           |  sbyte s,c
           |  s = sinTable[angle]
           |  angle = angle + 64
           |  c = sinTable[angle]
           |  rx = lo(((word(x)*word(c))>>7) - ((word(y)*word(s))>>7))
           |
           |  return rx;
           |}
           |
           |noinline sbyte rotatePointY(sbyte x,sbyte y,byte angle) {
           |  sbyte ry
           |  sbyte s,c
           |  s = sinTable[angle]
           |  angle = angle + 64
           |  c = sinTable[angle]
           |  ry = lo(((word(x)*word(s))>>7) + ((word(y)*word(c))>>7))
           |  return ry;
           |}
           | void main () {
           |  outputX = rotatePointX($x, $y, $angle)
           |  outputY = rotatePointY($x, $y, $angle)
           | }
            """.
          stripMargin){m =>
        for (a <- 0 until (256+64)) {
          val expected = (127 * math.sin(a * math.Pi / 128)).round.toInt
          m.readByte(0xc100 + a).toByte should equal(expected.toByte) withClue s"= sin($a)"
        }
        val s = (127 * math.sin(angle * math.Pi / 128)).round.toInt
        val c = (127 * math.sin((angle + 64) * math.Pi / 128)).round.toInt
        val rx = (x * c >> 7) - (y * s >> 7)
        val ry = (x * s >> 7) + (y * c >> 7)
        m.readByte(0xc000).toByte should equal(rx.toByte) withClue s"= x of ($x,$y) @ $angle"
        m.readByte(0xc001).toByte should equal(ry.toByte) withClue s"= y of ($x,$y) @ $angle"
      }
    }
  }

  test("Signed multiplication with type promotion") {
    for {
      (t1, t2) <- Seq("sbyte" -> "word", "sbyte" -> "signed16", "byte" -> "signed16", "signed16" -> "word")
      x <- Seq(0, -1, 1, 120, -120)
      x2 <- Seq(0, -1, 1, 120, -120)
    } {
      val x1 = if (t1 == "byte") x & 0xff else x
      EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
        s"""
           | import zp_reg
           | signed16 output @$$c000
           | bool typeOk @$$c002
           | void main () {
           |  $t1 v1
           |  v1 = $x1
           |  $t2 v2
           |  v2 = $x2
           |  memory_barrier()
           |  output = v1 * v2
           |  typeOk = typeof(v1 * v2) == typeof(signed16)
           | }""".
          stripMargin) { m =>
        m.readWord(0xc000).toShort should equal((x1 * x2).toShort) withClue s"= $t1($x1) * $t2($x2)"
        m.readByte(0xc002) should equal(1) withClue s"= typeof($t1 * $t2)"
      }
    }
  }

  test("Signed multiplication with type promotion 2") {
    for {
      t2 <- Seq("sbyte", "signed16", "byte", "word")
      x1 <- Seq(0, -1, 1, 120, -120)
      x <- Seq(0, -1, 1, 120, -120)
    } {
      val x2 = if (t2.startsWith("s")) x else if (t2.startsWith("w")) x & 0xffff else x & 0xff
      EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
        s"""
           | import zp_reg
           | signed16 output @$$c000
           | void main () {
           |  output = $x1
           |  $t2 v2
           |  v2 = $x2
           |  memory_barrier()
           |  output *= v2
           | }""".
          stripMargin) { m =>
        m.readWord(0xc000).toShort should equal((x1 * x2).toShort) withClue s"= signed16($x1) * $t2($x2)"
      }
    }
  }
}
