package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuBenchmarkRun, EmuCrossPlatformBenchmarkRun, EmuUltraBenchmarkRun, EmuUnoptimizedCrossPlatformRun}
import org.scalatest.{AppendedClues, FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class ByteMathSuite extends FunSuite with Matchers with AppendedClues {

  test("Complex expression") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | byte output @$c000
        | void main () {
        |  output = (one() + one()) | (((one()<<2)-1) ^ one())
        | }
        | byte one() {
        |   return 1
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(2))
  }

  test("Complex expression 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | byte output @$c000
        | void main () {
        |  output = 50
        |  f()
        | }
        | noinline void f() {
        |   byte a
        |   a = g()
        |   output -= a >> 1
        | }
        | noinline byte g() { return 3 }
        |
      """.stripMargin)(_.readByte(0xc000) should equal(49))
  }

  test("Byte addition") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | byte output @$c000, a
        | void main () {
        |  a = 1
        |  output = a + a
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(2))
  }

  test("Byte addition 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | byte output @$c000, a
        | void main () {
        |  a = 1
        |  output = a + 65
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(66))
  }

  test("In-place byte addition") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | array output[3] @$c000
        | byte a
        | void main () {
        |  a = 1
        |  output[1] = 5
        |  output[a] += 1
        |  output[a] += 36
        | }
      """.stripMargin)(_.readByte(0xc001) should equal(42))
  }

  test("LHS evaluation during in-place byte addition") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | array output[1] @$c000
        | byte call_count @$c001
        | void main () {
        |   output[0] = 1
        |   output[identity(0)] += identity(1)
        | }
        | noinline byte identity(byte a) {
        |   call_count += 1
        |   return a
        | }
      """.stripMargin){m =>
      m.readByte(0xc000) should equal(2)
      // TODO: currently the compiler emits separate evaluations of the left hand side for reading and writing
      // m.readByte(0xc001) should equal(2)
    }
  }

  test("Parameter order") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | byte output @$c000
        | array arr[6]
        | void main () {
        |  output = 42
        | }
        | byte test1(byte a) @$6000 {
        |   return 5 + a
        | }
        | byte test2(byte a) @$6100 {
        |   return 5 | a
        | }
        | byte test3(byte a) @$6200 {
        |   return a + arr[a]
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(42))
  }

  test("In-place byte addition 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | array output[3] @$c000
        | void main () {
        |  byte x, y, tmpx, tmpy
        |  tmpx = one()
        |  tmpy = one()
        |  x = tmpx
        |  y = tmpy
        |  output[y] = 36
        |  output[x] += 1
        | }
        | byte one() { return 1 }
      """.stripMargin)(_.readByte(0xc001) should equal(37))
  }

  test("In-place byte multiplication") {
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
    multiplyCase1(54, 4)
    multiplyCase1(2, 100)
    multiplyCase1(4, 54)
  }

  private def multiplyCase1(x: Int, y: Int): Unit = {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      s"""
         | import zp_reg
         | byte output @$$c000
         | void main () {
         |  output = $x
         |  output *= $y
         | }
          """.
        stripMargin)(_.readByte(0xc000) should equal(x * y))
  }

  test("Byte multiplication") {
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
    multiplyCase2(54, 4)
    multiplyCase2(2, 100)
    multiplyCase2(4, 54)
  }

  private def multiplyCase2(x: Int, y: Int): Unit = {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      s"""
         | import zp_reg
         | byte output @$$c000
         | void main () {
         |  byte a
         |  a = $x
         |  output = a * $y
         | }
          """.
        stripMargin)(_.readByte(0xc000) should equal(x * y) withClue s"= $x * $y")
  }

  test("Byte multiplication 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.R800, Cpu.Motorola6809)(
      """
        | import zp_reg
        | byte output1 @$c001, output2 @$c002, output3 @$c003
        | void main () {
        |   calc1()
        |   crash_if_bad()
        |   calc2()
        |   crash_if_bad()
        |   calc3()
        |   crash_if_bad()
        | }
        |
        | byte three() { return 3 }
        | byte four() { return 4 }
        | noinline byte five() { return 5 }
        |
        | noinline void calc1() {
        |   output1 = five() * four()
        |   output2 = 3 * three() * three()
        |   output3 = five() * three()
        | }
        |
        | noinline void calc2() {
        |   output2 = 3 * three() * three()
        |   output1 = five() * four()
        |   output3 = three() * five()
        | }
        |
        | noinline void calc3() {
        |   output2 = 3 * three() * three()
        |   output1 = four() * five()
        |   output3 = 3 * five()
        | }
        |
        | noinline void crash_if_bad() {
        | #if ARCH_6502
        |   if output1 != 20 { asm { lda $bfff }}
        |   if output2 != 27 { asm { lda $bfff }}
        |   if output3 != 15 { asm { lda $bfff }}
        | #elseif ARCH_I80
        |   if output1 != 20 { asm { ld a,($bfff) }}
        |   if output2 != 27 { asm { ld a,($bfff) }}
        |   if output3 != 15 { asm { ld a,($bfff) }}
        | #elseif ARCH_X86
        |   if output1 != 20 { asm { ld a,($bfff) }}
        |   if output2 != 27 { asm { ld a,($bfff) }}
        |   if output3 != 15 { asm { ld a,($bfff) }}
        | #elseif ARCH_6809
        |   if output1 != 20 { asm { lda $bfff }}
        |   if output2 != 27 { asm { lda $bfff }}
        |   if output3 != 15 { asm { lda $bfff }}
        | #else
        | #error unsupported architecture
        | #endif
        | }
      """.stripMargin){m =>
      m.readByte(0xc003) should equal(15)
      m.readByte(0xc002) should equal(27)
      m.readByte(0xc001) should equal(20)
    }
  }

  test("Byte multiplication 3") {
    multiplyCase3(0, 0)
    multiplyCase3(0, 1)
    multiplyCase3(0, 2)
    multiplyCase3(0, 5)
    multiplyCase3(1, 0)
    multiplyCase3(5, 0)
    multiplyCase3(7, 0)
    multiplyCase3(2, 5)
    multiplyCase3(7, 2)
    multiplyCase3(100, 2)
    multiplyCase3(54, 4)
    multiplyCase3(2, 100)
    multiplyCase3(4, 54)
  }

  private def multiplyCase3(x: Int, y: Int): Unit = {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      s"""
         | import zp_reg
         | byte output @$$c000
         | void main () {
         |  byte a
         |  a = f()
         |  output = a * g()
         | }
         | byte f() {return $x}
         | byte g() {return $y}
          """.
        stripMargin)(_.readByte(0xc000) should equal(x * y) withClue s"$x * $y")
  }

  test("Byte division 1") {
//    divisionCase1(0, 1)
//    divisionCase1(1, 1)
//    divisionCase1(2, 1)
//    divisionCase1(250, 1)
//    divisionCase1(0, 3)
//    divisionCase1(0, 5)
    divisionCase1(1, 5)
//    divisionCase1(6, 5)
//    divisionCase1(73, 5)
//    divisionCase1(75, 5)
//    divisionCase1(42, 11)
  }

  private def divisionCase1(x: Int, y: Int): Unit = {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      s"""
         | import zp_reg
         | byte output_q1 @$$c000, output_m1 @$$c001, output_q2 @$$c002, output_m2 @$$c003
         | void main () {
         |  byte a
         |  a = f()
         |  output_q1 = a / $y
         |  output_m1 = a %% $y
         |  output_q2 = a
         |  output_m2 = a
         |  output_q2 /= $y
         |  output_m2 %%= $y
         | }
         | byte f() {return $x}
          """.
        stripMargin) { m =>
      m.readByte(0xc000) should equal(x / y) withClue s"= $x / $y"
      m.readByte(0xc001) should equal(x % y) withClue s"= $x %% $y"
      m.readByte(0xc002) should equal(x / y) withClue s"= $x / $y"
      m.readByte(0xc003) should equal(x % y) withClue s"= $x %% $y"
    }
  }

  test("Byte division 2") {
    divisionCase2(0, 1)
    divisionCase2(1, 1)
    divisionCase2(2, 1)
    divisionCase2(250, 1)
    divisionCase2(0, 3)
    divisionCase2(0, 5)
    divisionCase2(1, 5)
    divisionCase2(6, 5)
    divisionCase2(73, 5)
    divisionCase2(75, 5)
    divisionCase2(42, 11)
  }

  private def divisionCase2(x: Int, y: Int): Unit = {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      s"""
         | import zp_reg
         | byte output_q1 @$$c000, output_m1 @$$c001
         | byte output_q2 @$$c002, output_m2 @$$c003
         | void main () {
         |  byte a
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
         | byte f() = $x
         | noinline byte g() = $y
          """.
        stripMargin) { m =>
      m.readByte(0xc000) should equal(x / y) withClue s"= $x / $y"
      m.readByte(0xc001) should equal(x % y) withClue s"= $x %% $y"
      m.readByte(0xc002) should equal(x / y) withClue s"= $x / $y"
      m.readByte(0xc003) should equal(x % y) withClue s"= $x %% $y"
    }
  }

  test("Byte division 3") {
    divisionCase1(0, 2)
    divisionCase1(1, 2)
    divisionCase1(2, 2)
    divisionCase1(250, 128)
    divisionCase1(0, 4)
    divisionCase1(0, 8)
    divisionCase1(1, 4)
    divisionCase1(6, 8)
    divisionCase1(73, 16)
    divisionCase1(75, 128)
    divisionCase1(42, 128)
    divisionCase1(142, 128)
  }

  test("Byte division 4") {
    divisionCase4(0, 2)
    divisionCase4(1, 2)
    divisionCase4(2, 2)
    divisionCase4(250, 128)
    divisionCase4(0, 4)
    divisionCase4(0, 8)
    divisionCase4(1, 4)
    divisionCase4(6, 8)
    divisionCase4(73, 16)
    divisionCase4(75, 128)
    divisionCase4(42, 128)
    divisionCase4(142, 128)
  }

  private def divisionCase4(x: Int, y: Int): Unit = {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      s"""
         | import zp_reg
         | byte output_q1 @$$c000, output_m1 @$$c001
         | byte output_q2 @$$c002, output_m2 @$$c003
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
         | noinline byte g() {return $x}
          """.
        stripMargin) { m =>
      m.readByte(0xc000) should equal(x / y) withClue s"= $x / $y"
      m.readByte(0xc001) should equal(x % y) withClue s"= $x %% $y"
      m.readByte(0xc002) should equal(x / y) withClue s"= $x / $y"
      m.readByte(0xc003) should equal(x % y) withClue s"= $x %% $y"
    }
  }

  test("Division bug repro"){
      EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
        s"""
           | import zp_reg
           | byte output_q1 @$$c000, output_m1 @$$c001
           | array zeroes[256] = [for i,0,until,256 [0]]
           | void main () {
           |  byte a
           |  byte b
           |  a = 186
           |  memory_barrier()
           |  a /= zeroes[b] | 2
           |  output_q1 = a
           | }
            """.
          stripMargin) { m =>
        m.readByte(0xc000) should equal(93)
      }
    }

  test("Multiplication bug repro") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
         | import zp_reg
         | byte output @$c000
         | array zeroes[256] = [for i,0,until,256 [0]]
         | void main () {
         |  byte a,b
         |  a = 5
         |  b = 5
         |  memory_barrier()
         |  output = a * (b * a)
         | }
          """.
        stripMargin) { m =>
      m.readByte(0xc000) should equal(125)
    }
  }

  test("Optimal multiplication detection") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
         | import zp_reg
         | word output @$c000
         | noinline void run(byte a, byte b) {
         |  output = word(a) * b
         | }
         | void main () {
         |  run(100, 42)
         | }
          """.
        stripMargin) { m =>
      m.readWord(0xc000) should equal(4200)
    }
  }
}
