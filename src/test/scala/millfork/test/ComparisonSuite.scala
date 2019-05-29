package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuBenchmarkRun, EmuCrossPlatformBenchmarkRun, EmuSuperOptimizedRun, EmuUltraBenchmarkRun}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class ComparisonSuite extends FunSuite with Matchers {

  test("Equality and inequality") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | byte output @$c000
        | void main () {
        |  output = 5
        |  if (output == 5) {
        |   output += 1
        |  } else {
        |   output +=2
        |  }
        |  if (output != 6) {
        |   output += 78
        |  }
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(6))
  }

  test("Less") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | byte output @$c000
        | void main () {
        |  output = 5
        |  while output < 150 {
        |    output += 1
        |  }
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(150))
  }

  test("Compare to zero") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | byte output @$c000
        | void main () {
        |  byte a
        |  a = 150
        |  while a != 0 {
        |    a -= 1
        |    output += 1
        |  }
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(150))
  }

  test("Carry flag optimization test") {
    EmuUltraBenchmarkRun(
      """
        | byte output @$c000
        | void main () {
        |  byte a
        |  a = 150
        |  if (a >= 50) {
        |    output = 4
        |  } else {
        |    output = 0
        |  }
        |  output += get(55)
        | }
        | byte get(byte x) {
        |   if x >= 6 {return 0} else {return 128}
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(4))
  }

  test("Does it even work") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | word output @$c000
        | void main () {
        |  byte a
        |  a = 150
        |  if a != 0 {
        |    output = 345
        |  }
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(345))
  }

  test("Word comparison constant") {
    val src =
      """
        | byte output @$c000
        | void main () {
        |  output = 0
        |  if 2222 == 2222 { output += 1 }
        |  if 2222 == 3333 { output -= 1 }
        | }
      """.stripMargin
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(src)(_.readByte(0xc000) should equal(src.count(_ == '+')))
  }

  test("Word comparison == and !=") {
    val src =
      """
        | byte output @$c000
        | void main () {
        |  word a
        |  word b
        |  word c
        |  output = 0
        |  a = 4
        |  b = 4
        |  c = 5
        |  if a == 4 { output += 1 }
        |  if a == b { output += 1 }
        |  if a != c { output += 1 }
        |  if a != 5 { output += 1 }
        |  if a != 260 { output += 1 }
        |  if a != 0 { output += 1 }
        | }
      """.stripMargin
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(src)(_.readByte(0xc000) should equal(src.count(_ == '+')))
  }

  test("Word comparison <=") {
    val src =
      """
        | byte output @$c000
        | void main () {
        |  word a
        |  word b
        |  word c
        |  output = 0
        |  a = 4
        |  b = 4
        |  c = 5
        |  if a <= 4 { output += 1 }
        |  if a <= 257 { output += 1 }
        |  if a <= b { output += 1 }
        |  if a <= c { output += 1 }
        | }
      """.stripMargin
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(src)(_.readByte(0xc000) should equal(src.count(_ == '+')))
  }
  test("Word comparison <") {
    val src =
      """
        | byte output @$c000
        | void main () {
        |  word a
        |  word b
        |  word c
        |  output = 0
        |  a = 4
        |  b = 4
        |  c = 5
        |  if a < 5 { output += 1 }
        |  if a < c { output += 1 }
        |  if a < 257 { output += 1 }
        | }
      """.stripMargin
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(src)(_.readByte(0xc000) should equal(src.count(_ == '+')))
  }


  test("Word comparison >") {
    val src =
      """
        | byte output @$c000
        | void main () {
        |  word a
        |  word b
        |  word c
        |  output = 0
        |  a = 4
        |  b = 4
        |  c = 5
        |  if c > a { output += 1 }
        |  if c > 1 { output += 1 }
        |  if c > 0 { output += 1 }
        | }
      """.stripMargin
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(src)(_.readByte(0xc000) should equal(src.count(_ == '+')))
  }

  test("Word comparison >=") {
    val src =
      """
        | byte output @$c000
        | void main () {
        |  word a
        |  word b
        |  word c
        |  output = 0
        |  a = 4
        |  b = 4
        |  c = 5
        |  if c >= 1 { output += 1 }
        |  if c >= a { output += 1 }
        |  if a >= a { output += 1 }
        |  if a >= 4 { output += 1 }
        |  if a >= 4 { output += 1 }
        |  if a >= 0 { output += 1 }
        | }
      """.stripMargin
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(src)(_.readByte(0xc000) should equal(src.count(_ == '+')))
  }

  test("Signed comparison >=") {
    val src =
      """
        | byte output @$c000
        | void main () {
        |  sbyte a
        |  sbyte b
        |  sbyte c
        |  output = 0
        |  a = 4
        |  b = 4
        |  c = 5
        |  if c >= 1 { output += 1 }
        |  if c >= a { output += 1 }
        |  if a >= a { output += 1 }
        |  if a >= 4 { output += 1 }
        |  if a >= 4 { output += 1 }
        |  if a >= 0 { output += 1 }
        | }
      """.stripMargin
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(src)(_.readByte(0xc000) should equal(src.count(_ == '+')))
  }

  test("Signed comparison with overflow") {
    val src =
      """
        | byte output @$c000
        | void main () {
        |  sbyte a
        |  sbyte b
        |  sbyte c
        |  output = 0
        |  a = 4
        |  b = 4
        |  c = 100
        |  if c >= -128 { output += 1 }
        |  if c >= c { output += 1 }
        |  if a >= -128 { output += 1 }
        |  if b >= -128 { output += 1 }
        |  if b >= -88 { output += 1 }
        |  if a >= -88 { output += 1 }
        |  if c >= -88 { output += 1 }
        |  if a >= -1 { output += 1 }
        |  if c >= -1 { output += 1 }
        |  if c > -128 { output += 1 }
        |  if a > -128 { output += 1 }
        |  if b > -128 { output += 1 }
        |  if b > -88 { output += 1 }
        |  if a > -88 { output += 1 }
        |  if c > -88 { output += 1 }
        |  if a > -1 { output += 1 }
        |  if c > -1 { output += 1 }
        | }
      """.stripMargin
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(src)(_.readByte(0xc000) should equal(src.count(_ == '+')))
  }

  test("Signed comparison < and <=") {
    val src =
      """
        | byte output @$c000
        | void main () {
        |  sbyte a
        |  sbyte b
        |  sbyte c
        |  output = 0
        |  a = -1
        |  b = 0
        |  c = 1
        |  if a < 0 { output += 1 }
        |  if b < 0 { output -= 7 }
        |  if c < 0 { output -= 7 }
        |  if a < 1 { output += 1 }
        |  if b < 1 { output += 1 }
        |  if c < 1 { output -= 7 }
        |  if a <= 0 { output += 1 }
        |  if b <= 0 { output += 1 }
        |  if c <= 0 { output -= 7 }
        |  if a <= 1 { output += 1 }
        |  if b <= 1 { output += 1 }
        |  if c <= 1 { output += 1 }
        |  if a <= -1 { output += 1 }
        |  if b <= -1 { output -= 7 }
        |  if c <= -1 { output -= 7 }
        | }
      """.stripMargin
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(src)(_.readByte(0xc000) should equal(src.count(_ == '+')))
  }

  test("Multiple params for equality") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | byte output @$c000
        | void main () {
        |  output = 5
        |  if (output == 5 == 5) {
        |   output += 1
        |  }
        |  if (output == 5 == 6) {
        |   output += 78
        |  }
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(6))
  }

  test("Multiple params for inequality") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | byte output @$c000
        | void main () {
        |  output = 5
        |  if 2 < 3 < 4 {
        |   output += 1
        |  }
        |  if 2 < 3 < 2 {
        |   output += 78
        |  }
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(6))
  }

  test("Warnings") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | byte output @$c000
        | void main () {
        |  output = 5
        |  if 2 <= three() <= 4 {
        |   output += 1
        |  }
        |  if 2 <= three() <= 2 {
        |   output += 78
        |  }
        | }
        | byte three() {
        |   return 3
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(6))
  }

  test("Long comparisons") {
    val src =
      """
        | byte output @$c000
        | void main () {
        |  long a
        |  long b
        |  long c
        |  output = 0
        |  a = 1234567
        |  b = 2345678
        |  c = 1234599
        |  if a == a { output += 1 }
        |  if c >= a { output += 1 }
        |  if c != a { output += 1 }
        |  if a <= c { output += 1 }
        |  if a <  c { output += 1 }
        |  if b >= a { output += 1 }
        |  if b >= 0 { output += 1 }
        |  if b >=  44564 { output += 1 }
        |  if a >= 335444 { output += 1 }
        |  if c >  335444 { output += 1 }
        | }
      """.stripMargin
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(src)(_.readByte(0xc000) should equal(src.count(_ == '+')))
  }

  test("Mixed type comparison") {
    val src =
      """
        | byte output @$c000
        | byte x @$c002
        | byte y @$c003
        | void main () {
        |  word z
        |  output = 0
        |  z = $100
        |  x = 4
        |  y = 1
        |  if x < z { output += 1 }
        | }
      """.stripMargin
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(src)(_.readByte(0xc000) should equal(1))
  }

  test("Compare beyond 2.2") {
    // see: http://www.6502.org/tutorials/compare_beyond.html
    EmuBenchmarkRun(
      """
        | array output [7] @$c000
        | inline void fastCompare(byte x, byte y, byte i) {
        |   if x > y {
        |     output[i] = 0
        |   } else {
        |     output[i] = 1
        |   }
        | }
        | void main() {
        |   fastCompare(1,2, 0)
        |   fastCompare(2,2, 1)
        |   fastCompare(2,1, 2)
        | }
      """.stripMargin) {m =>
      m.readByte(0xc000) should equal(1)
      m.readByte(0xc001) should equal(1)
      m.readByte(0xc002) should equal(0)
    }
  }
  
  test("Compare to $ffff") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80)(
      """
        | byte output @$c000
        | void main() {
        |   stuff($ffff)
        |   barrier()
        | }
        | noinline void stuff (word x) {
        |   if x == $ffff {
        |     output = 11
        |   }
        | }
        | noinline void barrier() {}
      """.stripMargin
    ) { m =>
      m.readByte(0xc000) should equal(11)
    }
  }

  test("Compare to 0") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80)(
      """
        | byte output @$c000
        | void main() {
        |   stuff(0)
        |   barrier()
        | }
        | noinline void stuff (word x) {
        |   if x == 0 {
        |     output = 11
        |   }
        | }
        | noinline void barrier() {}
      """.stripMargin
    ) { m =>
      m.readByte(0xc000) should equal(11)
    }
  }

  test("Signed compare to 0") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80)(
      """
        | byte output @$c000
        | void main() {
        |   stuff(0)
        | }
        | noinline void stuff (sbyte x) {
        |   if x >= 0 {
        |     output = 11
        |   }
        | }
      """.stripMargin
    ) { m =>
      m.readByte(0xc000) should equal(11)
    }
  }

  test("Signed compare to 1") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80)(
      """
        | byte output @$c000
        | void main() {
        |   stuff(2)
        | }
        | noinline void stuff (sbyte x) {
        |   if x > 1 {
        |     output = 11
        |   }
        | }
      """.stripMargin
    ) { m =>
      m.readByte(0xc000) should equal(11)
    }
  }
}
