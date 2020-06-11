package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuCrossPlatformBenchmarkRun, EmuUnoptimizedCrossPlatformRun, EmuUnoptimizedRun, ShouldNotCompile}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class ByteDecimalMathSuite extends FunSuite with Matchers {

  test("Decimal byte addition") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Ricoh, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | byte output @$c000
        | byte a
        | void main () {
        |  a = $36
        |  output = a +' a
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(0x72))
  }

  test("Decimal byte addition 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Ricoh, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | byte output @$c000
        | byte a
        | void main () {
        |  a = 1
        |  output = a +' $69
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(0x70))
  }

  test("Decimal byte addition comprehensive suite for Ricoh") {
    val pairs = List(
      0 -> 0,
      55 -> 55,
      90 -> 90,
      86 -> 84,
      7 -> 8,
      1 -> 9,
      1 -> 99,
      99 -> 1,
      2 -> 8
    )
    for ((i,j) <- pairs) {
      EmuUnoptimizedCrossPlatformRun(Cpu.Ricoh)(
        """
          | byte output @$c000
          | void main () {
          |   init()
          |   run()
          | }
          | void init() { output = $#i }
          | void run () { output +'= $#j }
        """.stripMargin.replace("#i", i.toString).replace("#j", j.toString)) { m =>
        toDecimal(m.readByte(0xc000)) should equal((i + j) % 100)
      }
    }
  }

  test("Decimal byte subtraction") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Ricoh, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | byte output @$c000
        | byte a
        | void main () {
        |  a = $50
        |  output = a -' $35
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(0x15))
  }

  test("In-place decimal byte addition") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Ricoh, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | array output[3] @$c000
        | byte a
        | void main () {
        |  a = 1
        |  output[1] = 5
        |  output[a] +'= 1
        |  output[a] +'= $36
        | }
      """.stripMargin)(_.readByte(0xc001) should equal(0x42))
  }

  test("In-place decimal byte addition 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Ricoh, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | array output[3] @$c000
        | void main () {
        |  byte x
        |  byte y
        |  byte tmpx
        |  byte tmpy
        |  tmpx = one()
        |  tmpy = one()
        |  x = tmpx
        |  y = tmpy
        |  output[y] = $39
        |  output[x] +'= 1
        | }
        | byte one() { return 1 }
      """.stripMargin)(_.readByte(0xc001) should equal(0x40))
  }

  test("In-place decimal byte subtraction") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Ricoh, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | byte output @$c000
        | byte a
        | void main () {
        |  output = $50
        |  output -'= $35
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(0x15))
  }

  test("Decimal word addition") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Ricoh, Cpu.Intel8086)(
      """
        | word output @$c000
        | void main () {
        |  output = f() +' g()
        | }
        | word f() {
        |   return $253
        | }
        | word g() {
        |   return $455
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(0x708))
  }

  test("Decimal word subtraction") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Ricoh, Cpu.Intel8086)(
      """
        | word output @$c000
        | void main () {
        |  output = f() -' $264
        | }
        | word f() {
        |   return $1500 -' g()
        | }
        | word g() {
        |   return $455
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(0x781))
  }

  test("In-place decimal word subtraction") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Ricoh, Cpu.Intel8086)(
      """
        | word output @$c000
        | word a
        | void main () {
        |  output = $1500
        |  output -'= $455
        |  a = $264
        |  output -'= a
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(0x781))
  }

  test("In-place decimal long subtraction") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Ricoh, Cpu.Intel8086)(
      """
        | long output @$c000
        | word a
        | void main () {
        |  output = $22334455
        |  output -'= f()
        | }
        | noinline long f() {
        |  return $20304050
        | }
      """.stripMargin)(_.readLong(0xc000) should equal(0x2030405))
  }

  test("Flag switching test") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Ricoh, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | byte output @$c000
        | void main () {
        |   output = addDecimal(9, 9) + addDecimal(9, 9)
        | }
        | byte addDecimal(byte a, byte b) { return a +' b }
      """.stripMargin)(_.readByte(0xc000) should equal(0x30))
  }

  test("Flag switching test 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Ricoh, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | byte output @$c000
        | void main () {
        |   output = addDecimalTwice(9, 9)
        | }
        | byte addDecimalTwice(byte a, byte b) { return (a +' b) + (a +' b) }
      """.stripMargin)(_.readByte(0xc000) should equal(0x30))
  }

  test("Flag switching test 3") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Ricoh, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | byte output @$c000
        | void main () {
        |   output = addDecimalTwice($c, $c)
        | }
        | byte addDecimalTwice(byte a, byte b) { return (a + b) +' (a + b) }
      """.stripMargin)(_.readByte(0xc000) should equal(0x36))
  }

  test("Decimal left shift test") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Ricoh, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | byte output @$c000
        | void main () {
        |   byte n
        |   n = nine()
        |   output = n <<' 2
        | }
        | byte nine() { return 9 }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(0x36)
    }
  }

  test("Decimal left shift test 2") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Ricoh, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | byte output @$c000
        | void main () {
        |   output = nine()
        |   output <<'= 2
        | }
        | byte nine() { return 9 }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(0x36)
    }
  }

  test("Decimal left shift test 3") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Ricoh, Cpu.Intel8086)(
      """
        | word output @$c000
        | void main () {
        |   output = nine()
        |   output <<'= 2
        | }
        | byte nine() { return $91 }
      """.stripMargin) { m =>
      m.readWord(0xc000) should equal(0x364)
    }
  }

  test("Decimal left shift test 4") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Ricoh, Cpu.Intel8086)(
      """
        | long output @$c000
        | void main () {
        |   output = nine()
        |   output <<'= 2
        | }
        | byte nine() { return $91 }
      """.stripMargin) { m =>
      m.readLong(0xc000) should equal(0x364)
    }
  }

  test("Decimal right shift test") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Ricoh, Cpu.Intel8086)(
      """
        | byte output @$c000
        | void main () {
        |   byte n
        |   n = thirty_six()
        |   output = n >>' 2
        | }
        | byte thirty_six() { return $36 }
      """.stripMargin) { m=>
      m.readByte(0xc000) should equal(9)
    }
  }

  test("Decimal right shift test 2") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Ricoh, Cpu.Intel8086)(
      """
        | byte output @$c000
        | void main () {
        |   output = thirty_six()
        |   output >>'= 2
        | }
        | byte thirty_six() { return $36 }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(9)
    }
  }

  test("Decimal right shift test 3") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Ricoh, Cpu.Intel8086)(
      """
        | word output @$c000
        | void main () {
        |   output = thirty_six()
        |   output >>'= 2
        | }
        | word thirty_six() { return $364 }
      """.stripMargin) { m =>
      m.readWord(0xc000) should equal(0x91)
    }
  }

  private def toDecimal(v: Int): Int = {
    if (v.&(0xf000) > 0x9000 || v.&(0xf00) > 0x900 || v.&(0xf0) > 0x90 || v.&(0xf) > 9)
      fail("Invalid decimal value: " + v.toHexString)
    v.&(0xf000).>>(12) * 1000 + v.&(0xf00).>>(8) * 100 + v.&(0xf0).>>(4) * 10 + v.&(0xf)
  }

  test("Decimal byte right-shift comprehensive byte suite") {
    for (i <- 0 to 99) {
      val m = EmuUnoptimizedRun(
        """
          | byte output @$c000
          | void main () {
          |   output = $#
          |   output >>'= 1
          | }
        """.stripMargin.replace("#", i.toString))
      toDecimal(m.readByte(0xc000)) should equal(i/2)
    }
  }

  test("Decimal word right-shift comprehensive suite") {
    for (i <- List(0, 1, 10, 100, 1000, 2000, 500, 200, 280, 300, 5234, 7723, 7344, 9, 16, 605, 1111, 2222, 3333, 9999, 8888, 8100)) {
      EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Ricoh, Cpu.Intel8086)(
        """
          | word output @$c000
          | void main () {
          |   output = $#
          |   output >>'= 1
          | }
        """.stripMargin.replace("#", i.toString)) {m =>
        toDecimal(m.readWord(0xc000)) should equal(i/2)
      }
    }
  }

  test("Decimal byte multiplication comprehensive suite") {
    val numbers = List(0, 1, 2, 3, 6, 8, 10, 11, 12, 14, 15, 16, 20, 40, 73, 81, 82, 98, 99)
    for (i <- numbers; j <- numbers) {
      EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Ricoh, Cpu.Intel8086)(
        """
          | byte output @$c000
          | void main () {
          |   init()
          |   run()
          | }
          | void init() { output = $#i }
          | void run () { output *'= $#j }
        """.stripMargin.replace("#i", i.toString).replace("#j", j.toString)) { m =>
        toDecimal(m.readByte(0xc000)) should equal((i * j) % 100)
      }
    }
  }

  test("Decimal multiplication by non-constant") {
    ShouldNotCompile(
      """
        | volatile byte x
        | void main() {
        |    byte tmp
        |    tmp = 3
        |    tmp *'= x
        | }
        |""".stripMargin)
  }

  test("Decimal multiplication of two variables") {
    ShouldNotCompile(
      """
        | volatile byte x
        | void main() {
        |    byte tmp
        |    tmp = x *' x
        | }
        |""".stripMargin)
  }

  test("Decimal comparison") {
    // CMP#0 shouldn't be elided after a decimal operation.
    // Currently no emulator used for testing can catch that.
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Ricoh, Cpu.Intel8086)(
      """
        | byte output @$c000
        | void main () {
        |   init()
        |   if output +' 1 == 0 {
        |     output = $22
        |   }
        |   output +'= 1
        | }
        | void init() { output = $99 }
      """.stripMargin
    ) { m =>
      m.readByte(0xc000) should equal(0x23)
    }
  }
}
