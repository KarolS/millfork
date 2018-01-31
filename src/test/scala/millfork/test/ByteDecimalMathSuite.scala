package millfork.test

import millfork.test.emu.{EmuBenchmarkRun, EmuUnoptimizedRun}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class ByteDecimalMathSuite extends FunSuite with Matchers {

  test("Decimal byte addition") {
    EmuBenchmarkRun(
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
    EmuBenchmarkRun(
      """
        | byte output @$c000
        | byte a
        | void main () {
        |  a = 1
        |  output = a +' $69
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(0x70))
  }

  test("In-place decimal byte addition") {
    EmuBenchmarkRun(
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
    EmuBenchmarkRun(
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

  test("Flag switching test") {
    EmuBenchmarkRun(
      """
        | byte output @$c000
        | void main () {
        |   output = addDecimal(9, 9) + addDecimal(9, 9)
        | }
        | byte addDecimal(byte a, byte b) { return a +' b }
      """.stripMargin)(_.readByte(0xc000) should equal(0x30))
  }

  test("Flag switching test 2") {
    EmuBenchmarkRun(
      """
        | byte output @$c000
        | void main () {
        |   output = addDecimalTwice(9, 9)
        | }
        | byte addDecimalTwice(byte a, byte b) { return (a +' b) + (a +' b) }
      """.stripMargin)(_.readByte(0xc000) should equal(0x30))
  }

  test("Flag switching test 3") {
    EmuBenchmarkRun(
      """
        | byte output @$c000
        | void main () {
        |   output = addDecimalTwice($c, $c)
        | }
        | byte addDecimalTwice(byte a, byte b) { return (a + b) +' (a + b) }
      """.stripMargin)(_.readByte(0xc000) should equal(0x36))
  }

  test("Decimal left shift test") {
    val m = EmuUnoptimizedRun(
      """
        | byte output @$c000
        | void main () {
        |   byte n
        |   n = nine()
        |   output = n <<' 2
        | }
        | byte nine() { return 9 }
      """.stripMargin)
    m.readByte(0xc000) should equal(0x36)
  }

  test("Decimal left shift test 2") {
    val m = EmuUnoptimizedRun(
      """
        | byte output @$c000
        | void main () {
        |   output = nine()
        |   output <<'= 2
        | }
        | byte nine() { return 9 }
      """.stripMargin)
    m.readByte(0xc000) should equal(0x36)
  }

  test("Decimal left shift test 3") {
    val m = EmuUnoptimizedRun(
      """
        | word output @$c000
        | void main () {
        |   output = nine()
        |   output <<'= 2
        | }
        | byte nine() { return $91 }
      """.stripMargin)
    m.readWord(0xc000) should equal(0x364)
  }

  test("Decimal right shift test") {
    val m = EmuUnoptimizedRun(
      """
        | byte output @$c000
        | void main () {
        |   byte n
        |   n = thirty_six()
        |   output = n >>' 2
        | }
        | byte thirty_six() { return $36 }
      """.stripMargin)
    m.readByte(0xc000) should equal(9)
  }

  test("Decimal right shift test 2") {
    val m = EmuUnoptimizedRun(
      """
        | byte output @$c000
        | void main () {
        |   output = thirty_six()
        |   output >>'= 2
        | }
        | byte thirty_six() { return $36 }
      """.stripMargin)
    m.readByte(0xc000) should equal(9)
  }

  test("Decimal right shift test 3") {
    val m = EmuUnoptimizedRun(
      """
        | word output @$c000
        | void main () {
        |   output = thirty_six()
        |   output >>'= 2
        | }
        | word thirty_six() { return $364 }
      """.stripMargin)
    m.readWord(0xc000) should equal(0x91)
  }

  def toDecimal(v: Int) = {
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
      val m = EmuUnoptimizedRun(
        """
          | word output @$c000
          | void main () {
          |   output = $#
          |   output >>'= 1
          | }
        """.stripMargin.replace("#", i.toString))
      toDecimal(m.readWord(0xc000)) should equal(i/2)
    }
  }

  test("Decimal byte multiplication comprehensive suite") {
    for (i <- List(1, 2, 3, 6, 8, 10, 11, 12, 14, 15, 16, 40, 99) ; j <- 0 to 99) {
      val m = EmuUnoptimizedRun(
        """
          | byte output @$c000
          | void main () {
          |   init()
          |   run()
          | }
          | void init() { output = $#i }
          | void run () { output *'= $#j }
        """.stripMargin.replace("#i", i.toString).replace("#j", j.toString))
      toDecimal(m.readWord(0xc000)) should equal((i * j) % 100)
    }
  }
}
