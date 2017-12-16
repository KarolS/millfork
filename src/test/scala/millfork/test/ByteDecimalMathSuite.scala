package millfork.test

import millfork.test.emu.EmuBenchmarkRun
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
}
