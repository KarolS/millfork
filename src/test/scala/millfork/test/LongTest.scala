package millfork.test

import millfork.test.emu.EmuBenchmarkRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class LongTest extends FunSuite with Matchers {

  test("Long assignment") {
    EmuBenchmarkRun(
      """
        | long output4 @$c000
        | long output2 @$c004
        | long output1 @$c008
        | void main () {
        |   output4 = $11223344
        |   output2 = $11223344
        |   output1 = $11223344
        |   output2 = $7788
        |   output1 = $55
        | }
      """.stripMargin) { m =>
      m.readLong(0xc000) should equal(0x11223344)
      m.readLong(0xc004) should equal(0x7788)
      m.readLong(0xc008) should equal(0x55)
    }
  }
  test("Long assignment 2") {
    EmuBenchmarkRun(
      """
        | long output4 @$c000
        | long output2 @$c004
        | word output1 @$c008
        | void main () {
        |   word w
        |   byte b
        |   w = $7788
        |   b = $55
        |   output4 = $11223344
        |   output2 = $11223344
        |   output1 = $11223344
        |   output2 = w
        |   output1 = b
        | }
      """.stripMargin) { m =>
      m.readLong(0xc000) should equal(0x11223344)
      m.readLong(0xc004) should equal(0x7788)
      m.readLong(0xc008) should equal(0x55)
    }
  }
  test("Long addition") {
    EmuBenchmarkRun(
      """
        | long output @$c000
        | void main () {
        |   word w
        |   long l
        |   byte b
        |   w = $8000
        |   b = $8
        |   l = $50000
        |   output = 0
        |   output += l
        |   output += w
        |   output += b
        | }
      """.stripMargin) { m =>
      m.readLong(0xc000) should equal(0x58008)
    }
  }
  test("Long addition 2") {
    EmuBenchmarkRun(
      """
        | long output @$c000
        | void main () {
        |   output = 0
        |   output += $50000
        |   output += $8000
        |   output += $8
        | }
      """.stripMargin) { m =>
      m.readLong(0xc000) should equal(0x58008)
    }
  }
  test("Long subtraction") {
    EmuBenchmarkRun(
      """
        | long output @$c000
        | void main () {
        |   word w
        |   long l
        |   byte b
        |   w = $8000
        |   b = $8
        |   l = $50000
        |   output = $58008
        |   output -= l
        |   output -= w
        |   output -= b
        | }
      """.stripMargin) { m =>
      m.readLong(0xc000) should equal(0)
    }
  }
  test("Long subtraction 2") {
    EmuBenchmarkRun(
      """
        | long output @$c000
        | void main () {
        |   output = $58008
        |   output -= $50000
        |   output -= $8000
        |   output -= $8
        | }
      """.stripMargin) { m =>
      m.readLong(0xc000) should equal(0)
    }
  }
  test("Long subtraction 3") {
    EmuBenchmarkRun(
      """
        | long output @$c000
        | void main () {
        |   output = $58008
        |   output -= w()
        |   output -= b()
        | }
        | byte b() {
        |   return $8
        | }
        | word w() {
        |   return $8000
        | }
      """.stripMargin) { m =>
      m.readLong(0xc000) should equal(0x50000)
    }
  }

  test("Long AND") {
    EmuBenchmarkRun(
      """
        | long output @$c000
        | void main () {
        |   output = $FFFFFF
        |   output &= w()
        |   output &= b()
        | }
        | byte b() {
        |   return $77
        | }
        | word w() {
        |   return $CCCC
        | }
      """.stripMargin) { m =>
      m.readLong(0xc000) should equal(0x44)
    }
  }
}
