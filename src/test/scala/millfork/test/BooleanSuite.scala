package millfork.test

import millfork.test.emu.EmuBenchmarkRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class BooleanSuite extends FunSuite with Matchers {

  test("Not") {
    EmuBenchmarkRun(
      """
        | byte output @$c000
        | array input = [5,6,7]
        | void main () {
        |   byte a
        |   a = 5
        |   if not(a < 3) {output = 4}
        |   if not(a > 3) {output = 3}
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(4))

  }


  test("And") {
    EmuBenchmarkRun(
      """
        | byte output @$c000
        | array input = [5,6,7]
        | void main () {
        |   byte a
        |   byte b
        |   a = 5
        |   b = 5
        |   if a > 3 && b > 3 {output = 4}
        |   if a < 3 && b > 3 {output = 5}
        |   if a > 3 && b < 3 {output = 2}
        |   if a < 3 && b < 3 {output = 3}
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(4))
  }


  test("Or") {
    EmuBenchmarkRun(
      """
        | byte output @$c000
        | array input = [5,6,7]
        | void main () {
        |   byte a
        |   byte b
        |   a = 5
        |   b = 5
        |   output = 0
        |   if a > 3 || b > 3 {output += 4}
        |   if a < 3 || b > 3 {output += 5}
        |   if a > 3 || b < 3 {output += 2}
        |   if a < 3 || b < 3 {output = 30}
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(11))
  }
}
