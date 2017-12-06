package millfork.test

import millfork.test.emu.EmuBenchmarkRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class ForLoopSuite extends FunSuite with Matchers {

  test("For-to") {
    EmuBenchmarkRun(
      """
        | word output @$c000
        | void main () {
        |   byte i
        |   output = 0
        |   for i,0,to,5 {
        |     output += i
        |   }
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(15))
  }
  test("For-downto") {
    EmuBenchmarkRun(
      """
        | word output @$c000
        | void main () {
        |   byte i
        |   output = 0
        |   for i,5,downto,0 {
        |     output += i
        |   }
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(15))
  }
  test("For-until") {
    EmuBenchmarkRun(
      """
        | word output @$c000
        | void main () {
        |   byte i
        |   output = 0
        |   for i,0,until,6 {
        |     output += i
        |   }
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(15))
  }
  test("For-parallelto") {
    EmuBenchmarkRun(
      """
        | word output @$c000
        | void main () {
        |   byte i
        |   output = 0
        |   for i,0,parallelto,5 {
        |     output += i
        |   }
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(15))
  }
  test("For-paralleluntil") {
    EmuBenchmarkRun(
      """
        | word output @$c000
        | void main () {
        |   byte i
        |   output = 0
        |   for i,0,paralleluntil,6 {
        |     output += i
        |   }
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(15))
  }
}
