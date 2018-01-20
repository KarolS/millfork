package millfork.test

import millfork.test.emu.{EmuBenchmarkRun, EmuUnoptimizedRun}
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

  test("For-to 2") {
    EmuBenchmarkRun(
      """
        | word output @$c000
        | byte five
        | void main () {
        |   init()
        |   byte i
        |   output = 0
        |   for i,0,to,five {
        |     output += i
        |   }
        | }
        | void init() {
        |   five = 5
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

  test("Various loops") {
    EmuUnoptimizedRun(
      """
        | void init() {
        |     zero = 0
        |     ff = $ff
        | }
        | byte zero
        | byte ff
        | byte flag @$c000
        | void main () {
        |     init()
        |     byte i
        |     flag = 0
        |     for i, zero, until, ff { }
        |     flag = 1
        |     for i, zero, to, ff { }
        |     flag = 2
        |     for i, ff, downto, zero { }
        |     flag = 3
        |     for i, zero, paralleluntil, ff { }
        |     flag = 4
        |     for i, zero, parallelto, ff { }
        |     flag = 5
        |     for i, ff, until, zero { _panic() }
        |     flag = 6
        |     for i, ff, paralleluntil, zero { _panic() }
        |     flag = 7
        |     for i, ff, paralleluntil, zero { _panic() }
        |     flag = 8
        |     for i, zero, until, ff { }
        |     flag = 9
        | }
        | void _panic(){while(true){}}
      """.stripMargin)
  }
}
