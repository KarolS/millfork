package millfork.test

import millfork.test.emu.EmuUndocumentedRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class IllegalSuite extends FunSuite with Matchers {

  test("ALR test 1") {
    EmuUndocumentedRun("""
        | byte output @$c000
        | byte input  @$cfff
        | void main () {
        |   output = (input & 0x45) >> 1
        | }
      """.stripMargin)
  }

  test("ALR test 2") {
    EmuUndocumentedRun("""
        | byte output @$c000
        | byte input  @$cfff
        | void main () {
        |   output = (input & 0x45) >> 1
        | }
      """.stripMargin)
  }

  test("ISC/DCP test") {
    val m = EmuUndocumentedRun("""
        | array output [10] @$c000
        | void main () {
        |   stack byte a
        |   output[5] = 36
        |   output[7] = 52
        |   five()
        |   a = 5
        |   five()
        |   output[a] += 1
        |   output[a + 2] -= 1
        | }
        | byte five () {
        |   return 5
        | }
      """.stripMargin)
    m.readByte(0xc005) should equal(37)
    m.readByte(0xc007) should equal(51)
  }
}
