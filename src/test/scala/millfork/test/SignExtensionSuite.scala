package millfork.test

import millfork.test.emu.EmuUnoptimizedRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class SignExtensionSuite extends FunSuite with Matchers {

  test("Sbyte to Word") {
    EmuUnoptimizedRun("""
        | word output @$c000
        | void main () {
        |   sbyte b
        |   b = -1
        |   output = b
        | }
      """.stripMargin).readWord(0xc000) should equal(0xffff)
  }
  test("Sbyte to Word 2") {
    EmuUnoptimizedRun("""
        | word output @$c000
        | void main () {
        |   output = b()
        | }
        | sbyte b() {
        |   return -1
        | }
      """.stripMargin).readWord(0xc000) should equal(0xffff)
  }
  test("Sbyte to Long") {
    EmuUnoptimizedRun("""
        | long output @$c000
        | void main () {
        |   output = 421
        |   output += b()
        | }
        | sbyte b() {
        |   return -1
        | }
      """.stripMargin).readLong(0xc000) should equal(420)
  }
}
