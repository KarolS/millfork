package millfork.test

import millfork.test.emu.EmuUnoptimizedRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class BasicSymonTest extends FunSuite with Matchers {
  test("Empty test") {
    EmuUnoptimizedRun(
      """
        | void main () {
        |
        | }
      """.stripMargin)
  }

  test("Panic test") {
    EmuUnoptimizedRun(
      """
        | byte output @$c000
        | void main () {
        |   panic()
        | }
        | macro asm void panic() {
        |    JSR _panic
        | }
        | void _panic() {
        |   asm {
        |     JSR doNothing
        |   }
        |   output = 1
        | }
        | void doNothing() { }
      """.stripMargin).readByte(0xc000) should equal(1)
  }

  test("Allocation test") {
    val src =
      """
 byte output @$c000
 void main () {
   function()
 }
 array thing @$20F = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
 void function() {
   output = 0
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
 }
      """
    EmuUnoptimizedRun(src).readByte(0xc000) should equal(src.count(_ == '+'))
  }

  test("Byte assignment") {
    EmuUnoptimizedRun(
      """
        | byte output @$c000
        | void main () {
        |  output = (1)
        | }
      """.stripMargin).readByte(0xc000) should equal(1)
  }

  test("Preallocated variables") {
    val m = EmuUnoptimizedRun(
      """
        | array output [2] @$c000
        | byte number = 4
        | void main () {
        |  output[0] = number
        |  number += 1
        |  output[1] = number
        | }
      """.stripMargin)
      m.readByte(0xc000) should equal(4)
      m.readByte(0xc001) should equal(5)
  }

  test("Preallocated variables 2") {
    val m = EmuUnoptimizedRun(
      """
        | word output @$c000
        | word number = 344
        | void main () {
        |  output = number
        | }
      """.stripMargin)
      m.readWord(0xc000) should equal(344)
  }

  test("Else if") {
    val m = EmuUnoptimizedRun(
      """
        | byte output @$c000
        | void main () {
        |  if 1 == 2 {
        |    output = 3
        |  } else if 1 == 1 {
        |    output = 4
        |  } else {
        |    output = 65
        |  }
        | }
      """.stripMargin)
      m.readWord(0xc000) should equal(4)
  }

  test("Segment syntax") {
    EmuUnoptimizedRun(
      """
        | segment(default)byte output @$c000
        | segment(default)array x[3]
        | segment(default)void main () {
        | }
      """.stripMargin)
  }
}
