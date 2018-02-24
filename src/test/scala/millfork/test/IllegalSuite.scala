package millfork.test

import millfork.test.emu.EmuUndocumentedRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class IllegalSuite extends FunSuite with Matchers {

  test("ALR test 1") {
    val m = EmuUndocumentedRun("""
        | byte output @$c000
        | byte input  @$cfff
        | void main () {
        |   init()
        |   output = (input & 0x45) >> 1
        | }
        | void init() {
        |   input = $45
        | }
      """.stripMargin)
    m.readByte(0xc000) should equal(0x22)
  }

  test("ALR test 2") {
    val m = EmuUndocumentedRun("""
        | byte output @$c000
        | byte input  @$cfff
        | void main () {
        |   init()
        |   output = (input >> 1) + 1
        | }
        | void init() {
        |   input = $8A
        | }
      """.stripMargin)
    m.readByte(0xc000) should equal(0x46)
  }

  test("ANC test") {
    val m = EmuUndocumentedRun("""
        | byte output @$c000
        | byte input  @$cfff
        | void main () {
        |   init()
        |   output = (input & $45) + 1
        | }
        |
        | void init() {
        |   input = $45
        | }
      """.stripMargin)
    m.readByte(0xc000) should equal(0x46)
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

  test("DCP test") {
    val m = EmuUndocumentedRun("""
        | byte output @$c000
        | array integers [10] @$c060
        | void main () {
        |   byte i
        |   for i,0,paralleluntil,10 { integers[i] += i }
        |   output = loop()
        | }
        | byte loop () {
        |   byte result
        |   result = 0
        |   byte i
        |   i = 7
        |   do {
        |     result += integers[i]
        |     integers[i + i] += 1
        |     integers[i] += 1
        |     i -= 1
        |   } while i != 0
        |   return result
        | }
      """.stripMargin)
    m.readByte(0xc000) should equal(28)
  }

  test("SLO test") {
    val m = EmuUndocumentedRun("""
        | long output @$c000
        | byte main () {
        |   output = five()
        |   output <<= 1
        |   return output
        | }
        | byte five () {
        |   return 5
        | }
      """.stripMargin)
    m.readLong(0xc000) should equal(10)
  }

  test("SAX test") {
    val m = EmuUndocumentedRun("""
        | byte output @$c000
        | void main () {
        |   byte a
        |   byte b
        |   byte c
        |   b = five(a)
        |   c = five(a)
        |   a = c
        |   five(a)
        |   c = a & b
        |   output = a + b + c
        | }
        | byte five (byte ignored) {
        |   return 5
        | }
      """.stripMargin)
    m.readLong(0xc000) should equal(15)
  }

  test("LAX test 1") {
    val m = EmuUndocumentedRun("""
        | word output @$c000
        | void main () {
        |   output = five(5)
        | }
        | word five (byte a) {
        |   return a:a
        | }
      """.stripMargin)
    m.readLong(0xc000) should equal(0x505)
  }

  test("LAX test 2") {
    val m = EmuUndocumentedRun("""
        | byte output @$c000
        | void main () {
        |   output = five(1)
        | }
        | byte five (byte a) {
        |   byte b
        |   b = a
        |   a += 4
        |   return b & a
        | }
      """.stripMargin)
    m.readLong(0xc000) should equal(1)
  }

  test("SBX test 1") {
    val m = EmuUndocumentedRun("""
        | array output [10] @$c000
        | void main () {
        |   output[6] = 7
        |   five(1)
        | }
        | void five (byte a) {
        |   byte b
        |   b = a
        |   output[b] += 1
        |   b += 5
        |   output[b] += 1
        | }
      """.stripMargin)
    m.readByte(0xc006) should equal(8)
  }


  test("ISC/DCP test 2") {
    val m = EmuUndocumentedRun("""
        | array output [3] @$c000
        | void main () {
        |   init()
        |   pointer p
        |   p = o()
        |   p[1] -= 1
        |   p[2] += 1
        | }
        | word o () {
        |   return output.addr
        | }
        | void init() {
        |   output[0] = 4
        |   output[1] = 4
        |   output[2] = 4
        | }
      """.stripMargin)
    m.readByte(0xc001) should equal(3)
    m.readByte(0xc002) should equal(5)
  }
}
