package millfork.test

import millfork.test.emu.{EmuBenchmarkRun, EmuUnoptimizedRun}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class TextCodecSuite extends FunSuite with Matchers {

  test("Char literals") {
    val m = EmuUnoptimizedRun(
      """
        | void main() {
        |   if 'a'ascii != 'a' ascii  { poke($bfff, 0) }
        |   if '¥'jis   != '\' ascii  { poke($bffe, 0) }
        |   if '£'pet   != '\' ascii  { poke($bffd, 0) }
        |   if '£'bbc   != 'é' iso_se { poke($bffc, 0) }
        | }
        | macro asm void poke(word const addr, byte a) {
        |   STA addr
        | }
      """.stripMargin)
  }

  test("Lenient encoding") {
    val m = EmuUnoptimizedRun(
      """
        | void main() {
        |   if 'å' != 'a'  { poke($bfff, 0) }
        |   if '÷' != '/'  { poke($bffd, 0) }
        |   if 'π' != '?'  { poke($bffc, 0) }
        | }
        | macro asm void poke(word const addr, byte a) {
        |   STA addr
        | }
      """.stripMargin)
  }

  test("Unicode") {
    val m = EmuUnoptimizedRun(
      """
        | void main() {
        |   pointer p
        |   p = "a"utf8z
        |   if p[0] != 'a'  { poke($bfff, 0) }
        |   if p[1] != 0    { poke($bffe, 0) }
        |   p = "a"utf16bez
        |   if p[0] != 0    { poke($bffd, 0) }
        |   if p[1] != 'a'  { poke($bffc, 0) }
        |   if p[2] != 0    { poke($bffb, 0) }
        |   if p[3] != 0    { poke($bffa, 0) }
        |   p = "a"utf16lez
        |   if p[0] != 'a'  { poke($bff9, 0) }
        |   if p[1] != 0    { poke($bff8, 0) }
        |   if p[2] != 0    { poke($bff7, 0) }
        |   if p[3] != 0    { poke($bff6, 0) }
        |   p = "𓀀"utf8z
        |   if p[0] == 0    { poke($bff3, p[0]) }
        |   if p[1] == 0    { poke($bff2, p[1]) }
        |   if p[2] == 0    { poke($bff1, p[2]) }
        |   if p[3] == 0    { poke($bff0, p[3]) }
        |   if p[4] != 0    { poke($bfef, p[4]) }
        | }
        | macro asm void poke(word const addr, byte a) {
        |   STA addr
        | }
      """.stripMargin)
  }

  test("Escape sequences") {
    val m = EmuUnoptimizedRun(
      """
        | void main() {
        |   pointer p
        |   p = "{n}"pet
        |   output = p[0]
        | }
        | byte output @$c000
      """.stripMargin)
    m.readByte(0xc000) should equal(13)
  }

  test("UTF-32") {
    val m = EmuUnoptimizedRun(
      """
        |pointer output @$c000
        |byte output2 @$c002
        |array test = "a"utf32bez
        | void main() {
        |   output = test.addr
        |   output2 = test.length
        | }
      """.stripMargin)
    m.readWord(0xc002) should equal(8)
    val addr = m.readWord(0xc000)
    m.readByte(addr + 0) should equal(0)
    m.readByte(addr + 1) should equal(0)
    m.readByte(addr + 2) should equal(0)
    m.readByte(addr + 3) should equal(97)
    m.readByte(addr + 4) should equal(0)
    m.readByte(addr + 5) should equal(0)
    m.readByte(addr + 6) should equal(0)
    m.readByte(addr + 7) should equal(0)
  }


}
