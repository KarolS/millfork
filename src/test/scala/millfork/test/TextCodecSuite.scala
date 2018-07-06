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
}
