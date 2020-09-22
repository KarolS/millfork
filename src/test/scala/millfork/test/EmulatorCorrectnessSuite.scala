package millfork.test

import millfork.test.emu.EmuUnoptimizedNative65816Run
import org.scalatest.{AppendedClues, FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class EmulatorCorrectnessSuite extends FunSuite with Matchers with AppendedClues {

  test("TXA should not clobber AH") {
    val m = EmuUnoptimizedNative65816Run(
      """
        |
        |asm void main() {
        |  SEP #$30
        |  LDA #1
        |  XBA
        |  LDX #3
        |  TXA
        |  REP #$30
        |  STA $700
        |  RTS
        |}
        |""".stripMargin)
    m.readWord(0x700) should equal(0x103)
  }
}
