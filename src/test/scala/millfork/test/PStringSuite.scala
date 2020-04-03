package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuUnoptimizedCrossPlatformRun, ShouldNotCompile, ShouldNotParse}
import org.scalatest.{AppendedClues, FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class PStringSuite extends FunSuite with Matchers with AppendedClues {
  test("pstrlen") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        |byte pstrlen(pointer str) {
        |    return str[0]
        |}
        |byte output @ $c000
        |void main() {
        |    output = pstrlen("hello"p)
        |}
        |""".stripMargin) { m =>
      m.readByte(0xc000) should be (5)
    }
  }

  test("P-string literal too long") {
    val longString = "a" * 256
    ShouldNotParse(s"""const pointer p = "$longString"p""")
  }
}
