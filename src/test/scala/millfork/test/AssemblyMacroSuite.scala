package millfork.test

import millfork.test.emu.{EmuBenchmarkRun, EmuUnoptimizedRun, ShouldNotParse}
import org.scalatest.{AppendedClues, FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class AssemblyMacroSuite extends FunSuite with Matchers with AppendedClues {

  test("Poke test 1") {
    EmuBenchmarkRun(
      """
        | macro asm void poke(word ref addr, byte a) {
        |    STA addr
        | }
        |
        | byte output @$c000
        | void main () {
        |   poke(output, 5)
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(5)
    }
  }
  test("Peek test 1") {
    EmuBenchmarkRun(
      """
        | macro asm byte peek(word ref addr) {
        |    ?LDA addr
        | }
        |
        | byte output @$c000
        | void main () {
        |   byte a
        |   a = 5
        |   output = peek(a)
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(5)
    }
  }
  test("Poke test 2") {
    EmuBenchmarkRun(
      """
        | macro asm void poke(word const addr, byte a) {
        |    STA addr
        | }
        |
        | byte output @$c000
        | void main () {
        |   poke($c000, 5)
        |   poke($c001, 5)
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(5)
    }
  }

  test("Peek test 2") {
    EmuBenchmarkRun(
      """
        | macro asm byte peek(word const addr) {
        |    ?LDA addr
        | }
        |
        | byte output @$c000
        | void main () {
        |   byte a
        |   a = 5
        |   output = peek(a.addr)
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(5)
    }
  }

  test("Labels test") {
    EmuBenchmarkRun(
      """
        | macro asm void doNothing () {
        |    JMP label
        |    label:
        | }
        |
        | byte output @$c000
        | void main () {
        |   output = 0
        |   doNothing()
        |   output += 1
        |   doNothing()
        |   output += 1
        |   doNothing()
        |   output += 1
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(3)
    }
  }

  test("All assembly macro param types") {
    val m = EmuUnoptimizedRun(
      """
        | array output [3] @$c000
        |
        | macro asm void test0(byte register(a) value) {
        |   sta $c000
        | }
        | macro asm void test1(byte ref variable) {
        |   lda variable
        |   sta $c001
        | }
        | macro asm void test2(byte const constant) {
        |   lda #constant
        |   sta $c002
        | }
        |
        | void main() {
        |   byte variable
        |   variable = 42
        |   test0(42)
        |   test1(variable)
        |   test2(42)
        | }
        |""".stripMargin)
    m.readByte(0xc000) should equal(42) withClue "$c000"
    m.readByte(0xc001) should equal(42) withClue "$c001"
    m.readByte(0xc002) should equal(42) withClue "$c002"
  }

  test("Invalid assembly macro param types") {
    ShouldNotParse(
      """
        | macro asm void test0(byte call value) {
        | }
        |""".stripMargin)
  }
}
