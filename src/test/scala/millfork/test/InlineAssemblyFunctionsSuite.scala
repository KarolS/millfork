package millfork.test

import millfork.assembly.opt.DangerousOptimizations
import millfork.test.emu.EmuBenchmarkRun
import millfork.{Cpu, OptimizationPresets}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class InlineAssemblyFunctionsSuite extends FunSuite with Matchers {

  test("Poke test 1") {
    EmuBenchmarkRun(
      """
        | inline asm void poke(word ref addr, byte const value) {
        |    ?LDA #value
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
        | inline asm byte peek(word ref addr) {
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
        | inline asm void poke(word const addr, byte const value) {
        |    ?LDA #value
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
        | inline asm byte peek(word const addr) {
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
}
