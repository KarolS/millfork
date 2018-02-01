package millfork.test

import millfork.assembly.opt.DangerousOptimizations
import millfork.test.emu.EmuBenchmarkRun
import millfork.{Cpu, OptimizationPresets}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class AssemblyMacroSuite extends FunSuite with Matchers {

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
}
