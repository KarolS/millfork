package millfork.test

import millfork.assembly.mos.opt.{AlwaysGoodOptimizations, LaterOptimizations, VariableToRegisterOptimization}
import millfork.test.emu.{EmuBenchmarkRun, EmuRun, EmuUltraBenchmarkRun}
import millfork.{Cpu, OptimizationPresets}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class SecondAssemblyOptimizationSuite extends FunSuite with Matchers {

  test("Add-shift-add") {
    EmuBenchmarkRun(
      """
        | byte output @$c000
        | void main () {
        |   byte a
        |   a = two()
        |   output = ((a + 3) << 2) + 9
        | }
        | byte two() { return 2 }
      """.stripMargin) { m => m.readByte(0xc000) should equal(29) }
  }

  test("And-shift-and") {
    EmuBenchmarkRun(
      """
        | byte output @$c000
        | void main () {
        |   byte a
        |   a = ee()
        |   output = ((a & $dd) << 1) & $55
        | }
        | byte ee() { return $ee }
      """.stripMargin) { m => m.readByte(0xc000) should equal(0x10) }
  }

  test("Add with limit") {
    EmuBenchmarkRun(
      """
        | byte output @$c000
        | const byte start = 5
        | const byte limit = 234
        | void main () {
        |   output += 1
        |   if output == limit {
        |     output = start
        |   }
        | }
      """.stripMargin) { m => m.readByte(0xc000) should equal(1) }
  }

  test("User register instead of stack") {
    EmuBenchmarkRun(
      """
        | array output [4] @$c000
        | void main () {
        |   output[0] = double(2)
        | }
        | asm byte double(byte a) {
        |   ? asl
        |   ? pha
        |     lda output
        |   ? pla
        |   ? rts
        | }
      """.stripMargin) { m => m.readByte(0xc000) should equal(4) }
  }
}
