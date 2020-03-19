package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuBenchmarkRun, EmuCrossPlatformBenchmarkRun, EmuOptimizedCmosRun, EmuOptimizedHudsonRun, EmuOptimizedRun, EmuUndocumentedRun, EmuUnoptimizedCrossPlatformRun, EmuUnoptimizedHudsonRun, EmuUnoptimizedRun, ShouldNotCompile}
import org.scalatest.{AppendedClues, FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class AbiSuite extends FunSuite with Matchers with AppendedClues {

  test("Test parameter storage #1") {
    EmuBenchmarkRun(
      """
        | byte output @$c000
        | void main() {
        |   f(42)
        | }
        |
        | noinline void f(byte parameter) {
        |   asm {
        |     ldy parameter
        |     sty output
        |   }
        | }
        |""".stripMargin) { m =>
      m.readByte(0xc000) should equal(42)
    }
  }
}
