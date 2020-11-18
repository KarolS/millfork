package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuBenchmarkRun, EmuOptimizedAccordingToLevelRun, EmuUnoptimizedCrossPlatformRun, EmuUnoptimizedRun, ShouldNotCompile, ShouldNotParse}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class OptimizationHintsSuite extends FunSuite with Matchers  {

  test("Optimization hints test 1") {
    EmuBenchmarkRun("""
        | asm void putchar(byte register(a) character) ¥( preserves_a, preserves_x, preserves_y ) @$ffd2 extern
        | noinline bool should_print(byte a) = a == 5
        | void main() {
        |   byte i
        |   if should_print(3) {
        |     for i,0,parallelto,255 {
        |       putchar(i)
        |       putchar(i)
        |     }
        |   }
        | }
        |""".stripMargin) { m =>

    }
  }
}
