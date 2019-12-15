package millfork.test

import millfork.Cpu
import millfork.test.emu.EmuCrossPlatformBenchmarkRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class BooleanOptimizationSuite extends FunSuite with Matchers {

  test("Cases") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        | noinline bool f1(byte a) = a & 1 != 0
        | noinline bool f2(byte a) = a & 4 != 4
        | noinline bool f3(byte a) = a ^ 5 == 6
        | noinline bool f4(byte a) = a == 0
        | noinline bool f5(byte a) = a == 1
        | void main() {
        |   f1(0)
        |   f2(0)
        |   f3(0)
        |   f4(0)
        |   f5(0)
        | }
      """.stripMargin){ m =>

    }
  }

}
