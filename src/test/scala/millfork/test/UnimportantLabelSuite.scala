package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuCrossPlatformBenchmarkRun, EmuUnoptimizedCrossPlatformRun}
import org.scalatest.{FunSuite, Matchers}


/**
  * @author Karol Stasiak
  */
class UnimportantLabelSuite extends FunSuite with Matchers {

  test("Unimportant labels") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos)(
      """
        |
        |  array(word) output[6] @$c000
        |  void main() {
        |   output[0] = segment.default.start
        |   output[1] = segment.default.end
        |   output[2] = segment.default.heapstart
        |   output[3] = segment.default.length
        |   output[4] = segment.default.bank
        |   output[5] = __rwdata_start
        |   output[6] = __rwdata_end
        |   output[7] = __heap_start
        |  }
      """.stripMargin) { m =>
    }
  }
}