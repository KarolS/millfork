package millfork.test

import millfork.{Cpu, OptimizationPresets}
import millfork.assembly.mos.opt.{AlwaysGoodOptimizations, DangerousOptimizations}
import millfork.test.emu._
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class ForArraySuite extends FunSuite with Matchers {

  test("Basic for-array test") {
    val m = EmuSuperOptimizedRun(
      """
        | byte output @$c000
        | array input = for i,0,until,8 [i + i]
        |
        | array useless0 = for x,0,until,8 for y,0,until,8 [0]
        | array useless1 = for x,0,until,8 [for y,0,until,8 [0]]
        | array useless2 = for x,0,until,8 "test" scr
        | array useless3 = for x,0,until,8 [1 << x]
        | array useless4 = for x,0,until,4 [(3 << (x * 2)) ^ 0xff]
        | array useless5 = [
        |   for x,0,until,4 [3]
        | ]
        | array useless6 = [
        |   "foo" ascii,
        |   for x,0,until,4 [3]
        | ]
        | array useless7 = [
        |   7,
        |   for x,0,until,4 [3]
        | ]
        |
        | void main () {
        |   output = useless0[0] + useless1[0] + useless2[0] + useless3[0] + useless4[0] + useless5[0] + useless6[0] + useless7[0]
        |   output = input.length + input[5]
        | }
      """.stripMargin)
    m.readByte(0xc000) should equal(18)
  }
}
