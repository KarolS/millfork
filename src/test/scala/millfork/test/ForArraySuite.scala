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
      val src = """
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
      """.stripMargin
    val m = EmuSuperOptimizedRun(src)
    m.readByte(0xc000) should equal(18)
    EmuCrossPlatformBenchmarkRun(Cpu.Cmos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(src) { m =>
      m.readByte(0xc000) should equal(18)
    }
  }

  test("Issue #125") {
      val src = """
        | byte output @$c000
        | import stdio
        | array(byte) data=[1,2,3,4]
        | void main(){
        |    byte i
        |    for i:data{
        |        output += i
        |    }
        |}
      """.stripMargin
    val m = EmuOptimizedRun(src)
    m.readByte(0xc000) should equal(6)
  }

  test("Issue #125 (ver. 2)") {
      val src = """
        | byte output @$c000
        | import stdio
        | array(byte) data=[1,2,3,4]
        | void main(){
        |    byte i,v
        |    for i,v:data{
        |        output += v
        |    }
        |}
      """.stripMargin
    val m = EmuOptimizedRun(src)
    m.readByte(0xc000) should equal(10)
  }
}
