package millfork.test

import millfork.Cpu
import millfork.test.emu.EmuCrossPlatformBenchmarkRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class CmosSuite extends FunSuite with Matchers {

  test("Zero store 1") {
    EmuCrossPlatformBenchmarkRun(Cpu.Cmos, Cpu.Z80, Cpu.Intel8080)("""
        | word output @$c000
        | void main () {
        |  output = 1
        |  output = 0
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(0))
  }
  test("Zero store 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Cmos, Cpu.Z80, Cpu.Intel8080)("""
        | byte output @$c000
        | void main () {
        |  output = 1
        |  output = 0
        |  output += 1
        |  output <<= 1
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(2))
  }
}
