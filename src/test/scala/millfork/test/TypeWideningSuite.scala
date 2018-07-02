package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuBenchmarkRun, EmuCrossPlatformBenchmarkRun}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class TypeWideningSuite extends FunSuite with Matchers {

  test("Word after simple ops") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80)("""
        | word output @$c000
        | void main () {
        |  output = random()
        |  output = output.hi << 1
        | }
        | word random() {
        |   return $777
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(14))
  }
}
