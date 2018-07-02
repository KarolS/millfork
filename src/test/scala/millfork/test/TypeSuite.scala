package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuBenchmarkRun, EmuCrossPlatformBenchmarkRun}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class TypeSuite extends FunSuite with Matchers {

  test("Word to word") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80)("""
        | word output @$c000
        | void main () {
        |  output = word(0x203)
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(0x203))
  }
  test("Byte to sbyte") {
    EmuBenchmarkRun("""
        | word output @$c000
        | void main () {
        |  if sbyte(0) > sbyte(255) { output = word(0x203) }
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(0x203))
  }
}
