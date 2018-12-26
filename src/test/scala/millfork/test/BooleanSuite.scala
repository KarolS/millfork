package millfork.test

import millfork.Cpu
import millfork.test.emu.EmuCrossPlatformBenchmarkRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class BooleanSuite extends FunSuite with Matchers {

  test("Not") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | byte output @$c000
        | array input = [5,6,7]
        | void main () {
        |   byte a
        |   a = 5
        |   if not(a < 3) {output = 4}
        |   if not(a > 3) {output = 3}
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(4))

  }


  test("And") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | byte output @$c000
        | array input = [5,6,7]
        | void main () {
        |   byte a
        |   byte b
        |   a = 5
        |   b = 5
        |   if a > 3 && b > 3 {output = 4}
        |   if a < 3 && b > 3 {output = 5}
        |   if a > 3 && b < 3 {output = 2}
        |   if a < 3 && b < 3 {output = 3}
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(4))
  }


  test("Or") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | byte output @$c000
        | array input = [5,6,7]
        | void main () {
        |   byte a
        |   byte b
        |   a = 5
        |   b = 5
        |   output = 0
        |   if a > 3 || b > 3 {output += 4}
        |   if a < 3 || b > 3 {output += 5}
        |   if a > 3 || b < 3 {output += 2}
        |   if a < 3 || b < 3 {output = 30}
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(11))
  }

  test("Constant conditions big suite") {
    val code ="""
      | byte output @$c000
      | const pointer outside = $bfff
      | void main () {
      |   output = 1
      |   if 1 == 1 { pass() }
      |   if 1 == 2 { fail() }
      |   if 1 != 1 { fail() }
      |   if 1 != 2 { pass() }
      |   if 1 < 2 { pass() }
      |   if 1 < 1 { fail() }
      |   if sbyte(1) < sbyte(255) { fail() }
      |   if sbyte(1) > sbyte(255) { pass() }
      |   if sbyte(1) > 0 { pass() }
      |   if sbyte(-1) > 0 { fail() }
      |   if 00001 < 00002 { pass() }
      |   if 00001 > 00002 { fail() }
      |   if 00001 != 00002 { pass() }
      |   if 00001 == 00002 { fail() }
      | }
      | inline void pass() { output += 1 }
      | noinline void fail() { outside[0] = 0 }
      """.stripMargin
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(code)(_.readByte(0xc000) should equal(code.sliding(4).count(_ == "pass")))
  }
}
