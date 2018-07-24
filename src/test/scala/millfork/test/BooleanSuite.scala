package millfork.test

import millfork.Cpu
import millfork.test.emu.EmuCrossPlatformBenchmarkRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class BooleanSuite extends FunSuite with Matchers {

  test("Not") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080)(
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
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080)(
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
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080)(
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
}
