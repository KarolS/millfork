package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuBenchmarkRun, EmuCrossPlatformBenchmarkRun, EmuUnoptimizedRun}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class BreakContinueSuite extends FunSuite with Matchers {

  test("Break from one-iteration loop 1") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | byte output @$c000
        | void main () {
        |   output = 0
        |   do {
        |     break
        |     output += 1
        |   } while false
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(0))
  }

  test("Break from one-iteration loop 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | byte output @$c000
        | void main () {
        |   output = 0
        |   byte i
        |   for i,0,to,0 {
        |     break
        |     output += 1
        |   }
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(0))
  }

  test("Break from infinite loop 1") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | byte output @$c000
        | void main () {
        |   output = 0
        |   while true {
        |     output += 1
        |     break
        |     output += 1
        |   }
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(1))
  }

  test("Break and continue from infinite loop 1") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | byte output @$c000
        | void main () {
        |   output = 0
        |   while true {
        |     if output != 0 { break }
        |     output += 1
        |     continue
        |     output += 1
        |   }
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(1))
  }

  test("Nested break") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | byte output @$c000
        | void main () {
        |   output = 0
        |   do {
        |     output += 1
        |     while true {
        |       break while
        |     }
        |     output += 1
        |   } while false
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(2))
  }

  test("Continue in for loop 1") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | byte output @$c000
        | byte counter @$c001
        | void main () {
        |   output = 0
        |   byte i
        |   for i,0,paralleluntil,50 {
        |     counter += 1
        |     if i != 30 { continue }
        |     output = i
        |     break
        |   }
        | }
      """.stripMargin){m =>
      m.readByte(0xc000) should equal(30)
      m.readByte(0xc001) should be > 10
    }
  }
}
