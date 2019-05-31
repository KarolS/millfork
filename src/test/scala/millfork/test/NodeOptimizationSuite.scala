package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuNodeOptimizedRun, EmuUnoptimizedCrossPlatformRun}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class NodeOptimizationSuite extends FunSuite with Matchers {

  test("Unreachable after return") {
    EmuNodeOptimizedRun(
      """
        | byte crash @$ffff
        | void main () {
        |   return
        |   crash = 2
        | }
      """.stripMargin)
  }

  test("Unused local variable") {
    EmuNodeOptimizedRun(
      """
        | byte crash @$ffff
        | void main () {
        |   byte a
        |   a = crash
        | }
      """.stripMargin)
  }

  test("Returning one variable") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086)(
      """
        | int64 output @$c000
        | void main () {
        |   int64 tmp
        |   tmp = f()
        |   output += tmp
        |   tmp = g($2000000)
        |   output += tmp
        | }
        | noinline int64 f() {
        |   int64 a
        |   a = 0
        |   a.b3 = 1
        |   return a
        | }
        | noinline int64 g(int64 p) = p
      """.stripMargin) { m =>
      m.readLong(0xc000) should equal (0x3000000)
    }
  }
}
