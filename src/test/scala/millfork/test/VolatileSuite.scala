package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuBenchmarkRun, EmuCrossPlatformBenchmarkRun, EmuSuperOptimizedRun, EmuUltraBenchmarkRun}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class VolatileSuite extends FunSuite with Matchers {

  test("Basic volatile test") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | word addr @$c000
        | volatile byte output @$c0ea
        | volatile byte unused_but_should_exist
        | byte unused_global
        | byte thing
        | void main () {
        |   static volatile byte unused_local
        |   f(55)
        |   addr = f.addr
        |   unused_local = 55
        | }
        | noinline void f(byte x) {
        |   output = 5
        |   thing = x
        |   output = x
        | }
      """.stripMargin) { m =>
      m.readByte(0xc0ea) should equal(55)
      var f = m.readWord(0xc000)
      var count = 0
      do {
        if (m.readWord(f) == 0xc0ea) count +=1
        f += 1
      } while (count < 2)
    }
  }
}
