package millfork.test
import millfork.Cpu
import millfork.test.emu.{EmuBenchmarkRun, EmuCrossPlatformBenchmarkRun, EmuUltraBenchmarkRun, EmuUnoptimizedCrossPlatformRun}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class InterruptSuite extends FunSuite with Matchers {

    test("Interrupts should compile") {
      EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Cmos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
        """
          | interrupt void f() {
          |   asm { nop }
          | }
          | kernal_interrupt void g() {
          |   asm { nop }
          | }
          | array hold = [ @word [f.addr, g.addr]]
          | void main() {
          | }
        """.stripMargin){m => }
    }
}
