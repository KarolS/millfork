package millfork.test

import millfork.Cpu
import millfork.test.emu.EmuUnoptimizedCrossPlatformRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class ConstantSuite extends FunSuite with Matchers {

    test("Constants should fold") {
      EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086)(
        """
          | array Sieve[4]
          | const byte two = 2
          | array __screen[4] = [4 / two, 4 %% two, 0, 0]
          | byte vic_mem
          | void main() {
          |   vic_mem = lo( ((Sieve.addr >> 10) & 8) | ((__screen.addr >> 6) & $f0) )
          | }
        """.stripMargin){m => }
    }
}