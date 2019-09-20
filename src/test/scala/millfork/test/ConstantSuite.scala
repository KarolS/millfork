package millfork.test

import millfork.Cpu
import millfork.env.{BasicPlainType, DerivedPlainType, NumericConstant}
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

    test("Constants should be negative when needed") {
      def signed(size: Int) = DerivedPlainType("", BasicPlainType("", size), isSigned = true, isPointy = false)
      NumericConstant(0xff, 1).isProvablyNegative(signed(1)) should be(true)
      NumericConstant(0x7f, 1).isProvablyNegative(signed(1)) should be(false)
      NumericConstant(0xff, 2).isProvablyNegative(signed(2)) should be(false)
      NumericConstant(0xff0f, 2).isProvablyNegative(signed(1)) should be(false)
      NumericConstant(-0x4000, 8).isProvablyNegative(signed(1)) should be(false)
      NumericConstant(0x7f, 2).isProvablyNegative(signed(2)) should be(false)
      NumericConstant(-1, 8).isProvablyNegative(signed(8)) should be(true)
    }
}