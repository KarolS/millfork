package millfork.test

import millfork.Cpu
import millfork.env.{BasicPlainType, DerivedPlainType, NumericConstant}
import millfork.test.emu.{EmuUnoptimizedCrossPlatformRun, ShouldNotCompile}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class ConstantSuite extends FunSuite with Matchers {

    test("Constants should fold") {
      EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086, Cpu.Motorola6809)(
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

  test ("Overflow errors should be nice") {
    ShouldNotCompile(
      """
        |word sum
        |void main() {
        |    sum = 246 + 18
        |}
        |""".stripMargin)
  }
  test("Special const functions should work") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | const array values = [111, if (0,1,2), if(1,2,3), min(2,3,4), max(2,3,4)
        | pointer output @$c000
        | void main() {
        |
        | }
      """.stripMargin){m =>
      val arrayStart = m.readWord(0xc000)
      m.readByte(arrayStart + 1) should equal(2)
      m.readByte(arrayStart + 2) should equal(2)
      m.readByte(arrayStart + 3) should equal(2)
      m.readByte(arrayStart + 4) should equal(4)
    }
  }

  test("Do not compile const functions with variables") {
    ShouldNotCompile(
      """
        | byte output
        | void main() {
        |    min(output,0)
        | }
      """.stripMargin)
  }
}
