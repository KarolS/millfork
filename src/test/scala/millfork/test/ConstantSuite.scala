package millfork.test

import millfork.Cpu
import millfork.env.{BasicPlainType, DerivedPlainType, NumericConstant}
import millfork.test.emu.{EmuBenchmarkRun, EmuOptimizedCmosRun, EmuUnoptimizedCrossPlatformRun, EmuUnoptimizedRun, ShouldNotCompile}
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
        | const array values = [111, if (0,1,2), if(1,2,3), min(2,3,4), max(2,3,4) ]
        | pointer output @$c000
        | void main() {
        |  output = values.addr
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

  test("Const-pure functions") {
    val m = EmuUnoptimizedRun(
      """
        | pointer output @$c000
        |
        | const byte twice(byte x) = x << 1
        | const byte abs(sbyte x) {
        |   if x < 0 { return -x }
        |   else {return x }
        | }
        |
        | const byte result = twice(30) + abs(-9)
        | const array values = [112, twice(21), abs(-4), result]
        |
        | void main() {
        |   output = values.addr
        | }
      """.stripMargin)
    val arrayStart = m.readWord(0xc000)
    m.readByte(arrayStart + 1) should equal(42)
    m.readByte(arrayStart + 2) should equal(4)
    m.readByte(arrayStart + 3) should equal(69)

  }

  test("Const-pure functions 2") {
    val m = EmuUnoptimizedRun(
      """
        | word output @$c000
        | const word BASE = $d800
        | const word pair(byte x, byte y) = (x+BASE.hi):y
        | const word result = pair(1,2)
        | void main() {
        |   output = result
        | }
      """.stripMargin)
    m.readWord(0xc000) should equal(0xd902)

  }

  test("Const-pure Fibonacci") {
    val m = EmuUnoptimizedRun(
      """
        | pointer output @$c000
        | byte output2 @ $c011
        |
        | const byte fib(byte x) {
        |   if x < 2 { return x }
        |   else {return fib(x-1) + fib(x-2) }
        | }
        |
        | const array values = [for i,0,until,12 [fib(i)]]
        |
        | void main() {
        |   output = values.addr
        |   output2 = fib(11)
        | }
        |
      """.stripMargin)
    val arrayStart = m.readWord(0xc000)
    m.readByte(arrayStart + 0) should equal(0)
    m.readByte(arrayStart + 1) should equal(1)
    m.readByte(arrayStart + 2) should equal(1)
    m.readByte(arrayStart + 3) should equal(2)
    m.readByte(arrayStart + 4) should equal(3)
    m.readByte(arrayStart + 5) should equal(5)
    m.readByte(arrayStart + 6) should equal(8)
    m.readByte(arrayStart + 7) should equal(13)
    m.readByte(arrayStart + 8) should equal(21)
    m.readByte(arrayStart + 9) should equal(34)
    m.readByte(arrayStart + 10) should equal(55)
    m.readByte(arrayStart + 11) should equal(89)
    m.readByte(0xc011) should equal(89)

  }

  test("Constant array sizes") {
    val m = EmuOptimizedCmosRun(
          """
            | const byte A = 8
            | const byte B = 8
            | const byte SIZE = A * B
            | array(byte) arr[SIZE] @$c000
            | void main() {
            |   arr[0] = 1
            | }
            |
          """.stripMargin)
    m.readByte(0xc000) should equal(1)
  }

  test("Large constants should work") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80)(
          """
            | const int24 A = 200
            | const int24 B = 8
            | const int24 C = A/B
            | array(int24) arr[10] @$c000
            | void main() {
            |   arr[0] = C
            |   arr[1] = A/B
            | }
            |
          """.stripMargin) {m =>
      m.readMedium(0xc000) should equal(200/8)
      m.readMedium(0xc003) should equal(200/8)
    }
  }
}
