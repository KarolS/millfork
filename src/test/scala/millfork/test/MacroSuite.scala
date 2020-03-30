package millfork.test

import millfork.Cpu
import millfork.output.MemoryBank
import millfork.test.emu.{EmuBenchmarkRun, EmuCrossPlatformBenchmarkRun, EmuOptimizedAccordingToLevelRun, EmuOptimizedRun, EmuUnoptimizedCrossPlatformRun, EmuUnoptimizedRun, ShouldNotCompile, ShouldNotParse}
import org.scalatest.{AppendedClues, FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class MacroSuite extends FunSuite with Matchers with AppendedClues {

  test("Most basic test") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | macro void run(byte x) {
        |    output = x
        | }
        |
        | byte output @$c000
        |
        | void main () {
        |   byte a
        |   a = 7
        |   run(a)
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(7)
    }
  }

  test("Macros in assembly") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | macro void run(byte x) {
        |    output = x
        | }
        |
        | byte output @$c000
        |
        | void main () {
        |   byte a
        |   a = 7
        |   asm {
        |     + run(a)
        |   }
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(7)
    }
  }

  test("Macros with loops and clashing variable names") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086)(
      """
        | macro void run(byte x) {
        |    while x != 0 {
        |     output += 1
        |     x -= 1
        |    }
        | }
        |
        | byte output @$c000
        |
        | void main () {
        |   output = 0
        |   byte x
        |   x = 3
        |   run(x)
        |   x = 4
        |   run(x)
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(7)
    }
  }

  test("Macro parameter type mismatch") {
    ShouldNotCompile(
      """
        | byte input
        | byte output @$c000
        |
        |void main() {
        |    input = $FF
        |    test_signed_macro(input)
        |}
        |
        |macro void test_signed_macro(sbyte value) {
        |    if value > 3 {
        |        output = 1
        |    }
        |}
      """.stripMargin)
  }

  test("Macro void parameter") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        | byte input
        | byte output @$c000
        |
        |void main() {
        |    input = $FF
        |    test_signed_macro(input)
        |}
        |
        |macro void test_signed_macro(void value) {
        |    if value > 3 {
        |        output = 1
        |    }
        |}
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(1)
    }
  }

  test("Some important macro test") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        | byte input
        | byte output @$c000
        |
        |void main() {
        |    input = $FF
        |    test_signed_macro(input)
        |}
        |
        |macro void test_signed_macro(void value) {
        |    if sbyte(value) > 3 {
        |        output = 1
        |    } else {
        |        output = 3
        |    }
        |}
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(3)
    }
  }

  test("Accessing fields of macro parameters") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        |byte output @$c000
        |
        |word test = $0380
        |
        |void main() {
        |    test_macro(test)
        |}
        |
        |macro void test_macro(word value) {
        |    if value.hi > 0 {
        |        output = 1
        |    }
        |}
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(1)
    }
  }

  test("Accessing fields of macro parameters when using void") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        |byte output @$c000
        |
        |word test = $0380
        |
        |void main() {
        |    test_macro(test)
        |}
        |
        |macro void test_macro(void value) {
        |    if value.hi > 0 {
        |        output = 1
        |    }
        |}
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(1)
    }
  }

  test("Issue #46") {
    val src =
      """
        |bool output
        |
        |byte input
        |byte y
        |
        |void main() {
        |    bool result
        |    input = 13
        |    y = 12
        |    test(result, input)
        |
        |    if (result) {
        |        output = true
        |    }
        |}
        |
        |macro void test(bool returnVal, byte x) {
        |    returnVal = x >= y
        |    returnVal = returnVal && x <= y + 8
        |}
        |""".stripMargin


    def assertAtLeastOneTrueInZeroPage(m: MemoryBank): Unit = {
      val countOfTrues = 0.to(0xff).count(a => m.readByte(a) == 1)
      countOfTrues should be > (0)
    }

    assertAtLeastOneTrueInZeroPage(EmuOptimizedAccordingToLevelRun(1)(src))
    assertAtLeastOneTrueInZeroPage(EmuOptimizedAccordingToLevelRun(2)(src))
    assertAtLeastOneTrueInZeroPage(EmuOptimizedAccordingToLevelRun(3)(src))
  }

  test("All non-assembly macro param types") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        |  array(word) output [3] @$c000
        |
        |  macro void test0(byte const constant) {
        |     output[0] = constant
        |  }
        |  macro void test1(byte ref variable) {
        |     variable += 1
        |     output[1] = variable
        |  }
        |  macro void test2(byte call tooMuch) {
        |     output[2] = tooMuch
        |     output[2].lo &= $7f
        |  }
        |
        |  macro void twice(void call expr) {
        |      expr
        |      expr
        |  }
        |
        |  inline void inc0() {
        |      output[0] += 1
        |  }
        |  void main() {
        |    byte variable
        |    variable = 41
        |    output[0]=0
        |    output[1]=0
        |    output[2]=0
        |    test0(40)
        |    twice(inc0())
        |    test1(variable)
        |    test2(sbyte($80 | 42))
        |  }
        |""".stripMargin) { m =>
          m.readWord(0xc000) should equal(42) withClue "$c000"
          m.readWord(0xc002) should equal(42) withClue "$c002"
          m.readWord(0xc004) should equal(42) withClue "$c004"
    }
  }

  test("Macros as params to void call macros") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        |    byte output @$c000
        |    macro void inc_x() {
        |        x += 1
        |    }
        |    macro void inc(void ref x) {
        |        inc_x()
        |    }
        |    macro void perform_twice(void call f) {
        |        f
        |        f
        |    }
        |    byte add_two_3(byte x) {
        |        perform_twice(inc(x))
        |        return x
        |    }
        |
        |    void main() {
        |       output = add_two_3(40)
        |    }
        |
        |""".stripMargin) { m =>
          m.readByte(0xc000) should equal(42)
    }
  }

  test("Invalid non-assembly macro param types") {
    ShouldNotParse(
      """
        | macro void test0(byte register(a) value) {
        | }
        |""".stripMargin)
  }
}
