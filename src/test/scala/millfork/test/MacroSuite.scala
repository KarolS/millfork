package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuBenchmarkRun, EmuCrossPlatformBenchmarkRun, EmuUnoptimizedCrossPlatformRun, ShouldNotCompile}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class MacroSuite extends FunSuite with Matchers {

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
}
