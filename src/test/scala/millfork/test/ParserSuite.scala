package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuUnoptimizedCrossPlatformRun, EmuUnoptimizedRun, ShouldNotCompile, ShouldNotParse, ShouldParse}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class ParserSuite extends FunSuite with Matchers  {

  test("Require space between flags and types") {
    ShouldNotCompile(
      """
        | constbyte a = 2
        |
      """.stripMargin)
  }

  test("Comment after last constant") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        |
        | byte __panic() = 1
        | const byte x = 5;
        | const byte a = 2;
        |
        |void main () {}//
        |
        |
        |//
        |
      """.stripMargin){ m =>

    }
  }

  test("Allow negation") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        | array output[100] @$c000
        | const byte x = 5
        | void main() {
        | output[0] = -x
        | output[1] = 0 | (-x)
        | output[2] = 1 * (-x)
        | if (x != -x) { output[3] = 5 }
        | }
      """.stripMargin){ m =>
      m.readByte(0xc000) should equal(251)
      m.readByte(0xc001) should equal(251)
      m.readByte(0xc002) should equal(251)
      m.readByte(0xc003) should equal(5)
    }
  }

  test("I hate Millfork, it won't let me prank my coworkers") {
    ShouldNotParse(
      """
        |const array a = ‟aa"
        |""".stripMargin)
  }

  test("Various valid identifiers") {
    // yes, this spews warnings.
    // I'll deal with them later.
    EmuUnoptimizedRun(
      """
        |byte a
        |byte a$a
        |byte a.$a
        |byte a.a
        |byte a.$a
        |byte a$a$a
        |byte a$.$a
        |byte a...
        |void main(){
        |  a += 0
        |  a$a += 0
        |  a.$a += 0
        |  a.a += 0
        |  a.$a += 0
        |  a$a$a += 0
        |  a$.$a += 0
        |  a... += 0
        |}
        |""".stripMargin)
  }

  test("EOL error reporting") {
    ShouldNotParse(
      """
        |void main() {
        |    for i,
        |}
        |""".stripMargin)
  }

  test("Very long expression test") {
    ShouldParse(
      """
        |void main(){
        |  const byte a = 1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1
        |}
        |""".stripMargin)
  }

}
