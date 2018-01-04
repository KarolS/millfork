package millfork.test
import millfork.test.emu.EmuBenchmarkRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class AssemblySuite extends FunSuite with Matchers {

  test("Inline assembly") {
    EmuBenchmarkRun(
      """
        | byte output @$c000
        | void main () {
        |  output = 0
        |  asm {
        |    inc $c000
        |  }
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(1))
  }

  test("Assembly functions") {
    EmuBenchmarkRun(
      """
        | byte output @$c000
        | void main () {
        |  output = 0
        |  thing()
        | }
        | asm void thing() {
        |  inc $c000
        |  rts
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(1))
  }

  test("Empty assembly") {
    EmuBenchmarkRun(
      """
        | byte output @$c000
        | void main () {
        |  output = 1
        |  asm {}
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(1))
  }

  test("Passing params to assembly") {
    EmuBenchmarkRun(
      """
        | byte output @$c000
        | void main () {
        |  output = f(5)
        | }
        | asm byte f(byte a) {
        |   clc
        |   adc #5
        |   rts
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(10))
  }

  test("Inline asm functions") {
    EmuBenchmarkRun(
      """
        | byte output @$c000
        | void main () {
        |  output = 0
        |  f()
        |  f()
        | }
        | inline asm void f() {
        |   inc $c000
        |   rts
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(1))
  }

  test("Inline asm functions 2") {
    EmuBenchmarkRun(
      """
        | byte output @$c000
        | void main () {
        |  output = 0
        |  add(output, 5)
        |  add(output, 5)
        | }
        | inline asm void add(byte ref v, byte const c) {
        |   lda v
        |   clc
        |   adc #c
        |   sta v
        |   rts
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(5))
  }

  test("Adresses in asm") {
    EmuBenchmarkRun(
      """
        | word output @$c000
        | void main () {
        |  output = 0
        |  add256(output)
        | }
        | inline asm void add256(word ref v) {
        |   inc v+1
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(0x100))
  }

  test("Example from docs") {
    EmuBenchmarkRun(
      """
        | byte output @$c000
        | void main () {
        |  output = ten()
        | }
        | const byte fiveConstant = 5
        | byte fiveVariable = 5
        |
        | byte ten() {
        |    byte result
        |    asm {
        |        LDA #fiveConstant
        |        CLC
        |        ADC fiveVariable
        |        STA result
        |    }
        |    return result
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(10))
  }
}
