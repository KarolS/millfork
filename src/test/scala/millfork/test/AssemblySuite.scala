package millfork.test
import millfork.test.emu.{EmuBenchmarkRun, EmuOptimizedCmosRun, EmuOptimizedRun}
import org.scalatest.{AppendedClues, FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class AssemblySuite extends FunSuite with Matchers with AppendedClues {

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

  test("Macro asm functions") {
    EmuBenchmarkRun(
      """
        | byte output @$c000
        | void main () {
        |  output = 0
        |  f()
        |  f()
        | }
        | macro asm void f() {
        |   inc $c000
        |   rts
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(1))
  }

  test("macro asm functions 2") {
    EmuBenchmarkRun(
      """
        | byte output @$c000
        | void main () {
        |  output = 0
        |  add(output, 5)
        |  add(output, 5)
        | }
        | macro asm void add(byte ref v, byte const c) {
        |   lda v
        |   clc
        |   adc #c
        |   sta v
        |   rts
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(5))
  }

  test("Addresses in asm") {
    EmuBenchmarkRun(
      """
        | word output @$c000
        | void main () {
        |  output = 0
        |  add256(output)
        | }
        | macro asm void add256(word ref v) {
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

  test("JSR") {
    EmuBenchmarkRun(
      """
        | byte output @$c000
        | asm void main () {
        |  JSR thing
        |  RTS
        | }
        |
        | void thing() {
        |    output = 10
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(10))
  }

  test("Inline raw bytes") {
    EmuBenchmarkRun(
      """
        | byte output @$c000
        | asm void main () {
        |   ? LDA #10
        |   [ for x,0,until,8 [$EA] ]
        |   [ $8d, 0, $c0]
        |   JMP cc
        |   "stuff" ascii
        |   cc:
        |   RTS
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(10))
  }

  test("Correctly use zeropage for CPU port on C64") {
    val m = EmuOptimizedRun(
      """
        | byte port @1
        | const byte port_addr = 1
        | byte port_alt @port_addr
        | void main () {
        |   a()
        |   b()
        |   c()
        |   d()
        |   e()
        |   f()
        | }
        | void a() {
        |   port = 1
        | }
        | void b() {
        |   port_alt = 2
        | }
        | asm void c() {
        |   lda #3
        |   sta 1
        |   rts
        | }
        | asm void d() {
        |   lda #4
        |   sta port
        |   rts
        | }
        | asm void e() {
        |   lda #5
        |   sta port_addr
        |   rts
        | }
        | asm void f() {
        |   lda #6
        |   sta port_alt
        |   rts
        | }
      """.stripMargin)
    for (addr <- 0x0200 to 0x02ff) {
      m.readable(addr) = true
      m.readByte(addr) should not equal 0x8d withClue f"STA abs at $addr%04x"
    }
  }

}
