package millfork.test
import millfork.Cpu
import millfork.test.emu.{EmuBenchmarkRun, EmuCrossPlatformBenchmarkRun, EmuOptimizedCmosRun, EmuOptimizedHudsonRun, EmuOptimizedRun, EmuUndocumentedRun, EmuUnoptimizedCrossPlatformRun, EmuUnoptimizedHudsonRun, EmuUnoptimizedM6809Run, EmuUnoptimizedRun, EmuUnoptimizedZ80Run, ShouldNotCompile}
import org.scalatest.{AppendedClues, FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class AssemblySuite extends FunSuite with Matchers with AppendedClues {

  test("Inline assembly") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Motorola6809)(
      """
        | byte output @$c000
        | void main () {
        |  output = 0
        |  asm {
        |    inc $c000 ; this is an assembly-style comment
        |  }
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(1))
  }

  test("Self-modifying assembly") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Motorola6809)(
      """
        | byte output @$c000
        | // w&x
        | asm void main () {
        |   lda #3
        |   sta .l+1
        |   .l: lda #55
        |   sta output
        |   rts
        |   ignored:}
      """.stripMargin)(_.readByte(0xc000) should equal(3))
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
        |  ignored:
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

  test("Constants") {
    EmuBenchmarkRun(
      """
        | const word COUNT = $400
        | array a[COUNT]@$c000
        | asm void main () {
        |   LDA #hi(a.addr+COUNT)
        |   RTS
        | }
      """.stripMargin){m =>

    }
  }

  test("Undocumented opcodes") {
    EmuUndocumentedRun(
      """
        | asm void main() {
        |     rts
        |     kil
        |     slo $4
        |     slo $400
        |     slo $4,x
        |     slo $400,x
        |     slo $400,y
        |     slo ($4,x)
        |     slo ($4),y
        |     rla $4
        |     rla $400
        |     rla $4,x
        |     rla $400,x
        |     rla $400,y
        |     rla ($4,x)
        |     rla ($4),y
        |     rra $4
        |     rra $400
        |     rra $4,x
        |     rra $400,x
        |     rra $400,y
        |     rra ($4,x)
        |     rra ($4),y
        |     sre $4
        |     sre $400
        |     sre $4,x
        |     sre $400,x
        |     sre $400,y
        |     sre ($4,x)
        |     sre ($4),y
        |     dcp $4
        |     dcp $400
        |     dcp $4,x
        |     dcp $400,x
        |     dcp $400,y
        |     dcp ($4,x)
        |     dcp ($4),y
        |     isc $4
        |     isc $400
        |     isc $4,x
        |     isc $400,x
        |     isc $400,y
        |     isc ($4,x)
        |     isc ($4),y
        |     lax $4
        |     lax $4,y
        |     lax $400
        |     lax $400,y
        |     lax ($4,x)
        |     lax ($4),y
        |     sax $4
        |     sax $4,y
        |     sax $400
        |     sax ($4,x)
        |     anc #$4
        |     alr #$4
        |     arr #$4
        |     xaa #$4
        |     lxa #$4
        |     sbx #$4
        |     ahx ($4),y
        |     ahx $400,y
        |     shy $400,x
        |     shx $400,y
        |     tas $400,y
        |     las $400,y
        |     bne $300
        |     brk #4
        |     bne #4
        |     rts
        | }
        |
        |""".stripMargin
    )
  }

  test("HuC6280 opcodes") {
    EmuOptimizedHudsonRun(
      """
        | asm void main() {
        |     rts
        |     tam #1
        |     tma #2
        |     bbr0 $33,main
        |     bbs0 $33,main
        |     clx
        |     cly
        |     csh
        |     csl
        |     rmb0 $80
        |     smb0 $80
        |     sax
        |     say
        |     set
        |     st0 #1
        |     st1 #1
        |     st2 #1
        |     stp
        |     sxy
        |     tam #3
        |     tma #5
        |     trb $800
        |     trb $4
        |     tsb $800
        |     tsb $4
        |     tai $4000,$5000,$300
        |     tia $4000,$5000,$300
        |     tii $4000,$5000,$300
        |     tin $4000,$5000,$300
        |     tdd $4000,$5000,$300
        |     tst #$44,4
        |     tst #$44,4,X
        |     tst #$44,3334
        |     tst #$44,3334,X
        |     rts
        | }
        |
        |""".stripMargin)
  }

  test("Short branch too large") {
    ShouldNotCompile(
      """
        |asm void main() {
        |bne __main_exit
        |[for i,0,until,200 [$EA]]
        |__main_exit:
        |rts
        |}
        |""".stripMargin, Set(Cpu.Mos))
  }

  test("Compile params properly") {
    val m = EmuUnoptimizedRun(
      """
        |noinline asm void f(byte register(a) v0, byte register(x) v1) {
        | sta $c000
        | stx $c001
        | rts
        |}
        |
        |void main() {
        | f(9,3)
        |}
        |""".stripMargin)
    m.readByte(0xc000) should equal(9)
    m.readByte(0xc001) should equal(3)
  }


  test("Compile local labels properly (6502)") {
    val m = EmuUnoptimizedRun(
      """
        |byte output1 @$c001
        |byte output2 @$c002
        |noinline asm byte one() {
        |  jmp .l
        |  .l:
        |  lda #1
        |  rts
        |}
        |noinline asm byte two() {
        |  jmp .l
        |  .l:
        |  lda #2
        |  rts
        |}
        |
        |void main() {
        |   output1 = one()
        |   output2 = two()
        |}
        |""".stripMargin)
    m.readByte(0xc001) should equal(1)
    m.readByte(0xc002) should equal(2)
  }

  test("Compile local labels properly (Z80)") {
    val m = EmuUnoptimizedZ80Run(
      """
        |byte output1 @$c001
        |byte output2 @$c002
        |noinline asm byte one() {
        |  jp .l
        |  .l:
        |  ld a,1
        |  ret
        |}
        |noinline asm byte two() {
        |  jp .l
        |  .l:
        |  ld a,2
        |  ret
        |}
        |
        |void main() {
        |   output1 = one()
        |   output2 = two()
        |}
        |""".stripMargin)
    m.readByte(0xc001) should equal(1)
    m.readByte(0xc002) should equal(2)
  }

  test("Compile local labels properly (6809)") {
    val m = EmuUnoptimizedM6809Run(
      """
        |byte output1 @$c001
        |byte output2 @$c002
        |noinline asm byte one() {
        |  jmp .l
        |  .l:
        |  ldb #1
        |  rts
        |}
        |noinline asm byte two() {
        |  jmp .l
        |  .l:
        |  ldb #2
        |  rts
        |}
        |
        |void main() {
        |   output1 = one()
        |   output2 = two()
        |}
        |""".stripMargin)
    m.readByte(0xc001) should equal(1)
    m.readByte(0xc002) should equal(2)
  }

  test("Interrupt functions in assembly should not have prologue (6502)") {
    val m = EmuUnoptimizedRun(
      """
        |byte output @$c000
        |noinline interrupt asm void i() {
        |  nop
        |  rti
        |}
        |void main() {
        |   output = pointer(i.addr)[0]
        |}
        |""".stripMargin)
    m.readByte(0xc000) should equal(0xea)
  }

  test("Interrupt functions in assembly should not have prologue (Z80)") {
    val m = EmuUnoptimizedZ80Run(
      """
        |byte output @$c000
        |noinline interrupt asm void i() {
        |  nop
        |  reti
        |}
        |void main() {
        |   output = pointer(i.addr)[0]
        |}
        |""".stripMargin)
    m.readByte(0xc000) should equal(0)
  }

  test("Interrupt functions in assembly should not have prologue (M6809)") {
    val m = EmuUnoptimizedM6809Run(
      """
        |byte output @$c000
        |noinline interrupt asm void i() {
        |  nop
        |  rti
        |}
        |void main() {
        |   output = pointer(i.addr)[0]
        |}
        |""".stripMargin)
    m.readByte(0xc000) should equal(0x12)
  }
}
