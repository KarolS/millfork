package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuUnoptimizedCrossPlatformRun, EmuUnoptimizedRun}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class BasicSymonTest extends FunSuite with Matchers {
  test("Empty test") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | void main () {
        |
        | }
      """.stripMargin) {
      m => ()
    }
  }

  test("Panic test") {
    EmuUnoptimizedRun(
      """
        | byte output @$c000
        | void main () {
        |   panic()
        | }
        | macro asm void panic() {
        |    JSR _panic
        | }
        | void _panic() {
        |   asm {
        |     JSR doNothing
        |   }
        |   output = 1
        | }
        | void doNothing() { }
      """.stripMargin).readByte(0xc000) should equal(1)
  }

  test("Allocation test") {
    val src =
      """
 byte output @$c000
 void main () {
   function()
 }
 array thing @$20F = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
 void function() {
   output = 0
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
   output += 1
 }
      """
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(src){m =>
      m.readByte(0xc000) should equal(src.count(_ == '+'))
    }
  }

  test("Byte assignment") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | byte output @$c000
        | void main () {
        |  output = (1)
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(1)
    }
  }

  test("Preallocated variables") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | array output [2] @$c000
        | byte number = 4
        | void main () {
        |  output[0] = number
        |  number += 1
        |  output[1] = number
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(4)
      m.readByte(0xc001) should equal(5)
    }
  }

  test("Preallocated variables 2") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | word output @$c000
        | word number = 344
        | void main () {
        |  output = number
        | }
      """.stripMargin) { m =>
      m.readWord(0xc000) should equal(344)
    }
  }

  test("Else if") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | byte output @$c000
        | void main () {
        |  if 1 == 2 {
        |    output = 3
        |  } else if 1 == 1 {
        |    output = 4
        |  } else {
        |    output = 65
        |  }
        | }
      """.stripMargin) { m =>
      m.readWord(0xc000) should equal(4)
    }
  }

  test("Segment syntax") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | segment(default)byte output @$c000
        | segment(default)array x[3]
        | segment(default)void main () {
        | }
      """.stripMargin){ m => () }
  }

  test("Alias test") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | alias small = byte
        | alias big = word
        | byte crash @$bfff
        | void main () {
        |   big w
        |   small b
        |   b = 1
        |   w = 1
        |   b <<= 8
        |   w <<= 8
        |   if b != 0 { crash = 1 }
        |   if w == 0 { crash = 2 }
        | }
      """.stripMargin){ m => () }
  }

  test("Preprocessor test") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | byte output @$c000
        |
        | #use ARCH_6502
        | #use ARCH_I80
        |
        | #if 1
        | asm void main () {
        |   #if ARCH_6502
        |     lda #ARCH_6502
        |     sta output
        |     rts
        |   #elseif ARCH_I80
        |     ld a,ARCH_I80
        |     ld (output),a
        |     ret
        |   #else
        |     #error unsupported architecture
        |   #endif
        | }
        | #endif
        |
        | #if 1 + 1 == 2
        |   #info 1
        |   #if      1 == 3
        |     #error 1 == 3
        |   #elseif  1 == 5
        |     #error 1 == 5
        |   #else
        |     #info 2
        |   #endif
        | #else
        |   #error not( 1 + 1 == 2 )
        | #endif
      """.stripMargin){ m =>
      m.readByte(0xc000) should equal(1)
    }
  }
}
