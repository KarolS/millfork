package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuCrossPlatformBenchmarkRun, EmuSizeOptimizedCrossPlatformRun, EmuUnoptimizedCrossPlatformRun, ShouldNotCompile}
import org.scalatest.{AppendedClues, FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class FunctionPointerSuite extends FunSuite with Matchers with AppendedClues{

  test("Function pointers 1") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Cmos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Motorola6809)(
      """
        |
        | byte output @$c000
        | void f1() {
        |   output = 100
        | }
        |
        | void main() {
        |   function.void.to.void p1
        |   p1 = f1.pointer
        |   call(p1)
        | }
        |
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(100)
    }
  }

  test("Function pointers 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Cmos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Motorola6809)(
      """
        | const byte COUNT = 128
        | array output0[COUNT] @$c000
        | array output1[COUNT] @$c100
        | array output2[COUNT] @$c200
        | array output3[COUNT] @$c300
        |
        | void tabulate(pointer target, function.byte.to.byte f) {
        |   byte i
        |   for i,0,until,COUNT {
        |     target[i] = call(f, i)
        |   }
        | }
        |
        | byte double(byte x) = x*2
        | byte negate(byte x) = 0-x
        | byte zero(byte x)   = 0
        | byte id(byte x)     = x
        |
        | void main() {
        |   tabulate(output0, zero)
        |   tabulate(output1, id)
        |   tabulate(output2, double)
        |   tabulate(output3, negate)
        | }
        |
      """.stripMargin) { m =>
      for (i <- 0 until 0x80) {
        m.readByte(0xc000 + i) should equal(0) withClue ("zero " + i)
        m.readByte(0xc100 + i) should equal(i) withClue ("id " + i)
        m.readByte(0xc200 + i) should equal(i * 2) withClue ("double " + i)
        m.readByte(0xc300 + i) should equal((256 - i) & 0xff) withClue ("negate " + i)
      }
    }
  }

  test("Function pointers: invalid types") {
    ShouldNotCompile(
      """
        |void main() {
        | call(main.pointer, 1)
        |}
        |""".stripMargin)
    ShouldNotCompile(
      """
        |void f(byte a) = 0
        |void main() {
        | call(f.pointer)
        |}
        |""".stripMargin)
    ShouldNotCompile(
      """
        |enum e {}
        |void f(e a) = 0
        |void main() {
        | call(f.pointer, 0)
        |}
        |""".stripMargin)
    ShouldNotCompile(
      """
        |enum e {}
        |void f(byte a) = 0
        |void main() {
        | call(f.pointer, e(7))
        |}
        |""".stripMargin)
  }

  test("Function pointers 3") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        | const byte COUNT = 128
        | array(word) output0[COUNT] @$c000
        | noinline void fill(function.word.to.word f) {
        |   byte i
        |   for i,0,until,COUNT {
        |     output0[i] = call(f, word(i))
        |   }
        | }
        | word id(word x) = x
        | void main() {
        |   fill(id.pointer)
        | }
        |
      """.stripMargin) { m =>
      for (i <- 0 until 0x80) {
        m.readWord(0xc000 + i * 2) should equal(i) withClue ("id " + i)
      }
    }
  }

  test("Function pointers 4") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        | const byte COUNT = 128
        | array(byte) output0[COUNT] @$c000
        | noinline void fill(function.byte.to.byte f) {
        |   byte i
        |   for i,0,until,COUNT {
        |     call(f, i)
        |   }
        | }
        | byte iota_at(byte x) {
        |   output0[x] = x
        |   return x
        | }
        | void main() {
        |   fill(iota_at.pointer)
        | }
        |
      """.stripMargin) { m =>
      for (i <- 0 until 0x80) {
        m.readByte(0xc000 + i) should equal(i) withClue ("id " + i)
      }
    }
  }

  test("Interrupt pointers") {
    EmuUnoptimizedCrossPlatformRun (Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        | pointer.interrupt i @$c000
        | pointer.kernal_interrupt k @$c002
        | function.void.to.void f @$c004
        | interrupt void i1() @$400 {}
        | kernal_interrupt void k1() @$500 {}
        | void main() {
        |   i = i1.pointer
        |   k = k1.pointer
        |   f = k1.pointer
        |   call(k)
        | }
        |
      """.stripMargin) { m =>
      m.readWord(0xc000) should equal(0x400)
      m.readWord(0xc002) should equal(0x500)
      m.readWord(0xc004) should equal(0x500)
    }
  }

}
