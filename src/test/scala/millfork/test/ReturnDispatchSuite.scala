package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuCrossPlatformBenchmarkRun, EmuUnoptimizedIntel8080Run, ShouldNotCompile}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class ReturnDispatchSuite extends FunSuite with Matchers {

  test("Trivial test") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Cmos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)(
      """
        | byte output @$c000
        | void main () {
        |   byte i
        |   i = 1
        |   return [i] {
        |     1 @ success
        |   }
        | }
        | void success() {
        |   output = 42
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(42)
    }
  }
  test("Parameter test") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Cmos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)(
      """
        | array output [200] @$c000
        | sbyte param
        | byte ptr
        | const byte L = 4
        | const byte R = 5
        | const byte W1 = 6
        | const byte W2 = 7
        | void main () {
        |   ptr = 0
        |   handler(W1)
        |   handler(R)
        |   handler(W2)
        |   handler(R)
        |   handler(W1)
        |   handler(L)
        |   handler(L)
        |   handler(10)
        | }
        | void handler(byte i) {
        |   return [i](param) {
        |     L @ move($ff)
        |     R @ move(1)
        |     W1 @ write(1)
        |     W2 @ write(2)
        |     default(0,10) @ zero
        |   }
        | }
        | void move() {
        |   ptr += param
        | }
        | void write() {
        |   output[ptr] = param
        | }
        | void zero() {
        |   output[ptr] = 42
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(42)
      m.readByte(0xc001) should equal(2)
      m.readByte(0xc002) should equal(1)
    }
  }

  test("Enum test") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Cmos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)(
      """
        | byte output @$c000
        | enum ugly {
        |   a
        |   b,c,
        |   d
        | }
        | void main () {
        |   ugly i
        |   i = a
        |   return [i] {
        |     a @ success
        |   }
        | }
        | void success() {
        |   output = 42
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(42)
    }
  }

  test("Mixed types shouldn't compile 1") {
    ShouldNotCompile(
      """
        | enum ugly {a,b,c,d}
        | void main () {
        |   ugly i
        |   return [i] {
        |     1 @ success
        |   }
        | }
        | void success() {}
      """.stripMargin)
  }

  test("Mixed types shouldn't compile 2") {
    ShouldNotCompile(
      """
        | enum ugly {a,b,c,d}
        | void main () {
        |   byte i
        |   return [i] {
        |     a @ success
        |   }
        | }
        | void success() {}
      """.stripMargin)
  }

  test("Non-empty enums can't have defined default ranges") {
    ShouldNotCompile(
      """
        | enum ugly {a,b,c,d}
        | void main () {
        |   ugly i
        |   return [i] {
        |     a @ success
        |     default(a,d) @ success
        |   }
        | }
        | void success() {}
      """.stripMargin)
  }

  test("Empty enums can have defined default ranges") {
    EmuUnoptimizedIntel8080Run(
      """
        | enum ugly {}
        | void main () {
        |   ugly i
        |   return [i] {
        |     default(ugly(0), ugly(10)) @ success
        |   }
        | }
        | void success() {}
      """.stripMargin)
  }

  test("Non-empty enums can have implied default ranges") {
    EmuUnoptimizedIntel8080Run(
      """
        | enum ugly {a,b,c}
        | void main () {
        |   ugly i
        |   return [i] {
        |     default @ success
        |   }
        | }
        | void success() {}
      """.stripMargin)
  }
}
