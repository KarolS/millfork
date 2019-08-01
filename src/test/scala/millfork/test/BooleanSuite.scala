package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuCrossPlatformBenchmarkRun, EmuUnoptimizedCrossPlatformRun, EmuUnoptimizedRun}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class BooleanSuite extends FunSuite with Matchers {

  test("Not") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)(
      """
        | byte output @$c000
        | array input = [5,6,7]
        | void main () {
        |   byte a
        |   a = 5
        |   if not(a < 3) { output  = $84 }
        |   if not(a > 3) { output  = $03 }
        |   if not(true)  { output  = $05 }
        |   if not(false) { output &= $7f }
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(4))

  }


  test("And") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)(
      """
        | byte output @$c000
        | array input = [5,6,7]
        | void main () {
        |   byte a
        |   byte b
        |   a = 5
        |   b = 5
        |   if a > 3 && b > 3 {output = 4}
        |   if a < 3 && b > 3 {output = 5}
        |   if a > 3 && b < 3 {output = 2}
        |   if a < 3 && b < 3 {output = 3}
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(4))
  }


  test("Or") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)(
      """
        | byte output @$c000
        | array input = [5,6,7]
        | void main () {
        |   byte a
        |   byte b
        |   a = 5
        |   b = 5
        |   output = 0
        |   if a > 3 || b > 3 {output += 4}
        |   if a < 3 || b > 3 {output += 5}
        |   if a > 3 || b < 3 {output += 2}
        |   if a < 3 || b < 3 {output = 30}
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(11))
  }

  test("Constant conditions big suite") {
    val code ="""
      | byte output @$c000
      | const pointer outside = $bfff
      | void main () {
      |   output = 1
      |   if 1 == 1 { pass() }
      |   if 1 == 2 { fail() }
      |   if 1 != 1 { fail() }
      |   if 1 != 2 { pass() }
      |   if 1 < 2 { pass() }
      |   if 1 < 1 { fail() }
      |   if sbyte(1) < sbyte(255) { fail() }
      |   if sbyte(1) > sbyte(255) { pass() }
      |   if sbyte(1) > 0 { pass() }
      |   if sbyte(-1) > 0 { fail() }
      |   if 00001 < 00002 { pass() }
      |   if 00001 > 00002 { fail() }
      |   if 00001 != 00002 { pass() }
      |   if 00001 == 00002 { fail() }
      | }
      | inline void pass() { output += 1 }
      | noinline void fail() { outside[0] = 0 }
      """.stripMargin
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)(code)(_.readByte(0xc000) should equal(code.sliding(4).count(_ == "pass")))
  }

  test("Fat boolean") {
    val code ="""
      | byte output @$c000
      | const pointer outside = $bfff
      | void main () {
      |   output = 1
      |   bool x
      |   x = true
      |   memory_barrier()
      |   if x { pass() }
      |   memory_barrier()
      |   x = false
      |   memory_barrier()
      |   if x { fail(1) }
      |
      |   if isZero(0) { pass() }
      |   if isZero(1) { fail(2) }
      |   if isLarge(10) { pass() }
      |   if isLarge(1) { fail(3) }
      |
      |   x = id(2) == 2
      |   if x { pass() }
      |   x = id(2) != 2
      |   if x { fail(4) }
      |
      |   if always() { pass() }
      |   if never() { fail(5) }
      |
      |   x = always()
      |   x = not(x)
      |   if x { fail(6) }
      |   if not(x) { pass() }
      |   if x && true { fail(7) }
      |   if x || true { pass() }
      | }
      | inline void pass() { output += 1 }
      | noinline void fail(byte i) { outside[0] = i }
      | noinline byte id(byte x) = x
      | bool isZero(byte x) = x == 0
      | bool isLarge(byte x) = x >= 10
      | bool always() = true
      | bool never() = false
      |
      """.stripMargin
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80)(code)(_.readByte(0xc000) should equal(code.sliding(4).count(_ == "pass")))
  }

  test("Fat boolean optimization") {
    val code ="""
      | byte output @$c000
      | noinline bool f(byte x) = x & 1 != 0
      | noinline bool g(byte x) = x & 1 == 0
      | void main () {
      |   output = 5
      |   if f(3) { output += 1 }
      |   if g(2) { output += 1 }
      |   if f(2) { output += 100 }
      |   if g(3) { output += 100 }
      | }
      |
      """.stripMargin
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80)(code)(_.readByte(0xc000) should equal(7))
  }

  test("Constant booleans") {
    val code ="""
      | word output @$c000
      | noinline bool f(byte x) = x & 1 != 0
      | noinline bool g(byte x) = x & 1 == 0
      | void main () {
      |   output = 5
      |   if f(3) && true { output += 1 }
      |   if f(3) && false { output += 100 }
      |   if g(2) && true { output += 1 }
      |   if g(2) && false { output += 100 }
      |   if f(2) && true { output += 100 }
      |   if f(2) && false { output += 100 }
      |   if g(3) && true { output += 100 }
      |   if g(3) && false { output += 100 }
      |
      |   if f(3) || true { output += 1 }
      |   if f(3) || false { output += 1 }
      |   if g(2) || true { output += 1 }
      |   if g(2) || false { output += 1 }
      |   if f(2) || true { output += 1 }
      |   if f(2) || false { output += 100 }
      |   if g(3) || true { output += 1 }
      |   if g(3) || false { output += 100 }
      | }
      |
      """.stripMargin
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80)(code)(_.readWord(0xc000) should equal(13))
  }

  test("Constant booleans mini") {
    val code ="""
      | byte output @$c000
      | noinline byte f(byte x) = x
      | void main () {
      |   output = 5
      |   if f(3) != 0 && false { output += 100 }
      | }
      |
      """.stripMargin
    EmuUnoptimizedRun(code).readWord(0xc000) should equal(5)
  }
}
