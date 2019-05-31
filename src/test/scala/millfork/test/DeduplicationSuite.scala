package millfork.test

import millfork.Cpu
import millfork.test.emu._
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class DeduplicationSuite extends FunSuite with Matchers {

  test("Code deduplication") {
    EmuOptimizedCmosRun(
      """
        |
        | void main() {
        |   times2(1)
        |   shift_left(2)
        |   nothing(2)
        | }
        | noinline byte shift_left(byte x) {
        |   return x << 1
        | }
        | noinline byte times2(byte x) {
        |   x *= 2
        |   return x
        | }
        | noinline void nothing(byte x) {
        | }
      """.stripMargin)
  }

  test("Subroutine extraction") {
    EmuSizeOptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Intel8086)(
      """
        | int24 output @$c000
        | void main() {
        |   output.b0 = f(1)
        |   output.b1 = g(2)
        |   output.b2 = h(2)
        | }
        | noinline byte f(byte x) {
        |   x += 2
        |   x |= 4
        |   x <<= 1
        |   x &= 7
        |   x -= 6
        |   return x
        | }
        | noinline byte g(byte x) {
        |   x += 3
        |   x |= 4
        |   x <<= 1
        |   x &= 7
        |   x -= 7
        |   return x
        | }
        | noinline byte h(byte x) {
        |   x += 5
        |   x |= 4
        |   x <<= 1
        |   x &= 7
        |   x -= 5
        |   return x
        | }
        | noinline void nothing(byte x) {
        | }
      """.stripMargin) {m =>
      m.readMedium(0xc000) should equal(0x1FB00)
    }
  }

  test("Loop subroutine extraction") {
    EmuSizeOptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Intel8086)(
      """
        | array output [8] @$c000
        | void main() {
        |   f(2)
        |   g(3)
        |   h(6)
        | }
        | noinline void f(byte x) {
        |   byte i
        |   for i,0,until,output.length {
        |     output[i] = x
        |   }
        | }
        | noinline void g(byte x) {
        |   byte i
        |   for i,0,until,output.length {
        |     output[i] = x
        |   }
        | }
        | noinline void h(byte x) {
        |   byte i
        |   for i,0,until,output.length {
        |     output[i] = x
        |   }
        | }
      """.stripMargin) {m =>
      m.readByte(0xc000) should equal(6)
    }
  }
}
