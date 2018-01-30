package millfork.test

import millfork.test.emu.{EmuBenchmarkRun, EmuCmosBenchmarkRun, EmuUnoptimizedRun}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class ReturnDispatchSuite extends FunSuite with Matchers {

  test("Trivial test") {
    EmuCmosBenchmarkRun(
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
    EmuCmosBenchmarkRun(
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
}
