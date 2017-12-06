package millfork.test

import millfork.test.emu.EmuBenchmarkRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class ErasthotenesSuite extends FunSuite with Matchers {

  test("Erasthotenes") {
    EmuBenchmarkRun(
      """
        | const pointer sieve = $C000
        | const byte sqrt = 128
        | void main () {
        |   byte i
        |   word j
        |   pointer s
        |   i = 2
        |   while i < sqrt {
        |     if sieve[i] == 0 {
        |       j = i << 1
        |       s = sieve
        |       s += j
        |       while j.hi == 0 {
        |         s[0] = 1
        |         s += i
        |         j += i
        |       }
        |     }
        |     i += 1
        |   }
        | }
      """.stripMargin){_=>}
  }
}
