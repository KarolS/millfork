package millfork.test
import millfork.test.emu.EmuBenchmarkRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class WordMathSuite extends FunSuite with Matchers {

  test("Word addition") {
    EmuBenchmarkRun("""
        | word output @$c000
        | word a
        | void main () {
        |  a = 640
        |  output = a
        |  output += a
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(1280))
  }

  test("Word subtraction") {
    EmuBenchmarkRun("""
        | word output @$c000
        | word a
        | void main () {
        |  a = 640
        |  output = 740
        |  output -= a
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(100))
  }

  test("Word subtraction 2") {
    EmuBenchmarkRun("""
        | word output @$c000
        | word a
        | void main () {
        |  a = 640
        |  output = a
        |  output -= 400
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(240))
  }

  test("Byte-to-word addition") {
    EmuBenchmarkRun("""
        | word output @$c000
        | word pair
        | void main () {
        |  pair = $A5A5
        |  pair.lo = 1
        |  output = 640
        |  output += pair.lo
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(641))
  }

  test("Literal addition") {
    EmuBenchmarkRun("""
        | word output @$c000
        | void main () {
        |  output = 640
        |  output += -0050
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(590))
  }

  test("Array element addition") {
    EmuBenchmarkRun("""
        | word output @$c000
        | word pair
        | array b[2]
        | void main () {
        |  byte i
        |  i = 1
        |  b[1] = 5
        |  pair = $A5A5
        |  pair.lo = 1
        |  output = 640
        |  output += b[i]
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(645))
  }

  test("nesdev.com example") {
    EmuBenchmarkRun("""
        | byte output @$c000
        | array map [256] @$c300
        | array b[2]
        | void main () {
        |   output = get(5, 6)
        | }
        | byte get(byte mx, byte my) {
        |   pointer p
        |   p = mx
        |   p <<= 5
        |   p += map
        |   return p[my]
        | }
      """.stripMargin)(m => ())
  }
}
