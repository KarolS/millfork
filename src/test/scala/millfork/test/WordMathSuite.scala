package millfork.test
import millfork.test.emu.{EmuBenchmarkRun, EmuCmosBenchmarkRun}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class WordMathSuite extends FunSuite with Matchers {

  test("Word addition") {
    EmuCmosBenchmarkRun("""
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
    EmuCmosBenchmarkRun("""
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
    EmuCmosBenchmarkRun("""
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
    EmuCmosBenchmarkRun("""
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
    EmuCmosBenchmarkRun("""
        | word output @$c000
        | void main () {
        |  output = 640
        |  output += -0050
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(590))
  }

  test("Array element addition") {
    EmuCmosBenchmarkRun("""
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
    EmuCmosBenchmarkRun("""
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

  test("hi()/lo()") {
    EmuCmosBenchmarkRun("""
        | array output [7] @$c000
        | void main () {
        |   output[0] = lo(33)
        |   output[1] = hi(33)
        |   output[2] = hi(w($504))
        |   output[3] = lo(w($209))
        |   output[4] = hi(s(-2))
        |   output[5] = lo(s(-2))
        |   output[6] = hi(b(200) + b(200))
        | }
        | word w(word w) {
        |   return w
        | }
        | byte b(byte b) {
        |   return b
        | }
        | sbyte s(sbyte s) {
        |   return s
        | }
      """.stripMargin){ m =>
      m.readByte(0xc000) should equal(33)
      m.readByte(0xc001) should equal(0)
      m.readByte(0xc002) should equal(5)
      m.readByte(0xc003) should equal(9)
      m.readByte(0xc004) should equal(255)
      m.readByte(0xc005) should equal(254)
      m.readByte(0xc006) should equal(0)
    }
  }
}
