package millfork.test
import millfork.test.emu.{EmuBenchmarkRun, EmuUltraBenchmarkRun, EmuUnoptimizedRun}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class ShiftSuite extends FunSuite with Matchers {

  test("In-place shifting") {
    EmuUnoptimizedRun("""
        | array output [3] @$c000
        | void main () {
        |   output[0] = 1
        |   output[1] = 3
        |   output[output[0]] <<= 2
        | }
      """.stripMargin).readByte(0xc001) should equal(12)
  }

  test("Byte shifting") {
    EmuBenchmarkRun("""
        | byte output @$c000
        | void main () {
        |   byte a
        |   a = 3
        |   output = a << 2
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(12))
  }

  test("Word shifting") {
    EmuBenchmarkRun("""
        | word output @$c000
        | void main () {
        |   byte a
        |   a = 3
        |   output = a
        |   output <<= 7
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(128 * 3))
  }

  test("Long shifting left") {
    EmuUltraBenchmarkRun("""
        | long output @$c000
        | void main () {
        |   output = $1010301
        |   output <<= 2
        | }
      """.stripMargin)(_.readLong(0xc000) should equal(0x4040C04))
  }

  test("Long shifting right") {
    EmuBenchmarkRun("""
        | long output @$c000
        | void main () {
        |   output = $4040C04
        |   output >>= 2
        | }
      """.stripMargin)(_.readLong(0xc000) should equal(0x1010301))
  }

  test("Word shifting via pseudoregister") {
    EmuBenchmarkRun("""
        | word output @$c000
        | void main () {
        |   output = identity(three() << 7)
        | }
        | word three() { return 3 }
        | word identity(word w) { return w }
      """.stripMargin)(_.readWord(0xc000) should equal(0x180))
  }

  test("Variable shifting") {
    EmuBenchmarkRun("""
        | word output0 @$c000
        | word output2 @$c002
        | byte output4 @$c004
        | byte output5 @$c005
        | void main () {
        |   byte a
        |   a = b(3)
        |   output0 = $0001 << a
        |   output2 = $0001 << b(3)
        |   output4 = 1 << a
        |   output5 = 1 << b(3)
        | }
        | noinline byte b(byte x) { return x }
      """.stripMargin){m =>
      m.readWord(0xc000) should equal(8)
      m.readWord(0xc002) should equal(8)
      m.readByte(0xc004) should equal(8)
      m.readByte(0xc005) should equal(8)
    }
  }
}
