package millfork.test

import millfork.test.emu.EmuBenchmarkRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class ComparisonSuite extends FunSuite with Matchers {

  test("Equality and inequality") {
    EmuBenchmarkRun(
      """
        | byte output @$c000
        | void main () {
        |  output = 5
        |  if (output == 5) {
        |   output += 1
        |  } else {
        |   output +=2
        |  }
        |  if (output != 6) {
        |   output += 78
        |  }
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(6))
  }

  test("Less") {
    EmuBenchmarkRun(
      """
        | byte output @$c000
        | void main () {
        |  output = 5
        |  while output < 150 {
        |    output += 1
        |  }
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(150))
  }

  test("Compare to zero") {
    EmuBenchmarkRun(
      """
        | byte output @$c000
        | void main () {
        |  byte a
        |  a = 150
        |  while a != 0 {
        |    a -= 1
        |    output += 1
        |  }
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(150))
  }

  test("Carry flag optimization test") {
    EmuBenchmarkRun(
      """
        | byte output @$c000
        | void main () {
        |  byte a
        |  a = 150
        |  if (a >= 50) {
        |    output = 4
        |  } else {
        |    output = 0
        |  }
        |  output += get(55)
        | }
        | byte get(byte x) {
        |   if x >= 6 {return 0} else {return 128}
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(4))
  }

  test("Does it even work") {
    EmuBenchmarkRun(
      """
        | word output @$c000
        | void main () {
        |  byte a
        |  a = 150
        |  if a != 0 {
        |    output = 345
        |  }
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(345))
  }

  test("Word comparison constant") {
    val src =
      """
        | byte output @$c000
        | void main () {
        |  output = 0
        |  if 2222 == 2222 { output += 1 }
        |  if 2222 == 3333 { output -= 1 }
        | }
      """.stripMargin
    EmuBenchmarkRun(src)(_.readWord(0xc000) should equal(src.count(_ == '+')))
  }

  test("Word comparison == and !=") {
    val src =
      """
        | byte output @$c000
        | void main () {
        |  word a
        |  word b
        |  word c
        |  output = 0
        |  a = 4
        |  b = 4
        |  c = 5
        |  if a == 4 { output += 1 }
        |  if a == b { output += 1 }
        |  if a != c { output += 1 }
        |  if a != 5 { output += 1 }
        |  if a != 260 { output += 1 }
        | }
      """.stripMargin
    EmuBenchmarkRun(src)(_.readWord(0xc000) should equal(src.count(_ == '+')))
  }

  test("Word comparison <=") {
    val src =
      """
        | byte output @$c000
        | void main () {
        |  word a
        |  word b
        |  word c
        |  output = 0
        |  a = 4
        |  b = 4
        |  c = 5
        |  if a <= 4 { output += 1 }
        |  if a <= 257 { output += 1 }
        |  if a <= b { output += 1 }
        |  if a <= c { output += 1 }
        | }
      """.stripMargin
    EmuBenchmarkRun(src)(_.readWord(0xc000) should equal(src.count(_ == '+')))
  }
  test("Word comparison <") {
    val src =
      """
        | byte output @$c000
        | void main () {
        |  word a
        |  word b
        |  word c
        |  output = 0
        |  a = 4
        |  b = 4
        |  c = 5
        |  if a < 5 { output += 1 }
        |  if a < c { output += 1 }
        |  if a < 257 { output += 1 }
        | }
      """.stripMargin
    EmuBenchmarkRun(src)(_.readWord(0xc000) should equal(src.count(_ == '+')))
  }


  test("Word comparison >") {
    val src =
      """
        | byte output @$c000
        | void main () {
        |  word a
        |  word b
        |  word c
        |  output = 0
        |  a = 4
        |  b = 4
        |  c = 5
        |  if c > a { output += 1 }
        |  if c > 1 { output += 1 }
        |  if c > 0 { output += 1 }
        | }
      """.stripMargin
    EmuBenchmarkRun(src)(_.readWord(0xc000) should equal(src.count(_ == '+')))
  }

  test("Word comparison >=") {
    val src =
      """
        | byte output @$c000
        | void main () {
        |  word a
        |  word b
        |  word c
        |  output = 0
        |  a = 4
        |  b = 4
        |  c = 5
        |  if c >= 1 { output += 1 }
        |  if c >= a { output += 1 }
        |  if a >= a { output += 1 }
        |  if a >= 4 { output += 1 }
        |  if a >= 4 { output += 1 }
        |  if a >= 0 { output += 1 }
        | }
      """.stripMargin
    EmuBenchmarkRun(src)(_.readWord(0xc000) should equal(src.count(_ == '+')))
  }

  test("Signed comparison >=") {
    val src =
      """
        | byte output @$c000
        | void main () {
        |  sbyte a
        |  sbyte b
        |  sbyte c
        |  output = 0
        |  a = 4
        |  b = 4
        |  c = 5
        |  if c >= 1 { output += 1 }
        |  if c >= a { output += 1 }
        |  if a >= a { output += 1 }
        |  if a >= 4 { output += 1 }
        |  if a >= 4 { output += 1 }
        |  if a >= 0 { output += 1 }
        | }
      """.stripMargin
    EmuBenchmarkRun(src)(_.readWord(0xc000) should equal(src.count(_ == '+')))
  }

  test("Signed comparison < and <=") {
    val src =
      """
        | byte output @$c000
        | void main () {
        |  sbyte a
        |  sbyte b
        |  sbyte c
        |  output = 0
        |  a = -1
        |  b = 0
        |  c = 1
        |  if a < 0 { output += 1 }
        |  if b < 0 { output -= 7 }
        |  if c < 0 { output -= 7 }
        |  if a < 1 { output += 1 }
        |  if b < 1 { output += 1 }
        |  if c < 1 { output -= 7 }
        |  if a <= 0 { output += 1 }
        |  if b <= 0 { output += 1 }
        |  if c <= 0 { output -= 7 }
        |  if a <= 1 { output += 1 }
        |  if b <= 1 { output += 1 }
        |  if c <= 1 { output += 1 }
        |  if a <= -1 { output += 1 }
        |  if b <= -1 { output -= 7 }
        |  if c <= -1 { output -= 7 }
        | }
      """.stripMargin
    EmuBenchmarkRun(src)(_.readWord(0xc000) should equal(src.count(_ == '+')))
  }
}
