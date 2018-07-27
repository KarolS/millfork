package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuBenchmarkRun, EmuCrossPlatformBenchmarkRun}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class SeparateBytesSuite extends FunSuite with Matchers {

  test("Separate assignment 1") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Cmos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)("""
        | word output @$c000
        | void main () {
        |  output = 2:3
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(0x203))
  }

  test("Separate assignment 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Cmos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)("""
        | byte output @$c000
        | byte ignore @$c001
        | void main () {
        |  word w
        |  w = $355
        |  output:ignore = w
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(3))
  }

  test("Separate assignment 3") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Cmos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)("""
        | byte output @$c000
        | byte ignore @$c001
        | void main () {
        |  output:ignore = lol()
        | }
        | word lol() {
        |   return $567
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(5))
  }

  test("Separate assignment 4") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Cmos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)("""
        | array output [5] @$c000
        | byte ignore @$c001
        | void main () {
        |  byte index
        |  index = 3
        |  output[index]:ignore = lol()
        | }
        | word lol() {
        |   return $567
        | }
      """.stripMargin)(_.readByte(0xc003) should equal(5))
  }

  test("Separate assignment 5") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Cmos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)("""
        | array output [5] @$c000
        | byte ignore @$c001
        | void main () {
        |  byte index
        |  index = 3
        |  ignore:output[index] = lol()
        | }
        | word lol() {
        |   return $567
        | }
      """.stripMargin)(_.readByte(0xc003) should equal(0x67))
  }

  test("Magic split array") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Cmos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)("""
        | array hi [16] @$c000
        | array lo [16] @$c010
        | void main () {
        |  word a
        |  word b
        |  word tmp
        |  a = 1
        |  b = 1
        |  byte i
        |  i = 0
        |  while i < 16 {
        |    hi[i]:lo[i] = a
        |    tmp = a
        |    tmp += b
        |    a = b
        |    b = tmp
        |    i += 1
        |  }
        | }
      """.stripMargin) { m=>
      m.readWord(0xc000, 0xc010) should equal(1)
      m.readWord(0xc001, 0xc011) should equal(1)
      m.readWord(0xc002, 0xc012) should equal(2)
      m.readWord(0xc003, 0xc013) should equal(3)
      m.readWord(0xc004, 0xc014) should equal(5)
      m.readWord(0xc005, 0xc015) should equal(8)
      m.readWord(0xc006, 0xc016) should equal(13)
      m.readWord(0xc007, 0xc017) should equal(21)
    }
  }

  test("Separate addition") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Cmos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)("""
        | word output @$c000
        | void main () {
        |  byte h
        |  byte l
        |  h = 6
        |  l = 5
        |  output = $101
        |  output += h:l
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(0x706))
  }

  test("Separate increase") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Cmos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)("""
        | word output @$c000
        | void main () {
        |  byte h
        |  byte l
        |  h = 6
        |  l = 5
        |  (h:l) += $101
        |  output = h:l
        |  (h:l) += 1
        |  output = h:l
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(0x707))
  }

//  test("Complex separate addition") {
//    EmuBenchmarkRun("""
//        | array hi [25] @$c000
//        | array lo [25] @$c080
//        | void main () {
//        |  byte i
//        |  hi[0] = 0
//        |  lo[0] = 0
//        |  hi[1] = 0
//        |  lo[1] = 1
//        |  for i,0,until,lo.length-2 {
//        |    barrier()
//        |    hi[addTwo(i)]:lo[i + (one() << 1)] = hi[i + one()]:lo[1+i]
//        |    barrier()
//        |    hi[addTwo(i)]:lo[i + (one() << 1)] += hi[i]:lo[i]
//        |    barrier()
//        |  }
//        | }
//        | byte one() { return 1 }
//        | byte addTwo(byte x) { return x + 2 }
//        | void barrier() {}
//      """.stripMargin){m =>
//      val h = m.readWord(0xc000 + 24)
//      val l = m.readWord(0xc080 + 24)
//      (h * 0x100 + l) should equal(46368)
//    }
//  }
}
