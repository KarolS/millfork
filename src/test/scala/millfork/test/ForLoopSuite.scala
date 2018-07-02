package millfork.test

import millfork.CpuFamily
import millfork.error.ErrorReporting
import millfork.test.emu.{EmuBenchmarkRun, EmuCrossPlatformBenchmarkRun, EmuUnoptimizedRun}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class ForLoopSuite extends FunSuite with Matchers {

  test("For-to") {
    EmuBenchmarkRun(
      """
        | word output @$c000
        | void main () {
        |   byte i
        |   output = 0
        |   for i,0,to,5 {
        |     output += i
        |   }
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(15))
  }

  test("For-to 2") {
    EmuBenchmarkRun(
      """
        | word output @$c000
        | byte five
        | void main () {
        |   init()
        |   byte i
        |   output = 0
        |   for i,0,to,five {
        |     output += i
        |   }
        | }
        | void init() {
        |   five = 5
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(15))
  }

  test("For-downto") {
    EmuBenchmarkRun(
      """
        | word output @$c000
        | void main () {
        |   byte i
        |   output = 0
        |   for i,5,downto,0 {
        |     output += i
        |   }
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(15))
  }

  test("For-downto 2") {
    EmuCrossPlatformBenchmarkRun(CpuFamily.M6502, CpuFamily.I80)(
      """
        | array output [55] @$c000
        | void main () {
        |   byte i
        |   output[0] = 0
        |   output[5] = 0
        |   output[6] = 0
        |   for i,5,downto,0 {
        |     output[i] += 1
        |   }
        | }
      """.stripMargin){m =>
      m.readByte(0xc000) should equal(1)
      m.readByte(0xc005) should equal(1)
      m.readByte(0xc006) should equal(0)
    }
  }

  test("For-until") {
    EmuBenchmarkRun(
      """
        | word output @$c000
        | void main () {
        |   byte i
        |   output = 0
        |   for i,0,until,6 {
        |     output += i
        |   }
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(15))
  }
  test("For-parallelto") {
    EmuBenchmarkRun(
      """
        | word output @$c000
        | void main () {
        |   byte i
        |   output = 0
        |   for i,0,parallelto,5 {
        |     output += i
        |   }
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(15))
  }
  test("For-paralleluntil") {
    EmuBenchmarkRun(
      """
        | word output @$c000
        | void main () {
        |   byte i
        |   output = 0
        |   for i,0,paralleluntil,6 {
        |     output += i
        |   }
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(15))
  }

  test("Various loops") {
    EmuUnoptimizedRun(
      """
        | void init() {
        |     zero = 0
        |     ff = $ff
        | }
        | byte zero
        | byte ff
        | byte flag @$c000
        | void main () {
        |     init()
        |     byte i
        |     flag = 0
        |     for i, zero, until, ff { }
        |     flag = 1
        |     for i, zero, to, ff { }
        |     flag = 2
        |     for i, ff, downto, zero { }
        |     flag = 3
        |     for i, zero, paralleluntil, ff { }
        |     flag = 4
        |     for i, zero, parallelto, ff { }
        |     flag = 5
        |     for i, ff, until, zero { _panic() }
        |     flag = 6
        |     for i, ff, paralleluntil, zero { _panic() }
        |     flag = 7
        |     for i, ff, paralleluntil, zero { _panic() }
        |     flag = 8
        |     for i, zero, until, ff { }
        |     flag = 9
        | }
        | void _panic(){while(true){}}
      """.stripMargin)
  }

  test("Memcpy") {
    EmuCrossPlatformBenchmarkRun(CpuFamily.M6502, CpuFamily.I80)(
      """
        | array output[5]@$c001
        | array input = [0,1,4,9,16,25,36,49]
        | void main () {
        |   byte i
        |   for i,0,until,output.length {
        |     output[i] = input[i+1]
        |   }
        | }
        | void _panic(){while(true){}}
      """.stripMargin){ m=>
      m.readByte(0xc001) should equal (1)
      m.readByte(0xc005) should equal (25)
    }
  }

  test("Various bulk operations") {
    EmuCrossPlatformBenchmarkRun(CpuFamily.M6502, CpuFamily.I80)(
      """
        | array output0[5]@$c000
        | array output1[5]@$c010
        | array output2[5]@$c020
        | array input = [0,1,4,9,16,25,36,49]
        | void main () {
        |   byte i
        |   for i,0,until,5 {
        |     output0[i] = 0
        |   }
        |   for i,0,paralleluntil,5 {
        |     output1[i] = 1
        |     output2[i] = i
        |   }
        |   for i,4,downto,0 {
        |     output0[i] +'= 4
        |     output2[i] <<= 1
        |   }
        |   for i,0,to,4 {
        |     output1[i] ^= i
        |     output1[i] += 5
        |   }
        | }
      """.stripMargin){ m=>
      m.dump(0xc000, 5)(ErrorReporting.debug(_))
      m.dump(0xc010, 5)(ErrorReporting.debug(_))
      m.dump(0xc020, 5)(ErrorReporting.debug(_))
      m.readByte(0xc001) should equal (4)
      m.readByte(0xc023) should equal (6)
      m.readByte(0xc013) should equal (7)
    }
  }


}
