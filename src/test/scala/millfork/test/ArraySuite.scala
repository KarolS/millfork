package millfork.test

import millfork.{Cpu, OptimizationPresets}
import millfork.assembly.opt.{AlwaysGoodOptimizations, DangerousOptimizations}
import millfork.test.emu._
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class ArraySuite extends FunSuite with Matchers {

  test("Array assignment") {
    val m = EmuSuperOptimizedRun(
      """
        | array output [3] @$c000
        | array input = [5,6,7]
        | void main () {
        |   copyEntry(0)
        |   copyEntry(1)
        |   copyEntry(2)
        | }
        | void copyEntry(byte index) {
        |   output[index] = input[index]
        | }
      """.stripMargin)
    m.readByte(0xc000) should equal(5)
    m.readByte(0xc001) should equal(6)
    m.readByte(0xc002) should equal(7)

  }
  test("Array assignment with offset") {
    EmuUltraBenchmarkRun(
      """
        | array output [8] @$c000
        | void main () {
        |   byte i
        |   i = 0
        |   while i != 6 {
        |     output[i + 2] = i + 1
        |     output[i] = output[i]
        |     i += 1
        |   }
        | }
      """.stripMargin) { m =>
      m.readByte(0xc002) should equal(1)
      m.readByte(0xc007) should equal(6)
    }
  }

  test("Array assignment with offset 1") {
    val m = new EmuRun(Cpu.StrictMos, Nil, DangerousOptimizations.All ++ OptimizationPresets.Good, true)(
      """
        | array output [8] @$c000
        | void main () {
        |   byte i
        |   i = 0
        |   while i != 6 {
        |     output[i + 2] = i + 1
        |     output[i] = output[i]
        |     i += 1
        |   }
        | }
      """.stripMargin)
      m.readByte(0xc002) should equal(1)
      m.readByte(0xc007) should equal(6)
  }

  test("Array assignment through a pointer") {
    val m = EmuUnoptimizedRun(
      """
        | array output [3] @$c000
        | pointer p
        | void main () {
        |   p = output.addr
        |   byte i
        |   byte ignored
        |   i = 1
        |   word w
        |   w = $105
        |   p[i]:ignored = w
        | }
      """.stripMargin)
    m.readByte(0xc001) should equal(1)

  }

  test("Array in place math") {
    EmuBenchmarkRun(
      """
        | array output [4] @$c000
        | void main () {
        |   byte i
        |   i = 3
        |   output[i] = 3
        |   output[i + 1 - 1] *= 4
        |   output[3] *= 5
        | }
      """.stripMargin)(_.readByte(0xc003) should equal(60))
  }

  test("Array simple read") {
    EmuBenchmarkRun(
      """
        | byte output @$c000
        | array a[7]
        | void main () {
        |   byte i
        |   i = 6
        |   a[i] = 6
        |   output = a[i]
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(6))
  }

  test("Array simple read 2") {
    EmuBenchmarkRun(
      """
        | word output @$c000
        | array a[7]
        | void main () {
        |   output = 777
        |   byte i
        |   i = 6
        |   a[i] = 6
        |   output = a[i]
        | }
      """.stripMargin){m =>
      m.readByte(0xc000) should equal(6)
      m.readByte(0xc001) should equal(0)
    }
  }
}
