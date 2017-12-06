package millfork.test

import millfork.{Cpu, OptimizationPresets}
import millfork.assembly.opt.{AlwaysGoodOptimizations, LaterOptimizations, VariableToRegisterOptimization}
import millfork.test.emu.{EmuBenchmarkRun, EmuUltraBenchmarkRun, EmuRun}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class AssemblyOptimizationSuite extends FunSuite with Matchers {

  test("Duplicate RTS") {
    EmuBenchmarkRun(
      """
        | void main () {
        |   if 1 == 1 {
        |     return
        |   }
        | }
      """.stripMargin) { _ => }
  }

  test("Inlining variable") {
    EmuBenchmarkRun(
      """
        | array output [5] @$C000
        | void main () {
        |   byte i
        |   i = 0
        |   while (i<5) {
        |     output[i] = i
        |     i += 1
        |   }
        | }
      """.stripMargin)(_.readByte(0xc003) should equal(3))
  }

  test("Loading modified variables") {
    EmuBenchmarkRun(
      """
        | byte output @$C000
        | void main () {
        |   byte x
        |   output = 5
        |   output += 1
        |   output += 1
        |   output += 1
        |   x = output
        |   output = x
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(8))
  }

  test("Bit ops") {
    EmuBenchmarkRun(
      """
        | byte output @$C000
        | void main () {
        |   output ^= output
        |   output |= 5 | 6
        |   output |= 5 | 6
        |   output &= 5 & 6
        |   output ^= 8 ^ 16
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(28))
  }

  test("Inlining after a while") {
    EmuBenchmarkRun(
      """
        | array output [2]@$C000
        | void main () {
        |   byte i
        |   output[0] = 6
        |   lol()
        |   i = 1
        |   if (i > 0) {
        |     output[i] = 4
        |   }
        | }
        | void lol() {}
      """.stripMargin)(_.readWord(0xc000) should equal(0x406))
  }

  test("Tail call") {
    EmuBenchmarkRun(
      """
        | byte output @$C000
        | void main () {
        |   if (output != 55) {
        |     output += 1
        |     main()
        |   }
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(55))
  }

  test("LDA-TAY elimination") {
    new EmuRun(Cpu.StrictMos, OptimizationPresets.NodeOpt, List(VariableToRegisterOptimization, AlwaysGoodOptimizations.YYY), false)(
      """
        |	array mouse_pointer[64]
        |	array arrow[64]
        | byte output @$C000
        | void main () {
        |		byte i
        |		i = 0
        |		while i < 63 {
        |			mouse_pointer[i] = arrow[i]
        |			i += 1
        |		}
        | }
      """.stripMargin)
  }

  test("Carry flag after AND-LSR") {
    EmuUltraBenchmarkRun(
      """
        | byte output @$C000
        | void main () {
        |   output = f(5)
        | }
        | byte f(byte x) {
        |   return ((x & $1E) >> 1) + 3
        | }
        |
      """.stripMargin)(_.readByte(0xc000) should equal(5))
  }

  test("Index sequence") {
    EmuUltraBenchmarkRun(
      """
        | array output[6] @$C000
        | void main () {
        |   pointer o
        |   o = output.addr
        |   o[3] = 8
        |   o[4] = 8
        |   o[5] = 8
        | }
        |
      """.stripMargin){m =>
      m.readByte(0xc005) should equal(8)
    }
  }

  test("Index switching") {
    EmuUltraBenchmarkRun(
      """
        | array output1[6] @$C000
        | array output2[6] @$C010
        | array input[6] @$C010
        | void main () {
        |   static byte a
        |   static byte b
        |   input[5] = 3
        |   a = five()
        |   b = five()
        |   output1[a] = input[b]
        |   output2[a] = input[b]
        | }
        | byte five() {
        |   return 5
        | }
        |
      """.stripMargin){m =>
      m.readByte(0xc005) should equal(3)
      m.readByte(0xc015) should equal(3)
    }
  }

  test("TAX-BCC-RTS-TXA optimization") {
    new EmuRun(Cpu.StrictMos,
      OptimizationPresets.NodeOpt, List(
        AlwaysGoodOptimizations.PointlessStoreAfterLoad,
        LaterOptimizations.PointlessLoadAfterStore,
        VariableToRegisterOptimization,
        LaterOptimizations.DoubleLoadToDifferentRegisters,
        LaterOptimizations.DoubleLoadToTheSameRegister,
        AlwaysGoodOptimizations.PointlessRegisterTransfers,
        AlwaysGoodOptimizations.PointlessRegisterTransfersBeforeReturn,
        AlwaysGoodOptimizations.PointlessLoadBeforeReturn,
        AlwaysGoodOptimizations.PoinlessLoadBeforeAnotherLoad,
        AlwaysGoodOptimizations.PointlessRegisterTransfers,
        AlwaysGoodOptimizations.PointlessRegisterTransfersBeforeReturn,
        AlwaysGoodOptimizations.PointlessRegisterTransfersBeforeReturn,
        AlwaysGoodOptimizations.PointlessLoadBeforeReturn,
        AlwaysGoodOptimizations.PoinlessLoadBeforeAnotherLoad,
        AlwaysGoodOptimizations.PointlessStashingToIndexOverShortSafeBranch,
        AlwaysGoodOptimizations.PointlessRegisterTransfersBeforeReturn,
        AlwaysGoodOptimizations.IdempotentDuplicateRemoval,
        AlwaysGoodOptimizations.IdempotentDuplicateRemoval,
        AlwaysGoodOptimizations.IdempotentDuplicateRemoval), false)(
      """
        | byte output @$C000
        | void main(){ delta() }
        | byte delta () {
        |    output = 0
        |    byte mouse_delta
        |	   mouse_delta = 6
        |	   mouse_delta &= $3f
        |	   if mouse_delta >= $20 {
        |	     mouse_delta |= $c0
        |      return 3
        |    }
        |    return mouse_delta
        | }
      """.stripMargin).readByte(0xc000) should equal(0)
  }

  test("Memory access detection"){
    EmuUltraBenchmarkRun(
      """
        | array h [4] @$C000
        | array l [4] @$C404
        | word output @$C00C
        | word a @$C200
        | void main () {
        |   byte i
        |   a = 0x102
        |   barrier()
        |   for i,0,until,4 {
        |     h[i]:l[i] = a
        |   }
        |   a.lo:a.hi=a
        |   output = a
        | }
        | void barrier (){}
        |
      """.stripMargin){m =>
      m.readByte(0xc000) should equal(1)
      m.readByte(0xc001) should equal(1)
      m.readByte(0xc002) should equal(1)
      m.readByte(0xc003) should equal(1)
      m.readByte(0xc404) should equal(2)
      m.readByte(0xc405) should equal(2)
      m.readByte(0xc406) should equal(2)
      m.readByte(0xc407) should equal(2)
      m.readWord(0xc00c) should equal(0x201)
    }
  }

  test("Memory access detection 2"){
    EmuUltraBenchmarkRun(
      """
        | array h [4]
        | array l [4]
        | word output @$C00C
        | word ptrh @$C000
        | word ptrl @$C002
        | void main () {
        |   ptrh = h.addr
        |   ptrl = l.addr
        |   byte i
        |   word a
        |   a = 0x102
        |   barrier()
        |   for i,0,until,4 {
        |     h[i]:l[i] = a
        |   }
        |   a.lo:a.hi=a
        |   output = a
        |   couput
        | }
        | void barrier (){}
        |
      """.stripMargin){m =>
      val ptrh = 0xffff & m.readWord(0xC000)
      val ptrl = 0xffff & m.readWord(0xC002)
      m.readByte(ptrh + 0) should equal(1)
      m.readByte(ptrh + 1) should equal(1)
      m.readByte(ptrh + 2) should equal(1)
      m.readByte(ptrh + 3) should equal(1)
      m.readByte(ptrl + 0) should equal(2)
      m.readByte(ptrl + 1) should equal(2)
      m.readByte(ptrl + 2) should equal(2)
      m.readByte(ptrl + 3) should equal(2)
      m.readWord(0xc00c) should equal(0x201)
    }
  }
}
