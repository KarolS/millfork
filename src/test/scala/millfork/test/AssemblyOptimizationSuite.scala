package millfork.test

import millfork.{Cpu, OptimizationPresets}
import millfork.assembly.mos.opt.{AlwaysGoodOptimizations, LaterOptimizations, VariableToRegisterOptimization}
import millfork.test.emu._
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class AssemblyOptimizationSuite extends FunSuite with Matchers {

  test("Duplicate RTS") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | void main () {
        |   if 1 == 1 {
        |     return
        |   }
        | }
      """.stripMargin) { _ => }
  }

  test("Inlining variable") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
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

  test("Inlining variable 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | array output [100] @$C000
        | void main () {
        |   register byte i
        |   i = 1
        |   while (i<50) {
        |     output[i] = i
        |     i <<= 1
        |   }
        | }
      """.stripMargin)(_.readByte(0xc004) should equal(4))
  }

  test("Loading modified variables") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
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
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
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
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
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
      """.stripMargin){m =>
      m.readByte(0xc000) should equal(6)
      m.readByte(0xc001) should equal(4)
    }
  }

  test("Tail call") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
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
    new EmuRun(Cpu.StrictMos, OptimizationPresets.NodeOpt, List(VariableToRegisterOptimization, AlwaysGoodOptimizations.YYY))(
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
        | byte f(byte x) = ((x & $1E) >> 1) + 3
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
        AlwaysGoodOptimizations.IdempotentDuplicateRemoval))(
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
        |   output
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

  test("Empty interrupt"){
    EmuSuperOptimizedRun(
      """
        | void main () {
        | }
        | interrupt void i (){}
        |
      """.stripMargin)
  }

  test("Adding a nonet") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | word output @$C000
        | byte source @$C002
        | void main () {
        |   init()
        |   output += nonet(source << 1)
        | }
        | void init() {
        |   output = 0
        |   source = $FF
        | }
        |
      """.stripMargin){m =>
      m.readWord(0xc000) should equal(0x1FE)
    }
  }

  test("Common indexing subexpression elimination") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | array output [55] @$C000
        | array input = [0,1,2,3,4,5,6,7,8,9,10]
        | void main () {
        |   byte indexer
        |   indexer = init()
        |   output[(indexer + 1) & 7] = input[(indexer + 1) & 7]
        | }
        | byte init() {
        |   return 2
        | }
        |
      """.stripMargin){m =>
      m.readByte(0xc003) should equal(3)
      m.readByte(0xc004) should equal(0)
    }
  }

  test("Effectively const variable") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        |byte output @$c000
        |void main() {
        |   byte b
        |   byte c
        |   b = five()
        |   five()
        |   c = b
        |   if output == 73 {
        |     output = 1
        |   } else {
        |     output = b & c
        |   }
        |}
        |noinline byte five () { return 5 }
      """.stripMargin
    ){m =>
      m.readByte(0xc000) should equal(5)
    }
  }

  test("Common conditions") {

    new EmuRun(Cpu.StrictMos,
      OptimizationPresets.NodeOpt, List(
        LaterOptimizations.PointlessLoadAfterStore,
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
        AlwaysGoodOptimizations.IdempotentDuplicateRemoval,
        AlwaysGoodOptimizations.CommonExpressionInConditional))(
      """
        | byte output @$C000
        | void main(){
        |   byte a
        |   byte b
        |   output = 0
        |   a = delta()
        |   if a == 0 {
        |     output += 1
        |     a += 1
        |   }
        |   b = a
        |   if b == 0 {
        |     output += 1
        |     a += 1
        |   }
        | }
        | byte delta () {
        |    return 0
        | }
      """.stripMargin).readByte(0xc000) should equal(1)
  }

  test("Constant pointers") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        |byte output0 @$c000
        |byte output1 @$c001
        |void main() {
        |   pointer p
        |   p = output0.addr
        |   p[0] = 33
        |   p = op()
        |   p[0] = 34
        |}
        |word op () { return output1.addr }
      """.stripMargin
    ){m =>
      m.readByte(0xc000) should equal(33)
      m.readByte(0xc001) should equal(34)
    }
  }

  test("Low bit") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | byte output @$c000
        | void main() {
        |   if f() << 1 == 1 {
        |     output = 5
        |   }
        | }
        | noinline byte f () {
        |   output = 33
        |   return 3
        | }
      """.stripMargin
    ){m =>
      m.readByte(0xc000) should equal(33)
    }
  }

  test("Low bit 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | byte output @$c000
        | void main() {
        |   if f() << 1 == 2 {
        |     output = 5
        |   }
        | }
        | noinline byte f () {
        |   output = 33
        |   return 3
        | }
      """.stripMargin
    ){m =>
      m.readByte(0xc000) should equal(33)
    }
  }

  test("Low bit 3") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | byte output @$c000
        | void main() {
        |   g(1)
        | }
        | void g(byte x) {
        |   if f() << 1 == x {
        |     output = 5
        |   }
        | }
        | noinline byte f () {
        |   output = 33
        |   return 3
        | }
      """.stripMargin
    ){m =>
      m.readByte(0xc000) should equal(33)
    }
  }

  test("Low bit 4") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | byte output @$c000
        | void main() {
        |   g(1)
        | }
        | void g(byte x) {
        |   f()
        |   output &= $F0
        |   output += 1
        | }
        | noinline byte f () {
        |   output = 33
        |   return 3
        | }
      """.stripMargin
    ){m =>
      m.readByte(0xc000) should equal(33)
    }
  }

  test("Identity page") {
      EmuUltraBenchmarkRun(
        """
          | byte output @$c000
          | void main() {
          |   byte b
          |   b = f()
          |   output = (b ^ $40) + b
          | }
          | noinline byte f () {
          |   return 3
          | }
        """.stripMargin
      ){m =>
        m.readByte(0xc000) should equal(0x46)
      }
    }

  test("Shift and increase") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
        """
          | byte output @$c000
          | void main() {
          |   output = twicePlusOne(5)
          | }
          | noinline byte twicePlusOne (byte x) {
          |   return x * 2 + 1
          | }
        """.stripMargin
      ){m =>
        m.readByte(0xc000) should equal(11)
      }
    }

  test("Add one bit") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
        """
          | byte output @$c000
          | void main() {
          |   output = 0
          |   increaseByBit(5)
          | }
          | noinline void increaseByBit (byte y) {
          |   output += y & 1
          | }
        """.stripMargin
      ){m =>
        m.readByte(0xc000) should equal(1)
      }
    }

  test("Shift, mask and increase 1") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Motorola6809)(
      """
        | byte output @$c000
        | void main() {
        |   output = twicePlusLowBit(5, 5)
        | }
        | noinline byte twicePlusLowBit (byte x, byte y) {
        |   return (y & 1) + x * 2
        | }
      """.stripMargin
    ) { m =>
      m.readByte(0xc000) should equal(11)
    }
  }

  test("Shift, mask and increase 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Motorola6809)(
      """
        | byte output @$c000
        | void main() {
        |   output = twicePlusLowBit(5, 5)
        | }
        | noinline byte twicePlusLowBit (byte x, byte y) {
        |   return x * 2 + (y & 1)
        | }
      """.stripMargin
    ) { m =>
      m.readByte(0xc000) should equal(11)
    }
  }

  test("Shift, mask, increase and test") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Motorola6809)(
      """
        | byte output @$c000
        | void main() {
        |   stuff(0, 4)
        | }
        | inline void stuff (byte x, byte y) {
        |   if ((f(y) & 1) + x * 2) == 0 {
        |     output = 11
        |   }
        | }
        | noinline byte f(byte y) { return y }
      """.stripMargin
    ) { m =>
      m.readByte(0xc000) should equal(11)
    }
  }

  test("Double indices") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Motorola6809)(
      """
        | array output[1000] @$c000
        | array xbuf[40]
        | array ybuf[25]
        | void main() {
        |   stuff(output.addr)
        | }
        | noinline void stuff (pointer scrn) {
        |   byte j
        |   byte jj
        |   for jj,0,until,25 {
        |    for j,0,until,40 {
        |       scrn[j] = xbuf[j] + ybuf[jj]
        |     }
        |    scrn += 40
        |   }
        | }
      """.stripMargin
    ) { m =>
    }
  }

  test("Using accumulator") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos)(
      """
        |
        |byte __last_used_device @$ba
        |noinline byte last_used_device() {
        |    byte device
        |    device = __last_used_device
        |    if device == 0 { device = 8 }
        |    return device
        |}
        |
        |
        |void main() {
        |    last_used_device()
        |}
      """.stripMargin
    ) { m =>
    }
  }

  test("Not using X") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Motorola6809)(
      """
        | word output @$c000
        | inline word w() = 300
        |
        | void main() {
        |    output = w()
        | }
      """.stripMargin
    ) { m =>
      m.readWord(0xc000) should equal(300)
    }
  }

  test("Some stuff") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Motorola6809)(
      """
        | array output[256] @ $c000
        | void main() {
        |   byte i
        |   static byte a
        |   static byte b
        |   for i,0,until,200 {
        |     a = i + 1
        |     b = i + 1
        |     output[nonet(a+b)>>>>1] = 3
        |   }
        | }
      """.stripMargin
    ) { m =>
      m.readByte(0xc001) should equal(3)
    }
  }

  test("Simple flags") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Intel8080, Cpu.Motorola6809)(
      """
        | byte output @ $c000
        | void main() {
        |   output = 0
        |   if f() & 1 != 0 {
        |     output += 1
        |   }
        | }
        | noinline byte f() = 1
      """.stripMargin
    ) { m =>
      m.readByte(0xc000) should equal(1)
    }
  }

  test("Optimize certain indexing operations") {
    EmuBenchmarkRun(
      """
        |array arr16[$100]
        |noinline word getValue(word index) {
        |    word tmp
        |    tmp.hi = arr16[((index & 0x7f)<<1)+1]
        |    tmp.lo = arr16[(index & 0x7f)<<1]
        |    return (tmp & 0xff) >> 1;
        |}
        |void main() {
        |    getValue(1)
        |}
      """.stripMargin){ m => }
  }


  test("Optimize parameters") {
    // TODO the parameters in the sum_until functions are not inlined into registers:
      val c = """
        | byte output @ $c000
        | array a = [2,5,6,8,8]
        | void main() {
        |   output = sum_until(a, 0, 3)
        | }
        |             noinline byte sum_until(pointer p, byte from, byte to_exclusive) {
        |                byte i
        |                byte sum
        |                sum = 0
        |                for i,from,paralleluntil,to_exclusive {
        |                    sum += p[0]
        |                    p+=1
        |                }
        |                return sum
        |            }
      """.stripMargin
    EmuOptimizedZ80Run(c).readByte(0xc000) should equal(13)
    EmuOptimizedIntel8080Run(c).readByte(0xc000) should equal(13)
  }

  test("Optimize load after test") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Intel8080, Cpu.Motorola6809)(
      """
        | byte output @ $c000
        | void main() {
        |   f(2)
        | }
        | noinline void f(byte i) {
        |   if (i == 2) {
        |     g(2)
        |   }
        | }
        | noinline void g(byte i) {
        |   output = i
        | }
      """.stripMargin
    ) { m =>
      m.readByte(0xc000) should equal(2)
    }
  }

  test("Inline variables correctly") {
    EmuBenchmarkRun("""
        |array arr = [2,2,2,1,1,2,2]
        |noinline byte f() {
        |  byte i
        |  i = 0
        |  while true {
        |   if arr[i] == 1 { return i }
        |   i += 1
        |  }
        |  return $ff
        |}
        |volatile byte output @$c000
        |void main() {
        | output = f()
        |}
        |""".stripMargin
    ) { m =>
      m.readByte(0xc000) should equal(3)
    }
  }

  test("Repeated struct array indexing optimization") {
    EmuBenchmarkRun(
      """
        | struct A {
        |   byte padding
        |   byte x
        |   byte y
        | }
        |
        | array(A) arr[40]
        |
        | byte output  @$c000
        | void main() {
        |     arr[0].x = 1
        |     f(0, 50)
        |     output = arr[0].y
        | }
        |
        | noinline void f(byte i, byte v) {
        |   arr[i].y = arr[i].x + v
        | }
        |""".stripMargin){ m =>
      m.readByte(0xc000) should equal(51)
    }
  }

  test("Test bug #41") {
    val code =
      """
        |struct Entity {
        |    byte x,
        |    byte y
        |}
        |
        |array(Entity) entities [2]
        |
        |byte output @$c000
        |
        |void main() {
        |    if test(0, 200) {
        |        output = 1
        |    }
        |}
        |
        |inline bool test(byte i, byte y) {
        |    return y >= entities[i].y && y <= entities[i].y + 8
        |}
        |
        |""".stripMargin
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(code) { m =>

    }
    EmuUndocumentedRun(code)
  }

  test("Optimize commutative in-place modifications using small arrays") {

    val code =
      """
        |
        |array output [4] @$c000
        |
        |void main() {
        |    f($c000, 1, $c000, 2)
        |    g(1, 2)
        |}
        |
        |noinline void f(pointer p1, byte i1, pointer p2, byte i2) {
        |    p1[i1] ^= p2[i2]
        |}
        |
        |noinline void g(byte i1, byte i2) {
        |    output[i1] ^= output[i2]
        |}
        |
        |""".stripMargin
    EmuCrossPlatformBenchmarkRun(Cpu.Mos)(code) { m =>

    }
  }

  test("Some combinations of shifting and indexing") {
    EmuZ80BenchmarkRun(
      """
        |volatile byte x
        |
        |noinline void f() {
        |    pointer(x/8)[0] |= 0x80>>(x&7)
        |}
        |
        |void main() {
        |    byte i
        |    for i,0,parallelto,255 { pointer(i)[0] = 0 }
        |    x = $91
        |    f()
        |}
        |""".stripMargin) { m =>
      m.readByte(0x91/8) should be (0x80>>(0x91 & 7))
    }
  }

  test("Issue 99") {
//    val m = new EmuRun(Cpu.StrictMos, Nil, List(
//      LaterOptimizations.DoubleLoadToDifferentRegisters,
//      LaterOptimizations.IndexSwitchingOptimization))(
    val m = EmuOptimizedRun(
      """
        |array(byte) output [256] @$c000
        |array(byte) input = [$10, $20, $30, $40, $50, $60, $70, $80, $90, $A0]
        |byte write_index
        |noinline void f(byte i, bool swap, byte first, byte data_1, byte data_2) {
        |  output[write_index] = first
        |  if (swap) {
        |    output[write_index + 1] = data_2
        |  } else {
        |    output[write_index + 1] = data_1
        |  }
        |  output[write_index + 2] = input[i]
        |
        |  output[write_index + 3] = first
        |  if (swap) {
        |    output[write_index + 4] = data_1
        |  } else {
        |    output[write_index + 4] = data_2
        |  }
        |  output[write_index + 5] = input[i]
        |
        |  write_index += 6
        |}
        |void main() {
        |  write_index = 0
        |
        |  byte j
        |  bool flip
        |  for j,0,until,10 {
        |    flip = j != 5
        |    f(j, flip, $FF, 1, 2)
        |  }
        |}
        |""".stripMargin)
  }

  test("Sign extension") {
    EmuBenchmarkRun(
      """
        |array(word) output [5]@$c000
        |
        |noinline void set(byte i, sbyte s) {
        |    word w
        |    output[i] = s
        |}
        |
        |void main() {
        |    set(0, 0)
        |    set(1, -1)
        |    set(2, 1)
        |    set(3, 0)
        |    set(4, 10)
        |}
        |""".stripMargin) { m =>
      m.readWord(0xc000) should equal(0)
      m.readWord(0xc002) should equal(0xffff)
      m.readWord(0xc004) should equal(1)
      m.readWord(0xc006) should equal(0)
      m.readWord(0xc008) should equal(10)
    }
  }
}
