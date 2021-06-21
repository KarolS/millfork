package millfork.test

import millfork.{Cpu, CpuFamily, OptimizationPresets}
import millfork.assembly.mos.opt.{AlwaysGoodOptimizations, DangerousOptimizations}
import millfork.test.emu._
import org.scalatest.{AppendedClues, FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class ArraySuite extends FunSuite with Matchers with AppendedClues {

  test("Array assignment") {
    val src =
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
      """.stripMargin
    val m = EmuSuperOptimizedRun(src)
    m.readByte(0xc000) should equal(5)
    m.readByte(0xc001) should equal(6)
    m.readByte(0xc002) should equal(7)
    EmuCrossPlatformBenchmarkRun(Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(src) { m =>
      m.readByte(0xc000) should equal(5)
      m.readByte(0xc001) should equal(6)
      m.readByte(0xc002) should equal(7)
    }

  }
  test("Array assignment with offset") {
    val src =
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
      """.stripMargin
    EmuUltraBenchmarkRun(src) { m =>
      m.readByte(0xc002) should equal(1)
      m.readByte(0xc007) should equal(6)
    }
    EmuCrossPlatformBenchmarkRun(Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(src) { m =>
      m.readByte(0xc002) should equal(1)
      m.readByte(0xc007) should equal(6)
    }
  }

  test("Array assignment with offset 1") {
    try {
      val m = new EmuRun(Cpu.StrictMos, Nil, DangerousOptimizations.All ++ OptimizationPresets.Good)(
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
    } catch {
      case th: Throwable =>
        th.printStackTrace(System.err)
        throw th
    }
  }

  test("Array assignment through a pointer") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
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
      """.stripMargin) { m =>
      m.readByte(0xc001) should equal(1)
    }

  }

  test("Array in place math") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
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
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
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
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
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
      m.readWord(0xc000) should equal(6)
    }
  }

  test("Pointers") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | byte output
        |   pointer a
        |   pointer b
        |   pointer c
        | void main () {
        |   setup()
        |   reset()
        | }
        | void setup() {
        |   a = output.addr
        |   b = output.addr
        |   c = output.addr
        | }
        | void reset() {
        |   a[0] = 0
        |   b[0] = 0
        |   c[0] = 0
        | }
      """.stripMargin)(_.readByte(0xc000) should equal(0))

  }

  test("Pointer indexing test") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | array output [4] @$c000
        |   pointer a
        |   byte i
        | void main () {
        |   setup()
        |   a[i + 1] = 55
        | }
        | void setup() {
        |   a = output.addr
        |   i = 2
        | }
      """.stripMargin)(_.readByte(0xc003) should equal(55))
  }

  test("Syntax") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | array a = [1, 2, 3]
        | array b = "text" ascii
        | array c = ["text" ascii, 5]
        | void main () {
        | }
      """.stripMargin){m => ()}

  }

  test("Negative subindex") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        |
        | array output [$fff] @$c000
        | void main () {
        |   byte i
        |   output[$100] = 55
        |   i = one()
        |   output[1 - i] = 5
        | }
        | noinline byte one() {return 1}
      """.stripMargin) { m =>
      m.dump(0xbf00, 0x200)(println(_))
      m.readByte(0xc100) should equal(55)
      m.readByte(0xc000) should equal(5)
    }
  }

  test("Word subindex 1") {
    EmuBenchmarkRun(
      """
        |
        | array output [$fff] @$c000
        | void main () {
        |   word i
        |   i = big()
        |   output[$64] = 55
        |   output[i] = 6
        |   output[i] = 5
        | }
        | noinline word big() {return $564}
      """.stripMargin) {m =>
      m.readByte(0xc064) should equal(55)
      m.readByte(0xc564) should equal(5)
    }

  }

  test("Word subindex 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        |
        | array output [$fff] @$c000
        | void main () {
        |   word i
        |   pointer o
        |   o = p()
        |   i = big()
        |   o[$164] = 55
        |   o[i] = 5
        | }
        | noinline word big() {return $564}
        | noinline word p() {return output.addr}
      """.stripMargin) {m =>
      m.readByte(0xc164) should equal(55)
      m.readByte(0xc564) should equal(5)
    }

  }

  test("Array filters") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | array x = @word_le [$1144]
        | byte output @$c000
        | void main () {
        |   output = x[0]
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(0x44)
    }
  }

  test("Array filters 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | array x = @long_le [$1144]
        | byte output @$c000
        | void main () {
        |   output = x[0]
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(0x44)
    }
  }

  test("Const arrays") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | const array square = [0, 1, 4, 9, 16, 25, 36, 49, 64]
        | byte five() = 5
        | byte output0 @$c000
        | byte output1 @$c001
        | void main () {
        |   output0 = square[3]
        |   output1 = square[five()]
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(9)
      m.readByte(0xc001) should equal(25)
    }
  }

  test("Writing to const arrays should not compile") {
    ShouldNotCompile(
      """
        | const array a = [0]
        | void main () {
        |   a[0] = 5
        | }
      """.stripMargin)
  }

  test("Struct array initializers") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | struct p { byte x, byte y }
        | struct line { p from, p to }
        | struct ratio { int32 num, int32 den }
        | const array data1 @$c000 = @struct [p(2,3), p(4,5)]
        | const array data2 @$c010 = @struct [line(p(6,7), p(8,9))]
        | const array data3 @$c020 = @struct [ratio($666, $777)]
        | void main () { }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(2)
      m.readByte(0xc001) should equal(3)
      m.readByte(0xc002) should equal(4)
      m.readByte(0xc003) should equal(5)
      m.readByte(0xc010) should equal(6)
      m.readByte(0xc011) should equal(7)
      m.readByte(0xc012) should equal(8)
      m.readByte(0xc013) should equal(9)
      m.readByte(0xc020) should equal(0x66)
      m.readByte(0xc021) should equal(6)
      m.readByte(0xc022) should equal(0)
      m.readByte(0xc023) should equal(0)
      m.readByte(0xc024) should equal(0x77)
      m.readByte(0xc025) should equal(7)
      m.readByte(0xc026) should equal(0)
      m.readByte(0xc027) should equal(0)
    }
  }

  test("Local arrays") {
    EmuUnoptimizedRun(
      """
        | byte output @$c000
        | void main () {
        |   array square[5]
        |   square[0] = 1
        |   output = square[0]
        | }
      """.stripMargin).readByte(0xc000) should equal(1)
    ShouldNotCompile(
      """
        | void main () {
        |   array square = [0]
        | }
      """.stripMargin)
    ShouldNotCompile(
      """
        | void f() {
        |   square[1] = 1
        | }
        | void main () {
        |   array square = [0]
        | }
      """.stripMargin)
    EmuUnoptimizedRun(
      """
        | byte output @$c000
        | void main () {
        |   const array square = [1]
        |   output = square[0]
        | }
      """.stripMargin).readByte(0xc000) should equal(1)
  }

  test("Arrays of words") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Intel8080, Cpu.Z80, Cpu.Motorola6809)(
      """
        | array(word) words[10] @$c000
        | void main () {
        |   words[2] = $702
        |   words[3] = $201
        |   memory_barrier()
        |   words[1] = words[3]
        |   words[4] = words[3] + words[2]
        |   words[5] = $101
        |   words[5] = words[5] << 1
        |   words[5] = words[5] + $1001
        | }
      """.stripMargin){ m =>
      m.readWord(0xc004) should equal(0x702)
      m.readWord(0xc006) should equal(0x201)
      m.readWord(0xc002) should equal(0x201)
      m.readWord(0xc008) should equal(0x903)
      m.readWord(0xc00a) should equal(0x1203)
    }
  }

  test("Initialized arrays of words") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Intel8080, Cpu.Z80, Cpu.Motorola6809)(
      """
        | struct coord { byte x, byte y }
        |
        | array(word) a = [1,2,3]
        | array(coord) c = [coord(1,2),coord(3,4)]
        | array(coord) h @$8000 = for i,0,until,100 [coord(i,i)]
        |
        | word output @$c000
        | coord output2 @$c002
        |
        | void main () {
        |   output = a[2]
        |   output2 = c[1]
        | }
        |
      """.stripMargin){ m =>
      m.readWord(0xc000) should equal(3)
      m.readByte(0xc002) should equal(3)
      m.readByte(0xc003) should equal(4)
    }
  }

  test("Pointers to array elements") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Intel8080, Cpu.Z80, Cpu.Motorola6809)(
      """
        | struct coord { byte x, byte y }
        |
        | array(coord) c = [coord(1,2),coord(3,4)]
        | array(byte) z = "hello!"
        |
        | word output @$c000
        |
        | void main () {
        |   output = 354
        |   output += word(c[0].pointer)
        |   output -= c[0].addr
        |   output += word(c[0].x.pointer)
        |   output -= word(c[0].x.addr)
        |   output += word(pointer.coord(c.addr)->x.addr)
        |   output -= word(pointer.coord(c.addr)->x.pointer)
        |   output += word(z[0].pointer)
        |   output -= z[0].addr
        | }
        |
      """.stripMargin){ m =>
      m.readWord(0xc000) should equal(354)
    }
  }

  test("Invalid array things that will become valid in the future") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Intel8080, Cpu.Z80 , Cpu.Sharp, Cpu.Motorola6809)(
      """
        | array(int32) a[7] @$c000
        | void main () {
        |   int32 tmp
        |   tmp = f()
        |   a[0] = tmp
        |   a[0] += 2
        |   a[0] <<= 2
        |   a[0] -= 7
        | }
        | noinline int32 f() = 5
      """.stripMargin) { m =>
      m.readLong(0xc000) should equal(21)
    }
  }

  test("Various large assignments involving arrays") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Intel8080, Cpu.Z80, Cpu.Motorola6809)(
      """
        | array(int32) a[7] @$c000
        | void main () {
        |   a[0] = 2
        | }
      """.stripMargin) { m => }
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Intel8080, Cpu.Z80, Cpu.Motorola6809)(
      """
        | array(int32) a[7] @$c000
        | int32 main () {
        |   return a[4]
        | }
      """.stripMargin) { m => }
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Intel8080, Cpu.Z80, Cpu.Motorola6809)(
      """
        | array(int32) a[7] @$c000
        | noinline void f(byte i) {
        |   a[i] = a[i+2]
        | }
        | void main () {
        |   f(1)
        | }
      """.stripMargin) { m => }
  }

  test("Various large assignments involving arrays and arithmetic conversions") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Intel8080, Cpu.Z80)(
      """
        | array(int32) a[7] @$c000
        | array(int24) b[7] @$c100
        | noinline void init() {
        |   b[0] = 1
        |   b[1] = 2
        |   a[0].b3 = 4
        |   a[1].b3 = 4
        |   a[2].b3 = 4
        |   a[3].b3 = 4
        |   a[4].b3 = 4
        |   a[5].b3 = 4
        | }
        | noinline byte id(byte x) = x
        | void main () {
        |   int24 tmp1
        |   stack int24 tmp2
        |   int32 tmp3
        |   stack int32 tmp4
        |   init()
        |   a[id(0)] = b[id(0)]
        |   memory_barrier()
        |   tmp1 = b[id(0)]
        |   memory_barrier()
        |   tmp2 = b[id(0)]
        |   memory_barrier()
        |   a[id(1)] = tmp1
        |   memory_barrier()
        |   a[id(2)] = tmp2
        |   memory_barrier()
        |   tmp3 = b[id(0)]
        |   memory_barrier()
        |   tmp4 = b[id(0)]
        |   memory_barrier()
        |   a[id(3)] = tmp3
        |   memory_barrier()
        |   a[id(4)] = tmp4
        | }
      """.stripMargin) { m =>
      m.readMedium(0xc100) should equal(1)
      m.readMedium(0xc103) should equal(2)
      m.readLong(0xc000) should equal(1)
      m.readLong(0xc004) should equal(1)
      m.readLong(0xc008) should equal(1)
      m.readLong(0xc00c) should equal(1)
      m.readLong(0xc010) should equal(1)
    }
  }

  test("Various large assignments involving arrays and arithmetic conversions 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Intel8080, Cpu.Z80)(
      """
        | array(int64) a[7] @$c000
        | array(int40) b[7] @$c100
        | noinline void init() {
        |   b[0] = 1
        |   b[1] = 2
        |   a[0].b7 = 4
        |   a[1].b7 = 4
        |   a[2].b7 = 4
        |   a[3].b7 = 4
        |   a[4].b7 = 4
        |   a[5].b7 = 4
        | }
        | noinline byte id(byte x) = x
        | void main () {
        |   int64 tmp1
        |   stack int64 tmp2
        |   int40 tmp3
        |   stack int40 tmp4
        |   init()
        |   a[id(0)] = b[id(0)]
        |   memory_barrier()
        |   tmp1 = b[id(0)]
        |   memory_barrier()
        |   tmp2 = b[id(0)]
        |   memory_barrier()
        |   a[id(1)] = tmp1
        |   memory_barrier()
        |   a[id(2)] = tmp2
        |   memory_barrier()
        |   tmp3 = b[id(0)]
        |   memory_barrier()
        |   tmp4 = b[id(0)]
        |   memory_barrier()
        |   a[id(3)] = tmp3
        |   memory_barrier()
        |   a[id(4)] = tmp4
        | }
      """.stripMargin) { m =>
      m.readLong(0xc100) should equal(1) withClue "init b[0]"
      m.readLong(0xc105) should equal(2) withClue "init b[1]"
      m.readLong(0xc000) should equal(1) withClue "b0..b3 of a[0] initted from b[0]"
      m.readLong(0xc004) should equal(0) withClue "b4..b7 of a[0] initted from b[0]"
      m.readLong(0xc008) should equal(1) withClue "b0..b3 of a[1] initted from static int64"
      m.readLong(0xc00c) should equal(0) withClue "b4..b7 of a[1] initted from static int64"
      m.readLong(0xc010) should equal(1) withClue "b0..b3 of a[2] initted from stack int64"
      m.readLong(0xc014) should equal(0) withClue "b4..b7 of a[2] initted from stack int64"
      m.readLong(0xc018) should equal(1) withClue "b0..b3 of a[3] initted from static int40"
      m.readLong(0xc01c) should equal(0) withClue "b4..b7 of a[3] initted from static int40"
      m.readLong(0xc020) should equal(1) withClue "b0..b3 of a[4] initted from stack int40"
      m.readLong(0xc024) should equal(0) withClue "b4..b7 of a[4] initted from stack int40"
    }
  }


  test("Fast small array indexing") {
    EmuBenchmarkRun(
      """
        | array(word) words[10] @$c000
        | noinline byte getLo(byte i) = words[i].lo
        | noinline byte getHi(byte i) = words[i].hi
        | noinline word get(byte i) = words[i]
        | void main () {
        |   get(5)
        | }
      """.stripMargin){ m =>
    }
  }

  test("Error message test") {
    ShouldNotParse(
      """
        | const array stuff = file("", 0, 0, 0)
        | void main () {
        | }
      """.stripMargin)
    ShouldNotParse(
      """
        | const array stuff = file()
        | void main () {
        | }
      """.stripMargin)
    ShouldNotParse(
      """
        | const array stuff = file(1)
        | void main () {
        | }
      """.stripMargin)
  }

  test("Arrays of aligned structs") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Intel8080, Cpu.Z80, Cpu.Motorola6809)(
      """
        | struct s align(2) { byte x }
        | array(s) a[7]
        | word output @$c000
        | byte output2 @$c002
        | void main () {
        |   output = a[1].pointer - a[0].pointer
        |   a[f(3)].x = 5
        |   output2 = a[3].x
        | }
        | noinline byte f(byte x) = x
      """.stripMargin) { m =>
      m.readWord(0xc000) should equal(2)
      m.readByte(0xc002) should equal(5)
    }
  }

  test("Accessing large fields of structs in arrays") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos/*, Cpu.Intel8080, Cpu.Z80, Cpu.Motorola6809*/)(
      """
        | struct s { word x, word y}
        | array(s) a[7]
        | word output @$c000
        |
        | void main () {
        |    a[f(4)].y = 5
        |    a[f(4)].y = a[f(4)].y
        |    output = a[f(4)].y
        | }
        | noinline byte f(byte x) = x
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(5)
    }
  }

  test("Accessing large fields of structs in arrays 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos/*, Cpu.Intel8080, Cpu.Z80, Cpu.Motorola6809*/)(
      """
        | struct s { word x, word y}
        | array(s) a[7] @$c000
        |
        | void main () {
        |    byte t
        |    t = f(4)
        |    a[t+4].x = t << 3
        |    a[t+4].y = t ^ 6
        | }
        | noinline byte f(byte x) = x
      """.stripMargin) { m =>
      m.readWord(0xc020) should equal(32)
      m.readWord(0xc022) should equal(2)
    }
  }

  test("Arrays from file") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos)(
      """
        | const byte nine = 9
        | array dummy @$4000 = file("src/test/resources/binarydata", nine, 3*3)
        |
        | void main () {
        |
        | }
        | noinline byte f(byte x) = x
      """.stripMargin) { m =>
      m.readByte(0x4000) should equal('1'.toInt)
      m.readByte(0x4008) should equal('9'.toInt)
    }
  }
}
