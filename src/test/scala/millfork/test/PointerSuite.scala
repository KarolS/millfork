package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuCrossPlatformBenchmarkRun, EmuUnoptimizedCrossPlatformRun, ShouldNotCompile}
import org.scalatest.{AppendedClues, FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class PointerSuite extends FunSuite with Matchers with AppendedClues {

  test("Pointers outside zeropage") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Sixteen, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)(
      """
        | pointer p @$c004
        | array output[2] @$c000
        | void main() {
        |   p = output.addr
        |   output[0] = 45
        |   p[1] = p[0]
        | }
      """.stripMargin) { m =>
      m.readByte(0xc001) should equal(45)
    }
  }

  test("Pointers on stack") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Sixteen, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)(
      """
        | array output[2] @$c000
        | void main() {
        |   stack pointer p
        |   p = output.addr
        |   output[0] = 45
        |   p[1] = p[0]
        | }
      """.stripMargin) { m =>
      m.readByte(0xc001) should equal(45)
    }
  }

  test("Typed byte-targeting pointers") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Sixteen, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)(
      """
        | enum e {}
        | array(e) output [3] @$c000
        | void main() {
        |   pointer.e p
        |   e x
        |   p = output.pointer
        |   x = p[0]
        |   p[0] = e(14)
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(14)
    }
  }

  test("Typed word-targeting pointers") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Sixteen, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)(
      """
        | word output @$c000
        | void main() {
        |   pointer.word p
        |   word x
        |   p = output.pointer
        |   x = p[0]
        |   p[0] = 1589
        | }
      """.stripMargin) { m =>
      m.readWord(0xc000) should equal(1589)
    }
  }

  test("Struct pointers") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086)(
      """
        | struct point { word x, word y }
        | struct pointlist { point head, pointer.pointlist tail }
        | array output [5] @$c000
        | array heap[$300] @$c400
        |
        | pointer.pointlist this
        | pointer heapEnd
        |
        | pointer.pointlist alloc() {
        |   pointer.pointlist result
        |   result = pointer.pointlist(heapEnd)
        |   heapEnd += sizeof(pointlist)
        |   return result
        | }
        |
        | void prepend(point p) {
        |   pointer.pointlist new
        |   new = alloc()
        |   // can't copy things larger than 2 bytes right now:
        |   new->head.x = p.x
        |   new->head.y = p.y
        |   new->tail = this
        |   this = new
        | }
        |
        | void main() {
        |   heapEnd = heap.addr
        |   this = nullptr
        |   point tmp
        |   tmp.x = 3
        |   tmp.y = 3
        |   prepend(tmp)
        |   tmp.x = 4
        |   prepend(tmp)
        |   tmp.x = 5
        |   prepend(tmp)
        |
        |   pointer.pointlist cursor
        |   byte index
        |   index = 0
        |   cursor = this
        |   while cursor != 0 {
        |       output[index] = cursor->head.x.lo
        |       index += 1
        |       cursor = cursor->tail
        |   }
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(5)
      m.readByte(0xc001) should equal(4)
      m.readByte(0xc002) should equal(3)
    }
  }

  test("Pointer optimization") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086)(
      """
        | struct s { word a, byte b }
        |
        | s global_s
        | byte output @$c000
        | word output_sink @$c005
        |
        | noinline pointer.s init() {
        |   global_s.b = 44
        |   return global_s.pointer
        | }
        | void main() {
        |   pointer.s p
        |   output_sink = p.addr
        |   p = init()
        |   output = p->b
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(44)
    }
  }

  test("nullptr") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086)(
      """
        | void main() {
        |   pointer.word pw
        |   pointer p
        |   word w
        |   pw = nullptr
        |   p = nullptr
        |   w = word(nullptr)
        | }
      """.stripMargin) { m =>

    }
  }

  test("Complex pointers") {
    // TODO: optimize it when inlined
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086)(
      """
        | array output[3] @$c000
        | struct s {
        |   pointer p
        | }
        | s tmp
        | pointer.s tmpptr
        | pointer.pointer.s get() {
        |   tmp.p = output.addr
        |   tmpptr = tmp.pointer
        |   return tmpptr.pointer
        | }
        | void main() {
        |   get()[0]->p[0] = 5
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(5)
    }
  }

  test("Indexing returned pointers") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086)(
      """
        | array output[10] @$c000
        | pointer get() = output.addr
        | void main() {
        |   byte i
        |   for i,0,paralleluntil,10 {
        |     get()[i] = 42
        |   }
        | }
      """.stripMargin) { m =>
      for(i <- 0xc000 until 0xc00a) {
        m.readByte(i) should equal(42) withClue i
      }
    }
  }

  test("Pointers and arrays") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80)(
      """
        | struct P {
        |    word i
        |    byte c
        |    byte d
        | }
        |
        | array a [sizeof(P)*8]
        |
        | noinline byte f(byte i) {
        |     return pointer.P(a.addr + sizeof(P)*i)->c
        | }
        |
        | void main() {
        |     f(6)
        | }
      """.stripMargin) { m =>
    }
  }

  test("Pointers and arrays to large elements") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80)(
      """
        | struct P {
        |    word i
        |    byte c
        |    byte d
        | }
        |
        | array(P) a [8]
        |
        | noinline byte f(byte i) {
        |     return a[i].c
        | }
        |
        | void main() {
        |     f(6)
        | }
      """.stripMargin) { m =>
    }
  }

  test("Page crossing with arrays of large elements") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80)(
      """
        | import zp_reg
        | struct P {
        |    word i
        |    byte c
        |    byte d
        | }
        |
        | array(P) a [80] @$c080
        | array(P) b [500] @$c280
        |
        | noinline void fill(word i) {
        |     if i < 80 { a[lo(i)].i = i }
        |     b[i].i = i
        | }
        |
        | void main() {
        |     word i
        |     for i,0,until,500 { fill(i) }
        | }
      """.stripMargin) { m =>
      for (i <- 0 until 80) {
        m.readWord(0xc080 + 4*i) should equal(i) withClue s"a[$i]"
      }
      for (i <- 0 until 500) {
        m.readWord(0xc280 + 4*i) should equal(i) withClue s"b[$i]"
      }
    }
  }

  test("Word pointers") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080) (
      """
        |pointer.word p
        |word output @$c000
        |void main () {
        | word tmp
        | p = tmp.pointer
        | tmp = $203
        | output = p[0]
        |}
      """.stripMargin
    ){ m =>
      m.readWord(0xc000) should equal(0x203)
    }
  }

  test("Word pointers and byte-to-word promotions") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080) (
      """
        |pointer.word p
        |word output @$c000
        |void main () {
        | p = output.pointer
        | p[0] = f()
        |}
        |noinline word g() = $100
        |noinline byte f() {
        |  g()
        |  return 5
        |}
      """.stripMargin
    ){ m =>
      m.readWord(0xc000) should equal(5)
    }
  }

  test("Fast pointer indexing") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos) (
      """
        |pointer.word p
        |array(word) input [6]
        |word output @$c000
        |void main () {
        | input[3] = 555
        | output = f(input.pointer)
        |}
        |noinline word f(pointer.word p) = p[3]
      """.stripMargin
    ){ m =>
      m.readWord(0xc000) should equal(555)
    }
  }
}
