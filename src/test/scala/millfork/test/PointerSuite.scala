package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuCrossPlatformBenchmarkRun, EmuUnoptimizedCrossPlatformRun, EmuUnoptimizedRun, ShouldNotCompile}
import org.scalatest.{AppendedClues, FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class PointerSuite extends FunSuite with Matchers with AppendedClues {

  test("Pointers outside zeropage") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Sixteen, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
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
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Sixteen, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
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
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Sixteen, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
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
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Sixteen, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
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
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086, Cpu.Motorola6809)(
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
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086, Cpu.Motorola6809)(
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
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086, Cpu.Motorola6809)(
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
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086, Cpu.Motorola6809)(
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
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
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
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
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
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
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
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Motorola6809) (
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
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Motorola6809) (
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
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Motorola6809) (
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

  test("Modifying a word via a pointer") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        |word output @$c000
        |void main () {
        |  byte constant
        |  constant = $50
        |  output = $850
        |
        |  pointer.word value_pointer
        |  value_pointer = output.pointer
        |  value_pointer[0] += constant
        |  value_pointer[0] |= constant
        |  value_pointer[0] <<= f(2)
        |  value_pointer[0] >>= f(2)
        |}
        |noinline byte f(byte i) = i
        |
      """.stripMargin
    ) { m =>
      m.readWord(0xc000) should equal(0x850.+(0x50).|(0x50))
    }
  }

  test("Modifying a word via a pointer 2") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80) (
      """
        |import zp_reg
        |word output2 @$c002
        |array(word) tmp[6] @$c100
        |void main () {
        |  byte constant
        |  constant = $50
        |  output2 = 0
        |  pointer.word value_pointer
        |  value_pointer = output2.pointer
        |  value_pointer[0] $+= constant
        |  tmp[0] = output2
        |  value_pointer[0] $+= constant
        |  tmp[1] = output2
        |  value_pointer[0] $-= constant
        |  tmp[2] = output2
        |  value_pointer[0] <<= f(1)
        |  tmp[3] = output2
        |  value_pointer[0] *= f(1)
        |  tmp[4] = output2
        |  value_pointer[0] /= f(1)
        |  tmp[5] = output2
        |}
        |noinline byte f(byte i) = i
        |
      """.stripMargin
    ){ m =>
      println(m.readWord(0xc100).toHexString)
      println(m.readWord(0xc102).toHexString)
      println(m.readWord(0xc104).toHexString)
      println(m.readWord(0xc106).toHexString)
      println(m.readWord(0xc108).toHexString)
      println(m.readWord(0xc10a).toHexString)
      m.readWord(0xc002) should equal(0xA0)

    }
  }

  test("Modifying a word via a pointer 3") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809) (
      """
        |struct entity_t {
        |  word x, byte y,
        |  byte frame_i, byte frame_tick,
        |  int24 padding // TODO: remove padding after 6809 gets multiplication support
        |}
        |array(entity_t) enemy[7] @$c100
        |
        |void main() {
        | enemy[0].x = 0x3ff
        | byte i
        | i = f(0)
        | enemy[i].x += 1
        |}
        |noinline byte f(byte x) = x
        |
        |""".stripMargin
    ){ m =>
      m.readWord(0xc100) should equal(0x400)
    }
  }

  test("Pointers should remain at zero page if used as pointers in assembly") {
    val m = EmuUnoptimizedRun(
      """
        | word output @$c000
        | pointer p
        | volatile pointer q1
        | volatile pointer q2
        | volatile pointer q3
        | volatile pointer q4
        | volatile pointer q5
        | volatile pointer q6
        | volatile pointer q7
        | volatile pointer q8
        | array arr [250] @$0
        | void main () {
        |   asm {
        |     lda (p),y
        |   }
        |   output = p.addr
        |   q1 = q2 + q3 + q4 + q5 + q6 + q7 + q8
        | }
        |""".stripMargin)
    m.readWord(0xc000) should be <(256)

  }

  test ("Pointers should have priority when allocating to the zeropage") {
    val m = EmuUnoptimizedRun(
      """
        | word output1 @$c000
        | word output2 @$c000
        |
        | array arr [252]
        |
        | noinline word f(pointer p, pointer q) {
        |   output1 = p.addr
        |   output2 = q.addr
        |   return p + q
        | }
        |
        | void main () {
        |   f(0,1)
        | }
        |""".stripMargin)
    m.readWord(0xc000) should be <(256)
    m.readWord(0xc002) should be <(256)
  }

  test("Raw pointers should work") {
    val m = EmuUnoptimizedRun(
      """
        |
        | array(word) arr [252] @$c000
        |
        | void main () {
        |   pointer.word p
        |   p = arr.pointer
        |   p.raw += sizeof(word)
        |   p[0] = 4
        | }
        |""".stripMargin)
    m.readWord(0xc002) should equal(4)
  }
}
