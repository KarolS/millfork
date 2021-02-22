package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuCrossPlatformBenchmarkRun, EmuUnoptimizedCrossPlatformRun, ShouldNotCompile, ShouldNotParse}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class StructSuite extends FunSuite with Matchers {

  test("Basic struct support") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Intel8086, Cpu.Motorola6809)("""
        | struct point {
        |   byte x
        |   byte y
        |   byte z
        |   byte colour
        | }
        | point output @$c000
        | void main () {
        |   stack point p
        |   p = f()
        |   output = p
        | }
        | point f() {
        |   point x
        |   x.x = 77
        |   x.y = 88
        |   x.z = 99
        |   x.colour = 14
        |   return x
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(77)
      m.readByte(0xc001) should equal(88)
      m.readByte(0xc002) should equal(99)
      m.readByte(0xc003) should equal(14)
    }
  }

  test("Nested structs") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Intel8080, Cpu.Intel8086, Cpu.Motorola6809)("""
        | struct inner { word x, word y }
        | struct s {
        |   word w
        |   byte b
        |   pointer p
        |   inner i
        | }
        | s output @$c000
        | void main () {
        |   output.b = 5
        |   output.w.hi = output.b
        |   output.p = output.w.addr
        |   #if BIG_ENDIAN
        |   output.p[1] = 6
        |   #else
        |   output.p[0] = 6
        |   #endif
        |   output.i.x.lo = 55
        |   output.i.x.hi = s.p.offset
        |   output.i.y = 777
        | }
      """.stripMargin) { m =>
      m.readWord(0xc000) should equal(0x506)
      m.readByte(0xc002) should equal(5)
      m.readWord(0xc003) should equal(0xc000)
      m.readWord(0xc005) should equal(3 * 256 + 55)
      m.readWord(0xc007) should equal(777)
    }
  }

  test("Basic union support") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Intel8080, Cpu.Intel8086, Cpu.Motorola6809)("""
        | struct point { byte x, byte y }
        | union point_or_word { point p, word w }
        | word output @$c000
        | void main () {
        |   point_or_word u
        |   u.p.x = 1
        |   u.p.y = 2
        |   output = u.w
        | }
      """.stripMargin) { m =>
      if (m.isBigEndian) {
        m.readWord(0xc000) should equal(0x102)
      } else {
        m.readWord(0xc000) should equal(0x201)
      }
    }
  }

  test("Optimize struct modifications") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086, Cpu.Motorola6809)("""
        | struct point { byte x, byte y }
        | enum direction { none, right }
        | direction last_direction @$c400
        | noinline point move_right(point p) {
        |   last_direction = right
        |   p.x += 1
        |   return p
        | }
        | byte output @$c000
        | void main () {
        |   point p
        |   p.x = 1
        |   p.y = 2
        |   p = move_right(p)
        |   output = p.x
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(2)
      m.readByte(0xc400) should equal(1)
    }
  }

  test("Struct literals") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086, Cpu.Motorola6809)("""
        | struct point { byte x, byte y }
        | const point origin = point(1,2)
        | noinline point move_right(point p) {
        |   p.x += 1
        |   return p
        | }
        | byte output @$c000
        | void main () {
        |   point p
        |   p = move_right(origin)
        |   p = move_right(point(1,2))
        |   output = p.x
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(2)
    }
  }

  test("Struct literals 2") {
    val code = """
        | struct point { word x, word y }
        | const point origin = point(6, 8)
        | noinline point move_right(point p) {
        |   p.x += 1
        |   return p
        | }
        | noinline point move_up(point p) {
        |   p.y += 1
        |   return p
        | }
        | word outputX @$c000
        | word outputY @$c002
        | void main () {
        |   point p
        |   p = point(0,0)
        |   p = move_up(point(0,0))
        |   p = origin
        |   p = move_up(p) // ↑
        |   p = move_right(p) // →
        |   p = move_right(p) // →
        |   p = move_up(p) // ↑
        |   p = move_right(p) // →
        |   p = move_right(p) // →
        |   p = move_up(p) // ↑
        |   p = move_up(p) // ↑
        |   p = move_up(p) // ↑
        |   p = move_right(p) // →
        |   p = move_up(p) // ↑
        |   p = move_up(p) // ↑
        |   p = move_up(p) // ↑
        |   outputX = p.x
        |   outputY = p.y
        | }
      """.stripMargin
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086, Cpu.Motorola6809)(code){ m =>
      m.readWord(0xc000) should equal(code.count(_ == '→') + 6)
      m.readWord(0xc002) should equal(code.count(_ == '↑') + 8)
    }
  }


  test("Boolean fields") {
    val code =
      """
        |struct Sprite {
        |    byte size,
        |    byte size2,
        |    byte size3,
        |    byte size4,
        |    bool x1,
        |    bool x,
        |    bool y,
        |    bool z
        |}
        |
        |array(Sprite) sprites [20] @ $c000
        |
        |void main() {
        |    sprites[0].x = true
        |}
        |""".stripMargin
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086, Cpu.Motorola6809)(code){ m =>
      m.readByte(0xc005) should equal(1)
    }
  }

  test("Field alignment") {
    val code =
      """
        |
        |struct alignedbyte align(2) { byte x }
        |struct S {
        |    alignedbyte a,
        |    alignedbyte b
        |}
        |
        |byte offset_a @$c000
        |byte offset_b @$c001
        |
        |void main() {
        |    offset_a = S.a.offset
        |    offset_b = S.b.offset
        |}
        |""".stripMargin
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086, Cpu.Motorola6809)(code){ m =>
      m.readByte(0xc000) should equal(0)
      m.readByte(0xc001) should equal(2)
    }
  }

  test("Array struct fields") {
    val code =
      """
        |import zp_reg
        |struct S {
        |    array tmp[8]
        |}
        |
        |S output @$c000
        |
        |array(S) outputAlias [1] @$c000
        |
        |noinline byte id(byte x) = x
        |noinline void dontOptimize(pointer.S dummy) {}
        |void main() {
        |    output.tmp[0] = 1
        |    output.tmp[4] = 4
        |    pointer.S p
        |    p = output.pointer
        |    p->tmp[1] = 77
        |    outputAlias[0].tmp[id(3)] = 3
        |    outputAlias[id(0)].tmp[5] = 55
        |}
        |""".stripMargin
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086, Cpu.Motorola6809)(code){ m =>
      m.readByte(0xc000) should equal(1)
      m.readByte(0xc001) should equal(77)
      m.readByte(0xc003) should equal(3)
      m.readByte(0xc004) should equal(4)
      m.readByte(0xc005) should equal(55)
    }
  }

  test("Struct layout with array fields") {
    val code =
      """
        |struct S {
        |    array (word) a[4]
        |    byte x
        |}
        |
        |array outputs [10] @$c000
        |
        |void main() {
        |    S tmp
        |    outputs[1] = sizeof(S)
        |    outputs[2] = S.a.offset
        |    outputs[4] = S.x.offset
        |    outputs[5] = lo(tmp.a[1].addr - tmp.a[0].addr)
        |    outputs[6] = lo(tmp.x.addr - tmp.a.addr)
        |}
        |""".stripMargin
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086, Cpu.Motorola6809)(code){ m =>
      m.readByte(0xc001) should equal(9)
      m.readByte(0xc002) should equal(0)
      m.readByte(0xc004) should equal(8)
      m.readByte(0xc005) should equal(2)
      m.readByte(0xc006) should equal(8)
    }
  }

  test("Structs with enum-indexed array fields") {
    val code =
      """
        | enum Suit {
        |   Hearts, Diamonds, Clubs, Spades
        | }
        | struct Deck {
        |   array(byte) count[Suit]
        | }
        |
        | array output[5] @$c000
        | void main() {
        |   Deck d
        |   output[0] = d.count.length
        |   output[1] = sizeof(Deck)
        |   d.count[Diamonds] = 5
        |   d.count[Clubs] = d.count[Diamonds]
        |   output[2] = d.count[Clubs]
        | }
        |""".stripMargin
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086, Cpu.Motorola6809)(code){ m =>
      m.readByte(0xc000) should equal(4)
      m.readByte(0xc001) should equal(4)
      m.readByte(0xc002) should equal(5)
    }
  }

  test("Structs with array fields – invalid uses") {
    ShouldNotCompile(
      """
        | struct S {
        |   array a[4]
        | }
        |
        | void main() {
        |   stack S s
        | }
        |""".stripMargin)
    ShouldNotCompile(
      """
        | struct S {
        |   array a[4]
        | }
        | void main() {
        |   S s
        |   s = S(4)
        | }
        |""".stripMargin)
  }

  test("Structs with array fields – invalid syntax") {
    ShouldNotParse(
      """
        | struct S {
        |   byte a[4]
        | }
        |""".stripMargin)
  }
}
