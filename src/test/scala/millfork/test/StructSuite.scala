package millfork.test

import millfork.Cpu
import millfork.test.emu.EmuUnoptimizedCrossPlatformRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class StructSuite extends FunSuite with Matchers {

  test("Basic struct support") {
    // TODO: 8080 has broken stack operations, fix and uncomment!
    EmuUnoptimizedCrossPlatformRun(Cpu.StrictMos, Cpu.Z80/*, Cpu.Intel8080*/)("""
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
    EmuUnoptimizedCrossPlatformRun(Cpu.StrictMos, Cpu.Intel8080)("""
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
        |   output.p[0] = 6
        |   output.i.x.lo = 55
        |   output.i.x.hi = s.p.offset
        |   output.i.y = 777
        | }
      """.stripMargin) { m =>
      m.readWord(0xc000) should equal(0x506)
      m.readByte(0xc002) should equal(5)
      m.readWord(0xc003) should equal(0xc000)
      m.readByte(0xc005) should equal(55)
      m.readByte(0xc006) should equal(3)
      m.readWord(0xc007) should equal(777)
    }
  }
}
