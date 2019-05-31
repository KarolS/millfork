package millfork.test

import millfork.Cpu
import millfork.test.emu.EmuUnoptimizedCrossPlatformRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class DerefSuite extends FunSuite with Matchers {
  test("Basic deref test") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086)(
      """
        |
        | byte output @$c000
        | word output2 @$c010
        | byte crash @$bfff
        | void main() {
        |   pointer p
        |   p = id(output.addr)
        |   ¥(p) = 13
        |   if (¥(p) != 13) { crash = 1 }
        |   p = id(output2.addr)
        |   ¥¥(p) = 600
        |   if (¥¥(p) != 600) { crash = 2 }
        | }
        |
        | noinline pointer id(pointer x) { return x }
      """.stripMargin){m =>
      m.readByte(0xc000) should equal (13)
      m.readWord(0xc010) should equal (600)
    }
  }

  test("Byte arithmetic deref test") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086)(
      """
        |
        | byte output1 @$c000
        | byte output2 @$c001
        | byte crash @$bfff
        | void main() {
        |   pointer p
        |   pointer q
        |   p = id(output1.addr)
        |   q = id(output2.addr)
        |   ¥(p) = 1
        |   ¥(q) = 3
        |   ¥(p) += 1
        |   ¥(p) |= 1
        |   ¥(q) = ¥(p) + ¥(q)
        | }
        |
        | noinline pointer id(pointer x) = x
      """.stripMargin){m =>
      m.readByte(0xc000) should equal (3)
      m.readByte(0xc001) should equal (6)
    }
  }

  test("Word arithmetic deref test") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086)(
      """
        |
        | word output1  @$c000
        | word output2  @$c002
        | byte crash    @$bfff
        | void main () {
        | pointer p
        | pointer q
        | p = id(output1.addr)
        | q = id(output2.addr)
        | ¥¥(p) = 100
        |   ¥¥(q) = 300
        |   ¥¥(p) = ¥¥(p) + 100
        |   ¥¥(p) = ¥¥(p) ^ (300 ^ 200)
        |   ¥¥(q) = ¥¥(p) + ¥¥(q)
        | }
        |
        | noinline pointer id(pointer x) { return x }
      """.stripMargin) { m =>
      m.readWord(0xc000) should equal(300)
      m.readWord(0xc002) should equal(600)
    }
  }
}
