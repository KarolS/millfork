package millfork.test

import millfork.Cpu
import millfork.test.emu.EmuUnoptimizedCrossPlatformRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class WarningSuite extends FunSuite with Matchers {

  test("Various warnings") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        | void putstrz(pointer p) {}
        | byte output@0xc000
        | byte main (byte x) {
        |  putstrz("a")
        |  byte a
        |  a
        |  5
        |  a *= 0
        |  a <<= 0
        |  output = 4
        | }
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(4)
    }
  }
  test("Loop over non-volatile variables") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        | byte flag
        | volatile byte vflag
        | void main () {
        |  flag = 0
        |  vflag = 0
        |  while (flag != 0) {}
        |  while (vflag != 0) {}
        | }
      """.stripMargin) { m =>
    }
  }

  test("Warn about byte to pointer comparisons") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        | volatile byte b @$cfff
        | noinline void f(){}
        | void main () {
        |  if b == "h" { f() }
        |  if b == b.pointer { f() }
        |  if b == b.addr { f() }
        | }
      """.stripMargin) { m =>
    }
  }

  test("Warn about unintended byte overflow") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos)(
      """
        | import zp_reg
        | const word screenOffset = (10*40)+5
        | noinline void func(byte x, byte y) {
        |     word screenOffset
        |     screenOffset = (x*40) + y
        | }
        | noinline word getNESScreenOffset(byte x, byte y) {
        |     word temp
        |     temp = (y << 5) +x
        | }
        | noinline word getSomeFunc(byte x, byte y, byte z) {
        |    word temp
        |    temp = ((x + z) << 2) + (y << 5)
        |    temp = byte((x + z) << 2) + (y << 5)
        | }
        |
        | noinline byte someFunc(byte x, byte y) {
        |    return (x*y)-24
        | }
        | void main() {
        |    func(0,0)
        |    getNESScreenOffset(0,0)
        |    getSomeFunc(0,screenOffset.lo,5)
        |    someFunc(0,0)
        | }
      """.stripMargin) { m =>
    }
  }
}
