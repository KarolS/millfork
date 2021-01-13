package millfork.test

import millfork.Cpu
import millfork.test.emu.EmuUnoptimizedCrossPlatformRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class SegmentSuite extends FunSuite with Matchers {
  test("Segments") {
    val source =
      """
        | segment(second) byte a1
        | segment(third) byte a2
        | segment(second) {
        |   byte a3
        |   segment(third) byte a4
        | }
        | byte a5
        | byte output @$c000
        | volatile byte output2 @$c001
        | volatile byte output3 @$c002
        | void main() {
        |   output = 0
        |   if a1.addr.hi & $e0 == $80 { output += 1 }
        |   if a2.addr.hi & $e0 == $a0 { output += 1 }
        |   if a3.addr.hi & $e0 == $80 { output += 1 }
        |   if a4.addr.hi & $e0 == $a0 { output += 1 }
        |   if a5.addr.hi & $e0 == $00 { output += 1 }
        |   output2 = a1.segment.bank ^ main.segment.bank
        |   output2 ^= segment.second.bank ^ segment.default.bank
        |   output3 = lo(main.segment.start)
        | }
      """.stripMargin
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(source) { m =>
      m.readByte(0xc000) should equal(source.count(_ == '+'))
      m.readByte(0xc001) should equal(0)
    }
  }
}
