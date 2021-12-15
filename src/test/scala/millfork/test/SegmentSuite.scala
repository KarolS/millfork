package millfork.test

import millfork.Cpu
import millfork.test.emu.EmuCrossPlatformBenchmarkRun
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
        | volatile pointer output4 @$c006
        | volatile pointer output5 @$c008
        | volatile pointer output6 @$c00a
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
        |   output4 = segment.default.start
        |   output5 = segment.default.datastart
        |   output6 = segment.default.heapstart
        | }
      """.stripMargin
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(source) { m =>
      m.readByte(0xc000) should equal(source.count(_ == '+'))
      m.readByte(0xc001) should equal(0)
      m.readWord(0xc006) should equal(0x200)
      m.readWord(0xc008) should be >(0x200)
      m.readWord(0xc00a) should be >(0x200)
    }
  }
}
