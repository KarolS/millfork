package millfork.test

import millfork.Cpu
import millfork.test.emu.EmuUnoptimizedCrossPlatformRun
import org.scalatest.{FunSuite, Matchers}


/**
  * @author Karol Stasiak
  */
class AlignmentSuite extends FunSuite with Matchers {

  test("Alignment") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | array output [5] @$c000
        | void main () {
        |   output[0] = lo(f.addr) & 0x3f
        |   output[1] = dump.addr.lo & 0x1f
        | }
        | void f() align(64){
        | }
        | array dump[4] align(32)
      """.stripMargin) {m =>
      m.readByte(0xc000) should equal(0)
      m.readByte(0xc001) should equal(0)
    }
  }
}
