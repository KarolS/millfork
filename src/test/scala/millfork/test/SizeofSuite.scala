package millfork.test
import millfork.Cpu
import millfork.test.emu.{EmuBenchmarkRun, EmuOptimizedCmosRun, EmuOptimizedRun, EmuUnoptimizedCrossPlatformRun}
import org.scalatest.{AppendedClues, FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class SizeofSuite extends FunSuite with Matchers with AppendedClues {

  test("Basic sizeof test") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        | const word sizeofbyte = sizeof(byte)
        | array output [6] @$c000
        | void main () {
        |   byte a
        |   word b
        |   output[0] = sizeofbyte.lo
        |   output[1] = lo(sizeof(a))
        |   output[2] = lo(sizeof(word))
        |   output[3] = lo(sizeof(b))
        |   output[4] = lo(sizeof(output[1]))
        |   output[5] = lo(sizeof(long))
        | }
      """.stripMargin){m =>
      m.readByte(0xc000) should equal(1)
      m.readByte(0xc001) should equal(1)
      m.readByte(0xc002) should equal(2)
      m.readByte(0xc003) should equal(2)
      m.readByte(0xc004) should equal(1)
      m.readByte(0xc005) should equal(4)
    }
  }
}
