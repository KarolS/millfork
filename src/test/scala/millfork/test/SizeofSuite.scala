package millfork.test
import millfork.Cpu
import millfork.test.emu.{EmuBenchmarkRun, EmuOptimizedCmosRun, EmuOptimizedRun, EmuUnoptimizedCrossPlatformRun}
import org.scalatest.{AppendedClues, FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class SizeofSuite extends FunSuite with Matchers with AppendedClues {

  test("Basic sizeof test") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80)(
      """
        | const byte sizeofbyte = sizeof(byte)
        | array output [6] @$c000
        | void main () {
        |   byte a
        |   word b
        |   output[0] = sizeofbyte
        |   output[1] = sizeof(a)
        |   output[2] = sizeof(word)
        |   output[3] = sizeof(b)
        |   output[4] = sizeof(output[1])
        |   output[5] = sizeof(long)
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
