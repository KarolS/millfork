package millfork.test

import millfork.Cpu
import millfork.test.emu.EmuCrossPlatformBenchmarkRun
import org.scalatest.{AppendedClues, FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class MemBulkSuite extends FunSuite with Matchers with AppendedClues {

  test("Memcpy should work fine") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80)(
      """
        |
        |array input @$c000 = [5,89,6,1,8,6,87,52,6,45,8,52,8,6,14,89]
        |array output [input.length] @$c100
        |byte size @$cfff
        |void main() {
        | word i
        | for i,0,paralleluntil,input.length { output[i] = input[i] }
        | size = input.length
        |}
        |""".stripMargin) { m =>
      val size = m.readByte(0xcfff)
      size should be >(0)
      for (i <- 0 until size) {
        m.readByte(0xc000 + i) should equal(m.readByte(0xc100 + i)) withClue s"[$i]"
      }
    }
  }

  test("Correctly increment array elements") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80)(
      """
        |array output [$100] @$c000
        |void main() {
        | word i
        | for i,0,paralleluntil,$100 { output[i] = 0 }
        | for i,0,paralleluntil,$100 { output[i] += i.lo }
        |}
        |""".stripMargin) { m =>
      for (i <- 0 until 0x100) {
        m.readByte(0xc000 + i) should equal(i) withClue s"[$i]"
      }
    }
  }
}
