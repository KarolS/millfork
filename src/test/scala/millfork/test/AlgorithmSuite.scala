package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuBenchmarkRun, EmuCrossPlatformBenchmarkRun, EmuUltraBenchmarkRun}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class AlgorithmSuite extends FunSuite with Matchers {

  test("RLE decoding") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086)(
      """
        | array output [4000] @$c000
        | array input = [
        | 2, 2,
        | $83, 6, 7, 8,
        | 3, 1,
        | 0]
        | void main () {
        |   rle_unpack(input, output)
        | }
        | void rle_unpack(pointer source, pointer target) {
        |   byte action
        |   byte i
        |   byte b
        |
        |   // currently the variable inliner is too dumb to notice
        |   // that `i` can be put safely into Y if used in multiple loops,
        |   // so another variable is created so it can be inlined too
        |   // TODO:
        |   // make the variable inliner smarter so a single variable can be reused
        |   byte j
        |
        |   while true {
        |     action = source[0]
        |     if action == 0 { return }
        |     if action & $80 != 0 {
        |       source += 1
        |       action &= $7f
        |       for i, 0, paralleluntil, action {
        |         target[i] = source[i]
        |       }
        |       target += action
        |       source += action
        |     } else {
        |       b = source[1]
        |       for j, 0, paralleluntil, action {
        |         target[j] = b
        |       }
        |       target += action
        |       source += 2
        |     }
        |   }
        | }
      """.stripMargin) {m =>
      m.readByte(0xc000) should equal(2)
      m.readByte(0xc001) should equal(2)
      m.readByte(0xc002) should equal(6)
      m.readByte(0xc003) should equal(7)
      m.readByte(0xc004) should equal(8)
      m.readByte(0xc005) should equal(1)
      m.readByte(0xc006) should equal(1)
      m.readByte(0xc007) should equal(1)
    }
  }
}
