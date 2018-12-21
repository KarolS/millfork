package millfork.test

import millfork.Cpu
import millfork.test.emu.EmuCrossPlatformBenchmarkRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class StatementOptimizationSuite extends FunSuite with Matchers {

  test("Statement optimization  1") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Intel8080, Cpu.Sharp)(
      """
        | array output[10] @$c000
        | void main() {
        |   byte i
        |   for i,0,paralleluntil,output.length {
        |     output[i] = f(i)
        |   }
        | }
        | noinline byte f(byte a) {
        |   byte b
        |   byte c
        |   byte d
        |   byte e
        |   byte f
        |   b = a
        |   c = 5
        |   d = c
        |   b += c
        |   if a > 4 {
        |     b += 1
        |   } else {
        |     b += 2
        |   }
        |   e = 4
        |   f = e
        |   while a > 0 {
        |     d += f
        |     a -= 1
        |   }
        |   return b + d
        | }
      """.stripMargin) { m=>
      m.readByte(0xc000) should equal(12)
      m.readByte(0xc001) should equal(17)
      m.readByte(0xc002) should equal(22)
      m.readByte(0xc003) should equal(27)
      m.readByte(0xc004) should equal(32)
      m.readByte(0xc005) should equal(36)
      m.readByte(0xc006) should equal(41)
      m.readByte(0xc007) should equal(46)
      m.readByte(0xc008) should equal(51)
      m.readByte(0xc009) should equal(56)
    }
  }


  test("Stdlib optimization 1") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Intel8080, Cpu.Sharp)(
      """
        | import stdio
        | byte output @$c000
        | void main() {
        |   output = strzlen("test"z)
        |   putstrz(""z)
        |   putstrz("a"z)
        |   putstrz("bc"z)
        |   putstrz("def"z)
        | }
      """.stripMargin
    ) { m =>
      m.readByte(0xc000) should equal(4)
    }
  }
}
