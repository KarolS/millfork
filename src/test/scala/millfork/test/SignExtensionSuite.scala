package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuCrossPlatformBenchmarkRun, EmuUnoptimizedCrossPlatformRun, ShouldNotCompile}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class SignExtensionSuite extends FunSuite with Matchers {

  test("Sbyte to Word") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
        | word output @$c000
        | void main () {
        |   sbyte b
        |   b = -2
        |   output = b
        | }
      """.stripMargin){m =>
      m.readWord(0xc000) should equal(0xfffe)
    }
  }
  test("Sbyte to Word 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
        | word output @$c000
        | void main () {
        |   output = b()
        | }
        | sbyte b() {
        |   return -2
        | }
      """.stripMargin){m => m.readWord(0xc000) should equal(0xfffe)}
  }
  test("Sbyte to Long") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
        | long output @$c000
        | void main () {
        |   output = 421
        |   output += b()
        | }
        | sbyte b() {
        |   return -1
        | }
      """.stripMargin){m => m.readLong(0xc000) should equal(420)}
  }

  test("Optimize pointless sign extension") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
        | array output [10] @$c000
        | word w
        | void main () {
        |   byte i
        |   sbyte b
        |   w = 435
        |   b = five()
        |   b &= $7f
        |   for i,0,paralleluntil,output.length {
        |     output[i] = i
        |   }
        |   w += b
        |   output[0] = w.lo
        |   output[1] = w.hi
        | }
        | sbyte five() {
        |   return 5
        | }
      """.stripMargin){m =>
      m.readWord(0xc001, 0xc000) should equal(440)
    }
  }

  test("Byte to Word") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)("""
        | word output @$c000
        | void main () {
        |   sbyte b
        |   b = -2
        |   memory_barrier()
        |   output = byte(b)
        | }
      """.stripMargin){m =>
      m.readWord(0xc000) should equal(0x00fe)
    }
  }

  test("Byte to Word 2") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)("""
        | word output @$c000
        | void main () {
        |   sbyte b
        |   b = -2
        |   memory_barrier()
        |   output = word(byte(b))
        | }
      """.stripMargin){m =>
      m.readWord(0xc000) should equal(0x00fe)
    }
  }

  test("Returning sbyte as word") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)("""
        | word output @$c000
        | void main () {
        |   output = f($f4)
        | }
        | noinline word f(byte x) = sbyte(x)
      """.stripMargin){m =>
      m.readWord(0xc000) should equal(0xfff4)
    }
  }

  test("Check multilayered conversions of signed and unsigned types") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        |long output @$c000
        |noinline sbyte f() = $FE
        |void main() {
        |  // should yield $0000fffe
        |  output = word(f())
        |}
        |""".stripMargin) { m =>
      m.readLong(0xc000) should equal(0xfffe)
    }
  }

  test("The silliest thing") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        |word output @$c000
        |word output2 @$c002
        |void main() {
        |  output = -3
        |  output2 = -30000
        |}
        |""".stripMargin) { m =>
      m.readWord(0xc000) should equal(0xfffd)
      m.readWord(0xc002).toShort.toInt should equal(-30000)
    }
  }

  test("Signed16 to int32 extension") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        |int32 output @$c000
        |
        |void main() {
        |  static volatile signed16 tmp
        |  tmp = -3
        |  memory_barrier()
        |  output = tmp
        |}
        |""".stripMargin){ m =>
      m.readLong(0xc000) should equal(-3)
    }
  }

  test("Signed16 to int32 extension 2") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        |int32 output @$c000
        |
        |void main() {
        |  static volatile signed16 tmp
        |  tmp = -3
        |  output = 0
        |  memory_barrier()
        |  output += tmp
        |}
        |""".stripMargin){ m =>
      m.readLong(0xc000) should equal(-3)
    }
  }

  test("Trivial implicit sbyte to word extension") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        |word output @$c000
        |
        |sbyte c
        |
        |noinline word f() {
        |  return c
        |}
        |
        |void main() {
        |  c = -2
        |  output = f()
        |}
        |""".stripMargin){ m =>
      m.readWord(0xc000) should equal(0xFFFE)
    }
  }

}
