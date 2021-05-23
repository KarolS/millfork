package millfork.test

import millfork.Cpu
import millfork.error.ConsoleLogger
import millfork.test.emu._
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class ForLoopSuite extends FunSuite with Matchers {

  test("For-to") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | word output @$c000
        | void main () {
        |   byte i
        |   output = 0
        |   for i,0,to,5 {
        |     output += i
        |   }
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(15))
  }

  test("For-to 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | word output @$c000
        | byte five
        | void main () {
        |   init()
        |   byte i
        |   output = 0
        |   for i,0,to,five {
        |     output += i
        |   }
        | }
        | void init() {
        |   five = 5
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(15))
  }

  test("For-downto") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | word output @$c000
        | void main () {
        |   byte i
        |   output = 0
        |   for i,5,downto,0 {
        |     output += i
        |   }
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(15))
  }

  test("For-downto 2") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | array output [55] @$c000
        | void main () {
        |   byte i
        |   output[0] = 0
        |   output[5] = 0
        |   output[6] = 0
        |   for i,5,downto,0 {
        |     output[i] += 1
        |   }
        | }
      """.stripMargin){m =>
      m.readByte(0xc000) should equal(1)
      m.readByte(0xc005) should equal(1)
      m.readByte(0xc006) should equal(0)
    }
  }

  test("For-downto 3") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | array output [55] @$c000
        | void main () {
        |   byte i
        |   output[0] = 0
        |   output[1] = 0
        |   output[5] = 0
        |   output[6] = 0
        |   for i,5,downto,1 {
        |     stuff()
        |     output[i] += 1
        |   }
        | }
        | noinline void stuff() {}
      """.stripMargin){m =>
      m.readByte(0xc000) should equal(0)
      m.readByte(0xc001) should equal(1)
      m.readByte(0xc005) should equal(1)
      m.readByte(0xc006) should equal(0)
    }
  }

  test("For-until") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | word output @$c000
        | void main () {
        |   byte i
        |   output = 0
        |   for i,0,until,6 {
        |     output += i
        |   }
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(15))
  }

  test("For-parallelto") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | word output @$c000
        | void main () {
        |   byte i
        |   output = 0
        |   for i,0,parallelto,5 {
        |     output += i
        |   }
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(15))
  }

  test("For-paralleluntil") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | word output @$c000
        | void main () {
        |   byte i
        |   output = 0
        |   for i,0,paralleluntil,6 {
        |     output += i
        |   }
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(15))
  }

  test("Various loops") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | void init() {
        |     zero = 0
        |     ff = $ff
        | }
        | byte zero
        | byte ff
        | byte flag @$c000
        | void main () {
        |     init()
        |     byte i
        |     flag = 0
        |     for i, zero, until, ff { }
        |     flag = 1
        |     for i, zero, to, ff { }
        |     flag = 2
        |     for i, ff, downto, zero { }
        |     flag = 3
        |     for i, zero, paralleluntil, ff { }
        |     flag = 4
        |     for i, zero, parallelto, ff { }
        |     flag = 8
        |     for i, zero, until, ff { }
        |     flag = 9
        |     for i, zero, until, zero { _panic() }
        |     flag = 10
        |     for i, ff, until, ff { _panic() }
        |     flag = 11
        |     for i, zero, paralleluntil, zero { _panic() }
        |     flag = 12
        |     for i, ff, paralleluntil, ff { _panic() }
        |     flag = ff
        | }
        | void _panic(){while(true){}}
      """.stripMargin){ m=>

    }
  }

  test("Memcpy") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | array output[5]@$c001
        | array input = [0,1,4,9,16,25,36,49]
        | void main () {
        |   byte i
        |   for i,0,until,output.length {
        |     output[i] = input[i+1]
        |   }
        | }
        | void _panic(){while(true){}}
      """.stripMargin){ m=>
      m.readByte(0xc001) should equal (1)
      m.readByte(0xc005) should equal (25)
    }
  }

  test("Memset with index") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | array output[5]@$c001
        | void main () {
        |   byte i
        |   for i,0,until,output.length {
        |     output[i] = 22
        |   }
        | }
        | void _panic(){while(true){}}
      """.stripMargin){ m=>
      m.readByte(0xc001) should equal (22)
      m.readByte(0xc005) should equal (22)
    }
  }

  test("Memset with pointer") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | array output[5]@$c001
        | void main () {
        |   pointer p
        |   for p,output.addr,until,output.addr+output.length {
        |     p[0] = 22
        |   }
        | }
        | void _panic(){while(true){}}
      """.stripMargin){ m=>
      m.readByte(0xc001) should equal (22)
      m.readByte(0xc005) should equal (22)
    }
  }

  test("Screen fill") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | array output[$400]@$c000
        | void main () {
        |   pointer p
        |   for p,$c000,paralleluntil,$c400{
        |     p[0] = 34
        |   }
        | }
        | void _panic(){while(true){}}
      """.stripMargin){ m=>
      for(i <- 0xc000 to 0xc3ff) {
        m.readByte(i) should equal (34)
      }
    }
  }

  test("Various bulk operations") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | array output0[5]@$c000
        | array output1[5]@$c010
        | array output2[5]@$c020
        | array input = [0,1,4,9,16,25,36,49]
        | void main () {
        |   byte i
        |   for i,0,until,5 {
        |     output0[i] = 0
        |   }
        |   for i,0,paralleluntil,5 {
        |     output1[i] = 1
        |     output2[i] = i
        |   }
        |   for i,4,downto,0 {
        |     output0[i] +'= 4
        |     output2[i] <<= 1
        |   }
        |   for i,0,to,4 {
        |     output1[i] ^= i
        |     output1[i] += 5
        |   }
        | }
      """.stripMargin){ m=>
      m.dump(0xc000, 5)(TestErrorReporting.log.debug(_))
      m.dump(0xc010, 5)(TestErrorReporting.log.debug(_))
      m.dump(0xc020, 5)(TestErrorReporting.log.debug(_))
      m.readByte(0xc001) should equal (4)
      m.readByte(0xc023) should equal (6)
      m.readByte(0xc013) should equal (7)
    }
  }

  test("Modifying whole array") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Motorola6809)(
      """
        | array output[$ff]@$c000
        | void main () {
        |   byte i
        |   for i,0,paralleluntil,$ff {
        |     output[i] = 0
        |   }
        |   memory_barrier()
        |   for i,0,paralleluntil,$f0 {
        |     output[i] += 1
        |   }
        |   memory_barrier()
        |   for i,0,paralleluntil,$e0 {
        |     output[i] -= 1
        |   }
        |   memory_barrier()
        |   for i,0,paralleluntil,$d0 {
        |     output[i] += 5
        |   }
        |   memory_barrier()
        |   for i,0,paralleluntil,$c0 {
        |     output[i] -= 4
        |   }
        |   memory_barrier()
        |   for i,0,paralleluntil,$b0 {
        |     output[i] <<= 4
        |   }
        |   memory_barrier()
        |   for i,0,paralleluntil,$a0 {
        |     output[i] >>= 3
        |   }
        |   memory_barrier()
        |   for i,0,paralleluntil,$90 {
        |     output[i] <<= 2
        |   }
        |   memory_barrier()
        |   for i,0,paralleluntil,$80 {
        |     output[i] >>= 2
        |   }
        |   memory_barrier()
        | }
      """.stripMargin){ m=>
      m.readByte(0xc0fe) should equal (0)
      m.readByte(0xc0f0) should equal (0)
      m.readByte(0xc0ef) should equal (1)
      m.readByte(0xc0e0) should equal (1)
      m.readByte(0xc0df) should equal (0)
      m.readByte(0xc0d0) should equal (0)
      m.readByte(0xc0cf) should equal (5)
      m.readByte(0xc0c0) should equal (5)
      m.readByte(0xc0bf) should equal (1)
      m.readByte(0xc0b0) should equal (1)
      m.readByte(0xc0af) should equal (16)
      m.readByte(0xc0a0) should equal (16)
      m.readByte(0xc09f) should equal (2)
      m.readByte(0xc090) should equal (2)
      m.readByte(0xc08f) should equal (8)
      m.readByte(0xc080) should equal (8)
      m.readByte(0xc07f) should equal (2)
      m.readByte(0xc070) should equal (2)
    }
  }

  test("Edge cases - positive") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
        | void main() {
        |     byte i
        |     for i,0,until,256 { f() }
        |     for i,0,paralleluntil,256 { f() }
        |     for i,0,until,255 { f() }
        |     for i,0,paralleluntil,255 { f() }
        |     for i,0,to,255 { f() }
        |     for i,0,parallelto,255 { f() }
        |     for i,255,downto,0 { f() }
        | }
        | void f() { }
      """.stripMargin){ m => }
  }

  test("Edge cases - negative") {
    ShouldNotCompile("""
        | void main() {
        |     byte i
        |     for i,0,until,257 { f() }
        | }
        | void f() { }
      """.stripMargin)
    ShouldNotCompile("""
        | void main() {
        |     byte i
        |     for i,0,paralleluntil,257 { f() }
        | }
        | void f() { }
      """.stripMargin)
    ShouldNotCompile("""
        | void main() {
        |     byte i
        |     for i,0,to,256 { f() }
        | }
        | void f() { }
      """.stripMargin)
    ShouldNotCompile("""
        | void main() {
        |     byte i
        |     for i,0,parallelto,256 { f() }
        | }
        | void f() { }
      """.stripMargin)
    ShouldNotCompile("""
        | void main() {
        |     byte i
        |     for i,256,downto,0 { f() }
        | }
        | void f() { }
      """.stripMargin)
  }


  test("For each") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8086, Cpu.Motorola6809)(
      """
        | array output[$400]@$c000
        | void main () {
        |   pointer p
        |   for p:[$c000, $c003, $c005, $c007]{
        |     p[0] = 34
        |   }
        |   word w
        |   w = $c004
        |   for p:[$c001, w, $c006, w+4]{
        |     p[0] = 42
        |   }
        | }
        | void _panic(){while(true){}}
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(34)
      m.readByte(0xc003) should equal(34)
      m.readByte(0xc005) should equal(34)
      m.readByte(0xc007) should equal(34)
      m.readByte(0xc001) should equal(42)
      m.readByte(0xc004) should equal(42)
      m.readByte(0xc006) should equal(42)
      m.readByte(0xc008) should equal(42)
    }
  }

  test("Folding loops on 6502") {
    EmuUnoptimizedRun(
      """
        |array a [100]
        |void main() {
        |  byte sum
        |  byte i
        |  pointer p
        |  p = a.addr
        |  sum = 0
        |  for i,0,paralleluntil,100 { sum += a[i] }
        |  for i,0,paralleluntil,100 { sum $+= a[i] }
        |  for i,0,paralleluntil,100 { sum &= a[i] }
        |  for i,0,until,100 { sum &= a[i] }
        |  for i,0,until,50 { sum &= a[i+1] }
        |  for i,0,parallelto,50 { sum &= a[i] }
        |  for i,0,until,100 { sum &= p[i] }
        |  word wsum
        |  for i,0,paralleluntil,100 { wsum += a[i] }
        |  stack byte ssum
        |  stack word swsum
        |  for i,0,paralleluntil,100 { ssum += a[i] }
        |  for i,0,paralleluntil,100 { swsum += a[i] }
        |}
      """.stripMargin)
  }

  test("Folding loops on Z80") {
    EmuOptimizedZ80Run(
      """
        |array a [10] = [0,1,2,3,4,5,6,7,8,9]
        |byte output @$c000
        |void main() {
        |  byte sum
        |  byte i
        |  sum = 0
        |  for i,0,paralleluntil,10 { sum += a[i] }
        |  output = sum
        |}
      """.stripMargin).readByte(0xc000) should equal(45)
  }

  test("Some pointers in loops") {
    val code =
      """
        |struct Sprite {
        |    byte y
        |}
        |
        |array(Sprite) sprites [20]
        |Sprite test
        |
        |void main() {
        |    byte i
        |    test = Sprite(1)
        |    pointer.Sprite test_pointer
        |    test_pointer = test.pointer
        |
        |    sprites[0] = Sprite(5)
        |    pointer.Sprite current_sprite
        |    current_sprite = pointer.Sprite(sprites.pointer)
        |    for i,0,until,20 {
        |        current_sprite->y = test_pointer->y
        |    }
        |}
        |""".stripMargin
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809) (code) { m =>
      // OK
    }
  }


  test("Looping across arrays") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        |
        | array source = [1,2,3,4,5]
        | array target[5] @$c000
        | void main() {
        |   byte i
        |   for i:source {
        |     target[i] = source[i]
        |   }
        | }
        |""".stripMargin) { m =>
      m.readByte(0xc000) should equal(1)
      m.readByte(0xc001) should equal(2)
      m.readByte(0xc002) should equal(3)
      m.readByte(0xc003) should equal(4)
      m.readByte(0xc004) should equal(5)
    }
  }
  test("Looping across arrays 2") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        |
        | array source = [1,2,3,4,5]
        | array target[5] @$c000
        | void main() {
        |   byte i
        |   byte w
        |   for i,w:source {
        |     target[i] = w
        |   }
        | }
        |""".stripMargin) { m =>
      m.readByte(0xc000) should equal(1)
      m.readByte(0xc001) should equal(2)
      m.readByte(0xc002) should equal(3)
      m.readByte(0xc003) should equal(4)
      m.readByte(0xc004) should equal(5)
    }
  }
  test("Looping across arrays 3") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        |
        | array source = [1,2,3,4,5]
        | array target[5] @$c000
        | void main() {
        |   byte i
        |   pointer.byte p
        |   for i,p:source {
        |     target[i] = p[0]
        |   }
        | }
        |""".stripMargin) { m =>
      m.readByte(0xc000) should equal(1)
      m.readByte(0xc001) should equal(2)
      m.readByte(0xc002) should equal(3)
      m.readByte(0xc003) should equal(4)
      m.readByte(0xc004) should equal(5)
    }
  }
  test("Looping across arrays 4") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        |
        | array target[5] @$c000 = [1,2,3,4,5]
        | void main() {
        |   byte i
        |   pointer.byte p
        |   for i,p:target {
        |     p[0] += 1
        |   }
        | }
        |""".stripMargin) { m =>
      m.readByte(0xc000) should equal(2)
      m.readByte(0xc001) should equal(3)
      m.readByte(0xc002) should equal(4)
      m.readByte(0xc003) should equal(5)
      m.readByte(0xc004) should equal(6)
    }
  }

}
