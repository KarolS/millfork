package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuCrossPlatformBenchmarkRun, EmuUnoptimizedCrossPlatformRun, EmuUnoptimizedRun, ShouldNotCompile}
import org.scalatest.{AppendedClues, FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class BooleanFlagTypesSuite extends FunSuite with Matchers with AppendedClues {

  test("clear_carry") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        | asm clear_carry testCarryResult(byte register(a) dummy)
        |{
        | // we always set carry, so this is always false:
        | #if ARCH_6502
        |   STA param
        |	  SEC
        |	  RTS
        | #elseif ARCH_I80
        |   #pragma zilog_syntax
        |   LD (param),A
        |   OR A
        |   CCF
        |   RET
        | #elseif ARCH_6809
        |   STA param
        |   COMA
        |   RTS
        | #else
        |   #error unknown arch
        | #endif
        |}
        |
        |byte output @$c000
        |byte param @$c001
        |
        |void main() {
        | bool clearCarry
        |	clearCarry = testCarryResult(55)
        |	if clearCarry {
        |		output = 0
        |	} else {
        |		output = 1
        |	}
        |}
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(1)
      m.readByte(0xc001) should equal(55)
    }
  }


  test("set_carry") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        | asm set_carry testCarryResult(byte register(a) dummy)
        |{
        | // we always set carry, so this is always true:
        | #if ARCH_6502
        |   STA param
        |	  SEC
        |	  RTS
        | #elseif ARCH_I80
        |   #pragma zilog_syntax
        |   LD (param),A
        |   OR A
        |   CCF
        |   RET
        | #elseif ARCH_6809
        |   STA param
        |   COMA
        |   RTS
        | #else
        |   #error unknown arch
        | #endif
        |}
        |
        |byte output @$c000
        |byte param @$c001
        |
        |void main() {
        | bool setCarry
        |	setCarry = testCarryResult(55)
        |	if setCarry {
        |		output = 1
        |	} else {
        |		output = 0
        |	}
        |}
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(1)
      m.readByte(0xc001) should equal(55)
    }

  }


  test("set_zero") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        | asm set_zero testCarryResult(byte register(a) dummy)
        |{
        | // we always set zero, so this is always true:
        | #if ARCH_6502
        |   STA param
        |	  LDA #0
        |	  RTS
        | #elseif ARCH_I80
        |   #pragma zilog_syntax
        |   LD (param),A
        |   XOR A
        |   CCF
        |   RET
        | #elseif ARCH_6809
        |   STA param
        |   LDA #0
        |   RTS
        | #else
        |   #error unknown arch
        | #endif
        |}
        |
        |byte output @$c000
        |byte param @$c001
        |
        |void main() {
        | bool setZero
        |	setZero = testCarryResult(55)
        |	if setZero {
        |		output = 1
        |	} else {
        |		output = 0
        |	}
        |}
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(1)
      m.readByte(0xc001) should equal(55)
    }

  }


  test("clear_zero") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Motorola6809)(
      """
        | asm clear_zero testCarryResult(byte register(a) dummy)
        |{
        | // we always set zero, so this is always false:
        | #if ARCH_6502
        |   STA param
        |	  LDA #0
        |	  RTS
        | #elseif ARCH_I80
        |   #pragma zilog_syntax
        |   LD (param),A
        |   XOR A
        |   CCF
        |   RET
        | #elseif ARCH_6809
        |   STA param
        |   LDA #0
        |   RTS
        | #else
        |   #error unknown arch
        | #endif
        |}
        |
        |byte output @$c000
        |byte param @$c001
        |
        |void main() {
        | bool clearZero
        |	clearZero = testCarryResult(55)
        |	if clearZero {
        |		output = 0
        |	} else {
        |		output = 1
        |	}
        |}
      """.stripMargin) { m =>
      m.readByte(0xc000) should equal(1)
      m.readByte(0xc001) should equal(55)
    }

  }

}
