package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuBenchmarkRun, EmuCrossPlatformBenchmarkRun, EmuUnoptimizedCrossPlatformRun, ShouldNotCompile}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class TypeSuite extends FunSuite with Matchers {

  test("Word to word") {
    EmuCrossPlatformBenchmarkRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp, Cpu.Intel8086, Cpu.Motorola6809)("""
        | word output @$c000
        | void main () {
        |  output = word(0x203)
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(0x203))
  }
  test("Byte to sbyte") {
    EmuBenchmarkRun("""
        | word output @$c000
        | void main () {
        |  if sbyte(0) > sbyte(255) { output = word(0x203) }
        | }
      """.stripMargin)(_.readWord(0xc000) should equal(0x203))
  }

  test("Function pointer conversions") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80)("""
        | volatile pointer.kernal_interrupt k
        | volatile function.void.to.void f
        |
        | void main () {
        |   f = k
        | }
      """.stripMargin){_ => }
    ShouldNotCompile("""
        | volatile pointer.kernal_interrupt k
        | volatile function.void.to.void f
        |
        | void main () {
        |   k = f
        | }
      """.stripMargin)
    ShouldNotCompile("""
        | volatile pointer.interrupt i
        | volatile function.void.to.void f
        |
        | void main () {
        |   i = f
        | }
      """.stripMargin)
    ShouldNotCompile("""
        | volatile pointer.interrupt i
        | volatile function.void.to.void f
        |
        | void main () {
        |   f = i
        | }
      """.stripMargin)
    ShouldNotCompile("""
        | volatile pointer.interrupt i
        | volatile pointer.kernal_interrupt k
        |
        | void main () {
        |   i = k
        | }
      """.stripMargin)
    ShouldNotCompile("""
        | volatile pointer.interrupt i
        | volatile pointer.kernal_interrupt k
        |
        | void main () {
        |   k = i
        | }
      """.stripMargin)
  }
}
