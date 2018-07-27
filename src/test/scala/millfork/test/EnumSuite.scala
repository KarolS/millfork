package millfork.test

import millfork.Cpu
import millfork.test.emu.{EmuUnoptimizedCrossPlatformRun, ShouldNotCompile}
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class EnumSuite extends FunSuite with Matchers {

  test("Enum basic test") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | enum ugly {
        |   a
        |   b,c,
        |   d
        | }
        | void main () {
        |   byte i
        |   ugly e
        |   e = a
        |   if byte(e) != 0 { crash() }
        |   i = 1
        |   if ugly(i) != b { crash() }
        | }
        | asm void crash() {
        | #if ARCH_6502
        |   sta $bfff
        |   rts
        | #elseif ARCH_I80
        |   ld ($bfff),a
        |   ret
        | #else
        | #error
        | #endif
        | }
      """.stripMargin){_=>}
  }

  test("Enum arrays") {
    EmuUnoptimizedCrossPlatformRun(Cpu.Mos, Cpu.Z80, Cpu.Intel8080, Cpu.Sharp)(
      """
        | enum ugly {
        |   a
        |   b,c,
        |   d
        | }
        | array a1 [ugly]
        | array a2 [ugly] = [6,7,8,9]
        | void main () {
        |   if a2[a] != 6 { crash() }
        | }
        | asm void crash() {
        | #if ARCH_6502
        |   sta $bfff
        |   rts
        | #elseif ARCH_I80
        |   ld ($bfff),a
        |   ret
        | #else
        | #error
        | #endif
        | }
      """.stripMargin){_=>}
  }

  test("Enum-byte incompatibility test") {
    ShouldNotCompile(
      """
        | enum ugly { a }
        | void main() {
        |     byte b
        |     ugly u
        |     b = u
        | }
      """.stripMargin)

    ShouldNotCompile(
      """
        | enum ugly { a }
        | void main() {
        |     byte b
        |     ugly u
        |     u = b
        | }
      """.stripMargin)

    ShouldNotCompile(
      """
        | enum ugly { a }
        | byte main() {
        |     byte b
        |     ugly u
        |     return u
        | }
      """.stripMargin)

    ShouldNotCompile(
      """
        | enum ugly { a }
        | ugly main() {
        |     byte b
        |     ugly u
        |     return b
        | }
      """.stripMargin)

    ShouldNotCompile(
      """
        | enum ugly { a }
        | byte main() {
        |     byte b
        |     ugly u
        |     return b + u
        | }
      """.stripMargin)

    ShouldNotCompile(
      """
        | enum ugly { a }
        | void main() {
        |     byte b
        |     ugly u
        |     if b > u {}
        | }
      """.stripMargin)

    ShouldNotCompile(
      """
        | enum ugly { a }
        | array arr[ugly] = []
        | void main() {
        | }
      """.stripMargin)

    ShouldNotCompile(
      """
        | enum ugly { a }
        | array arr[ugly] = [1,2,3]
        | void main() {
        | }
      """.stripMargin)

    ShouldNotCompile(
      """
        | enum ugly { a }
        | array arr[ugly]
        | ugly main() {
        |     return a[0]
        | }
      """.stripMargin)
  }
}
