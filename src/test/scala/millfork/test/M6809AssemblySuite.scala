package millfork.test

import millfork.test.emu._
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class M6809AssemblySuite extends FunSuite with Matchers {

  test("Common instructions") {
    // TODO: handle more
    EmuUnoptimizedM6809Run(
      """
        | asm void main () {
        |   rts
        |   inca
        |   inc 0
        |   bra main.addr
        |   lda #3
        |   lda ,x
        |   lda 1,x
        |   pshs a,b
        |   puls a,b
        |   tfr a,b
        |   swi2
        |   swi3
        |   ldx [d,x]
        |   leas ,x++
        |   cmpb ,--y
        |   ldu [4]
        |   ldd #3465
        |   stb <55
        |   rts
        | }
      """.stripMargin)
  }

}
