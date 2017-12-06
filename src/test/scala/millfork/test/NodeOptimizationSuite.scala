package millfork.test

import millfork.test.emu.EmuNodeOptimizedRun
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class NodeOptimizationSuite extends FunSuite with Matchers {

  test("Unreachable after return") {
    EmuNodeOptimizedRun(
      """
        | byte crash @$ffff
        | void main () {
        |   return
        |   crash = 2
        | }
      """.stripMargin)
  }

  test("Unused local variable") {
    EmuNodeOptimizedRun(
      """
        | byte crash @$ffff
        | void main () {
        |   byte a
        |   a = crash
        | }
      """.stripMargin)
  }
}
