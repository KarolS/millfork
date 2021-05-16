package millfork.test.auxilary

import org.scalatest.{FunSuite, Matchers}
import millfork.node.MBF

import java.lang.Double.{doubleToRawLongBits, longBitsToDouble}

/**
  * @author Karol Stasiak
  */
class MBFSuite extends FunSuite with Matchers {
  test("MBF test") {
    MBF.format(10) should equal(Some(List(0x84, 0x20, 0, 0, 0)))
    MBF.format(2) should equal(Some(List(0x82, 0, 0, 0, 0)))
    MBF.format(1) should equal(Some(List(0x81, 0, 0, 0, 0)))
    MBF.format(longBitsToDouble(doubleToRawLongBits(1) + 1)) should equal(Some(List(0x81, 0, 0, 0, 0)))
    MBF.format(0) should equal(Some(List(0, 0, 0, 0, 0)))
    MBF.format(0.5) should equal(Some(List(0x80, 0, 0, 0, 0)))
    MBF.format(0.25) should equal(Some(List(0x7F, 0, 0, 0, 0)))
    MBF.format(-0.5) should equal(Some(List(0x80, 0x80, 0, 0, 0)))
    MBF.format(Double.MaxValue) should equal(None)
    MBF.format(Double.NegativeInfinity) should equal(None)
    MBF.format(Double.NaN) should equal(None)
    MBF.format(.00014352314036) should equal(Some(List(0x74, 0x16, 0x7E, 0xB3, 0x1B)))
    MBF.format(.0013422634824) should equal(Some(List(0x77, 0x2F, 0xEE, 0xE3, 0x85)))
    MBF.format(.0096140170119) should equal(Some(List(0x7A, 0x1D, 0x84, 0x1C, 0x2A)))
    MBF.format(.055505126860) should equal(Some(List(0x7C, 0x63, 0x59, 0x58, 0x0A)))
    MBF.format(.24022638462) should equal(Some(List(0x7E, 0x75, 0xFD, 0xE7, 0xC6)))
    MBF.format(.69314718608) should equal(Some(List(0x80, 0x31, 0x72, 0x18, 0x10)))
    MBF.format(1 / Math.log(2)) should equal(Some(List(0x81, 0x38, 0xAA, 0x3B, 0x29)))
    // 11.00 1001 0000 1111 1101 1010 1010 0010 0010 0001 01101000110000100011010011000100110001100110001010001011100000
    MBF.format(Math.PI) should equal(Some(List(0x82, 0x49, 0x0F, 0xDA, 0xA2)))
  }
}
