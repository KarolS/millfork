package millfork.node

import millfork.env.{Environment, Type}
import millfork.output.NoAlignment

import java.lang.Double.doubleToRawLongBits
import java.lang.Float.floatToRawIntBits

/**
  * @author Karol Stasiak
  */
abstract class FloatFormat(val name: String, val hasInfinity: Boolean, val mantissaLength: Int, val exponentLength: Int) {

  def format(f: Double): Option[List[Int]]

  def toStruct(env: Environment): StructDefinitionStatement = {
    val alignment = env.get[Type]("word").alignment
    val size = format(0.0).size
    StructDefinitionStatement("float." + name,
      List.tabulate(size)(i => FieldDesc("byte", "b" + i, None)),
      Some(if (size % 2 == 0) alignment else NoAlignment))
  }
}

object IEEE64LE extends FloatFormat("ieee64le", true, 53, 10) {

  override def format(f: Double): Option[List[Int]] = {
    val l = doubleToRawLongBits(f)
    Some(List(
      l.>>(0).toInt.&(0xff),
      l.>>(8).toInt.&(0xff),
      l.>>(16).toInt.&(0xff),
      l.>>(24).toInt.&(0xff),
      l.>>(32).toInt.&(0xff),
      l.>>(40).toInt.&(0xff),
      l.>>(48).toInt.&(0xff),
      l.>>(56).toInt.&(0xff)
    ))
  }
}

object IEEE64BE extends FloatFormat("ieee64be", true, 53, 10) {

  override def format(f: Double): Option[List[Int]] = {
    val l = doubleToRawLongBits(f)
    Some(List(
      l.>>(56).toInt.&(0xff),
      l.>>(48).toInt.&(0xff),
      l.>>(40).toInt.&(0xff),
      l.>>(32).toInt.&(0xff),
      l.>>(24).toInt.&(0xff),
      l.>>(16).toInt.&(0xff),
      l.>>(8).toInt.&(0xff),
      l.>>(0).toInt.&(0xff)
    ))
  }
}

object IEEE32LE extends FloatFormat("ieee32le", true, 53, 10) {

  override def format(f: Double): Option[List[Int]] = {
    val l = java.lang.Float.floatToIntBits(f.toFloat)
    if (!l.isNaN && !l.isInfinite) {
      if (l < Float.MinValue || l > Float.MaxValue) {
        return None
      }
    }
    Some(List(
      l.>>(0).&(0xff),
      l.>>(8).&(0xff),
      l.>>(16).&(0xff),
      l.>>(24).&(0xff)
    ))
  }
}

object IEEE32BE extends FloatFormat("ieee32be", true, 53, 10) {

  override def format(f: Double): Option[List[Int]] = {
    val l = floatToRawIntBits(f.toFloat)
    if (!l.isNaN && !l.isInfinite) {
      if (l < Float.MinValue || l > Float.MaxValue) {
        return None
      }
    }
    Some(List(
      l.>>(24).&(0xff),
      l.>>(16).&(0xff),
      l.>>(8).&(0xff),
      l.>>(0).&(0xff)
    ))
  }
}


object MBF extends FloatFormat("mbf", false, 31, 8) {

  override def format(f: Double): Option[List[Int]] = {
    val l = doubleToRawLongBits(f)
    val sign = if (l < 0) 0x80 else 0
    var mantissa = l.&(1L.<<(52) - 1).>>(21).toInt
    if (l.&(1 << 20) != 0L) {
      mantissa += 1
    }
    var exponent = l.>>>(52).&(0x7ff).toInt - 1023
    if (mantissa < 0) {
      mantissa -= Int.MinValue
      exponent += 1
    }
    if (f != 0 && (exponent > 126 || exponent < -129)) return None
    if (f == 0) return Some(List(0, sign,0,0,0));
    Some(List(
      exponent + 129,
      sign ^ mantissa.>>(24).&(0xff),
      mantissa.>>(16).&(0xff),
      mantissa.>>(8).&(0xff),
      mantissa.>>(0).&(0xff)
    ))
  }
}
