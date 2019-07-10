package millfork.env

import millfork.DecimalUtils._
import millfork.output.DivisibleAlignment

object Constant {
  val Zero: Constant = NumericConstant(0, 1)
  val One: Constant = NumericConstant(1, 1)

  def apply(i: Long): Constant = NumericConstant(i, minimumSize(i))

  def minimumSize(value: Long): Int = if (value < -128 || value > 255) 2 else 1 // TODO !!!
}

import millfork.env.Constant.minimumSize
import millfork.error.ConsoleLogger
import millfork.node.Position

sealed trait Constant {

  def toIntelString: String

  def isProvablyZero: Boolean = false
  def isProvably(value: Int): Boolean = false
  def isProvablyInRange(startInclusive: Int, endInclusive: Int): Boolean = false
  def isProvablyNonnegative: Boolean = false
  def isProvablyGreaterOrEqualThan(other: Constant): Boolean = other match {
    case NumericConstant(0, _) => true
    case _ => false
  }
  def fitsProvablyIntoByte: Boolean = false
  def isProvablyDivisibleBy256: Boolean = false

  def asl(i: Constant): Constant = i match {
    case NumericConstant(sa, _) => asl(sa.toInt)
    case _ => CompoundConstant(MathOperator.Shl, this, i)
  }

  def asl(i: Int): Constant = CompoundConstant(MathOperator.Shl, this, NumericConstant(i, 1))

  def requiredSize: Int

  def +(that: Constant): Constant = CompoundConstant(MathOperator.Plus, this, that)

  def *(scale: Int): Constant = CompoundConstant(MathOperator.Times, this, NumericConstant(scale, Constant.minimumSize(scale) min 2)).quickSimplify

  def -(that: Constant): Constant = CompoundConstant(MathOperator.Minus, this, that)

  def +(that: Long): Constant = if (that == 0) this else this + NumericConstant(that, minimumSize(that))

  def -(that: Long): Constant = if (that == 0) this else this - NumericConstant(that, minimumSize(that))

  def loByte: Constant = {
    if (requiredSize == 1) return this
    if (isProvablyDivisibleBy256) Constant.Zero else SubbyteConstant(this, 0)
  }

  def hiByte: Constant = {
    if (requiredSize == 1) Constant.Zero
    else SubbyteConstant(this, 1)
  }

  def subbyte(index: Int): Constant = {
    if (requiredSize <= index) Constant.Zero
    else index match {
      case 0 => if (isProvablyDivisibleBy256) Constant.Zero else loByte
      case 1 => hiByte
      case _ => SubbyteConstant(this, index)
    }
  }

  def subword(index: Int): Constant = {
    if (requiredSize <= index) Constant.Zero
    else {
      // TODO: check if ok
      CompoundConstant(MathOperator.Or, CompoundConstant(MathOperator.Shl, subbyte(index + 1), NumericConstant(8, 1)), subbyte(index)).quickSimplify
    }
  }

  def subconstant(offset: Int, length: Int): Constant = {
    if (offset == 0 && length == requiredSize) {
      this
    } else if (length == 1) {
      subbyte(offset)
    } else if (offset >= requiredSize) {
      Constant.Zero
    } else {
      ((length - 1) to 0 by (-1)).map { i =>
        val index = i + offset
        if (i == 0) subbyte(index) else CompoundConstant(MathOperator.Shl, subbyte(index), NumericConstant(8 * i, 1))
      }.reduceLeft((l, r) => CompoundConstant(MathOperator.Or, l, r).quickSimplify).quickSimplify
    }
  }

  def isLowestByteAlwaysEqual(i: Int) : Boolean = false

  def quickSimplify: Constant = this

  def isRelatedTo(v: Thing): Boolean

  def refersTo(name: String): Boolean

  def fitsInto(typ: Type): Boolean = true // TODO

  final def succ: Constant = (this + 1).quickSimplify

}

case class AssertByte(c: Constant) extends Constant {
  override def isProvablyGreaterOrEqualThan(other: Constant): Boolean = c.isProvablyGreaterOrEqualThan(other)
  override def isProvablyZero: Boolean = c.isProvablyZero
  override def isProvably(i: Int): Boolean = c.isProvably(i)
  override def isProvablyNonnegative: Boolean = c.isProvablyNonnegative
  override def isProvablyInRange(startInclusive: Int, endInclusive: Int): Boolean = c.isProvablyInRange(startInclusive, endInclusive)
  override def fitsProvablyIntoByte: Boolean = true

  override def requiredSize: Int = 1

  override def isRelatedTo(v: Thing): Boolean = c.isRelatedTo(v)

  override def refersTo(name: String): Boolean = c.refersTo(name)

  override def quickSimplify: Constant = AssertByte(c.quickSimplify)

  override def fitsInto(typ: Type): Boolean = true
  override def toIntelString: String = c.toIntelString
}

case class StructureConstant(typ: StructType, fields: List[Constant]) extends Constant {
  override def toIntelString: String = typ.name + fields.map(_.toIntelString).mkString("(",",",")")

  override def toString: String = typ.name + fields.map(_.toString).mkString("(",",",")")

  override def requiredSize: Int = typ.size

  override def isRelatedTo(v: Thing): Boolean = fields.exists(_.isRelatedTo(v))

  override def refersTo(name: String): Boolean = typ.name == name || fields.exists(_.refersTo(name))

  override def loByte: Constant = subbyte(0)

  override def hiByte: Constant = subbyte(1)

  override def subbyte(index: Int): Constant = {
    var offset = 0
    for ((fv, (ft, _)) <- fields.zip(typ.mutableFieldsWithTypes)) {
      val fs = ft.size
      if (index < offset + fs) {
        return fv.subbyte(index - offset)
      }
      offset += fs
    }
    Constant.Zero
  }
}

case class UnexpandedConstant(name: String, requiredSize: Int) extends Constant {
  override def isRelatedTo(v: Thing): Boolean = false

  override def toString: String = name

  override def toIntelString: String = name

  override def refersTo(name: String): Boolean = name == this.name
}

case class NumericConstant(value: Long, requiredSize: Int) extends Constant {
  if (requiredSize == 1) {
    if (value < -128 || value > 255) {
      throw new IllegalArgumentException(s"The constant $value is too big")
    }
  }
  override def isProvablyGreaterOrEqualThan(other: Constant): Boolean = value >= 0 && (other match {
    case NumericConstant(o, _) if o >= 0  => value >= o
    case _ => false
  })
  override def isProvablyInRange(startInclusive: Int, endInclusive: Int): Boolean = value >= startInclusive && value <= endInclusive
  override def isProvablyZero: Boolean = value == 0
  override def isProvably(i: Int): Boolean = value == i
  override def isProvablyNonnegative: Boolean = value >= 0
  override def fitsProvablyIntoByte: Boolean = requiredSize == 1
  override def isProvablyDivisibleBy256: Boolean = (value & 0xff) == 0

  override def isLowestByteAlwaysEqual(i: Int) : Boolean = (value & 0xff) == (i&0xff)

  override def asl(i: Int): Constant = {
    val newSize = requiredSize + i / 8
    val mask = (1 << (8 * newSize)) - 1
    NumericConstant((value << i) & mask, newSize)
  }

  override def +(that: Constant): Constant = that + value

  override def +(that: Long) = NumericConstant(value + that, minimumSize(value + that))

  override def -(that: Long) = NumericConstant(value - that, minimumSize(value - that))

  override def toString: String = if (value > 9) value.formatted("$%X") else value.toString

  override def toIntelString: String = if (value > 9) {
    val tmp = value.formatted("%Xh")
    if (tmp(0) > '9') "0" + tmp else tmp
  } else value.toString

  override def isRelatedTo(v: Thing): Boolean = false

  override def refersTo(name: String): Boolean = false

  override def fitsInto(typ: Type): Boolean = {
    if (typ.isSigned) {
      typ.size match {
        case 1 => value == value.toByte
        case 2 => value == value.toShort
        case 3 => value == ((value.toInt << 8) >> 8)
        case 4 => value == value.toInt
        case _ => true
      }
    } else {
      typ.size match {
        case 1 => value == (value & 0xff)
        case 2 => value == (value & 0xffff)
        case 3 => value == (value & 0xffffff)
        case 4 => value == (value & 0xffffffffL)
        case _ => true
      }
    }
  }
}

case class MemoryAddressConstant(var thing: ThingInMemory) extends Constant {

  override def isProvablyNonnegative: Boolean = true
  override def isProvablyGreaterOrEqualThan(other: Constant): Boolean = other match {
    case NumericConstant(0, _) => true
    case MemoryAddressConstant(otherThing) => thing == otherThing
    case CompoundConstant(MathOperator.Plus, MemoryAddressConstant(otherThing), c) =>
      thing == otherThing && c.isProvablyNonnegative
    case CompoundConstant(MathOperator.Plus, c, MemoryAddressConstant(otherThing)) =>
      thing == otherThing && c.isProvablyNonnegative
    case _ => false
  }

  override def isProvablyDivisibleBy256: Boolean = thing match {
    case t: PreallocableThing => t.alignment match {
      case DivisibleAlignment(divisor) => divisor.&(0xff) == 0
      case _ => false
    }
    case t: UninitializedMemory => t.alignment match {
      case DivisibleAlignment(divisor) => divisor.&(0xff) == 0
      case _ => false
    }
    case _ => false
  }

  override def fitsProvablyIntoByte: Boolean = thing.zeropage // TODO: check if it's true only on 6502

  override def requiredSize = 2

  override def toString: String = thing.name

  override def toIntelString: String = thing.name

  override def isRelatedTo(v: Thing): Boolean = thing.name == v.name

  override def refersTo(name: String): Boolean = name == thing.name
}

case class SubbyteConstant(base: Constant, index: Int) extends Constant {
  override def quickSimplify: Constant = {
    val simplified = base.quickSimplify
    simplified match {
      case NumericConstant(x, size) => if (index >= size) {
        Constant.Zero
      } else {
        NumericConstant((x >> (index * 8)) & 0xff, 1)
      }
      case _ =>
        if (index == 0 && simplified.isProvablyDivisibleBy256) Constant.Zero
        else SubbyteConstant(simplified, index)
    }
  }

  override def requiredSize = 1

  override def isProvablyNonnegative: Boolean = true
  override def fitsProvablyIntoByte: Boolean = true
  override def isProvablyDivisibleBy256: Boolean = index == 0 && base.isProvablyDivisibleBy256

  override def toString: String = index match {
    case 0 => s"lo($base)"
    case 1 => s"hi($base)"
    case i => s"b$i($base)"
  }

  override def toIntelString: String = index match {
      case 0 => s"lo(${base.toIntelString})"
      case 1 => s"hi(${base.toIntelString})"
      case i => s"b$i(${base.toIntelString})"
    }

  override def isRelatedTo(v: Thing): Boolean = base.isRelatedTo(v)

  override def refersTo(name: String): Boolean = base.refersTo(name)
}

object MathOperator extends Enumeration {
  val Plus, Minus, Times, Shl, Shr, Shl9, Shr9, Plus9, DecimalPlus9,
  DecimalPlus, DecimalMinus, DecimalTimes, DecimalShl, DecimalShl9, DecimalShr,
  Divide, Modulo,
  And, Or, Exor = Value
}

case class CompoundConstant(operator: MathOperator.Value, lhs: Constant, rhs: Constant) extends Constant {

  override def isProvablyNonnegative: Boolean = {
    import MathOperator._
    operator match {
      case Plus | DecimalPlus |
           Times | DecimalTimes |
           Shl | DecimalShl |
           Shl9 | DecimalShl9 |
           Shr | DecimalShr |
           Divide | Modulo |
           And | Or | Exor => lhs.isProvablyNonnegative && rhs.isProvablyNonnegative
      case _ => false
    }
  }

  override def isProvablyDivisibleBy256: Boolean = operator match {
    case MathOperator.And | MathOperator.Times =>
      lhs.isProvablyDivisibleBy256 || rhs.isProvablyDivisibleBy256
    case MathOperator.Or | MathOperator.Exor | MathOperator.Plus | MathOperator.Minus =>
      lhs.isProvablyDivisibleBy256 && rhs.isProvablyDivisibleBy256
    case MathOperator.Shl =>
      rhs.isProvablyGreaterOrEqualThan(NumericConstant(8, 1))
    case _ => false
  }

  override def quickSimplify: Constant = {
    val l = lhs.quickSimplify
    val r = rhs.quickSimplify
    (l, r) match {
      case (MemoryAddressConstant(lt), MemoryAddressConstant(rt)) if operator == MathOperator.Minus && lt == rt => Constant.Zero
      case (CompoundConstant(MathOperator.Plus, a, ll@NumericConstant(lv, _)), rr@NumericConstant(rv, _)) if operator == MathOperator.Plus =>
        CompoundConstant(MathOperator.Plus, a, ll + rr).quickSimplify
      case (CompoundConstant(MathOperator.Minus, a, ll@NumericConstant(lv, _)), rr@NumericConstant(rv, _)) if operator == MathOperator.Minus =>
        CompoundConstant(MathOperator.Minus, a, ll + rr).quickSimplify
      case (CompoundConstant(MathOperator.Plus, a, ll@NumericConstant(lv, _)), rr@NumericConstant(rv, _)) if operator == MathOperator.Minus =>
        if (lv >= rv) {
          CompoundConstant(MathOperator.Plus, a, ll - rr).quickSimplify
        } else {
          CompoundConstant(MathOperator.Minus, a, rr - ll).quickSimplify
        }
      case (CompoundConstant(MathOperator.Plus, a, ll@NumericConstant(lv, _)), b) if operator == MathOperator.Minus && a == b =>
        ll.quickSimplify
      case (CompoundConstant(MathOperator.Minus, a, ll@NumericConstant(lv, _)), rr@NumericConstant(rv, _)) if operator == MathOperator.Plus =>
        if (lv >= rv) {
          CompoundConstant(MathOperator.Minus, a, ll - rr).quickSimplify
        } else {
          CompoundConstant(MathOperator.Plus, a, rr - ll).quickSimplify
        }
      case (_, CompoundConstant(MathOperator.Minus, a, b)) if operator == MathOperator.Minus =>
        ((l + b) - a).quickSimplify
      case (_, CompoundConstant(MathOperator.Plus, a, b)) if operator == MathOperator.Minus =>
        ((l - a) - b).quickSimplify

      case (CompoundConstant(MathOperator.Shl, SubbyteConstant(c1, 1), NumericConstant(8, _)), SubbyteConstant(c2, 0))
        if operator == MathOperator.Or || operator == MathOperator.Plus && c1 == c2 && c1.requiredSize <= 2 => c1
      case (CompoundConstant(MathOperator.Times, SubbyteConstant(c1, 1), NumericConstant(256, _)), SubbyteConstant(c2, 0))
        if operator == MathOperator.Or || operator == MathOperator.Plus && c1 == c2 && c1.requiredSize <= 2 => c1
      case (CompoundConstant(MathOperator.Times, NumericConstant(256, _), SubbyteConstant(c1, 1)), SubbyteConstant(c2, 0))
        if operator == MathOperator.Or || operator == MathOperator.Plus && c1 == c2 && c1.requiredSize <= 2 => c1

      case (_, CompoundConstant(MathOperator.DecimalMinus, a, b)) if operator == MathOperator.DecimalPlus =>
        CompoundConstant(MathOperator.DecimalMinus, CompoundConstant(MathOperator.DecimalPlus, l, a), b).quickSimplify
      case (NumericConstant(0, 1), c) =>
        operator match {
          case MathOperator.Plus => c
          case MathOperator.Plus9 => c
          case MathOperator.DecimalPlus => c
          case MathOperator.DecimalPlus9 => c
          case MathOperator.Minus => CompoundConstant(operator, l, r)
          case MathOperator.Times => Constant.Zero
          case MathOperator.Shl => Constant.Zero
          case MathOperator.Shr => Constant.Zero
          case MathOperator.Shl9 => Constant.Zero
          case MathOperator.Shr9 => Constant.Zero
          case MathOperator.Exor => c
          case MathOperator.Or => c
          case MathOperator.And => Constant.Zero
          case MathOperator.Divide => Constant.Zero
          case MathOperator.Modulo => Constant.Zero
          case _ => CompoundConstant(operator, l, r)
        }
      case (c, NumericConstant(0, 1)) =>
        operator match {
          case MathOperator.Plus => c
          case MathOperator.Plus9 => c
          case MathOperator.DecimalPlus => c
          case MathOperator.DecimalPlus9 => c
          case MathOperator.Minus => c
          case MathOperator.DecimalMinus => c
          case MathOperator.Times => Constant.Zero
          case MathOperator.Shl => c
          case MathOperator.Shr => c
          case MathOperator.Shl9 => c
          case MathOperator.Shr9 => c
          case MathOperator.Exor => c
          case MathOperator.Or => c
          case MathOperator.And => Constant.Zero
          case _ => CompoundConstant(operator, l, r)
        }
      case (NumericConstant(lv, ls), NumericConstant(rv, rs)) =>
        var size = ls max rs
        val bitmask = (1L << (8*size)) - 1
        val value = operator match {
          case MathOperator.Plus => lv + rv
          case MathOperator.Minus => lv - rv
          case MathOperator.Times => lv * rv
          case MathOperator.Shl => lv << rv
          case MathOperator.Shr => lv >> rv
          case MathOperator.Shl9 => (lv << rv) & 0x1ff
          case MathOperator.Plus9 => (lv + rv) & 0x1ff
          case MathOperator.Shr9 => (lv & 0x1ff) >> rv
          case MathOperator.Exor => (lv ^ rv) & bitmask
          case MathOperator.Or => lv | rv
          case MathOperator.And => lv & rv & bitmask
          case MathOperator.Divide if lv >= 0 && rv >= 0 => lv / rv
          case MathOperator.Modulo if lv >= 0 && rv >= 0 => lv % rv
          case MathOperator.DecimalPlus if ls == 1 && rs == 1 =>
            asDecimal(lv & 0xff, rv & 0xff, _ + _) & 0xff
          case MathOperator.DecimalMinus if ls == 1 && rs == 1 && lv.&(0xff) >= rv.&(0xff) =>
            asDecimal(lv & 0xff, rv & 0xff, _ - _) & 0xff
          case _ => return this
        }
        operator match {
          case MathOperator.Plus9 | MathOperator.DecimalPlus9 | MathOperator.Shl9 | MathOperator.DecimalShl9 =>
            size = 2
          case MathOperator.Times | MathOperator.Shl =>
            val mask = (1 << (size * 8)) - 1
            if (value != (value & mask)) {
              size = ls + rs
            }
          case _ =>
        }
        NumericConstant(value, size)
      case _ => CompoundConstant(operator, l, r)
    }
  }


  import MathOperator._

  override def +(that: Constant): Constant = {
    that match {
      case NumericConstant(n, _) => this + n
      case _ => super.+(that)
    }
  }

  override def -(that: Long): Constant = this + (-that)

  override def +(that: Long): Constant = {
    if (that == 0) {
      return this
    }
    val That = that
    val MinusThat = -that
    this match {
      case CompoundConstant(Plus, CompoundConstant(Shl, SubbyteConstant(c1, 1), NumericConstant(8, _)), SubbyteConstant(c2, 0)) if c1 == c2 => c1
      case CompoundConstant(Plus, NumericConstant(MinusThat, _), r) => r
      case CompoundConstant(Plus, l, NumericConstant(MinusThat, _)) => l
      case CompoundConstant(Plus, NumericConstant(x, _), r) => CompoundConstant(Plus, r, NumericConstant(x + that, minimumSize(x + that)))
      case CompoundConstant(Plus, l, NumericConstant(x, _)) => CompoundConstant(Plus, l, NumericConstant(x + that, minimumSize(x + that)))
      case CompoundConstant(Minus, l, NumericConstant(That, _)) => l
      case _ => CompoundConstant(Plus, this, NumericConstant(that, minimumSize(that)))
    }
  }

  private def plhs: String = lhs match {
    case _: NumericConstant | _: MemoryAddressConstant => lhs.toString
    case _ => "(" + lhs + ')'
  }

  private def prhs: String = lhs match {
    case _: NumericConstant | _: MemoryAddressConstant => rhs.toString
    case _ => "(" + rhs + ')'
  }


  private def plhis: String = lhs match {
    case _: NumericConstant | _: MemoryAddressConstant => lhs.toIntelString
    case _ => "(" + lhs.toIntelString + ')'
  }

  private def prhis: String = lhs match {
    case _: NumericConstant | _: MemoryAddressConstant => rhs.toIntelString
    case _ => "(" + rhs.toIntelString + ')'
  }

  override def toString: String = {
    operator match {
      case Plus => s"$plhs + $prhs"
      case Plus9 => s"nonet($plhs + $prhs)"
      case Minus => s"$plhs - $prhs"
      case Times => s"$plhs * $prhs"
      case Shl => s"$plhs << $prhs"
      case Shr => s"$plhs >> $prhs"
      case Shl9 => s"nonet($plhs << $prhs)"
      case Shr9 => s"$plhs >>>> $prhs"
      case DecimalPlus => s"$plhs +' $prhs"
      case DecimalPlus9 => s"nonet($plhs +' $prhs)"
      case DecimalMinus => s"$plhs -' $prhs"
      case DecimalTimes => s"$plhs *' $prhs"
      case DecimalShl => s"$plhs <<' $prhs"
      case DecimalShl9 => s"nonet($plhs <<' $prhs)"
      case DecimalShr => s"$plhs >>' $prhs"
      case And => s"$plhs & $prhs"
      case Or => s"$plhs | $prhs"
      case Exor => s"$plhs ^ $prhs"
      case Divide => s"$plhs / $prhs"
      case Modulo => s"$plhs %% $prhs"
    }
  }

  override def toIntelString: String = {
    operator match {
      case Plus => s"$plhis + $prhis"
      case Plus9 => s"nonet($plhis + $prhis)"
      case Minus => s"$plhis - $prhis"
      case Times => s"$plhis * $prhis"
      case Shl => s"$plhis << $prhis"
      case Shr => s"$plhis >> $prhis"
      case Shl9 => s"nonet($plhis << $prhis)"
      case Shr9 => s"$plhis >>>> $prhis"
      case DecimalPlus => s"$plhis +' $prhis"
      case DecimalPlus9 => s"nonet($plhis +' $prhis)"
      case DecimalMinus => s"$plhis -' $prhis"
      case DecimalTimes => s"$plhis *' $prhis"
      case DecimalShl => s"$plhis <<' $prhis"
      case DecimalShl9 => s"nonet($plhis <<' $prhis)"
      case DecimalShr => s"$plhis >>' $prhis"
      case And => s"$plhis & $prhis"
      case Or => s"$plhis | $prhis"
      case Exor => s"$plhis ^ $prhis"
      case Divide => s"$plhs / $prhs"
      case Modulo => s"$plhs %% $prhs"
    }
  }

  override def requiredSize: Int = {
    import MathOperator._
    operator match {
      case Plus9 | DecimalPlus9 | Shl9 | DecimalShl9 => 2
      case Times | Shl =>
        // TODO
        lhs.requiredSize max rhs.requiredSize
      case _ => lhs.requiredSize max rhs.requiredSize
    }
  }

  override def fitsProvablyIntoByte: Boolean = {
    import MathOperator._
    operator match {
      case And => lhs.fitsProvablyIntoByte || rhs.fitsProvablyIntoByte
      case Or | Exor => lhs.fitsProvablyIntoByte && rhs.fitsProvablyIntoByte
      case Shr | Shr9 => lhs.fitsProvablyIntoByte
      case Plus => (lhs, rhs) match {
        case (MemoryAddressConstant(thing), offset) => thing.name == "__reg" && offset.fitsProvablyIntoByte
        case (offset, MemoryAddressConstant(thing)) => thing.name == "__reg" && offset.fitsProvablyIntoByte
        case _ => false
      }
      case Minus => (lhs, rhs) match {
        case (MemoryAddressConstant(thing), offset) => thing.name == "__reg" && offset.fitsProvablyIntoByte
        case _ => false
      }
      case _ => false
    }
  }

  override def isRelatedTo(v: Thing): Boolean = lhs.isRelatedTo(v) || rhs.isRelatedTo(v)

  override def refersTo(name: String): Boolean = lhs.refersTo(name) || rhs.refersTo(name)
}
