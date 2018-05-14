package millfork.env

import millfork.error.ErrorReporting
import millfork.node.Position

object Constant {
  val Zero: Constant = NumericConstant(0, 1)
  val One: Constant = NumericConstant(1, 1)

  def error(msg: String, position: Option[Position] = None): Constant = {
    ErrorReporting.error(msg, position)
    Zero
  }

  def minimumSize(value: Long): Int = if (value < -128 || value > 255) 2 else 1 // TODO !!!
}

import millfork.env.Constant.minimumSize
import millfork.error.ErrorReporting
import millfork.node.Position

sealed trait Constant {
  def isProvablyZero: Boolean = false

  def asl(i: Constant): Constant = i match {
    case NumericConstant(sa, _) => asl(sa.toInt)
    case _ => CompoundConstant(MathOperator.Shl, this, i)
  }

  def asl(i: Int): Constant = CompoundConstant(MathOperator.Shl, this, NumericConstant(i, 1))

  def requiredSize: Int

  def +(that: Constant): Constant = CompoundConstant(MathOperator.Plus, this, that)

  def -(that: Constant): Constant = CompoundConstant(MathOperator.Minus, this, that)

  def +(that: Long): Constant = if (that == 0) this else this + NumericConstant(that, minimumSize(that))

  def -(that: Long): Constant = if (that == 0) this else this - NumericConstant(that, minimumSize(that))

  def loByte: Constant = {
    if (requiredSize == 1) return this
    SubbyteConstant(this, 0)
  }

  def hiByte: Constant = {
    if (requiredSize == 1) Constant.Zero
    else SubbyteConstant(this, 1)
  }

  def subbyte(index: Int): Constant = {
    if (requiredSize <= index) Constant.Zero
    else index match {
      case 0 => loByte
      case 1 => hiByte
      case _ => SubbyteConstant(this, index)
    }
  }

  def subword(index: Int): Constant = {
    if (requiredSize <= index) Constant.Zero
    else {
      // TODO: check if ok
      CompoundConstant(MathOperator.Or, CompoundConstant(MathOperator.Shl, subbyte(index+1), NumericConstant(8, 1)), subbyte(0)).quickSimplify
    }
  }

  def isLowestByteAlwaysEqual(i: Int) : Boolean = false

  def quickSimplify: Constant = this

  def isRelatedTo(v: Variable): Boolean
}

case class AssertByte(c: Constant) extends Constant {
  override def isProvablyZero: Boolean = c.isProvablyZero

  override def requiredSize: Int = 1

  override def isRelatedTo(v: Variable): Boolean = c.isRelatedTo(v)

  override def quickSimplify: Constant = AssertByte(c.quickSimplify)
}

case class UnexpandedConstant(name: String, requiredSize: Int) extends Constant {
  override def isRelatedTo(v: Variable): Boolean = false
}

case class NumericConstant(value: Long, requiredSize: Int) extends Constant {
  if (requiredSize == 1) {
    if (value < -128 || value > 255) {
      throw new IllegalArgumentException("The constant is too big")
    }
  }
  override def isProvablyZero: Boolean = value == 0

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

  override def isRelatedTo(v: Variable): Boolean = false
}

case class MemoryAddressConstant(var thing: ThingInMemory) extends Constant {
  override def requiredSize = 2

  override def toString: String = thing.name

  override def isRelatedTo(v: Variable): Boolean = thing.name == v.name
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
      case _ => SubbyteConstant(simplified, index)
    }
  }

  override def requiredSize = 1

  override def toString: String = base + (index match {
    case 0 => ".lo"
    case 1 => ".hi"
    case 2 => ".b2"
    case 3 => ".b3"
  })

  override def isRelatedTo(v: Variable): Boolean = base.isRelatedTo(v)
}

object MathOperator extends Enumeration {
  val Plus, Minus, Times, Shl, Shr, Shl9, Shr9, Plus9, DecimalPlus9,
  DecimalPlus, DecimalMinus, DecimalTimes, DecimalShl, DecimalShl9, DecimalShr,
  And, Or, Exor = Value
}

case class CompoundConstant(operator: MathOperator.Value, lhs: Constant, rhs: Constant) extends Constant {
  override def quickSimplify: Constant = {
    val l = lhs.quickSimplify
    val r = rhs.quickSimplify
    (l, r) match {
      case (CompoundConstant(MathOperator.Plus, a, ll@NumericConstant(lv, _)), rr@NumericConstant(rv,_)) if operator == MathOperator.Plus =>
        CompoundConstant(MathOperator.Plus, a, ll + rr).quickSimplify
      case (CompoundConstant(MathOperator.Minus, a, ll@NumericConstant(lv, _)), rr@NumericConstant(rv,_)) if operator == MathOperator.Minus =>
        CompoundConstant(MathOperator.Minus, a, ll + rr).quickSimplify
      case (CompoundConstant(MathOperator.Plus, a, ll@NumericConstant(lv, _)), rr@NumericConstant(rv,_)) if operator == MathOperator.Minus =>
        if (lv >= rv) {
          CompoundConstant(MathOperator.Plus, a, ll - rr).quickSimplify
        } else {
          CompoundConstant(MathOperator.Minus, a, rr - ll).quickSimplify
        }
      case (CompoundConstant(MathOperator.Minus, a, ll@NumericConstant(lv, _)), rr@NumericConstant(rv,_)) if operator == MathOperator.Plus =>
        if (lv >= rv) {
          CompoundConstant(MathOperator.Minus, a, ll - rr).quickSimplify
        } else {
          CompoundConstant(MathOperator.Plus, a, rr - ll).quickSimplify
        }
      case (CompoundConstant(MathOperator.Shl, SubbyteConstant(c1, 1), NumericConstant(8, _)), SubbyteConstant(c2, 0)) if operator == MathOperator.Or && c1 == c2 => c1
      case (NumericConstant(lv, ls), NumericConstant(rv, rs)) =>
        var size = ls max rs
        val value = operator match {
          case MathOperator.Plus => lv + rv
          case MathOperator.Minus => lv - rv
          case MathOperator.Times => lv * rv
          case MathOperator.Shl => lv << rv
          case MathOperator.Shr => lv >> rv
          case MathOperator.Shl9 => (lv << rv) & 0x1ff
          case MathOperator.Plus9 => (lv + rv) & 0x1ff
          case MathOperator.Shr9 => (lv & 0x1ff) >> rv
          case MathOperator.Exor => lv ^ rv
          case MathOperator.Or => lv | rv
          case MathOperator.And => lv & rv
          case _ => return this
        }
        operator match {
          case MathOperator.Plus9 | MathOperator.DecimalPlus9 =>
            size = 2
          case MathOperator.Times | MathOperator.Shl =>
            val mask = (1 << (size * 8)) - 1
            if (value != (value & mask)){
              size = ls + rs
            }
          case _ =>
        }
        NumericConstant(value, size)
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
          case _ => CompoundConstant(operator, l, r)
        }
      case (c, NumericConstant(0, 1)) =>
        operator match {
          case MathOperator.Plus => c
          case MathOperator.Plus9 => c
          case MathOperator.DecimalPlus => c
          case MathOperator.DecimalPlus9 => c
          case MathOperator.Minus => c
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

  private def plhs = lhs match {
    case _: NumericConstant | _: MemoryAddressConstant => lhs
    case _ => "(" + lhs + ')'
  }

  private def prhs = lhs match {
    case _: NumericConstant | _: MemoryAddressConstant => rhs
    case _ => "(" + rhs + ')'
  }

  override def toString: String = {
    operator match {
      case Plus => f"$plhs + $prhs"
      case Plus9 => f"nonet($plhs + $prhs)"
      case Minus => f"$plhs - $prhs"
      case Times => f"$plhs * $prhs"
      case Shl => f"$plhs << $prhs"
      case Shr => f"$plhs >> $prhs"
      case Shl9 => f"nonet($plhs << $prhs)"
      case Shr9 => f"$plhs >>>> $prhs"
      case DecimalPlus => f"$plhs +' $prhs"
      case DecimalPlus9 => f"nonet($plhs +' $prhs)"
      case DecimalMinus => f"$plhs -' $prhs"
      case DecimalTimes => f"$plhs *' $prhs"
      case DecimalShl => f"$plhs <<' $prhs"
      case DecimalShl9 => f"nonet($plhs <<' $prhs)"
      case DecimalShr => f"$plhs >>' $prhs"
      case And => f"$plhs & $prhs"
      case Or => f"$plhs | $prhs"
      case Exor => f"$plhs ^ $prhs"
    }
  }

  override def requiredSize: Int = lhs.requiredSize max rhs.requiredSize

  override def isRelatedTo(v: Variable): Boolean = lhs.isRelatedTo(v) || rhs.isRelatedTo(v)
}