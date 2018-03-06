package millfork.assembly.opt

import millfork.assembly.State

/**
  * @author Karol Stasiak
  */


sealed trait Status[+T] {

  def |[U >: T](value: => Status[U]): Status[U] = this match {
    case AnyStatus | UnknownStatus => value
    case x => x
  }

  def contains[U >: T](value: U): Boolean

  def ~[U >: T](that: Status[U]): Status[U] = {
    (this, that) match {
      case (AnyStatus, _) => AnyStatus
      case (_, AnyStatus) => AnyStatus
      case (SingleStatus(x), SingleStatus(y)) => if (x == y) SingleStatus(x) else AnyStatus
      case (SingleStatus(x), UnknownStatus) => SingleStatus(x)
      case (UnknownStatus, SingleStatus(x)) => SingleStatus(x)
      case (UnknownStatus, UnknownStatus) => UnknownStatus
    }
  }

  def <*>[U, V](that: Status[U])(f: (T,U) => V): Status[V] = (this, that) match {
    case (SingleStatus(t), SingleStatus(u)) => SingleStatus(f(t, u))
    case (UnknownStatus, UnknownStatus) => UnknownStatus
    case _ => AnyStatus
  }

  def map[U](f: T => U): Status[U] = this match {
    case SingleStatus(x) => SingleStatus(f(x))
    case _ => AnyStatus
  }

  def flatMap[U](f: T => Status[U]): Status[U] = this match {
    case SingleStatus(x) => f(x)
    case _ => AnyStatus
  }
}

object SourceOfNZ {
  val A = SingleStatus(SourceOfNZ(a = true))
  val AW = SingleStatus(SourceOfNZ(aw = true))
  val X = SingleStatus(SourceOfNZ(x = true))
  val Y = SingleStatus(SourceOfNZ(y = true))
  val Z = SingleStatus(SourceOfNZ(iz = true))
  val AX = SingleStatus(SourceOfNZ(a = true, x = true))
  val AY = SingleStatus(SourceOfNZ(a = true, y = true))
  val AZ = SingleStatus(SourceOfNZ(a = true, iz = true))
  val XY = SingleStatus(SourceOfNZ(x = true, y = true))
}

case class SourceOfNZ(a: Boolean = false, aw: Boolean = false, x: Boolean = false, y: Boolean = false, iz: Boolean = false) {
  override def toString: String = {
    val builder = new StringBuilder
    if (a) builder += 'A'
    if (aw) builder += 'C'
    if (x) builder += 'X'
    if (y) builder += 'Y'
    if (iz) builder += 'Z'
    if (builder.isEmpty) "?"
    else builder.mkString
  }
}

object Status {

  val SingleTrue: Status[Boolean] = SingleStatus(true)
  val SingleFalse: Status[Boolean] = SingleStatus(false)
  val SingleZero: Status[Int] = Status.SingleZero
  val SingleFF: Status[Int] = Status.SingleFF
  @inline
  private def wrapBool(b: Boolean) = if (b) SingleTrue else SingleFalse

  def flatMap2[T, U, R](a: Status[T], b: Status[U])(f: (T, U) => Status[R]): Status[R] = (a, b) match {
    case (SingleStatus(t), SingleStatus(u)) => f(t, u)
    case (UnknownStatus, UnknownStatus) => UnknownStatus
    case _ => AnyStatus
  }

  def flatMap3[T, U, V, R](a: Status[T], b: Status[U], c: Status[V])(f: (T, U, V) => Status[R]): Status[R] = (a, b, c) match {
    case (SingleStatus(t), SingleStatus(u), SingleStatus(v)) => f(t, u, v)
    case (UnknownStatus, UnknownStatus, UnknownStatus) => UnknownStatus
    case _ => AnyStatus
  }

  implicit class BoolStatusOps(val inner: Status[Boolean]) extends AnyVal {
    def withHiddenHi: Status[Boolean] = inner match {
      case SingleStatus(false) => inner
      case _ => AnyStatus
    }
    def negate: Status[Boolean] = inner match {
      case SingleStatus(x) => wrapBool(!x)
      case x => x
    }
  }
  implicit class SourceOfNZStatusOps(val inner: Status[SourceOfNZ]) extends AnyVal {
    def isFromA: Boolean = inner match {
      case SingleStatus(v) => v.a
      case _ => false
    }
    def isFromAW: Boolean = inner match {
      case SingleStatus(v) => v.aw
      case _ => false
    }
    def isFromX: Boolean = inner match {
      case SingleStatus(v) => v.x
      case _ => false
    }
    def isFromY: Boolean = inner match {
      case SingleStatus(v) => v.y
      case _ => false
    }
    def isFromIZ: Boolean = inner match {
      case SingleStatus(v) => v.iz
      case _ => false
    }
  }
  implicit class IntStatusOps(val inner: Status[Int]) extends AnyVal {

    def bit0: Status[Boolean] = inner match {
      case SingleStatus(x) => SingleStatus((x & 1) == 0)
      case _ => AnyStatus
    }

    def bit7: Status[Boolean] = inner match {
      case SingleStatus(x) => SingleStatus((x & 0x80) == 0)
      case _ => AnyStatus
    }

    def z(f: Int => Int = identity): Status[Boolean] = inner match {
      case SingleStatus(x) =>
        val y = f(x) & 0xff
        wrapBool(y == 0)
      case _ => AnyStatus
    }

    def n(f: Int => Int = identity): Status[Boolean] = inner match {
      case SingleStatus(x) =>
        val y = f(x) & 0xff
        wrapBool(y >= 0x80)
      case _ => AnyStatus
    }

    def zw(f: Int => Int = identity): Status[Boolean] = inner match {
      case SingleStatus(x) =>
        val y = f(x) & 0xffff
        wrapBool(y == 0)
      case _ => AnyStatus
    }

    def nw(f: Int => Int = identity): Status[Boolean] = inner match {
      case SingleStatus(x) =>
        val y = f(x) & 0xffff
        wrapBool(y >= 0x8000)
      case _ => AnyStatus
    }

    def lo: Status[Int] = inner match {
      case SingleStatus(x) => SingleStatus(x & 0xff)
      case _ => AnyStatus
    }

    def hi: Status[Int] = inner match {
      case SingleStatus(x) => SingleStatus(x.&(0xff00).>>(8))
      case _ => AnyStatus
    }

    def adc(value: Int, carry: Status[Boolean], decimal: Status[Boolean]): Status[Int] = inner match {
      case SingleStatus(x) => decimal match {
        case SingleStatus(false) => carry match {
          case SingleStatus(true) => SingleStatus((x + value + 1) & 0xff)
          case SingleStatus(false) => SingleStatus((x + value) & 0xff)
          case _ => AnyStatus
        }
        case _ => AnyStatus
      }
      case _ => AnyStatus
    }

    def sbc(value: Int, carry: Status[Boolean], decimal: Status[Boolean]): Status[Int] = inner match {
      case SingleStatus(x) => decimal match {
        case SingleStatus(false) => carry match {
          case SingleStatus(true) => SingleStatus((x - value) & 0xff)
          case SingleStatus(false) => SingleStatus((x - value - 1) & 0xff)
          case _ => AnyStatus
        }
        case _ => AnyStatus
      }
      case _ => AnyStatus
    }

    def adc_w(value: Int, carry: Status[Boolean], decimal: Status[Boolean]): Status[Int] = inner match {
      case SingleStatus(x) => decimal match {
        case SingleStatus(false) => carry match {
          case SingleStatus(true) => SingleStatus((x + value + 1) & 0xffff)
          case SingleStatus(false) => SingleStatus((x + value) & 0xffff)
          case _ => AnyStatus
        }
        case _ => AnyStatus
      }
      case _ => AnyStatus
    }
  }

}


case class SingleStatus[T](t: T) extends Status[T] {
  override def contains[U >: T](value: U): Boolean = t == value

  override def toString: String = t match {
    case true => "1"
    case false => "0"
    case _ => t.toString
  }
}

case object UnknownStatus extends Status[Nothing] {
  override def contains[U >: Nothing](value: U) = false

  override def toString: String = "_"
}

case object AnyStatus extends Status[Nothing] {
  override def contains[U >: Nothing](value: U) = false

  override def toString: String = "#"
}
//noinspection RedundantNewCaseClass
case class CpuStatus(a: Status[Int] = UnknownStatus,
                     ah: Status[Int] = UnknownStatus,
                     a0: Status[Boolean] = UnknownStatus,
                     a7: Status[Boolean] = UnknownStatus,
                     x: Status[Int] = UnknownStatus,
                     y: Status[Int] = UnknownStatus,
                     iz: Status[Int] = UnknownStatus,
                     src: Status[SourceOfNZ] = UnknownStatus,
                     z: Status[Boolean] = UnknownStatus,
                     n: Status[Boolean] = UnknownStatus,
                     c: Status[Boolean] = UnknownStatus,
                     v: Status[Boolean] = UnknownStatus,
                     d: Status[Boolean] = UnknownStatus,
                     m: Status[Boolean] = UnknownStatus,
                     w: Status[Boolean] = UnknownStatus
                    ) {

  override def toString: String = s"A=$a,B=$ah,X=$x,Y=$y,Z=$iz; Z=$z,N=$n,C=$c,V=$v,D=$d,M=$m,X=$w; A7=$a7,A0=$a0,NZ:$src"

  def aw: Status[Int] = (ah, a) match {
    case (SingleStatus(h), SingleStatus(l)) => SingleStatus(h.&(0xff).<<(8).+(l&0xff))
    case (UnknownStatus, UnknownStatus) => UnknownStatus
    case _ => AnyStatus
  }

  def nz: CpuStatus =
    this.copy(n = AnyStatus, z = AnyStatus)

  def nz(i: Long): CpuStatus =
    this.copy(n = SingleStatus((i & 0x80) != 0), z = SingleStatus((i & 0xff) == 0))

  def nzw(i: Long): CpuStatus =
    this.copy(n = SingleStatus((i & 0x8000) != 0), z = SingleStatus((i & 0xffff) == 0))

  def ~(that: CpuStatus) = new CpuStatus(
    a = this.a ~ that.a,
    ah = this.ah ~ that.ah,
    a7 = this.a7 ~ that.a7,
    a0 = this.a0 ~ that.a0,
    x = this.x ~ that.x,
    y = this.y ~ that.y,
    iz = this.iz ~ that.iz,
    src = this.src ~ that.src,
    z = this.z ~ that.z,
    n = this.n ~ that.n,
    c = this.c ~ that.c,
    v = this.v ~ that.v,
    d = this.d ~ that.d,
    m = this.m ~ that.m,
    w = this.w ~ that.w,
  )

  def hasClear(state: State.Value): Boolean = state match {
    case State.A => a.contains(0)
    case State.AH => ah.contains(0)
    case State.X => x.contains(0)
    case State.Y => y.contains(0)
    case State.IZ => iz.contains(0)
    case State.Z => z.contains(false)
    case State.N => n.contains(false)
    case State.C => c.contains(false)
    case State.V => v.contains(false)
    case State.D => d.contains(false)
    case State.M => m.contains(false)
    case State.W => w.contains(false)
    case _ => false
  }

  def hasSet(state: State.Value): Boolean = state match {
    case State.A => false
    case State.AH => false
    case State.X => false
    case State.Y => false
    case State.IZ => false
    case State.Z => z.contains(true)
    case State.N => n.contains(true)
    case State.C => c.contains(true)
    case State.V => v.contains(true)
    case State.D => d.contains(true)
    case State.M => m.contains(true)
    case State.W => w.contains(true)
    case _ => false
  }
}
