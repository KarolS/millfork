package millfork.assembly.mos.opt

import millfork.assembly.mos.State
import millfork.assembly.opt._

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
  def matches(state: State.Value): Boolean = state match {
    case State.A => a
    case State.X => x
    case State.Y => y
    case State.IZ => iz
    case _ => throw new IllegalArgumentException
  }

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




//noinspection RedundantNewCaseClass
case class CpuStatus(a: Status[Int] = UnknownStatus,
                     ah: Status[Int] = UnknownStatus,
                     a0: Status[Boolean] = UnknownStatus,
                     a7: Status[Boolean] = UnknownStatus,
                     x: Status[Int] = UnknownStatus,
                     y: Status[Int] = UnknownStatus,
                     iz: Status[Int] = UnknownStatus,
                     src: Status[SourceOfNZ] = UnknownStatus,
                     eqSX: Boolean = false,
                     z: Status[Boolean] = UnknownStatus,
                     n: Status[Boolean] = UnknownStatus,
                     c: Status[Boolean] = UnknownStatus,
                     v: Status[Boolean] = UnknownStatus,
                     d: Status[Boolean] = UnknownStatus,
                     m: Status[Boolean] = UnknownStatus,
                     w: Status[Boolean] = UnknownStatus
                    ) {
//  assert(a ne null)
//  assert(ah ne null)
//  assert(x ne null)
//  assert(y ne null)
//  assert(iz ne null)
//  assert(z ne null)
//  assert(n ne null)
//  assert(v ne null)
//  assert(c ne null)
//  assert(d ne null)
//  assert(m ne null)
//  assert(w ne null)
//  assert(a0 ne null)
//  assert(a7 ne null)
//  (a, a7) match {
//    case (SingleStatus(o), SingleStatus(b7)) => if (o.&(0x80).!=(0).!=(b7)) {
//      println(a)
//      println(a7)
//      println(a0)
//      ???
//    }
//    case _ =>
//  }
//  (a, a0) match {
//    case (SingleStatus(o), SingleStatus(b0)) => if (o.&(1).!=(0).!=(b0)) {
//      println(a)
//      println(a7)
//      println(a0)
//      ???
//    }
//    case _ =>
//  }

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
    eqSX = this.eqSX && that.eqSX,
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

object CpuStatus {
  val initialStatusStandard = CpuStatus(
        a = AnyStatus,
        x = AnyStatus,
        y = AnyStatus,
        c = AnyStatus,
        v = AnyStatus,
        z = AnyStatus,
        n = AnyStatus,
        d = SingleStatus(false),
        m = SingleStatus(true),
        w = SingleStatus(true),
        iz = SingleStatus(0)
      )
  val initialStatusCE = CpuStatus(
        a = AnyStatus,
        x = AnyStatus,
        y = AnyStatus,
        c = AnyStatus,
        v = AnyStatus,
        z = AnyStatus,
        n = AnyStatus,
        d = SingleStatus(false),
        m = SingleStatus(true),
        w = SingleStatus(true),
        iz = AnyStatus
      )

  val initialInterruptStatusStandard = CpuStatus(
        a = AnyStatus,
        x = AnyStatus,
        y = AnyStatus,
        c = AnyStatus,
        v = AnyStatus,
        z = AnyStatus,
        n = AnyStatus,
        d = AnyStatus,
        m = AnyStatus,
        w = AnyStatus,
        iz = SingleStatus(0)
      )
  val initialInterruptStatusCmos = CpuStatus(
        a = AnyStatus,
        x = AnyStatus,
        y = AnyStatus,
        c = AnyStatus,
        v = AnyStatus,
        z = AnyStatus,
        n = AnyStatus,
        d = SingleStatus(false),
        m = AnyStatus,
        w = AnyStatus,
        iz = SingleStatus(0)
      )
  val initialInterruptStatusCE = CpuStatus(
        a = AnyStatus,
        x = AnyStatus,
        y = AnyStatus,
        c = AnyStatus,
        v = AnyStatus,
        z = AnyStatus,
        n = AnyStatus,
        d = SingleStatus(false),
        m = AnyStatus,
        w = AnyStatus,
        iz = AnyStatus
      )

  val emptyStatusStandard = CpuStatus(iz = SingleStatus(0))

  val emptyStatusCE = CpuStatus(iz = AnyStatus)
}