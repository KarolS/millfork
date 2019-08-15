package millfork.assembly.m6809.opt

import millfork.assembly.mos.State
import millfork.assembly.opt._
import millfork.env.Constant

//noinspection RedundantNewCaseClass
case class CpuStatus(a: Status[Int] = UnknownStatus,
                     b0: Status[Boolean] = UnknownStatus,
                     b7: Status[Boolean] = UnknownStatus,
                     memStack: Map[Int, Status[Int]] = Map(),
                     b: Status[Int] = UnknownStatus,
                     x: Status[Constant] = UnknownStatus,
                     y: Status[Constant] = UnknownStatus,
                     u: Status[Constant] = UnknownStatus,
                     z: Status[Boolean] = UnknownStatus,
                     n: Status[Boolean] = UnknownStatus,
                     c: Status[Boolean] = UnknownStatus,
                     v: Status[Boolean] = UnknownStatus
                    ) {


  override def toString: String = s"A=$a,B=$b,X=$x,Y=$y; Z=$z,N=$n,C=$c,V=$v; B7=$b7,B0=$b0"

  def d: Status[Int] = (a, b) match {
    case (SingleStatus(h), SingleStatus(l)) => SingleStatus(h.&(0xff).<<(8).+(l&0xff))
    case (UnknownStatus, UnknownStatus) => UnknownStatus
    case _ => AnyStatus
  }

  def nz: CpuStatus =
    this.copy(n = AnyStatus, z = AnyStatus)

  def nz(i: Long): CpuStatus =
    this.copy(n = SingleStatus((i & 0x80) != 0), z = SingleStatus((i & 0xff) == 0))

  def ~(that: CpuStatus) = new CpuStatus(
    a = this.a ~ that.a,
    b = this.b ~ that.b,
    b7 = this.b7 ~ that.b7,
    b0 = this.b0 ~ that.b0,
    x = this.x ~ that.x,
    y = this.y ~ that.y,
    u = this.u ~ that.u,
    memStack = (this.memStack.keySet | that.memStack.keySet).map(k => k -> (this.memStack.getOrElse(k, UnknownStatus) ~ that.memStack.getOrElse(k, UnknownStatus))).toMap,
    z = this.z ~ that.z,
    n = this.n ~ that.n,
    c = this.c ~ that.c,
    v = this.v ~ that.v
  )

}

object CpuStatus {
  val initialStatusStandard = CpuStatus(
        a = AnyStatus,
        b = AnyStatus,
        b0 = AnyStatus,
        b7 = AnyStatus,
        x = AnyStatus,
        y = AnyStatus,
        u = AnyStatus,
        c = AnyStatus,
        v = AnyStatus,
        z = AnyStatus,
        n = AnyStatus,
      )
}