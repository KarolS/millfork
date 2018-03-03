package millfork.assembly.opt

import millfork.{CompilationFlag, CompilationOptions}
import millfork.assembly.{AssemblyLine, OpcodeClasses, State}
import millfork.env.{Label, MemoryAddressConstant, NormalFunction, NumericConstant}

import scala.collection.immutable

/**
  * @author Karol Stasiak
  */

sealed trait Status[T] {
  def contains(value: T): Boolean

  def ~(that: Status[T]): Status[T] = {
    (this, that) match {
      case (AnyStatus(), _) => AnyStatus()
      case (_, AnyStatus()) => AnyStatus()
      case (SingleStatus(x), SingleStatus(y)) => if (x == y) SingleStatus(x) else AnyStatus()
      case (SingleStatus(x), UnknownStatus()) => SingleStatus(x)
      case (UnknownStatus(), SingleStatus(x)) => SingleStatus(x)
      case (UnknownStatus(), UnknownStatus()) => UnknownStatus()
    }
  }

}

object Status {

  implicit class BoolStatusOps(val inner: Status[Boolean]) extends AnyVal {
    def withHiddenHi: Status[Boolean] = inner match {
      case SingleStatus(false) => inner
      case _ => AnyStatus()
    }
  }
  implicit class IntStatusOps(val inner: Status[Int]) extends AnyVal {
    def map[T](f: Int => T): Status[T] = inner match {
      case SingleStatus(x) => SingleStatus(f(x))
      case _ => AnyStatus()
    }

    def z(f: Int => Int = identity): Status[Boolean] = inner match {
      case SingleStatus(x) =>
        val y = f(x) & 0xff
        SingleStatus(y == 0)
      case _ => AnyStatus()
    }

    def n(f: Int => Int = identity): Status[Boolean] = inner match {
      case SingleStatus(x) =>
        val y = f(x) & 0xff
        SingleStatus(y >= 0x80)
      case _ => AnyStatus()
    }

    def adc(value: Int, carry: Status[Boolean], decimal: Status[Boolean]): Status[Int] = inner match {
      case SingleStatus(x) => decimal match {
        case SingleStatus(false) => carry match {
          case SingleStatus(true) => SingleStatus(x + value + 1)
          case SingleStatus(false) => SingleStatus(x + value)
          case _ => AnyStatus()
        }
        case _ => AnyStatus()
      }
      case _ => AnyStatus()
    }
  }

}


case class SingleStatus[T](t: T) extends Status[T] {
  override def contains(value: T): Boolean = t == value

  override def toString: String = t match {
    case true => "1"
    case false => "0"
    case _ => t.toString
  }
}

case class UnknownStatus[T]() extends Status[T] {
  override def contains(value: T) = false

  override def toString: String = "_"
}

case class AnyStatus[T]() extends Status[T] {
  override def contains(value: T) = false

  override def toString: String = "#"
}
//noinspection RedundantNewCaseClass
case class CpuStatus(a: Status[Int] = UnknownStatus(),
                     x: Status[Int] = UnknownStatus(),
                     y: Status[Int] = UnknownStatus(),
                     iz: Status[Int] = UnknownStatus(),
                     z: Status[Boolean] = UnknownStatus(),
                     n: Status[Boolean] = UnknownStatus(),
                     c: Status[Boolean] = UnknownStatus(),
                     v: Status[Boolean] = UnknownStatus(),
                     d: Status[Boolean] = UnknownStatus(),
                     m: Status[Boolean] = UnknownStatus(),
                     w: Status[Boolean] = UnknownStatus()
                    ) {

  override def toString: String = s"A=$a,X=$x,Y=$y,Z=$z,N=$n,C=$c,V=$v,D=$d"

  def nz: CpuStatus =
    this.copy(n = AnyStatus(), z = AnyStatus())

  def nz(i: Long): CpuStatus =
    this.copy(n = SingleStatus((i & 0x80) != 0), z = SingleStatus((i & 0xff) == 0))

  def ~(that: CpuStatus) = new CpuStatus(
    a = this.a ~ that.a,
    x = this.x ~ that.x,
    y = this.y ~ that.y,
    iz = this.iz ~ that.iz,
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

object CoarseFlowAnalyzer {
  //noinspection RedundantNewCaseClass
  def analyze(f: NormalFunction, code: List[AssemblyLine], compilationOptions: CompilationOptions): List[CpuStatus] = {
    val emptyIz: Status[Int] = if (compilationOptions.flag(CompilationFlag.Emit65CE02Opcodes)) UnknownStatus() else SingleStatus(0)
    val emptyStatus = CpuStatus(iz = emptyIz)
    val flagArray = Array.fill[CpuStatus](code.length)(emptyStatus)
    val codeArray = code.toArray
    val initialStatus = new CpuStatus(
      d = SingleStatus(false),
      m = SingleStatus(true),
      w = SingleStatus(true),
      iz = emptyIz
    )

    var changed = true
    while (changed) {
      changed = false
      var currentStatus: CpuStatus = if (f.interrupt) emptyStatus else initialStatus
      for (i <- codeArray.indices) {
        import millfork.assembly.Opcode._
        import millfork.assembly.AddrMode._
        if (flagArray(i) != currentStatus) {
          changed = true
          flagArray(i) = currentStatus
        }
        codeArray(i) match {
          case AssemblyLine(LABEL, _, MemoryAddressConstant(Label(l)), _) =>
            val L = l
            currentStatus = codeArray.indices.flatMap(j => codeArray(j) match {
              case AssemblyLine(_, _, MemoryAddressConstant(Label(L)), _) => Some(flagArray(j))
              case _ => None
            }).fold(emptyStatus)(_ ~ _)

          case AssemblyLine(BCC, _, _, _) =>
            currentStatus = currentStatus.copy(c = currentStatus.c ~ SingleStatus(true))
          case AssemblyLine(BCS, _, _, _) =>
            currentStatus = currentStatus.copy(c = currentStatus.c ~ SingleStatus(false))
          case AssemblyLine(BVS, _, _, _) =>
            currentStatus = currentStatus.copy(v = currentStatus.v ~ SingleStatus(false))
          case AssemblyLine(BVC, _, _, _) =>
            currentStatus = currentStatus.copy(v = currentStatus.v ~ SingleStatus(true))
          case AssemblyLine(BMI, _, _, _) =>
            currentStatus = currentStatus.copy(n = currentStatus.n ~ SingleStatus(false))
          case AssemblyLine(BPL, _, _, _) =>
            currentStatus = currentStatus.copy(n = currentStatus.n ~ SingleStatus(true))
          case AssemblyLine(BEQ, _, _, _) =>
            currentStatus = currentStatus.copy(z = currentStatus.z ~ SingleStatus(false))
          case AssemblyLine(BNE, _, _, _) =>
            currentStatus = currentStatus.copy(z = currentStatus.z ~ SingleStatus(true))

          case AssemblyLine(SED, _, _, _) =>
            currentStatus = currentStatus.copy(d = SingleStatus(true))
          case AssemblyLine(SEC, _, _, _) =>
            currentStatus = currentStatus.copy(c = SingleStatus(true))
          case AssemblyLine(CLD, _, _, _) =>
            currentStatus = currentStatus.copy(d = SingleStatus(false))
          case AssemblyLine(CLC, _, _, _) =>
            currentStatus = currentStatus.copy(c = SingleStatus(false))
          case AssemblyLine(CLV, _, _, _) =>
            currentStatus = currentStatus.copy(v = SingleStatus(false))

          case AssemblyLine(REP, Immediate, NumericConstant(nn, _), _) =>
            if ((nn & 1) != 0) currentStatus = currentStatus.copy(c = SingleStatus(false))
            if ((nn & 2) != 0) currentStatus = currentStatus.copy(z = SingleStatus(false))
            if ((nn & 8) != 0) currentStatus = currentStatus.copy(d = SingleStatus(false))
            if ((nn & 0x10) != 0) currentStatus = currentStatus.copy(w = SingleStatus(false))
            if ((nn & 0x20) != 0) currentStatus = currentStatus.copy(m = SingleStatus(false))
            if ((nn & 0x40) != 0) currentStatus = currentStatus.copy(v = SingleStatus(false))
            if ((nn & 0x80) != 0) currentStatus = currentStatus.copy(n = SingleStatus(false))
          case AssemblyLine(SEP, Immediate, NumericConstant(nn, _), _) =>
            if ((nn & 1) != 0) currentStatus = currentStatus.copy(c = SingleStatus(true))
            if ((nn & 2) != 0) currentStatus = currentStatus.copy(z = SingleStatus(true))
            if ((nn & 8) != 0) currentStatus = currentStatus.copy(d = SingleStatus(true))
            if ((nn & 0x10) != 0) currentStatus = currentStatus.copy(w = SingleStatus(true))
            if ((nn & 0x20) != 0) currentStatus = currentStatus.copy(m = SingleStatus(true))
            if ((nn & 0x40) != 0) currentStatus = currentStatus.copy(v = SingleStatus(true))
            if ((nn & 0x80) != 0) currentStatus = currentStatus.copy(n = SingleStatus(true))
          case AssemblyLine(XCE, _, _, _) =>
            currentStatus = currentStatus.copy(c = AnyStatus(), m = AnyStatus(), x = AnyStatus())

          case AssemblyLine(JSR, _, _, _) =>
            currentStatus = initialStatus

          case AssemblyLine(LDX, Immediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt & 0xff
            currentStatus = currentStatus.nz(n).copy(x = SingleStatus(n))
          case AssemblyLine(LDY, Immediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt & 0xff
            currentStatus = currentStatus.nz(n).copy(y = SingleStatus(n))
          case AssemblyLine(LDA, Immediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt & 0xff
            currentStatus = currentStatus.nz(n).copy(a = SingleStatus(n))
          case AssemblyLine(LDZ, Immediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt & 0xff
            currentStatus = currentStatus.nz(n).copy(iz = SingleStatus(n))

          case AssemblyLine(ADC, Immediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt
            val newA = currentStatus.a.adc(n, currentStatus.c, currentStatus.d)
            currentStatus = currentStatus.copy(n = newA.n(), z = newA.z(), a = newA, c = AnyStatus(), v = AnyStatus())
          case AssemblyLine(EOR, Immediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt
            currentStatus = currentStatus.copy(n = currentStatus.a.n(_ ^ n), z = currentStatus.a.z(_ ^ n), a = currentStatus.a.map(_ ^ n))
          case AssemblyLine(AND, Immediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt
            currentStatus = currentStatus.copy(n = currentStatus.a.n(_ & n), z = currentStatus.a.z(_ & n), a = currentStatus.a.map(_ & n))
          case AssemblyLine(ANC, Immediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt
            currentStatus = currentStatus.copy(n = currentStatus.a.n(_ & n), c = currentStatus.a.n(_ & n), z = currentStatus.x.z(_ & n), a = currentStatus.a.map(_ & n))
          case AssemblyLine(ORA, Immediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt
            currentStatus = currentStatus.copy(n = currentStatus.a.n(_ | n), z = currentStatus.a.z(_ | n), a = currentStatus.a.map(_ | n))
          case AssemblyLine(ALR, Immediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt
            currentStatus = currentStatus.copy(
              n = currentStatus.a.n(i => (i & n & 0xff) >> 1),
              z = currentStatus.a.z(i => (i & n & 0xff) >> 1),
              c = currentStatus.a.map(i => (i & n & 1) == 0),
              a = currentStatus.a.map(i => (i & n & 0xff) >> 1))


          case AssemblyLine(ADC_W, WordImmediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt & 0xff
            val newA = currentStatus.a.adc(n, currentStatus.c, currentStatus.d)
            currentStatus = currentStatus.copy(n = AnyStatus(), z = newA.z().withHiddenHi, a = newA, c = AnyStatus(), v = AnyStatus())
          case AssemblyLine(EOR_W, WordImmediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt & 0xff
            currentStatus = currentStatus.copy(n = AnyStatus(), z = currentStatus.a.z(_ ^ n).withHiddenHi, a = currentStatus.a.map(_ ^ n))
          case AssemblyLine(AND_W, WordImmediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt & 0xff
            currentStatus = currentStatus.copy(n = AnyStatus(), z = currentStatus.a.z(_ & n).withHiddenHi, a = currentStatus.a.map(_ & n))
          case AssemblyLine(ORA_W, WordImmediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt & 0xff
            currentStatus = currentStatus.copy(n = AnyStatus(), z = currentStatus.a.z(_ | n).withHiddenHi, a = currentStatus.a.map(_ | n))

          case AssemblyLine(INX, Implied, _, _) =>
            currentStatus = currentStatus.copy(n = currentStatus.x.n(_ + 1), z = currentStatus.x.z(_ + 1), x = currentStatus.x.map(v => (v + 1) & 0xff))
          case AssemblyLine(DEX, Implied, _, _) =>
            currentStatus = currentStatus.copy(n = currentStatus.x.n(_ - 1), z = currentStatus.x.z(_ - 1), x = currentStatus.x.map(v => (v - 1) & 0xff))
          case AssemblyLine(INY, Implied, _, _) =>
            currentStatus = currentStatus.copy(n = currentStatus.y.n(_ + 1), z = currentStatus.y.z(_ + 1), y = currentStatus.y.map(v => (v + 1) & 0xff))
          case AssemblyLine(DEY, Implied, _, _) =>
            currentStatus = currentStatus.copy(n = currentStatus.y.n(_ - 1), z = currentStatus.y.z(_ - 1), y = currentStatus.y.map(v => (v - 1) & 0xff))
          case AssemblyLine(INC, Implied, _, _) =>
            currentStatus = currentStatus.copy(n = currentStatus.a.n(_ + 1), z = currentStatus.a.z(_ + 1), a = currentStatus.a.map(v => (v + 1) & 0xff))
          case AssemblyLine(DEC, Implied, _, _) =>
            currentStatus = currentStatus.copy(n = currentStatus.a.n(_ - 1), z = currentStatus.a.z(_ - 1), a = currentStatus.a.map(v => (v - 1) & 0xff))
          case AssemblyLine(NEG, Implied, _, _) =>
            currentStatus = currentStatus.copy(n = currentStatus.a.n(256 - _), z = currentStatus.a.z(256 - _), a = currentStatus.a.map(v => (256 - v) & 0xff))

          case AssemblyLine(INX_W, Implied, _, _) =>
            currentStatus = currentStatus.copy(n = AnyStatus(), z = currentStatus.x.z(_ + 1).withHiddenHi, x = currentStatus.x.map(v => (v + 1) & 0xff))
          case AssemblyLine(DEX_W, Implied, _, _) =>
            currentStatus = currentStatus.copy(n = AnyStatus(), z = currentStatus.x.z(_ - 1).withHiddenHi, x = currentStatus.x.map(v => (v - 1) & 0xff))
          case AssemblyLine(INY_W, Implied, _, _) =>
            currentStatus = currentStatus.copy(n = AnyStatus(), z = currentStatus.y.z(_ + 1).withHiddenHi, y = currentStatus.y.map(v => (v + 1) & 0xff))
          case AssemblyLine(DEY_W, Implied, _, _) =>
            currentStatus = currentStatus.copy(n = AnyStatus(), z = currentStatus.y.z(_ - 1).withHiddenHi, y = currentStatus.y.map(v => (v - 1) & 0xff))
          case AssemblyLine(INC_W, Implied, _, _) =>
            currentStatus = currentStatus.copy(n = AnyStatus(), z = currentStatus.a.z(_ + 1).withHiddenHi, a = currentStatus.a.map(v => (v + 1) & 0xff))
          case AssemblyLine(DEC_W, Implied, _, _) =>
            currentStatus = currentStatus.copy(n = AnyStatus(), z = currentStatus.a.z(_ - 1).withHiddenHi, a = currentStatus.a.map(v => (v - 1) & 0xff))

          case AssemblyLine(TAX, _, _, _) =>
            currentStatus = currentStatus.copy(x = currentStatus.a, n = currentStatus.a.n(), z = currentStatus.a.z())
          case AssemblyLine(TXA, _, _, _) =>
            currentStatus = currentStatus.copy(a = currentStatus.x, n = currentStatus.x.n(), z = currentStatus.x.z())
          case AssemblyLine(TAY, _, _, _) =>
            currentStatus = currentStatus.copy(y = currentStatus.a, n = currentStatus.a.n(), z = currentStatus.a.z())
          case AssemblyLine(TYA, _, _, _) =>
            currentStatus = currentStatus.copy(a = currentStatus.y, n = currentStatus.y.n(), z = currentStatus.y.z())
          case AssemblyLine(TAZ, _, _, _) =>
            currentStatus = currentStatus.copy(iz = currentStatus.a, n = currentStatus.a.n(), z = currentStatus.a.z())
          case AssemblyLine(TZA, _, _, _) =>
            currentStatus = currentStatus.copy(a = currentStatus.iz, n = currentStatus.iz.n(), z = currentStatus.iz.z())

          case AssemblyLine(ASL, Implied, _, _) =>
            currentStatus = currentStatus.copy(a = currentStatus.a.map(v => (v << 1) & 0xff), n = currentStatus.a.n(_ << 1), z = currentStatus.a.z(_ << 1),c = currentStatus.a.map(a => a.&(0xff).!=(0)))
          case AssemblyLine(LSR, Implied, _, _) =>
            currentStatus = currentStatus.copy(a = currentStatus.a.map(a => a.>>(1).&(0x7f)), n = currentStatus.a.n(a => a.>>(1).&(0x7f)), z = currentStatus.a.z(a => a.>>(1).&(0x7f)),c = currentStatus.a.map(a => a.&(1).!=(0)))
          case AssemblyLine(ASR, Implied, _, _) =>
            currentStatus = currentStatus.copy(a = currentStatus.a.map(a => a.toByte.>>(1).&(0xff)), n = currentStatus.a.n(a => a.toByte.>>(1).&(0xff)), z = currentStatus.a.z(a => a.toByte.>>(1).&(0xff)),c = currentStatus.a.map(a => a.&(1).!=(0)))

          case AssemblyLine(ASL_W, Implied, _, _) =>
            currentStatus = currentStatus.copy(a = currentStatus.a.map(v => (v << 1) & 0xff), n = AnyStatus(), z = currentStatus.a.z(_ << 1).withHiddenHi, c = AnyStatus())
          case AssemblyLine(LSR_W, Implied, _, _) =>
            currentStatus = currentStatus.copy(a = AnyStatus(), n = AnyStatus(), z = currentStatus.a.z(a => a.>>(1).&(0x7f)).withHiddenHi, c = currentStatus.a.map(a => a.&(1).!=(0)))

          case AssemblyLine(opcode, addrMode, parameter, _) =>
            if (OpcodeClasses.ChangesX(opcode)) currentStatus = currentStatus.copy(x = AnyStatus())
            if (OpcodeClasses.ChangesY(opcode)) currentStatus = currentStatus.copy(y = AnyStatus())
            if (OpcodeClasses.ChangesAAlways(opcode)) currentStatus = currentStatus.copy(a = AnyStatus())
            if (addrMode == Implied && OpcodeClasses.ChangesAIfImplied(opcode)) currentStatus = currentStatus.copy(a = AnyStatus())
            if (OpcodeClasses.ChangesNAndZ(opcode)) currentStatus = currentStatus.nz
            if (OpcodeClasses.ChangesC(opcode)) currentStatus = currentStatus.copy(c = AnyStatus())
            if (OpcodeClasses.ChangesV(opcode)) currentStatus = currentStatus.copy(v = AnyStatus())
            if (opcode == CMP || opcode == CPX || opcode == CPY) {
              if (addrMode == Immediate) parameter match {
                case NumericConstant(0, _) => currentStatus = currentStatus.copy(c = SingleStatus(true))
                case _ => ()
              }
            }
        }
      }
//                  flagArray.zip(codeArray).foreach{
//                    case (fl, y) => if (y.isPrintable) println(f"$fl%-32s $y%-32s")
//                  }
//                  println("---------------------")
    }

    flagArray.toList
  }
}
