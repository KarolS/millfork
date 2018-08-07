package millfork.assembly.mos.opt

import millfork.assembly.mos.Opcode
import millfork.assembly.mos.Opcode._
import millfork.assembly.opt.{AnyStatus, SingleStatus, Status}

/**
  * @author Karol Stasiak
  */
object FlowAnalyzerForTheRest {
  private val map: Map[Opcode.Value, (CpuStatus, Option[Int]) => CpuStatus] = Map(
    STA -> ((c,zpreg) => c.setReg(zpreg, c.a)),
    STX -> ((c,zpreg) => c.setReg(zpreg, c.x)),
    STY -> ((c,zpreg) => c.setReg(zpreg, c.y)),
    STZ -> ((c,zpreg) => c.setReg(zpreg, c.iz)),
    SAX -> ((c,zpreg) => c.setReg(zpreg, (c.a <*> c.x)(_ & _))),
    NOP -> ((x,_) => x),
    DISCARD_AF -> ((x,_) => x),
    DISCARD_XF -> ((x,_) => x),
    DISCARD_YF -> ((x,_) => x),
    REP -> ((currentStatus, zpreg) => {
      currentStatus.copy(c = AnyStatus, d = AnyStatus, n = AnyStatus, z= AnyStatus, v = AnyStatus, m = AnyStatus, w = AnyStatus)
    }),
    SEP -> ((currentStatus, zpreg) => {
      currentStatus.copy(c = AnyStatus, d = AnyStatus, n = AnyStatus, z= AnyStatus, v = AnyStatus, m = AnyStatus, w = AnyStatus)
    }),
    BCC -> ((currentStatus, zpreg) => {
      currentStatus.copy(c = Status.SingleTrue)
    }),
    BCS -> ((currentStatus, zpreg) => {
      currentStatus.copy(c = Status.SingleFalse)
    }),
    BVS -> ((currentStatus, zpreg) => {
      currentStatus.copy(v = Status.SingleFalse)
    }),
    BVC -> ((currentStatus, zpreg) => {
      currentStatus.copy(v = Status.SingleTrue)
    }),
    BMI -> ((c, zpreg) => {
      var currentStatus = c
      currentStatus = currentStatus.copy(n = Status.SingleFalse)
      if (currentStatus.src.isFromA) {
        currentStatus = currentStatus.copy(a = currentStatus.a.flatMap(aa => if (aa.&(0x80)!=0) SingleStatus(aa) else AnyStatus), a7 = Status.SingleTrue)
      }
      currentStatus
    }),
    BPL -> ((c, zpreg) => {
      var currentStatus = c
      currentStatus = currentStatus.copy(n = Status.SingleTrue)
      if (currentStatus.src.isFromA) {
        currentStatus = currentStatus.copy(a = currentStatus.a.flatMap(aa => if (aa.&(0x80)==0) SingleStatus(aa) else AnyStatus), a7 = Status.SingleFalse)
      }
      currentStatus
    }),
    BEQ -> ((currentStatus, zpreg) => {
      currentStatus.copy(z = Status.SingleFalse)
    }),
    BNE -> ((c, zpreg) => {
      var currentStatus = c
      currentStatus = currentStatus.copy(z = Status.SingleTrue)
      if (currentStatus.src.isFromA) {
        currentStatus = currentStatus.copy(a7 = Status.SingleFalse, a0 = Status.SingleFalse, a = Status.SingleZero)
      }
      if (currentStatus.src.isFromAW) {
        currentStatus = currentStatus.copy(
          a7 = Status.SingleFalse,
          a0 = Status.SingleFalse,
          a = Status.SingleZero,
          ah = Status.SingleZero)
      }
      if (currentStatus.src.isFromX) {
        currentStatus = currentStatus.copy(x = Status.SingleZero)
      }
      if (currentStatus.src.isFromY) {
        currentStatus = currentStatus.copy(y = Status.SingleZero)
      }
      if (currentStatus.src.isFromIZ) {
        currentStatus = currentStatus.copy(iz = Status.SingleZero)
      }
      currentStatus
    }),
    LDX -> ((currentStatus, zpreg) => {
      val newX = currentStatus.getReg(zpreg)
      currentStatus.copy(
        x = newX,
        n = newX.n(),
        z = newX.z(),
        src = SourceOfNZ.X)
    }),
    LDY -> ((currentStatus, zpreg) => {
      val newY = currentStatus.getReg(zpreg)
      currentStatus.copy(
        y = newY,
        n = newY.n(),
        z = newY.z(),
        src = SourceOfNZ.Y)
    }),
    LDA -> ((currentStatus, zpreg) => {
      val newA = currentStatus.getReg(zpreg)
      currentStatus.copy(
        a = newA,
        a7 = newA.bit7,
        a0 = newA.bit0,
        n = newA.n(),
        z = newA.z(),
        src = SourceOfNZ.A)
    }),
    LDZ -> ((currentStatus, zpreg) => {
      val newZ = currentStatus.getReg(zpreg)
      currentStatus.copy(
        iz = newZ,
        n = newZ.n(),
        z = newZ.z(),
        src = SourceOfNZ.Z)
    }),
    LAX -> ((currentStatus, zpreg) => {
      val newA = currentStatus.getReg(zpreg)
      currentStatus.copy(
        x = newA,
        a = newA,
        a7 = newA.bit7,
        a0 = newA.bit0,
        n = newA.n(),
        z = newA.z(),
        src = SourceOfNZ.AX)
    }),
    LDA_W -> ((currentStatus, zpreg) => {
      val newA = currentStatus.getReg(zpreg)
      val newAH = currentStatus.getRegHi(zpreg)
      currentStatus.copy(
        ah = newAH,
        a = newA,
        a7 = AnyStatus,
        a0 = newA.bit0,
        n = AnyStatus,
        z = AnyStatus,
        src = SourceOfNZ.AW)
    }),
    LDX_W -> ((currentStatus, zpreg) => {
      currentStatus.copy(
        x = AnyStatus,
        n = AnyStatus,
        z = AnyStatus,
        src = AnyStatus)
    }),
    LDY_W -> ((currentStatus, zpreg) => {
      currentStatus.copy(
        y = AnyStatus,
        n = AnyStatus,
        z = AnyStatus,
        src = AnyStatus)
    }),
    ADC -> ((currentStatus, zpreg) => {
      val r = currentStatus.getReg(zpreg)
      val newA: Status[Int]= if (currentStatus.d.contains(false)) Status.flatMap3(currentStatus.a, r, currentStatus.c) {
        case (m, n, false) => SingleStatus((m + n) & 0xff)
        case (m, n, true) => SingleStatus((m + n + 1) & 0xff)
      } else AnyStatus
      val newC: Status[Boolean]= if (currentStatus.d.contains(false)) Status.flatMap3(currentStatus.a, r, currentStatus.c) {
        case (m, n, false) => SingleStatus((m.&(0xff) + n.&(0xff)) > 0xff)
        case (m, n, true) => SingleStatus((m.&(0xff) + n.&(0xff) + 1) > 0xff)
      } else AnyStatus
      currentStatus.copy(
        a = newA,
        a7 = newA.bit7,
        a0 = newA.bit0,
        v = AnyStatus,
        c = newC | Status.flatMap3(currentStatus.a, currentStatus.c, currentStatus.d) {
          case (0, false, false) => SingleStatus[Boolean](false)
          case _ => AnyStatus
        },
        z = newA.z(),
        n = newA.n(),
        src = currentStatus.d.flatMap((dec: Boolean) => if (dec) AnyStatus else SourceOfNZ.A))
    }),
    SBC -> ((currentStatus, zpreg) => {
      val r = currentStatus.getReg(zpreg)
      val newA: Status[Int]= if (currentStatus.d.contains(false)) Status.flatMap3(currentStatus.a, r, currentStatus.c) {
        case (m, n, false) => SingleStatus((m - n + 0xff) & 0xff)
        case (m, n, true) => SingleStatus((m - n + 0x100) & 0xff)
      } else AnyStatus
      currentStatus.copy(
        a = newA,
        a7 = newA.bit7,
        a0 = newA.bit0,
        v = AnyStatus,
        c = Status.flatMap3(currentStatus.a, currentStatus.c, currentStatus.d) {
          case (0xff, true, false) => SingleStatus[Boolean](true)
          case _ => AnyStatus
        },
        z = newA.z(),
        n = newA.n(),
        src = currentStatus.d.flatMap((dec: Boolean) => if (dec) AnyStatus else SourceOfNZ.A))
    }),
    AND -> ((currentStatus, zpreg) => {
      val newA: Status[Int] =
        currentStatus.a.flatMap(v => if ((v & 0xff) == 0) Status.SingleZero else AnyStatus) |
          (currentStatus.a <*> currentStatus.getReg(zpreg))(_ & _)
      currentStatus.copy(
        a = newA,
        a7 = currentStatus.a7.flatMap(v => if (!v) Status.SingleFalse else AnyStatus),
        a0 = currentStatus.a0.flatMap(v => if (!v) Status.SingleFalse else AnyStatus),
        n = newA.n(),
        z = newA.z(),
        src = SourceOfNZ.A)
    }),
    ORA -> ((currentStatus, zpreg) => {
      val newA: Status[Int] =
        currentStatus.a.flatMap(v => if ((v & 0xff) == 0xff) Status.SingleFF else AnyStatus) |
          (currentStatus.a <*> currentStatus.getReg(zpreg))(_ | _)
      currentStatus.copy(
        a = newA,
        a7 = currentStatus.a7.flatMap(v => if (v) Status.SingleTrue else AnyStatus),
        a0 = currentStatus.a0.flatMap(v => if (v) Status.SingleTrue else AnyStatus),
        n = newA.n(),
        z = newA.z(),
        src = SourceOfNZ.A)
    }),
    SLO -> ((currentStatus, zpreg) => {
      val newA: Status[Int] = currentStatus.a.flatMap(v => if ((v & 0xff) == 0xff) Status.SingleFF else AnyStatus)
      currentStatus.copy(
        c = AnyStatus,
        a = newA,
        a7 = currentStatus.a7.flatMap(v => if (v) Status.SingleTrue else AnyStatus),
        a0 = currentStatus.a0.flatMap(v => if (v) Status.SingleTrue else AnyStatus),
        n = newA.n(),
        z = newA.z(),
        src = SourceOfNZ.A).setReg(zpreg, AnyStatus)
    }),
    EOR -> ((currentStatus, zpreg) => {
      val newA: Status[Int] = (currentStatus.a <*> currentStatus.getReg(zpreg))(_ ^ _)
      currentStatus.copy(
        n = newA.n(),
        z = newA.z(),
        a = newA,
        a7 = newA.bit7,
        a0 = newA.bit0,
        src = SourceOfNZ.A).setReg(zpreg, AnyStatus)
    }),
    SRE -> ((currentStatus, zpreg) => {
      currentStatus.copy(
        c = AnyStatus,
        n = AnyStatus,
        z = AnyStatus,
        a = AnyStatus,
        a7 = AnyStatus,
        a0 = AnyStatus,
        src = SourceOfNZ.A).setReg(zpreg, AnyStatus)
    }),
    RLA -> ((currentStatus, zpreg) => {
      currentStatus.copy(
        c = AnyStatus,
        n = AnyStatus,
        z = AnyStatus,
        a = AnyStatus,
        a7 = AnyStatus,
        a0 = AnyStatus,
        src = SourceOfNZ.A).setReg(zpreg, AnyStatus)
    }),
    RRA -> ((currentStatus, zpreg) => {
      currentStatus.copy(
        c = AnyStatus,
        n = AnyStatus,
        v = AnyStatus,
        a = AnyStatus,
        a7 = AnyStatus,
        a0 = AnyStatus,
        z = AnyStatus,
        src = currentStatus.d.flatMap(dec => if (dec) AnyStatus else SourceOfNZ.A)).setReg(zpreg, AnyStatus)
    }),
    DCP -> ((currentStatus, zpreg) => {
      val r = currentStatus.getReg(zpreg)
      currentStatus.copy(
        c = AnyStatus,
        n = AnyStatus,
        z = AnyStatus,
        src = AnyStatus).setReg(zpreg, r.map(n => (n - 1) & 0xff))
    }),
    ISC -> ((currentStatus, zpreg) => {
      val r = currentStatus.getReg(zpreg)
      currentStatus.copy(
        c = AnyStatus,
        n = AnyStatus,
        v = AnyStatus,
        a = AnyStatus,
        a7 = AnyStatus,
        a0 = AnyStatus,
        z = AnyStatus,
        src = currentStatus.d.flatMap(dec => if (dec) AnyStatus else SourceOfNZ.A)).setReg(zpreg, r.map(n => (n + 1) & 0xff))
    }),
    ROL -> ((currentStatus, zpreg) => {
      val r = currentStatus.getReg(zpreg)
      currentStatus.copy(c = r.bit7, n = AnyStatus, z = AnyStatus, src = AnyStatus).setReg(zpreg, currentStatus.c.flatMap { c =>
        r.map(n => (n << 1) & 0xff | (if (c) 1 else 0))
      })
    }),
    ROR -> ((currentStatus, zpreg) => {
      val r = currentStatus.getReg(zpreg)
      currentStatus.copy(c = r.bit0, n = AnyStatus, z = AnyStatus, src = AnyStatus).setReg(zpreg, currentStatus.c.flatMap { c =>
        r.map(n => (n >> 1) & 0x7f | (if (c) 0x80 else 0))
      })
    }),
    ASL -> ((currentStatus, zpreg) => {
      val r = currentStatus.getReg(zpreg)
      currentStatus.copy(c = r.bit7, n = AnyStatus, z = AnyStatus, src = AnyStatus).setReg(zpreg, r.map(n => (n << 1) & 0xff))
    }),
    LSR -> ((currentStatus, zpreg) => {
      val r = currentStatus.getReg(zpreg)
      currentStatus.copy(c = r.bit0, n = AnyStatus, z = AnyStatus, src = AnyStatus).setReg(zpreg, r.map(n => (n >> 1) & 0xff))
    }),
    INC -> ((currentStatus, zpreg) => {
      val r = currentStatus.getReg(zpreg)
      currentStatus.copy(n = AnyStatus, z = AnyStatus, src = AnyStatus).setReg(zpreg, r.map(n => (n + 1) & 0xff))
    }),
    DEC -> ((currentStatus, zpreg) => {
      val r = currentStatus.getReg(zpreg)
      currentStatus.copy(n = AnyStatus, z = AnyStatus, src = AnyStatus).setReg(zpreg, r.map(n => (n - 1) & 0xff))
    }),
    CMP -> ((currentStatus, zpreg) => {
      val r = currentStatus.getReg(zpreg)
      currentStatus.copy(c = AnyStatus, n = AnyStatus, z = (currentStatus.a <*> r)(_==_), src = if (r.contains(0)) SourceOfNZ.A else AnyStatus)
    }),
    CPX -> ((currentStatus, zpreg) => {
      val r = currentStatus.getReg(zpreg)
      currentStatus.copy(c = AnyStatus, n = AnyStatus, z = (currentStatus.x <*> r)(_==_), src = if (r.contains(0)) SourceOfNZ.X else AnyStatus)
    }),
    CPY -> ((currentStatus, zpreg) => {
      val r = currentStatus.getReg(zpreg)
      currentStatus.copy(c = AnyStatus, n = AnyStatus, z = (currentStatus.y <*> r)(_==_), src = if (r.contains(0)) SourceOfNZ.Y else AnyStatus)
    }),
    CPZ -> ((currentStatus, zpreg) => {
      val r = currentStatus.getReg(zpreg)
      currentStatus.copy(c = AnyStatus, n = AnyStatus, z = (currentStatus.iz <*> r)(_==_), src = if (r.contains(0)) SourceOfNZ.Z else AnyStatus)
    }),
    BIT -> ((currentStatus, zpreg) => currentStatus.copy(v = AnyStatus, n = AnyStatus, z = AnyStatus, src = AnyStatus)),
    TRB -> ((currentStatus, zpreg) => currentStatus.copy(z = AnyStatus, src = AnyStatus).setReg(zpreg, AnyStatus)),
    TSB -> ((currentStatus, zpreg) => currentStatus.copy(z = AnyStatus, src = AnyStatus).setReg(zpreg, AnyStatus)),
    JMP -> ((_,_) => CpuStatus()),
    BRA -> ((_,_) => CpuStatus()),
    BRL -> ((_,_) => CpuStatus()),
  )

  def hasDefinition(opcode: Opcode.Value): Boolean = map.contains(opcode)

  def get(opcode: Opcode.Value): (CpuStatus, Option[Int]) => CpuStatus = map(opcode)
}
