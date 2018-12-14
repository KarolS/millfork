package millfork.assembly.mos.opt

import millfork.assembly.mos.Opcode
import millfork.assembly.mos.Opcode._
import millfork.assembly.opt.{AnyStatus, SingleStatus, Status}

/**
  * @author Karol Stasiak
  */
object FlowAnalyzerForTheRest {
  private val map: Map[Opcode.Value, (CpuStatus, Option[Int], Boolean) => CpuStatus] = Map(
    STA -> ((c, zpreg, sp) => c.setReg(zpreg, c.a).overwriteSp(sp)),
    STX -> ((c, zpreg, sp) => {
      if (sp) c.copy(eqSpX = true)
      else c.setReg(zpreg, c.x)
    }),
    STY -> ((c, zpreg, sp) => c.setReg(zpreg, c.y).overwriteSp(sp)),
    STZ -> ((c, zpreg, sp) => c.setReg(zpreg, c.iz).overwriteSp(sp)),
    SAX -> ((c, zpreg, sp) => c.setReg(zpreg, (c.a <*> c.x) (_ & _)).copy(eqSpX = false)),
    NOP -> ((x, _, _) => x),
    DISCARD_AF -> ((x, _, _) => x),
    DISCARD_XF -> ((x, _, _) => x),
    DISCARD_YF -> ((x, _, _) => x),
    REP -> ((currentStatus, zpreg, sp) => {
      currentStatus.copy(c = AnyStatus, d = AnyStatus, n = AnyStatus, z= AnyStatus, v = AnyStatus, m = AnyStatus, w = AnyStatus)
    }),
    SEP -> ((currentStatus, zpreg, sp) => {
      currentStatus.copy(c = AnyStatus, d = AnyStatus, n = AnyStatus, z= AnyStatus, v = AnyStatus, m = AnyStatus, w = AnyStatus)
    }),
    BCC -> ((currentStatus, zpreg, sp) => {
      currentStatus.copy(c = Status.SingleTrue)
    }),
    BCS -> ((currentStatus, zpreg, sp) => {
      currentStatus.copy(c = Status.SingleFalse)
    }),
    BVS -> ((currentStatus, zpreg, sp) => {
      currentStatus.copy(v = Status.SingleFalse)
    }),
    BVC -> ((currentStatus, zpreg, sp) => {
      currentStatus.copy(v = Status.SingleTrue)
    }),
    BMI -> ((c, zpreg,_) => {
      var currentStatus = c
      currentStatus = currentStatus.copy(n = Status.SingleFalse)
      if (currentStatus.src.isFromA) {
        currentStatus = currentStatus.copy(a = currentStatus.a.flatMap(aa => if (aa.&(0x80)!=0) SingleStatus(aa) else AnyStatus), a7 = Status.SingleTrue)
      }
      currentStatus
    }),
    BPL -> ((c, zpreg,_) => {
      var currentStatus = c
      currentStatus = currentStatus.copy(n = Status.SingleTrue)
      if (currentStatus.src.isFromA) {
        currentStatus = currentStatus.copy(a = currentStatus.a.flatMap(aa => if (aa.&(0x80)==0) SingleStatus(aa) else AnyStatus), a7 = Status.SingleFalse)
      }
      currentStatus
    }),
    BEQ -> ((currentStatus, zpreg, sp) => {
      currentStatus.copy(z = Status.SingleFalse)
    }),
    BNE -> ((c, zpreg,_) => {
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
    LDX -> ((currentStatus, zpreg, sp) => {
      val newX = currentStatus.getReg(zpreg)
      currentStatus.copy(
        x = newX,
        n = newX.n(),
        z = newX.z(),
        eqSX = false,
        eqSpX = sp,
        src = SourceOfNZ.X)
    }),
    LDY -> ((currentStatus, zpreg, sp) => {
      val newY = currentStatus.getReg(zpreg)
      currentStatus.copy(
        y = newY,
        n = newY.n(),
        z = newY.z(),
        src = SourceOfNZ.Y)
    }),
    LDA -> ((currentStatus, zpreg, sp) => {
      val newA = currentStatus.getReg(zpreg)
      currentStatus.copy(
        a = newA,
        a7 = newA.bit7,
        a0 = newA.bit0,
        n = newA.n(),
        z = newA.z(),
        src = SourceOfNZ.A)
    }),
    LDZ -> ((currentStatus, zpreg, sp) => {
      val newZ = currentStatus.getReg(zpreg)
      currentStatus.copy(
        iz = newZ,
        n = newZ.n(),
        z = newZ.z(),
        src = SourceOfNZ.Z)
    }),
    LAX -> ((currentStatus, zpreg, sp) => {
      val newA = currentStatus.getReg(zpreg)
      currentStatus.copy(
        x = newA,
        a = newA,
        a7 = newA.bit7,
        a0 = newA.bit0,
        n = newA.n(),
        z = newA.z(),
        eqSX = false,
        eqSpX = sp,
        src = SourceOfNZ.AX)
    }),
    LDA_W -> ((currentStatus, zpreg, sp) => {
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
    LDX_W -> ((currentStatus, zpreg, sp) => {
      currentStatus.copy(
        x = AnyStatus,
        n = AnyStatus,
        z = AnyStatus,
        eqSX = false,
        eqSpX = false,
        src = AnyStatus)
    }),
    LDY_W -> ((currentStatus, zpreg, sp) => {
      currentStatus.copy(
        y = AnyStatus,
        n = AnyStatus,
        z = AnyStatus,
        src = AnyStatus)
    }),
    ADC -> ((currentStatus, zpreg, sp) => {
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
    SBC -> ((currentStatus, zpreg, sp) => {
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
    AND -> ((currentStatus, zpreg, sp) => {
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
    ORA -> ((currentStatus, zpreg, sp) => {
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
    SLO -> ((currentStatus, zpreg, sp) => {
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
    EOR -> ((currentStatus, zpreg, sp) => {
      val newA: Status[Int] = (currentStatus.a <*> currentStatus.getReg(zpreg))(_ ^ _)
      currentStatus.copy(
        n = newA.n(),
        z = newA.z(),
        a = newA,
        a7 = newA.bit7,
        a0 = newA.bit0,
        src = SourceOfNZ.A).setReg(zpreg, AnyStatus)
    }),
    SRE -> ((currentStatus, zpreg, sp) => {
      currentStatus.copy(
        c = AnyStatus,
        n = AnyStatus,
        z = AnyStatus,
        a = AnyStatus,
        a7 = AnyStatus,
        a0 = AnyStatus,
        src = SourceOfNZ.A).setReg(zpreg, AnyStatus).overwriteSp(sp)
    }),
    RLA -> ((currentStatus, zpreg, sp) => {
      currentStatus.copy(
        c = AnyStatus,
        n = AnyStatus,
        z = AnyStatus,
        a = AnyStatus,
        a7 = AnyStatus,
        a0 = AnyStatus,
        src = SourceOfNZ.A).setReg(zpreg, AnyStatus).overwriteSp(sp)
    }),
    RRA -> ((currentStatus, zpreg, sp) => {
      currentStatus.copy(
        c = AnyStatus,
        n = AnyStatus,
        v = AnyStatus,
        a = AnyStatus,
        a7 = AnyStatus,
        a0 = AnyStatus,
        z = AnyStatus,
        src = currentStatus.d.flatMap(dec => if (dec) AnyStatus else SourceOfNZ.A)).setReg(zpreg, AnyStatus).overwriteSp(sp)
    }),
    DCP -> ((currentStatus, zpreg, sp) => {
      val r = currentStatus.getReg(zpreg)
      currentStatus.copy(
        c = AnyStatus,
        n = AnyStatus,
        z = AnyStatus,
        src = AnyStatus).setReg(zpreg, r.map(n => (n - 1) & 0xff)).overwriteSp(sp)
    }),
    ISC -> ((currentStatus, zpreg, sp) => {
      val r = currentStatus.getReg(zpreg)
      currentStatus.copy(
        c = AnyStatus,
        n = AnyStatus,
        v = AnyStatus,
        a = AnyStatus,
        a7 = AnyStatus,
        a0 = AnyStatus,
        z = AnyStatus,
        src = currentStatus.d.flatMap(dec => if (dec) AnyStatus else SourceOfNZ.A)).setReg(zpreg, r.map(n => (n + 1) & 0xff)).overwriteSp(sp)
    }),
    ROL -> ((currentStatus, zpreg, sp) => {
      val r = currentStatus.getReg(zpreg)
      currentStatus.copy(c = r.bit7, n = AnyStatus, z = AnyStatus, src = AnyStatus).setReg(zpreg, currentStatus.c.flatMap { c =>
        r.map(n => (n << 1) & 0xff | (if (c) 1 else 0))
      }).overwriteSp(sp)
    }),
    ROR -> ((currentStatus, zpreg, sp) => {
      val r = currentStatus.getReg(zpreg)
      currentStatus.copy(c = r.bit0, n = AnyStatus, z = AnyStatus, src = AnyStatus).setReg(zpreg, currentStatus.c.flatMap { c =>
        r.map(n => (n >> 1) & 0x7f | (if (c) 0x80 else 0))
      }).overwriteSp(sp)
    }),
    ASL -> ((currentStatus, zpreg, sp) => {
      val r = currentStatus.getReg(zpreg)
      currentStatus.copy(c = r.bit7, n = AnyStatus, z = AnyStatus, src = AnyStatus).setReg(zpreg, r.map(n => (n << 1) & 0xff)).overwriteSp(sp)
    }),
    LSR -> ((currentStatus, zpreg, sp) => {
      val r = currentStatus.getReg(zpreg)
      currentStatus.copy(c = r.bit0, n = AnyStatus, z = AnyStatus, src = AnyStatus).setReg(zpreg, r.map(n => (n >> 1) & 0xff)).overwriteSp(sp)
    }),
    INC -> ((currentStatus, zpreg, sp) => {
      val r = currentStatus.getReg(zpreg)
      currentStatus.copy(n = AnyStatus, z = AnyStatus, src = AnyStatus).setReg(zpreg, r.map(n => (n + 1) & 0xff)).overwriteSp(sp)
    }),
    DEC -> ((currentStatus, zpreg, sp) => {
      val r = currentStatus.getReg(zpreg)
      currentStatus.copy(n = AnyStatus, z = AnyStatus, src = AnyStatus).setReg(zpreg, r.map(n => (n - 1) & 0xff)).overwriteSp(sp)
    }),
    CMP -> ((currentStatus, zpreg, sp) => {
      val r = currentStatus.getReg(zpreg)
      currentStatus.copy(c = AnyStatus, n = AnyStatus, z = (currentStatus.a <*> r)(_==_), src = if (r.contains(0)) SourceOfNZ.A else AnyStatus)
    }),
    CPX -> ((currentStatus, zpreg, sp) => {
      val r = currentStatus.getReg(zpreg)
      currentStatus.copy(c = AnyStatus, n = AnyStatus, z = (currentStatus.x <*> r)(_==_), src = if (r.contains(0)) SourceOfNZ.X else AnyStatus)
    }),
    CPY -> ((currentStatus, zpreg, sp) => {
      val r = currentStatus.getReg(zpreg)
      currentStatus.copy(c = AnyStatus, n = AnyStatus, z = (currentStatus.y <*> r)(_==_), src = if (r.contains(0)) SourceOfNZ.Y else AnyStatus)
    }),
    CPZ -> ((currentStatus, zpreg, sp) => {
      val r = currentStatus.getReg(zpreg)
      currentStatus.copy(c = AnyStatus, n = AnyStatus, z = (currentStatus.iz <*> r)(_==_), src = if (r.contains(0)) SourceOfNZ.Z else AnyStatus)
    }),
    BIT -> ((currentStatus, zpreg, sp) => currentStatus.copy(v = AnyStatus, n = AnyStatus, z = AnyStatus, src = AnyStatus)),
    TRB -> ((currentStatus, zpreg, sp) => currentStatus.copy(z = AnyStatus, src = AnyStatus).setReg(zpreg, AnyStatus)),
    TSB -> ((currentStatus, zpreg, sp) => currentStatus.copy(z = AnyStatus, src = AnyStatus).setReg(zpreg, AnyStatus)),
    JMP -> ((_,_,_) => CpuStatus()),
    BRA -> ((_,_,_) => CpuStatus()),
    BRL -> ((_,_,_) => CpuStatus()),
  )

  def hasDefinition(opcode: Opcode.Value): Boolean = map.contains(opcode)

  def get(opcode: Opcode.Value): (CpuStatus, Option[Int], Boolean) => CpuStatus = map(opcode)
}
