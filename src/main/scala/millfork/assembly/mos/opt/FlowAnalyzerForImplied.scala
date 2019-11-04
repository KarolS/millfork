package millfork.assembly.mos.opt

import millfork.assembly.mos.Opcode
import millfork.assembly.mos.Opcode._
import millfork.assembly.opt.{AnyStatus, Status}

/**
  * @author Karol Stasiak
  */
object FlowAnalyzerForImplied {
  private val map: Map[Opcode.Value, CpuStatus => CpuStatus] = Map(
    NOP -> identity,
    RTS -> identity,
    RTL -> identity,
    RTI -> identity,
    SEI -> identity,
    CLI -> identity,
    TXS -> (_.copy(eqSX = true, eqSpX = false)),
    PHP -> (_.copy(eqSX = false)),
    PHA -> (_.copy(eqSX = false)),
    PHA_W -> (_.copy(eqSX = false)),
    PHX -> (_.copy(eqSX = false)),
    PHX_W -> (_.copy(eqSX = false)),
    PHY -> (_.copy(eqSX = false)),
    PHY_W -> (_.copy(eqSX = false)),
    PHZ -> (_.copy(eqSX = false)),
    PHD -> (_.copy(eqSX = false)),
    PHK -> (_.copy(eqSX = false)),
    PHB -> (_.copy(eqSX = false)),
    SED -> (_.copy(d = Status.SingleTrue)),
    CLD -> (_.copy(d = Status.SingleFalse)),
    CLC -> (_.copy(c = Status.SingleFalse)),
    SEC -> (_.copy(c = Status.SingleTrue)),
    CLV -> (_.copy(v = Status.SingleFalse)),
    CLA -> (currentStatus => currentStatus.copy(a = Status.SingleZero, src = currentStatus.src.butNotA)),
    CLX -> (currentStatus => currentStatus.copy(x = Status.SingleZero, src = currentStatus.src.butNotX, eqSX = false, eqSpX = false)),
    CLY -> (currentStatus => currentStatus.copy(y = Status.SingleZero, src = currentStatus.src.butNotY)),
    XCE -> (_.copy(c = AnyStatus, m = AnyStatus, w = AnyStatus, x = AnyStatus, y = AnyStatus, eqSX = false)),
    PLA -> (_.copy(src = SourceOfNZ.A, a = AnyStatus, n = AnyStatus, z = AnyStatus, eqSX = false)),
    PLX -> (_.copy(src = SourceOfNZ.X, x = AnyStatus, n = AnyStatus, z = AnyStatus, eqSX = false, eqSpX = false)),
    PLX_W -> (_.copy(src = SourceOfNZ.X, x = AnyStatus, n = AnyStatus, z = AnyStatus, eqSX = false, eqSpX = false)),
    PLY -> (_.copy(src = SourceOfNZ.Y, y = AnyStatus, n = AnyStatus, z = AnyStatus, eqSX = false)),
    PLY_W -> (_.copy(src = SourceOfNZ.Y, y = AnyStatus, n = AnyStatus, z = AnyStatus, eqSX = false)),
    PLZ -> (_.copy(src = SourceOfNZ.Z, iz = AnyStatus, n = AnyStatus, z = AnyStatus, eqSX = false)),
    XBA -> (currentStatus => currentStatus.copy(
      a = currentStatus.ah,
      a0 = currentStatus.ah.bit0,
      a7 = currentStatus.ah.bit7,
      n = currentStatus.ah.n(),
      z = currentStatus.ah.z(),
      src = SourceOfNZ.A,
      ah = currentStatus.a)),
    INX -> (currentStatus => {
      val newX = currentStatus.x.map(v => (v + 1) & 0xff)
      currentStatus.copy(
        n = newX.n(),
        z = newX.z(),
        x = newX,
        eqSX = false,
        eqSpX = false,
        src = SourceOfNZ.X)
    }),
    DEX -> (currentStatus => {
      val newX = currentStatus.x.map(v => (v - 1) & 0xff)
      currentStatus.copy(
        n = newX.n(),
        z = newX.z(),
        x = newX,
        eqSX = false,
        eqSpX = false,
        src = SourceOfNZ.X)
    }),
    INY -> (currentStatus => {
      val newY = currentStatus.y.map(v => (v + 1) & 0xff)
      currentStatus.copy(
        n = newY.n(),
        z = newY.z(),
        y = newY,
        src = SourceOfNZ.Y)
    }),
    DEY -> (currentStatus => {
      val newY = currentStatus.y.map(v => (v - 1) & 0xff)
      currentStatus.copy(
        n = newY.n(),
        z = newY.z(),
        y = newY,
        src = SourceOfNZ.Y)
    }),
    INZ -> (currentStatus => {
      val newIZ = currentStatus.iz.map(v => (v + 1) & 0xff)
      currentStatus.copy(
        n = newIZ.n(),
        z = newIZ.z(),
        iz = newIZ,
        src = SourceOfNZ.Z)
    }),
    DEZ -> (currentStatus => {
      val newIZ = currentStatus.iz.map(v => (v - 1) & 0xff)
      currentStatus.copy(
        n = newIZ.n(),
        z = newIZ.z(),
        iz = newIZ,
        src = SourceOfNZ.Z)
    }),
    INC -> (currentStatus => {
      val newA = currentStatus.a.map(v => (v + 1) & 0xff)
      currentStatus.copy(
        n = newA.n(),
        z = newA.z(),
        a = newA,
        a0 = currentStatus.a0.negate,
        a7 = newA.bit7,
        src = SourceOfNZ.A)
    }),
    DEC -> (currentStatus => {
      val newA = currentStatus.a.map(v => (v - 1) & 0xff)
      currentStatus.copy(
        n = newA.n(),
        z = newA.z(),
        a = newA,
        a0 = currentStatus.a0.negate,
        a7 = newA.bit7,
        src = SourceOfNZ.A)
    }),
    NEG -> (currentStatus => {
      val newA = currentStatus.a.map(v => (256 - v) & 0xff)
      currentStatus.copy(
        n = newA.n(),
        z = newA.z(),
        a = newA,
        a0 = currentStatus.a0,
        a7 = newA.bit7,
        src = SourceOfNZ.A)
    }),
    INX_W -> (currentStatus => {
      val newX = currentStatus.x.map(v => (v + 1) & 0xff)
      currentStatus.copy(
        n = AnyStatus,
        z = newX.z().withHiddenHi,
        x = newX,
        eqSX = false,
        eqSpX = false,
        src = AnyStatus)
    }),
    DEX_W -> (currentStatus => {
      val newX = currentStatus.x.map(v => (v - 1) & 0xff)
      currentStatus.copy(
        n = AnyStatus,
        z = newX.z().withHiddenHi,
        x = newX,
        eqSX = false,
        eqSpX = false,
        src = AnyStatus)
    }),
    INY_W -> (currentStatus => {
      val newY = currentStatus.y.map(v => (v + 1) & 0xff)
      currentStatus.copy(
        n = AnyStatus,
        z = newY.z().withHiddenHi,
        y = newY,
        src = AnyStatus)
    }),
    DEY_W -> (currentStatus => {
      val newY = currentStatus.y.map(v => (v - 1) & 0xff)
      currentStatus.copy(
        n = AnyStatus,
        z = newY.z().withHiddenHi,
        y = newY,
        src = AnyStatus)
    }),
    INC_W -> (currentStatus => {
      // TODO: smarter in case of only half of accu being known
      val newA = currentStatus.aw.map(v => (v + 1) & 0xffff)
      currentStatus.copy(
        n = newA.nw(),
        z = newA.zw(),
        a = newA.lo,
        a0 = newA.bit0,
        a7 = newA.bit7,
        ah = newA.hi,
        src = AnyStatus)
    }),
    DEC_W -> (currentStatus => {
      // TODO: smarter in case of only half of accu being known
      val newA = currentStatus.aw.map(v => (v - 1) & 0xffff)
      currentStatus.copy(
        n = newA.nw(),
        z = newA.zw(),
        a = newA.lo,
        a0 = newA.bit0,
        a7 = newA.bit7,
        ah = newA.hi,
        src = AnyStatus)
    }),
    TAX -> (currentStatus => {
      currentStatus.copy(
        x = currentStatus.a,
        eqSX = false,
        eqSpX = false,
        n = currentStatus.a.n(),
        z = currentStatus.a.z(),
        src = SourceOfNZ.AX)
    }),
    TXA -> (currentStatus => {
      currentStatus.copy(
        a = currentStatus.x,
        a0 = currentStatus.x.bit0,
        a7 = currentStatus.x.bit7,
        n = currentStatus.x.n(),
        z = currentStatus.x.z(),
        src = SourceOfNZ.AX)
    }),
    TAY -> (currentStatus => {
      currentStatus.copy(
        y = currentStatus.a,
        n = currentStatus.a.n(),
        z = currentStatus.a.z(),
        src = SourceOfNZ.AY)
    }),
    TYA -> (currentStatus => {
      currentStatus.copy(
        a = currentStatus.y,
        a0 = currentStatus.y.bit0,
        a7 = currentStatus.y.bit7,
        n = currentStatus.y.n(),
        z = currentStatus.y.z(),
        src = SourceOfNZ.AY)
    }),
    TXY -> (currentStatus => {
      currentStatus.copy(
        y = currentStatus.x,
        n = currentStatus.x.n(),
        z = currentStatus.x.z(),
        src = SourceOfNZ.XY)
    }),
    TYX -> (currentStatus => {
      currentStatus.copy(
        x = currentStatus.y,
        eqSX = false,
        eqSpX = false,
        n = currentStatus.y.n(),
        z = currentStatus.y.z(),
        src = SourceOfNZ.XY)
    }),
    TAZ -> (currentStatus => {
      currentStatus.copy(
        iz = currentStatus.a,
        n = currentStatus.a.n(),
        z = currentStatus.a.z(),
        src = SourceOfNZ.AZ)
    }),
    TZA -> (currentStatus => {
      currentStatus.copy(
        a = currentStatus.iz,
        a0 = currentStatus.iz.bit0,
        a7 = currentStatus.iz.bit7,
        n = currentStatus.iz.n(),
        z = currentStatus.iz.z(),
        src = SourceOfNZ.AZ)
    }),
    HuSAX -> (currentStatus => {
      currentStatus.copy(
        a = currentStatus.x,
        a0 = currentStatus.x.bit0,
        a7 = currentStatus.x.bit7,
        src = currentStatus.src.map(_.swapAX),
        eqSX = false,
        eqSpX = false,
        x = currentStatus.a)
    }),
    SAY -> (currentStatus => {
      currentStatus.copy(
        a = currentStatus.y,
        a0 = currentStatus.y.bit0,
        a7 = currentStatus.y.bit7,
        src = currentStatus.src.map(_.swapAY),
        y = currentStatus.a)
    }),
    SXY -> (currentStatus => {
      currentStatus.copy(
        y = currentStatus.x,
        eqSX = false,
        eqSpX = false,
        src = currentStatus.src.map(_.swapXY),
        x = currentStatus.y)
    }),
    ASL -> (currentStatus => {
      val newA = currentStatus.a.map(v => (v << 1) & 0xff)
      currentStatus.copy(
        a = newA,
        a7 = newA.bit7,
        a0 = Status.SingleFalse,
        n = newA.n(),
        z = newA.z(),
        c = currentStatus.a7,
        src = SourceOfNZ.A)
    }),
    LSR -> (currentStatus => {
      val newA = currentStatus.a.map(a => a.>>(1).&(0x7f))
      currentStatus.copy(
        a = newA,
        a0 = newA.bit0,
        a7 = Status.SingleFalse,
        n = newA.n(),
        z = newA.z(),
        c = currentStatus.a0,
        src = SourceOfNZ.A)
    }),
    ROL -> (currentStatus => {
      val newA = (currentStatus.a <*> currentStatus.c) { (aa, cc) => aa.<<(1).&(0xff).|(if (cc) 1 else 0) }
      currentStatus.copy(
        a = newA,
        a7 = newA.bit7,
        a0 = currentStatus.c,
        n = newA.n(),
        z = newA.z(),
        c = currentStatus.a7,
        src = SourceOfNZ.A)
    }),
    ROR -> (currentStatus => {
      val newA = (currentStatus.a <*> currentStatus.c) { (aa, cc) => aa.>>(1).&(0x7f).|(if (cc) 0x80 else 0) }
      currentStatus.copy(
        a = newA,
        a0 = newA.bit0,
        a7 = currentStatus.c,
        n = newA.n(),
        z = newA.z(),
        c = currentStatus.a0,
        src = SourceOfNZ.A)
    }),
    ASR -> (currentStatus => {
      val newA = currentStatus.a.map(a => a.toByte.>>(1).&(0xff))
      currentStatus.copy(
        a = newA,
        a0 = newA.bit0,
        a7 = currentStatus.a7,
        n = newA.n(),
        z = newA.n(),
        c = currentStatus.a0,
        src = SourceOfNZ.A)
    }),
    ASL_W -> (currentStatus => {
      val newA = currentStatus.a.map(v => (v << 1) & 0xff)
      currentStatus.copy(
        a = newA,
        ah = AnyStatus,
        a7 = newA.bit7,
        a0 = Status.SingleFalse,
        n = AnyStatus,
        z = currentStatus.a.z(_ << 1).withHiddenHi,
        c = AnyStatus,
        src = SourceOfNZ.AW)
    }),
    LSR_W -> (currentStatus => {
      currentStatus.copy(
        a = AnyStatus,
        ah = AnyStatus,
        a7 = AnyStatus,
        a0 = currentStatus.a.map(aa => (aa & 0x2) != 0),
        n = AnyStatus,
        z = currentStatus.a.z(a => a.>>(1).&(0x7f)).withHiddenHi,
        c = currentStatus.a.map(a => a.&(1).!=(0)),
        src = SourceOfNZ.AW)
    }),
    ROL_W -> (currentStatus => {
      currentStatus.copy(
        a = AnyStatus,
        ah = AnyStatus,
        a7 = AnyStatus,
        a0 = currentStatus.c,
        n = AnyStatus,
        z = AnyStatus,
        c = AnyStatus,
        src = SourceOfNZ.AW)
    }),
    ROR_W -> (currentStatus => {
      currentStatus.copy(
        a = AnyStatus,
        a7 = AnyStatus,
        a0 = AnyStatus,
        n = AnyStatus,
        z = AnyStatus,
        c = AnyStatus,
        src = SourceOfNZ.AW)
    }),
    TSX -> (currentStatus => {
      currentStatus.copy(
        x = AnyStatus,
        eqSX = true,
        eqSpX = false,
        src = SourceOfNZ.X)
    }),
  )

  def hasDefinition(opcode: Opcode.Value): Boolean = map.contains(opcode)

  def get(opcode: Opcode.Value): CpuStatus => CpuStatus = map(opcode)
}
