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
    TXS -> identity,
    PHP -> identity,
    PHA -> identity,
    PHA_W -> identity,
    PHX -> identity,
    PHX_W -> identity,
    PHY -> identity,
    PHY_W -> identity,
    PHZ -> identity,
    PHD -> identity,
    PHK -> identity,
    PHB -> identity,
    SED -> (_.copy(d = Status.SingleTrue)),
    CLD -> (_.copy(d = Status.SingleFalse)),
    CLC -> (_.copy(c = Status.SingleFalse)),
    SEC -> (_.copy(c = Status.SingleTrue)),
    CLV -> (_.copy(v = Status.SingleFalse)),
    XCE -> (_.copy(c = AnyStatus, m = AnyStatus, x = AnyStatus)),
    PLA -> (_.copy(src = SourceOfNZ.A, a = AnyStatus, n = AnyStatus, z = AnyStatus)),
    PLX -> (_.copy(src = SourceOfNZ.X, x = AnyStatus, n = AnyStatus, z = AnyStatus)),
    PLY -> (_.copy(src = SourceOfNZ.Y, y = AnyStatus, n = AnyStatus, z = AnyStatus)),
    PLZ -> (_.copy(src = SourceOfNZ.Z, iz = AnyStatus, n = AnyStatus, z = AnyStatus)),
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
        src = SourceOfNZ.X)
    }),
    DEX -> (currentStatus => {
      val newX = currentStatus.x.map(v => (v - 1) & 0xff)
      currentStatus.copy(
        n = newX.n(),
        z = newX.z(),
        x = newX,
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
        src = AnyStatus)
    }),
    DEX_W -> (currentStatus => {
      val newX = currentStatus.x.map(v => (v - 1) & 0xff)
      currentStatus.copy(
        n = AnyStatus,
        z = newX.z().withHiddenHi,
        x = newX,
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
        x = currentStatus.a)
    }),
    SAY -> (currentStatus => {
      currentStatus.copy(
        a = currentStatus.y,
        a0 = currentStatus.y.bit0,
        a7 = currentStatus.y.bit7,
        y = currentStatus.a)
    }),
    SXY -> (currentStatus => {
      currentStatus.copy(
        y = currentStatus.x,
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
        src = SourceOfNZ.X)
    }),
  )

  def hasDefinition(opcode: Opcode.Value): Boolean = map.contains(opcode)

  def get(opcode: Opcode.Value): CpuStatus => CpuStatus = map(opcode)
}
