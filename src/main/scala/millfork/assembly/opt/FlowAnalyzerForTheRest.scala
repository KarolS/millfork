package millfork.assembly.opt

import millfork.assembly.Opcode
import millfork.assembly.Opcode._

/**
  * @author Karol Stasiak
  */
object FlowAnalyzerForTheRest {
  private val map: Map[Opcode.Value, CpuStatus => CpuStatus] = Map(
    STA -> identity,
    STX -> identity,
    STY -> identity,
    STZ -> identity,
    SAX -> identity,
    NOP -> identity,
    DISCARD_AF -> identity,
    DISCARD_XF -> identity,
    DISCARD_YF -> identity,
    REP -> (currentStatus => {
      currentStatus.copy(c = AnyStatus, d = AnyStatus, n = AnyStatus, z= AnyStatus, v = AnyStatus, m = AnyStatus, w = AnyStatus)
    }),
    SEP -> (currentStatus => {
      currentStatus.copy(c = AnyStatus, d = AnyStatus, n = AnyStatus, z= AnyStatus, v = AnyStatus, m = AnyStatus, w = AnyStatus)
    }),
    BCC -> (currentStatus => {
      currentStatus.copy(c = Status.SingleTrue)
    }),
    BCS -> (currentStatus => {
      currentStatus.copy(c = Status.SingleFalse)
    }),
    BVS -> (currentStatus => {
      currentStatus.copy(v = Status.SingleFalse)
    }),
    BVC -> (currentStatus => {
      currentStatus.copy(v = Status.SingleTrue)
    }),
    BMI -> (c => {
      var currentStatus = c
      currentStatus = currentStatus.copy(n = Status.SingleFalse)
      if (currentStatus.src.isFromA) {
        currentStatus = currentStatus.copy(a = currentStatus.a.flatMap(aa => if (aa.&(0x80)!=0) SingleStatus(aa) else AnyStatus), a7 = Status.SingleTrue)
      }
      currentStatus
    }),
    BPL -> (c => {
      var currentStatus = c
      currentStatus = currentStatus.copy(n = Status.SingleTrue)
      if (currentStatus.src.isFromA) {
        currentStatus = currentStatus.copy(a = currentStatus.a.flatMap(aa => if (aa.&(0x80)==0) SingleStatus(aa) else AnyStatus), a7 = Status.SingleFalse)
      }
      currentStatus
    }),
    BEQ -> (currentStatus => {
      currentStatus.copy(z = Status.SingleFalse)
    }),
    BNE -> (c => {
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
    LDX -> (currentStatus => {
      currentStatus.copy(
        x = AnyStatus,
        n = AnyStatus,
        z = AnyStatus,
        src = SourceOfNZ.X)
    }),
    LDY -> (currentStatus => {
      currentStatus.copy(
        y = AnyStatus,
        n = AnyStatus,
        z = AnyStatus,
        src = SourceOfNZ.Y)
    }),
    LDA -> (currentStatus => {
      currentStatus.copy(
        a = AnyStatus,
        a7 = AnyStatus,
        a0 = AnyStatus,
        n = AnyStatus,
        z = AnyStatus,
        src = SourceOfNZ.A)
    }),
    LDZ -> (currentStatus => {
      currentStatus.copy(
        iz = AnyStatus,
        n = AnyStatus,
        z = AnyStatus,
        src = SourceOfNZ.Z)
    }),
    LAX -> (currentStatus => {
      currentStatus.copy(
        x = AnyStatus,
        a = AnyStatus,
        a7 = AnyStatus,
        a0 = AnyStatus,
        n = AnyStatus,
        z = AnyStatus,
        src = SourceOfNZ.AX)
    }),
    LDA_W -> (currentStatus => {
      currentStatus.copy(
        ah = AnyStatus,
        a = AnyStatus,
        a7 = AnyStatus,
        a0 = AnyStatus,
        n = AnyStatus,
        z = AnyStatus,
        src = SourceOfNZ.AW)
    }),
    LDX_W -> (currentStatus => {
      currentStatus.copy(
        x = AnyStatus,
        n = AnyStatus,
        z = AnyStatus,
        src = AnyStatus)
    }),
    LDY_W -> (currentStatus => {
      currentStatus.copy(
        y = AnyStatus,
        n = AnyStatus,
        z = AnyStatus,
        src = AnyStatus)
    }),
    ADC -> (currentStatus => {
      currentStatus.copy(
        a = AnyStatus,
        a7 = AnyStatus,
        a0 = AnyStatus,
        v = AnyStatus,
        c = Status.flatMap3(currentStatus.a, currentStatus.c, currentStatus.d) {
          case (0, false, false) => SingleStatus[Boolean](false)
          case _ => AnyStatus
        },
        z = AnyStatus,
        n = AnyStatus,
        src = currentStatus.d.flatMap((dec: Boolean) => if (dec) AnyStatus else SourceOfNZ.A))
    }),
    SBC -> (currentStatus => {
      currentStatus.copy(
        a = AnyStatus,
        a7 = AnyStatus,
        a0 = AnyStatus,
        v = AnyStatus,
        c = Status.flatMap3(currentStatus.a, currentStatus.c, currentStatus.d) {
          case (0xff, true, false) => SingleStatus[Boolean](true)
          case _ => AnyStatus
        },
        z = AnyStatus,
        n = AnyStatus,
        src = currentStatus.d.flatMap((dec: Boolean) => if (dec) AnyStatus else SourceOfNZ.A))
    }),
    AND -> (currentStatus => {
      val newA: Status[Int] = currentStatus.a.flatMap(v => if ((v & 0xff) == 0) Status.SingleZero else AnyStatus)
      currentStatus.copy(
        a = newA,
        a7 = currentStatus.a7.flatMap(v => if (!v) Status.SingleFalse else AnyStatus),
        a0 = currentStatus.a0.flatMap(v => if (!v) Status.SingleFalse else AnyStatus),
        n = newA.n(),
        z = newA.z(),
        src = SourceOfNZ.A)
    }),
    ORA -> (currentStatus => {
      val newA: Status[Int] = currentStatus.a.flatMap(v => if ((v & 0xff) == 0xff) Status.SingleFF else AnyStatus)
      currentStatus.copy(
        a = newA,
        a7 = currentStatus.a7.flatMap(v => if (v) Status.SingleTrue else AnyStatus),
        a0 = currentStatus.a0.flatMap(v => if (v) Status.SingleTrue else AnyStatus),
        n = newA.n(),
        z = newA.z(),
        src = SourceOfNZ.A)
    }),
    SLO -> (currentStatus => {
      val newA: Status[Int] = currentStatus.a.flatMap(v => if ((v & 0xff) == 0xff) Status.SingleFF else AnyStatus)
      currentStatus.copy(
        c = AnyStatus,
        a = newA,
        a7 = currentStatus.a7.flatMap(v => if (v) Status.SingleTrue else AnyStatus),
        a0 = currentStatus.a0.flatMap(v => if (v) Status.SingleTrue else AnyStatus),
        n = newA.n(),
        z = newA.z(),
        src = SourceOfNZ.A)
    }),
    EOR -> (currentStatus => {
      currentStatus.copy(
        n = AnyStatus,
        z = AnyStatus,
        a = AnyStatus,
        a7 = AnyStatus,
        a0 = AnyStatus,
        src = SourceOfNZ.A)
    }),
    SRE -> (currentStatus => {
      currentStatus.copy(
        c = AnyStatus,
        n = AnyStatus,
        z = AnyStatus,
        a = AnyStatus,
        a7 = AnyStatus,
        a0 = AnyStatus,
        src = SourceOfNZ.A)
    }),
    RLA -> (currentStatus => {
      currentStatus.copy(
        c = AnyStatus,
        n = AnyStatus,
        z = AnyStatus,
        a = AnyStatus,
        a7 = AnyStatus,
        a0 = AnyStatus,
        src = SourceOfNZ.A)
    }),
    RRA -> (currentStatus => {
      currentStatus.copy(
        c = AnyStatus,
        n = AnyStatus,
        v = AnyStatus,
        a = AnyStatus,
        a7 = AnyStatus,
        a0 = AnyStatus,
        z = AnyStatus,
        src = currentStatus.d.flatMap(dec => if (dec) AnyStatus else SourceOfNZ.A))
    }),
    ISC -> (currentStatus => {
      currentStatus.copy(
        c = AnyStatus,
        n = AnyStatus,
        v = AnyStatus,
        a = AnyStatus,
        a7 = AnyStatus,
        a0 = AnyStatus,
        z = AnyStatus,
        src = currentStatus.d.flatMap(dec => if (dec) AnyStatus else SourceOfNZ.A))
    }),
    ROL -> (_.copy(c = AnyStatus, n = AnyStatus, z = AnyStatus, src = AnyStatus)),
    ROR -> (_.copy(c = AnyStatus, n = AnyStatus, z = AnyStatus, src = AnyStatus)),
    ASL -> (_.copy(c = AnyStatus, n = AnyStatus, z = AnyStatus, src = AnyStatus)),
    LSR -> (_.copy(c = AnyStatus, n = AnyStatus, z = AnyStatus, src = AnyStatus)),
    INC -> (_.copy(n = AnyStatus, z = AnyStatus, src = AnyStatus)),
    DEC -> (_.copy(n = AnyStatus, z = AnyStatus, src = AnyStatus)),
    CMP -> (_.copy(c = AnyStatus, n = AnyStatus, z = AnyStatus, src = AnyStatus)),
    CPX -> (_.copy(c = AnyStatus, n = AnyStatus, z = AnyStatus, src = AnyStatus)),
    CPY -> (_.copy(c = AnyStatus, n = AnyStatus, z = AnyStatus, src = AnyStatus)),
    CPZ -> (_.copy(c = AnyStatus, n = AnyStatus, z = AnyStatus, src = AnyStatus)),
    BIT -> (_.copy(c = AnyStatus, v = AnyStatus, n = AnyStatus, z = AnyStatus, src = AnyStatus)),
    TRB -> (_.copy(z = AnyStatus, src = AnyStatus)),
    TSB -> (_.copy(z = AnyStatus, src = AnyStatus)),
    JMP -> (_ => CpuStatus()),
    BRA -> (_ => CpuStatus()),
    BRL -> (_ => CpuStatus()),
  )

  def hasDefinition(opcode: Opcode.Value): Boolean = map.contains(opcode)

  def get(opcode: Opcode.Value): CpuStatus => CpuStatus = map(opcode)
}
