package millfork.assembly.mos.opt

import millfork.assembly.mos.Opcode
import millfork.assembly.mos.Opcode._

/**
  * @author Karol Stasiak
  */
object ReverseFlowAnalyzerPerImpiedOpcode {

  private val finalImportance = CpuImportance(
    a = Important, ah = Important,
    x = Important, y = Important, iz = Important,
    c = Important, v = Important, d = Important, z = Important, n = Important,
    m = Important, w = Important)

  private def allAddingOutputsUnimportant(currentImportance: CpuImportance): Boolean =
    currentImportance.a == Unimportant &&
      currentImportance.c == Unimportant &&
      currentImportance.z == Unimportant &&
      currentImportance.n == Unimportant &&
      currentImportance.v == Unimportant

  private def allLoadingAccuOutputsUnimportant(currentImportance: CpuImportance): Boolean =
    currentImportance.a == Unimportant &&
      currentImportance.z == Unimportant &&
      currentImportance.n == Unimportant

  private def allCompareOutputsAreUnimportant(currentImportance: CpuImportance): Boolean =
      currentImportance.z == Unimportant &&
      currentImportance.n == Unimportant&&
      currentImportance.c == Unimportant

  private val map: Map[Opcode.Value, CpuImportance => CpuImportance] = Map(
    NOP -> identity,
    SEI -> identity,
    CLI -> identity,
    WAI -> identity,
    STP -> identity,
    BRK -> (_ => finalImportance),
    COP -> (_ => finalImportance),
    RTS -> (_ => finalImportance),
    RTL -> (_ => finalImportance),

    RTI -> (_ => CpuImportance(
      a = Unimportant, ah = Unimportant,
      x = Unimportant, y = Unimportant, iz = Unimportant,
      z = Unimportant, n = Unimportant, c = Unimportant, v = Unimportant, d = Unimportant,
      m = Unimportant, w = Unimportant)),

    TAX -> (currentImportance => {
      currentImportance.copy(
        a = currentImportance.x ~ currentImportance.a ~ currentImportance.n ~ currentImportance.z,
        x = Unimportant,
        n = Unimportant, z = Unimportant, m = Important, w = Important)
    }),
    TAY -> (currentImportance => {
      currentImportance.copy(
        a = currentImportance.y ~ currentImportance.a ~ currentImportance.n ~ currentImportance.z,
        y = Unimportant,
        n = Unimportant, z = Unimportant, m = Important, w = Important)
    }),
    TXA -> (currentImportance => {
      currentImportance.copy(
        x = currentImportance.a ~ currentImportance.x ~ currentImportance.n ~ currentImportance.z,
        a = Unimportant,
        n = Unimportant, z = Unimportant, m = Important, w = Important)
    }),
    TYA -> (currentImportance => {
      currentImportance.copy(
        y = currentImportance.a ~ currentImportance.y ~ currentImportance.n ~ currentImportance.z,
        a = Unimportant,
        n = Unimportant, z = Unimportant, m = Important, w = Important)
    }),
    TAZ -> (currentImportance => {
      currentImportance.copy(
        a = currentImportance.iz ~ currentImportance.a ~ currentImportance.n ~ currentImportance.z,
        iz = Unimportant,
        n = Unimportant, z = Unimportant, m = Important, w = Important)
    }),
    TZA -> (currentImportance => {
      currentImportance.copy(
        iz = currentImportance.a ~ currentImportance.iz ~ currentImportance.n ~ currentImportance.z,
        a = Unimportant,
        n = Unimportant, z = Unimportant, m = Important, w = Important)
    }),
    TXY -> (currentImportance => {
      currentImportance.copy(
        x = currentImportance.y ~ currentImportance.x ~ currentImportance.n ~ currentImportance.z,
        y = Unimportant,
        n = Unimportant, z = Unimportant, m = Important, w = Important)
    }),
    TYX -> (currentImportance => {
      currentImportance.copy(
        y = currentImportance.x ~ currentImportance.y ~ currentImportance.n ~ currentImportance.z,
        x = Unimportant,
        n = Unimportant, z = Unimportant, m = Important, w = Important)
    }),
    TSX -> (_.copy(x = Unimportant, n = Unimportant, z = Unimportant, w = Important)),
    TXS -> (_.copy(x = Important, w = Important)),
    XBA -> (currentImportance => {
      currentImportance.copy(
        ah = currentImportance.a ~ currentImportance.iz ~ currentImportance.n ~ currentImportance.z,
        a = currentImportance.ah,
        n = Unimportant, z = Unimportant)
    }),
    TCD -> (_.copy(a = Important, ah = Important)),
    TDC -> (_.copy(a = Unimportant, ah = Unimportant, n = Unimportant, z = Unimportant)),
    TCS -> (_.copy(a = Important, ah = Important)),
    TSC -> (_.copy(a = Unimportant, ah = Unimportant, n = Unimportant, z = Unimportant)),

    TRB -> (_.copy(a = Important, z = Unimportant, m = Important)),
    TSB -> (_.copy(a = Important, z = Unimportant, m = Important)),

    HuSAX -> (currentImportance => {
      currentImportance.copy(a = currentImportance.x, x = currentImportance.a, m = Important, w = Important)
    }),
    SAY -> (currentImportance => {
      currentImportance.copy(y = currentImportance.a, a = currentImportance.y, m = Important, w = Important)
    }),
    SXY -> (currentImportance => {
      currentImportance.copy(y = currentImportance.x, x = currentImportance.y, m = Important, w = Important)
    }),

    ASL -> (_.copy(a = Important, c = Unimportant, n = Unimportant, z = Unimportant, m = Important)),
    LSR -> (_.copy(a = Important, c = Unimportant, n = Unimportant, z = Unimportant, m = Important)),
    ROL -> (_.copy(a = Important, c = Important, n = Unimportant, z = Unimportant, m = Important)),
    ROR -> (_.copy(a = Important, c = Important, n = Unimportant, z = Unimportant, m = Important)),

    ASL_W -> (_.copy(a = Important, ah = Important, c = Unimportant, n = Unimportant, z = Unimportant, m = Important)),
    LSR_W -> (_.copy(a = Important, ah = Important, c = Unimportant, n = Unimportant, z = Unimportant, m = Important)),
    ROL_W -> (_.copy(a = Important, ah = Important, c = Important, n = Unimportant, z = Unimportant, m = Important)),
    ROR_W -> (_.copy(a = Important, ah = Important, c = Important, n = Unimportant, z = Unimportant, m = Important)),

    DEC -> (_.copy(a = Important, n = Unimportant, z = Unimportant, m = Important)),
    DEX -> (_.copy(x = Important, n = Unimportant, z = Unimportant, w = Important)),
    DEY -> (_.copy(y = Important, n = Unimportant, z = Unimportant, w = Important)),
    DEZ -> (_.copy(iz = Important, n = Unimportant, z = Unimportant)),

    INC -> (_.copy(a = Important, n = Unimportant, z = Unimportant, m = Important)),
    INX -> (_.copy(x = Important, n = Unimportant, z = Unimportant, w = Important)),
    INY -> (_.copy(y = Important, n = Unimportant, z = Unimportant, w = Important)),
    INZ -> (_.copy(iz = Important, n = Unimportant, z = Unimportant)),

    DEC_W -> (_.copy(a = Important, ah = Important, n = Unimportant, z = Unimportant, m = Important)),
    DEX_W -> (_.copy(x = Important, n = Unimportant, z = Unimportant, w = Important)),
    DEY_W -> (_.copy(y = Important, n = Unimportant, z = Unimportant, w = Important)),
    INC_W -> (_.copy(x = Important, ah = Important, n = Unimportant, z = Unimportant, m = Important)),
    INX_W -> (_.copy(x = Important, n = Unimportant, z = Unimportant, w = Important)),
    INY_W -> (_.copy(y = Important, n = Unimportant, z = Unimportant, w = Important)),

    PHP -> (_.copy(n = Important, c = Important, d = Important, z = Important, v = Important, m = Important, w = Important)),
    PLP -> (_.copy(n = Unimportant, c = Unimportant, d = Unimportant, z = Unimportant, v = Unimportant, m = Unimportant, w = Unimportant)),

    PHA -> (_.copy(a = Important, m = Important)),
    PLA -> (_.copy(a = Unimportant, m = Important, n = Unimportant, z = Unimportant)),
    PHX -> (_.copy(x = Important, w = Important)),
    PLX -> (_.copy(x = Unimportant, w = Important, n = Unimportant, z = Unimportant)),
    PHY -> (_.copy(y = Important, w = Important)),
    PLY -> (_.copy(y = Unimportant, w = Important, n = Unimportant, z = Unimportant)),
    PHZ -> (_.copy(iz = Important, w = Important)),
    PLZ -> (_.copy(iz = Unimportant, w = Important, n = Unimportant, z = Unimportant)),

    PHA_W -> (_.copy(a = Important, ah = Important, m = Important)),
    PLA_W -> (_.copy(a = Unimportant, ah = Unimportant, m = Important, n = Unimportant, z = Unimportant)),
    PHX_W -> (_.copy(x = Important, w = Important)),
    PLX_W -> (_.copy(x = Unimportant, w = Important, n = Unimportant, z = Unimportant)),
    PHY_W -> (_.copy(y = Important, w = Important)),
    PLY_W -> (_.copy(y = Unimportant, w = Important, n = Unimportant, z = Unimportant)),

    CLC -> (_.copy(c = Unimportant)),
    SEC -> (_.copy(c = Unimportant)),
    CLD -> (_.copy(d = Unimportant)),
    SED -> (_.copy(d = Unimportant)),
    CLV -> (_.copy(v = Unimportant)),

    CLA -> (_.copy(a = Unimportant)),
    CLX -> (_.copy(x = Unimportant)),
    CLY -> (_.copy(y = Unimportant)),

  )

  def hasDefinition(opcode: Opcode.Value): Boolean = map.contains(opcode)

  def get(opcode: Opcode.Value): CpuImportance => CpuImportance = map(opcode)
}
