package millfork.assembly.opt

import millfork.assembly.Opcode
import millfork.assembly.Opcode._

/**
  * @author Karol Stasiak
  */
object ReverseFlowAnalyzerPerOpcode {

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

  private val map: Map[Opcode.Value, CpuImportance => CpuImportance] = Map(
    NOP -> identity,
    SEI -> identity,
    CLI -> identity,

    JSR -> (_ => finalImportance),
    BSR -> (_ => finalImportance),
    BRK -> (_ => finalImportance),
    COP -> (_ => finalImportance),
    RTS -> (_ => finalImportance),
    RTL -> (_ => finalImportance),
    BYTE -> (_ => finalImportance),

    RTI -> (_ => CpuImportance(
      a = Unimportant, ah = Unimportant,
      x = Unimportant, y = Unimportant, iz = Unimportant,
      z = Unimportant, n = Unimportant, c = Unimportant, v = Unimportant, d = Unimportant,
      m = Unimportant, w = Unimportant)),

    BNE -> (_.copy(z = Important)),
    BEQ -> (_.copy(z = Important)),
    BMI -> (_.copy(n = Important)),
    BPL -> (_.copy(n = Important)),
    BVC -> (_.copy(v = Important)),
    BVS -> (_.copy(v = Important)),
    BCC -> (_.copy(c = Important)),
    BCS -> (_.copy(c = Important)),

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
    XBA -> (currentImportance => {
      currentImportance.copy(
        ah = currentImportance.a ~ currentImportance.iz ~ currentImportance.n ~ currentImportance.z,
        a = currentImportance.ah,
        n = Unimportant, z = Unimportant)
    }),

    HuSAX -> (currentImportance => {
      currentImportance.copy(a = currentImportance.x, x = currentImportance.a, m = Important, w = Important)
    }),
    SAY -> (currentImportance => {
      currentImportance.copy(y = currentImportance.a, a = currentImportance.y, m = Important, w = Important)
    }),
    SXY -> (currentImportance => {
      currentImportance.copy(y = currentImportance.x, x = currentImportance.y, m = Important, w = Important)
    }),

    DISCARD_XF -> (_.copy(x = Unimportant, n = Unimportant, z = Unimportant, c = Unimportant, v = Unimportant, r0 = Unimportant, r1 = Unimportant)),
    DISCARD_YF -> (_.copy(y = Unimportant, iz = Unimportant, n = Unimportant, z = Unimportant, c = Unimportant, v = Unimportant, r0 = Unimportant, r1 = Unimportant)),
    DISCARD_AF -> (_.copy(a = Unimportant, n = Unimportant, z = Unimportant, c = Unimportant, v = Unimportant, r0 = Unimportant, r1 = Unimportant)),

    LDA -> (_.copy(a = Unimportant, n = Unimportant, z = Unimportant, m = Important)),
    LDX -> (_.copy(x = Unimportant, n = Unimportant, z = Unimportant, w = Important)),
    LDY -> (_.copy(y = Unimportant, n = Unimportant, z = Unimportant, w = Important)),
    LDZ -> (_.copy(iz = Unimportant, n = Unimportant, z = Unimportant)),
    LAX -> (_.copy(a = Unimportant, x = Unimportant, n = Unimportant, z = Unimportant)),

    ORA -> (currentImportance => {
      val ignoreOutput = allLoadingAccuOutputsUnimportant(currentImportance)
      currentImportance.copy(
        a = if (ignoreOutput) Unimportant else Important,
        n = Unimportant, z = Unimportant,
        m = Important)
    }),
    AND -> (currentImportance => {
      val ignoreOutput = allLoadingAccuOutputsUnimportant(currentImportance)
      currentImportance.copy(
        a = if (ignoreOutput) Unimportant else Important,
        n = Unimportant, z = Unimportant,
        m = Important)
    }),
    EOR -> (currentImportance => {
      val ignoreOutput = allLoadingAccuOutputsUnimportant(currentImportance)
      currentImportance.copy(
        a = if (ignoreOutput) Unimportant else Important,
        n = Unimportant, z = Unimportant,
        m = Important)
    }),

    ADC -> (currentImportance => {
      val ignoreOutput = allAddingOutputsUnimportant(currentImportance)
      currentImportance.copy(
        a = if (ignoreOutput) Unimportant else Important,
        c = if (ignoreOutput) Unimportant else Important,
        d = if (ignoreOutput) currentImportance.d else Important,
        n = Unimportant, z = Unimportant, v = Unimportant,
        m = Important)
    }),
    SBC -> (currentImportance => {
      val ignoreOutput = allAddingOutputsUnimportant(currentImportance)
      currentImportance.copy(
        a = if (ignoreOutput) Unimportant else Important,
        c = if (ignoreOutput) Unimportant else Important,
        d = if (ignoreOutput) currentImportance.d else Important,
        n = Unimportant, z = Unimportant, v = Unimportant,
        m = Important)
    }),
    ISC -> (currentImportance => {
      val ignoreOutput = allAddingOutputsUnimportant(currentImportance)
      currentImportance.copy(
        a = if (ignoreOutput) Unimportant else Important,
        c = if (ignoreOutput) Unimportant else Important,
        d = if (ignoreOutput) currentImportance.d else Important,
        n = Unimportant, z = Unimportant, v = Unimportant)
    }),

    LDA_W -> (_.copy(a = Unimportant, ah = Unimportant, n = Unimportant, z = Unimportant, m = Important)),
    LDX_W -> (_.copy(x = Unimportant, n = Unimportant, z = Unimportant, w = Important)),
    LDY_W -> (_.copy(y = Unimportant, n = Unimportant, z = Unimportant, w = Important)),

    STA -> (importance => importance.copy(a = Important, m = Important)),
    STX -> (importance => importance.copy(x = Important, w = Important)),
    STY -> (importance => importance.copy(y = Important, w = Important)),
    STZ -> (importance => importance.copy(iz = Important, m = Important)),
    SAX -> (importance => importance.copy(a = Important, x = Important)),

    STA_W -> (importance => importance.copy(a = Important, ah = Important, m = Important)),
    STX_W -> (importance => importance.copy(x = Important, w = Important)),
    STY_W -> (importance => importance.copy(y = Important, w = Important)),
    STZ_W -> (importance => importance.copy(iz = Important, m = Important)),

    DEX -> (_.copy(x = Important, n = Unimportant, z = Unimportant, w = Important)),
    DEY -> (_.copy(y = Important, n = Unimportant, z = Unimportant, w = Important)),
    DEZ -> (_.copy(iz = Important, n = Unimportant, z = Unimportant)),

    INX -> (_.copy(x = Important, n = Unimportant, z = Unimportant, w = Important)),
    INY -> (_.copy(y = Important, n = Unimportant, z = Unimportant, w = Important)),
    INZ -> (_.copy(iz = Important, n = Unimportant, z = Unimportant)),

    DEX_W -> (_.copy(x = Important, n = Unimportant, z = Unimportant, w = Important)),
    DEY_W -> (_.copy(y = Important, n = Unimportant, z = Unimportant, w = Important)),
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

  )

  def hasDefinition(opcode: Opcode.Value): Boolean = map.contains(opcode)

  def get(opcode: Opcode.Value): CpuImportance => CpuImportance = map(opcode)
}
