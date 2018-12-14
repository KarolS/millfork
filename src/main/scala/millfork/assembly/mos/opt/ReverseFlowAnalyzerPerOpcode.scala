package millfork.assembly.mos.opt

import millfork.assembly.mos.Opcode
import millfork.assembly.mos.Opcode._

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

  private def allCompareOutputsAreUnimportant(currentImportance: CpuImportance): Boolean =
      currentImportance.z == Unimportant &&
      currentImportance.n == Unimportant&&
      currentImportance.c == Unimportant

  private val map: Map[Opcode.Value, CpuImportance => CpuImportance] = Map(
    NOP -> identity,
    LABEL -> identity,
    JSR -> (_ => finalImportance),
    BSR -> (_ => finalImportance),
    BRK -> (_ => finalImportance),
    COP -> (_ => finalImportance),
    BYTE -> (_ => finalImportance),

    BNE -> (_.copy(z = Important)),
    BEQ -> (_.copy(z = Important)),
    BMI -> (_.copy(n = Important)),
    BPL -> (_.copy(n = Important)),
    BVC -> (_.copy(v = Important)),
    BVS -> (_.copy(v = Important)),
    BCC -> (_.copy(c = Important)),
    BCS -> (_.copy(c = Important)),

    DISCARD_XF -> (_.copy(x = Unimportant, n = Unimportant, z = Unimportant, c = Unimportant, v = Unimportant, r0 = Unimportant, r1 = Unimportant)),
    DISCARD_YF -> (_.copy(y = Unimportant, iz = Unimportant, n = Unimportant, z = Unimportant, c = Unimportant, v = Unimportant, r0 = Unimportant, r1 = Unimportant)),
    DISCARD_AF -> (_.copy(a = Unimportant, n = Unimportant, z = Unimportant, c = Unimportant, v = Unimportant, r0 = Unimportant, r1 = Unimportant)),

    LDA -> (_.copy(a = Unimportant, n = Unimportant, z = Unimportant, m = Important)),
    LDX -> (_.copy(x = Unimportant, n = Unimportant, z = Unimportant, w = Important)),
    LDY -> (_.copy(y = Unimportant, n = Unimportant, z = Unimportant, w = Important)),
    LDZ -> (_.copy(iz = Unimportant, n = Unimportant, z = Unimportant)),
    LAX -> (_.copy(a = Unimportant, x = Unimportant, n = Unimportant, z = Unimportant)),
    LXA -> (_.copy(a = Important, x = Unimportant, n = Unimportant, z = Unimportant)),
    XAA -> (_.copy(a = Important, x = Important, n = Unimportant, z = Unimportant)),

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
    SLO -> (currentImportance => {
      val ignoreOutput = allLoadingAccuOutputsUnimportant(currentImportance)
      currentImportance.copy(
        a = if (ignoreOutput) Unimportant else Important,
        n = Unimportant, z = Unimportant, c = Unimportant,
        m = Important)
    }),
    SRE -> (currentImportance => {
      val ignoreOutput = allLoadingAccuOutputsUnimportant(currentImportance)
      currentImportance.copy(
        a = if (ignoreOutput) Unimportant else Important,
        n = Unimportant, z = Unimportant, c = Unimportant,
        m = Important)
    }),
    RLA -> (currentImportance => {
      val ignoreOutput = allLoadingAccuOutputsUnimportant(currentImportance)
      currentImportance.copy(
        a = if (ignoreOutput) Unimportant else Important,
        n = Unimportant, z = Unimportant, c = Important,
        m = Important)
    }),
    BIT -> (currentImportance => {
      currentImportance.copy(
        a = currentImportance.z,
        n = Unimportant,
        z = Unimportant,
        v = Unimportant,
        m = Important)
    }),
    CMP -> (currentImportance => {
      val ignoreOutput = allCompareOutputsAreUnimportant(currentImportance)
      currentImportance.copy(
        a = if (ignoreOutput) currentImportance.a else Important,
        n = Unimportant, z = Unimportant, c = Unimportant,
        m = Important)
    }),
    DCP -> (currentImportance => {
      val ignoreOutput = allCompareOutputsAreUnimportant(currentImportance)
      currentImportance.copy(
        a = if (ignoreOutput) currentImportance.a else Important,
        n = Unimportant, z = Unimportant, c = Unimportant)
    }),
    CPX -> (currentImportance => {
      val ignoreOutput = allCompareOutputsAreUnimportant(currentImportance)
      currentImportance.copy(
        x = if (ignoreOutput) currentImportance.x else Important,
        n = Unimportant, z = Unimportant, c = Unimportant,
        w = Important)
    }),
    CPY -> (currentImportance => {
      val ignoreOutput = allCompareOutputsAreUnimportant(currentImportance)
      currentImportance.copy(
        y = if (ignoreOutput) currentImportance.y else Important,
        n = Unimportant, z = Unimportant, c = Unimportant,
        w = Important)
    }),
    CPZ -> (currentImportance => {
      val ignoreOutput = allCompareOutputsAreUnimportant(currentImportance)
      currentImportance.copy(
        iz = if (ignoreOutput) currentImportance.iz else Important,
        n = Unimportant, z = Unimportant, c = Unimportant,
        w = Important)
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

    INC -> (importance => importance.copy(n = Unimportant, z = Unimportant, m = Important)),
    DEC -> (importance => importance.copy(n = Unimportant, z = Unimportant, m = Important)),
    ROL -> (importance => importance.copy(n = Unimportant, z = Unimportant, c = Important, m = Important)),
    ROR -> (importance => importance.copy(n = Unimportant, z = Unimportant, c = Important, m = Important)),
    ASL -> (importance => importance.copy(n = Unimportant, z = Unimportant, c = Unimportant, m = Important)),
    LSR -> (importance => importance.copy(n = Unimportant, z = Unimportant, c = Unimportant, m = Important)),


    SBX -> (importance => importance.copy(a = Important, x = Important, n = Unimportant, z = Unimportant, c = Unimportant, m = Important)),
    ANC -> (currentImportance => {
      val ignoreOutput = allAddingOutputsUnimportant(currentImportance)
      currentImportance.copy(
        a = if (ignoreOutput) Unimportant else Important,
        n = Unimportant, z = Unimportant, c = Unimportant,
        m = Important)
    }),
    ALR -> (currentImportance => {
      val ignoreOutput = allAddingOutputsUnimportant(currentImportance)
      currentImportance.copy(
        a = if (ignoreOutput) Unimportant else Important,
        n = Unimportant, z = Unimportant, c = Unimportant,
        m = Important)
    }),

    STA_W -> (importance => importance.copy(a = Important, ah = Important, m = Important)),
    STX_W -> (importance => importance.copy(x = Important, w = Important)),
    STY_W -> (importance => importance.copy(y = Important, w = Important)),
    STZ_W -> (importance => importance.copy(iz = Important, m = Important)),

  )

  def hasDefinition(opcode: Opcode.Value): Boolean = map.contains(opcode)

  def get(opcode: Opcode.Value): CpuImportance => CpuImportance = map(opcode)
}
