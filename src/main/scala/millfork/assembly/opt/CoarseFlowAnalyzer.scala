package millfork.assembly.opt

import millfork.{CompilationFlag, CompilationOptions}
import millfork.assembly._
import millfork.env.{Label, MemoryAddressConstant, NormalFunction, NumericConstant}

/**
  * @author Karol Stasiak
  */
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
          //            if (currentStatus.src.isFromA) {
          //              currentStatus = currentStatus.copy(a7 = currentStatus.a7 ~ SingleStatus(true))
          //            }
          case AssemblyLine(BPL, _, _, _) =>
            currentStatus = currentStatus.copy(n = currentStatus.n ~ SingleStatus(true))
          //            if (currentStatus.src.isFromA) {
          //              currentStatus = currentStatus.copy(a7 = currentStatus.a7 ~ SingleStatus(false))
          //            }
          case AssemblyLine(BEQ, _, _, _) =>
            currentStatus = currentStatus.copy(z = currentStatus.z ~ SingleStatus(false))
          case AssemblyLine(BNE, _, _, _) =>
            currentStatus = currentStatus.copy(z = currentStatus.z ~ SingleStatus(true))
          //            if (currentStatus.src.isFromA) {
          //              currentStatus = currentStatus.copy(
          //                a7 = currentStatus.a7 ~ SingleStatus(false),
          //                a0 = currentStatus.a0 ~ SingleStatus(false),
          //                a = currentStatus.a ~ SingleStatus(0))
          //            }
          //            if (currentStatus.src.isFromAW) {
          //              currentStatus = currentStatus.copy(
          //                a7 = currentStatus.a7 ~ SingleStatus(false),
          //                a0 = currentStatus.a0 ~ SingleStatus(false),
          //                a = currentStatus.a ~ SingleStatus(0),
          //                ah = currentStatus.a ~ SingleStatus(0))
          //            }
          //            if (currentStatus.src.isFromX) {
          //              currentStatus = currentStatus.copy(x = currentStatus.x ~ SingleStatus(0))
          //            }
          //            if (currentStatus.src.isFromY) {
          //              currentStatus = currentStatus.copy(y = currentStatus.y ~ SingleStatus(0))
          //            }
          //            if (currentStatus.src.isFromIZ) {
          //              currentStatus = currentStatus.copy(iz = currentStatus.iz ~ SingleStatus(0))
          //            }

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
            currentStatus = currentStatus.nz(n).copy(x = SingleStatus(n), src = SingleStatus(SourceOfNZ(x = true)))
          case AssemblyLine(LDY, Immediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt & 0xff
            currentStatus = currentStatus.nz(n).copy(y = SingleStatus(n), src = SingleStatus(SourceOfNZ(y = true)))
          case AssemblyLine(LDA, Immediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt & 0xff
            currentStatus = currentStatus.nz(n).copy(
              a = SingleStatus(n),
              a7 = SingleStatus((n & 0x80) != 0),
              a0 = SingleStatus((n & 1) != 0),
              src = SingleStatus(SourceOfNZ(a = true)))
          case AssemblyLine(LDZ, Immediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt & 0xff
            currentStatus = currentStatus.nz(n).copy(iz = SingleStatus(n), src = SingleStatus(SourceOfNZ(iz = true)))

          case AssemblyLine(LDX_W, WordImmediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt & 0xff
            currentStatus = currentStatus.nzw(nn).copy(x = SingleStatus(n), src = AnyStatus())
          case AssemblyLine(LDY_W, WordImmediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt & 0xff
            currentStatus = currentStatus.nzw(nn).copy(y = SingleStatus(n), src = AnyStatus())
          case AssemblyLine(LDA_W, WordImmediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt & 0xff
            val nh = (nn.toInt >> 8) & 0xff
            currentStatus = currentStatus.nzw(nn).copy(
              a = SingleStatus(n),
              a7 = SingleStatus((n & 0x80) != 0),
              a0 = SingleStatus((n & 1) != 0),
              ah = SingleStatus(nh),
              src = SingleStatus(SourceOfNZ(aw = true)))


          case AssemblyLine(LDX | PLX, _, _, _) =>
            currentStatus = currentStatus.copy(
              x = AnyStatus(),
              n = AnyStatus(),
              z = AnyStatus(),
              src = SingleStatus(SourceOfNZ(x = true)))
          case AssemblyLine(LDY | PLY, _, _, _) =>
            currentStatus = currentStatus.copy(
              y = AnyStatus(),
              n = AnyStatus(),
              z = AnyStatus(),
              src = SingleStatus(SourceOfNZ(y = true)))
          case AssemblyLine(LDA | PLA, _, _, _) =>
            currentStatus = currentStatus.copy(
              a = AnyStatus(),
              a7 = AnyStatus(),
              a0 = AnyStatus(),
              n = AnyStatus(),
              z = AnyStatus(),
              src = SingleStatus(SourceOfNZ(a = true)))
          case AssemblyLine(LDZ | PLZ, _, _, _) =>
            currentStatus = currentStatus.copy(
              iz = AnyStatus(),
              n = AnyStatus(),
              z = AnyStatus(),
              src = SingleStatus(SourceOfNZ(iz = true)))
          case AssemblyLine(LAX, _, _, _) =>
            currentStatus = currentStatus.copy(
              x = AnyStatus(),
              a = AnyStatus(),
              a7 = AnyStatus(),
              a0 = AnyStatus(),
              n = AnyStatus(),
              z = AnyStatus(),
              src = SingleStatus(SourceOfNZ(a = true, x = true)))
          case AssemblyLine(LDA_W, _, _, _) =>
            currentStatus = currentStatus.copy(
              ah = AnyStatus(),
              a = AnyStatus(),
              a7 = AnyStatus(),
              a0 = AnyStatus(),
              n = AnyStatus(),
              z = AnyStatus(),
              src = SingleStatus(SourceOfNZ(aw = true)))

          case AssemblyLine(XBA, _, _, _) =>
            currentStatus = currentStatus.copy(
              a = currentStatus.ah,
              a0 = currentStatus.ah.bit0,
              a7 = currentStatus.ah.bit7,
              n = currentStatus.ah.n(),
              z = currentStatus.ah.z(),
              src = SingleStatus(SourceOfNZ(a = true)),
              ah = currentStatus.a)

          case AssemblyLine(ADC, Immediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt & 0xff
            val newA = currentStatus.a.adc(n, currentStatus.c, currentStatus.d)
            currentStatus = currentStatus.copy(
              n = newA.n(),
              z = newA.z(),
              src = currentStatus.d.flatMap((dec: Boolean) => if (dec) AnyStatus() else SingleStatus(SourceOfNZ(a = true))),
              a = newA,
              a0 = newA.bit0,
              a7 = newA.bit7,
              c = Status.flatMap3(currentStatus.a, currentStatus.c, currentStatus.d) {
                case (aa, false, false) => SingleStatus((aa & 0xff) + n >= 0x100)
                case _ => AnyStatus()
              },
              v = AnyStatus())
          case AssemblyLine(SBC, Immediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt & 0xff
            val newA = currentStatus.a.sbc(n, currentStatus.c, currentStatus.d)
            currentStatus = currentStatus.copy(
              n = newA.n(),
              z = newA.z(),
              src = currentStatus.d.flatMap((dec: Boolean) => if (dec) AnyStatus() else SingleStatus(SourceOfNZ(a = true))),
              a = newA,
              a0 = newA.bit0,
              a7 = newA.bit7,
              c = AnyStatus(),
              v = AnyStatus())
          case AssemblyLine(EOR, Immediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt & 0xff
            val newA = currentStatus.a.map(_ ^ n)
            currentStatus = currentStatus.copy(
              n = newA.n(),
              z = newA.z(),
              a = newA,
              a7 = if ((nn & 0x80) != 0) currentStatus.a7.map(!_) else currentStatus.a7,
              a0 = if ((nn & 1) != 0) currentStatus.a0.map(!_) else currentStatus.a0,
              src = SingleStatus(SourceOfNZ(a = true)))
          case AssemblyLine(AND, Immediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt & 0xff
            val newA = currentStatus.a.map(_ & n)
            currentStatus = currentStatus.copy(
              n = newA.n(),
              z = newA.z(),
              a = newA,
              a7 = if ((nn & 0x80) != 0) currentStatus.a7 else SingleStatus(false),
              a0 = if ((nn & 1) != 0) currentStatus.a0 else SingleStatus(false),
              src = SingleStatus(SourceOfNZ(a = true)))
          case AssemblyLine(ANC, Immediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt & 0xff
            val newA = currentStatus.a.map(_ & n)
            currentStatus = currentStatus.copy(
              n = newA.n(),
              c = newA.n(),
              z = newA.z(),
              a = newA,
              a7 = if ((nn & 0x80) != 0) currentStatus.a7 else SingleStatus(false),
              a0 = if ((nn & 1) != 0) currentStatus.a0 else SingleStatus(false),
              src = SingleStatus(SourceOfNZ(a = true)))
          case AssemblyLine(ORA, Immediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt & 0xff
            val newA = currentStatus.a.map(_ | n)
            currentStatus = currentStatus.copy(
              n = newA.n(),
              z = newA.z(),
              a = newA,
              a7 = if ((nn & 0x80) == 0) currentStatus.a7 else SingleStatus(true),
              a0 = if ((nn & 1) == 0) currentStatus.a0 else SingleStatus(true),
              src = SingleStatus(SourceOfNZ(a = true)))
          case AssemblyLine(ALR, Immediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt & 0xff
            val newA = currentStatus.a.map(i => (i & n & 0xff) >> 1)
            currentStatus = currentStatus.copy(
              n = newA.n(),
              z = newA.z(),
              c = currentStatus.a.map(i => (i & n & 1) == 0),
              a = newA,
              a0 = newA.bit0,
              a7 = newA.bit7,
              src = SingleStatus(SourceOfNZ(a = true)))


          case AssemblyLine(ADC_W, WordImmediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt & 0xffff
            val newA = currentStatus.aw.adc_w(n, currentStatus.c, currentStatus.d)
            currentStatus = currentStatus.copy(
              n = newA.nw(),
              z = newA.zw(),
              a = newA.lo,
              ah = newA.hi,
              a0 = newA.bit0,
              a7 = newA.bit7,
              c = AnyStatus(),
              v = AnyStatus(),
              src = SingleStatus(SourceOfNZ(aw = true)))
          case AssemblyLine(AND_W, WordImmediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt & 0xffff
            val newA = currentStatus.aw.map(_ & 0xffff)
            currentStatus = currentStatus.copy(
              n = newA.nw(),
              z = newA.zw(),
              a = newA.lo,
              ah = newA.hi,
              a0 = newA.bit0,
              a7 = newA.bit7,
              src = SingleStatus(SourceOfNZ(aw = true)))
          case AssemblyLine(EOR_W, WordImmediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt & 0xffff
            val newA = currentStatus.aw.map(_ ^ 0xffff)
            currentStatus = currentStatus.copy(
              n = newA.nw(),
              z = newA.zw(),
              a = newA.lo,
              ah = newA.hi,
              a0 = newA.bit0,
              a7 = newA.bit7,
              src = SingleStatus(SourceOfNZ(aw = true)))
          case AssemblyLine(ORA_W, WordImmediate, NumericConstant(nn, _), _) =>
            val n = nn.toInt & 0xffff
            val newA = currentStatus.aw.map(_ | 0xffff)
            currentStatus = currentStatus.copy(
              n = newA.nw(),
              z = newA.zw(),
              a = newA.lo,
              ah = newA.hi,
              a0 = newA.bit0,
              a7 = newA.bit7,
              src = SingleStatus(SourceOfNZ(aw = true)))

          case AssemblyLine(INX, Implied, _, _) =>
            val newX = currentStatus.x.map(v => (v + 1) & 0xff)
            currentStatus = currentStatus.copy(
              n = newX.n(),
              z = newX.z(),
              x = newX,
              src = SingleStatus(SourceOfNZ(x = true)))
          case AssemblyLine(DEX, Implied, _, _) =>
            val newX = currentStatus.x.map(v => (v - 1) & 0xff)
            currentStatus = currentStatus.copy(
              n = newX.n(),
              z = newX.z(),
              x = newX,
              src = SingleStatus(SourceOfNZ(x = true)))
          case AssemblyLine(INY, Implied, _, _) =>
            val newY = currentStatus.y.map(v => (v + 1) & 0xff)
            currentStatus = currentStatus.copy(
              n = newY.n(),
              z = newY.z(),
              y = newY,
              src = SingleStatus(SourceOfNZ(y = true)))
          case AssemblyLine(DEY, Implied, _, _) =>
            val newY = currentStatus.y.map(v => (v - 1) & 0xff)
            currentStatus = currentStatus.copy(
              n = newY.n(),
              z = newY.z(),
              y = newY,
              src = SingleStatus(SourceOfNZ(y = true)))
          case AssemblyLine(INZ, Implied, _, _) =>
            val newIZ = currentStatus.iz.map(v => (v + 1) & 0xff)
            currentStatus = currentStatus.copy(
              n = newIZ.n(),
              z = newIZ.z(),
              iz = newIZ,
              src = SingleStatus(SourceOfNZ(iz = true)))
          case AssemblyLine(DEZ, Implied, _, _) =>
            val newIZ = currentStatus.iz.map(v => (v - 1) & 0xff)
            currentStatus = currentStatus.copy(
              n = newIZ.n(),
              z = newIZ.z(),
              iz = newIZ,
              src = SingleStatus(SourceOfNZ(iz = true)))
          case AssemblyLine(INC, Implied, _, _) =>
            val newA = currentStatus.a.map(v => (v + 1) & 0xff)
            currentStatus = currentStatus.copy(
              n = newA.n(),
              z = newA.z(),
              a = newA,
              a0 = currentStatus.a0.map(!_),
              a7 = newA.bit7,
              src = SingleStatus(SourceOfNZ(a = true)))
          case AssemblyLine(DEC, Implied, _, _) =>
            val newA = currentStatus.a.map(v => (v - 1) & 0xff)
            currentStatus = currentStatus.copy(
              n = newA.n(),
              z = newA.z(),
              a = newA,
              a0 = currentStatus.a0.map(!_),
              a7 = newA.bit7,
              src = SingleStatus(SourceOfNZ(a = true)))
          case AssemblyLine(NEG, Implied, _, _) =>
            val newA = currentStatus.a.map(v => (256 - v) & 0xff)
            currentStatus = currentStatus.copy(
              n = newA.n(),
              z = newA.z(),
              a = newA,
              a0 = currentStatus.a0,
              a7 = newA.bit7,
              src = SingleStatus(SourceOfNZ(a = true)))

          case AssemblyLine(INX_W, Implied, _, _) =>
            val newX = currentStatus.x.map(v => (v + 1) & 0xff)
            currentStatus = currentStatus.copy(
              n = AnyStatus(),
              z = newX.z().withHiddenHi,
              x = newX,
              src = AnyStatus())
          case AssemblyLine(DEX_W, Implied, _, _) =>
            val newX = currentStatus.x.map(v => (v - 1) & 0xff)
            currentStatus = currentStatus.copy(
              n = AnyStatus(),
              z = newX.z().withHiddenHi,
              x = newX,
              src = AnyStatus())
          case AssemblyLine(INY_W, Implied, _, _) =>
            val newY = currentStatus.y.map(v => (v + 1) & 0xff)
            currentStatus = currentStatus.copy(
              n = AnyStatus(),
              z = newY.z().withHiddenHi,
              y = newY,
              src = AnyStatus())
          case AssemblyLine(DEY_W, Implied, _, _) =>
            val newY = currentStatus.y.map(v => (v - 1) & 0xff)
            currentStatus = currentStatus.copy(
              n = AnyStatus(),
              z = newY.z().withHiddenHi,
              y = newY,
              src = AnyStatus())
          case AssemblyLine(INC_W, Implied, _, _) =>
            val newA = currentStatus.aw.map(v => (v + 1) & 0xffff)
            currentStatus = currentStatus.copy(
              n = newA.nw(),
              z = newA.zw(),
              a = newA.lo,
              a0 = newA.bit0,
              a7 = newA.bit7,
              ah = newA.hi,
              src = AnyStatus())
          case AssemblyLine(DEC_W, Implied, _, _) =>
            val newA = currentStatus.aw.map(v => (v - 1) & 0xffff)
            currentStatus = currentStatus.copy(
              n = newA.nw(),
              z = newA.zw(),
              a = newA.lo,
              a0 = newA.bit0,
              a7 = newA.bit7,
              ah = newA.hi,
              src = AnyStatus())

          case AssemblyLine(TAX, _, _, _) =>
            currentStatus = currentStatus.copy(
              x = currentStatus.a,
              n = currentStatus.a.n(),
              z = currentStatus.a.z(),
              src = SingleStatus(SourceOfNZ(a = true, x = true)))
          case AssemblyLine(TXA, _, _, _) =>
            currentStatus = currentStatus.copy(
              a = currentStatus.x,
              a0 = currentStatus.x.bit0,
              a7 = currentStatus.x.bit7,
              n = currentStatus.x.n(),
              z = currentStatus.x.z(),
              src = SingleStatus(SourceOfNZ(a = true, x = true)))
          case AssemblyLine(TAY, _, _, _) =>
            currentStatus = currentStatus.copy(
              y = currentStatus.a,
              n = currentStatus.a.n(),
              z = currentStatus.a.z(),
              src = SingleStatus(SourceOfNZ(a = true, y = true)))
          case AssemblyLine(TYA, _, _, _) =>
            currentStatus = currentStatus.copy(
              a = currentStatus.y,
              a0 = currentStatus.y.bit0,
              a7 = currentStatus.y.bit7,
              n = currentStatus.y.n(),
              z = currentStatus.y.z(),
              src = SingleStatus(SourceOfNZ(a = true, y = true)))
          case AssemblyLine(TXY, _, _, _) =>
            currentStatus = currentStatus.copy(
              y = currentStatus.x,
              n = currentStatus.x.n(),
              z = currentStatus.x.z(),
              src = SingleStatus(SourceOfNZ(a = true, y = true)))
          case AssemblyLine(TYX, _, _, _) =>
            currentStatus = currentStatus.copy(
              x = currentStatus.y,
              n = currentStatus.y.n(),
              z = currentStatus.y.z(),
              src = SingleStatus(SourceOfNZ(a = true, y = true)))
          case AssemblyLine(TAZ, _, _, _) =>
            currentStatus = currentStatus.copy(
              iz = currentStatus.a,
              n = currentStatus.a.n(),
              z = currentStatus.a.z(),
              src = SingleStatus(SourceOfNZ(a = true, iz = true)))
          case AssemblyLine(TZA, _, _, _) =>
            currentStatus = currentStatus.copy(
              a = currentStatus.iz,
              a0 = currentStatus.iz.bit0,
              a7 = currentStatus.iz.bit7,
              n = currentStatus.iz.n(),
              z = currentStatus.iz.z(),
              src = SingleStatus(SourceOfNZ(a = true, iz = true)))

          case AssemblyLine(HuSAX, _, _, _) =>
            currentStatus = currentStatus.copy(
              a = currentStatus.x,
              a0 = currentStatus.x.bit0,
              a7 = currentStatus.x.bit7,
              x = currentStatus.a)
          case AssemblyLine(SAY, _, _, _) =>
            currentStatus = currentStatus.copy(
              a = currentStatus.y,
              a0 = currentStatus.y.bit0,
              a7 = currentStatus.y.bit7,
              y = currentStatus.a)
          case AssemblyLine(SXY, _, _, _) =>
            currentStatus = currentStatus.copy(
              y = currentStatus.x,
              x = currentStatus.y)

          case AssemblyLine(ASL, Implied, _, _) =>
            val newA = currentStatus.a.map(v => (v << 1) & 0xff)
            currentStatus = currentStatus.copy(
              a = newA,
              a7 = newA.bit7,
              a0 = SingleStatus(false),
              n = newA.n(),
              z = newA.z(),
              c = currentStatus.a7,
              src = SingleStatus(SourceOfNZ(a = true)))
          case AssemblyLine(LSR, Implied, _, _) =>
            val newA = currentStatus.a.map(a => a.>>(1).&(0x7f))
            currentStatus = currentStatus.copy(
              a = newA,
              a0 = newA.bit0,
              a7 = SingleStatus(false),
              n = newA.n(),
              z = newA.z(),
              c = currentStatus.a0,
              src = SingleStatus(SourceOfNZ(a = true)))
          case AssemblyLine(ROL, Implied, _, _) =>
            val newA = (currentStatus.a <*> currentStatus.c) { (aa, cc) => aa.<<(1).&(0xff).|(if (cc) 1 else 0) }
            currentStatus = currentStatus.copy(
              a = newA,
              a7 = newA.bit7,
              a0 = currentStatus.c,
              n = newA.n(),
              z = newA.z(),
              c = currentStatus.a7,
              src = SingleStatus(SourceOfNZ(a = true)))
          case AssemblyLine(ROR, Implied, _, _) =>
            val newA = (currentStatus.a <*> currentStatus.c) { (aa, cc) => aa.>>(1).&(0x7f).|(if (cc) 0x80 else 0) }
            currentStatus = currentStatus.copy(
              a = newA,
              a0 = newA.bit0,
              a7 = currentStatus.c,
              n = newA.n(),
              z = newA.z(),
              c = currentStatus.a0,
              src = SingleStatus(SourceOfNZ(a = true)))
          case AssemblyLine(ASR, Implied, _, _) =>
            val newA = currentStatus.a.map(a => a.toByte.>>(1).&(0xff))
            currentStatus = currentStatus.copy(
              a = newA,
              a0 = newA.bit0,
              a7 = currentStatus.a7,
              n = newA.n(),
              z = newA.n(),
              c = currentStatus.a0,
              src = SingleStatus(SourceOfNZ(a = true)))

          case AssemblyLine(ASL_W, Implied, _, _) =>
            val newA = currentStatus.a.map(v => (v << 1) & 0xff)
            currentStatus = currentStatus.copy(
              a = newA,
              a7 = newA.bit7,
              a0 = SingleStatus(false),
              n = AnyStatus(),
              z = currentStatus.a.z(_ << 1).withHiddenHi,
              c = AnyStatus(),
              src = SingleStatus(SourceOfNZ(aw = true)))
          case AssemblyLine(LSR_W, Implied, _, _) =>
            currentStatus = currentStatus.copy(
              a = AnyStatus(),
              a7 = AnyStatus(),
              a0 = currentStatus.a.map(aa => (aa & 0x2) != 0),
              n = AnyStatus(),
              z = currentStatus.a.z(a => a.>>(1).&(0x7f)).withHiddenHi,
              c = currentStatus.a.map(a => a.&(1).!=(0)),
              src = SingleStatus(SourceOfNZ(aw = true)))

          case AssemblyLine(TSX, _, _, _) =>
            currentStatus = currentStatus.copy(
              x = AnyStatus(),
              src = SingleStatus(SourceOfNZ(x = true)))
          case AssemblyLine(
          SEI | CLI | TXS | NOP |
          STA | STX | STY | SAX | STZ |
          RTS | RTI | RTL |
          PHA | PHX | PHY | PHZ |
          PHA_W | PHX_W | PHY_W |
          PHP | PHD | PHB | PHK |
          DISCARD_AF | DISCARD_XF | DISCARD_YF, _, _, _) =>
          // no changes
          case AssemblyLine(ADC, _, _, _) =>
            currentStatus = currentStatus.copy(
              a = AnyStatus(),
              a7 = AnyStatus(),
              a0 = AnyStatus(),
              v = AnyStatus(),
              c = Status.flatMap3(currentStatus.a, currentStatus.c, currentStatus.d) {
                case (0, false, false) => SingleStatus[Boolean](false)
                case _ => AnyStatus()
              },
              z = AnyStatus(),
              n = AnyStatus(),
              src = currentStatus.d.flatMap((dec: Boolean) => if (dec) AnyStatus() else SingleStatus(SourceOfNZ(a = true))))
          case AssemblyLine(SBC, _, _, _) =>
            currentStatus = currentStatus.copy(
              a = AnyStatus(),
              a7 = AnyStatus(),
              a0 = AnyStatus(),
              v = AnyStatus(),
              c = Status.flatMap3(currentStatus.a, currentStatus.c, currentStatus.d) {
                case (0xff, true, false) => SingleStatus[Boolean](true)
                case _ => AnyStatus()
              },
              z = AnyStatus(),
              n = AnyStatus(),
              src = currentStatus.d.flatMap((dec: Boolean) => if (dec) AnyStatus() else SingleStatus(SourceOfNZ(a = true))))
          case AssemblyLine(AND, _, _, _) =>
            val newA: Status[Int] = currentStatus.a.flatMap(v => if ((v & 0xff) == 0) SingleStatus(0) else AnyStatus())
            currentStatus = currentStatus.copy(
              a = newA,
              a7 = currentStatus.a7.flatMap(v => if (!v) SingleStatus(false) else AnyStatus()),
              a0 = currentStatus.a0.flatMap(v => if (!v) SingleStatus(false) else AnyStatus()),
              n = newA.n(),
              z = newA.z(),
              src = SingleStatus(SourceOfNZ(a = true)))
          case AssemblyLine(ORA, _, _, _) =>
            val newA: Status[Int] = currentStatus.a.flatMap(v => if ((v & 0xff) == 0xff) SingleStatus(0xff) else AnyStatus())
            currentStatus = currentStatus.copy(
              a = newA,
              a7 = currentStatus.a7.flatMap(v => if (v) SingleStatus(true) else AnyStatus()),
              a0 = currentStatus.a0.flatMap(v => if (v) SingleStatus(true) else AnyStatus()),
              n = newA.n(),
              z = newA.z(),
              src = SingleStatus(SourceOfNZ(a = true)))
          case AssemblyLine(SLO, _, _, _) =>
            val newA: Status[Int] = currentStatus.a.flatMap(v => if ((v & 0xff) == 0xff) SingleStatus(0xff) else AnyStatus())
            currentStatus = currentStatus.copy(
              c = AnyStatus(),
              a = newA,
              a7 = currentStatus.a7.flatMap(v => if (v) SingleStatus(true) else AnyStatus()),
              a0 = currentStatus.a0.flatMap(v => if (v) SingleStatus(true) else AnyStatus()),
              n = newA.n(),
              z = newA.z(),
              src = SingleStatus(SourceOfNZ(a = true)))
          case AssemblyLine(EOR, _, _, _) =>
            currentStatus = currentStatus.copy(
              n = AnyStatus(),
              z = AnyStatus(),
              a = AnyStatus(),
              a7 = AnyStatus(),
              a0 = AnyStatus(),
              src = SingleStatus(SourceOfNZ(a = true)))
          case AssemblyLine(SRE | RLA, _, _, _) =>
            currentStatus = currentStatus.copy(
              c = AnyStatus(),
              n = AnyStatus(),
              z = AnyStatus(),
              a = AnyStatus(),
              a7 = AnyStatus(),
              a0 = AnyStatus(),
              src = SingleStatus(SourceOfNZ(a = true)))
          case AssemblyLine(ISC | RRA, _, _, _) =>
            currentStatus = currentStatus.copy(
              c = AnyStatus(),
              n = AnyStatus(),
              v = AnyStatus(),
              a = AnyStatus(),
              a7 = AnyStatus(),
              a0 = AnyStatus(),
              z = AnyStatus(),
              src = currentStatus.d.flatMap(dec => if (dec) AnyStatus() else SingleStatus(SourceOfNZ(a = true))))
          case AssemblyLine(ROR | ROL | LSR | ASL, _, _, _) =>
            currentStatus = currentStatus.copy(c = AnyStatus(), n = AnyStatus(), z = AnyStatus(), src = AnyStatus())
          case AssemblyLine(INC | DEC, _, _, _) =>
            currentStatus = currentStatus.copy(n = AnyStatus(), z = AnyStatus(), src = AnyStatus())

          case AssemblyLine(CMP, Immediate, NumericConstant(nn, _), _) =>
            currentStatus = currentStatus.copy(
              c = if (nn == 0) SingleStatus(true) else AnyStatus(),
              n = AnyStatus(),
              z = currentStatus.a.map(v => (v & 0xff) == (nn & 0xff)),
              src = if (nn == 0) SingleStatus(SourceOfNZ(a = true)) else AnyStatus())

          case AssemblyLine(CPX, Immediate, NumericConstant(nn, _), _) =>
            currentStatus = currentStatus.copy(
              c = if (nn == 0) SingleStatus(true) else AnyStatus(),
              n = AnyStatus(),
              z = currentStatus.x.map(v => (v & 0xff) == (nn & 0xff)),
              src = if (nn == 0) SingleStatus(SourceOfNZ(x = true)) else AnyStatus())

          case AssemblyLine(CPY, Immediate, NumericConstant(nn, _), _) =>
            currentStatus = currentStatus.copy(
              c = if (nn == 0) SingleStatus(true) else AnyStatus(),
              n = AnyStatus(),
              z = currentStatus.y.map(v => (v & 0xff) == (nn & 0xff)),
              src = if (nn == 0) SingleStatus(SourceOfNZ(y = true)) else AnyStatus())

          case AssemblyLine(CPZ, Immediate, NumericConstant(nn, _), _) =>
            currentStatus = currentStatus.copy(
              c = if (nn == 0) SingleStatus(true) else AnyStatus(),
              n = AnyStatus(),
              z = currentStatus.iz.map(v => (v & 0xff) == (nn & 0xff)),
              src = if (nn == 0) SingleStatus(SourceOfNZ(iz = true)) else AnyStatus())

          case AssemblyLine(CMP | CPX | CPY | CPZ, _, _, _) =>
            currentStatus = currentStatus.copy(
              c = AnyStatus(),
              n = AnyStatus(),
              z = AnyStatus(),
              src = AnyStatus())
          case AssemblyLine(BIT, _, _, _) =>
            currentStatus = currentStatus.copy(
              c = AnyStatus(),
              v = AnyStatus(),
              n = AnyStatus(),
              z = AnyStatus(),
              src = AnyStatus())
          case AssemblyLine(TRB | TSB, _, _, _) =>
            currentStatus = currentStatus.copy(
              z = AnyStatus(),
              src = AnyStatus())

          case AssemblyLine(JMP | BRA | BRL, _, _, _) =>
            currentStatus = new CpuStatus()

          case AssemblyLine(opcode, addrMode, parameter, _) =>
            currentStatus = currentStatus.copy(src = AnyStatus())
            if (OpcodeClasses.ChangesX(opcode)) currentStatus = currentStatus.copy(x = AnyStatus())
            if (OpcodeClasses.ChangesY(opcode)) currentStatus = currentStatus.copy(y = AnyStatus())
            if (OpcodeClasses.ChangesAAlways(opcode)) currentStatus = currentStatus.copy(a = AnyStatus(), a0 = AnyStatus(), a7 = AnyStatus())
            if (addrMode == Implied && OpcodeClasses.ChangesAIfImplied(opcode)) currentStatus = currentStatus.copy(a = AnyStatus(), a0 = AnyStatus(), a7 = AnyStatus())
            if (OpcodeClasses.ChangesAHAlways(opcode)) currentStatus = currentStatus.copy(ah = AnyStatus())
            if (addrMode == Implied && OpcodeClasses.ChangesAHIfImplied(opcode)) currentStatus = currentStatus.copy(ah = AnyStatus())
            if (OpcodeClasses.ChangesNAndZ(opcode)) currentStatus = currentStatus.nz
            if (OpcodeClasses.ChangesC(opcode)) currentStatus = currentStatus.copy(c = AnyStatus())
            if (OpcodeClasses.ChangesV(opcode)) currentStatus = currentStatus.copy(v = AnyStatus())
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
