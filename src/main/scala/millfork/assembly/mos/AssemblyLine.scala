package millfork.assembly.mos

import millfork.assembly.{AbstractCode, Elidability, SourceLine}
import millfork.assembly.mos.Opcode._
import millfork.compiler.CompilationContext
import millfork.compiler.mos.MosCompiler
import millfork.env._
import millfork.node.Position
import millfork.{CompilationFlag, CompilationOptions}

//noinspection TypeAnnotation
object OpcodeClasses {

  val SingleBit = Set(
    BBR0, BBR1, BBR2, BBR3, BBR4, BBR5, BBR6, BBR7,
    BBS0, BBS1, BBS2, BBS3, BBS4, BBS5, BBS6, BBS7,
    RMB0, RMB1, RMB2, RMB3, RMB4, RMB5, RMB6, RMB7,
    SMB0, SMB1, SMB2, SMB3, SMB4, SMB5, SMB6, SMB7
  )
  val SingleBitBranch = Set(
    BBR0, BBR1, BBR2, BBR3, BBR4, BBR5, BBR6, BBR7,
    BBS0, BBS1, BBS2, BBS3, BBS4, BBS5, BBS6, BBS7
  )

  val HudsonTransfer = Set(TAI, TIA, TDD, TIN, TII)

  val ReadsAAlways = Set(
    ADC, AND, BIT, CMP, EOR, ORA, PHA, SBC, STA,
    ADC_W, AND_W, BIT_W, CMP_W, EOR_W, ORA_W, PHA_W, SBC_W, STA_W,
    TAX, TAY,
    SAX, SBX, ANC, DCP, ISC, RRA, RLA, SRE, SLO, LXA, XAA, AHX, TAS,
    TSB, TRB,
    TSB_W, TRB_W,
    TAZ, TAB,
    HuSAX, SAY, TAM,
    TCD, TCS, XBA,
  )
  val ReadsAIfImplied = Set(
    ASL, LSR, ROL, ROR, INC, DEC,
    DEC_W, INC_W, ROL_W, ROR_W, ASL_W, LSR_W,
  )
  val ReadsAHAlways = Set(
    ADC_W, AND_W, BIT_W, CMP_W, EOR_W, ORA_W, PHA_W, SBC_W, STA_W,
    TCD, TCS, XBA,
    TSB_W, TRB_W,
  )
  val ReadsAHIfImplied = Set(
    DEC_W, INC_W, ROL_W, ROR_W, ASL_W, LSR_W,
  )
  val ReadsXAlways = Set(
    CPX, DEX, INX, STX,
    CPX_W, DEX_W, INX_W, STX_W,
    TXA, TXS, SBX,
    PLX, PLX_W,
    XAA, SAX, AHX, SHX, TAS,
    HuSAX, SXY, SET,
    TXY,
  )
  val ReadsYAlways = Set(CPY, DEY, INY, STY, TYA, PLY, SHY, SAY, SXY, TYX)
  val ReadsIZAlways = Set(CPZ, DEZ, INZ, STZ, TZA, PLZ)
  val ReadsM = Set(
    ORA, AND, EOR, ADC, SBC, CMP, LDA, STA,
    ORA_W, AND_W, EOR_W, ADC_W, SBC_W, CMP_W, LDA_W, STA_W,
    STZ, BIT,
    STZ_W, BIT_W,
    PHA, PLA, PHP,
    PHA_W, PLA_W,
    DEC, INC, ROL, ROR, ASL, LSR,
    DEC_W, INC_W, ROL_W, ROR_W, ASL_W, LSR_W,
    TAX, TXA, TAY, TYA)
  val ReadsW = Set(
    LDX, LDY, CPX, CPY, STX, STY, INX, INY, DEX, DEY,
    LDX_W, LDY_W, CPX_W, CPY_W, STX_W, STY_W, INX_W, INY_W, DEX_W, DEY_W,
    PLX, PLY, PHX, PHY, PHP,
    PLX_W, PLY_W, PHX_W, PHY_W,
    TAX, TXA, TAY, TYA, TXY, TYX)
  val ReadsZ = Set(BNE, BEQ, PHP)
  val ReadsN = Set(BMI, BPL, PHP)
  val ReadsNOrZ = ReadsZ ++ ReadsN
  val ReadsV = Set(BVS, BVC, PHP)
  val ReadsD = Set(PHP, ADC, SBC, RRA, ARR, ISC, ADC_W, SBC_W)
  val ReadsC = Set(
    PHP, BCC, BCS,
    ADC, SBC, ROL, ROR,
    ADC_W, SBC_W, ROL_W, ROR_W,
    ALR, ARR, ISC, RLA, RRA, SLO, SRE,
    XCE
  )

  val ChangesAAlways = Set(
    TXA, TYA, PLA, PLA_W,
    ORA, AND, EOR, ADC, LDA, SBC,
    ORA_W, AND_W, EOR_W, ADC_W, LDA_W, SBC_W,
    SLO, RLA, SRE, RRA, LAX, ISC,
    XAA, ANC, ALR, ARR, LXA, LAS,
    TZA, NEG,
    TMA,
    XBA, TDC,
    HuSAX, SAY,
  )

  val ChangesAIfImplied = Set(
    ASL, LSR, ROL, ROR, INC, DEC,
    ASL_W, LSR_W, ROL_W, ROR_W, INC_W, DEC_W,
  )

  val ChangesAHAlways = Set(
    PLA_W,
    ORA_W, AND_W, EOR_W, ADC_W, LDA_W, SBC_W,
    XBA, TDC,
  )

  val ChangesAHIfImplied = Set(
    ASL, LSR, ROL, ROR, INC, DEC,
    ASL_W, LSR_W, ROL_W, ROR_W, INC_W, DEC_W,
  )

  val ChangesX = Set(
    DEX, INX, LDX,
    DEX_W, INX_W, LDX_W,
    TAX, TSX,
    SBX, LAX, LXA, LAS,
    PLX, PLX_W,
    TYX, SXY,
  )
  val ChangesY = Set(
    DEY, INY, LDY,
    DEY_W, INY_W, LDY_W,
    TAY,
    PLY, PLY_W,
    TXY, SXY,
  )
  val ChangesIZ = Set(
    DEZ, INZ, TAZ, LDZ,
  )
  val ChangesDirectPageRegister = Set(XCE, PLD, TCD)
  val ChangesDataBankRegister = Set(XCE, PLB, MVN, MVP)
  val ChangesS = Set(
    PHA, PLA,
    PHA_W, PLA_W,
    PHP, PLP, TXS,
    PHX, PHY, PLX, PLY,
    PHX_W, PHY_W, PLX_W, PLY_W,
    TAS, LAS,
    PHZ,
    PHB, PHD, PHK, PLB, PLD, RTL,
    PEA, PEI, PER,
    XCE, TCS, TYS,
  )
  val ChangesMemoryAlways = Set(
    STA, STY, STZ, STX,
    STA_W, STY_W, STZ_W, STX_W,
    TRB, TSB,
    TRB_W, TSB_W,
    SAX, DCP, ISC,
    SLO, RLA, SRE, RRA,
    AHX, SHY, SHX, TAS, LAS,
    TAM, TIN, TII, TIA, TAI, TST, TDD,
    COP,
    CHANGED_MEM,
  )
  val ChangesMemoryIfNotImplied = Set(
    DEC, INC, ASL, ROL, LSR, ROR,
    DEC_W, INC_W, ASL_W, ROL_W, LSR_W, ROR_W,
  )
  val ReadsMemoryIfNotImpliedOrImmediate = Set(
    LDY, CPX, CPY, BIT,
    LDY_W, CPX_W, CPY_W, BIT_W,
    ORA, AND, EOR, ADC, LDA, CMP, SBC,
    ORA_W, AND_W, EOR_W, ADC_W, LDA_W, CMP_W, SBC_W,
    ASL, ROL, LSR, ROR, LDX, DEC, INC,
    ASL_W, ROL_W, LSR_W, ROR_W, LDX_W, DEC_W, INC_W,
    SLO, RLA, SRE, RRA, LAX, DCP, ISC,
    LAS,
    TRB, TSB,
    TRB_W, TSB_W,
    TST, TAM, TII, TAI, TIN, TIA, TDD,
    CHANGED_MEM,
  ) ++ SingleBitBranch

  val AccessesWordInMemory = Set(
    LDA_W, LDX_W, LDY_W,
    STA_W, STX_W, STY_W,
    CMP_W, CPX_W, CPY_W,
    DEC_W, INC_W, ASL_W, ROL_W, LSR_W, ROR_W,
    ORA_W, AND_W, EOR_W, ADC_W, SBC_W,
    TSB_W, TRB_W, BIT_W,
    PHW,
  )

  val AccessesWordInMemoryAlwaysIfNotImplied = Set(
  )

  val StoresByte = Set(STA, STX, STY, STZ, SAX)
  val StoresWord = Set(STA_W, STX_W, STY_W, STZ_W)

  val OverwritesA = Set(
    LDA, PLA,
    LDA_W, PLA_W,
    TXA, TYA,
    LAX, LAS,
    TBA, TZA,
    HuSAX, SAY,
    TDC, TSC,
  )
  val OverwritesAH = Set(
    LDA_W, PLA_W,
    TDC, TSC,
  )
  val OverwritesX = Set(
    TAX, LDX, TSX, PLX,
    LAX, LAS,
    TYX, HuSAX, SXY,
  )
  val OverwritesY = Set(
    TAY, LDY, PLY,
    TSY, TXY, SAY, SXY,
  )
  val OverwritesIZ = Set(
    TAZ, LDZ, PLZ,
  )
  val OverwritesC = Set(CLC, SEC, PLP, XCE)
  val OverwritesD = Set(CLD, SED, PLP)
  val OverwritesI = Set(CLI, SEI, PLP)
  val OverwritesV = Set(CLV, PLP)
  val ConcernsAAlways = ReadsAAlways ++ ChangesAAlways
  val ConcernsAHAlways = ReadsAHAlways ++ ChangesAHAlways
  val ConcernsAIfImplied = ReadsAIfImplied ++ ChangesAIfImplied
  val ConcernsAHIfImplied = ReadsAHIfImplied ++ ChangesAHIfImplied
  val ConcernsXAlways = ReadsXAlways | ChangesX
  val ConcernsYAlways = ReadsYAlways | ChangesY
  val ConcernsIZAlways = ReadsIZAlways | ChangesIZ

  val ChangesStack = Set(
    PHA, PLA, PHP, PLP,
    PHX, PLX, PHY, PLY,
    PHA_W, PLA_W,
    PHX_W, PLX_W, PHY_W, PLY_W,
    TXS,
    JSR, RTS, RTI,
    TAS, LAS,
    PHW, PHZ, PLZ,
    TYS, TCS,
    RTL, BSR,
    PHB, PHD, PHK, PLB, PLD,
    PEA, PEI, PER,
    XCE,
  )

  val ConcernsStackAlways = ChangesStack ++ Set(TSX, TSY, TSC)
  val ConcernsSAlways = ChangesS ++ Set(TSX, TSY, TSC)

  val ChangesNAndZ = Set(
    ADC, AND, ASL, BIT, CMP, CPX, CPY, DEC, DEX, DEY, EOR, INC, INX, INY, LDA,
    ADC_W, AND_W, ASL_W, BIT_W, CMP_W, CPX_W, CPY_W, DEC_W, DEX_W, DEY_W, EOR_W, INC_W, INX_W, INY_W, LDA_W,
    LDX, LDY, LSR, ORA, PLP, ROL, ROR, SBC, TAX, TAY, TXA, TYA,
    LDX_W, LDY_W, LSR_W, ORA_W, ROL_W, ROR_W, SBC_W,
    LAX, SBX, ANC, ALR, ARR, DCP, ISC, RLA, RRA, SLO, SRE, SAX,
    TSB, TRB, // These two do not change N, but lets pretend they do for simplicity
    TSB_W, TRB_W,
    NEG, ASR,
    CPZ, DEZ, INZ, LDZ,
    REP, SEP, // People usually don't use there to change N or Z, but let's assume they do
  )
  val ChangesC = Set(
    PLP, CLC, SEC,
    ADC, ASL, CMP, CPX, CPY, LSR, ROL, ROR, SBC,
    ADC_W, ASL_W, CMP_W, CPX_W, CPY_W, LSR_W, ROL_W, ROR_W, SBC_W,
    SBX, ANC, ALR, ARR, DCP, ISC, RLA, RRA, SLO, SRE,
    CPZ, ASR,
    XCE,
    REP, SEP, // People usually don't use there to change C, but let's assume they do
  )
  val ChangesV = Set(
    ADC, BIT, PLP, SBC,
    ARR, ISC, RRA,
    REP, SEP, // People usually don't use there to change V, but let's assume they do
  )

  val SupportsAbsoluteX = Set(
    ORA, AND, EOR, ADC, CMP, SBC,
    ORA_W, AND_W, EOR_W, ADC_W, CMP_W, SBC_W,
    ASL, ROL, LSR, ROR, DEC, INC,
    ASL_W, ROL_W, LSR_W, ROR_W, DEC_W, INC_W,
    SLO, RLA, SRE, RRA, DCP, ISC,
    STA, LDA, LDY, STZ,
    STA_W, LDA_W, LDY_W, STZ_W,
    SHY,
  )

  val SupportsAbsoluteY = Set(
    ORA, AND, EOR, ADC, CMP, SBC,
    ORA_W, AND_W, EOR_W, ADC_W, CMP_W, SBC_W,
    SLO, RLA, SRE, RRA, DCP, ISC,
    STA, LDA, LDX,
    STA_W, LDA_W, LDX_W,
    LAX, AHX, SHX, TAS, LAS,
  )

  val SupportsAbsolute = Set(
    ORA, AND, EOR, ADC, STA, LDA, CMP, SBC,
    ORA_W, AND_W, EOR_W, ADC_W, STA_W, LDA_W, CMP_W, SBC_W,
    ASL, ROL, LSR, ROR, STX, LDX, DEC, INC,
    ASL_W, ROL_W, LSR_W, ROR_W, STX_W, LDX_W, DEC_W, INC_W,
    SLO, RLA, SRE, RRA, SAX, LAX, DCP, ISC,
    STY, LDY,
    BIT, JMP, JSR,
    STZ, TRB, TSB,
    LDZ,
  )

  val SupportsIndexedZ = Set(
    ORA, AND, EOR, ADC, STA, LDA, CMP, SBC,
    ORA_W, AND_W, EOR_W, ADC_W, STA_W, LDA_W, CMP_W, SBC_W,
  )

  val SupportsLongIndexedZ = Set(
    ORA, AND, EOR, ADC, STA, LDA, CMP, SBC,
    ORA_W, AND_W, EOR_W, ADC_W, STA_W, LDA_W, CMP_W, SBC_W,
  )

  val ShortConditionalBranching = Set(BEQ, BNE, BMI, BPL, BVC, BVS, BCC, BCS)
  val ShortBranching = ShortConditionalBranching + BRA
  val AllDirectJumps = ShortBranching ++ Set(JMP, BRL)
  val AllLinear = Set(
    ORA, AND, EOR,
    ORA_W, AND_W, EOR_W,
    ADC, SBC, CMP, CPX, CPY,
    ADC_W, SBC_W, CMP_W, CPX_W, CPY_W,
    DEC, DEX, DEY, INC, INX, INY,
    DEC_W, DEX_W, DEY_W, INC_W, INX_W, INY_W,
    ASL, ROL, LSR, ROR,
    ASL_W, ROL_W, LSR_W, ROR_W,
    LDA, STA, LDX, STX, LDY, STY,
    LDA_W, STA_W, LDX_W, STX_W, LDY_W, STY_W,
    TAX, TXA, TAY, TYA, TXS, TSX,
    PLA, PLP, PHA, PHP,
    PLA_W, PHA_W,
    BIT, BIT_W, NOP,
    CLC, SEC, CLD, SED, CLI, SEI, CLV,
    STZ, PHX, PHY, PLX, PLY, TSB, TRB,
    STZ_W, PHX_W, PHY_W, PLX_W, PLY_W, TSB_W, TRB_W,
    SLO, RLA, SRE, RRA, SAX, LAX, DCP, ISC,
    ANC, ALR, ARR, XAA, LXA, SBX,
    CPZ, LDZ, INZ, DEZ,
    TAZ, TZA, TYS, TSY,
    TBA,
    PLZ, PHZ, PHW,
    CLA, CLX, CLY,
    CSH, CSL,
    TXY, TYX, XBA,
    PHD, PHB, PHK,
    DISCARD_AF, DISCARD_XF, DISCARD_YF, CHANGED_MEM)

  val NoopDiscardsFlags = Set(DISCARD_AF, DISCARD_XF, DISCARD_YF)
  val DiscardsV = NoopDiscardsFlags | OverwritesV
  val DiscardsC = NoopDiscardsFlags | OverwritesC
  val DiscardsD = OverwritesD
  val DiscardsI = NoopDiscardsFlags | OverwritesI

  val SupportsZeropage = Set(
    LDA, LDX, LDY, LAX, // LDZ doesn't!
    LDA_W, LDX_W, LDY_W,
    STA, STX, STY, SAX, STZ,
    STA_W, LDX_W, LDY_W,
    BIT,
    CPX, CPY, CPZ,
    ADC, SBC, CMP, AND, ORA, EOR,
    ADC_W, SBC_W, CMP_W, AND_W, ORA_W, EOR_W,
    INC, DEC, ROL, ROR, ASL, LSR,
    INC_W, DEC_W, ROL_W, ROR_W, ASL_W, LSR_W,
    ISC, DCP, RLA, RRA, SLO, SRE,
    TSB, TRB,
  )

}

object AssemblyLine {

  val accu8: AssemblyLine = AssemblyLine.immediate(SEP, 0x20)
  val accu16: AssemblyLine = AssemblyLine.immediate(REP, 0x20)
  val index8: AssemblyLine = AssemblyLine.immediate(SEP, 0x10)
  val index16: AssemblyLine = AssemblyLine.immediate(REP, 0x10)

  def treatment(lines: List[AssemblyLine], state: State.Value): Treatment.Value =
    lines.map(_.treatment(state)).foldLeft(Treatment.Unchanged)(_ ~ _)

  def label(label: String): AssemblyLine = AssemblyLine.label(Label(label))

  def label(label: Label): AssemblyLine = AssemblyLine(LABEL, AddrMode.DoesNotExist, label.toAddress)

  def discardAF() = AssemblyLine(DISCARD_AF, AddrMode.DoesNotExist, Constant.Zero)

  def discardXF() = AssemblyLine(DISCARD_XF, AddrMode.DoesNotExist, Constant.Zero)

  def discardYF() = AssemblyLine(DISCARD_YF, AddrMode.DoesNotExist, Constant.Zero)

  def immediate(opcode: Opcode.Value, value: Long) = AssemblyLine(opcode, AddrMode.Immediate, NumericConstant(value, 1))

  def immediate(opcode: Opcode.Value, value: Constant) = AssemblyLine(opcode, AddrMode.Immediate, value)

  def implied(opcode: Opcode.Value) = AssemblyLine(opcode, AddrMode.Implied, Constant.Zero)

  private val opcodesForNopVariableOperation = Set(STA, SAX, STX, STY, STZ)
  private val opcodesForZeroedVariableOperation = Set(ADC, EOR, ORA, AND, SBC, CMP, CPX, CPY)
  private val opcodesForZeroedOrSignExtendedVariableOperation = Set(LDA, LDX, LDY, LDZ)

  def variable(ctx: CompilationContext, opcode: Opcode.Value, variable: Variable, offset: Int = 0): List[AssemblyLine] =
    if (offset >= variable.typ.size) {
      if (opcodesForNopVariableOperation(opcode)) {
        Nil
      } else if (opcodesForZeroedVariableOperation(opcode)) {
        if (variable.typ.isSigned) ???
        List(AssemblyLine.immediate(opcode, 0))
      } else if (opcodesForZeroedOrSignExtendedVariableOperation(opcode)) {
        if (variable.typ.isSigned) {
          val label = ctx.nextLabel("sx")
          AssemblyLine.variable(ctx, LDA, variable, variable.typ.size - 1) ++ List(
            AssemblyLine.immediate(ORA, 0x7f),
            AssemblyLine.relative(BMI, label),
            AssemblyLine.immediate(LDA, 0),
            AssemblyLine.label(label)) ++ (opcode match {
            case LDA => Nil
            case LDX | LAX => List(AssemblyLine.implied(TAX))
            case LDY => List(AssemblyLine.implied(TAY))
            case LDZ => List(AssemblyLine.implied(TAZ))
          })
        } else {
          List(AssemblyLine.immediate(opcode, 0))
        }
      } else {
        ???
      }
    } else {
      val elidability = if (variable.isVolatile) Elidability.Volatile else Elidability.Elidable
      variable match {
        case v@MemoryVariable(_, _, VariableAllocationMethod.Zeropage) =>
          List(AssemblyLine.zeropage(opcode, v.toAddress + offset).copy(elidability = elidability))
        case v@RelativeVariable(_, _, _, true, None, _) =>
          List(AssemblyLine.zeropage(opcode, v.toAddress + offset).copy(elidability = elidability))
        case v: VariableInMemory => List(AssemblyLine.absolute(opcode, v.toAddress + offset).copy(elidability = elidability))
        case v: StackVariable =>
          AssemblyLine.tsx(ctx) :+ AssemblyLine.dataStackX(ctx, opcode, v, offset)
      }
    }

  def zeropage(opcode: Opcode.Value, addr: Constant) =
    AssemblyLine(opcode, AddrMode.ZeroPage, addr)

  def zeropage(opcode: Opcode.Value, thing: ThingInMemory, offset: Int = 0) =
    AssemblyLine(opcode, AddrMode.ZeroPage, thing.toAddress + offset)

  def absolute(opcode: Opcode.Value, addr: Constant) =
    AssemblyLine(opcode, AddrMode.Absolute, addr)

  def absoluteOrLongAbsolute(opcode: Opcode.Value, thing: ThingInMemory, options: CompilationOptions): AssemblyLine =
    if (thing.isFar(options)) AssemblyLine(opcode, AddrMode.LongAbsolute, thing.toAddress)
    else AssemblyLine(opcode, AddrMode.Absolute, thing.toAddress)

  def absolute(opcode: Opcode.Value, thing: ThingInMemory, offset: Int = 0) =
    AssemblyLine(opcode, AddrMode.Absolute, thing.toAddress + offset)

  def relative(opcode: Opcode.Value, thing: ThingInMemory, offset: Int = 0) =
    AssemblyLine(opcode, AddrMode.Relative, thing.toAddress + offset)

  def relative(opcode: Opcode.Value, label: String) =
    AssemblyLine(opcode, AddrMode.Relative, Label(label).toAddress)

  def absoluteY(opcode: Opcode.Value, addr: Constant) =
    AssemblyLine(opcode, AddrMode.AbsoluteY, addr)

  def absoluteY(opcode: Opcode.Value, thing: ThingInMemory, offset: Int = 0) =
    AssemblyLine(opcode, AddrMode.AbsoluteY, thing.toAddress + offset)

  def absoluteX(opcode: Opcode.Value, addr: Int) =
    AssemblyLine(opcode, AddrMode.AbsoluteX, NumericConstant(addr, 2))

  def dataStackX(ctx: CompilationContext, opcode: Opcode.Value, v: StackVariable): AssemblyLine =
    dataStackX(ctx, opcode, v.baseOffset)

  def dataStackX(ctx: CompilationContext, opcode: Opcode.Value, v: StackVariable, offset: Int): AssemblyLine =
    dataStackX(ctx, opcode, v.baseOffset + offset)

  def dataStackX(ctx: CompilationContext, opcode: Opcode.Value, offset: Int): AssemblyLine =
    if (ctx.options.flag(CompilationFlag.SoftwareStack)) {
      val stack = ctx.env.get[ThingInMemory]("__stack")
      if (offset == 0x108) {
        println()
      }
      AssemblyLine.absoluteX(opcode, stack.toAddress + (offset - 0x100))
    } else if (ctx.options.flag(CompilationFlag.EmitEmulation65816Opcodes)) {
      AssemblyLine.stackRelative(opcode, offset + ctx.extraStackOffset)
    } else {
      AssemblyLine.absoluteX(opcode, offset + ctx.extraStackOffset)
    }

  def tsx(ctx: CompilationContext): List[AssemblyLine] =
    if (ctx.options.flag(CompilationFlag.SoftwareStack)) {
      val sp = ctx.env.get[ThingInMemory]("__sp")
      List(AssemblyLine.absolute(LDX, sp))
    } else if (ctx.options.flag(CompilationFlag.EmitEmulation65816Opcodes)) {
      Nil
    } else {
      List(AssemblyLine.implied(TSX))
    }

  def absoluteX(opcode: Opcode.Value, addr: Constant) =
    AssemblyLine(opcode, AddrMode.AbsoluteX, addr)

  def absoluteX(opcode: Opcode.Value, thing: ThingInMemory, offset: Int = 0) =
    AssemblyLine(opcode, AddrMode.AbsoluteX, thing.toAddress + offset)

  def indexedY(opcode: Opcode.Value, addr: Int) =
    AssemblyLine(opcode, AddrMode.IndexedY, NumericConstant(addr & 0xff, 1))

  def indexedY(opcode: Opcode.Value, addr: Constant) =
    AssemblyLine(opcode, AddrMode.IndexedY, addr)

  def indexedY(opcode: Opcode.Value, thing: ThingInMemory, offset: Int = 0) =
    AssemblyLine(opcode, AddrMode.IndexedY, thing.toAddress + offset)

  def indexedSY(opcode: Opcode.Value, offset: Int = 0) =
    AssemblyLine(opcode, AddrMode.IndexedSY, NumericConstant(offset & 0xff, 1))

  def stackRelative(opcode: Opcode.Value, addr: Int) =
    AssemblyLine(opcode, AddrMode.Stack, NumericConstant(addr & 0xff, 1))
}

object AssemblyLine0 {
  @inline
  def unapply(a: AssemblyLine): Some[(Opcode.Value, AddrMode.Value, Constant)] = Some(a.opcode, a.addrMode, a.parameter)
}

case class AssemblyLine(opcode: Opcode.Value, addrMode: AddrMode.Value, var parameter: Constant, elidability: Elidability.Value = Elidability.Elidable, source: Option[SourceLine] = None) extends AbstractCode {

  def pos(s: Option[SourceLine]): AssemblyLine = if (s.isEmpty || s == source) this else this.copy(source = s)

  def pos(s1: Option[SourceLine], s2: Option[SourceLine]): AssemblyLine = pos(Seq(s1, s2))

  def position(s: Option[Position]): AssemblyLine = pos(SourceLine.of(s))

  def positionIfEmpty(s: Option[Position]): AssemblyLine = if (s.isEmpty || source.isDefined) this else pos(SourceLine.of(s))

  def pos(s: Seq[Option[SourceLine]]): AssemblyLine = pos(SourceLine.merge(s))

  def mergePos(s: Seq[Option[SourceLine]]): AssemblyLine = if (s.isEmpty) this else pos(SourceLine.merge(this.source, s))

  @inline
  def elidable: Boolean = elidability == Elidability.Elidable

  @inline
  def notFixed: Boolean = elidability != Elidability.Fixed

  def refersTo(name: String): Boolean = parameter.refersTo(name)

  import AddrMode._
  import OpcodeClasses._
  import State._
  import Treatment._

  def reads(state: State.Value): Boolean = state match {
    case A => if (addrMode == Implied) ReadsAIfImplied(opcode) else ReadsAAlways(opcode)
    case AH => if (addrMode == Implied) ReadsAHIfImplied(opcode) else ReadsAHAlways(opcode)
    case X => addrMode == AbsoluteX || addrMode == LongAbsoluteX || addrMode == ZeroPageX || addrMode == IndexedX || ReadsXAlways(opcode)
    case Y => addrMode == AbsoluteY || addrMode == ZeroPageY || addrMode == IndexedY || addrMode == LongIndexedY || ReadsYAlways(opcode)
    case C => ReadsC(opcode)
    case D => ReadsD(opcode)
    case N => ReadsN(opcode)
    case V => ReadsV(opcode)
    case Z => ReadsZ(opcode)
    case IZ => addrMode == IndexedZ || ReadsIZAlways(opcode)
    case M => ReadsM(opcode)
    case W => ReadsW(opcode)
  }

  def concernsX: Boolean = addrMode == AbsoluteX || addrMode == LongAbsoluteX || addrMode == ZeroPageX || addrMode == IndexedX || ConcernsXAlways(opcode)

  def concernsY: Boolean = addrMode == AbsoluteY || addrMode == ZeroPageY || addrMode == IndexedY || addrMode == IndexedSY || addrMode == LongIndexedY || ConcernsYAlways(opcode)

  def treatment(state: State.Value): Treatment.Value = opcode match {
    case LABEL => Unchanged // TODO: ???
    case NOP => Unchanged
    case JSR | JMP | BEQ | BNE | BMI | BPL | BRK | BCC | BVC | BCS | BVS | BSR => Changed
    case CLC => if (state == C) Cleared else Unchanged
    case SEC => if (state == C) Set else Unchanged
    case CLV => if (state == V) Cleared else Unchanged
    case CLD => if (state == D) Cleared else Unchanged
    case SED => if (state == D) Set else Unchanged
    case SEP => parameter match {
      case NumericConstant(n, _) =>
        if (isAffectedBySepRep(state, n)) Set else Unchanged
      case _ => Changed
    }
    case REP => parameter match {
      case NumericConstant(n, _) =>
        if (isAffectedBySepRep(state, n)) Cleared else Unchanged
      case _ => Changed
    }
    case XCE => Changed
    case _ => state match { // TODO: smart detection of constants
      case A =>
        if (ChangesAAlways(opcode) || addrMode == Implied && ChangesAIfImplied(opcode))
          Changed
        else
          Unchanged
      case AH =>
        if (ChangesAHAlways(opcode) || addrMode == Implied && ChangesAHIfImplied(opcode))
          Changed
        else
          Unchanged
      case X => if (ChangesX(opcode)) Changed else Unchanged
      case Y => if (ChangesY(opcode)) Changed else Unchanged
      case IZ => if (ChangesIZ(opcode)) Changed else Unchanged
      case C => if (ChangesC(opcode)) Changed else Unchanged
      case V => if (ChangesV(opcode)) Changed else Unchanged
      case N | Z => if (ChangesNAndZ(opcode)) Changed else Unchanged
      case W | M | D => Unchanged
    }
  }

  override def sizeInBytes: Int = addrMode match {
    case Implied | RawByte => 1
    case Relative | ZeroPageX | ZeroPage | ZeroPageY | IndexedZ | IndexedX | IndexedY | IndexedSY | Stack | LongIndexedY | LongIndexedZ | Immediate => 2
    case AbsoluteIndexedX | AbsoluteX | Absolute | AbsoluteY | Indirect | LongRelative | WordImmediate | ZeroPageWithRelative | ImmediateWithZeroPage | ImmediateWithZeroPageX => 3
    case LongAbsolute | LongAbsoluteX | LongIndirect | ImmediateWithAbsolute | ImmediateWithAbsoluteX => 4
    case TripleAbsolute => 7
    case DoesNotExist => 0
  }

  def cost: Int = addrMode match {
    case Implied | RawByte => 1000
    case Relative | Immediate => 2000
    case ZeroPage => 2001
    case Stack | ZeroPageX | ZeroPageY => 2002
    case IndexedX | IndexedY | IndexedZ => 2003
    case IndexedSY | LongIndexedY | LongIndexedZ => 2004
    case WordImmediate => 3000
    case Absolute => 3001
    case AbsoluteX | AbsoluteY | Indirect => 3002
    case AbsoluteIndexedX => 3003
    case ZeroPageWithRelative => 3004
    case ImmediateWithZeroPage => 3005
    case ImmediateWithZeroPageX => 3006
    case LongAbsolute => 4000
    case LongAbsoluteX => 4001
    case LongIndirect => 4002
    case ImmediateWithAbsolute => 4003
    case ImmediateWithAbsoluteX => 4004
    case TripleAbsolute => 7000
    case DoesNotExist => 1
  }

  def isPrintable: Boolean = true //addrMode != AddrMode.DoesNotExist || opcode == LABEL

  override def toString: String = {
    val raw = if (opcode == LABEL) {
      parameter.toString + ':'
    } else if (opcode == BYTE) {
      "    !byte " + parameter.toString
    } else if (addrMode == TripleAbsolute) {
      parameter match {
        case StructureConstant(_, List(a,b,c)) => s"    $opcode $a,$b,$c"
      }
    } else if (addrMode == ZeroPageWithRelative) {
      parameter match {
        case StructureConstant(_, List(a,b)) => s"    $opcode $a,$b"
      }
    } else if (addrMode == ImmediateWithAbsolute || addrMode == ImmediateWithZeroPage) {
      parameter match {
        case StructureConstant(_, List(a,b)) => s"    $opcode #$a,$b"
      }
    } else if (addrMode == ImmediateWithAbsoluteX || addrMode == ImmediateWithZeroPageX) {
      parameter match {
        case StructureConstant(_, List(a,b)) => s"    $opcode #$a,$b,X"
      }
    } else if (addrMode == DoesNotExist) {
      s"    ; $opcode"
    } else {
      val op = opcode match {
        case HuSAX => "SAX"
        case _ => opcode.toString
      }
      s"    $op ${AddrMode.addrModeToString(addrMode, parameter.toString)}"
    }
    source match {
      case Some(SourceLine(_, line)) if line > 0 => f"$raw%-30s \t; @ $line%d"
      case _ => raw
    }
  }
}
