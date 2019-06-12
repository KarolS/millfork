package millfork.assembly.z80

/**
  * @author Karol Stasiak
  */
object ZOpcode extends Enumeration {
  val LD,
  ADD, ADC, SUB, SBC, AND, XOR, OR, CP,
  INC, DEC,
  LD_16, ADD_16, ADC_16, SBC_16, INC_16, DEC_16,
  IN_IMM, IN_C, OUT_IMM, OUT_C,
  INI, INIR, OUTI, OUTIR, IND, INDR, OUTD, OUTDR,
  LDI, LDIR, LDD, LDDR,
  CPI, CPIR, CPD, CPDR,
  SCF, CCF, DAA, CPL, NEG,
  RES0, RES1, RES2, RES3, RES4, RES5, RES6, RES7,
  BIT0, BIT1, BIT2, BIT3, BIT4, BIT5, BIT6, BIT7,
  SET0, SET1, SET2, SET3, SET4, SET5, SET6, SET7,
  POP, PUSH,
  NOP,
  RLC, RRC, RL, RR, SLA, SRA, SRL, SLL, RLD, RRD,
  RLCA, RLA, RRA, RRCA,
  EXX, EX_DE_HL, EX_AF_AF, EX_SP,
  RST, IM, EI, DI,
  DJNZ, JP, JR, CALL, RET, RETN, RETI, HALT,
  // 8085:
  RIM, SIM,
  // 8085 undocumented
  LD_DESP, LD_DEHL, RRHL, RLDE, DSUB, RSTV, LHLX, SHLX,
  //sharp:
  LD_AHLI, LD_AHLD, LD_HLIA, LD_HLDA, SWAP, LDH_DA, LDH_AD, LDH_CA, LDH_AC, LD_HLSP, ADD_SP, STOP,
  DISCARD_A, DISCARD_F, DISCARD_HL, DISCARD_BC, DISCARD_DE, DISCARD_IX, DISCARD_IY, CHANGED_MEM,
  LABEL, BYTE = Value
}

object ZOpcodeClasses {

  import ZOpcode._

  
  val RES_seq: IndexedSeq[ZOpcode.Value] = IndexedSeq(RES0, RES1, RES2, RES3, RES4, RES5, RES6, RES7)
  val SET_seq: IndexedSeq[ZOpcode.Value] = IndexedSeq(SET0, SET1, SET2, SET3, SET4, SET5, SET6, SET7)
  val BIT_seq: IndexedSeq[ZOpcode.Value] = IndexedSeq(BIT0, BIT1, BIT2, BIT3, BIT4, BIT5, BIT6, BIT7)
  private val all_bit_seq = BIT_seq ++ RES_seq ++ SET_seq

  def singleBitOpcode(op:ZOpcode.Value): Int = 0x40 + all_bit_seq.indexOf(op) * 8

  val RES: Set[ZOpcode.Value] = RES_seq.toSet
  val SET: Set[ZOpcode.Value] = SET_seq.toSet
  val BIT: Set[ZOpcode.Value] = BIT_seq.toSet
  val AllSingleBit: Set[ZOpcode.Value] = RES ++ SET ++ BIT
  val RES_or_SET: Set[ZOpcode.Value] = RES ++ SET

  val CbInstructions: Set[ZOpcode.Value] = Set(SLA, SRA, SRL, SLL, RLC, RRC, RL, RR) ++ BIT ++ RES ++ SET
  val EdInstructions: Set[ZOpcode.Value] = Set(NEG, RETN, RETI, IM, RRD, RLD,
    INI, INIR, OUTI, OUTIR, IND, INDR, OUTD, OUTDR,
    LDI, LDIR, LDD, LDDR, CPI, CPIR, CPD, CPDR) ++ BIT ++ RES ++ SET

  val NoopDiscards: Set[ZOpcode.Value] = Set(DISCARD_F, DISCARD_A, DISCARD_HL, DISCARD_BC, DISCARD_DE, DISCARD_IX, DISCARD_IY)

  val ChangesAFAlways: Set[ZOpcode.Value] = Set( // TODO: !
    DAA, ADD, ADC, SUB, SBC, XOR, OR, AND, INC, DEC,
    SCF, CCF, NEG, RIM,
    LDH_AC, LDH_AD, LD_AHLI, LD_AHLD,
    ADD_16, ADC_16, SBC_16, INC_16, DEC_16,
    INI, INIR, OUTI, OUTIR, IND, INDR, OUTD, OUTDR,
    LDI, LDIR, LDD, LDDR, CPI, CPIR, CPD, CPDR,
    EXX, CALL, JR, JP, LABEL, DJNZ)
  val ChangesBCAlways: Set[ZOpcode.Value] = Set(
    INI, INIR, OUTI, OUTIR, IND, INDR, OUTD, OUTDR,
    LDI, LDIR, LDD, LDDR, CPI, CPIR, CPD, CPDR,
    EXX, CALL, JR, JP, LABEL, DJNZ)
  val ChangesHLAlways: Set[ZOpcode.Value] = Set(
    INI, INIR, OUTI, OUTIR, IND, INDR, OUTD, OUTDR,
    LDI, LDIR, LDD, LDDR, CPI, CPIR, CPD, CPDR,
    LD_AHLI, LD_AHLD, LD_HLIA, LD_HLDA, LD_HLSP, DSUB,
    RRHL, LHLX,
    EXX, EX_DE_HL, CALL, JR, JP, LABEL)
  val ChangesDEAlways: Set[ZOpcode.Value] = Set(
    LDI, LDIR, LDD, LDDR,
    LD_DESP, LD_DEHL, RLDE,
    EXX, EX_DE_HL, CALL, JR, JP, LABEL)
  val ChangesOnlyRegister: Set[ZOpcode.Value] = Set(INC, DEC, INC_16, DEC_16, POP, EX_SP, IN_C, IN_IMM, RL, RR, RLC, RRC, SLA, SRA, SRL, SLL) ++ SET ++ RES
  val ChangesFirstRegister: Set[ZOpcode.Value] = Set(LD, LD_16, ADD_16, SBC_16)
  val ChangesAAlways: Set[ZOpcode.Value] = Set(DAA, ADD, ADC, SUB, SBC, XOR, OR, AND, LD_AHLI, LD_AHLD, RIM)
  val NonLinear: Set[ZOpcode.Value] = Set(JP, JR, CALL, LABEL, BYTE, EXX, EX_DE_HL, EX_SP, EXX, RET, RETI, RETN, HALT, RST, RSTV)
}
