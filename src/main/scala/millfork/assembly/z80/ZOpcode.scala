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
  EXX, EX_DE_HL, EX_AF_AF, EX_SP,
  RST, IM, EI, DI,
  DJNZ, JP, JR, CALL, RET, RETN, RETI, HALT,
  //sharp:
  LD_AHLI, LD_AHLD, LD_HLIA, LD_HLDA, SWAP, LD_H, LD_HLSP, ADD_SP, STOP,
  DISCARD_A, DISCARD_F, DISCARD_HL, DISCARD_BC, DISCARD_DE, DISCARD_IX, DISCARD_IY,
  LABEL, BYTE = Value
}

object ZOpcodeClasses {

  import ZOpcode._

  
  val RES_seq = IndexedSeq(RES0, RES1, RES2, RES3, RES4, RES5, RES6, RES7)
  val SET_seq = IndexedSeq(SET0, SET1, SET2, SET3, SET4, SET5, SET6, SET7)
  val BIT_seq = IndexedSeq(BIT0, BIT1, BIT2, BIT3, BIT4, BIT5, BIT6, BIT7)
  private val all_bit_seq = BIT_seq ++ RES_seq ++ SET_seq

  def singleBitOpcode(op:ZOpcode.Value): Int = 0x40 + all_bit_seq.indexOf(op) * 8

  val RES: Set[ZOpcode.Value] = RES_seq.toSet
  val SET: Set[ZOpcode.Value] = SET_seq.toSet
  val BIT: Set[ZOpcode.Value] = BIT_seq.toSet
  val AllSingleBit: Set[ZOpcode.Value] = RES ++ SET ++ BIT
  val RES_or_SET: Set[ZOpcode.Value] = RES ++ SET

  val CbInstructions: Set[ZOpcode.Value] = Set(SLA, SRA, SRL, SLL) ++ BIT ++ RES ++ SET
  val CbInstructionsUnlessA = Set(RLC, RRC, RL, RR)
  val EdInstructions: Set[ZOpcode.Value] = Set(NEG, RETN, RETI, IM, RRD, RLD,
    INI, INIR, OUTI, OUTIR, IND, INDR, OUTD, OUTDR,
    LDI, LDIR, LDD, LDDR, CPI, CPIR, CPD, CPDR) ++ BIT ++ RES ++ SET

  val NoopDiscards = Set(DISCARD_F, DISCARD_A, DISCARD_HL, DISCARD_BC, DISCARD_DE, DISCARD_IX, DISCARD_IY)

  val ChangesAFAlways = Set( // TODO: !
    DAA, ADD, ADC, SUB, SBC, XOR, OR, AND, INC, DEC,
    SCF, CCF, NEG,
    ADD_16, ADC_16, SBC_16, INC_16, DEC_16,
    INI, INIR, OUTI, OUTIR, IND, INDR, OUTD, OUTDR,
    LDI, LDIR, LDD, LDDR, CPI, CPIR, CPD, CPDR,
    EXX, CALL, JR, JP, LABEL, DJNZ)
  val ChangesBCAlways = Set(
    INI, INIR, OUTI, OUTIR, IND, INDR, OUTD, OUTDR,
    LDI, LDIR, LDD, LDDR, CPI, CPIR, CPD, CPDR,
    EXX, CALL, JR, JP, LABEL, DJNZ)
  val ChangesHLAlways = Set(
    INI, INIR, OUTI, OUTIR, IND, INDR, OUTD, OUTDR,
    LDI, LDIR, LDD, LDDR, CPI, CPIR, CPD, CPDR,
    EXX, EX_DE_HL, CALL, JR, JP, LABEL)
  val ChangesDEAlways = Set(
    LDI, LDIR, LDD, LDDR,
    EXX, EX_DE_HL, CALL, JR, JP, LABEL)
  val ChangesOnlyRegister: Set[ZOpcode.Value] = Set(INC, DEC, INC_16, DEC_16, POP, EX_SP, IN_C, IN_IMM, RL, RR, RLC, RRC, SLA, SRA, SRL, SLL) ++ SET ++ RES
  val ChangesFirstRegister = Set(LD, LD_16, ADD_16, SBC_16)
  val ChangesAAlways = Set(DAA, ADD, ADC, SUB, SBC, XOR, OR, AND)
  val NonLinear = Set(JP, JR, CALL, LABEL, BYTE, EXX, EX_DE_HL, EX_SP, EXX, RET, RETI, RETN, HALT)
}