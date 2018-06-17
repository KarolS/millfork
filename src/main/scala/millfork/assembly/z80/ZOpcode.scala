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
  POP, PUSH,
  NOP,
  RLC, RRC, RL, RR, SLA, SRA, SRL, SLL, RLD,
  BIT, RES, SET,
  EXX, EX_DE_HL, EX_AF_AF, EX_SP,
  RST, IM, EI, DI,
  DJNZ, JP, JR, CALL, RET, RETN, RETI, HALT,
  DISCARD_A, DISCARD_F, DISCARD_HL, DISCARD_BCDEIX,
  LABEL, BYTE = Value
}

object ZOpcodeClasses {

  import ZOpcode._

  val EdInstructions = Set(SLA, SRA, SRL, SLL, BIT, RES, SET)
  val CbInstructions = Set(SLA, SRA, SRL, SLL, BIT, RES, SET)
  val CbInstructionsUnlessA = Set(RLC, RRC, RL, RR)

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
  val ChangesOnlyRegister = Set(INC, DEC, INC_16, DEC_16, POP, EX_SP, IN_C, IN_IMM, RL, RR, RLC, RRC, SLA, SRA, SRL, SLL, SET, RES)
  val ChangesFirstRegister = Set(LD, LD_16, ADD_16, SBC_16)
  val ChangesAAlways = Set(DAA, ADD, ADC, SUB, SBC, XOR, OR, AND)
  val NonLinear = Set(JP, JR, CALL, LABEL, BYTE, EXX, EX_DE_HL, EX_SP, EXX, RET, RETI, RETN, HALT)
}