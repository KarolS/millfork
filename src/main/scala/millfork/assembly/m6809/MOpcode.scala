package millfork.assembly.m6809

/**
  * @author Karol Stasiak
  */

object MFlag extends Enumeration {
  val Z, C, H, N, V = Value
}

object MOpcode extends Enumeration {
  val ABX, ADCA, ADCB, ADDA, ADDB, ADDD, ANDA, ANDB, ANDCC, ASL, ASR,
  BITA, BITB,
  BRA, BRN, BHI, BLS, BCC, BCS, BNE, BEQ, BVC, BVS, BPL, BMI, BGE, BLT, BGT, BLE,
  CLR, CMPA, CMPB, CMPD, CMPS, CMPU, CMPX, CMPY, COMA, COMB, COM, CWAI,
  DAA, DEC,
  EORA, EORB, EXG,
  INC,
  JMP, JSR,
  LDA, LDB, LDD, LDS, LDU, LDX, LDY, LEAS, LEAU, LEAX, LEAY, LSR,
  LBRA, LBRN, LBHI, LBLS, LBCC, LBCS, LBNE, LBEQ, LBVC, LBVS, LBPL, LBMI, LBGE, LBLT, LBGT, LBLE,
  MUL,
  NEG, NOP,
  ORA, ORB, ORCC,
  PSHS, PSHU, PULS, PULU,
  ROL, ROR, RTI, RTS,
  SBCA, SBCB, SEX, STA, STB, STD, STS, STU, STX, STY, SUBA, SUBB, SUBD, SWI, SWI2, SWI3, SYNC,
  TFR, TST,
  DISCARD_D, DISCARD_X, DISCARD_Y, DISCARD_CC, CHANGED_MEM, BYTE, LABEL = Value

  val NoopDiscard: Set[MOpcode.Value] = Set(DISCARD_D, DISCARD_X, DISCARD_Y, DISCARD_CC)
  val PrefixedBy10: Set[MOpcode.Value] = Set(CMPD, CMPY, LDS, LDY, SWI2) // TODO: branches
  val PrefixedBy11: Set[MOpcode.Value] = Set(CMPS, CMPU, SWI3)
  val Prefixed: Set[MOpcode.Value] = PrefixedBy10 ++ PrefixedBy11
}
