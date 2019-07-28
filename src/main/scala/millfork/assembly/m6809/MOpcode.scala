package millfork.assembly.m6809

import java.util.Locale

import millfork.error.Logger
import millfork.node.Position

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
  MUL,
  NEG, NOP,
  ORA, ORB, ORCC,
  PSHS, PSHU, PULS, PULU,
  ROL, ROR, RTI, RTS,
  SBCA, SBCB, SEX, STA, STB, STD, STS, STU, STX, STY, SUBA, SUBB, SUBD, SWI, SWI2, SWI3, SYNC,
  TFR, TST,
  DISCARD_D, DISCARD_X, DISCARD_Y, DISCARD_CC, CHANGED_MEM, BYTE, LABEL = Value

  private val toMap: Map[String, MOpcode.Value] = {
    val notActualOpcodes: Set[MOpcode.Value] = Set(DISCARD_D, DISCARD_X, DISCARD_Y, DISCARD_CC, CHANGED_MEM, BYTE, LABEL)
    values.filterNot(notActualOpcodes).map(o => o.toString -> o).toMap ++ Map("BHS" -> BCC, "BLO" -> BCS, "LSL" -> ASL)
  }
  val NoopDiscard: Set[MOpcode.Value] = Set(DISCARD_D, DISCARD_X, DISCARD_Y, DISCARD_CC)
  val PrefixedBy10: Set[MOpcode.Value] = Set(CMPD, CMPY, LDS, LDY, SWI2) // TODO: branches
  val PrefixedBy11: Set[MOpcode.Value] = Set(CMPS, CMPU, SWI3)
  val Prefixed: Set[MOpcode.Value] = PrefixedBy10 ++ PrefixedBy11
  val CanHaveInherentAccumulator: Set[MOpcode.Value] = Set(ASL, ASR, CLR, COM, DEC, INC, LSR, NEG, ROL, ROR, TST)
  val Branching: Set[MOpcode.Value] = Set(BRA, BRN, BHI, BLS, BCC, BCS, BNE, BEQ, BVC, BVS, BPL, BMI, BGE, BLT, BGT, BLE)

  def lookup(opcode: String, position: Some[Position], log: Logger): (MOpcode.Value, Option[MAddrMode]) = {
    val o = opcode.toUpperCase(Locale.ROOT)
    System.out.flush()
    if (o.startsWith("LB") && toMap.contains(o.substring(1))) {
      toMap(o.substring(1)) -> Some(LongRelative)
    } else if (o.endsWith("A") && toMap.contains(o.init) && CanHaveInherentAccumulator(toMap(o.init))) {
      toMap(o.init) -> Some(InherentA)
    } else if (o.endsWith("B") && toMap.contains(o.init) && CanHaveInherentAccumulator(toMap(o.init))) {
      toMap(o.init) -> Some(InherentB)
    } else if (toMap.contains(o)) {
      toMap(o) -> None
    } else {
      log.error(s"Invalid opcode: $o", position)
      NOP -> None
    }
  }
}
