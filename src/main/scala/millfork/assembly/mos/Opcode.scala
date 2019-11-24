package millfork.assembly.mos

import java.util.Locale

import millfork.error.{ConsoleLogger, Logger}
import millfork.node.Position

object State extends Enumeration {
  val
  // standard 6502 8-bit registers
  A, X, Y,
  // hi bytes of registers on 65816
  AH, XH, YH,
  // extra 65816 registers
  DP, DBL, PB,
  // extra register of both 65816 (it's called the high byte of DB) and 65CE02 (it's called B)
  DBH,
  // extra 65CE02 register
  IZ,
  // 8 extra HuC6280 MMU registers
  MM,
  // standard 6502 flags
  Z, D, C, N, V,
  // extra 65816 flags; W means X flag, not to confuse with the X register
  E, M, W = Value



  def isAffectedBySepRep(state: State.Value, n: Long): Boolean = {
    state match {
      case C => (n & 1) == 0
      case Z => (n & 2) == 0
      case D => (n & 8) == 0
      case W | XH | YH => (n & 0x10) == 0
      case M => (n & 0x20) == 0
      case V => (n & 0x40) == 0
      case N => (n & 0x80) == 0
      case _ => false
    }
  }
}

object Treatment extends Enumeration {
  val Unchanged, Unsure, Changed, Cleared, Set = Value

  implicit class OverriddenValue(val left: Value) extends AnyVal {
    def ~(right: Treatment.Value): Treatment.Value = right match {
      case Unchanged => left
      case Cleared | Set => if (left == Unsure) Changed else right
      case _ => right
    }
  }

}

object Opcode extends Enumeration {
  val ADC, AND, ASL,
  BIT, BNE, BEQ, BPL, BMI, BVS, BVC, BCC, BCS, BRK,
  CMP, CPX, CPY, CLV, CLC, CLI, CLD,
  DEC, DEX, DEY,
  EOR,
  INC, INX, INY,
  JMP, JSR,
  LDA, LDX, LDY, LSR,
  NOP,
  ORA,
  PHA, PHP, PLA, PLP,
  ROL, ROR, RTS, RTI,
  SBC, SEC, SED, SEI, STA, STX, STY,
  TAX, TAY, TXA, TXS, TSX, TYA,

  // illegals:
  LXA, XAA, ANC, ARR, ALR, SBX,
  LAX, SAX, RLA, RRA, SLO, SRE, DCP, ISC,
  TAS, LAS, SHX, SHY, AHX,

  // 65C02:
  STZ, PHX, PHY, PLX, PLY,
  BRA, TRB, TSB, STP, WAI,
  BBR0, BBR1, BBR2, BBR3, BBR4, BBR5, BBR6, BBR7,
  BBS0, BBS1, BBS2, BBS3, BBS4, BBS5, BBS6, BBS7,
  RMB0, RMB1, RMB2, RMB3, RMB4, RMB5, RMB6, RMB7,
  SMB0, SMB1, SMB2, SMB3, SMB4, SMB5, SMB6, SMB7,

  // 65CE02:
  CPZ, LDZ, DEZ, INZ,
  PHW,
  // DEW, INW, ASW, ROW, // aliases for DEC_W, INC_W, ASL_W, ROR_W (?)
  NEG, ASR,
  TAZ, TZA, PHZ, PLZ,
  TSY, TYS,
  TAB, TBA,
  CLE, SEE, // no idea what these do
  BSR,
  MAP, // also called AUG

  //HuC6280:
  CLA, CLX, CLY,
  CSH, CSL,
  SAY, SXY, HuSAX,
   SET,
  ST0, ST1, ST2,
  // BSR, // the same as on 65CE02
  TAM, TMA,
  TAI, TIA, TDD, TIN, TII, // memcpy instructions
  TST,

  //65816:
  BRL,
  COP,
  MVN, MVP, // memcpy instructions
  PEA, PEI, PER,
  PHB, PHD, PHK, PLB, PLD, // there's no PLK for the same reason Intel removed POP CS from 80186
  REP, SEP,
  RTL,
  TCD, TDC, TSC, TCS,
  TXY, TYX, XBA,
  XCE,
  DEC_W, INC_W, ROL_W, ROR_W, ASL_W, LSR_W,
  ORA_W, AND_W, EOR_W, ADC_W, LDA_W, STA_W, CMP_W, SBC_W, STZ_W, BIT_W, TRB_W, TSB_W,
  LDX_W, LDY_W, STX_W, STY_W, CPX_W, CPY_W,
  INX_W, INY_W, DEX_W, DEY_W,
  PHA_W, PLA_W,
  PHX_W, PHY_W, PLY_W, PLX_W,

  DISCARD_AF, DISCARD_XF, DISCARD_YF, CHANGED_MEM,
  BYTE, LABEL = Value

  def widen(opcode: Opcode.Value): Option[Opcode.Value] = opcode match {
    case ORA => Some(ORA_W)
    case AND => Some(AND_W)
    case EOR => Some(EOR_W)
    case ADC => Some(ADC_W)
    case SBC => Some(SBC_W)
    case CMP => Some(CMP_W)
    case LDA => Some(LDA_W)
    case STA => Some(STA_W)
    case STZ => Some(STZ_W)

    case LDX => Some(LDX_W)
    case LDY => Some(LDY_W)
    case STX => Some(STX_W)
    case STY => Some(STY_W)
    case INX => Some(INX_W)
    case INY => Some(INX_W)
    case DEX => Some(DEX_W)
    case DEY => Some(DEY_W)
    case CPX => Some(CPX_W)
    case CPY => Some(CPY_W)

    case INC => Some(INC_W)
    case DEC => Some(DEC_W)
    case ROL => Some(ROL_W)
    case ROR => Some(ROR_W)
    case ASL => Some(ASL_W)
    case LSR => Some(LSR_W)

    case PHA => Some(PHA_W)
    case PLA => Some(PLA_W)
    case PHX => Some(PHX_W)
    case PLX => Some(PLX_W)
    case PHY => Some(PHY_W)
    case PLY => Some(PLY_W)

    case _ => None
  }

  def lookup(opcode: String, position: Option[Position], log: Logger): Opcode.Value = opcode.toUpperCase(Locale.ROOT) match {
    case "ADC" => ADC
    case "AHX" => AHX
    case "ALR" => ALR
    case "ANC" => ANC
    case "AND" => AND
    case "ANE" => XAA
    case "ARR" => ARR
    case "ASL" => ASL
    case "ASO" => SLO
    case "ASR" => ASR
    case "ASW" => ASL_W
    case "AXA" => AHX
    case "AXS" => SBX // could mean SAX
    case "BCC" => BCC
    case "BCS" => BCS
    case "BEQ" => BEQ
    case "BIT" => BIT
    case "BMI" => BMI
    case "BNE" => BNE
    case "BPL" => BPL
    case "BRA" => BRA
    case "BRK" => BRK
    case "BRL" => BRL
    case "BSR" => BSR
    case "BVC" => BVC
    case "BVS" => BVS
    case "CLC" => CLC
    case "CLD" => CLD
    case "CLI" => CLI
    case "CLV" => CLV
    case "CLX" => CLX
    case "CLY" => CLY
    case "CMP" => CMP
    case "COP" => COP
    case "CPX" => CPX
    case "CPY" => CPY
    case "CPZ" => CPZ
    case "CSH" => CSH
    case "CSL" => CSL
    case "DCM" => DCP
    case "DCP" => DCP
    case "DEC" => DEC
    case "DEW" => DEC_W
    case "DEX" => DEX
    case "DEY" => DEY
    case "DEZ" => DEZ
    case "EOR" => EOR
    case "INC" => INC
    case "INS" => ISC
    case "INW" => INC_W
    case "INX" => INX
    case "INY" => INY
    case "INZ" => INZ
    case "ISC" => ISC
    case "JMP" => JMP
    case "JSR" => JSR
    case "LAS" => LAS
    case "LAX" => LAX
    case "LDA" => LDA
    case "LDX" => LDX
    case "LDY" => LDY
    case "LDZ" => LDZ
    case "LSE" => SRE
    case "LSR" => LSR
    case "LXA" => LXA
    case "NEG" => NEG
    case "NOP" => NOP
    case "OAL" => LXA
    case "ORA" => ORA
    case "PEA" => PEA
    case "PEI" => PEI
    case "PER" => PER
    case "PHA" => PHA
    case "PHB" => PHB
    case "PHD" => PHD
    case "PHK" => PHK
    case "PHP" => PHP
    case "PHW" => PHW
    case "PHX" => PHX
    case "PHY" => PHY
    case "PLA" => PLA
    case "PLB" => PLB
    case "PLD" => PLD
    case "PLP" => PLP
    case "PLX" => PLX
    case "PLY" => PLY
    case "REP" => REP
    case "RLA" => RLA
    case "ROL" => ROL
    case "ROR" => ROR
    case "ROW" => ROR_W // TODO: is this correct?
    case "RRA" => RRA
    case "RTI" => RTI
    case "RTL" => RTL
    case "RTS" => RTS
    case "SAX" => SAX // could mean SBX; also, HuC6280 has another SAX that means something else
    case "SAY" => SAY // could mean SHY
    case "SBC" => SBC
    case "SBX" => SBX
    case "SEC" => SEC
    case "SED" => SED
    case "SEI" => SEI
    case "SEP" => SEP
    case "SET" => SET
    case "SHX" => SHX
    case "SHY" => SHY
    case "SLO" => SLO
    case "SRE" => SRE
    case "ST0" => ST0
    case "ST1" => ST1
    case "ST2" => ST2
    case "STA" => STA
    case "STP" => STP
    case "STX" => STX
    case "STY" => STY
    case "STZ" => STZ
    case "SXY" => SXY
    case "TAB" => TAB
    case "TAI" => TAI
    case "TAM" => TAM
    case "TAS" => TAS
    case "TAX" => TAX
    case "TAY" => TAY
    case "TAZ" => TAZ
    case "TBA" => TBA
    case "TCD" => TCD
    case "TDC" => TDC
    case "TDD" => TDD
    case "TCS" => TCS
    case "TIA" => TIA
    case "TII" => TII
    case "TSC" => TSC
    case "TMA" => TMA
    case "TRB" => TRB
    case "TSB" => TSB
    case "TSX" => TSX
    case "TSY" => TSY
    case "TXA" => TXA
    case "TXS" => TXS
    case "TXY" => TXY
    case "TYA" => TYA
    case "TYS" => TYS
    case "TYX" => TYX
    case "TZA" => TZA
    case "WAI" => WAI
    case "XAA" => XAA
    case "XAS" => SHX
    case "XBA" => XBA
    case "XCE" => XCE
    case "BBR0" => BBR0
    case "BBR1" => BBR1
    case "BBR2" => BBR2
    case "BBR3" => BBR3
    case "BBR4" => BBR4
    case "BBR5" => BBR5
    case "BBR6" => BBR6
    case "BBR7" => BBR7
    case "BBS0" => BBS0
    case "BBS1" => BBS1
    case "BBS2" => BBS2
    case "BBS3" => BBS3
    case "BBS4" => BBS4
    case "BBS5" => BBS5
    case "BBS6" => BBS6
    case "BBS7" => BBS7
    case "RMB0" => RMB0
    case "RMB1" => RMB1
    case "RMB2" => RMB2
    case "RMB3" => RMB3
    case "RMB4" => RMB4
    case "RMB5" => RMB5
    case "RMB6" => RMB6
    case "RMB7" => RMB7
    case "SMB0" => SMB0
    case "SMB1" => SMB1
    case "SMB2" => SMB2
    case "SMB3" => SMB3
    case "SMB4" => SMB4
    case "SMB5" => SMB5
    case "SMB6" => SMB6
    case "SMB7" => SMB7
    case "TAI" => TAI
    case "TIA" => TIA
    case "TDD" => TDD
    case "TII" => TII
    case "TIN" => TIN
    case "TST" => TST

      // TODO: add all of those
    case _ =>
      log.error(s"Invalid opcode `$opcode`", position)
      LABEL
  }

}

