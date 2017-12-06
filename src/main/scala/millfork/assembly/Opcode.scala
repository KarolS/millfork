package millfork.assembly

import java.util.Locale

import millfork.error.ErrorReporting
import millfork.node.Position

object State extends Enumeration {
  val A, X, Y, Z, D, C, N, V = Value
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

  LXA, XAA, ANC, ARR, ALR, SBX,
  LAX, SAX, RLA, RRA, SLO, SRE, DCP, ISC,
  TAS, LAS, SHX, SHY, AHX,
  STZ, PHX, PHY, PLX, PLY,
  BRA, TRB, TSB, STP, WAI,
  DISCARD_AF, DISCARD_XF, DISCARD_YF,
  LABEL = Value

  def lookup(opcode: String, position: Option[Position]): Opcode.Value = opcode.toUpperCase(Locale.ROOT) match {
    case "ADC" => ADC
    case "AHX" => AHX
    case "ALR" => ALR
    case "ANC" => ANC
    case "AND" => AND
    case "ANE" => XAA
    case "ARR" => ARR
    case "ASL" => ASL
    case "ASO" => SLO
    case "AXA" => AHX
    case "AXS" => SBX // TODO: could mean SAX
    case "BCC" => BCC
    case "BCS" => BCS
    case "BEQ" => BEQ
    case "BIT" => BIT
    case "BMI" => BMI
    case "BNE" => BNE
    case "BPL" => BPL
    case "BRA" => BRA
    case "BRK" => BRK
    case "BVC" => BVC
    case "BVS" => BVS
    case "CLC" => CLC
    case "CLD" => CLD
    case "CLI" => CLI
    case "CLV" => CLV
    case "CMP" => CMP
    case "CPX" => CPX
    case "CPY" => CPY
    case "DCM" => DCP
    case "DCP" => DCP
    case "DEC" => DEC
    case "DEX" => DEX
    case "DEY" => DEY
    case "EOR" => EOR
    case "INC" => INC
    case "INS" => ISC
    case "INX" => INX
    case "INY" => INY
    case "ISC" => ISC
    case "JMP" => JMP
    case "JSR" => JSR
    case "LAS" => LAS
    case "LAX" => LAX
    case "LDA" => LDA
    case "LDX" => LDX
    case "LDY" => LDY
    case "LSE" => SRE
    case "LSR" => LSR
    case "LXA" => LXA
    case "NOP" => NOP
    case "OAL" => LXA
    case "ORA" => ORA
    case "PHA" => PHA
    case "PHP" => PHP
    case "PHX" => PHX
    case "PHY" => PHY
    case "PLA" => PLA
    case "PLP" => PLP
    case "PLX" => PLX
    case "PLY" => PLY
    case "RLA" => RLA
    case "ROL" => ROL
    case "ROR" => ROR
    case "RRA" => RRA
    case "RTI" => RTI
    case "RTS" => RTS
    case "SAX" => SAX // TODO: could mean SBX
    case "SAY" => SHY
    case "SBC" => SBC
    case "SBX" => SBX
    case "SEC" => SEC
    case "SED" => SED
    case "SEI" => SEI
    case "SHX" => SHX
    case "SHY" => SHY
    case "SLO" => SLO
    case "SRE" => SRE
    case "STA" => STA
    case "STP" => STP
    case "STX" => STX
    case "STY" => STY
    case "STZ" => STZ
    case "TAS" => TAS
    case "TAX" => TAX
    case "TAY" => TAY
    case "TRB" => TRB
    case "TSB" => TSB
    case "TSX" => TSX
    case "TXA" => TXA
    case "TXS" => TXS
    case "TYA" => TYA
    case "WAI" => WAI
    case "XAA" => XAA
    case "XAS" => SHX
    case _ =>
      ErrorReporting.error(s"Invalid opcode `$opcode`", position)
      LABEL
  }

}

object AddrMode extends Enumeration {
  val Implied,
  Immediate,
  Relative,
  ZeroPage,
  ZeroPageX,
  ZeroPageY,
  Absolute,
  AbsoluteX,
  AbsoluteY,
  Indirect,
  IndexedX,
  IndexedY,
  AbsoluteIndexedX,
  ZeroPageIndirect,
  Undecided,
  DoesNotExist = Value


  def argumentLength(a: AddrMode.Value): Int = a match {
    case Absolute | AbsoluteX | AbsoluteY | Indirect =>
      2
    case _ =>
      1
  }

  def addrModeToString(am: AddrMode.Value, argument: String): String = {
    am match {
      case Implied => ""
      case Immediate => "#" + argument
      case AbsoluteX | ZeroPageX => argument + ", X"
      case AbsoluteY | ZeroPageY => argument + ", Y"
      case IndexedX | AbsoluteIndexedX => "(" + argument + ", X)"
      case IndexedY => "(" + argument + "), Y"
      case Indirect | ZeroPageIndirect => "(" + argument + ")"
      case _ => argument;
    }
  }
}
