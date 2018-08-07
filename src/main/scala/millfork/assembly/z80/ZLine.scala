package millfork.assembly.z80

import millfork.CompilationFlag
import millfork.assembly.AbstractCode
import millfork.compiler.CompilationContext
import millfork.env.{Constant, Label, NumericConstant, ThingInMemory}
import millfork.node.ZRegister

/**
  * @author Karol Stasiak
  */

object ZFlag extends Enumeration {
  val Z, P, C, S, H, N = Value

  val AllButSZ: Seq[Value] = Seq(P, C, H, N)
  val AllButZ: Seq[Value] = Seq(P, C, H, N, S)
}

sealed trait ZRegisters

case object NoRegisters extends ZRegisters

case class IfFlagSet(flag: ZFlag.Value) extends ZRegisters

case class IfFlagClear(flag: ZFlag.Value) extends ZRegisters

case class OneRegister(register: ZRegister.Value) extends ZRegisters {
//  if (register == ZRegister.MEM_IY_D || register == ZRegister.MEM_IX_D) ???
}

case class TwoRegisters(target: ZRegister.Value, source: ZRegister.Value) extends ZRegisters {
//  if (target == ZRegister.MEM_IY_D || target == ZRegister.MEM_IX_D) ???
//  if (source == ZRegister.MEM_IY_D || source == ZRegister.MEM_IX_D) ???
}

case class OneRegisterOffset(register: ZRegister.Value, offset: Int) extends ZRegisters {
  if (register != ZRegister.MEM_IY_D && register != ZRegister.MEM_IX_D) ???
}

case class TwoRegistersOffset(target: ZRegister.Value, source: ZRegister.Value, offset: Int) extends ZRegisters {
  if (target != ZRegister.MEM_IY_D && target != ZRegister.MEM_IX_D && source != ZRegister.MEM_IY_D && source != ZRegister.MEM_IX_D) ???
}

sealed abstract class LocalVariableAddressOperand(val memViaRegister: ZRegister.Value) {
  def offset: Int
  def oneRegister: ZRegisters
  def asTargetWithSource(reg: ZRegister.Value): ZRegisters
  def asSourceWithTarget(reg: ZRegister.Value): ZRegisters
}

case class LocalVariableAddressViaIX(offset: Int) extends LocalVariableAddressOperand(ZRegister.MEM_IX_D) {

  override def oneRegister: ZRegisters = OneRegisterOffset(ZRegister.MEM_IX_D, offset)

  override def asTargetWithSource(reg: ZRegister.Value): ZRegisters = TwoRegistersOffset(ZRegister.MEM_IX_D, reg, offset)

  override def asSourceWithTarget(reg: ZRegister.Value): ZRegisters = TwoRegistersOffset(reg, ZRegister.MEM_IX_D, offset)
}

case class LocalVariableAddressViaIY(offset: Int) extends LocalVariableAddressOperand(ZRegister.MEM_IY_D) {

  override def oneRegister: ZRegisters = OneRegisterOffset(ZRegister.MEM_IY_D, offset)

  override def asTargetWithSource(reg: ZRegister.Value): ZRegisters = TwoRegistersOffset(ZRegister.MEM_IY_D, reg, offset)

  override def asSourceWithTarget(reg: ZRegister.Value): ZRegisters = TwoRegistersOffset(reg, ZRegister.MEM_IY_D, offset)
}

case object LocalVariableAddressViaHL extends LocalVariableAddressOperand(ZRegister.MEM_HL) {
  override def offset: Int = 0

  override def oneRegister: ZRegisters = OneRegister(ZRegister.MEM_HL)

  override def asTargetWithSource(reg: ZRegister.Value): ZRegisters = TwoRegisters(ZRegister.MEM_HL, reg)

  override def asSourceWithTarget(reg: ZRegister.Value): ZRegisters = TwoRegisters(reg, ZRegister.MEM_HL)
}

object ZLine {

  import ZOpcode._
  import ZRegister._

  def label(label: String): ZLine = ZLine.label(Label(label))

  def label(label: Label): ZLine = ZLine(LABEL, NoRegisters, label.toAddress)

  def jump(label: String): ZLine = ZLine(JP, NoRegisters, Label(label).toAddress)

  def jump(label: Label): ZLine = ZLine(JP, NoRegisters, label.toAddress)

  def jump(label: String, condition: ZRegisters): ZLine = ZLine(JP, condition, Label(label).toAddress)

  def jump(label: Label, condition: ZRegisters): ZLine = ZLine(JP, condition, label.toAddress)

  def jumpR(ctx: CompilationContext, label: String): ZLine = ZLine(if (ctx.options.flag(CompilationFlag.EmitExtended80Opcodes)) JR else JP, NoRegisters, Label(label).toAddress)

  def jumpR(ctx: CompilationContext, label: Label): ZLine = ZLine(if (ctx.options.flag(CompilationFlag.EmitExtended80Opcodes)) JR else JP, NoRegisters, label.toAddress)

  def jumpR(ctx: CompilationContext, label: String, condition: ZRegisters): ZLine = ZLine(if (ctx.options.flag(CompilationFlag.EmitExtended80Opcodes)) JR else JP, condition, Label(label).toAddress)

  def jumpR(ctx: CompilationContext, label: Label, condition: ZRegisters): ZLine = ZLine(if (ctx.options.flag(CompilationFlag.EmitExtended80Opcodes)) JR else JP, condition, label.toAddress)

  def djnz(ctx: CompilationContext, label: String): List[ZLine] =
    if (ctx.options.flag(CompilationFlag.EmitZ80Opcodes)) List(ZLine(DJNZ, NoRegisters, Label(label).toAddress))
    else List(ZLine.register(DEC, ZRegister.B), ZLine.jumpR(ctx, label, IfFlagClear(ZFlag.Z)))

  def djnz(ctx: CompilationContext, label: Label): List[ZLine] =
      if (ctx.options.flag(CompilationFlag.EmitZ80Opcodes)) List(ZLine(DJNZ, NoRegisters, label.toAddress))
      else List(ZLine.register(DEC, ZRegister.B), ZLine.jumpR(ctx, label, IfFlagClear(ZFlag.Z)))

  def implied(opcode: ZOpcode.Value): ZLine = ZLine(opcode, NoRegisters, Constant.Zero)

  def register(opcode: ZOpcode.Value, register: ZRegister.Value): ZLine = ZLine(opcode, OneRegister(register), Constant.Zero)

  def register(opcode: ZOpcode.Value, register: LocalVariableAddressOperand): ZLine = ZLine(opcode, register.oneRegister, Constant.Zero)

  def imm8(opcode: ZOpcode.Value, value: Constant): ZLine = ZLine(opcode, OneRegister(IMM_8), value)

  def imm8(opcode: ZOpcode.Value, value: Int): ZLine = ZLine(opcode, OneRegister(IMM_8), NumericConstant(value & 0xff, 1))

  def registers(opcode: ZOpcode.Value, target: ZRegister.Value, source: ZRegister.Value): ZLine = ZLine(opcode, TwoRegisters(target, source), Constant.Zero)

  def ld8(target: ZRegister.Value, source: ZRegister.Value): ZLine = ZLine(LD, TwoRegisters(target, source), Constant.Zero)

  def ld8(target: LocalVariableAddressOperand, source: ZRegister.Value): ZLine = ZLine(LD, target.asTargetWithSource(source), Constant.Zero)

  def ld8(target: ZRegister.Value, source: LocalVariableAddressOperand): ZLine = ZLine(LD, source.asSourceWithTarget(target), Constant.Zero)

  def ld16(target: ZRegister.Value, source: ZRegister.Value): ZLine = ZLine(LD_16, TwoRegisters(target, source), Constant.Zero)

  def ldImm8(target: LocalVariableAddressOperand, source: Int): ZLine = ZLine(LD, target.asTargetWithSource(IMM_8), NumericConstant(source & 0xff, 1))

  def ldImm8(target: LocalVariableAddressOperand, source: Constant): ZLine = ZLine(LD, target.asTargetWithSource(IMM_8), source)

  def ldImm8(target: ZRegister.Value, source: Int): ZLine = ZLine(LD, TwoRegisters(target, IMM_8), NumericConstant(source & 0xff, 1))

  def ldImm8(target: ZRegister.Value, source: Constant): ZLine = ZLine(LD, TwoRegisters(target, IMM_8), source)

  def ldImm16(target: ZRegister.Value, source: Int): ZLine = ZLine(LD_16, TwoRegisters(target, IMM_16), NumericConstant(source & 0xffff, 2))

  def ldImm16(target: ZRegister.Value, source: Constant): ZLine = ZLine(LD_16, TwoRegisters(target, IMM_16), source)

  def ldAbs8(target: ZRegister.Value, source: ThingInMemory): ZLine = ZLine(LD, TwoRegisters(target, MEM_ABS_8), source.toAddress)

  def ldAbs16(target: ZRegister.Value, source: ThingInMemory): ZLine = ZLine(LD_16, TwoRegisters(target, MEM_ABS_16), source.toAddress)

  def ldAbs8(target: ZRegister.Value, source: Constant): ZLine = ZLine(LD, TwoRegisters(target, MEM_ABS_8), source)

  def ldAbs16(target: ZRegister.Value, source: Constant): ZLine = ZLine(LD_16, TwoRegisters(target, MEM_ABS_16), source)

  def ldAbs8(target: ThingInMemory, source: ZRegister.Value): ZLine = ZLine(LD, TwoRegisters(MEM_ABS_8, source), target.toAddress)

  def ldAbs16(target: ThingInMemory, source: ZRegister.Value): ZLine = ZLine(LD_16, TwoRegisters(MEM_ABS_16, source), target.toAddress)

  def ldAbs8(target: Constant, source: ZRegister.Value): ZLine = ZLine(LD, TwoRegisters(MEM_ABS_8, source), target)

  def ldAbs16(target: Constant, source: ZRegister.Value): ZLine = ZLine(LD_16, TwoRegisters(MEM_ABS_16, source), target)

  def ldViaIx(target: ZRegister.Value, sourceOffset: Int): ZLine = ZLine(LD, TwoRegistersOffset(target, ZRegister.MEM_IX_D, sourceOffset), Constant.Zero)

  def ldViaIx(targetOffset: Int, source: ZRegister.Value): ZLine = ZLine(LD, TwoRegistersOffset(ZRegister.MEM_IX_D, source, targetOffset), Constant.Zero)

  def ldViaIy(target: ZRegister.Value, sourceOffset: Int): ZLine = ZLine(LD, TwoRegistersOffset(target, ZRegister.MEM_IY_D, sourceOffset), Constant.Zero)

  def ldViaIy(targetOffset: Int, source: ZRegister.Value): ZLine = ZLine(LD, TwoRegistersOffset(ZRegister.MEM_IY_D, source, targetOffset), Constant.Zero)

  def ldViaIxy(x: Boolean, target: ZRegister.Value, sourceOffset: Int): ZLine = if (x) ldViaIx(target, sourceOffset) else ldViaIy(target, sourceOffset)

  def ldViaIxy(x: Boolean, targetOffset: Int, source: ZRegister.Value): ZLine = if (x) ldViaIx(targetOffset, source) else ldViaIy(targetOffset, source)
}

case class ZLine(opcode: ZOpcode.Value, registers: ZRegisters, parameter: Constant, elidable: Boolean = true) extends AbstractCode {

  override def sizeInBytes: Int = {
    import ZOpcode._
    import ZRegister._
    val inherent = opcode match {
      case BYTE => 1
      case LABEL => return 0
      case d if ZOpcodeClasses.NoopDiscards(d) => return 0
      case JP => registers match {
        case OneRegister(HL | IX | IY) => 1
        case _ => 2
      }
      case JR => 2
      case o if ZOpcodeClasses.EdInstructions(o) => 2
      case o if ZOpcodeClasses.CbInstructions(o) => 2
      case _ => 1 // TODO!!!
    }
    val fromParams = registers match {
      case OneRegister(IX | IXL | IXH | IY | IYH | IYL | IMM_8) => 1
      case OneRegister(IMM_16 | MEM_ABS_8 | MEM_ABS_16) => 2
      case OneRegisterOffset(MEM_IX_D | MEM_IY_D, _) => 2
      case TwoRegisters(_, IX | IXL | IXH | IY | IYH | IYL | IMM_8) => 1
      case TwoRegisters(_, MEM_IX_D | MEM_IY_D | IMM_16 | MEM_ABS_8 | MEM_ABS_16) => 2
      case TwoRegistersOffset(_, MEM_IX_D | MEM_IY_D, _) => 2
      case TwoRegisters(IX | IXL | IXH | IY | IYH | IYL | IMM_8, _) => 1
      case TwoRegisters(IMM_16 | MEM_ABS_8 | MEM_ABS_16, _) => 2
      case TwoRegistersOffset(MEM_IX_D | MEM_IY_D, _, _) => 2
      case _ => 0
    }
    inherent + fromParams
  }


  override def isPrintable: Boolean = true

  private def asAssemblyString(r: ZRegister.Value, offset: Int = 0): String = r match {
    case ZRegister.A => "A"
    case ZRegister.B => "B"
    case ZRegister.C => "C"
    case ZRegister.D => "D"
    case ZRegister.E => "E"
    case ZRegister.H => "H"
    case ZRegister.L => "L"
    case ZRegister.AF => "AF"
    case ZRegister.BC => "BC"
    case ZRegister.DE => "DE"
    case ZRegister.HL => "HL"
    case ZRegister.SP => "SP"
    case ZRegister.IX => "IX"
    case ZRegister.IY => "IY"
    case ZRegister.IXH => "IXH"
    case ZRegister.IYH => "IYH"
    case ZRegister.IXL => "IXL"
    case ZRegister.IYL => "IYL"
    case ZRegister.R => "R"
    case ZRegister.I => "I"
    case ZRegister.MEM_ABS_8 => s"($parameter)"
    case ZRegister.MEM_ABS_16 => s"($parameter)"
    case ZRegister.IMM_8 => s"$parameter"
    case ZRegister.IMM_16 => s"$parameter"
    case ZRegister.MEM_IX_D => s"IX($offset)"
    case ZRegister.MEM_IY_D => s"IY($offset)"
    case ZRegister.MEM_HL => "(HL)"
    case ZRegister.MEM_BC => "(BC)"
    case ZRegister.MEM_DE => "(DE)"
  }

  private def asIntelAssemblyString(r: ZRegister.Value): String = r match {
    case ZRegister.A => "A"
    case ZRegister.B => "B"
    case ZRegister.C => "C"
    case ZRegister.D => "D"
    case ZRegister.E => "E"
    case ZRegister.H => "H"
    case ZRegister.L => "L"
    case ZRegister.AF => "PSW"
    case ZRegister.BC => "B"
    case ZRegister.DE => "D"
    case ZRegister.HL => "H"
    case ZRegister.SP => "SP"
    case ZRegister.MEM_ABS_8 => s"$parameter"
    case ZRegister.MEM_ABS_16 => s"$parameter"
    case ZRegister.IMM_8 => s"$parameter"
    case ZRegister.IMM_16 => s"$parameter"
    case ZRegister.MEM_HL => "M"
    case _ => "???"
  }

  override def toString: String = {
    import ZOpcode._
    opcode match {
      case DISCARD_A => "    ; DISCARD_A"
      case DISCARD_HL => "    ; DISCARD_HL"
      case DISCARD_F => "    ; DISCARD_F"
      case DISCARD_BC => "    ; DISCARD_BC"
      case DISCARD_DE => "    ; DISCARD_DE"
      case DISCARD_IX => "    ; DISCARD_IX"
      case DISCARD_IY => "    ; DISCARD_IY"
      case BYTE => "    DB " + parameter.toString // TODO: format?
      case LABEL => parameter.toString + ":"
      case RST => s"    RST $parameter"
      case IM => s"    IM $parameter"
      case EX_AF_AF => "    EX AF,AF'"
      case EX_DE_HL => "    EX DE,HL"
      case LD_AHLI => "    LD A,(HLI)"
      case LD_AHLD => "    LD A,(HLD)"
      case LD_HLIA => "    LD (HLI),A"
      case LD_HLDA => "    LD (HLD),A"
      case LDH_AC => "    LDH A,(C)"
      case LDH_CA => "    LDH (C),A"
      case LDH_DA => s"    LDH ($parameter),A"
      case LDH_AD => s"    LDH A,($parameter)"
      case LD_HLSP => "    LD HL,SP+" + parameter
      case ADD_SP => "    ADD SP," + parameter
      case EX_SP => registers match {
        case OneRegister(r) => s"    EX (SP),${asAssemblyString(r)}"
        case _ => ???
      }
      case JP | JR | DJNZ | CALL =>
        val ps = registers match {
          case NoRegisters => s" $parameter"
          case IfFlagSet(ZFlag.P) => s" PO,$parameter"
          case IfFlagClear(ZFlag.P) => s" PE,$parameter"
          case IfFlagSet(ZFlag.S) => s" M,$parameter"
          case IfFlagClear(ZFlag.S) => s" P,$parameter"
          case IfFlagSet(f) => s" $f,$parameter"
          case IfFlagClear(f) => s" N$f,$parameter"
          case OneRegister(r) => s" (${asAssemblyString(r)})"
        }
        s"    $opcode$ps"
      case op@(ADD | SBC | ADC) =>
        val os = op.toString
        val ps = (registers match {
          case OneRegister(r) => s" ${asAssemblyString(r)}"
          case OneRegisterOffset(r, o) => s" ${asAssemblyString(r, o)}"
        }).stripPrefix(" ")
        s"    $op A,$ps"
      case op if ZOpcodeClasses.BIT(op) =>
        val ps = registers match {
          case OneRegister(r) => s" ${asAssemblyString(r)}"
          case OneRegisterOffset(r, o) => s" ${asAssemblyString(r, o)}"
        }
        s"    BIT ${ZOpcodeClasses.BIT_seq.indexOf(op)},$ps"
      case op if ZOpcodeClasses.SET(op) =>
        val ps = registers match {
          case OneRegister(r) => s" ${asAssemblyString(r)}"
          case OneRegisterOffset(r, o) => s" ${asAssemblyString(r, o)}"
        }
        s"    SET ${ZOpcodeClasses.SET_seq.indexOf(op)},$ps"
      case op if ZOpcodeClasses.RES(op) =>
        val ps = registers match {
          case OneRegister(r) => s" ${asAssemblyString(r)}"
          case OneRegisterOffset(r, o) => s" ${asAssemblyString(r, o)}"
        }
        s"    RES ${ZOpcodeClasses.RES_seq.indexOf(op)},$ps"
      case op =>
        val os = op.toString.stripSuffix("_16")
        val ps = registers match {
          case NoRegisters => ""
          case IfFlagSet(ZFlag.P) => " PO"
          case IfFlagClear(ZFlag.P) => " PE"
          case IfFlagSet(ZFlag.S) => " M"
          case IfFlagClear(ZFlag.S) => " P"
          case IfFlagSet(f) => s" $f"
          case IfFlagClear(f) => s" N$f"
          case OneRegister(r) => s" ${asAssemblyString(r)}"
          case TwoRegisters(t, s) => s" ${asAssemblyString(t)},${asAssemblyString(s)}"
          case TwoRegistersOffset(t, s, o) => s" ${asAssemblyString(t, o)},${asAssemblyString(s, o)}"
          case OneRegisterOffset(r, o) => s" ${asAssemblyString(r, o)}"
        }
        s"    $os$ps"
    }
  }

  def toIntelString: String = {
    import ZOpcode._
    import ZRegister._
    val result = opcode match {
      case LABEL => parameter.toString + ":"
      case DISCARD_A => "    ; DISCARD_A"
      case DISCARD_HL => "    ; DISCARD_HL"
      case DISCARD_F => "    ; DISCARD_F"
      case DISCARD_BC => "    ; DISCARD_BC"
      case DISCARD_DE => "    ; DISCARD_DE"
      case DISCARD_IX => "    ; DISCARD_IX"
      case DISCARD_IY => "    ; DISCARD_IY"
      case BYTE => "    DB " + parameter.toString
      case LD => registers match {
        case TwoRegisters(target, IMM_8) => s"    MVI ${asIntelAssemblyString(target)}, ${parameter.toIntelString}"
        case TwoRegisters(A, MEM_ABS_8) => s"    LDA ${parameter.toIntelString}"
        case TwoRegisters(MEM_ABS_8, A) => s"    STA ${parameter.toIntelString}"
        case TwoRegisters(A, MEM_BC) => "    LDAX B"
        case TwoRegisters(MEM_BC, A) => "    STAX B"
        case TwoRegisters(A, MEM_DE) => "    LDAX D"
        case TwoRegisters(MEM_DE, A) => "    STAX D"
        case TwoRegisters(target, source) => s"    MOV ${asIntelAssemblyString(target)}, ${asIntelAssemblyString(source)}"
        case _ => "???"
      }
      case LD_16 => registers match {
        case TwoRegisters(SP, HL) => "    SPHL"
        case TwoRegisters(target, IMM_16) => s"    LXI ${asIntelAssemblyString(target)}, ${parameter.toIntelString}"
        case TwoRegisters(HL, MEM_ABS_16) => s"    LHLD ${parameter.toIntelString}"
        case TwoRegisters(MEM_ABS_16, HL) => s"    SHLD ${parameter.toIntelString}"
        case _ => "???"
      }
      case ADD_16 => registers match {
        case TwoRegisters(HL, source) => s"    DAD ${asIntelAssemblyString(source)}"
        case _ => "???"
      }
      case DEC_16 => registers match {
        case OneRegister(register) => s"    DCX ${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case INC_16 => registers match {
        case OneRegister(register) => s"    INX ${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case DEC => registers match {
        case OneRegister(register) => s"    DCR ${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case INC => registers match {
        case OneRegister(register) => s"    INR ${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case PUSH => registers match {
        case OneRegister(register) => s"    PUSH ${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case POP => registers match {
        case OneRegister(register) => s"    POP ${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case DAA => "    DAA"
      case RLA => "    RAL"
      case RLCA => "    RLC"
      case RRA => "    RAR"
      case RRCA => "    RRC"
      case HALT => "    HLT"
      case RET => registers match {
        case NoRegisters => "    RET" 
        case IfFlagClear(ZFlag.C) => "    RNC"
        case IfFlagClear(ZFlag.Z) => "    RNZ"
        case IfFlagClear(ZFlag.S) => "    RP"
        case IfFlagClear(ZFlag.P) => "    RPO"
        case IfFlagSet(ZFlag.C) => "    RC"
        case IfFlagSet(ZFlag.Z) => "    RZ" 
        case IfFlagSet(ZFlag.S) => "    RM" 
        case IfFlagSet(ZFlag.P) => "    RPE"
        case _ => "???"
      }
      case JP => registers match {
        case OneRegister(HL) => "    PCHL"
        case NoRegisters => s"    JMP ${parameter.toIntelString}"
        case IfFlagClear(ZFlag.C) => s"    JNC ${parameter.toIntelString}"
        case IfFlagClear(ZFlag.Z) => s"    JNZ ${parameter.toIntelString}"
        case IfFlagClear(ZFlag.S) => s"    JP ${parameter.toIntelString}"
        case IfFlagClear(ZFlag.P) => s"    JPO ${parameter.toIntelString}"
        case IfFlagSet(ZFlag.C) => s"    JC ${parameter.toIntelString}"
        case IfFlagSet(ZFlag.Z) => s"    JZ ${parameter.toIntelString}" 
        case IfFlagSet(ZFlag.S) => s"    JM ${parameter.toIntelString}" 
        case IfFlagSet(ZFlag.P) => s"    JPE ${parameter.toIntelString}"
        case _ => "???"
      }
      case CALL => registers match {
        case NoRegisters => s"    CALL ${parameter.toIntelString}"
        case IfFlagClear(ZFlag.C) => s"    CNC ${parameter.toIntelString}"
        case IfFlagClear(ZFlag.Z) => s"    CNZ ${parameter.toIntelString}"
        case IfFlagClear(ZFlag.S) => s"    CP ${parameter.toIntelString}"
        case IfFlagClear(ZFlag.P) => s"    CPO ${parameter.toIntelString}"
        case IfFlagSet(ZFlag.C) => s"    CC ${parameter.toIntelString}"
        case IfFlagSet(ZFlag.Z) => s"    CZ ${parameter.toIntelString}" 
        case IfFlagSet(ZFlag.S) => s"    CM ${parameter.toIntelString}" 
        case IfFlagSet(ZFlag.P) => s"    CPE ${parameter.toIntelString}"
        case _ => "???"
      }
      case ADD => registers match {
        case OneRegister(IMM_8) => s"    ADI ${parameter.toIntelString}"
        case OneRegister(register) => s"    ADD ${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case ADC => registers match {
        case OneRegister(IMM_8) => s"    ACI ${parameter.toIntelString}"
        case OneRegister(register) => s"    ADC ${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case SUB => registers match {
        case OneRegister(IMM_8) => s"    SUI ${parameter.toIntelString}"
        case OneRegister(register) => s"    SUB ${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case SBC => registers match {
        case OneRegister(IMM_8) => s"    SBI ${parameter.toIntelString}"
        case OneRegister(register) => s"    SBB ${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case AND => registers match {
        case OneRegister(IMM_8) => s"    ANI ${parameter.toIntelString}"
        case OneRegister(register) => s"    ANA ${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case OR => registers match {
        case OneRegister(IMM_8) => s"    ORI ${parameter.toIntelString}"
        case OneRegister(register) => s"    ORA ${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case XOR => registers match {
        case OneRegister(IMM_8) => s"    XRI ${parameter.toIntelString}"
        case OneRegister(register) => s"    XRA ${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case CP => registers match {
        case OneRegister(IMM_8) => s"    CPI ${parameter.toIntelString}"
        case OneRegister(register) => s"    CMP ${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case EX_SP => registers match {
        case OneRegister(HL) => s"    XTHL"
        case _ => "???"
      }
      case RST => parameter match {
        case NumericConstant(n, _) if n % 8 == 0 => s"    RST ${n / 8}"
        case _ => "???"
      }
      case IN_IMM => s"    IN ${parameter.toIntelString}"
      case OUT_IMM => s"    OUT ${parameter.toIntelString}"
      case EI => "    EI"
      case DI => "    EI"
      case EX_DE_HL => "    XCHG"
      case NOP => "    NOP"
      case CPL => "    CMA"
      case SCF => "    STC"
      case CCF => "    CMC"
      case EI => "    EI"
      case EI => "    EI"
      case EI => "    EI"
      case _ => "???"
    }
    if (result.contains("???")) s"    ??? (${this.toString.stripPrefix("    ")})" else result
  }

  def readsRegister(r: ZRegister.Value): Boolean = {
    import ZOpcode._
    import ZRegister._
    r match {
      case HL => readsRegister(L) || readsRegister(H)
      case BC => readsRegister(B) || readsRegister(C)
      case DE => readsRegister(D) || readsRegister(E)
      case IX => readsRegister(IXH) || readsRegister(IXL)
      case IY => readsRegister(IYH) || readsRegister(IYL)
      case AF => ???
      case MEM_ABS_8 | MEM_ABS_16 | IMM_8 | IMM_16 | MEM_DE | MEM_HL | MEM_BC | MEM_IX_D | MEM_IY_D | SP => ???
      case _ =>
        opcode match {
          case ADD_16 | ADC_16 | SBC_16 => registers match {
            case TwoRegisters(HL, HL) => r == H || r == L
            case TwoRegisters(HL, BC) => r == H || r == L || r == B || r == C
            case TwoRegisters(HL, DE) => r == H || r == L || r == D || r == E
            case TwoRegisters(HL, SP) => r == H || r == L || r == SP
            case TwoRegisters(IX, DE) => r == IXH || r == IXL || r == D || r == E
            case TwoRegisters(IX, BC) => r == IXH || r == IXL || r == B || r == C
            case TwoRegisters(IX, SP) => r == IXH || r == IXL || r == SP
            case _ => true
          }
          case LD => (registers match {
            case TwoRegisters(_, MEM_HL) => r == H || r == L
            case TwoRegisters(_, MEM_BC) => r == B || r == C
            case TwoRegisters(_, MEM_DE) => r == D || r == E
            case TwoRegisters(_, MEM_IX_D) => r == IXH || r == IXL
            case TwoRegisters(_, MEM_IY_D) => r == IYH || r == IYL
            case TwoRegisters(_, MEM_ABS_8 | MEM_ABS_16 | IMM_8 | IMM_16) => false
            case TwoRegisters(_, s) => r == s
            case TwoRegistersOffset(_, s, _) => r == s
            case _ => false
          }) || (registers match {
            case TwoRegisters(MEM_HL, _) => r == H || r == L
            case TwoRegisters(MEM_BC, _) => r == B || r == C
            case TwoRegisters(MEM_DE, _) => r == D || r == E
            case TwoRegisters(MEM_IX_D, _) => r == IXH || r == IXL
            case TwoRegisters(MEM_IY_D, _) => r == IYH || r == IYL
            case _ => false
          })
          case LD_16 => registers match {
            case TwoRegisters(_, MEM_ABS_8 | MEM_ABS_16 | IMM_8 | IMM_16) => false
            case TwoRegisters(_, MEM_HL | HL) => r == H || r == L
            case TwoRegisters(_, MEM_BC | BC) => r == B || r == C
            case TwoRegisters(_, MEM_DE | DE) => r == D || r == E
            case TwoRegisters(_, MEM_IX_D | IX) => r == IXH || r == IXL
            case TwoRegisters(_, MEM_IY_D | IY) => r == IYH || r == IYL
            case TwoRegisters(_, s) => r == s
            case TwoRegistersOffset(_, s, _) => r == s
            case _ => false
          }
          case AND | ADD | ADC | OR | XOR | CP | SUB | SBC => registers match {
            case OneRegister(MEM_HL) => r == H || r == L || r == A
            case OneRegister(MEM_BC) => r == B || r == C || r == A
            case OneRegister(MEM_DE) => r == D || r == E || r == A
            case OneRegister(MEM_IX_D) => r == IXH || r == IXL || r == A
            case OneRegister(MEM_IY_D) => r == IYH || r == IYL || r == A
            case OneRegister(IMM_8 | IMM_16) => r == A
            case OneRegister(s) => r == s || r == A
            case OneRegisterOffset(s, _) => r == s
            case _ => r == A
          }
          case INC | DEC | RL | RLC | RR | RRC | SLA | SLL | SRA | SRL | SWAP => registers match {
            case OneRegister(MEM_HL) => r == H || r == L
            case OneRegister(MEM_BC) => r == B || r == C
            case OneRegister(MEM_DE) => r == D || r == E
            case OneRegister(MEM_IX_D) => r == IXH || r == IXL
            case OneRegister(MEM_IY_D) => r == IYH || r == IYL
            case OneRegister(s) => r == s
            case OneRegisterOffset(s, _) => r == s
            case _ => false
          }
          case op if ZOpcodeClasses.AllSingleBit(op) => registers match {
            case OneRegister(MEM_HL) => r == H || r == L
            case OneRegister(MEM_IX_D) => r == IXH || r == IXL
            case OneRegister(MEM_IY_D) => r == IYH || r == IYL
            case OneRegister(s) => r == s
            case OneRegisterOffset(s, _) => r == s
            case _ => false
          }
          case INC_16 | DEC_16 | PUSH => registers match {
            case OneRegister(HL) => r == H || r == L
            case OneRegister(BC) => r == B || r == C
            case OneRegister(DE) => r == D || r == E
            case OneRegister(IX) => r == IXH || r == IXL
            case OneRegister(IY) => r == IYH || r == IYL
            case OneRegister(AF) => r == A
            case OneRegisterOffset(s, _) => r == s
            case _ => false
          }
          case EX_DE_HL => r == D || r == E || r == H || r == L
          case LDIR | LDDR => r == D || r == E || r == H || r == L || r == B || r == C
          case JP | JR | RET | RETI | RETN |
               POP |
               DISCARD_A | DISCARD_BC | DISCARD_DE | DISCARD_IX | DISCARD_IY | DISCARD_HL | DISCARD_F => false
          case DJNZ => r == B
          case DAA | NEG | CPL | RLA | RRA | RLCA | RRCA => r == A
          case LABEL | DI | EI | NOP | HALT => false
          case LDH_AC => r == C
          case LDH_CA => r == C || r == A
          case LDH_DA => r == A
          case LDH_AD => false
          case LD_HLIA | LD_HLDA => r == H || r == L | r == A
          case LD_AHLI | LD_AHLD => r == H || r == L
          case _ => true // TODO
        }
    }
  }

  def changesRegisterAndOffset(r: ZRegister.Value, o: Int): Boolean = {
    import ZOpcode._
    import ZRegister._
    r match {
      case MEM_IX_D | MEM_IY_D =>
        opcode match {
          case LD => registers match {
            case TwoRegistersOffset(s, _, p) => r == s && o == p
            case _ => false
          }
          case INC | DEC | RL | RLC | RR | RRC | SLA | SLL | SRA | SRL => registers match {
            case OneRegisterOffset(s, p) => r == s && o == p
            case _ => false
          }
          case op if ZOpcodeClasses.RES_or_SET(op) => registers match {
            case OneRegisterOffset(s, p) => r == s && o == p
            case _ => false
          }
          case POP | INC_16 | DEC_16 => registers match {
            case OneRegister(IX | IY) => true
            case _ => false
          }
          case LD_16 | ADD_16 | ADC_16 | SBC_16 => registers match {
            case TwoRegisters(IX | IY, _) => true
            case _ => false
          }
          case _ => false // TODO
        }
      case _ => changesRegister(r)
    }
  }

  def readsRegisterAndOffset(r: ZRegister.Value, o: Int): Boolean = {
    import ZOpcode._
    import ZRegister._
    r match {
      case MEM_IX_D | MEM_IY_D =>
        opcode match {
          case LD => registers match {
            case TwoRegistersOffset(_, s, p) => r == s && o == p
            case _ => false
          }
          case ADD | ADC | OR | XOR | AND | SUB | SBC | CP |
               INC | DEC | RL | RLC | RR | RRC | SLA | SLL | SRA | SRL => registers match {
            case OneRegisterOffset(s, p) => r == s && o == p
            case _ => false
          }
          case op if ZOpcodeClasses.AllSingleBit(op) => registers match {
            case OneRegisterOffset(s, p) => r == s && o == p
            case _ => false
          }
          case PUSH | INC_16 | DEC_16 => registers match {
            case OneRegister(IX | IY) => true
            case _ => false
          }
          case LD_16 => registers match {
            case TwoRegisters(_, IX | IY) => true
            case _ => false
          }
          case ADD_16 | ADC_16 | SBC_16 => registers match {
            case TwoRegisters(_, IX | IY) => true
            case TwoRegisters(IX | IY, _) => true
            case _ => false
          }
          case _ => false // TODO
        }
      case _ => readsRegister(r)
    }
  }

  def changesRegister(r: ZRegister.Value): Boolean = {
    import ZOpcode._
    import ZRegister._
    r match {
      case HL => changesRegister(L) || changesRegister(H)
      case BC => changesRegister(B) || changesRegister(C)
      case DE => changesRegister(D) || changesRegister(E)
      case IX => changesRegister(IXH) || changesRegister(IXL)
      case IY => changesRegister(IYH) || changesRegister(IYL)
      case AF => ???
      case IMM_8 | IMM_16 => false
      case MEM_ABS_8 | MEM_ABS_16 | MEM_DE | MEM_HL | MEM_BC | MEM_IX_D | MEM_IY_D | SP => ???
      case _ =>
        opcode match {
          case LD => registers match {
            case TwoRegisters(s, _) => r == s
            case TwoRegistersOffset(s, _, _) => r == s
            case _ => false
          }
          case LD_16 | ADD_16 | SBC_16 | ADC_16 => registers match {
            case TwoRegisters(HL, _) => r == H || r == L
            case TwoRegisters(BC, _) => r == B || r == C
            case TwoRegisters(DE, _) => r == D || r == E
            case TwoRegisters(IX, _) => r == IXH || r == IXL
            case TwoRegisters(IY, _) => r == IYH || r == IYL
            case TwoRegisters(s, _) => r == s
            case TwoRegistersOffset(s, _, _) => r == s
            case _ => false
          }
          case INC | DEC | RL | RLC | RR | RRC | SLA | SLL | SRA | SRL | SWAP => registers match {
            case OneRegister(s) => r == s
            case OneRegisterOffset(s, _) => r == s
            case _ => false
          }
          case op if ZOpcodeClasses.RES_or_SET(op) => registers match {
            case OneRegister(MEM_HL) => false
            case OneRegister(MEM_IX_D) => false
            case OneRegister(MEM_IY_D) => false
            case OneRegister(s) => r == s
            case OneRegisterOffset(s, _) => r == s
            case _ => false
          }
          case INC_16 | DEC_16 | POP => registers match {
            case OneRegister(HL) => r == H || r == L
            case OneRegister(BC) => r == B || r == C
            case OneRegister(DE) => r == D || r == E
            case OneRegister(IX) => r == IXH || r == IXL
            case OneRegister(IY) => r == IYH || r == IYL
            case OneRegister(AF) => r == A
            case OneRegisterOffset(s, _) => r == s
            case _ => false
          }
          case EX_DE_HL => r == D || r == E || r == H || r == L
          case LDIR | LDDR => r == D || r == E || r == H || r == L || r == B || r == C
          case JP | JR | RET | RETI | RETN |
               PUSH |
               DISCARD_A | DISCARD_BC | DISCARD_DE | DISCARD_IX | DISCARD_IY | DISCARD_HL | DISCARD_F => false
          case ADD | ADC | AND | OR | XOR | SUB | SBC | DAA | NEG | CPL | RLA | RRA | RLCA | RRCA => r == A
          case CP => false
          case DJNZ => r == B
          case LABEL | DI | EI | NOP | HALT => false
          case CALL => r != IXH && r != IXL && r != SP
          case LDH_CA | LDH_DA => false
          case LDH_AC | LDH_AD => r == A
          case LD_HLIA | LD_HLDA => r == H || r == L
          case LD_AHLI | LD_AHLD => r == H || r == L | r == A
          case _ => true // TODO
        }
    }
  }

  def readsMemory: Boolean = {
    import ZOpcode._
    import ZRegister._
    opcode match {
      case POP => true
      case LD | LD_16 => registers match {
        case TwoRegisters(_, MEM_IX_D | MEM_ABS_16 | MEM_ABS_8 | MEM_DE | MEM_BC | MEM_IY_D | MEM_HL) => true
        case _ => false
      }
      case ADC_16 | ADD_16 | SBC_16 => registers match {
        case TwoRegisters(_, MEM_IX_D | MEM_ABS_16 | MEM_ABS_8 | MEM_DE | MEM_BC | MEM_IY_D | MEM_HL) => true
        case TwoRegisters(MEM_IX_D | MEM_ABS_16 | MEM_ABS_8 | MEM_DE | MEM_BC | MEM_IY_D | MEM_HL, _) => true
        case _ => false
      }
      case ADD | ADC | OR | XOR | CP | SUB | SBC | INC | DEC | INC_16 | DEC_16 | RL | RLC | RR | RRC | SLA | SLL | SRA | SRL => registers match {
        case OneRegister(MEM_IX_D | MEM_ABS_16 | MEM_ABS_8 | MEM_DE | MEM_BC | MEM_IY_D | MEM_HL) => true
        case _ => false
      }
      case JP | JR | RET | RETI | RETN |
           PUSH | DJNZ | DAA |
           DISCARD_A | DISCARD_BC | DISCARD_DE | DISCARD_IX | DISCARD_IY | DISCARD_HL | DISCARD_F => false
      case EX_DE_HL | NEG => false
      case LABEL | DI | EI | NOP => false
      case LDH_AC | LDH_AD | LD_AHLI | LD_AHLD => false
      case LDH_CA | LDH_DA | LD_HLIA | LD_HLDA => true
      case _ => true // TODO
    }
  }

  def changesMemory: Boolean = {
    import ZOpcode._
    import ZRegister._
    opcode match {
      case POP => true
      case LD | LD_16 | ADC_16 | ADD_16 | SBC_16 => registers match {
        case TwoRegisters(MEM_IX_D | MEM_ABS_16 | MEM_ABS_8 | MEM_DE | MEM_BC | MEM_IY_D | MEM_HL, _) => true
        case _ => false
      }
      case INC | DEC | INC_16 | DEC_16 | RL | RLC | RR | RRC | SLA | SLL | SRA | SRL => registers match {
        case OneRegister(MEM_IX_D | MEM_ABS_16 | MEM_ABS_8 | MEM_DE | MEM_BC | MEM_IY_D | MEM_HL) => true
        case _ => false
      }
      case JP | JR | RET | RETI | RETN |
           PUSH | DJNZ | DAA |
           DISCARD_A | DISCARD_BC | DISCARD_DE | DISCARD_IX | DISCARD_IY | DISCARD_HL | DISCARD_F => false
      case EX_DE_HL | NEG => false
      case LABEL | DI | EI | NOP | HALT => false
      case LDH_AC | LDH_AD | LD_AHLI | LD_AHLD => true
      case LDH_CA | LDH_DA | LD_HLIA | LD_HLDA => false
      case _ => true // TODO
    }
  }
}