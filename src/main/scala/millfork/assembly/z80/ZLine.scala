package millfork.assembly.z80

import millfork.CompilationFlag
import millfork.assembly.{AbstractCode, Elidability, SourceLine}
import millfork.compiler.CompilationContext
import millfork.env.{Constant, Label, NumericConstant, ThingInMemory}
import millfork.node.{Position, ZRegister}

/**
  * @author Karol Stasiak
  */

object ZFlag extends Enumeration {
  val Z, P, C, S, H, N, V, K = Value

  val AllButSZ: Seq[Value] = Seq(P, C, H, N)
  val AllButZ: Seq[Value] = Seq(P, C, H, N, S)
  val All: Seq[Value] = Seq(P, C, H, N, S, Z)
}

sealed trait ZRegisters {
  def negate: ZRegisters = this
}

case object NoRegisters extends ZRegisters

case class IfFlagSet(flag: ZFlag.Value) extends ZRegisters {
  override def negate: ZRegisters = IfFlagClear(flag)
}

case class IfFlagClear(flag: ZFlag.Value) extends ZRegisters {
  override def negate: ZRegisters = IfFlagSet(flag)
}

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

  def elidability(source: ThingInMemory): Elidability.Value = {
    if (source.isVolatile) Elidability.Volatile else Elidability.Elidable
  }

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

  def ldAbs8(target: ZRegister.Value, source: ThingInMemory): ZLine =
    ZLine(LD, TwoRegisters(target, MEM_ABS_8), source.toAddress, elidability = elidability(source))

  def ldAbs16(target: ZRegister.Value, source: ThingInMemory): ZLine =
    ZLine(LD_16, TwoRegisters(target, MEM_ABS_16), source.toAddress, elidability = elidability(source))

  def ldAbs8(target: ZRegister.Value, source: ThingInMemory, offset: Int): ZLine =
    ZLine(LD, TwoRegisters(target, MEM_ABS_8), source.toAddress + offset, elidability = elidability(source))

  def ldAbs16(target: ZRegister.Value, source: ThingInMemory, offset: Int): ZLine =
    ZLine(LD_16, TwoRegisters(target, MEM_ABS_16), source.toAddress + offset, elidability = elidability(source))

  def ldAbs8(target: ZRegister.Value, source: Constant): ZLine = ZLine(LD, TwoRegisters(target, MEM_ABS_8), source)

  def ldAbs16(target: ZRegister.Value, source: Constant): ZLine = ZLine(LD_16, TwoRegisters(target, MEM_ABS_16), source)

  def ldAbs8(target: ZRegister.Value, source: Constant, elidability: Elidability.Value): ZLine =
    ZLine(LD, TwoRegisters(target, MEM_ABS_8), source, elidability = elidability)

  def ldAbs16(target: ZRegister.Value, source: Constant, elidability: Elidability.Value): ZLine =
    ZLine(LD_16, TwoRegisters(target, MEM_ABS_16), source, elidability = elidability)

  def ldAbs8(target: ThingInMemory, source: ZRegister.Value): ZLine =
    ZLine(LD, TwoRegisters(MEM_ABS_8, source), target.toAddress, elidability = elidability(target))

  def ldAbs16(target: ThingInMemory, source: ZRegister.Value): ZLine =
    ZLine(LD_16, TwoRegisters(MEM_ABS_16, source), target.toAddress, elidability = elidability(target))

  def ldAbs8(target: ThingInMemory, source: ZRegister.Value, offset: Int): ZLine =
    ZLine(LD, TwoRegisters(MEM_ABS_8, source), target.toAddress + offset, elidability = elidability(target))

  def ldAbs16(target: ThingInMemory, source: ZRegister.Value, offset: Int): ZLine =
    ZLine(LD_16, TwoRegisters(MEM_ABS_16, source), target.toAddress + offset, elidability = elidability(target))

  def ldAbs8(target: Constant, source: ZRegister.Value): ZLine = ZLine(LD, TwoRegisters(MEM_ABS_8, source), target)

  def ldAbs16(target: Constant, source: ZRegister.Value): ZLine = ZLine(LD_16, TwoRegisters(MEM_ABS_16, source), target)

  def ldAbs8(target: Constant, source: ZRegister.Value, elidability: Elidability.Value): ZLine =
    ZLine(LD, TwoRegisters(MEM_ABS_8, source), target, elidability = elidability)

  def ldAbs16(target: Constant, source: ZRegister.Value, elidability: Elidability.Value): ZLine =
    ZLine(LD_16, TwoRegisters(MEM_ABS_16, source), target, elidability = elidability)

  def ldViaIx(target: ZRegister.Value, sourceOffset: Int): ZLine = ZLine(LD, TwoRegistersOffset(target, ZRegister.MEM_IX_D, sourceOffset), Constant.Zero)

  def ldViaIx(targetOffset: Int, source: ZRegister.Value): ZLine = ZLine(LD, TwoRegistersOffset(ZRegister.MEM_IX_D, source, targetOffset), Constant.Zero)

  def ld0ViaIx(targetOffset: Int): ZLine = ZLine(LD, TwoRegistersOffset(ZRegister.MEM_IX_D, ZRegister.IMM_8, targetOffset), Constant.Zero)

  def ldViaIy(target: ZRegister.Value, sourceOffset: Int): ZLine = ZLine(LD, TwoRegistersOffset(target, ZRegister.MEM_IY_D, sourceOffset), Constant.Zero)

  def ldViaIy(targetOffset: Int, source: ZRegister.Value): ZLine = ZLine(LD, TwoRegistersOffset(ZRegister.MEM_IY_D, source, targetOffset), Constant.Zero)

  def ld0ViaIy(targetOffset: Int): ZLine = ZLine(LD, TwoRegistersOffset(ZRegister.MEM_IY_D, ZRegister.IMM_8, targetOffset), Constant.Zero)

  def ldViaIxy(x: Boolean, target: ZRegister.Value, sourceOffset: Int): ZLine = if (x) ldViaIx(target, sourceOffset) else ldViaIy(target, sourceOffset)

  def ldViaIxy(x: Boolean, targetOffset: Int, source: ZRegister.Value): ZLine = if (x) ldViaIx(targetOffset, source) else ldViaIy(targetOffset, source)
}

object ZLine0 {
  @inline
  def unapply(a: ZLine): Some[(ZOpcode.Value, ZRegisters, Constant)] = Some(a.opcode, a.registers, a.parameter)
}

case class ZLine(opcode: ZOpcode.Value, registers: ZRegisters, parameter: Constant, elidability: Elidability.Value = Elidability.Elidable, source: Option[SourceLine] = None) extends AbstractCode {

  def pos(s: Option[SourceLine]): ZLine = if (s.isEmpty || s == source) this else this.copy(source = s)

  def pos(s1: Option[SourceLine], s2: Option[SourceLine]): ZLine = pos(Seq(s1, s2))

  def position(s: Option[Position]): ZLine = pos(SourceLine.of(s))

  def positionIfEmpty(s: Option[Position]): ZLine = if (s.isEmpty || source.isDefined) this else pos(SourceLine.of(s))

  def pos(s: Seq[Option[SourceLine]]): ZLine = pos(SourceLine.merge(s))

  def mergePos(s: Seq[Option[SourceLine]]): ZLine = if (s.isEmpty) this else pos(SourceLine.merge(this.source, s))

  @inline
  def elidable: Boolean = elidability == Elidability.Elidable

  @inline
  def notFixed: Boolean = elidability != Elidability.Fixed

  def refersTo(name: String): Boolean = parameter.refersTo(name)

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

  private def asAssemblyString(r: ZRegister.Value, offset: Int = 666): String = r match {
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
    case ZRegister.IX => "IX"
    case ZRegister.IY => "IY"
    case _ => "???"
  }

  private def asIntel8086AssemblyString(r: ZRegister.Value): String = r match {
    case ZRegister.A => "AL"
    case ZRegister.B => "CH"
    case ZRegister.C => "CL"
    case ZRegister.D => "DH"
    case ZRegister.E => "DL"
    case ZRegister.H => "BH"
    case ZRegister.L => "BL"
    case ZRegister.AF => "AX"
    case ZRegister.BC => "CX"
    case ZRegister.DE => "DX"
    case ZRegister.HL => "BX"
    case ZRegister.SP => "SP"
    case ZRegister.IX => "BP"
    case ZRegister.MEM_ABS_8 => s"BYTE PTR [${parameter.toIntelString}]"
    case ZRegister.MEM_ABS_16 => s"WORD PTR [${parameter.toIntelString}]"
    case ZRegister.IMM_8 => parameter.toIntelString
    case ZRegister.IMM_16 => parameter.toIntelString
    case ZRegister.MEM_HL => "BYTE PTR [BX]"
    case _ => "???"
  }

  override def toString: String = {
    import ZOpcode._
    val raw = opcode match {
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
      case IN_IMM => s"    IN A,($parameter)"
      case IN_C => s"    IN A,(C)"
      case OUT_IMM => s"    OUT ($parameter),A"
      case OUT_C => s"    OUT (C),a"
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

      case DSUB => "    DSUB"
      case RRHL => "    SRA HL"
      case RLDE => "    RL DE"
      case LD_DEHL => s"    LD DE,HL+${parameter.toString}"
      case LD_DESP => s"    LD DE,SP+${parameter.toString}"
      case RSTV => "    RSTV"
      case LHLX => "    LD HL,(DE)"
      case SHLX => "    LD (DE),HL"

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
    source match {
      case Some(SourceLine(_, line)) if line > 0 => f"$raw%-30s \t; @ $line%d"
      case _ => raw
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
        case TwoRegistersOffset(MEM_IX_D, IMM_8, offset) => s"    MVIX ${parameter.toIntelString}, ${offset}"
        case TwoRegistersOffset(MEM_IY_D, IMM_8, offset) => s"    MVIY ${parameter.toIntelString}, ${offset}"
        case TwoRegistersOffset(MEM_IX_D, source, offset) => s"    STX ${asIntelAssemblyString(source)}, ${offset}"
        case TwoRegistersOffset(MEM_IY_D, source, offset) => s"    STY ${asIntelAssemblyString(source)}, ${offset}"
        case TwoRegistersOffset(target, MEM_IX_D, offset) => s"    LDX ${asIntelAssemblyString(target)}, ${offset}"
        case TwoRegistersOffset(target, MEM_IY_D, offset) => s"    LDY ${asIntelAssemblyString(target)}, ${offset}"
        case TwoRegisters(target, IMM_8) => s"    MVI ${asIntelAssemblyString(target)}, ${parameter.toIntelString}"
        case TwoRegisters(A, MEM_ABS_8) => s"    LDA ${parameter.toIntelString}"
        case TwoRegisters(MEM_ABS_8, A) => s"    STA ${parameter.toIntelString}"
        case TwoRegisters(A, MEM_BC) => "    LDAX B"
        case TwoRegisters(MEM_BC, A) => "    STAX B"
        case TwoRegisters(A, MEM_DE) => "    LDAX D"
        case TwoRegisters(MEM_DE, A) => "    STAX D"
        case TwoRegisters(I, A) => "    STAI"
        case TwoRegisters(A, I) => "    LDAI"
        case TwoRegisters(R, A) => "    STAR"
        case TwoRegisters(A, R) => "    LDAR"
        case TwoRegisters(target, source) => s"    MOV ${asIntelAssemblyString(target)}, ${asIntelAssemblyString(source)}"
        case _ => "???"
      }
      case LD_16 => registers match {
        case TwoRegisters(SP, HL) => "    SPHL"
        case TwoRegisters(SP, IX) => "    SPIX"
        case TwoRegisters(SP, IY) => "    SPIY"
        case TwoRegisters(IX, IMM_16) => s"    LXIX ${parameter.toIntelString}"
        case TwoRegisters(IY, IMM_16) => s"    LXIY ${parameter.toIntelString}"
        case TwoRegisters(target, IMM_16) => s"    LXI ${asIntelAssemblyString(target)}, ${parameter.toIntelString}"
        case TwoRegisters(HL, MEM_ABS_16) => s"    LHLD ${parameter.toIntelString}"
        case TwoRegisters(BC, MEM_ABS_16) => s"    LBCD ${parameter.toIntelString}"
        case TwoRegisters(DE, MEM_ABS_16) => s"    LDED ${parameter.toIntelString}"
        case TwoRegisters(IX, MEM_ABS_16) => s"    LIXD ${parameter.toIntelString}"
        case TwoRegisters(IY, MEM_ABS_16) => s"    LIYD ${parameter.toIntelString}"
        case TwoRegisters(SP, MEM_ABS_16) => s"    LSPD ${parameter.toIntelString}"
        case TwoRegisters(MEM_ABS_16, HL) => s"    SHLD ${parameter.toIntelString}"
        case TwoRegisters(MEM_ABS_16, BC) => s"    SBCD ${parameter.toIntelString}"
        case TwoRegisters(MEM_ABS_16, DE) => s"    SDED ${parameter.toIntelString}"
        case TwoRegisters(MEM_ABS_16, IX) => s"    SIXD ${parameter.toIntelString}"
        case TwoRegisters(MEM_ABS_16, IY) => s"    SIYD ${parameter.toIntelString}"
        case TwoRegisters(MEM_ABS_16, SP) => s"    SSPD ${parameter.toIntelString}"
        case _ => "???"
      }
      case ADD_16 => registers match {
        case TwoRegisters(IX, source) => s"    DADX ${asIntelAssemblyString(source)}"
        case TwoRegisters(IY, source) => s"    DADY ${asIntelAssemblyString(source)}"
        case TwoRegisters(HL, source) => s"    DAD ${asIntelAssemblyString(source)}"
        case _ => "???"
      }
      case ADC_16 => registers match {
        case TwoRegisters(HL, source) => s"    DADC ${asIntelAssemblyString(source)}"
        case _ => "???"
      }
      case SBC_16 => registers match {
        case TwoRegisters(HL, source) => s"    DSBC ${asIntelAssemblyString(source)}"
        case _ => "???"
      }
      case DEC_16 => registers match {
        case OneRegister(IX) => s"    DCXIX"
        case OneRegister(IY) => s"    DCXIY"
        case OneRegister(register) => s"    DCX ${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case INC_16 => registers match {
        case OneRegister(IX) => s"    INXIX"
        case OneRegister(IY) => s"    INXIY"
        case OneRegister(register) => s"    INX ${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case DEC => registers match {
        case OneRegisterOffset(MEM_IX_D, offset) => s"    DCRX ${offset}"
        case OneRegisterOffset(MEM_IY_D, offset) => s"    DCRY ${offset}"
        case OneRegister(register) => s"    DCR ${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case INC => registers match {
        case OneRegisterOffset(MEM_IX_D, offset) => s"    INRX ${offset}"
        case OneRegisterOffset(MEM_IY_D, offset) => s"    INRY ${offset}"
        case OneRegister(register) => s"    INR ${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case op if ZOpcodeClasses.RES(op) => registers match {
        case OneRegisterOffset(MEM_IX_D, offset) => s"    RESX ${ZOpcodeClasses.RES_seq.indexOf(op)},${offset}"
        case OneRegisterOffset(MEM_IY_D, offset) => s"    RESY ${ZOpcodeClasses.RES_seq.indexOf(op)},${offset}"
        case OneRegister(register) => s"    RES ${ZOpcodeClasses.RES_seq.indexOf(op)},${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case op if ZOpcodeClasses.SET(op) => registers match {
        case OneRegisterOffset(MEM_IX_D, offset) => s"    SETX ${ZOpcodeClasses.SET_seq.indexOf(op)},${offset}"
        case OneRegisterOffset(MEM_IY_D, offset) => s"    SETY ${ZOpcodeClasses.SET_seq.indexOf(op)},${offset}"
        case OneRegister(register) => s"    SETB ${ZOpcodeClasses.SET_seq.indexOf(op)},${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case op if ZOpcodeClasses.BIT(op) => registers match {
        case OneRegisterOffset(MEM_IX_D, offset) => s"    BITX ${ZOpcodeClasses.BIT_seq.indexOf(op)},${offset}"
        case OneRegisterOffset(MEM_IY_D, offset) => s"    BITY ${ZOpcodeClasses.BIT_seq.indexOf(op)},${offset}"
        case OneRegister(register) => s"    BIT ${ZOpcodeClasses.BIT_seq.indexOf(op)},${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case PUSH => registers match {
        case OneRegister(IX) => s"    PUSHIX"
        case OneRegister(IY) => s"    PUSHIY"
        case OneRegister(register) => s"    PUSH ${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case POP => registers match {
        case OneRegister(IX) => s"    POPIX"
        case OneRegister(IY) => s"    POPIY"
        case OneRegister(register) => s"    POP ${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case IM => parameter match {
        case NumericConstant(0, _) => s"    IM0"
        case NumericConstant(1, _) => s"    IM1"
        case NumericConstant(2, _) => s"    IM2"
        case _ => "???"
      }
      case RL => registers match {
        case OneRegister(register) => s"    RALR ${asIntelAssemblyString(register)}"
        case OneRegisterOffset(MEM_IX_D, offset) => s"    RALX $offset"
        case OneRegisterOffset(MEM_IY_D, offset) => s"    RALY $offset"
        case _ => "???"
      }
      case RLC => registers match {
        case OneRegister(register) => s"    RLCR ${asIntelAssemblyString(register)}"
        case OneRegisterOffset(MEM_IX_D, offset) => s"    RLCX $offset"
        case OneRegisterOffset(MEM_IY_D, offset) => s"    RLCY $offset"
        case _ => "???"
      }
      case RR => registers match {
        case OneRegister(register) => s"    RARR ${asIntelAssemblyString(register)}"
        case OneRegisterOffset(MEM_IX_D, offset) => s"    RARX $offset"
        case OneRegisterOffset(MEM_IY_D, offset) => s"    RARY $offset"
        case _ => "???"
      }
      case RRC => registers match {
        case OneRegister(register) => s"    RRCR ${asIntelAssemblyString(register)}"
        case OneRegisterOffset(MEM_IX_D, offset) => s"    RRCX $offset"
        case OneRegisterOffset(MEM_IY_D, offset) => s"    RRCY $offset"
        case _ => "???"
      }
      case SLA => registers match {
        case OneRegister(register) => s"    SLAR ${asIntelAssemblyString(register)}"
        case OneRegisterOffset(MEM_IX_D, offset) => s"    SLAX $offset"
        case OneRegisterOffset(MEM_IY_D, offset) => s"    SLAY $offset"
        case _ => "???"
      }
      case SLL => registers match {
        case OneRegister(register) => s"    SLLR ${asIntelAssemblyString(register)}"
        case OneRegisterOffset(MEM_IX_D, offset) => s"    SLLX $offset"
        case OneRegisterOffset(MEM_IY_D, offset) => s"    SLLY $offset"
        case _ => "???"
      }
      case SRA => registers match {
        case OneRegister(register) => s"    SRAR ${asIntelAssemblyString(register)}"
        case OneRegisterOffset(MEM_IX_D, offset) => s"    SRAX $offset"
        case OneRegisterOffset(MEM_IY_D, offset) => s"    SRAY $offset"
        case _ => "???"
      }
      case SRL => registers match {
        case OneRegister(register) => s"    SRLR ${asIntelAssemblyString(register)}"
        case OneRegisterOffset(MEM_IX_D, offset) => s"    SRLX $offset"
        case OneRegisterOffset(MEM_IY_D, offset) => s"    SRLY $offset"
        case _ => "???"
      }
      case DAA => "    DAA"
      case RLA => "    RAL"
      case RLCA => "    RLC"
      case RRA => "    RAR"
      case RRCA => "    RRC"
      case HALT => "    HLT"
      case LDI|LDIR|LDD|LDDR|INI|INIR|IND|INDR|OUTI|OUTD|EXX|NEG|RLD|RRD|RETN|RETI => "    " + opcode
      case OUTIR => "    OUTIR"
      case OUTDR => "    OUTDR"
      case CPI => "    CCI"
      case CPIR => "    CCIR"
      case CPD => "    CCD"
      case CPDR => "    CCDR"
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
        case OneRegister(IX) => "    PCIX"
        case OneRegister(IY) => "    PCIY"
        case NoRegisters => s"    JMP ${parameter.toIntelString}"
        case IfFlagClear(ZFlag.C) => s"    JNC ${parameter.toIntelString}"
        case IfFlagClear(ZFlag.Z) => s"    JNZ ${parameter.toIntelString}"
        case IfFlagClear(ZFlag.S) => s"    JP ${parameter.toIntelString}"
        case IfFlagClear(ZFlag.P) => s"    JPO ${parameter.toIntelString}"
        case IfFlagClear(ZFlag.K) => s"    JNK ${parameter.toIntelString}"
        case IfFlagSet(ZFlag.C) => s"    JC ${parameter.toIntelString}"
        case IfFlagSet(ZFlag.Z) => s"    JZ ${parameter.toIntelString}" 
        case IfFlagSet(ZFlag.S) => s"    JM ${parameter.toIntelString}" 
        case IfFlagSet(ZFlag.P) => s"    JPE ${parameter.toIntelString}"
        case IfFlagSet(ZFlag.K) => s"    JK ${parameter.toIntelString}"
        case _ => "???"
      }
      case JR => registers match {
        case NoRegisters => s"    JR ${parameter.toIntelString}"
        case IfFlagClear(ZFlag.C) => s"    JRNC ${parameter.toIntelString}"
        case IfFlagClear(ZFlag.Z) => s"    JRNZ ${parameter.toIntelString}"
        case IfFlagSet(ZFlag.C) => s"    JRC ${parameter.toIntelString}"
        case IfFlagSet(ZFlag.Z) => s"    JRZ ${parameter.toIntelString}"
        case _ => "???"
      }
      case DJNZ => s"    DJNZ ${parameter.toIntelString}"
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
        case OneRegisterOffset(MEM_IX_D, offset) => s"    ADDX ${offset}"
        case OneRegisterOffset(MEM_IY_D, offset) => s"    ADDY ${offset}"
        case OneRegister(register) => s"    ADD ${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case ADC => registers match {
        case OneRegister(IMM_8) => s"    ACI ${parameter.toIntelString}"
        case OneRegisterOffset(MEM_IX_D, offset) => s"    ADCX ${offset}"
        case OneRegisterOffset(MEM_IY_D, offset) => s"    ADCY ${offset}"
        case OneRegister(register) => s"    ADC ${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case SUB => registers match {
        case OneRegister(IMM_8) => s"    SUI ${parameter.toIntelString}"
        case OneRegisterOffset(MEM_IX_D, offset) => s"    SUBX ${offset}"
        case OneRegisterOffset(MEM_IY_D, offset) => s"    SUBY ${offset}"
        case OneRegister(register) => s"    SUB ${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case SBC => registers match {
        case OneRegister(IMM_8) => s"    SBI ${parameter.toIntelString}"
        case OneRegisterOffset(MEM_IX_D, offset) => s"    SBCX ${offset}"
        case OneRegisterOffset(MEM_IY_D, offset) => s"    SBCY ${offset}"
        case OneRegister(register) => s"    SBB ${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case AND => registers match {
        case OneRegister(IMM_8) => s"    ANI ${parameter.toIntelString}"
        case OneRegisterOffset(MEM_IX_D, offset) => s"    ANDX ${offset}"
        case OneRegisterOffset(MEM_IY_D, offset) => s"    ANDY ${offset}"
        case OneRegister(register) => s"    ANA ${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case OR => registers match {
        case OneRegister(IMM_8) => s"    ORI ${parameter.toIntelString}"
        case OneRegisterOffset(MEM_IX_D, offset) => s"    ORX ${offset}"
        case OneRegisterOffset(MEM_IY_D, offset) => s"    ORY ${offset}"
        case OneRegister(register) => s"    ORA ${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case XOR => registers match {
        case OneRegister(IMM_8) => s"    XRI ${parameter.toIntelString}"
        case OneRegisterOffset(MEM_IX_D, offset) => s"    XORX ${offset}"
        case OneRegisterOffset(MEM_IY_D, offset) => s"    XORY ${offset}"
        case OneRegister(register) => s"    XRA ${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case CP => registers match {
        case OneRegister(IMM_8) => s"    CPI ${parameter.toIntelString}"
        case OneRegisterOffset(MEM_IX_D, offset) => s"    CMPX ${offset}"
        case OneRegisterOffset(MEM_IY_D, offset) => s"    CMPY ${offset}"
        case OneRegister(register) => s"    CMP ${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case EX_SP => registers match {
        case OneRegister(HL) => s"    XTHL"
        case OneRegister(IX) => s"    XTIX"
        case OneRegister(IY) => s"    XTIY"
        case _ => "???"
      }
      case RST => parameter match {
        case NumericConstant(n, _) if n % 8 == 0 => s"    RST ${n / 8}"
        case _ => "???"
      }
      case IN_C => registers match {
        case OneRegister(register) => s"    INP ${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case OUT_C => registers match {
        case OneRegister(register) => s"    OUTP ${asIntelAssemblyString(register)}"
        case _ => "???"
      }
      case IN_IMM => s"    IN ${parameter.toIntelString}"
      case OUT_IMM => s"    OUT ${parameter.toIntelString}"
      case EI => "    EI"
      case DI => "    DI"
      case EX_DE_HL => "    XCHG"
      case NOP => "    NOP"
      case CPL => "    CMA"
      case SCF => "    STC"
      case CCF => "    CMC"
      case EI => "    EI"

      case EX_AF_AF => "    EXAF"

      case DSUB => "    DSUB"
      case RRHL => "    ARHL"
      case RLDE => "    RLDE"
      case LD_DEHL => s"    LDHI ${parameter.toIntelString}"
      case LD_DESP => s"    LDSI ${parameter.toIntelString}"
      case RSTV => "    RSTV"
      case LHLX => "    LHLX"
      case SHLX => "    SHLX"
      case _ => "???"
    }
    if (result.contains("???")) s"    ??? (${this.toString.stripPrefix("    ")})" else result
  }



  def toIntel8086String: String = {
    import ZOpcode._
    import ZRegister._
    val result = opcode match {
      case LABEL => parameter.toString + ":"
      case DISCARD_A => "    ; DISCARD_AL"
      case DISCARD_HL => "    ; DISCARD_BX"
      case DISCARD_F => "    ; DISCARD_F"
      case DISCARD_BC => "    ; DISCARD_CX"
      case DISCARD_DE => "    ; DISCARD_DX"
      case DISCARD_IX => "    ; DISCARD_BP"
      case BYTE => "    DB " + parameter.toString
      case LD | LD_16 => registers match {
        case TwoRegisters(MEM_DE, A) => s"    MOV SI, DX\n    MOV BYTE PTR [SI], AL"
        case TwoRegisters(MEM_BC, A) => s"    MOV SI, CX\n    MOV BYTE PTR [SI], AL"
        case TwoRegisters(A, MEM_DE) => s"    MOV SI, DX\n    MOV AL, BYTE PTR [SI]"
        case TwoRegisters(A, MEM_BC) => s"    MOV SI, CX\n    MOV AL, BYTE PTR [SI]"
        case TwoRegistersOffset(MEM_IX_D, IMM_8, offset) => s"    MOV BYTE PTR [BP+$offset}], ${parameter.toIntelString}"
        case TwoRegisters(target, IMM_16 | IMM_8) => s"    MOV ${asIntel8086AssemblyString(target)}, ${parameter.toIntelString}"
        case TwoRegistersOffset(MEM_IX_D, source, offset) => s"    MOV BYTE PTR [BP+$offset}], ${asIntel8086AssemblyString(source)}"
        case TwoRegistersOffset(target, MEM_IX_D, offset) => s"    MOV ${asIntel8086AssemblyString(target)}, BYTE PTR [BP+$offset}]"
        case TwoRegisters(target, source) => s"    MOV ${asIntel8086AssemblyString(target)}, ${asIntel8086AssemblyString(source)}"
        case _ => "???"
      }
      case ADD_16 => registers match {
        case TwoRegisters(target, source) => s"    ADD ${asIntel8086AssemblyString(target)}, ${asIntel8086AssemblyString(source)}"
        case _ => "???"
      }
      case DEC|DEC_16 => registers match {
        case OneRegister(register) => s"    DEC ${asIntel8086AssemblyString(register)}"
        case _ => "???"
      }
      case INC|INC_16 => registers match {
        case OneRegister(register) => s"    INC ${asIntel8086AssemblyString(register)}"
        case _ => "???"
      }
      case PUSH => registers match {
        case OneRegister(AF) => s"    LAHF\n    XCHG AH,AL\n    PUSH AX\n    XCHG AH, AL"
        case OneRegister(register) => s"    PUSH ${asIntel8086AssemblyString(register)}"
        case _ => "???"
      }
      case POP => registers match {
        case OneRegister(AF) => s"    POP AX\n    XCHG AH, AL\n    SAHF"
        case OneRegister(register) => s"    POP ${asIntel8086AssemblyString(register)}"
        case _ => "???"
      }
      case DAA => "    DAA"
      case RLA => "    RCL AL"
      case RLCA => "    ROL AL"
      case RRA => "    RCL AL"
      case RRCA => "    ROR AL"
      case HALT => "    HLT"
      case RET | JP | CALL =>
        val (prefix, suffix) = registers match {
          case NoRegisters => "    " -> ""
          case OneRegister(_) => "    " -> ""
          case IfFlagClear(ZFlag.C) => "    JB .next\n    " -> "\n.next:"
          case IfFlagClear(ZFlag.Z) => "    JE .next\n    " -> "\n.next:"
          case IfFlagClear(ZFlag.S) => "    JS .next\n    " -> "\n.next:"
          case IfFlagClear(ZFlag.P) => "    JPE .next\n    " -> "\n.next:"
          case IfFlagClear(ZFlag.K) => "    JL .next\n    " -> "\n.next:"
          case IfFlagSet(ZFlag.C) => "    JAE .next\n    " -> "\n.next:"
          case IfFlagSet(ZFlag.Z) => "    JNE .next\n    " -> "\n.next:"
          case IfFlagSet(ZFlag.S) => "    JNS .next\n    " -> "\n.next:"
          case IfFlagSet(ZFlag.P) => "    JPO .next\n    " -> "\n.next:"
          case IfFlagSet(ZFlag.K) => "    JGE .next\n    " -> "\n.next:"
          case _ => "    ???" -> ""
        }
        prefix + (opcode match {
          case RET => "RET"
          case JP => registers match {
            case OneRegister(HL) => "JMP BX"
            case _ => "JMP " + parameter.toIntelString
          }
          case CALL => "CALL " + parameter.toIntelString
        }) + suffix
      case ADD => registers match {
        case OneRegister(IMM_8) => s"    ADD AL, ${parameter.toIntelString}"
        case OneRegister(register) => s"    ADD AL, ${asIntel8086AssemblyString(register)}"
        case OneRegisterOffset(MEM_IX_D, offset) => s"    ADD AL, BYTE PTR[BP+$offset]"
        case _ => "???"
      }
      case ADC => registers match {
        case OneRegister(IMM_8) => s"    ADC AL, ${parameter.toIntelString}"
        case OneRegister(register) => s"    ADC AL, ${asIntel8086AssemblyString(register)}"
        case OneRegisterOffset(MEM_IX_D, offset) => s"    ADC AL, BYTE PTR[BP+$offset]"
        case _ => "???"
      }
      case SUB => registers match {
        case OneRegister(IMM_8) => s"    SUB AL, ${parameter.toIntelString}"
        case OneRegister(register) => s"    SUB AL, ${asIntel8086AssemblyString(register)}"
        case OneRegisterOffset(MEM_IX_D, offset) => s"    SUB AL, BYTE PTR[BP+$offset]"
        case _ => "???"
      }
      case SBC => registers match {
        case OneRegister(IMM_8) => s"    SBB AL, ${parameter.toIntelString}"
        case OneRegister(register) => s"    SBB AL, ${asIntel8086AssemblyString(register)}"
        case OneRegisterOffset(MEM_IX_D, offset) => s"    SBB AL, BYTE PTR[BP+$offset]"
        case _ => "???"
      }
      case AND => registers match {
        case OneRegister(IMM_8) => s"    AND AL, ${parameter.toIntelString}"
        case OneRegister(register) => s"    AND AL, ${asIntel8086AssemblyString(register)}"
        case OneRegisterOffset(MEM_IX_D, offset) => s"    AND AL, BYTE PTR[BP+$offset]"
        case _ => "???"
      }
      case OR => registers match {
        case OneRegister(IMM_8) => s"    OR AL, ${parameter.toIntelString}"
        case OneRegister(register) => s"    OR AL, ${asIntel8086AssemblyString(register)}"
        case OneRegisterOffset(MEM_IX_D, offset) => s"    OR AL, BYTE PTR[BP+$offset]"
        case _ => "???"
      }
      case XOR => registers match {
        case OneRegister(IMM_8) => s"    XOR AL, ${parameter.toIntelString}"
        case OneRegister(register) => s"    XOR AL, ${asIntel8086AssemblyString(register)}"
        case OneRegisterOffset(MEM_IX_D, offset) => s"    XOR AL, BYTE PTR[BP+$offset]"
        case _ => "???"
      }
      case CP => registers match {
        case OneRegister(IMM_8) => s"    CMP AL, ${parameter.toIntelString}"
        case OneRegister(register) => s"    CMP AL, ${asIntel8086AssemblyString(register)}"
        case OneRegisterOffset(MEM_IX_D, offset) => s"    CMP AL, BYTE PTR[BP+$offset]"
        case _ => "???"
      }
      case EX_SP => registers match {
        case OneRegister(HL) => s"    XCHG BX, [SP]"
        case OneRegister(IX) => s"    XCHG BP, [SP]"
        case _ => "???"
      }
      case RST => parameter match {
        case NumericConstant(n, _) if n % 8 == 0 => s"    RST ${n / 8}" // TODO
        case _ => "???"
      }
      case IN_IMM => s"    IN ${parameter.toIntelString}" // TODO
      case OUT_IMM => s"    OUT ${parameter.toIntelString}" // TODO
      case EI => "    STI"
      case DI => "    CLI"
      case EX_DE_HL => "    XCHG BX, DX"
      case NOP => "    NOP"
      case CPL => "    NOT AL"
      case SCF => "    STC"
      case CCF => "    CMC"

      case LD_HLSP => "    LEA BX,[BX+${parameter.toIntelString}]"

      case DSUB => "    SUB BX, CX" // TODO: ???
      case LD_DESP => s"    LEA DX, [SP+${parameter.toIntelString}]"
      case LD_DEHL => s"    LEA DX, [BX+${parameter.toIntelString}]"
      case RSTV => "    JNO .next\n    RST 8\n.next" // TODO
      case LHLX => "    MOV BX, WORD PTR [DX]"
      case SHLX => "    MOV WORD PTR [DX], BX"
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
          case LD_HLSP => r == SP

          case LD_DEHL => r == H || r == L
          case LD_DESP => r == SP
          case DSUB => r == B || r == C || r == H || r == L
          case SHLX => r == D || r == E || r == H || r == L
          case LHLX | RLDE => r == D || r == E
          case RRHL => r == H || r == L

          case MULUB => r == A || (registers match {
            case TwoRegisters(p, q) => r == q || r == p
            case _ => true
          })
          case MULUW => r == H || r == L || (registers match {
            case TwoRegisters(_, BC) => r == B || r == C
            case TwoRegisters(_, DE) => r == D || r == E
            case TwoRegisters(_, SP) => r == SP
            case _ => true
          })

          case _ => true // TODO
        }
    }
  }

  def accessesMemoryViaGivenRegister(r: ZRegister.Value): Boolean = {
    import ZRegister._
    import ZOpcode._
    registers match {
      case TwoRegisters(MEM_HL, _) | TwoRegisters(_, MEM_HL) | OneRegister(MEM_HL) => r == MEM_HL
      case TwoRegisters(MEM_BC, _) | TwoRegisters(_, MEM_BC) | OneRegister(MEM_BC) => r == MEM_BC
      case TwoRegisters(MEM_DE, _) | TwoRegisters(_, MEM_DE) | OneRegister(MEM_DE) => r == MEM_DE
      // TODO: IX and IY
      case  _ => opcode match {
        case LD_HLIA | LD_HLDA | LD_AHLD | LD_AHLI => r == MEM_HL
        case LHLX => r == MEM_DE
        case SHLX => r == MEM_DE
        case _ => false
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
      case AF => true
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
          case JP | JR | RET | RETI | RETN | SIM | RIM |
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

          case LD_DESP | LD_DEHL | RLDE => r == D || r == E
          case LHLX | RRHL | DSUB => r == H || r == L
          case SHLX => false

          case MULUB => r == H || r == L
          case MULUW => r == H || r == L || r == D || r == E

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
      case LD_HLSP => false
      case LHLX => true
      case SHLX | LD_DESP | LD_DEHL | DSUB => false
      case _ => true // TODO
    }
  }

  def changesMemory: Boolean = {
    import ZOpcode._
    import ZRegister._
    opcode match {
      case CHANGED_MEM => true
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
      case LD_HLSP => false
      case SHLX => true
      case LHLX | LD_DESP | LD_DEHL | DSUB => false
      case _ => true // TODO
    }
  }
}