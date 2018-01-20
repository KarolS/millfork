package millfork.assembly

import java.lang.management.MemoryType

import millfork.assembly.Opcode._
import millfork.assembly.opt.ReadsA
import millfork.compiler.{CompilationContext, MfCompiler}
import millfork.env._

//noinspection TypeAnnotation
object OpcodeClasses {

  val ReadsAAlways = Set(
    ADC, AND, BIT, CMP, EOR, ORA, PHA, SBC, STA, TAX, TAY,
    SAX, SBX, ANC, DCP, ISC, RRA, RLA, SRE, SLO, LXA, XAA, AHX, TAS
  )
  val ReadsAIfImplied = Set(ASL, LSR, ROL, ROR, INC, DEC)
  val ReadsXAlways = Set(
    CPX, DEX, INX, STX, TXA, TXS, SBX,
    PLX,
    XAA, SAX, AHX, SHX, TAS
  )
  val ReadsYAlways = Set(CPY, DEY, INY, STY, TYA, PLY, SHY)
  val ReadsZ = Set(BNE, BEQ, PHP)
  val ReadsN = Set(BMI, BPL, PHP)
  val ReadsNOrZ = ReadsZ ++ ReadsN
  val ReadsV = Set(BVS, BVC, PHP)
  val ReadsD = Set(PHP, ADC, SBC, RRA, ARR, ISC, DCP) // TODO: ??
  val ReadsC = Set(
    PHP, ADC, SBC, BCC, BCS, ROL, ROR,
    ALR, ARR, ISC, RLA, RRA, SLO, SRE // TODO: ??
  )
  val ChangesAAlways = Set(
    TXA, TYA, PLA,
    ORA, AND, EOR, ADC, LDA, SBC,
    SLO, RLA, SRE, RRA, LAX, ISC,
    XAA, ANC, ALR, ARR, LXA, LAS
  )
  val ChangesAIfImplied = Set(ASL, LSR, ROL, ROR, INC, DEC)
  val ChangesX = Set(
    DEX, INX, TAX, LDX, TSX,
    SBX, LAX, LXA, LAS,
    PLX,
  )
  val ChangesY = Set(
    DEY, INY, TAY, LDY
  )
  val ChangesS = Set(
    PHA, PLA, PHP, PLP, TXS,
    PHX, PHY, PLX, PLY, TAS, LAS
  )
  val ChangesMemoryAlways = Set(
    STA, STY, STZ,
    STX, DEC, INC,
    SAX, DCP, ISC,
    SLO, RLA, SRE, RRA,
    AHX, SHY, SHX, TAS, LAS
  )
  val ChangesMemoryIfNotImplied = Set(
    ASL, ROL, LSR, ROR
  )
  val ReadsMemoryIfNotImpliedOrImmediate = Set(
    LDY, CPX, CPY,
    ORA, AND, EOR, ADC, LDA, CMP, SBC,
    ASL, ROL, LSR, ROR, LDX, DEC, INC,
    SLO, RLA, SRE, RRA, LAX, DCP, ISC,
    LAS,
    TRB, TSB
  )
  val OverwritesA = Set(
    LDA, PLA, TXA, TYA,
    LAX, LAS
  )
  val OverwritesX = Set(
    TAX, LDX, TSX, PLX,
    LAX, LAS
  )
  val OverwritesY = Set(
    TAY, LDY, PLY
  )
  val OverwritesC = Set(CLC, SEC, PLP)
  val OverwritesD = Set(CLD, SED, PLP)
  val OverwritesI = Set(CLI, SEI, PLP)
  val OverwritesV = Set(CLV, PLP)
  val ConcernsAAlways = ReadsAAlways ++ ChangesAAlways
  val ConcernsAIfImplied = ReadsAIfImplied ++ ChangesAIfImplied
  val ConcernsXAlways = ReadsXAlways | ChangesX
  val ConcernsYAlways = ReadsYAlways | ChangesY

  val ChangesStack = Set(
    PHA, PLA, PHP, PLP,
    PHX, PLX, PHY, PLY,
    TXS,
    JSR, RTS, RTI,
    TAS, LAS,
  )

  val ConcernsStack = ChangesStack + TSX

  val ChangesNAndZ = Set(
    ADC, AND, ASL, BIT, CMP, CPX, CPY, DEC, DEX, DEY, EOR, INC, INX, INY, LDA,
    LDX, LDY, LSR, ORA, PLP, ROL, ROR, SBC, TAX, TAY, TXA, TYA,
    LAX, SBX, ANC, ALR, ARR, DCP, ISC, RLA, RRA, SLO, SRE, SAX,
    TSB, TRB // These two do not change N, but lets pretend they do for simplicity
  )
  val ChangesC = Set(
    CLC, SEC, ADC, ASL, CMP, CPX, CPY, LSR, PLP, ROL, ROR, SBC,
    SBX, ANC, ALR, ARR, DCP, ISC, RLA, RRA, SLO, SRE
  )
  val ChangesV = Set(
    ADC, BIT, PLP, SBC,
    ARR, ISC, RRA,
  )

  val SupportsAbsoluteX = Set(
    ORA, AND, EOR, ADC, CMP, SBC,
    ASL, ROL, LSR, ROR, DEC, INC,
    SLO, RLA, SRE, RRA, DCP, ISC,
    STA, LDA, LDY, STZ, SHY,
  )

  val SupportsAbsoluteY = Set(
    ORA, AND, EOR, ADC, CMP, SBC,
    SLO, RLA, SRE, RRA, DCP, ISC,
    STA, LDA, LDX,
    LAX, AHX, SHX, TAS, LAS,
  )

  val SupportsAbsolute = Set(
    ORA, AND, EOR, ADC, STA, LDA, CMP, SBC,
    ASL, ROL, LSR, ROR, STX, LDX, DEC, INC,
    SLO, RLA, SRE, RRA, SAX, LAX, DCP, ISC,
    STY, LDY,
    BIT, JMP, JSR,
    STZ, TRB, TSB,
  )

  val SupportsZeroPageIndirect = Set(ORA, AND, EOR, ADC, STA, LDA, CMP, SBC)

  val ShortConditionalBranching = Set(BEQ, BNE, BMI, BPL, BVC, BVS, BCC, BCS)
  val ShortBranching = ShortConditionalBranching + BRA
  val AllDirectJumps = ShortBranching + JMP
  val AllLinear = Set(
    ORA, AND, EOR,
    ADC, SBC, CMP, CPX, CPY,
    DEC, DEX, DEY, INC, INX, INY,
    ASL, ROL, LSR, ROR,
    LDA, STA, LDX, STX, LDY, STY,
    TAX, TXA, TAY, TYA, TXS, TSX,
    PLA, PLP, PHA, PHP,
    BIT, NOP,
    CLC, SEC, CLD, SED, CLI, SEI, CLV,
    STZ, PHX, PHY, PLX, PLY, TSB, TRB,
    SLO, RLA, SRE, RRA, SAX, LAX, DCP, ISC,
    ANC, ALR, ARR, XAA, LXA, SBX,
    DISCARD_AF, DISCARD_XF, DISCARD_YF)

  val NoopDiscardsFlags = Set(DISCARD_AF, DISCARD_XF, DISCARD_YF)
  val DiscardsV = NoopDiscardsFlags | OverwritesV
  val DiscardsC = NoopDiscardsFlags | OverwritesC
  val DiscardsD = OverwritesD
  val DiscardsI = NoopDiscardsFlags | OverwritesI

}

object AssemblyLine {

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
  private val opcodesForZeroedOrSignExtendedVariableOperation = Set(LDA, LDX, LDY)

  def variable(ctx: CompilationContext, opcode: Opcode.Value, variable: Variable, offset: Int = 0): List[AssemblyLine] =
    if (offset > variable.typ.size) {
      if (opcodesForNopVariableOperation(opcode)) {
        Nil
      } else if (opcodesForZeroedVariableOperation(opcode)) {
        if (variable.typ.isSigned) ???
        List(AssemblyLine.immediate(opcode, 0))
      } else if (opcodesForZeroedOrSignExtendedVariableOperation(opcode)) {
        if (variable.typ.isSigned) {
          val label = MfCompiler.nextLabel("sx")
          AssemblyLine.variable(ctx, opcode, variable, variable.typ.size - 1) ++ List(
            AssemblyLine.immediate(ORA, 0x7f),
            AssemblyLine.relative(BMI, label),
            AssemblyLine.immediate(LDA, 0),
            AssemblyLine.label(label))
        } else {
          List(AssemblyLine.immediate(opcode, 0))
        }
      } else {
        ???
      }
    } else {
      variable match {
        case v@MemoryVariable(_, _, VariableAllocationMethod.Zeropage) =>
          List(AssemblyLine.zeropage(opcode, v.toAddress + offset))
        case v@RelativeVariable(_, _, _, true) =>
          List(AssemblyLine.zeropage(opcode, v.toAddress + offset))
        case v: VariableInMemory => List(AssemblyLine.absolute(opcode, v.toAddress + offset))
        case v: StackVariable => List(AssemblyLine.implied(TSX), AssemblyLine.absoluteX(opcode, v.baseOffset + offset + ctx.extraStackOffset))
      }
    }

  def zeropage(opcode: Opcode.Value, addr: Constant) =
    AssemblyLine(opcode, AddrMode.ZeroPage, addr)

  def zeropage(opcode: Opcode.Value, thing: ThingInMemory, offset: Int = 0) =
    AssemblyLine(opcode, AddrMode.ZeroPage, thing.toAddress + offset)

  def absolute(opcode: Opcode.Value, addr: Constant) =
    AssemblyLine(opcode, AddrMode.Absolute, addr)

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

  def absoluteX(opcode: Opcode.Value, addr: Constant) =
    AssemblyLine(opcode, AddrMode.AbsoluteX, addr)

  def absoluteX(opcode: Opcode.Value, thing: ThingInMemory, offset: Int = 0) =
    AssemblyLine(opcode, AddrMode.AbsoluteX, thing.toAddress + offset)

  def indexedY(opcode: Opcode.Value, addr: Constant) =
    AssemblyLine(opcode, AddrMode.IndexedY, addr)

  def indexedY(opcode: Opcode.Value, thing: ThingInMemory, offset: Int = 0) =
    AssemblyLine(opcode, AddrMode.IndexedY, thing.toAddress + offset)
}

case class AssemblyLine(opcode: Opcode.Value, addrMode: AddrMode.Value, var parameter: Constant, elidable: Boolean = true) {


  import AddrMode._
  import State._
  import OpcodeClasses._
  import Treatment._

  def reads(state: State.Value): Boolean = state match {
    case A => if (addrMode == Implied) ReadsAIfImplied(opcode) else ReadsAAlways(opcode)
    case X => addrMode == AbsoluteX || addrMode == ZeroPageX || addrMode == IndexedX || ReadsXAlways(opcode)
    case Y => addrMode == AbsoluteY || addrMode == ZeroPageY || addrMode == IndexedY || ReadsYAlways(opcode)
    case C => ReadsC(opcode)
    case D => ReadsD(opcode)
    case N => ReadsN(opcode)
    case V => ReadsV(opcode)
    case Z => ReadsZ(opcode)
  }

  def treatment(state: State.Value): Treatment.Value = opcode match {
    case LABEL => Unchanged // TODO: ???
    case NOP => Unchanged
    case JSR | JMP | BEQ | BNE | BMI | BPL | BRK | BCC | BVC | BCS | BVS => Changed
    case CLC => if (state == C) Cleared else Unchanged
    case SEC => if (state == C) Set else Unchanged
    case CLV => if (state == V) Cleared else Unchanged
    case CLD => if (state == D) Cleared else Unchanged
    case SED => if (state == D) Set else Unchanged
    case _ => state match { // TODO: smart detection of constants
      case A =>
        if (ChangesAAlways(opcode) || addrMode == Implied && ChangesAIfImplied(opcode))
          Changed
        else
          Unchanged
      case X => if (ChangesX(opcode)) Changed else Unchanged
      case Y => if (ChangesY(opcode)) Changed else Unchanged
      case C => if (ChangesC(opcode)) Changed else Unchanged
      case V => if (ChangesV(opcode)) Changed else Unchanged
      case N | Z => if (ChangesNAndZ(opcode)) Changed else Unchanged
      case D => Unchanged
    }
  }

  def sizeInBytes: Int = addrMode match {
    case Implied => 1
    case Relative | ZeroPageX | ZeroPage | ZeroPageY | IndexedX | IndexedY | Immediate => 2
    case AbsoluteX | Absolute | AbsoluteY | Indirect => 3
    case DoesNotExist => 0
  }

  def cost: Int = addrMode match {
    case Implied => 1000
    case Relative | Immediate => 2000
    case ZeroPage => 2001
    case ZeroPageX | ZeroPageY => 2002
    case IndexedX | IndexedY => 2003
    case Absolute => 3000
    case AbsoluteX | AbsoluteY | Indirect => 3001
    case DoesNotExist => 1
  }

  def isPrintable: Boolean = true //addrMode != AddrMode.DoesNotExist || opcode == LABEL

  override def toString: String =
    if (opcode == LABEL) {
      parameter.toString
    } else if (addrMode == DoesNotExist) {
      s"    ; $opcode"
    } else {
      s"    $opcode ${AddrMode.addrModeToString(addrMode, parameter.toString)}"
    }
}
