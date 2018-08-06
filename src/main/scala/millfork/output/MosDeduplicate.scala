package millfork.output

import millfork.CompilationOptions
import millfork.assembly.mos.{AddrMode, AssemblyLine, Opcode}
import millfork.env.{Environment, Label, MemoryAddressConstant}
import Opcode._
import millfork.assembly.mos.AddrMode._

/**
  * @author Karol Stasiak
  */
class MosDeduplicate(env: Environment, options: CompilationOptions) extends Deduplicate[AssemblyLine](env, options) {
  override def getJump(line: AssemblyLine): Option[String] = line match {
    case AssemblyLine(Opcode.JMP, Absolute, MemoryAddressConstant(thing), _) => Some(thing.name)
    case _ => None
  }

  override def createLabel(name: String): AssemblyLine = AssemblyLine.label(name)

  override def actualCode(FunctionName: String, functionCode: List[AssemblyLine]): List[AssemblyLine] = {
    functionCode match {
      case AssemblyLine(Opcode.LABEL, _, MemoryAddressConstant(Label(FunctionName)), _) :: xs => xs
      case xs => xs
    }
  }

  private val goodOpcodes = Set(
    ADC, SBC, CMP, AND, EOR, ORA,
    ADC_W, SBC_W, CMP_W, AND_W, EOR_W, ORA_W,
    ASL, ROL, LSR, ROR, INC, DEC,
    ASL_W, ROL_W, LSR_W, ROR_W, INC_W, DEC_W,
    NEG, ASR,
    LDA, STA, LDX, STX, LDY, STY, LDZ, STZ,
    LDA_W, STA_W, LDX_W, STX_W, LDY_W, STY_W, STZ_W,
    TAX, TXA, TAY, TYA, TXY, TYX, TAZ, TZA, XBA,
    SLO, SRE, RRA, RLA, ARR, ALR, ANC, SBX, LXA, XAA, DCP, ISC,
    CPX, CPY, CPZ, CPX_W, CPY_W,
    INX, INY, INZ, INX_W, INY_W,
    DEX, DEY, DEZ, DEX_W, DEY_W,
    BIT, TRB, TSB,
    JSR,
    NOP, WAI, STP,
    SED, CLD, SEC, CLC, CLV, SEI, CLI, SEP, REP,
    HuSAX, SAY, SXY,
    CLA, CLX, CLY,
  )

  private val badAddressingModes = Set(Stack, IndexedSY, Relative)

  override def isExtractable(line: AssemblyLine): Boolean =
    goodOpcodes(line.opcode) && !badAddressingModes(line.addrMode)

  override def isBadExtractedCodeHead(head: AssemblyLine): Boolean = false

  override def isBadExtractedCodeLast(head: AssemblyLine): Boolean = false

  override def createCall(functionName: String): AssemblyLine = AssemblyLine.absolute(Opcode.JSR, Label(functionName))

  override def createReturn(): AssemblyLine = AssemblyLine.implied(RTS)

  override def tco(code: List[AssemblyLine]): List[AssemblyLine] = code match {
    case (call@AssemblyLine(JSR, Absolute, _, _)) :: AssemblyLine(RTS, _, _, _) :: xs => call.copy(opcode = JMP) :: tco(xs)
    case x :: xs => x :: tco(xs)
    case Nil => Nil
  }
}
