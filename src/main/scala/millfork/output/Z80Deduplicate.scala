package millfork.output

import millfork.CompilationOptions
import millfork.assembly.z80.{ZOpcode, _}
import millfork.env.{Environment, Label, MemoryAddressConstant}
import ZOpcode._
import millfork.node.ZRegister.SP

/**
  * @author Karol Stasiak
  */
class Z80Deduplicate(env: Environment, options: CompilationOptions) extends Deduplicate[ZLine](env, options) {
  override def getJump(line: ZLine): Option[String] = line match {
    case ZLine(JP, NoRegisters, MemoryAddressConstant(thing), _) => Some(thing.name)
    case _ => None
  }

  override def createLabel(name: String): ZLine = ZLine.label(name)

  override def actualCode(FunctionName: String, functionCode: List[ZLine]): List[ZLine] = {
    functionCode match {
      case ZLine(LABEL, _, MemoryAddressConstant(Label(FunctionName)), _) :: xs => xs
      case xs => xs
    }
  }

  private val alwaysGoodOpcodes: Set[ZOpcode.Value] = Set(
    ADD, ADC, SUB, SBC, XOR, OR, AND, CP,
    LD, INC, DEC,
    DAA, CPL, SCF, CCF, NEG, EX_DE_HL,
    RLA, RRA, RLCA, RRCA,
    RL, RR, RLC, RRC, SLA, SLL, SRL, SRA, SWAP,
    RLD, RRD,
    EI, DI, IM, HALT, NOP,
    LDI, LDD, LDIR, LDDR,
    INI, IND, INIR, INDR,
    CPI, CPD, CPIR, CPDR,
    OUTI, OUTD, OUTIR, OUTDR,
    IN_IMM, OUT_IMM, IN_C, OUT_C,
    LD_AHLI, LD_AHLD, LD_HLIA, LD_HLDA,
    LDH_AC, LDH_AD, LDH_CA, LDH_DA,
    CALL,
  ) ++ ZOpcodeClasses.AllSingleBit

  private val conditionallyGoodOpcodes = Set(
    LD_16, ADD_16, SBC_16, ADC_16, INC_16, DEC_16,
  )

  override def isExtractable(line: ZLine): Boolean = {
    alwaysGoodOpcodes(line.opcode) ||
      conditionallyGoodOpcodes(line.opcode) && (line.registers match {
        case OneRegister(SP) => false
        case TwoRegisters(_, SP) => false
        case TwoRegisters(SP, _) => false
        case _ => true
      })
  }

  override def isBadExtractedCodeHead(head: ZLine): Boolean = false

  override def isBadExtractedCodeLast(head: ZLine): Boolean = head.opcode match {
    case EI | DI | IM => true
    case _ => false
  }

  override def createCall(functionName: String): ZLine = ZLine(CALL, NoRegisters, MemoryAddressConstant(Label(functionName)), elidable = false)

  override def createReturn(): ZLine = ZLine.implied(RET)

  override def tco(code: List[ZLine]): List[ZLine] = code match {
    case (call@ZLine(CALL, _, _, _)) :: ZLine(RET, NoRegisters, _, _) :: xs => call.copy(opcode = JP) :: tco(xs)
    case x :: xs => x :: tco(xs)
    case Nil => Nil
  }
}
