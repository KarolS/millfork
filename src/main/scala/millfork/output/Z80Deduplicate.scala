package millfork.output

import millfork.CompilationOptions
import millfork.assembly.z80.{ZOpcode, _}
import millfork.env.{Environment, Label, MemoryAddressConstant}
import ZOpcode._
import millfork.assembly.Elidability
import millfork.node.ZRegister.SP

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
class Z80Deduplicate(env: Environment, options: CompilationOptions) extends Deduplicate[ZLine](env, options) {
  override def getJump(line: ZLine): Option[String] = line match {
    case ZLine0(JP, NoRegisters, MemoryAddressConstant(thing)) => Some(thing.name)
    case _ => None
  }

  override def createLabel(name: String): ZLine = ZLine.label(name)

  override def actualCode(FunctionName: String, functionCode: List[ZLine]): List[ZLine] = {
    functionCode match {
      case ZLine0(LABEL, _, MemoryAddressConstant(Label(FunctionName))) :: xs => xs
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
    CALL, JP, JR, LABEL,
  ) ++ ZOpcodeClasses.AllSingleBit

  private val conditionallyGoodOpcodes = Set(
    LD_16, ADD_16, SBC_16, ADC_16, INC_16, DEC_16,
  )

  override def isExtractable(line: ZLine): Boolean = {
    line.elidable && (alwaysGoodOpcodes(line.opcode) ||
      conditionallyGoodOpcodes(line.opcode) && (line.registers match {
        case OneRegister(SP) => false
        case TwoRegisters(_, SP) => false
        case TwoRegisters(SP, _) => false
        case _ => true
      }))
  }

  override def isBadExtractedCodeHead(head: ZLine): Boolean = false

  override def isBadExtractedCodeLast(last: ZLine): Boolean = last.opcode match {
    case EI | DI | IM => true
    case _ => false
  }

  override def createCall(functionName: String): ZLine = ZLine(CALL, NoRegisters, MemoryAddressConstant(Label(functionName)), elidability = Elidability.Fixed)

  override def createReturn(): ZLine = ZLine.implied(RET)

  def retPrecededByDiscards(xs: List[ZLine]): Option[List[ZLine]] = {
    xs match {
      case ZLine0(op, _, _) :: ys if ZOpcodeClasses.NoopDiscards(op) => retPrecededByDiscards(ys)
      case ZLine0(RET, _, _) :: ys => Some(ys)
      case _ => None
    }
  }

  override def tco(code: List[ZLine]): List[ZLine] = code match {
    case (call@ZLine0(CALL, _, _)) :: xs => retPrecededByDiscards(xs) match {
      case Some(rest) =>call.copy(opcode = JP) :: tco(rest)
      case _ => call :: tco(xs)
    }
    case x :: xs => x :: tco(xs)
    case Nil => Nil
  }

  override def renumerateLabels(code: List[ZLine], temporary: Boolean): List[ZLine] = {
    val map = mutable.Map[String, String]()
    var counter = 0
    code.foreach{
      case ZLine0(LABEL, _, MemoryAddressConstant(Label(x))) if x.startsWith(".") =>
        map(x) = if (temporary) ".ddtmp__" + counter else env.nextLabel("dd")
        counter += 1
      case _ =>
    }
    code.map{
      case l@ZLine0(_, _, MemoryAddressConstant(Label(x))) if map.contains(x) =>
        l.copy(parameter = MemoryAddressConstant(Label(map(x))))
      case l => l
    }
  }

  def checkIfLabelsAreInternal(snippet: List[ZLine], wholeCode: List[ZLine]): Boolean = {
    val myLabels = mutable.Set[String]()
    val useCount = mutable.Map[String, Int]()
    snippet.foreach{
      case ZLine0(LABEL, _, MemoryAddressConstant(Label(x))) =>
        myLabels += x
      case ZLine0(_, _, MemoryAddressConstant(Label(x))) =>
        useCount(x) = useCount.getOrElse(x, 0) - 1
      case _ =>
    }
    wholeCode.foreach {
      case ZLine0(op, _, MemoryAddressConstant(Label(x))) if op != LABEL && myLabels(x) =>
        useCount(x) = useCount.getOrElse(x, 0) + 1
      case _ =>
    }
    useCount.values.forall(_ == 0)
  }

  override def removePositionInfo(line: ZLine): ZLine = line.copy(source = None)

}
