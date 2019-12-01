package millfork.output

import millfork.{CompilationOptions, Prehashed}
import millfork.assembly.mos._
import millfork.env.{Environment, Label, MemoryAddressConstant, StructureConstant}
import Opcode._
import millfork.assembly.mos.AddrMode._

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
class MosDeduplicate(env: Environment, options: CompilationOptions) extends Deduplicate[AssemblyLine](env, options) {
  override def getJump(line: AssemblyLine): Option[String] = line match {
    case AssemblyLine0(Opcode.JMP, Absolute, MemoryAddressConstant(thing)) => Some(thing.name)
    case _ => None
  }

  override def createLabel(name: String): AssemblyLine = AssemblyLine.label(name)

  override def actualCode(FunctionName: String, functionCode: List[AssemblyLine]): List[AssemblyLine] = {
    functionCode match {
      case AssemblyLine0(Opcode.LABEL, _, MemoryAddressConstant(Label(FunctionName))) :: xs => xs
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
    JMP, BRA, BEQ, BNE, BMI, BCC, BCS, BVC, BVS, LABEL,
  )

  private val badAddressingModes = Set(Stack, IndexedSY, AbsoluteIndexedX, Indirect, LongIndirect)

  override def isExtractable(line: AssemblyLine): Boolean =
    line.elidable && goodOpcodes(line.opcode) && !badAddressingModes(line.addrMode) && (line.parameter match {
      case MemoryAddressConstant(Label(x)) => !x.startsWith(".")
      case StructureConstant(_, List(_, MemoryAddressConstant(Label(x)))) => !x.startsWith(".")
      case _ => true
    })

  override def isBadExtractedCodeHead(head: AssemblyLine): Boolean = false

  override def isBadExtractedCodeLast(last: AssemblyLine): Boolean = false

  override def createCall(functionName: String): AssemblyLine = AssemblyLine.absolute(Opcode.JSR, Label(functionName))

  override def createReturn(): AssemblyLine = AssemblyLine.implied(RTS)

  def rtsPrecededByDiscards(xs: List[AssemblyLine]): Option[List[AssemblyLine]] = {
    xs match {
      case AssemblyLine0(op, _, _) :: xs if OpcodeClasses.NoopDiscardsFlags(op) => rtsPrecededByDiscards(xs)
      case AssemblyLine0(RTS, _, _) :: xs => Some(xs)
      case _ => None
    }
  }

  override def tco(code: List[AssemblyLine]): List[AssemblyLine] = code match {
    case (call@AssemblyLine0(JSR, Absolute | LongAbsolute, _)) :: xs => rtsPrecededByDiscards(xs) match {
      case Some(rest) => call.copy(opcode = JMP) :: tco(rest)
      case _ => call :: tco(xs)
    }
    case x :: xs => x :: tco(xs)
    case Nil => Nil
  }

  override def renumerateLabels(code: List[AssemblyLine], temporary: Boolean): Prehashed[List[AssemblyLine]] = {
    val map = mutable.Map[String, String]()
    var counter = 0
    code.foreach{
      case AssemblyLine0(LABEL, _, MemoryAddressConstant(Label(x))) if x.startsWith(".") =>
        map(x) = if (temporary) ".ddtmp__" + counter else env.nextLabel("dd")
        counter += 1
      case _ =>
    }
    Prehashed(code.map{
      case l@AssemblyLine0(_, _, MemoryAddressConstant(Label(x))) if map.contains(x) =>
        l.copy(parameter = MemoryAddressConstant(Label(map(x))))
      case l => l
    })
  }

  def checkIfLabelsAreInternal(snippet: List[AssemblyLine], wholeCode: List[AssemblyLine]): Boolean = {
    val myLabels = mutable.Set[String]()
    val useCount = mutable.Map[String, Int]()
    snippet.foreach{
      case AssemblyLine0(LABEL, _, MemoryAddressConstant(Label(x))) =>
        myLabels += x
      case AssemblyLine0(_, _, MemoryAddressConstant(Label(x))) =>
        useCount(x) = useCount.getOrElse(x, 0) - 1
      case _ =>
    }
    wholeCode.foreach {
      case AssemblyLine0(op, _, MemoryAddressConstant(Label(x))) if op != LABEL && myLabels(x) =>
        useCount(x) = useCount.getOrElse(x, 0) + 1
      case _ =>
    }
    useCount.values.forall(_ == 0)
  }

  override def removePositionInfo(line: AssemblyLine): AssemblyLine = line.copy(source = None)

}
