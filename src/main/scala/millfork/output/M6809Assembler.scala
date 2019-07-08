package millfork.output

import millfork.{CompilationOptions, Platform}
import millfork.assembly.m6809.{MOpcode, _}
import millfork.compiler.m6809.M6809Compiler
import millfork.env.{Environment, Label, MemoryAddressConstant, NormalFunction}
import millfork.node.{NiceFunctionProperty, Program}

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
class M6809Assembler(program: Program,
                     rootEnv: Environment,
                     platform: Platform) extends AbstractAssembler[MLine](program, rootEnv, platform, M6809InliningCalculator, M6809Compiler) {
  override def bytePseudoopcode: String = "FCB"

  override def deduplicate(options: CompilationOptions, compiledFunctions: mutable.Map[String, CompiledFunction[MLine]]): Unit = ()

  override def injectLabels(labelMap: Map[String, (Int, Int)], code: List[MLine]): List[MLine] = code

  override def quickSimplify(code: List[MLine]): List[MLine] = code

  override def gatherNiceFunctionProperties(niceFunctionProperties: mutable.Set[(NiceFunctionProperty, String)], functionName: String, code: List[MLine]): Unit = ()

  override def performFinalOptimizationPass(f: NormalFunction, actuallyOptimize: Boolean, options: CompilationOptions, code: List[MLine]): List[MLine] = code

  override def emitInstruction(bank: String, options: CompilationOptions, index: Int, instr: MLine): Int = {
    import millfork.assembly.m6809.MOpcode._
    instr match {
      case MLine0(BYTE, RawByte, c) =>
        writeByte(bank, index, c)
        index + 1
      case MLine0(BYTE, _, _) => log.fatal("BYTE opcode failure")
      case MLine0(_, RawByte, _) => log.fatal("BYTE opcode failure")
      case MLine0(LABEL, NonExistent, MemoryAddressConstant(Label(labelName))) =>
        val bank0 = mem.banks(bank)
        labelMap(labelName) = bank0.index -> index
        index
      case MLine0(op, NonExistent, _) if MOpcode.NoopDiscard(op) =>
        index
      case MLine0(op, Inherent, _) if M6809Assembler.inherent.contains(op) =>
        writeByte(bank, index, M6809Assembler.inherent(op))
        index + 1
      case MLine0(op, InherentA, _) if M6809Assembler.inherentA.contains(op) =>
        writeByte(bank, index, M6809Assembler.inherentA(op))
        index + 1
      case MLine0(op, InherentB, _) if M6809Assembler.inherentB.contains(op) =>
        writeByte(bank, index, M6809Assembler.inherentB(op))
        index + 1
      case MLine0(op, Immediate, param) if M6809Assembler.immediate.contains(op) =>
        writeByte(bank, index, M6809Assembler.immediate(op))
        writeByte(bank, index + 1, param)
        index + 1
      case _ =>
        // TODO
        throw new IllegalArgumentException("Not supported: " + instr)
    }
  }
}

object M6809Assembler {

  val inherent: mutable.Map[MOpcode.Value, Byte] = mutable.Map[MOpcode.Value, Byte]()
  val inherentA: mutable.Map[MOpcode.Value, Byte] = mutable.Map[MOpcode.Value, Byte]()
  val inherentB: mutable.Map[MOpcode.Value, Byte] = mutable.Map[MOpcode.Value, Byte]()
  val immediate: mutable.Map[MOpcode.Value, Byte] = mutable.Map[MOpcode.Value, Byte]()

  private def inh(op: MOpcode.Value, value: Int): Unit = inherent(op) = value.toByte

  private def inab(op: MOpcode.Value, value: Int): Unit = {
    inherentA(op) = value.toByte
    inherentB(op) = value.+(0x10).toByte
  }

  private def imm(op: MOpcode.Value, value: Int): Unit = immediate(op) = value.toByte

  import MOpcode._

  inh(ABX, 0x3a)
  inh(DAA, 0x19)
  inh(MUL, 0x3d)
  inh(NOP, 0x12)
  inh(RTI, 0x3B)
  inh(RTS, 0x39)
  inh(SEX, 0x1d)
  inh(SWI, 0x3f)
  inh(SYNC, 0x13)

  inab(ASL, 0x48)
  inab(ASR, 0x47)
  inab(CLR, 0x4f)
  inab(COM, 0x43)
  inab(DEC, 0x4a)
  inab(INC, 0x48)
  inab(LSR, 0x44)
  inab(NEG, 0x40)
  inab(ROL, 0x49)
  inab(ROR, 0x46)
  inab(TST, 0x4d)

}