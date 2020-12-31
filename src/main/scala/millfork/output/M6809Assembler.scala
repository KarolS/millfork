package millfork.output

import millfork.assembly.SourceLine
import millfork.assembly.m6809.opt.JumpFixing
import millfork.{CompilationOptions, Platform}
import millfork.assembly.m6809.{MOpcode, _}
import millfork.compiler.m6809.M6809Compiler
import millfork.env.{Environment, FunctionInMemory, Label, MemoryAddressConstant, NormalFunction, NumericConstant}
import millfork.node.{M6809Register, NiceFunctionProperty, Position, Program}

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

  override def gatherNiceFunctionProperties(options: CompilationOptions, niceFunctionProperties: mutable.Set[(NiceFunctionProperty, String)], function: NormalFunction, code: List[MLine]): Unit = ()

  override def gatherFunctionOptimizationHints(options: CompilationOptions, niceFunctionProperties: mutable.Set[(NiceFunctionProperty, String)], function: FunctionInMemory): Unit = {
    import NiceFunctionProperty._
    import millfork.node.M6809NiceFunctionProperty._
    val functionName = function.name
    if (function.optimizationHints("preserves_memory")) niceFunctionProperties += DoesntWriteMemory -> functionName
    if (function.optimizationHints("idempotent")) niceFunctionProperties += Idempotent -> functionName
    if (function.optimizationHints("preserves_a")) niceFunctionProperties += DoesntChangeA -> functionName
    if (function.optimizationHints("preserves_b")) niceFunctionProperties += DoesntChangeB -> functionName
    if (function.optimizationHints("preserves_d")) {
      niceFunctionProperties += DoesntChangeA -> functionName
      niceFunctionProperties += DoesntChangeB -> functionName
    }
    if (function.optimizationHints("preserves_x")) niceFunctionProperties += DoesntChangeX -> functionName
    if (function.optimizationHints("preserves_y")) niceFunctionProperties += DoesntChangeY -> functionName
    if (function.optimizationHints("preserves_u")) niceFunctionProperties += DoesntChangeU -> functionName
    if (function.optimizationHints("preserves_dp")) niceFunctionProperties += DoesntChangeDP -> functionName
    if (function.optimizationHints("preserves_c")) niceFunctionProperties += DoesntChangeCF -> functionName
  }

  override def performFinalOptimizationPass(f: NormalFunction, actuallyOptimize: Boolean, options: CompilationOptions, code: List[MLine]): List[MLine] = {
    JumpFixing(f, code, options)
  }

  private def registerCode(register: M6809Register.Value): Int = {
    import M6809Register._
    register match {
      case D => 0
      case X => 1
      case Y => 2
      case U => 3
      case S => 4
      case PC => 5
      case A => 8
      case B => 9
      case CC => 10
      case DP => 11
    }
  }
  private def registerPushMask(register: M6809Register.Value, useUserStack: Boolean, position: Option[Position]): Int = {
    import M6809Register._
    register match {
      case D => 6
      case X => 0x10
      case Y => 0x20
      case U =>
        0x40
      case S =>
        if (!useUserStack) log.error("Can't push/pull the S register onto the system stack", position)
        0x40
      case PC => 0x80
      case A => 2
      case B => 4
      case CC => 1
      case DP =>
        log.error("Can't push/pull the DP register", position)
        0
    }
  }

  override def emitInstruction(bank: String, options: CompilationOptions, startIndex: Int, instr: MLine): Int = {
    implicit val position = instr.source.map(sl => Position(sl.moduleName, sl.line, 0, 0))
    import millfork.assembly.m6809.MOpcode._
    var index: Int = startIndex
    if (MOpcode.PrefixedBy10(instr.opcode)) {
      writeByte(bank, index, 0x10)
      index += 1
    }
    if (MOpcode.PrefixedBy11(instr.opcode)) {
      writeByte(bank, index, 0x11)
      index += 1
    }
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
      case MLine0(CHANGED_MEM, NonExistent, MemoryAddressConstant(Label(l))) if l.contains("..brk") =>
        breakpointSet += mem.banks(bank).index -> index
        index
      case MLine0(CHANGED_MEM, _, _) =>
        index
      case MLine0(TFR, TwoRegisters(source, target), param)
        if M6809Register.registerSize(source) == M6809Register.registerSize(target) && param.isProvablyZero =>
        writeByte(bank, index, 0x1f)
        writeByte(bank, index + 1, registerCode(source) * 16 + registerCode(target))
        index + 2
      case MLine0(EXG, TwoRegisters(source, target), param)
        if M6809Register.registerSize(source) == M6809Register.registerSize(target) && param.isProvablyZero =>
        writeByte(bank, index, 0x1e)
        writeByte(bank, index + 1, registerCode(source) * 16 + registerCode(target))
        index + 2
      case l@MLine0(op, RegisterSet(set), param) if param.isProvablyZero && M6809Assembler.pushpull.contains(op) =>
        writeByte(bank, index, M6809Assembler.pushpull(op))
        val useUserStack = op == PULU || op == PSHU
        if (useUserStack && set(M6809Register.U)) log.error("Can't push/pull the U register onto the user stack", position)
        if (!useUserStack && set(M6809Register.S)) log.error("Can't push/pull the U register onto the system stack", position)
        if (set(M6809Register.DP)) log.error("Can't push/pull the DP register", position)
        writeByte(bank, index + 1, set.foldLeft(0)((acc,reg) => acc | registerPushMask(reg, useUserStack, position)))
        index + 2
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
        index + 2
      case MLine0(op, Immediate, param) if M6809Assembler.standardByte.contains(op) =>
        writeByte(bank, index, M6809Assembler.standardByte(op))
        writeByte(bank, index + 1, param)
        index + 2
      case MLine0(op, Immediate, param) if M6809Assembler.standardWord.contains(op) =>
        writeByte(bank, index, M6809Assembler.standardWord(op))
        writeWord(bank, index + 1, param)
        index + 3
      case MLine0(op, DirectPage, param) if M6809Assembler.standard.contains(op) =>
        if (M6809Assembler.standard(op).&(0xf0) == 0x40) {
          writeByte(bank, index, M6809Assembler.standard(op) & 0xf)
        } else {
          writeByte(bank, index, M6809Assembler.standard(op) + 0x10)
        }
        writeByte(bank, index + 1, param)
        index + 2
      case MLine0(op, Absolute(false), param) if M6809Assembler.standard.contains(op) =>
        writeByte(bank, index, M6809Assembler.standard(op) + 0x30)
        writeWord(bank, index + 1, param)
        index + 3
      case MLine0(op, Absolute(true), param) if M6809Assembler.indexable.contains(op) =>
        writeByte(bank, index, M6809Assembler.standard(op) + 0x20)
        writeByte(bank, index + 1, 0x9f)
        writeWord(bank, index + 2, param)
        index + 4
      case MLine0(op, Indexed(M6809Register.PC, indirect), param) if M6809Assembler.indexable.contains(op) =>
        ???
      case MLine0(op, Indexed(register, indirect), param) if M6809Assembler.indexable.contains(op) =>
        writeByte(bank, index, M6809Assembler.indexable(op) + 0x20)
        val ri = getRegByte(register, indirect)
        param match {
          case NumericConstant(0, _) =>
            writeByte(bank, index + 1, 0x84 + ri)
            index + 2
          case NumericConstant(n, _) if !indirect && n >= -16 && n <= 15 =>
            writeByte(bank, index + 1, ri + n.toInt.&(0x1f))
            index + 2
          case NumericConstant(n, _) if n >= -128 && n <= 127 =>
            writeByte(bank, index + 1, 0x88 + ri)
            writeWord(bank, index + 2, param)
            index + 3
          case _ =>
            writeByte(bank, index + 1, 0x89 + ri)
            writeWord(bank, index + 2, param)
            index + 4
        }
      case MLine0(op, PostIncremented(register, 1, false), param) if M6809Assembler.indexable.contains(op) =>
        if (!param.isProvablyZero) log.error(s"Index offset has to be zero, not $param", position)
        writeByte(bank, index, M6809Assembler.indexable(op) + 0x20)
        writeByte(bank, index + 1, 0x80 + getRegByte(register, indirect = false))
        index + 2
      case MLine0(op, PostIncremented(register, 2, indirect), param) if M6809Assembler.indexable.contains(op) =>
        if (!param.isProvablyZero) log.error(s"Index offset has to be zero, not $param", position)
        writeByte(bank, index, M6809Assembler.indexable(op) + 0x20)
        writeByte(bank, index + 1, 0x81 + getRegByte(register, indirect))
        index + 2
      case MLine0(op, PreDecremented(register, 1, false), param) if M6809Assembler.indexable.contains(op) =>
        if (!param.isProvablyZero) log.error(s"Index offset has to be zero, not $param", position)
        writeByte(bank, index, M6809Assembler.indexable(op) + 0x20)
        writeByte(bank, index + 1, 0x82 + getRegByte(register, indirect = false))
        index + 2
      case MLine0(op, PreDecremented(register, 2, indirect), param) if M6809Assembler.indexable.contains(op) =>
        if (!param.isProvablyZero) log.error(s"Index offset has to be zero, not $param", position)
        writeByte(bank, index, M6809Assembler.indexable(op) + 0x20)
        writeByte(bank, index + 1, 0x83 + getRegByte(register, indirect))
        index + 2
      case MLine0(op, BAccumulatorIndexed(register, indirect), param) if M6809Assembler.indexable.contains(op) =>
        if (!param.isProvablyZero) log.error(s"Index offset has to be zero, not $param", position)
        writeByte(bank, index, M6809Assembler.indexable(op) + 0x20)
        writeByte(bank, index + 1, 0x85 + getRegByte(register, indirect))
        index + 2
      case MLine0(op, AAccumulatorIndexed(register, indirect), param) if M6809Assembler.indexable.contains(op) =>
        if (!param.isProvablyZero) log.error(s"Index offset has to be zero, not $param", position)
        writeByte(bank, index, M6809Assembler.indexable(op) + 0x20)
        writeByte(bank, index + 1, 0x86 + getRegByte(register, indirect))
        index + 2
      case MLine0(op, DAccumulatorIndexed(register, indirect), param) if M6809Assembler.indexable.contains(op) =>
        if (!param.isProvablyZero) log.error(s"Index offset has to be zero, not $param", position)
        writeByte(bank, index, M6809Assembler.indexable(op) + 0x20)
        writeByte(bank, index + 1, 0x8b + getRegByte(register, indirect))
        index + 2
      case MLine0(op, Relative, param) if M6809Assembler.branches.contains(op) =>
        writeByte(bank, index, M6809Assembler.branches(op))
        writeByte(bank, index + 1, param - (index + 2))
        index + 2
      case MLine0(BRA, LongRelative, param) =>
        writeByte(bank, index, 0x16)
        writeWord(bank, index + 1, param - (index + 3))
        index + 3
      case MLine0(op, LongRelative, param) if M6809Assembler.branches.contains(op) =>
        writeByte(bank, index, 0x10)
        writeByte(bank, index + 1, M6809Assembler.branches(op))
        writeWord(bank, index + 2, param - (index + 4))
        index + 4
      case _ =>
        // TODO
        throw new IllegalArgumentException("Not supported: " + instr + " " + instr.productIterator.mkString("/"))
    }
  }

  private def getRegByte(register: M6809Register.Value, indirect: Boolean) = {
    (register match {
      case M6809Register.X => 0x00
      case M6809Register.Y => 0x20
      case M6809Register.U => 0x40
      case M6809Register.S => 0x60
    }) + (if (indirect) 0x10 else 0)
  }
}

object M6809Assembler {

  val inherent: mutable.Map[MOpcode.Value, Int] = mutable.Map[MOpcode.Value, Int]()
  val inherentA: mutable.Map[MOpcode.Value, Int] = mutable.Map[MOpcode.Value, Int]()
  val inherentB: mutable.Map[MOpcode.Value, Int] = mutable.Map[MOpcode.Value, Int]()
  val immediate: mutable.Map[MOpcode.Value, Int] = mutable.Map[MOpcode.Value, Int]()
  val standardByte: mutable.Map[MOpcode.Value, Int] = mutable.Map[MOpcode.Value, Int]()
  val standardWord: mutable.Map[MOpcode.Value, Int] = mutable.Map[MOpcode.Value, Int]()
  val standard: mutable.Map[MOpcode.Value, Int] = mutable.Map[MOpcode.Value, Int]()
  val indexable: mutable.Map[MOpcode.Value, Int] = mutable.Map[MOpcode.Value, Int]()
  val pushpull: mutable.Map[MOpcode.Value, Int] = mutable.Map[MOpcode.Value, Int]()
  val branches: mutable.Map[MOpcode.Value, Int] = mutable.Map[MOpcode.Value, Int]()

  private def br(op: MOpcode.Value, value: Int): Unit = branches(op) = value

  private def inh(op: MOpcode.Value, value: Int): Unit = inherent(op) = value

  private def pp(op: MOpcode.Value, value: Int): Unit = pushpull(op) = value

  private def inab(op: MOpcode.Value, value: Int): Unit = {
    inherentA(op) = value
    inherentB(op) = value.+(0x10)
  }

  private def imm(op: MOpcode.Value, value: Int): Unit = immediate(op) = value

  private def stb(op: MOpcode.Value, value: Int): Unit = {
    standardByte(op) = value
    standard(op) = value
    indexable(op) = value
  }

  private def stw(op: MOpcode.Value, value: Int): Unit = {
    standardWord(op) = value
    standard(op) = value
    indexable(op) = value
  }

  private def stm(op: MOpcode.Value, value: Int): Unit = {
    standard(op) = value
    indexable(op) = value
  }

  private def lea(op: MOpcode.Value, value: Int): Unit = {
    indexable(op) = value
  }

  import MOpcode._

  inh(ABX, 0x3a)
  inh(DAA, 0x19)
  inh(MUL, 0x3d)
  inh(NOP, 0x12)
  inh(RTI, 0x3B)
  inh(RTS, 0x39)
  inh(SEX, 0x1d)
  inh(SWI, 0x3f)
  inh(SWI2, 0x3f)
  inh(SWI3, 0x3f)
  inh(SYNC, 0x13)

  inab(ASL, 0x48)
  inab(ASR, 0x47)
  inab(CLR, 0x4f)
  inab(COM, 0x43)
  inab(DEC, 0x4a)
  inab(INC, 0x4c)
  inab(LSR, 0x44)
  inab(NEG, 0x40)
  inab(ROL, 0x49)
  inab(ROR, 0x46)
  inab(TST, 0x4d)

  stm(ASL, 0x48)
  stm(ASR, 0x47)
  stm(CLR, 0x4f)
  stm(COM, 0x43)
  stm(DEC, 0x4a)
  stm(INC, 0x4c)
  stm(LSR, 0x44)
  stm(NEG, 0x40)
  stm(ROL, 0x49)
  stm(ROR, 0x46)
  stm(TST, 0x4d)

  imm(ANDCC, 0x1c)
  imm(CWAI, 0x3c)
  imm(ORCC, 0x1a)

  pp(PSHS, 0x34)
  pp(PSHU, 0x36)
  pp(PULS, 0x35)
  pp(PULU, 0x37)


  stb(ADCA, 0x89)
  stb(ADCB, 0xc9)
  stb(ADDA, 0x8b)
  stb(ADDB, 0xcb)
  stw(ADDD, 0xc3)
  stb(ANDA, 0x84)
  stb(ANDB, 0xc4)
  stb(BITA, 0x85)
  stb(BITB, 0xc5)
  stb(CMPA, 0x81)
  stb(CMPB, 0xc1)
  stw(CMPD, 0x83)
  stw(CMPS, 0x8c)
  stw(CMPU, 0x83)
  stw(CMPX, 0x8c)
  stw(CMPY, 0x8c)
  stb(EORA, 0x88)
  stb(EORB, 0xc8)
  stm(JMP, 0x4e)
  stm(JSR, 0x8d)
  stb(LDA, 0x86)
  stb(LDB, 0xc6)
  stw(LDD, 0xcc)
  stw(LDS, 0xce)
  stw(LDU, 0xce)
  stw(LDX, 0x8e)
  stw(LDY, 0x8e)
  lea(LEAS, 0x12)
  lea(LEAU, 0x13)
  lea(LEAX, 0x10)
  lea(LEAY, 0x11)
  stb(ORA, 0x8A)
  stb(ORB, 0xcA)
  stb(SBCA, 0x82)
  stb(SBCB, 0xc2)
  stm(STA, 0x87)
  stm(STB, 0xc7)
  stm(STD, 0xcd)
  stm(STS, 0xcf)
  stm(STU, 0xcf)
  stm(STX, 0x8f)
  stm(STY, 0x8f)
  stb(SUBA, 0x80)
  stb(SUBB, 0xc0)
  stw(SUBD, 0x83)

  br(BRA, 0x20)
  br(BRN, 0x21)
  br(BHI, 0x22)
  br(BLS, 0x23)
  br(BCC, 0x24)
  br(BCS, 0x25)
  br(BNE, 0x26)
  br(BEQ, 0x27)
  br(BVC, 0x28)
  br(BVS, 0x29)
  br(BPL, 0x2a)
  br(BMI, 0x2b)
  br(BGE, 0x2c)
  br(BLT, 0x2d)
  br(BGT, 0x2e)
  br(BLE, 0x2f)


}

