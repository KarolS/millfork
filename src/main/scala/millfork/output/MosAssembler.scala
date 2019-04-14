package millfork.output

import millfork.assembly.mos.opt.{HudsonOptimizations, JumpFixing, JumpFollowing, JumpShortening}
import millfork.assembly._
import millfork.env._
import millfork.error.{ConsoleLogger, FatalErrorReporting}
import millfork.node.{MosNiceFunctionProperty, NiceFunctionProperty, Program}
import millfork._
import millfork.assembly.mos._
import millfork.compiler.mos.MosCompiler

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
class MosAssembler(program: Program,
                   rootEnv: Environment,
                   platform: Platform) extends AbstractAssembler[AssemblyLine](program, rootEnv, platform, MosInliningCalculator, MosCompiler) {


  override def performFinalOptimizationPass(f: NormalFunction, actuallyOptimize: Boolean, options: CompilationOptions, code: List[AssemblyLine]):List[AssemblyLine] = {
    val optimizationContext = OptimizationContext(options, Map(), f.environment.maybeGet[ThingInMemory]("__reg"), Set())
    if (actuallyOptimize) {
      val finalCode = if (options.flag(CompilationFlag.EmitHudsonOpcodes)) HudsonOptimizations.removeLoadZero(f, code, optimizationContext) else code
      JumpShortening(f, JumpShortening(f, JumpFixing(f, JumpFollowing(options, finalCode), options), optimizationContext), optimizationContext)
    }
    else JumpFixing(f, code, options)
  }

  override def emitInstruction(bank: String, options: CompilationOptions, index: Int, instr: AssemblyLine): Int = {
    import millfork.assembly.mos.AddrMode._
    import millfork.assembly.mos.Opcode._
    instr match {
      case AssemblyLine0(BYTE, RawByte, c) =>
        writeByte(bank, index, c)
        index + 1
      case AssemblyLine0(BYTE, _, _) => log.fatal("BYTE opcode failure")
      case AssemblyLine0(_, RawByte, _) => log.fatal("BYTE opcode failure")
      case AssemblyLine0(LABEL, _, MemoryAddressConstant(Label(labelName))) =>
        labelMap(labelName) = index
        index
      case AssemblyLine0(_, DoesNotExist, _) =>
        index
      case AssemblyLine0(op, Implied, _) =>
        writeByte(bank, index, MosAssembler.opcodeFor(op, Implied, options))
        index + 1
      case AssemblyLine0(op, Relative, param) =>
        writeByte(bank, index, MosAssembler.opcodeFor(op, Relative, options))
        writeByte(bank, index + 1, AssertByte(param - (index + 2)))
        index + 2
      case AssemblyLine0(op, am@(Immediate | ZeroPage | ZeroPageX | ZeroPageY | IndexedY | IndexedX | IndexedZ | IndexedSY | LongIndexedY | LongIndexedZ | Stack), param) =>
        writeByte(bank, index, MosAssembler.opcodeFor(op, am, options))
        writeByte(bank, index + 1, param)
        index + 2
      case AssemblyLine0(op, am@(WordImmediate | Absolute | AbsoluteY | AbsoluteX | Indirect | AbsoluteIndexedX), param) =>
        writeByte(bank, index, MosAssembler.opcodeFor(op, am, options))
        writeWord(bank, index + 1, param)
        index + 3
      case AssemblyLine0(op, am@(LongAbsolute | LongAbsoluteX | LongIndirect), param) =>
        writeByte(bank, index, MosAssembler.opcodeFor(op, am, options))
        writeWord(bank, index + 1, param)
        writeByte(bank, index + 3, extractBank(param, options))
        index + 4
    }
  }

  override def injectLabels(labelMap: Map[String, Int], code: List[AssemblyLine]): List[AssemblyLine] = {
    import Opcode._
    code.map {
      case l@AssemblyLine(LDA | STA | CMP |
                          LDX | STX | CPX |
                          LDY | STY | CPY |
                          LDZ | STZ | CPZ |
                          BIT |
                          ADC | SBC | AND | ORA | EOR |
                          INC | DEC | ROL | ROR | ASL | LSR |
                          ISC | DCP | LAX | SAX | RLA | RRA | SLO | SRE, AddrMode.Absolute, p, Elidability.Elidable, _) =>
        p match {
          case NumericConstant(n, _) => if (n <= 255) l.copy(addrMode = AddrMode.ZeroPage) else l
          case MemoryAddressConstant(th) => if (labelMap.getOrElse(th.name, 0x800) < 0x100) l.copy(addrMode = AddrMode.ZeroPage) else l
          case CompoundConstant(MathOperator.Plus,
          MemoryAddressConstant(th),
          NumericConstant(n, _)) => if (labelMap.getOrElse(th.name, 0x800) + n < 0x100) l.copy(addrMode = AddrMode.ZeroPage) else l
          case _ => l
        }

      case l => l
    }
  }

  @tailrec
  private def isNaughty(code: List[AssemblyLine]): Boolean = {
    import Opcode._
    import AddrMode._
    code match {
      case AssemblyLine0(JMP | JSR | BSR, Indirect | LongIndirect | AbsoluteIndexedX, _) :: _ => true
      case AssemblyLine0(PHA, _, _) :: AssemblyLine0(RTS | RTL, _, _) :: _ => true
      case _ :: xs => isNaughty(xs)
      case Nil => false
    }
  }

  override def quickSimplify(code: List[AssemblyLine]): List[AssemblyLine] = code.map(a => a.copy(parameter = a.parameter.quickSimplify))

  override def gatherNiceFunctionProperties(niceFunctionProperties: mutable.Set[(NiceFunctionProperty, String)], functionName: String, code: List[AssemblyLine]): Unit = {
    import Opcode._
    import AddrMode._
    import MosNiceFunctionProperty._
    import NiceFunctionProperty._
    if (isNaughty(code)) return
    val localLabels = code.flatMap {
      case AssemblyLine0(LABEL, _, MemoryAddressConstant(Label(l))) => Some(l)
      case _ => None
    }.toSet
    def genericPropertyScan(niceFunctionProperty: NiceFunctionProperty)(predicate: AssemblyLine => Boolean): Unit = {
      val preserved = code.forall {
        case AssemblyLine0(JSR | BSR | JMP, Absolute | LongAbsolute, MemoryAddressConstant(th)) => niceFunctionProperties(niceFunctionProperty -> th.name)
        case AssemblyLine0(JSR | BSR, _, _) => false
        case AssemblyLine0(op, _, MemoryAddressConstant(Label(label))) if OpcodeClasses.AllDirectJumps(op) => localLabels(label)
        case AssemblyLine0(op, _, _) if OpcodeClasses.AllDirectJumps(op) => false
        case l => predicate(l)
      }
      if (preserved) {
        niceFunctionProperties += (niceFunctionProperty -> functionName)
      }
    }
    genericPropertyScan(DoesntChangeX) {
      case AssemblyLine0(op, _, _) => !OpcodeClasses.ChangesX(op)
    }
    genericPropertyScan(DoesntChangeY) {
      case AssemblyLine0(op, _, _) => !OpcodeClasses.ChangesY(op)
    }
    genericPropertyScan(DoesntChangeA) {
      case AssemblyLine0(op, _, _) if OpcodeClasses.ChangesAAlways(op) => false
      case AssemblyLine0(op, _, Implied) if OpcodeClasses.ChangesAIfImplied(op) => false
      case _ => true
    }
    genericPropertyScan(DoesntChangeIZ) {
      case AssemblyLine0(op, _, _) => !OpcodeClasses.ChangesIZ(op)
    }
    genericPropertyScan(DoesntChangeAH) {
      case AssemblyLine0(op, _, _) if OpcodeClasses.ChangesAHAlways(op) => false
      case AssemblyLine0(op, _, Implied) if OpcodeClasses.ChangesAHIfImplied(op) => false
      case _ => true
    }
    genericPropertyScan(DoesntChangeC) {
      case AssemblyLine0(SEP | REP, Immediate, NumericConstant(imm, _)) => (imm & 1) == 0
      case AssemblyLine0(SEP | REP, _, _) => false
      case AssemblyLine0(op, _, _) => !OpcodeClasses.ChangesC(op)
    }
    genericPropertyScan(DoesntConcernD) {
      case AssemblyLine0(SEP | REP, Immediate, NumericConstant(imm, _)) => (imm & 8) == 0
      case AssemblyLine0(SEP | REP, _, _) => false
      case AssemblyLine0(op, _, _) => !OpcodeClasses.ReadsD(op) && !OpcodeClasses.OverwritesD(op)
    }
    genericPropertyScan(DoesntReadMemory) {
      case AssemblyLine0(op, Implied | Immediate | WordImmediate, _) => true
      case AssemblyLine0(op, _, _) if OpcodeClasses.ReadsMemoryIfNotImpliedOrImmediate(op) => false
      case _ => true
    }
    genericPropertyScan(DoesntWriteMemory) {
      case AssemblyLine0(op, Implied | Immediate | WordImmediate, _)  => true
      case AssemblyLine0(op, _, _) if OpcodeClasses.ChangesMemoryIfNotImplied(op) || OpcodeClasses.ChangesMemoryAlways(op) => false
      case _ => true
    }
    genericPropertyScan(IsLeaf) {
      case AssemblyLine0(JSR | BSR, Implied | Immediate | WordImmediate, _)  => false
      case AssemblyLine0(JMP, Absolute, th:Thing)  => th.name.startsWith(".")
      case _ => true
    }
  }

  override def bytePseudoopcode: String = "!byte"

  override def deduplicate(options: CompilationOptions, compiledFunctions: mutable.Map[String, CompiledFunction[AssemblyLine]]): Unit =
    new MosDeduplicate(rootEnv, options).apply(compiledFunctions)
}


object MosAssembler {
  private val opcodes = mutable.Map[(Opcode.Value, AddrMode.Value), Byte]()
  private val illegalOpcodes = mutable.Map[(Opcode.Value, AddrMode.Value), Byte]()
  private val cmosOpcodes = mutable.Map[(Opcode.Value, AddrMode.Value), Byte]()
  private val cmosNopOpcodes = mutable.Map[(Opcode.Value, AddrMode.Value), Byte]()
  private val ce02Opcodes = mutable.Map[(Opcode.Value, AddrMode.Value), Byte]()
  private val hudsonOpcodes = mutable.Map[(Opcode.Value, AddrMode.Value), Byte]()
  private val emulation65816Opcodes = mutable.Map[(Opcode.Value, AddrMode.Value), Byte]()
  private val native65816Opcodes = mutable.Map[(Opcode.Value, AddrMode.Value), Byte]()

  def opcodeFor(opcode: Opcode.Value, addrMode: AddrMode.Value, options: CompilationOptions): Byte = {
    val key = opcode -> addrMode
    opcodes.get(key).foreach(return _)
    if (options.flag(CompilationFlag.EmitIllegals)) illegalOpcodes.get(key).foreach(return _)
    if (options.flag(CompilationFlag.EmitCmosOpcodes)) cmosOpcodes.get(key).foreach(return _)
    if (options.flag(CompilationFlag.EmitCmosNopOpcodes)) cmosNopOpcodes.get(key).foreach(return _)
    if (options.flag(CompilationFlag.Emit65CE02Opcodes)) ce02Opcodes.get(key).foreach(return _)
    if (options.flag(CompilationFlag.EmitHudsonOpcodes))  hudsonOpcodes.get(key).foreach(return _)
    if (options.flag(CompilationFlag.EmitEmulation65816Opcodes)) emulation65816Opcodes.get(key).foreach(return _)
    if (options.flag(CompilationFlag.EmitNative65816Opcodes)) native65816Opcodes.get(key).foreach(return _)
    options.log.fatal("Cannot assemble an unknown opcode " + key)
  }

  private def op(op: Opcode.Value, am: AddrMode.Value, x: Int): Unit = {
    if (x < 0 || x > 0xff) FatalErrorReporting.reportFlyingPig("Invalid code for" + (op -> am))
    opcodes(op -> am) = x.toByte
    if (am == AddrMode.Relative) opcodes(op -> AddrMode.Immediate) = x.toByte
  }

  private def cm(op: Opcode.Value, am: AddrMode.Value, x: Int): Unit = {
    if (x < 0 || x > 0xff) FatalErrorReporting.reportFlyingPig("Invalid code for" + (op -> am))
    cmosOpcodes(op -> am) = x.toByte
  }

  private def cn(op: Opcode.Value, am: AddrMode.Value, x: Int): Unit = {
    if (x < 0 || x > 0xff) FatalErrorReporting.reportFlyingPig("Invalid code for" + (op -> am))
    cmosNopOpcodes(op -> am) = x.toByte
  }

  private def il(op: Opcode.Value, am: AddrMode.Value, x: Int): Unit = {
    if (x < 0 || x > 0xff) FatalErrorReporting.reportFlyingPig("Invalid code for" + (op -> am))
    illegalOpcodes(op -> am) = x.toByte
  }

  private def hu(op: Opcode.Value, am: AddrMode.Value, x: Int): Unit = {
    if (x < 0 || x > 0xff) FatalErrorReporting.reportFlyingPig("Invalid code for" + (op -> am))
    hudsonOpcodes(op -> am) = x.toByte
  }

  private def ce(op: Opcode.Value, am: AddrMode.Value, x: Int): Unit = {
    if (x < 0 || x > 0xff) FatalErrorReporting.reportFlyingPig("Invalid code for" + (op -> am))
    ce02Opcodes(op -> am) = x.toByte
  }

  private def em(op: Opcode.Value, am: AddrMode.Value, x: Int): Unit = {
    if (x < 0 || x > 0xff) FatalErrorReporting.reportFlyingPig("Invalid code for" + (op -> am))
    emulation65816Opcodes(op -> am) = x.toByte
  }

  private def na(op: Opcode.Value, am: AddrMode.Value, x: Int): Unit = {
    if (x < 0 || x > 0xff) FatalErrorReporting.reportFlyingPig("Invalid code for" + (op -> am))
    native65816Opcodes(op -> am) = x.toByte
  }

  def getStandardLegalOpcodes: Set[Int] = opcodes.values.map(_ & 0xff).toSet

  import AddrMode._
  import Opcode._

  op(ADC, Immediate, 0x69)
  op(ADC, ZeroPage, 0x65)
  op(ADC, ZeroPageX, 0x75)
  op(ADC, Absolute, 0x6D)
  op(ADC, AbsoluteX, 0x7D)
  op(ADC, AbsoluteY, 0x79)
  op(ADC, IndexedX, 0x61)
  op(ADC, IndexedY, 0x71)

  op(AND, Immediate, 0x29)
  op(AND, ZeroPage, 0x25)
  op(AND, ZeroPageX, 0x35)
  op(AND, Absolute, 0x2D)
  op(AND, AbsoluteX, 0x3D)
  op(AND, AbsoluteY, 0x39)
  op(AND, IndexedX, 0x21)
  op(AND, IndexedY, 0x31)

  op(ASL, Implied, 0x0A)
  op(ASL, ZeroPage, 0x06)
  op(ASL, ZeroPageX, 0x16)
  op(ASL, Absolute, 0x0E)
  op(ASL, AbsoluteX, 0x1E)

  op(BIT, ZeroPage, 0x24)
  op(BIT, Absolute, 0x2C)

  op(BPL, Relative, 0x10)
  op(BMI, Relative, 0x30)
  op(BVC, Relative, 0x50)
  op(BVS, Relative, 0x70)
  op(BCC, Relative, 0x90)
  op(BCS, Relative, 0xB0)
  op(BNE, Relative, 0xD0)
  op(BEQ, Relative, 0xF0)

  op(BRK, Implied, 0)

  op(CMP, Immediate, 0xC9)
  op(CMP, ZeroPage, 0xC5)
  op(CMP, ZeroPageX, 0xD5)
  op(CMP, Absolute, 0xCD)
  op(CMP, AbsoluteX, 0xDD)
  op(CMP, AbsoluteY, 0xD9)
  op(CMP, IndexedX, 0xC1)
  op(CMP, IndexedY, 0xD1)

  op(CPX, Immediate, 0xE0)
  op(CPX, ZeroPage, 0xE4)
  op(CPX, Absolute, 0xEC)

  op(CPY, Immediate, 0xC0)
  op(CPY, ZeroPage, 0xC4)
  op(CPY, Absolute, 0xCC)

  op(DEC, ZeroPage, 0xC6)
  op(DEC, ZeroPageX, 0xD6)
  op(DEC, Absolute, 0xCE)
  op(DEC, AbsoluteX, 0xDE)

  op(EOR, Immediate, 0x49)
  op(EOR, ZeroPage, 0x45)
  op(EOR, ZeroPageX, 0x55)
  op(EOR, Absolute, 0x4D)
  op(EOR, AbsoluteX, 0x5D)
  op(EOR, AbsoluteY, 0x59)
  op(EOR, IndexedX, 0x41)
  op(EOR, IndexedY, 0x51)

  op(INC, ZeroPage, 0xE6)
  op(INC, ZeroPageX, 0xF6)
  op(INC, Absolute, 0xEE)
  op(INC, AbsoluteX, 0xFE)

  op(CLC, Implied, 0x18)
  op(SEC, Implied, 0x38)
  op(CLI, Implied, 0x58)
  op(SEI, Implied, 0x78)
  op(CLV, Implied, 0xB8)
  op(CLD, Implied, 0xD8)
  op(SED, Implied, 0xF8)

  op(JMP, Absolute, 0x4C)
  op(JMP, Indirect, 0x6C)

  op(JSR, Absolute, 0x20)

  op(LDA, Immediate, 0xA9)
  op(LDA, ZeroPage, 0xA5)
  op(LDA, ZeroPageX, 0xB5)
  op(LDA, Absolute, 0xAD)
  op(LDA, AbsoluteX, 0xBD)
  op(LDA, AbsoluteY, 0xB9)
  op(LDA, IndexedX, 0xA1)
  op(LDA, IndexedY, 0xB1)

  op(LDX, Immediate, 0xA2)
  op(LDX, ZeroPage, 0xA6)
  op(LDX, ZeroPageY, 0xB6)
  op(LDX, Absolute, 0xAE)
  op(LDX, AbsoluteY, 0xBE)

  op(LDY, Immediate, 0xA0)
  op(LDY, ZeroPage, 0xA4)
  op(LDY, ZeroPageX, 0xB4)
  op(LDY, Absolute, 0xAC)
  op(LDY, AbsoluteX, 0xBC)

  op(LSR, Implied, 0x4A)
  op(LSR, ZeroPage, 0x46)
  op(LSR, ZeroPageX, 0x56)
  op(LSR, Absolute, 0x4E)
  op(LSR, AbsoluteX, 0x5E)

  op(NOP, Implied, 0xEA)

  op(ORA, Immediate, 0x09)
  op(ORA, ZeroPage, 0x05)
  op(ORA, ZeroPageX, 0x15)
  op(ORA, Absolute, 0x0D)
  op(ORA, AbsoluteX, 0x1D)
  op(ORA, AbsoluteY, 0x19)
  op(ORA, IndexedX, 0x01)
  op(ORA, IndexedY, 0x11)

  op(TAX, Implied, 0xAA)
  op(TXA, Implied, 0x8A)
  op(DEX, Implied, 0xCA)
  op(INX, Implied, 0xE8)
  op(TAY, Implied, 0xA8)
  op(TYA, Implied, 0x98)
  op(DEY, Implied, 0x88)
  op(INY, Implied, 0xC8)

  op(ROL, Implied, 0x2A)
  op(ROL, ZeroPage, 0x26)
  op(ROL, ZeroPageX, 0x36)
  op(ROL, Absolute, 0x2E)
  op(ROL, AbsoluteX, 0x3E)

  op(ROR, Implied, 0x6A)
  op(ROR, ZeroPage, 0x66)
  op(ROR, ZeroPageX, 0x76)
  op(ROR, Absolute, 0x6E)
  op(ROR, AbsoluteX, 0x7E)

  op(RTI, Implied, 0x40)
  op(RTS, Implied, 0x60)

  op(SBC, Immediate, 0xE9)
  op(SBC, ZeroPage, 0xE5)
  op(SBC, ZeroPageX, 0xF5)
  op(SBC, Absolute, 0xED)
  op(SBC, AbsoluteX, 0xFD)
  op(SBC, AbsoluteY, 0xF9)
  op(SBC, IndexedX, 0xE1)
  op(SBC, IndexedY, 0xF1)

  op(STA, ZeroPage, 0x85)
  op(STA, ZeroPageX, 0x95)
  op(STA, Absolute, 0x8D)
  op(STA, AbsoluteX, 0x9D)
  op(STA, AbsoluteY, 0x99)
  op(STA, IndexedX, 0x81)
  op(STA, IndexedY, 0x91)

  op(TXS, Implied, 0x9A)
  op(TSX, Implied, 0xBA)
  op(PHA, Implied, 0x48)
  op(PLA, Implied, 0x68)
  op(PHP, Implied, 0x08)
  op(PLP, Implied, 0x28)

  op(STX, ZeroPage, 0x86)
  op(STX, ZeroPageY, 0x96)
  op(STX, Absolute, 0x8E)

  op(STY, ZeroPage, 0x84)
  op(STY, ZeroPageX, 0x94)
  op(STY, Absolute, 0x8C)

  il(LAX, ZeroPage, 0xA7)
  il(LAX, ZeroPageY, 0xB7)
  il(LAX, Absolute, 0xAF)
  il(LAX, AbsoluteY, 0xBF)
  il(LAX, IndexedX, 0xA3)
  il(LAX, IndexedY, 0xB3)

  il(SAX, ZeroPage, 0x87)
  il(SAX, ZeroPageY, 0x97)
  il(SAX, Absolute, 0x8F)
  il(TAS, AbsoluteY, 0x9B)
  il(AHX, AbsoluteY, 0x9F)
  il(SAX, IndexedX, 0x83)
  il(AHX, IndexedY, 0x93)
  il(SHY, AbsoluteX, 0x9C)

  il(ANC, Immediate, 0x0B)
  il(ALR, Immediate, 0x4B)
  il(ARR, Immediate, 0x6B)
  il(XAA, Immediate, 0x8B)
  il(LXA, Immediate, 0xAB)
  il(SBX, Immediate, 0xCB)

  il(SLO, ZeroPage, 0x07)
  il(SLO, ZeroPageX, 0x17)
  il(SLO, IndexedX, 0x03)
  il(SLO, IndexedY, 0x13)
  il(SLO, Absolute, 0x0F)
  il(SLO, AbsoluteX, 0x1F)
  il(SLO, AbsoluteY, 0x1B)

  il(RLA, ZeroPage, 0x27)
  il(RLA, ZeroPageX, 0x37)
  il(RLA, IndexedX, 0x23)
  il(RLA, IndexedY, 0x33)
  il(RLA, Absolute, 0x2F)
  il(RLA, AbsoluteX, 0x3F)
  il(RLA, AbsoluteY, 0x3B)

  il(SRE, ZeroPage, 0x47)
  il(SRE, ZeroPageX, 0x57)
  il(SRE, IndexedX, 0x43)
  il(SRE, IndexedY, 0x53)
  il(SRE, Absolute, 0x4F)
  il(SRE, AbsoluteX, 0x5F)
  il(SRE, AbsoluteY, 0x5B)

  il(RRA, ZeroPage, 0x67)
  il(RRA, ZeroPageX, 0x77)
  il(RRA, IndexedX, 0x63)
  il(RRA, IndexedY, 0x73)
  il(RRA, Absolute, 0x6F)
  il(RRA, AbsoluteX, 0x7F)
  il(RRA, AbsoluteY, 0x7B)

  il(DCP, ZeroPage, 0xC7)
  il(DCP, ZeroPageX, 0xD7)
  il(DCP, IndexedX, 0xC3)
  il(DCP, IndexedY, 0xD3)
  il(DCP, Absolute, 0xCF)
  il(DCP, AbsoluteX, 0xDF)
  il(DCP, AbsoluteY, 0xDB)

  il(ISC, ZeroPage, 0xE7)
  il(ISC, ZeroPageX, 0xF7)
  il(ISC, IndexedX, 0xE3)
  il(ISC, IndexedY, 0xF3)
  il(ISC, Absolute, 0xEF)
  il(ISC, AbsoluteX, 0xFF)
  il(ISC, AbsoluteY, 0xFB)

  il(NOP, Immediate, 0x80)
  il(NOP, ZeroPage, 0x44)
  il(NOP, ZeroPageX, 0x54)
  il(NOP, Absolute, 0x5C)
  il(NOP, AbsoluteX, 0x1C)

  cn(NOP, Immediate, 0x02)
  cn(NOP, ZeroPage, 0x44)
  cn(NOP, ZeroPageX, 0x54)
  cn(NOP, Absolute, 0x5C)

  cm(STZ, ZeroPage, 0x64)
  cm(STZ, ZeroPageX, 0x74)
  cm(STZ, Absolute, 0x9C)
  cm(STZ, AbsoluteX, 0x9E)

  cm(PHX, Implied, 0xDA)
  cm(PHY, Implied, 0x5A)
  cm(PLX, Implied, 0xFA)
  cm(PLY, Implied, 0x7A)

  cm(ORA, IndexedZ, 0x12)
  cm(AND, IndexedZ, 0x32)
  cm(EOR, IndexedZ, 0x52)
  cm(ADC, IndexedZ, 0x72)
  cm(STA, IndexedZ, 0x92)
  cm(LDA, IndexedZ, 0xB2)
  cm(CMP, IndexedZ, 0xD2)
  cm(SBC, IndexedZ, 0xF2)

  cm(TSB, ZeroPage, 0x04)
  cm(TSB, Absolute, 0x0C)
  cm(TRB, ZeroPage, 0x14)
  cm(TRB, Absolute, 0x1C)

  cm(BRA, Relative, 0x80)
  cm(BIT, ZeroPageX, 0x34)
  cm(BIT, AbsoluteX, 0x3C)
  cm(INC, Implied, 0x1A)
  cm(DEC, Implied, 0x3A)
  cm(JMP, AbsoluteIndexedX, 0x7C)
  cm(WAI, Implied, 0xCB)
  cm(STP, Implied, 0xDB)

  ce(CPZ, Immediate, 0xC2)
  ce(CPZ, ZeroPage, 0xD4)
  ce(CPZ, Absolute, 0xDC)
  ce(DEZ, Implied, 0x3B)
  ce(INZ, Implied,0x1B )
  ce(DEC_W, ZeroPage, 0xC3)
  ce(INC_W, ZeroPage, 0xE3)
  ce(ASL_W, Absolute, 0xCB)
  // TODO: or is it ROL_W?
  ce(ROR_W, Absolute, 0xEB)
  ce(ASR, Implied, 0x43)
  ce(ASR, ZeroPage, 0x44)
  ce(ASR, ZeroPageX, 0x54)
  ce(LDZ, Immediate, 0xA3)
  ce(LDZ, Absolute, 0xAB)
  ce(LDZ, AbsoluteX, 0xBB)
  ce(TAB, Implied, 0x5B)
  ce(TBA, Implied, 0x7B)
  ce(TAZ, Implied, 0x4B)
  ce(TZA, Implied, 0x6B)
  ce(TSY, Implied, 0x0B)
  ce(TYS, Implied, 0x2B)
  ce(PHW, WordImmediate, 0xF4)
  ce(PHW, Absolute, 0xFC)
  ce(PHZ, Implied, 0xDB)
  ce(PLZ, Implied, 0xFB)
//  ce(CLE, Implied, )
//  ce(SEE, Implied, )
//  ce(BSR, , )

  hu(CLY, Implied, 0xC2)
  hu(CLX, Implied, 0x82)
  hu(CLA, Implied, 0x62)
  hu(CSH, Implied, 0xD4)
  hu(CSL, Implied, 0x54)
  hu(HuSAX, Implied, 0x22)
  hu(SAY, Implied, 0x42)
  hu(SXY, Implied, 0x02)
  hu(TAM, Immediate, 0x53)
  hu(TMA, Immediate, 0x43)

  em(ORA, Stack, 0x03)
  em(ORA, IndexedSY, 0x13)
  na(ORA, LongIndexedZ, 0x07)
  na(ORA, LongIndexedY, 0x17)
  na(ORA, LongAbsolute, 0x0F)
  na(ORA, LongAbsoluteX, 0x1F)
  em(AND, Stack, 0x23)
  em(AND, IndexedSY, 0x33)
  na(AND, LongIndexedZ, 0x27)
  na(AND, LongIndexedY, 0x37)
  na(AND, LongAbsolute, 0x2F)
  na(AND, LongAbsoluteX, 0x3F)
  em(EOR, Stack, 0x43)
  em(EOR, IndexedSY, 0x53)
  na(EOR, LongIndexedZ, 0x47)
  na(EOR, LongIndexedY, 0x57)
  na(EOR, LongAbsolute, 0x4F)
  na(EOR, LongAbsoluteX, 0x5F)
  em(ADC, Stack, 0x63)
  em(ADC, IndexedSY, 0x73)
  na(ADC, LongIndexedZ, 0x67)
  na(ADC, LongIndexedY, 0x77)
  na(ADC, LongAbsolute, 0x6F)
  na(ADC, LongAbsoluteX, 0x7F)
  em(STA, Stack, 0x83)
  em(STA, IndexedSY, 0x93)
  na(STA, LongIndexedZ, 0x87)
  na(STA, LongIndexedY, 0x97)
  na(STA, LongAbsolute, 0x8F)
  na(STA, LongAbsoluteX, 0x9F)
  em(LDA, Stack, 0xA3)
  em(LDA, IndexedSY, 0xB3)
  na(LDA, LongIndexedZ, 0xA7)
  na(LDA, LongIndexedY, 0xB7)
  na(LDA, LongAbsolute, 0xAF)
  na(LDA, LongAbsoluteX, 0xBF)
  em(CMP, Stack, 0xA3)
  em(CMP, IndexedSY, 0xB3)
  na(CMP, LongIndexedZ, 0xA7)
  na(CMP, LongIndexedY, 0xB7)
  na(CMP, LongAbsolute, 0xAF)
  na(CMP, LongAbsoluteX, 0xBF)

  em(COP, Immediate, 0x02)
  em(XBA, Implied, 0xEB)
  em(TXY, Implied, 0x9B)
  em(TYX, Implied, 0xBB)


  na(RTL, Implied, 0x6B)
  na(JMP, LongAbsolute, 0x5C)
  na(JMP, LongIndirect, 0x7C)
  na(BRL, LongRelative, 0x82)

  em(PHD, Implied, 0x0B)
  em(PLD, Implied, 0x2B)
  em(PHB, Implied, 0x8B)
  em(PLB, Implied, 0xAB)
  em(PHK, Implied, 0x4B)

  na(REP, Immediate, 0xC2)
  na(SEP, Immediate, 0xE2)

  na(XCE, Implied, 0xFB)
  na(TCD, Implied, 0x5B)
  na(TDC, Implied, 0x7B)
  na(TSC, Implied, 0x3B)
  na(TCS, Implied, 0x1B)

  for {
    ((narrow, am), code) <- emulation65816Opcodes ++ opcodes ++ cmosOpcodes ++ native65816Opcodes
    wide <- Opcode.widen(narrow)
  } na(wide, if (am == Immediate) WordImmediate else am, code & 0xff)

}
