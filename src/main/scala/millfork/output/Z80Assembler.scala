package millfork.output

import millfork.CompilationFlag.EmitIllegals
import millfork.assembly.OptimizationContext
import millfork.assembly.opt.{SingleStatus, Status}
import millfork.{CompilationFlag, CompilationOptions, Cpu, Platform}
import millfork.assembly.z80.{ZOpcode, _}
import millfork.assembly.z80.opt.{CoarseFlowAnalyzer, ConditionalInstructions, CpuStatus, JumpFollowing, JumpShortening}
import millfork.compiler.z80.Z80Compiler
import millfork.env._
import millfork.node.NiceFunctionProperty.DoesntWriteMemory
import millfork.node.Z80NiceFunctionProperty.{DoesntChangeA, DoesntChangeBC, DoesntChangeCF, DoesntChangeDE, DoesntChangeHL, DoesntChangeIY, SetsATo}
import millfork.node.{NiceFunctionProperty, Position, Program, ZRegister}

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
class Z80Assembler(program: Program,
                   rootEnv: Environment,
                   platform: Platform) extends AbstractAssembler[ZLine](program, rootEnv, platform, Z80InliningCalculator, Z80Compiler) {
  override def performFinalOptimizationPass(f: NormalFunction, actuallyOptimize: Boolean, options: CompilationOptions, code: List[ZLine]): List[ZLine] = {
    if (actuallyOptimize) {
      JumpShortening(f, ConditionalInstructions(options, JumpFollowing(options, code)), options)
    } else code
  }

  private def internalRegisterIndex(reg: ZRegister.Value): Int = reg match {
    case ZRegister.B => 0
    case ZRegister.C => 1
    case ZRegister.D => 2
    case ZRegister.E => 3
    case ZRegister.H => 4
    case ZRegister.L => 5
    case ZRegister.IXH => 4
    case ZRegister.IXL => 5
    case ZRegister.IYH => 4
    case ZRegister.IYL => 5
    case ZRegister.MEM_HL => 6
    case ZRegister.MEM_IX_D => 6
    case ZRegister.MEM_IY_D => 6
    case ZRegister.A => 7
    case ZRegister.BC => 0
    case ZRegister.DE => 1
    case ZRegister.HL => 2
    case ZRegister.AF => 3
    case ZRegister.SP => 3
  }

  private def prefixByte(reg: ZRegister.Value): Int = reg match {
    case ZRegister.IX | ZRegister.MEM_IX_D => 0xdd
    case ZRegister.IY | ZRegister.MEM_IY_D => 0xfd
    case ZRegister.IXH | ZRegister.IXL => 0xdd
    case ZRegister.IYH | ZRegister.IYL => 0xfd
  }

  override def emitInstruction(bank: String, options: CompilationOptions, index: Int, instr: ZLine): Int = {
    import millfork.assembly.z80.ZOpcode._
    import ZRegister._
    import Z80Assembler._
    import CompilationFlag._
    def requireIntel8080(): Unit = if (!options.flag(EmitIntel8080Opcodes)) log.error("Unsupported instruction: " + instr)

    def requireZ80(): Unit = if (!options.flag(EmitZ80Opcodes)) log.error("Unsupported instruction: " + instr)

    def requireZ80Illegals(): Unit = if (!options.flag(EmitZ80Opcodes) || !options.flag(EmitIllegals)) log.error("Unsupported instruction: " + instr)

    def requireExtended80(): Unit = if (!options.flag(EmitExtended80Opcodes)) log.error("Unsupported instruction: " + instr)

    def requireSharp(): Unit = if (!options.flag(EmitSharpOpcodes)) log.error("Unsupported instruction: " + instr)

    def requireEZ80(): Unit = if (!options.flag(EmitEZ80Opcodes)) log.error("Unsupported instruction: " + instr)

    def requireIntel8085(): Unit = if (!options.flag(EmitIntel8085Opcodes)) log.error("Unsupported instruction: " + instr)

    def requireIntel8085Illegals(): Unit = if (!options.flag(EmitIntel8085Opcodes) || !options.flag(EmitIllegals)) log.error("Unsupported instruction: " + instr)

    def requireNext(): Unit = if (!options.flag(EmitZ80NextOpcodes)) log.error("Unsupported instruction: " + instr)

    def useSharpOpcodes():Boolean = {
      if (!options.flag(EmitSharpOpcodes) && !options.flag(EmitIntel8080Opcodes))
        log.error("Cannot determine which variant to emit : " + instr)
      options.flag(EmitSharpOpcodes)
    }

    implicit val position = instr.source.map(sl => Position(sl.moduleName, sl.line, 0, 0))
    try { instr match {
      case ZLine0(LABEL, NoRegisters, MemoryAddressConstant(Label(labelName))) =>
        val bank0 = mem.banks(bank)
        labelMap(labelName) = bank0.index -> index
        index
      case ZLine0(BYTE, NoRegisters, param) =>
        writeByte(bank, index, param)
        index + 1
      case ZLine0(CHANGED_MEM, NoRegisters, MemoryAddressConstant(Label(l))) if l.contains("..brk") =>
        breakpointSet += mem.banks(bank).index -> index
        index
      case ZLine0(DISCARD_F | DISCARD_HL | DISCARD_BC | DISCARD_DE | DISCARD_IX | DISCARD_IY | DISCARD_A | CHANGED_MEM, NoRegisters, _) =>
        index
      case ZLine0(LABEL | BYTE | DISCARD_F | DISCARD_HL | DISCARD_BC | DISCARD_DE | DISCARD_IX | DISCARD_IY | DISCARD_A | CHANGED_MEM, _, _) =>
        ???
      case ZLine0(RIM, NoRegisters, _) =>
        requireIntel8085()
        writeByte(bank, index, 0x20)
        index + 1
      case ZLine0(SIM, NoRegisters, _) =>
        requireIntel8085()
        writeByte(bank, index, 0x30)
        index + 1
      case ZLine0(RST, NoRegisters, param) =>
        val opcode = param.quickSimplify match {
          case NumericConstant(n, _) if n >=0 && n <= 0x38 && n % 8 == 0 => 0xc7 + n.toInt
          case _ => log.error("Invalid param for RST"); 0xc7
        }
        writeByte(bank, index, opcode)
        index + 1
      case ZLine0(IM, NoRegisters, param) =>
        requireZ80()
        val opcode = param.quickSimplify match {
          case NumericConstant(0, _) => 0x46
          case NumericConstant(1, _) => 0x56
          case NumericConstant(2, _) => 0x5e
          case _ => log.error("Invalid param for IM"); 0xc7
        }
        writeByte(bank, index, 0xED)
        writeByte(bank, index + 1, opcode)
        index + 2
      case ZLine0(op, OneRegister(reg), _) if ZOpcodeClasses.AllSingleBit(op) =>
        requireExtended80()
        writeByte(bank, index, 0xcb)
        writeByte(bank, index + 1, ZOpcodeClasses.singleBitOpcode(op) + internalRegisterIndex(reg))
        index + 2
      case ZLine0(op, NoRegisters, _) if implieds.contains(op) =>
        writeByte(bank, index, implieds(op))
        index + 1
      case ZLine0(EXX, NoRegisters, _)=>
        requireZ80()
        writeByte(bank, index, 0xd9)
        index + 1
      case ZLine0(EX_AF_AF, NoRegisters, _)=>
        requireZ80()
        writeByte(bank, index, 8)
        index + 1
      case ZLine0(RETI, NoRegisters, _) =>
        requireExtended80()
        if (useSharpOpcodes()) {
          writeByte(bank, index, 0xd9)
          index + 1
        } else {
          writeByte(bank, index, 0xed)
          writeByte(bank, index + 1, 0x4d)
          index + 2
        }
      case ZLine0(op, NoRegisters, _) if edImplieds.contains(op) =>
        requireZ80()
        writeByte(bank, index, 0xed)
        writeByte(bank, index + 1, edImplieds(op))
        index + 2
      case ZLine0(op, NoRegisters, _) if nextEdImplieds.contains(op) =>
        requireNext()
        writeByte(bank, index, 0xed)
        writeByte(bank, index + 1, nextEdImplieds(op))
        index + 2
      case ZLine0(ADD_16, TwoRegisters(r@(ZRegister.HL | ZRegister.BC | ZRegister.DE), ZRegister.A), _) =>
        requireNext()
        writeByte(bank, index, 0xed)
        writeByte(bank, index + 1, 0x33 - internalRegisterIndex(r))
        index + 2
      case ZLine0(ADD_16, TwoRegisters(r@(ZRegister.HL | ZRegister.BC | ZRegister.DE), ZRegister.IMM_16), nn) =>
        requireNext()
        writeByte(bank, index, 0xed)
        writeByte(bank, index + 1, 0x36 - internalRegisterIndex(r))
        writeWord(bank, index + 2, nn)
        index + 4
      case ZLine0(PUSH, OneRegister(ZRegister.IMM_16), nn) =>
        requireNext()
        writeByte(bank, index, 0xed)
        writeByte(bank, index + 1, 0x8A)
        writeByte(bank, index + 2, nn.hiByte)
        writeByte(bank, index + 3, nn.loByte)
        index + 4
      case ZLine0(TEST, OneRegister(ZRegister.IMM_8), nn) =>
        requireNext()
        writeByte(bank, index, 0xed)
        writeByte(bank, index + 1, 0x27)
        writeByte(bank, index + 2, nn)
        index + 3
      case ZLine0(NEXTREG, TwoRegisters(ZRegister.IMM_8, ZRegister.IMM_8), nn) =>
        requireNext()
        writeByte(bank, index, 0xed)
        writeByte(bank, index + 1, 0x91)
        writeWord(bank, index + 2, nn)
        index + 4
      case ZLine0(NEXTREG, TwoRegisters(ZRegister.IMM_8, ZRegister.A), nn) =>
        requireNext()
        writeByte(bank, index, 0xed)
        writeByte(bank, index + 1, 0x92)
        writeByte(bank, index + 2, nn)
        index + 3
      case ZLine0(ADD_16, TwoRegisters(ZRegister.HL, source), _) =>
        writeByte(bank, index, 9 + 16 * internalRegisterIndex(source))
        index + 1
      case ZLine0(ADD_16, TwoRegisters(ix@(ZRegister.IX | ZRegister.IY), source@(ZRegister.IX | ZRegister.IY)), _)=>
        requireZ80()
        if (ix == source) {
          writeByte(bank, index, prefixByte(ix))
          writeByte(bank, index + 1, 9 + 16 * internalRegisterIndex(HL))
          index + 2
        } else {
          log.fatal("Cannot assemble " + instr)
          index
        }
      case ZLine0(ADD_16, TwoRegisters(ix@(ZRegister.IX | ZRegister.IY), source), _) =>
        requireZ80()
        writeByte(bank, index, prefixByte(ix))
        writeByte(bank, index + 1, 9 + 16 * internalRegisterIndex(source))
        index + 2
      case ZLine0(ADC_16, TwoRegisters(ZRegister.HL, reg), _) =>
        requireZ80()
        writeByte(bank, index, 0xed)
        writeByte(bank, index + 1, 0x4a + 0x10 * internalRegisterIndex(reg))
        index + 2
      case ZLine0(SBC_16, TwoRegisters(ZRegister.HL, reg), _) =>
        requireZ80()
        writeByte(bank, index, 0xed)
        writeByte(bank, index + 1, 0x42 + 0x10 * internalRegisterIndex(reg))
        index + 2
      case ZLine0(LD_16, TwoRegisters(ix@(ZRegister.IX | ZRegister.IY), ZRegister.IMM_16), param) =>
        requireZ80()
        writeByte(bank, index, prefixByte(ix))
        writeByte(bank, index + 1, 0x21)
        writeWord(bank, index + 2, param)
        index + 4
      case ZLine0(LD_16, TwoRegisters(target, ZRegister.IMM_16), param) =>
        writeByte(bank, index, 1 + 16 * internalRegisterIndex(target))
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine0(LD_16, TwoRegisters(ix@(ZRegister.IX | ZRegister.IY), ZRegister.MEM_ABS_16), param) =>
        requireZ80()
        writeByte(bank, index, prefixByte(ix))
        writeByte(bank, index + 1, 0x2a)
        writeWord(bank, index + 2, param)
        index + 4
      case ZLine0(LD_16, TwoRegisters(ZRegister.MEM_ABS_16, ix@(ZRegister.IX | ZRegister.IY)), param) =>
        requireZ80()
        writeByte(bank, index, prefixByte(ix))
        writeByte(bank, index + 1, 0x22)
        writeWord(bank, index + 2, param)
        index + 4
      case ZLine0(LD_16, TwoRegisters(ZRegister.HL, ZRegister.MEM_ABS_16), param) =>
        requireIntel8080()
        writeByte(bank, index, 0x2a)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine0(LD_16, TwoRegisters(ZRegister.MEM_ABS_16, ZRegister.HL), param) =>
        requireIntel8080()
        writeByte(bank, index, 0x22)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine0(LD_16, TwoRegisters(reg@(ZRegister.BC | ZRegister.DE | ZRegister.SP), ZRegister.MEM_ABS_16), param) =>
        requireZ80()
        writeByte(bank, index, 0xed)
        writeByte(bank, index+1, 0x4b + 0x10 * internalRegisterIndex(reg))
        writeWord(bank, index + 2, param)
        index + 4
      case ZLine0(LD_16, TwoRegisters(ZRegister.MEM_ABS_16, ZRegister.SP), param) =>
        requireExtended80()
        if (useSharpOpcodes()) {
          writeByte(bank, index, 8)
          writeWord(bank, index + 1, param)
          index + 3
        } else {
          writeByte(bank, index, 0xed)
          writeByte(bank, index + 1, 0x43 + 0x10 * internalRegisterIndex(SP))
          writeWord(bank, index + 2, param)
          index + 4
        }
      case ZLine0(LD_16, TwoRegisters(ZRegister.MEM_ABS_16, reg@(ZRegister.BC | ZRegister.DE)), param) =>
        requireZ80()
        writeByte(bank, index, 0xed)
        writeByte(bank, index+1, 0x43 + 0x10 * internalRegisterIndex(reg))
        writeWord(bank, index + 2, param)
        index + 4
      case ZLine0(LD_16, TwoRegisters(ZRegister.SP, ZRegister.HL), _) =>
        writeByte(bank, index, 0xF9)
        index + 1
      case ZLine0(LD_16, TwoRegisters(ZRegister.SP, ix@(ZRegister.IX | ZRegister.IY)), _) =>
        requireZ80()
        writeByte(bank, index, prefixByte(ix))
        writeByte(bank, index + 1, 0xF9)
        index + 2
      case ZLine0(SRA, OneRegister(HL), _) =>
        requireIntel8085Illegals()
        writeByte(bank, index, 0x10)
        index + 1
      case ZLine0(RL, OneRegister(DE), _) =>
        requireIntel8085Illegals()
        writeByte(bank, index, 0x18)
        index + 1
      case ZLine0(op, OneRegister(ZRegister.IMM_8), param) if immediates.contains(op) =>
        val o = immediates(op)
        writeByte(bank, index, o)
        writeByte(bank, index + 1, param)
        index + 2
      case ZLine0(op, OneRegisterOffset(ix@(ZRegister.MEM_IX_D | ZRegister.MEM_IY_D), offset), _) if ZOpcodeClasses.AllSingleBit(op) =>
        requireZ80()
        writeByte(bank, index, prefixByte(ix))
        writeByte(bank, index + 1, 0xcb)
        writeByte(bank, index + 2, offset)
        writeByte(bank, index + 3, ZOpcodeClasses.singleBitOpcode(op) + internalRegisterIndex(ZRegister.MEM_HL))
        index + 4
      case ZLine0(op, OneRegisterOffset(ix@(ZRegister.MEM_IX_D | ZRegister.MEM_IY_D), offset), _) if oneRegister.contains(op) =>
        requireZ80()
        val o = oneRegister(op)
        writeByte(bank, index, prefixByte(ix))
        writeByte(bank, index + 1, o.opcode + internalRegisterIndex(ZRegister.MEM_HL) * o.multiplier)
        writeByte(bank, index + 2, offset)
        index + 3
      case ZLine0(op, OneRegister(ix@(ZRegister.IX | ZRegister.IY)), _) if oneRegister.contains(op) =>
        requireZ80()
        val o = oneRegister(op)
        writeByte(bank, index, prefixByte(ix))
        writeByte(bank, index + 1, o.opcode + internalRegisterIndex(ZRegister.HL) * o.multiplier)
        writeByte(bank, index + 2, instr.parameter)
        index + 3
      case ZLine0(op, OneRegister(ix@(IXH | IYH | IXL | IYL)), _) if oneRegister.contains(op) =>
        requireZ80Illegals()
        val o = oneRegister(op)
        writeByte(bank, index, prefixByte(ix))
        writeByte(bank, index + 1, o.opcode + internalRegisterIndex(ix) * o.multiplier)
        index + 2
      case ZLine0(op, OneRegister(reg), _) if reg != ZRegister.IMM_8 && oneRegister.contains(op) =>
        val o = oneRegister(op)
        writeByte(bank, index, o.opcode + internalRegisterIndex(reg) * o.multiplier)
        index + 1
      case ZLine0(SLL, OneRegister(reg), _) =>
        requireZ80Illegals()
        writeByte(bank, index, 0xcb)
        writeByte(bank, index + 1, 0x30 + internalRegisterIndex(reg))
        index + 2
      case ZLine0(SWAP, OneRegister(reg), _) =>
        requireSharp()
        writeByte(bank, index, 0xcb)
        writeByte(bank, index + 1, 0x30 + internalRegisterIndex(reg))
        index + 2
      case ZLine0(SLL, OneRegisterOffset(ix@(ZRegister.MEM_IX_D | ZRegister.MEM_IY_D), offset), _) =>
        requireZ80Illegals()
        writeByte(bank, index, prefixByte(ix))
        writeByte(bank, index + 1, 0xcb)
        writeByte(bank, index + 2, offset)
        writeByte(bank, index + 3, 0x30 + internalRegisterIndex(MEM_HL))
        index + 4
      case ZLine0(op, OneRegister(reg), _) if cbOneRegister.contains(op) =>
        requireExtended80()
        val o = cbOneRegister(op)
        writeByte(bank, index, 0xcb)
        writeByte(bank, index + 1, o.opcode + internalRegisterIndex(reg) * o.multiplier)
        index + 2
      case ZLine0(op, OneRegisterOffset(ix@(ZRegister.MEM_IX_D | ZRegister.MEM_IY_D), offset), _) if cbOneRegister.contains(op) =>
        requireZ80()
        val o = cbOneRegister(op)
        writeByte(bank, index, prefixByte(ix))
        writeByte(bank, index + 1, 0xcb)
        writeByte(bank, index + 2, offset)
        writeByte(bank, index + 3, o.opcode + internalRegisterIndex(MEM_HL) * o.multiplier)
        index + 4
      case ZLine0(LD, registers, _) =>
        registers match {
          case TwoRegisters(I, A) =>
            requireZ80()
            writeByte(bank, index, 0xed)
            writeByte(bank, index + 1, 0x47)
            index + 2
          case TwoRegisters(A, I) =>
            requireZ80()
            writeByte(bank, index, 0xed)
            writeByte(bank, index + 1, 0x57)
            index + 2
          case TwoRegisters(R, A) =>
            requireZ80()
            writeByte(bank, index, 0xed)
            writeByte(bank, index + 1, 0x4f)
            index + 2
          case TwoRegisters(A, R) =>
            requireZ80()
            writeByte(bank, index, 0xed)
            writeByte(bank, index + 1, 0x5f)
            index + 2
          case TwoRegisters(I | R, _) | TwoRegisters(_, I | R) =>
            log.fatal("Cannot assemble " + instr)
            index
          case TwoRegisters(reg, ZRegister.IMM_8) =>
            writeByte(bank, index, 6 + 8 * internalRegisterIndex(reg))
            writeByte(bank, index + 1, instr.parameter)
            index + 2
          case TwoRegistersOffset(ix@(ZRegister.MEM_IX_D | ZRegister.MEM_IY_D), ZRegister.IMM_8, offset) =>
            requireZ80()
            writeByte(bank, index, prefixByte(ix))
            writeByte(bank, index + 1, 0x36)
            writeByte(bank, index + 2, offset)
            writeByte(bank, index + 3, instr.parameter)
            index + 4
          case TwoRegisters(ZRegister.A, ZRegister.MEM_ABS_8) =>
            if (useSharpOpcodes()) {
              writeByte(bank, index, 0xfa)
            } else {
              writeByte(bank, index, 0x3a)
            }
            writeWord(bank, index + 1, instr.parameter)
            index + 3
          case TwoRegisters(ZRegister.MEM_ABS_8, ZRegister.A) =>
            if (useSharpOpcodes()) {
              writeByte(bank, index, 0xea)
            } else {
              writeByte(bank, index, 0x32)
            }
            writeWord(bank, index + 1, instr.parameter)
            index + 3
          case TwoRegisters(ZRegister.MEM_BC, ZRegister.A) =>
            writeByte(bank, index, 2)
            index + 1
          case TwoRegisters(ZRegister.MEM_DE, ZRegister.A) =>
            writeByte(bank, index, 0x12)
            index + 1
          case TwoRegisters(ZRegister.A, ZRegister.MEM_BC) =>
            writeByte(bank, index, 0x0a)
            index + 1
          case TwoRegisters(ZRegister.A, ZRegister.MEM_DE) =>
            writeByte(bank, index, 0x1a)
            index + 1
          case TwoRegistersOffset(ix@(ZRegister.MEM_IX_D | ZRegister.MEM_IY_D), source, offset) =>
            requireZ80()
            writeByte(bank, index, prefixByte(ix))
            writeByte(bank, index + 1, 0x40 + internalRegisterIndex(source) + internalRegisterIndex(ZRegister.MEM_HL) * 8)
            writeByte(bank, index + 2, offset)
            index + 3
          case TwoRegistersOffset(target, ix@(ZRegister.MEM_IX_D | ZRegister.MEM_IY_D), offset) =>
            requireZ80()
            writeByte(bank, index, prefixByte(ix))
            writeByte(bank, index + 1, 0x40 + internalRegisterIndex(ZRegister.MEM_HL) + internalRegisterIndex(target) * 8)
            writeByte(bank, index + 2, offset)
            index + 3
          case TwoRegisters(target@(IXH | IYH | IXL | IYL), source@(A | B | C | D | E)) =>
            requireZ80Illegals()
            writeByte(bank, index, prefixByte(target))
            writeByte(bank, index, 0x40 + internalRegisterIndex(source) + internalRegisterIndex(target) * 8)
            index + 2
          case TwoRegisters(target@(A | B | C | D | E), source@(IXH | IYH | IXL | IYL)) =>
            requireZ80Illegals()
            writeByte(bank, index, prefixByte(source))
            writeByte(bank, index, 0x40 + internalRegisterIndex(source) + internalRegisterIndex(target) * 8)
            index + 2
          case TwoRegisters(target@(IXH | IXL), source@(IXH | IXL)) =>
            requireZ80Illegals()
            writeByte(bank, index, prefixByte(source))
            writeByte(bank, index, 0x40 + internalRegisterIndex(source) + internalRegisterIndex(target) * 8)
            index + 2
          case TwoRegisters(target@(IYH | IYL), source@(IYH | IYL)) =>
            requireZ80Illegals()
            writeByte(bank, index, prefixByte(source))
            writeByte(bank, index, 0x40 + internalRegisterIndex(source) + internalRegisterIndex(target) * 8)
            index + 2
          case TwoRegisters(target@(H | L), source@(H | L)) =>
            writeByte(bank, index, 0x40 + internalRegisterIndex(source) + internalRegisterIndex(target) * 8)
            index + 1
          case TwoRegisters(target@(IXH | IYH | IXL | IYL | H | L), source@(IXH | IYH | IXL | IYL | H | L)) =>
            log.error("Cannot assemble " + instr)
            writeByte(bank, index, 0x40 + internalRegisterIndex(source) + internalRegisterIndex(target) * 8)
            index + 1
          case TwoRegisters(target, source) =>
            writeByte(bank, index, 0x40 + internalRegisterIndex(source) + internalRegisterIndex(target) * 8)
            index + 1
          case TwoRegisters(target, source) =>
            writeByte(bank, index, 0x40 + internalRegisterIndex(source) + internalRegisterIndex(target) * 8)
            index + 1
        }
      case ZLine0(IN_IMM, OneRegister(ZRegister.A), param) =>
        requireIntel8080()
        writeByte(bank, index, 0xdb)
        writeByte(bank, index + 1, param)
        index + 2
      case ZLine0(IN_C, OneRegister(reg), param) =>
        requireZ80()
        writeByte(bank, index, 0xed)
        writeByte(bank, index + 1, 0x40 + 8 * internalRegisterIndex(reg))
        index + 2
      case ZLine0(OUT_IMM, OneRegister(ZRegister.A), param) =>
        requireIntel8080()
        writeByte(bank, index, 0xd3)
        writeByte(bank, index + 1, param)
        index + 2
      case ZLine0(OUT_C, OneRegister(reg), param) =>
        requireZ80()
        writeByte(bank, index, 0xed)
        writeByte(bank, index + 1, 0x41 + 8 * internalRegisterIndex(reg))
        index + 2

      case ZLine0(CALL, NoRegisters, param) =>
        writeByte(bank, index, 0xcd)
        writeWord(bank, index + 1, param)
        index + 3



      case ZLine0(CALL, IfFlagClear(ZFlag.Z), param) =>
        writeByte(bank, index, 0xc4)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine0(CALL, IfFlagClear(ZFlag.C), param) =>
        writeByte(bank, index, 0xd4)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine0(CALL, IfFlagClear(ZFlag.P), param) =>
        requireIntel8080()
        writeByte(bank, index, 0xe4)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine0(CALL, IfFlagClear(ZFlag.S), param) =>
        requireIntel8080()
        writeByte(bank, index, 0xf4)
        writeWord(bank, index + 1, param)
        index + 3

      case ZLine0(CALL, IfFlagSet(ZFlag.Z), param) =>
        writeByte(bank, index, 0xcc)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine0(CALL, IfFlagSet(ZFlag.C), param) =>
        writeByte(bank, index, 0xdc)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine0(CALL, IfFlagSet(ZFlag.P), param) =>
        requireIntel8080()
        writeByte(bank, index, 0xec)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine0(CALL, IfFlagSet(ZFlag.S), param) =>
        requireIntel8080()
        writeByte(bank, index, 0xfc)
        writeWord(bank, index + 1, param)
        index + 3

      case ZLine0(JP, NoRegisters, param) =>
        writeByte(bank, index, 0xc3)
        writeWord(bank, index + 1, param)
        index + 3

      case ZLine0(JP, IfFlagClear(ZFlag.Z), param) =>
        writeByte(bank, index, 0xc2)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine0(JP, IfFlagClear(ZFlag.C), param) =>
        writeByte(bank, index, 0xd2)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine0(JP, IfFlagClear(ZFlag.P), param) =>
        requireIntel8080()
        writeByte(bank, index, 0xe2)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine0(JP, IfFlagClear(ZFlag.S), param) =>
        requireIntel8080()
        writeByte(bank, index, 0xf2)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine0(JP, IfFlagClear(ZFlag.K), param) =>
        requireIntel8085Illegals()
        writeByte(bank, index, 0xdd)
        writeWord(bank, index + 1, param)
        index + 3

      case ZLine0(JP, IfFlagSet(ZFlag.Z), param) =>
        writeByte(bank, index, 0xca)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine0(JP, IfFlagSet(ZFlag.C), param) =>
        writeByte(bank, index, 0xda)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine0(JP, IfFlagSet(ZFlag.P), param) =>
        requireIntel8080()
        writeByte(bank, index, 0xea)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine0(JP, IfFlagSet(ZFlag.S), param) =>
        requireIntel8080()
        writeByte(bank, index, 0xfa)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine0(JP, IfFlagSet(ZFlag.K), param) =>
        requireIntel8085Illegals()
        writeByte(bank, index, 0xfd)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine0(JP, OneRegister(HL), _) =>
        writeByte(bank, index, 0xe9)
        index + 1
      case ZLine0(JP, OneRegister(IX), _) =>
        requireZ80()
        writeByte(bank, index, 0xdd)
        writeByte(bank, index, 0xe9)
        index + 2
      case ZLine0(JP, OneRegister(IY), _) =>
        requireZ80()
        writeByte(bank, index, 0xfd)
        writeByte(bank, index, 0xe9)
        index + 2

      case ZLine0(RET, IfFlagClear(ZFlag.Z), _) =>
        writeByte(bank, index, 0xc0)
        index + 1
      case ZLine0(RET, IfFlagClear(ZFlag.C), _) =>
        writeByte(bank, index, 0xd0)
        index + 1
      case ZLine0(RET, IfFlagClear(ZFlag.P), _) =>
        requireIntel8080()
        writeByte(bank, index, 0xe0)
        index + 1
      case ZLine0(RET, IfFlagClear(ZFlag.S), _) =>
        requireIntel8080()
        writeByte(bank, index, 0xf0)
        index + 1

      case ZLine0(RET, IfFlagSet(ZFlag.Z), _) =>
        writeByte(bank, index, 0xc8)
        index + 1
      case ZLine0(RET, IfFlagSet(ZFlag.C), _) =>
        writeByte(bank, index, 0xd8)
        index + 1
      case ZLine0(RET, IfFlagSet(ZFlag.P), _) =>
        requireIntel8080()
        writeByte(bank, index, 0xe8)
        index + 1
      case ZLine0(RET, IfFlagSet(ZFlag.S), _) =>
        requireIntel8080()
        writeByte(bank, index, 0xf8)
        index + 1

      case ZLine0(JR, IfFlagClear(ZFlag.Z), param) =>
        requireExtended80()
        writeByte(bank, index, 0x20)
        writeByte(bank, index + 1, AssertByte(param - index - 2))
        index + 2
      case ZLine0(JR, IfFlagClear(ZFlag.C), param) =>
        requireExtended80()
        writeByte(bank, index, 0x30)
        writeByte(bank, index + 1, AssertByte(param - index - 2))
        index + 2

      case ZLine0(JR, IfFlagSet(ZFlag.Z), param) =>
        requireExtended80()
        writeByte(bank, index, 0x28)
        writeByte(bank, index + 1, AssertByte(param - index - 2))
        index + 2
      case ZLine0(JR, IfFlagSet(ZFlag.C), param) =>
        requireExtended80()
        writeByte(bank, index, 0x38)
        writeByte(bank, index + 1, AssertByte(param - index - 2))
        index + 2

      case ZLine0(JR, NoRegisters, param) =>
        requireExtended80()
        writeByte(bank, index, 0x18)
        writeByte(bank, index + 1, AssertByte(param - index - 2))
        index + 2
      case ZLine0(DJNZ, NoRegisters, param) =>
        requireZ80()
        writeByte(bank, index, 0x10)
        writeByte(bank, index + 1, AssertByte(param - index - 2))
        index + 2
      case ZLine0(EX_SP, OneRegister(HL), _) =>
        requireIntel8080()
        writeByte(bank, index, 0xe3)
        index + 1
      case ZLine0(EX_SP, OneRegister(IX), _) =>
        requireZ80()
        writeByte(bank, index, 0xdd)
        writeByte(bank, index + 1, 0xe3)
        index + 2
      case ZLine0(EX_SP, OneRegister(IY), _) =>
        requireZ80()
        writeByte(bank, index, 0xfd)
        writeByte(bank, index + 1, 0xe3)
        index + 2
      case ZLine0(EX_DE_HL, _, _) =>
        requireIntel8080()
        writeByte(bank, index, 0xeb)
        index + 1

      case ZLine0(ADD_SP, _, param) =>
        requireSharp()
        writeByte(bank, index, 0xe8)
        writeByte(bank, index + 1, param)
        index + 2
      case ZLine0(LD_HLSP, _, param) =>
        requireSharp()
        writeByte(bank, index, 0xf8)
        writeByte(bank, index + 1, param)
        index + 2
      case ZLine0(LD_AHLI, _, _) =>
        requireSharp()
        writeByte(bank, index, 0x2a)
        index + 1
      case ZLine0(LD_AHLD, _, _) =>
        requireSharp()
        writeByte(bank, index, 0x3a)
        index + 1
      case ZLine0(LD_HLIA, _, _) =>
        requireSharp()
        writeByte(bank, index, 0x22)
        index + 1
      case ZLine0(LD_HLDA, _, _) =>
        requireSharp()
        writeByte(bank, index, 0x32)
        index + 1
      case ZLine0(LDH_AD, _, param) =>
        requireSharp()
        writeByte(bank, index, 0xf0)
        writeByte(bank, index + 1, param.loByte)
        index + 2
      case ZLine0(LDH_DA, _, param) =>
        requireSharp()
        writeByte(bank, index, 0xe0)
        writeByte(bank, index + 1, param.loByte)
        index + 2
      case ZLine0(LDH_AC, _, _) =>
        requireSharp()
        writeByte(bank, index, 0xf2)
        index + 1
      case ZLine0(LDH_CA, _, _) =>
        requireSharp()
        writeByte(bank, index, 0xe2)
        index + 1
      case ZLine0(STOP, _, _) =>
        requireSharp()
        writeByte(bank, index, 0x10)
        index + 1
      case ZLine0(DSUB, _, _) =>
        requireIntel8085Illegals()
        writeByte(bank, index, 0x08)
        index + 1
      case ZLine0(LD_DEHL, _, param) =>
        requireIntel8085Illegals()
        writeByte(bank, index, 0x28)
        writeByte(bank, index + 1, param)
        index + 2
      case ZLine0(LD_DESP, _, param) =>
        requireIntel8085Illegals()
        writeByte(bank, index, 0x38)
        writeByte(bank, index + 1, param)
        index + 2
      case ZLine0(RSTV, _, _) =>
        requireIntel8085Illegals()
        writeByte(bank, index, 0xcb)
        index + 1
      case ZLine0(SHLX, _, _) =>
        requireIntel8085Illegals()
        writeByte(bank, index, 0xd9)
        index + 1
      case ZLine0(LHLX, _, _) =>
        requireIntel8085Illegals()
        writeByte(bank, index, 0xed)
        index + 1
      case ZLine0(RLDE, _, _) =>
        requireIntel8085Illegals()
        writeByte(bank, index, 0x18)
        index + 1
      case ZLine0(RRHL, _, _) =>
        requireIntel8085Illegals()
        writeByte(bank, index, 0x10)
        index + 1
      case _ =>
        log.fatal("Cannot assemble " + instr)
        index
    } } catch {
      case _ : MatchError =>
        log.fatal("Cannot assemble " + instr)
        index
    }
  }

  override def injectLabels(labelMap: Map[String, (Int, Int)], code: List[ZLine]): List[ZLine] = code // TODO

  override def quickSimplify(code: List[ZLine]): List[ZLine] = code.map(a => a.copy(parameter = a.parameter.quickSimplify))

  override def gatherNiceFunctionProperties(options: CompilationOptions, niceFunctionProperties: mutable.Set[(NiceFunctionProperty, String)], function: NormalFunction, code: List[ZLine]): Unit = {
    import ZOpcode._
    import NiceFunctionProperty._
    val functionName = function.name
    if (isNaughty(code)) return
    val localLabels = code.flatMap {
      case ZLine0(LABEL, _, MemoryAddressConstant(Label(l))) => Some(l)
      case _ => None
    }.toSet
    if (code.exists {
      case ZLine0(JP | JR, _, MemoryAddressConstant(Label(l))) => !localLabels(l)
      case ZLine0(JP | JR, _, _) => true
      case ZLine0(RST, _, _) => true
      case _ => false
    }) return
    val optimizationContext = OptimizationContext(options, Map(), None, Set())
    val flow = CoarseFlowAnalyzer.analyze(function, code, optimizationContext)
    def retPropertyScan[T](extractor: CpuStatus => Status[T])(niceFunctionProperty: Status[T] => Option[NiceFunctionProperty]): Unit = {
      val statuses = code.zipWithIndex.flatMap{
        case (ZLine0(RET | RETI | RETN, _, _), ix) => Some(extractor(flow(ix)))
        case _ => None
      }.toSet
      statuses.toSeq match {
        case Seq(only) =>
          niceFunctionProperty(only).foreach { np =>
            niceFunctionProperties += (np -> functionName)
          }
        case _ =>
      }
    }
    def simpleRetPropertyScan[T](extractor: CpuStatus => Status[T])(niceFunctionProperty: T => NiceFunctionProperty): Unit = {
      retPropertyScan(extractor) {
        case SingleStatus(x) => Some(niceFunctionProperty(x))
        case _ => None
      }
    }
    def genericPropertyScan(niceFunctionProperty: NiceFunctionProperty)(predicate: ZLine => Boolean): Unit = {
      val preserved = code.forall {
        case ZLine0(JP | JR, _, MemoryAddressConstant(Label(label))) => localLabels(label)
        case ZLine0(CALL | JP | JR, _, MemoryAddressConstant(th)) => niceFunctionProperties(niceFunctionProperty -> th.name)
        case ZLine0(CALL | JP | JR, _, _) => false
        case l => predicate(l)
      }
      if (preserved) {
        niceFunctionProperties += (niceFunctionProperty -> functionName)
      }
    }
    genericPropertyScan(DoesntChangeA)(l => !l.changesRegister(ZRegister.A))
    genericPropertyScan(DoesntChangeHL)(l => !l.changesRegister(ZRegister.HL))
    genericPropertyScan(DoesntChangeDE)(l => !l.changesRegister(ZRegister.DE))
    genericPropertyScan(DoesntChangeBC)(l => !l.changesRegister(ZRegister.BC))
    genericPropertyScan(DoesntChangeIY)(l => !l.changesRegister(ZRegister.IY))
    simpleRetPropertyScan(_.a)(SetsATo)
  }

  override def gatherFunctionOptimizationHints(options: CompilationOptions, niceFunctionProperties: mutable.Set[(NiceFunctionProperty, String)], function: FunctionInMemory): Unit = {
    import NiceFunctionProperty._
    val functionName = function.name
    if (function.optimizationHints("preserves_a")) niceFunctionProperties += DoesntChangeA -> functionName
    if (function.optimizationHints("preserves_bc")) niceFunctionProperties += DoesntChangeBC -> functionName
    if (function.optimizationHints("preserves_de")) niceFunctionProperties += DoesntChangeDE -> functionName
    if (function.optimizationHints("preserves_hl")) niceFunctionProperties += DoesntChangeHL -> functionName
    if (function.optimizationHints("preserves_cf")) niceFunctionProperties += DoesntChangeCF -> functionName
    if (function.optimizationHints("preserves_memory")) niceFunctionProperties += DoesntWriteMemory -> functionName
    if (function.optimizationHints("idempotent")) niceFunctionProperties += Idempotent -> functionName
  }

  @tailrec
  private def isNaughty(code: List[ZLine]): Boolean = {
    import ZOpcode._
    code match {
      case ZLine0(JP, OneRegister(_), _) :: _ => true
      case ZLine0(PUSH, _, _) :: ZLine0(RET | RETI | RETN, _, _) :: _ => true
      case _ :: xs => isNaughty(xs)
      case Nil => false
    }
  }

  override def bytePseudoopcode: String = "DB"

  override def deduplicate(options: CompilationOptions, compiledFunctions: mutable.Map[String, CompiledFunction[ZLine]]): Unit =
    new Z80Deduplicate(rootEnv, options).apply(compiledFunctions)
}

object Z80Assembler {

  case class One(opcode: Int, multiplier: Int)

  val implieds: mutable.Map[ZOpcode.Value, Int] = mutable.Map[ZOpcode.Value, Int]()
  val immediates: mutable.Map[ZOpcode.Value, Int] = mutable.Map[ZOpcode.Value, Int]()
  val edImplieds: mutable.Map[ZOpcode.Value, Int] = mutable.Map[ZOpcode.Value, Int]()
  val oneRegister: mutable.Map[ZOpcode.Value, One] = mutable.Map[ZOpcode.Value, One]()
  val cbOneRegister: mutable.Map[ZOpcode.Value, One] = mutable.Map[ZOpcode.Value, One]()
  val nextEdImplieds: mutable.Map[ZOpcode.Value, Int] = mutable.Map[ZOpcode.Value, Int]()

  do {
    import ZOpcode._
    implieds(NOP) = 0
    implieds(DAA) = 0x27
    implieds(SCF) = 0x37
    implieds(CPL) = 0x2f
    implieds(CCF) = 0x3f
    implieds(RET) = 0xc9
    implieds(EI) = 0xfb
    implieds(DI) = 0xf3
    implieds(HALT) = 0x76
    implieds(RLCA) = 7
    implieds(RRCA) = 0xf
    implieds(RLA) = 0x17
    implieds(RRA) = 0x1f

    immediates(ADD) = 0xc6
    immediates(ADC) = 0xce
    immediates(SUB) = 0xd6
    immediates(SBC) = 0xde
    immediates(AND) = 0xe6
    immediates(XOR) = 0xee
    immediates(OR) = 0xf6
    immediates(CP) = 0xfe

    edImplieds(NEG) = 0x44
    edImplieds(RETN) = 0x45
    edImplieds(RRD) = 0x67
    edImplieds(RLD) = 0x6f
    edImplieds(LDI) = 0xa0
    edImplieds(LDIR) = 0xb0
    edImplieds(CPI) = 0xa1
    edImplieds(CPIR) = 0xb1
    edImplieds(INI) = 0xa2
    edImplieds(INIR) = 0xb2
    edImplieds(OUTI) = 0xa3
    edImplieds(OUTIR) = 0xb3
    edImplieds(LDD) = 0xa8
    edImplieds(LDDR) = 0xb8
    edImplieds(CPD) = 0xa9
    edImplieds(CPDR) = 0xb9
    edImplieds(IND) = 0xaa
    edImplieds(INDR) = 0xba
    edImplieds(OUTD) = 0xab
    edImplieds(OUTDR) = 0xbb

    oneRegister(ADD) = One(0x80, 1)
    oneRegister(ADC) = One(0x88, 1)
    oneRegister(SUB) = One(0x90, 1)
    oneRegister(SBC) = One(0x98, 1)
    oneRegister(AND) = One(0xa0, 1)
    oneRegister(XOR) = One(0xa8, 1)
    oneRegister(OR) = One(0xb0, 1)
    oneRegister(CP) = One(0xb8, 1)
    oneRegister(INC) = One(4, 8)
    oneRegister(DEC) = One(5, 8)
    oneRegister(INC_16) = One(3, 0x10)
    oneRegister(DEC_16) = One(0xb, 0x10)

    oneRegister(POP) = One(0xc1, 0x10)
    oneRegister(PUSH) = One(0xc5, 0x10)

    cbOneRegister(RLC) = One(0, 1)
    cbOneRegister(RRC) = One(8, 1)
    cbOneRegister(RL) = One(0x10, 1)
    cbOneRegister(RR) = One(0x18, 1)
    cbOneRegister(SLA) = One(0x20, 1)
    cbOneRegister(SRA) = One(0x28, 1)
    cbOneRegister(SRL) = One(0x38, 1)

    nextEdImplieds(LDIX) = 0xa4
    nextEdImplieds(LDWS) = 0xa5
    nextEdImplieds(LDIRX) = 0xb4
    nextEdImplieds(LDDX) = 0xb5
    nextEdImplieds(LDDRX) = 0xac
    nextEdImplieds(LDPIRX) = 0xbc
    nextEdImplieds(OUTINB) = 0x90
    nextEdImplieds(MUL) = 0xa4
    nextEdImplieds(SWAPNIB) = 0x23
    nextEdImplieds(MIRROR) = 0x24
    nextEdImplieds(PIXELDN) = 0x93
    nextEdImplieds(PIXELAD) = 0x94
    nextEdImplieds(SETAE) = 0x95
  } while (false)

}

