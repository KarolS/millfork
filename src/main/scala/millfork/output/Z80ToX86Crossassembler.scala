package millfork.output

import millfork.{CompilationFlag, CompilationOptions, Cpu, Platform}
import millfork.assembly.z80.{ZOpcode, _}
import millfork.assembly.z80.opt.{ConditionalInstructions, JumpFollowing, JumpShortening}
import millfork.compiler.z80.Z80Compiler
import millfork.env._
import millfork.node.{NiceFunctionProperty, Position, Program, ZRegister}

import scala.collection.mutable


/**
  * @author Karol Stasiak
  */
class Z80ToX86Crossassembler(program: Program,
                   rootEnv: Environment,
                   platform: Platform) extends AbstractAssembler[ZLine](program, rootEnv, platform, Z80InliningCalculator, Z80Compiler) {
  override def performFinalOptimizationPass(f: NormalFunction, actuallyOptimize: Boolean, options: CompilationOptions, code: List[ZLine]): List[ZLine] = {
    if (actuallyOptimize) {
      JumpShortening(f, ConditionalInstructions(options, JumpFollowing(options, code)), options)
    } else code
  }

  override def injectLabels(labelMap: Map[String, (Int, Int)], code: List[ZLine]): List[ZLine] = code // TODO

  override def quickSimplify(code: List[ZLine]): List[ZLine] = code.map(a => a.copy(parameter = a.parameter.quickSimplify))

  override def gatherNiceFunctionProperties(options: CompilationOptions, niceFunctionProperties: mutable.Set[(NiceFunctionProperty, String)], function: NormalFunction, code: List[ZLine]): Unit = {
    // do nothing yet
  }

  override def gatherFunctionOptimizationHints(options: CompilationOptions, niceFunctionProperties: mutable.Set[(NiceFunctionProperty, String)], function: FunctionInMemory): Unit = {
    import NiceFunctionProperty._
    val functionName = function.name
    if (function.optimizationHints("preserves_memory")) niceFunctionProperties += DoesntWriteMemory -> functionName
    if (function.optimizationHints("idempotent")) niceFunctionProperties += Idempotent -> functionName
  }

  override def bytePseudoopcode: String = "DB"

  override def deduplicate(options: CompilationOptions, compiledFunctions: mutable.Map[String, CompiledFunction[ZLine]]): Unit =
    new Z80Deduplicate(rootEnv, options).apply(compiledFunctions)

  private def registerIndex(r: ZRegister.Value) = {
    import ZRegister._
    r match {
      case A => 0
      case C => 1
      case E => 2
      case L => 3
      case B => 5
      case D => 6
      case H => 7
      case AF => 0
      case BC => 1
      case DE => 2
      case HL => 3
      case SP => 4
      case IX => 5
    }
  }

  private def flagSkip(flag: ZRegisters): Int = flag match {
    case IfFlagSet(ZFlag.C) => 0x73 // JAE
    case IfFlagClear(ZFlag.C) => 0x72 // JB
    case IfFlagSet(ZFlag.S) => 0x79 // JNS
    case IfFlagClear(ZFlag.S) => 0x78 // JS
    case IfFlagSet(ZFlag.Z) => 0x75 // JNE
    case IfFlagClear(ZFlag.Z) => 0x74 // JE
    case IfFlagSet(ZFlag.P) => 0x7b // JPO
    case IfFlagClear(ZFlag.P) => 0x7a // JPE
    case IfFlagSet(ZFlag.K) => 0x7d // JGE
    case IfFlagClear(ZFlag.K) => 0x7c // JL
  }

  override def emitInstruction(bank: String, options: CompilationOptions, index: Int, instr: ZLine): Int = {
    import millfork.assembly.z80.ZOpcode._
    import ZRegister._
    import CompilationFlag._
    import Z80ToX86Crossassembler._
    implicit val position = instr.source.map(sl => Position(sl.moduleName, sl.line, 0, 0))
    instr match {
      case ZLine0(LABEL, NoRegisters, MemoryAddressConstant(Label(labelName))) =>
        val bank0 = mem.banks(bank)
        labelMap(labelName) = bank0.index -> index
        index
      case ZLine0(BYTE, NoRegisters, param) =>
        writeByte(bank, index, param)
        index + 1
      case ZLine0(DISCARD_F | DISCARD_HL | DISCARD_BC | DISCARD_DE | DISCARD_IX | DISCARD_IY | DISCARD_A | CHANGED_MEM, NoRegisters, _) =>
        index
      case ZLine0(LABEL | BYTE | DISCARD_F | DISCARD_HL | DISCARD_BC | DISCARD_DE | DISCARD_IX | DISCARD_IY | DISCARD_A | CHANGED_MEM, _, _) =>
        ???
      case ZLine0(op, NoRegisters, _) if implieds.contains(op) =>
        writeByte(bank, index, implieds(op))
        index + 1

      case ZLine0(JP, NoRegisters, param) =>
        writeByte(bank, index, 0xe9)
        writeWord(bank, index + 1, param + (- 3 - index))
        index + 3
      case ZLine0(JP, OneRegister(HL), param) =>
        writeByte(bank, index, 0xff)
        writeByte(bank, index + 1, 0xe3)
        index + 2
      case ZLine0(CALL, NoRegisters, param) =>
        writeByte(bank, index, 0xe8)
        writeWord(bank, index + 1, param + (- 3 -index))
        index + 3

      case ZLine0(RET, flag@(IfFlagSet(_) | IfFlagClear(_)), _) =>
        writeByte(bank, index, flagSkip(flag))
        writeByte(bank, index + 1, 1)
        writeByte(bank, index + 2, 0xc3)
        index + 3
      case ZLine0(JP, flag@(IfFlagSet(_) | IfFlagClear(_)), param) =>
        writeByte(bank, index, flagSkip(flag))
        writeByte(bank, index + 1, 3)
        writeByte(bank, index + 2, 0xe9)
        writeWord(bank, index + 3, param + (- 5 - index))
        index + 5
      case ZLine0(CALL, flag@(IfFlagSet(_) | IfFlagClear(_)), param) =>
        writeByte(bank, index, flagSkip(flag))
        writeByte(bank, index + 1, 3)
        writeByte(bank, index + 2, 0xe8)
        writeWord(bank, index + 3, param + (- 5 - index))
        index + 5

      case ZLine0(PUSH, OneRegister(AF), _) =>
        writeByte(bank, index, 0x9f)
        writeByte(bank, index+1, 0x86)
        writeByte(bank, index+2, 0xc4)
        writeByte(bank, index+3, 0x50)
        writeByte(bank, index+4, 0x86)
        writeByte(bank, index+5, 0xc4)
        index + 6
      case ZLine0(POP, OneRegister(AF), _) =>
        writeByte(bank, index, 0x58)
        writeByte(bank, index+1, 0x86)
        writeByte(bank, index+2, 0xc4)
        writeByte(bank, index+3, 0x9e)
        index + 4
      case ZLine0(PUSH, OneRegister(reg), _) =>
        writeByte(bank, index, 0x50 + registerIndex(reg))
        index + 1
      case ZLine0(POP, OneRegister(reg), _) =>
        writeByte(bank, index, 0x58 + registerIndex(reg))
        index + 1

      case ZLine0(LD, TwoRegisters(MEM_HL, IMM_8), param) =>
        writeByte(bank, index, 0xc6)
        writeByte(bank, index+1, 0x07)
        writeByte(bank, index+2, param)
        index + 3
      case ZLine0(LD, TwoRegisters(MEM_ABS_8, A), param) =>
        writeByte(bank, index, 0xa2)
        writeWord(bank, index+1, param)
        index + 3
      case ZLine0(LD, TwoRegisters(A, MEM_ABS_8), param) =>
        writeByte(bank, index, 0xa0)
        writeWord(bank, index+1, param)
        index + 3
      case ZLine0(LD, TwoRegistersOffset(MEM_IX_D, IMM_8, offset), param) =>
        writeByte(bank, index, 0xc6)
        writeByte(bank, index+1, 0x46)
        writeByte(bank, index+2, offset)
        writeByte(bank, index+3, param)
        index + 4
      case ZLine0(LD, TwoRegistersOffset(MEM_IX_D, reg, offset), param) =>
        writeByte(bank, index, 0x88)
        writeByte(bank, index + 1, 0x46 + registerIndex(reg) * 8)
        writeByte(bank, index + 2, offset)
        index + 3
      case ZLine0(LD, TwoRegistersOffset(reg, MEM_IX_D, offset), param) =>
        writeByte(bank, index, 0x8a)
        writeByte(bank, index + 1, 0x46 + registerIndex(reg) * 8)
        writeByte(bank, index + 2, offset)
        index + 3

      case ZLine0(LD, TwoRegisters(MEM_HL, reg), param) =>
        writeByte(bank, index, 0x88)
        writeByte(bank, index + 1, 0x7 + registerIndex(reg) * 8)
        index + 2
      case ZLine0(LD, TwoRegisters(reg, MEM_HL), param) =>
        writeByte(bank, index, 0x8a)
        writeByte(bank, index + 1, 0x7 + registerIndex(reg) * 8)
        index + 2

      case ZLine0(LD, TwoRegisters(MEM_DE, A), param) =>
        writeByte(bank, index, 0x89)
        writeByte(bank, index + 1, 0xd6)
        writeByte(bank, index + 2, 0x88)
        writeByte(bank, index + 3, 0x04)
        index + 4
      case ZLine0(LD, TwoRegisters(A, MEM_DE), param) =>
        writeByte(bank, index, 0x89)
        writeByte(bank, index + 1, 0xd6)
        writeByte(bank, index + 2, 0x8a)
        writeByte(bank, index + 3, 0x04)
        index + 4
      case ZLine0(LD, TwoRegisters(MEM_BC, A), param) =>
        writeByte(bank, index, 0x89)
        writeByte(bank, index + 1, 0xde)
        writeByte(bank, index + 2, 0x88)
        writeByte(bank, index + 3, 0x04)
        index + 4
      case ZLine0(LD, TwoRegisters(A, MEM_BC), param) =>
        writeByte(bank, index, 0x89)
        writeByte(bank, index + 1, 0xde)
        writeByte(bank, index + 2, 0x8a)
        writeByte(bank, index + 3, 0x04)
        index + 4

      case ZLine0(LD, TwoRegisters(reg, IMM_8), param) =>
        writeByte(bank, index, 0xb0 + registerIndex(reg))
        writeByte(bank, index + 1, param)
        index + 2
      case ZLine0(LD, TwoRegisters(target, source), param) =>
        writeByte(bank, index, 0x88)
        writeByte(bank, index + 1, 0xc0 + registerIndex(source) * 8 + registerIndex(target))
        index + 2

      case ZLine0(LD_16, TwoRegisters(reg, MEM_ABS_16), param) =>
        writeByte(bank, index, 0x8b)
        writeByte(bank, index + 1, 0x6 + registerIndex(reg) * 8)
        writeWord(bank, index + 2, param)
        index + 4
      case ZLine0(LD_16, TwoRegisters(MEM_ABS_16, reg), param) =>
        writeByte(bank, index, 0x89)
        writeByte(bank, index + 1, 0x6 + registerIndex(reg) * 8)
        writeWord(bank, index + 2, param)
        index + 4
      case ZLine0(LD_16, TwoRegisters(reg, IMM_16), param) =>
        writeByte(bank, index, 0xb8 + registerIndex(reg))
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine0(LD_16, TwoRegisters(SP, HL), param) =>
        writeByte(bank, index, 0x89)
        writeByte(bank, index + 1, 0xdc)
        index + 2
      case ZLine0(LD_16, TwoRegisters(SP, IX), param) =>
        writeByte(bank, index, 0x89)
        writeByte(bank, index + 1, 0xec)
        index + 2
        // TODO: other loads

      case ZLine0(CPL, _, _) =>
        writeByte(bank, index, 0xf6)
        writeByte(bank, index + 1, 0xd0)
        index + 2
      case ZLine0(op, OneRegister(IMM_8), param) if arith.contains(op) =>
        writeByte(bank, index, arith(op)+4)
        writeByte(bank, index + 1, param)
        index + 2
      case ZLine0(op, OneRegisterOffset(MEM_IX_D, offset), _) if arith.contains(op) =>
        writeByte(bank, index, arith(op)+2)
        writeByte(bank, index + 1, 0x46)
        writeByte(bank, index + 2, offset)
        index + 3
      case ZLine0(op, OneRegister(MEM_HL), _) if arith.contains(op) =>
        writeByte(bank, index, arith(op)+2)
        writeByte(bank, index + 1, 0x07)
        index + 2
      case ZLine0(op, OneRegister(reg), _) if arith.contains(op) =>
        writeByte(bank, index, arith(op))
        writeByte(bank, index + 1, 0xc0 + registerIndex(reg) * 8)
        index + 2

      case ZLine0(INC, OneRegisterOffset(MEM_IX_D, offset), _) =>
        writeByte(bank, index, 0xfe)
        writeByte(bank, index + 1, 0x46)
        writeByte(bank, index + 2, offset)
        index + 3
      case ZLine0(INC, OneRegister(MEM_HL), _) =>
        writeByte(bank, index, 0xfe)
        writeByte(bank, index + 1, 0x07)
        index + 2
      case ZLine0(INC, OneRegister(reg), _) =>
        writeByte(bank, index, 0xfe)
        writeByte(bank, index + 1, 0xc0 + registerIndex(reg))
        index + 2
      case ZLine0(DEC, OneRegisterOffset(MEM_IX_D, offset), _) =>
        writeByte(bank, index, 0xfe)
        writeByte(bank, index + 1, 0x4e)
        writeByte(bank, index + 2, offset)
        index + 3
      case ZLine0(DEC, OneRegister(MEM_HL), _) =>
        writeByte(bank, index, 0xfe)
        writeByte(bank, index + 1, 0x0f)
        index + 2
      case ZLine0(DEC, OneRegister(reg), _) =>
        writeByte(bank, index, 0xfe)
        writeByte(bank, index + 1, 0xc8 + registerIndex(reg))
        index + 2

      case ZLine0(INC_16, OneRegister(reg), _) =>
        writeByte(bank, index, 0x40 + registerIndex(reg))
        index + 1
      case ZLine0(DEC_16, OneRegister(reg), _) =>
        writeByte(bank, index, 0x48 + registerIndex(reg))
        index + 1

      case ZLine0(ADD_16, TwoRegisters(HL, reg), _) =>
        writeByte(bank, index, 0x1)
        writeByte(bank, index + 1, 0xc3 + registerIndex(reg) * 8)
        index + 2
      case ZLine0(ADD_16, TwoRegisters(IX, reg), _) =>
        writeByte(bank, index, 0x1)
        writeByte(bank, index + 1, 0xc5 + registerIndex(reg) * 8)
        index + 2

      case ZLine0(EX_DE_HL, _, _) =>
        writeByte(bank, index, 0x87)
        writeByte(bank, index + 1, 0xd3)
        index + 2

      case ZLine0(EX_SP, OneRegister(HL), _) =>
        writeByte(bank, index, 0x89)
        writeByte(bank, index + 1, 0x86)
        writeByte(bank, index + 2, 0x87)
        writeByte(bank, index + 3, 0x1c)
        index + 2

      case ZLine0(EX_SP, OneRegister(IX), _) =>
        writeByte(bank, index, 0x89)
        writeByte(bank, index + 1, 0x86)
        writeByte(bank, index + 2, 0x87)
        writeByte(bank, index + 3, 0x2c)
        index + 2

      case ZLine0(RRA, _, _) =>
        writeByte(bank, index, 0xd0)
        writeByte(bank, index + 1, 0xd8)
        index + 2
      case ZLine0(RRCA, _, _) =>
        writeByte(bank, index, 0xd0)
        writeByte(bank, index + 1, 0xc8)
        index + 2
      case ZLine0(RLA, _, _) =>
        writeByte(bank, index, 0xd0)
        writeByte(bank, index + 1, 0xd0)
        index + 2
      case ZLine0(RLCA, _, _) =>
        writeByte(bank, index, 0xd0)
        writeByte(bank, index + 1, 0xc0)
        index + 2

      case ZLine0(IN_IMM, _, param) =>
        writeByte(bank, index, 0xe4)
        writeByte(bank, index + 1, param)
        index + 2
      case ZLine0(OUT_IMM, _, param) =>
        writeByte(bank, index, 0xe6)
        writeByte(bank, index + 1, param)
        index + 2

      case ZLine0(LD_DEHL, _, param) =>
        writeByte(bank, index, 0x8d)
        writeByte(bank, index + 1 , 0x57)
        writeByte(bank, index + 2, param)
        index + 3

      case ZLine0(LD_DESP, _, param) =>
        writeByte(bank, index, 0x89)
        writeByte(bank, index + 1, 0xe6)
        writeByte(bank, index + 2, 0x8d)
        writeByte(bank, index + 3 , 0x54)
        writeByte(bank, index + 4, param)
        index + 5

      case ZLine0(LD_HLSP, _, param) =>
        writeByte(bank, index, 0x89)
        writeByte(bank, index + 1, 0xe6)
        writeByte(bank, index + 2, 0x8d)
        writeByte(bank, index + 3 , 0x5c)
        writeByte(bank, index + 4, param)
        index + 5

      case ZLine0(DSUB, _, param) =>
        writeByte(bank, index, 0x29)
        writeByte(bank, index + 1, 0xcb)
        index + 2

      case ZLine0(SHLX, _, param) =>
        writeByte(bank, index, 0x89)
        writeByte(bank, index + 1, 0xd6)
        writeByte(bank, index + 2, 0x89)
        writeByte(bank, index + 3 , 0x1c)
        index + 4

      case ZLine0(LHLX, _, param) =>
        writeByte(bank, index, 0x89)
        writeByte(bank, index + 1, 0xd6)
        writeByte(bank, index + 2, 0x8b)
        writeByte(bank, index + 3 , 0x1c)
        index + 4

      case _ =>
        println("TODO: " + instr)
        ???
    }
  }
}

object Z80ToX86Crossassembler {

  case class One(opcode: Int, multiplier: Int)

  val implieds: mutable.Map[ZOpcode.Value, Int] = mutable.Map[ZOpcode.Value, Int]()
  val arith: mutable.Map[ZOpcode.Value, Int] = mutable.Map[ZOpcode.Value, Int]()
  val edImplieds: mutable.Map[ZOpcode.Value, Int] = mutable.Map[ZOpcode.Value, Int]()
  val oneRegister: mutable.Map[ZOpcode.Value, One] = mutable.Map[ZOpcode.Value, One]()
  val cbOneRegister: mutable.Map[ZOpcode.Value, One] = mutable.Map[ZOpcode.Value, One]()

  do {
    import ZOpcode._
    implieds(NOP) = 0x90
    implieds(DAA) = 0x27
    implieds(SCF) = 0xf9
    implieds(CCF) = 0xf5
    implieds(RET) = 0xc3
    implieds(EI) = 0xfb
    implieds(DI) = 0xfa
    implieds(HALT) = 0xf4

    arith(ADD) = 0x00
    arith(ADC) = 0x10
    arith(SUB) = 0x28
    arith(SBC) = 0x18
    arith(AND) = 0x20
    arith(XOR) = 0x30
    arith(OR) = 0x08
    arith(CP) = 0x38

  } while (false)

}

