package millfork.output

import millfork.{CompilationOptions, Platform}
import millfork.assembly.z80._
import millfork.compiler.z80.Z80Compiler
import millfork.env._
import millfork.error.ErrorReporting
import millfork.node.{NiceFunctionProperty, Program, ZRegister}

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
class Z80Assembler(program: Program,
                   rootEnv: Environment,
                   platform: Platform) extends AbstractAssembler[ZLine](program, rootEnv, platform, Z80InliningCalculator, Z80Compiler) {
  override def performFinalOptimizationPass(f: NormalFunction, actuallyOptimize: Boolean, options: CompilationOptions, code: List[ZLine]): List[ZLine] = code

  private def internalRegisterIndex(reg: ZRegister.Value): Int = reg match {
    case ZRegister.B => 0
    case ZRegister.C => 1
    case ZRegister.D => 2
    case ZRegister.E => 3
    case ZRegister.H => 4
    case ZRegister.L => 5
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

  private def internalArithmeticIndex(opcode: ZOpcode.Value): Int = {
    import ZOpcode._
    opcode match {
      case ADD => 0x80
      case ADC => 0x88
      case SUB => 0x90
      case SBC => 0x98
      case AND => 0xa0
      case XOR => 0xa8
      case OR => 0xb0
      case CP => 0xb8
    }
  }

  override def emitInstruction(bank: String, options: CompilationOptions, index: Int, instr: ZLine): Int = {
    import millfork.assembly.z80.ZOpcode._
    import Z80Assembler._
    instr match {
      case ZLine(LABEL, NoRegisters, MemoryAddressConstant(Label(labelName)), _) =>
        labelMap(labelName) = index
        index
      case ZLine(BYTE, NoRegisters, param, _) =>
        writeByte(bank, index, param)
        index + 1
      case ZLine(DISCARD_F | DISCARD_HL | DISCARD_BCDEIX | DISCARD_A, NoRegisters, _, _) =>
        index
      case ZLine(LABEL | BYTE | DISCARD_F | DISCARD_HL | DISCARD_BCDEIX | DISCARD_A, _, _, _) =>
        ???
      case ZLine(RST, NoRegisters, param, _) =>
        val opcode = param.quickSimplify match {
          case NumericConstant(n, _) if n >=0 && n <= 0x38 && n % 8 == 0 => 0xc7 + n.toInt
          case _ => ErrorReporting.error("Invalid param for RST"); 0xc7
        }
        writeByte(bank, index, opcode)
        index + 1
      case ZLine(op, NoRegisters, _, _) if implieds.contains(op) =>
        writeByte(bank, index, implieds(op))
        index + 1
      case ZLine(op, NoRegisters, _, _) if edImplieds.contains(op) =>
        writeByte(bank, index, 0xed)
        writeByte(bank, index + 1, edImplieds(op))
        index + 2
      case ZLine(ADD_16, TwoRegisters(ZRegister.HL, source), param, _) =>
        writeByte(bank, index, 9 + 16 * internalRegisterIndex(source))
        index + 1
      case ZLine(SBC_16, TwoRegisters(ZRegister.HL, reg), _, _) =>
        writeByte(bank, index, 0xed)
        writeByte(bank, index + 1, 0x42 + 0x10 * internalRegisterIndex(reg))
        index + 2
      case ZLine(LD_16, TwoRegisters(target, ZRegister.IMM_16), param, _) =>
        writeByte(bank, index, 1 + 16 * internalRegisterIndex(target))
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine(LD_16, TwoRegisters(ZRegister.HL, ZRegister.MEM_ABS_16), param, _) =>
        writeByte(bank, index, 0x2a)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine(LD_16, TwoRegisters(ZRegister.MEM_ABS_16, ZRegister.HL), param, _) =>
        writeByte(bank, index, 0x22)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine(op, OneRegister(ZRegister.IMM_8), param, _) if immediates.contains(op) =>
        val o = immediates(op)
        writeByte(bank, index, o)
        writeByte(bank, index + 1, param)
        index + 2
      case ZLine(op, OneRegister(ZRegister.MEM_IX_D), _, _) if oneRegister.contains(op) =>
        val o = oneRegister(op)
        writeByte(bank, index, 0xdd)
        writeByte(bank, index + 1, o.opcode + internalRegisterIndex(ZRegister.MEM_HL) * o.multiplier)
        writeByte(bank, index + 2, instr.parameter)
        index + 3
      case ZLine(op, OneRegister(ZRegister.MEM_IY_D), _, _) if oneRegister.contains(op) =>
        val o = oneRegister(op)
        writeByte(bank, index, 0xfd)
        writeByte(bank, index + 1, o.opcode + internalRegisterIndex(ZRegister.MEM_HL) * o.multiplier)
        writeByte(bank, index + 2, instr.parameter)
        index + 3
      case ZLine(op, OneRegister(ZRegister.IX), _, _) if oneRegister.contains(op) =>
        val o = oneRegister(op)
        writeByte(bank, index, 0xdd)
        writeByte(bank, index + 1, o.opcode + internalRegisterIndex(ZRegister.HL) * o.multiplier)
        writeByte(bank, index + 2, instr.parameter)
        index + 3
      case ZLine(op, OneRegister(ZRegister.IY), _, _) if oneRegister.contains(op) =>
        val o = oneRegister(op)
        writeByte(bank, index, 0xfd)
        writeByte(bank, index + 1, o.opcode + internalRegisterIndex(ZRegister.HL) * o.multiplier)
        writeByte(bank, index + 2, instr.parameter)
        index + 3
      case ZLine(op, OneRegister(reg), _, _) if reg != ZRegister.IMM_8 && oneRegister.contains(op) =>
        val o = oneRegister(op)
        writeByte(bank, index, o.opcode + internalRegisterIndex(reg) * o.multiplier)
        index + 1
      case ZLine(op, OneRegister(reg), _, _) if cbOneRegister.contains(op) =>
        val o = cbOneRegister(op)
        writeByte(bank, index, 0xcb)
        writeByte(bank, index + 1, o.opcode + internalRegisterIndex(reg) * o.multiplier)
        index + 2
      case ZLine(LD, registers, _, _) =>
        registers match {
          case TwoRegisters(reg, ZRegister.IMM_8) =>
            writeByte(bank, index, 6 + 8 * internalRegisterIndex(reg))
            writeByte(bank, index + 1, instr.parameter)
            index + 2
          case TwoRegisters(ZRegister.A, ZRegister.MEM_ABS_8) =>
            writeByte(bank, index, 0x3a)
            writeWord(bank, index + 1, instr.parameter)
            index + 3
          case TwoRegisters(ZRegister.MEM_ABS_8, ZRegister.A) =>
            writeByte(bank, index, 0x32)
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
          case TwoRegisters(ZRegister.MEM_IX_D, source) =>
            writeByte(bank, index, 0xdd)
            writeByte(bank, index + 1, 0x40 + internalRegisterIndex(source) + internalRegisterIndex(ZRegister.MEM_HL) * 8)
            writeByte(bank, index + 1, instr.parameter)
            index + 3
          case TwoRegisters(ZRegister.MEM_IY_D, source) =>
            writeByte(bank, index, 0xfd)
            writeByte(bank, index + 1, 0x40 + internalRegisterIndex(source) + internalRegisterIndex(ZRegister.MEM_HL) * 8)
            writeByte(bank, index + 1, instr.parameter)
            index + 3
          case TwoRegisters(target, ZRegister.MEM_IX_D) =>
            writeByte(bank, index, 0xdd)
            writeByte(bank, index, 0x40 + internalRegisterIndex(ZRegister.MEM_HL) + internalRegisterIndex(target) * 8)
            writeByte(bank, index + 1, instr.parameter)
            index + 3
          case TwoRegisters(target, ZRegister.MEM_IY_D) =>
            writeByte(bank, index, 0xfd)
            writeByte(bank, index, 0x40 + internalRegisterIndex(ZRegister.MEM_HL) + internalRegisterIndex(target) * 8)
            writeByte(bank, index + 1, instr.parameter)
            index + 3
          case TwoRegisters(target, source) =>
            writeByte(bank, index, 0x40 + internalRegisterIndex(source) + internalRegisterIndex(target) * 8)
            index + 1
          case TwoRegisters(target, source) =>
            writeByte(bank, index, 0x40 + internalRegisterIndex(source) + internalRegisterIndex(target) * 8)
            index + 1
        }
      case ZLine(CALL, NoRegisters, param, _) =>
        writeByte(bank, index, 0xcd)
        writeWord(bank, index + 1, param)
        index + 3



      case ZLine(CALL, IfFlagClear(ZFlag.Z), param, _) =>
        writeByte(bank, index, 0xc4)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine(CALL, IfFlagClear(ZFlag.C), param, _) =>
        writeByte(bank, index, 0xd4)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine(CALL, IfFlagClear(ZFlag.P), param, _) =>
        writeByte(bank, index, 0xe4)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine(CALL, IfFlagClear(ZFlag.S), param, _) =>
        writeByte(bank, index, 0xf4)
        writeWord(bank, index + 1, param)
        index + 3

      case ZLine(CALL, IfFlagSet(ZFlag.Z), param, _) =>
        writeByte(bank, index, 0xcc)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine(CALL, IfFlagSet(ZFlag.C), param, _) =>
        writeByte(bank, index, 0xdc)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine(CALL, IfFlagSet(ZFlag.P), param, _) =>
        writeByte(bank, index, 0xec)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine(CALL, IfFlagSet(ZFlag.S), param, _) =>
        writeByte(bank, index, 0xfc)
        writeWord(bank, index + 1, param)
        index + 3

      case ZLine(JP, NoRegisters, param, _) =>
        writeByte(bank, index, 0xc3)
        writeWord(bank, index + 1, param)
        index + 3

      case ZLine(JP, IfFlagClear(ZFlag.Z), param, _) =>
        writeByte(bank, index, 0xc2)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine(JP, IfFlagClear(ZFlag.C), param, _) =>
        writeByte(bank, index, 0xd2)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine(JP, IfFlagClear(ZFlag.P), param, _) =>
        writeByte(bank, index, 0xe2)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine(JP, IfFlagClear(ZFlag.S), param, _) =>
        writeByte(bank, index, 0xf2)
        writeWord(bank, index + 1, param)
        index + 3

      case ZLine(JP, IfFlagSet(ZFlag.Z), param, _) =>
        writeByte(bank, index, 0xca)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine(JP, IfFlagSet(ZFlag.C), param, _) =>
        writeByte(bank, index, 0xda)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine(JP, IfFlagSet(ZFlag.P), param, _) =>
        writeByte(bank, index, 0xea)
        writeWord(bank, index + 1, param)
        index + 3
      case ZLine(JP, IfFlagSet(ZFlag.S), param, _) =>
        writeByte(bank, index, 0xfa)
        writeWord(bank, index + 1, param)
        index + 3

      case ZLine(RET, IfFlagClear(ZFlag.Z), param, _) =>
        writeByte(bank, index, 0xc0)
        index + 1
      case ZLine(RET, IfFlagClear(ZFlag.C), param, _) =>
        writeByte(bank, index, 0xd0)
        index + 1
      case ZLine(RET, IfFlagClear(ZFlag.P), param, _) =>
        writeByte(bank, index, 0xe0)
        index + 1
      case ZLine(RET, IfFlagClear(ZFlag.S), param, _) =>
        writeByte(bank, index, 0xf0)
        index + 1

      case ZLine(RET, IfFlagSet(ZFlag.Z), param, _) =>
        writeByte(bank, index, 0xc8)
        index + 1
      case ZLine(RET, IfFlagSet(ZFlag.C), param, _) =>
        writeByte(bank, index, 0xd8)
        index + 1
      case ZLine(RET, IfFlagSet(ZFlag.P), param, _) =>
        writeByte(bank, index, 0xe8)
        index + 1
      case ZLine(RET, IfFlagSet(ZFlag.S), param, _) =>
        writeByte(bank, index, 0xf8)
        index + 1

      case ZLine(JR, IfFlagClear(ZFlag.Z), param, _) =>
        writeByte(bank, index, 0x20)
        writeByte(bank, index + 1, AssertByte(param - index - 2))
        index + 2
      case ZLine(JR, IfFlagClear(ZFlag.C), param, _) =>
        writeByte(bank, index, 0x30)
        writeByte(bank, index + 1, AssertByte(param - index - 2))
        index + 2

      case ZLine(JR, IfFlagSet(ZFlag.Z), param, _) =>
        writeByte(bank, index, 0x28)
        writeByte(bank, index + 1, AssertByte(param - index - 2))
        index + 2
      case ZLine(JR, IfFlagSet(ZFlag.C), param, _) =>
        writeByte(bank, index, 0x38)
        writeByte(bank, index + 1, AssertByte(param - index - 2))
        index + 2

      case ZLine(JR, NoRegisters, param, _) =>
        writeByte(bank, index, 0x18)
        writeByte(bank, index + 1, AssertByte(param - index - 2))
        index + 2
      case ZLine(DJNZ, NoRegisters, param, _) =>
        writeByte(bank, index, 0x10)
        writeByte(bank, index + 1, AssertByte(param - index - 2))
        index + 2
      case _ =>
        ErrorReporting.fatal("Cannot assemble " + instr)
        index
    }
  }

  override def injectLabels(labelMap: Map[String, Int], code: List[ZLine]): List[ZLine] = code // TODO

  override def gatherNiceFunctionProperties(niceFunctionProperties: mutable.Set[(NiceFunctionProperty, String)], functionName: String, code: List[ZLine]): Unit = {
    // do nothing yet
  }
}

object Z80Assembler {

  case class One(opcode: Int, multiplier: Int)

  val implieds = mutable.Map[ZOpcode.Value, Int]()
  val immediates = mutable.Map[ZOpcode.Value, Int]()
  val edImplieds = mutable.Map[ZOpcode.Value, Int]()
  val oneRegister = mutable.Map[ZOpcode.Value, One]()
  val cbOneRegister = mutable.Map[ZOpcode.Value, One]()

  do {
    import ZOpcode._
    implieds(NOP) = 0
    implieds(DAA) = 0x27
    implieds(SCF) = 0x37
    implieds(CPL) = 0x37
    implieds(CCF) = 0x3f
    implieds(EX_AF_AF) = 8
    implieds(RET) = 0xc9
    implieds(EXX) = 0xd9
    implieds(EI) = 0xfb
    implieds(DI) = 0xf3
    implieds(HALT) = 0x76

    immediates(ADD) = 0xc6
    immediates(ADC) = 0xce
    immediates(SUB) = 0xd6
    immediates(SBC) = 0xde
    immediates(AND) = 0xe6
    immediates(XOR) = 0xee
    immediates(OR) = 0xf6
    immediates(CP) = 0xfe

    edImplieds(NEG) = 0x44
    edImplieds(RETI) = 0x4d
    edImplieds(RETN) = 0x45
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
    cbOneRegister(SLL) = One(0x30, 1)
    cbOneRegister(SRL) = One(0x38, 1)
  } while (false)

}

