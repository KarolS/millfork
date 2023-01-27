package millfork.assembly.mos.opt

import millfork.CompilationFlag
import millfork.assembly.{AssemblyOptimization, OptimizationContext}
import millfork.assembly.mos.{AssemblyLine, AssemblyLine0, OpcodeClasses}
import millfork.env.NormalFunction

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
object UseAccumulatorInsteadOfYRegister extends UseAccumulatorInsteadOfIndexRegister(true)
object UseAccumulatorInsteadOfXRegister extends UseAccumulatorInsteadOfIndexRegister(false)

class UseAccumulatorInsteadOfIndexRegister(doY: Boolean) extends AssemblyOptimization[AssemblyLine] {
  override def name: String = if (doY) "Use accumulator instead of the Y register" else "Use accumulator instead of the X register"

  override def minimumRequiredLines: Int = 2

  override def optimize(f: NormalFunction, code: List[AssemblyLine], context: OptimizationContext): List[AssemblyLine] = {
    val log = context.log
    if (f.params.length == 1) return code
    if (!doY && f.returnType.size == 2) return code
    val requiresResult = f.returnType.size == 1 || f.returnType.size == 2
    if (doY) {
      if (!code.exists(line => line.concernsY)) return code
    } else {
      if (!code.exists(line => line.concernsX)) return code
    }
    var resultDumped = true
    val result = mutable.ListBuffer[AssemblyLine]()
    val supportsInc = context.options.flag(CompilationFlag.EmitCmosOpcodes)
    for(line <- code) {
      import millfork.assembly.mos.Opcode._
      import millfork.assembly.mos.AddrMode._
      if (doY) {
        if (OpcodeClasses.ChangesY(line.opcode)) resultDumped = false
      } else {
        if (OpcodeClasses.ChangesX(line.opcode)) resultDumped = false
      }
      line match {
        case AssemblyLine0(_, ZeroPageY, _) => return code
        case AssemblyLine0(_, AbsoluteY | ZeroPageY | IndexedSY | IndexedY | LongIndexedY, _) if doY => return code
        case AssemblyLine0(_, AbsoluteX | ZeroPageX | IndexedX | LongAbsoluteX, _) if !doY => return code
        case AssemblyLine0(SAY | SHY | XAA | LAS | AHX | SAX | HuSAX | SBX | SHX, _, _) => return code
        case AssemblyLine0(TSX | TXS, _, _) if !doY => return code
        case AssemblyLine0(INY, _, _) if doY =>
          if (supportsInc) result += AssemblyLine.implied(INC)
          else return code
        case AssemblyLine0(INX, _, _) if !doY =>
          if (supportsInc) result += AssemblyLine.implied(INC)
          else return code
        case AssemblyLine0(DEY, _, _) if doY =>
          if (supportsInc) result += AssemblyLine.implied(DEC)
          else return code
        case AssemblyLine0(DEX, _, _) if !doY =>
          if (supportsInc) result += AssemblyLine.implied(DEX)
          else return code
        case AssemblyLine0(LDY, _, _) if doY => result += line.copy(opcode = LDA)
        case AssemblyLine0(LDX, _, _) if !doY => result += line.copy(opcode = LDA)
        case AssemblyLine0(LAX, _, _) if !doY =>
          result += line.copy(opcode = LDA)
          resultDumped = true
        case AssemblyLine0(STY, _, _) if doY => result += line.copy(opcode = STA)
        case AssemblyLine0(STX, _, _) if !doY => result += line.copy(opcode = STA)
        case AssemblyLine0(TYA, _, _) if doY =>
          result += line.copy(opcode = TAY)
          resultDumped = true
        case AssemblyLine0(TXA, _, _) if !doY =>
          result += line.copy(opcode = TAX)
          resultDumped = true
        case AssemblyLine0(CPY, _, _) if doY => result += line.copy(opcode = CMP)
        case AssemblyLine0(CPX, _, _) if !doY => result += line.copy(opcode = CMP)
        case AssemblyLine0(op, _, _) if OpcodeClasses.NoopDiscardsFlags(op) => result += line
        case AssemblyLine0(op, _, _) if OpcodeClasses.ConcernsAAlways(op) => return code
        case AssemblyLine0(op, Implied, _) if OpcodeClasses.ConcernsAIfImplied(op) => return code
        case AssemblyLine0(RTS | RTI | JMP, _, _) =>
          if (requiresResult && !resultDumped) return code
          else result += line
        case AssemblyLine0(LABEL, _, _) =>
          resultDumped = false
          result += line
        case AssemblyLine0(op, _, _) if OpcodeClasses.ShortBranching(op) => result += line
        case AssemblyLine0(op, _, _) if !OpcodeClasses.AllLinear(op) => return code
        case _ => result += line
      }
    }
    log.debug(name)
    if (log.traceEnabled) {
      code.foreach(l => log.trace(l.toString))
      log.trace("     ↓")
      result.foreach(l => log.trace(l.toString))
    }
    result.toList
  }
}
