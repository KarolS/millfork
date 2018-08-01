package millfork.assembly.z80.opt

import millfork.CompilationFlag
import millfork.assembly.{AssemblyOptimization, OptimizationContext}
import millfork.assembly.z80._
import millfork.env.{MemoryAddressConstant, NormalFunction, NumericConstant}
import millfork.node.ZRegister

/**
  * @author Karol Stasiak
  */
object CompactStackFrame extends AssemblyOptimization[ZLine] {
  override def name: String = "Compacting the stack frame"

  override def optimize(f: NormalFunction, code: List[ZLine], context: OptimizationContext): List[ZLine] = {
    val register =
      if (context.options.flag(CompilationFlag.UseIxForStack)) ZRegister.IX
      else if (context.options.flag(CompilationFlag.UseIyForStack)) ZRegister.IY
      else return code
    optimizeStart(code, register) match {
      case Some((optimized, before, after)) =>
        context.log.debug(s"Optimized stack frame from $before to $after bytes")
        optimized
      case None => code
    }
  }

  def optimizeStart(code: List[ZLine], Index: ZRegister.Value): Option[(List[ZLine], Int, Int)] = {
    import millfork.assembly.z80.ZOpcode._
    import millfork.node.ZRegister._
    code match {
      case (name@ZLine(LABEL, _, _, _)) ::
        ZLine(PUSH, OneRegister(Index), _, true) ::
        ZLine(LD_16, TwoRegisters(Index, IMM_16), NumericConstant(negativeSize, _), true) ::
        ZLine(ADD_16, TwoRegisters(Index, SP), _, true) ::
        ZLine(LD_16, TwoRegisters(SP, Index), _, true) :: tail =>
        val sourceSize = (-negativeSize).&(0xffff).toInt
        val usedOffsets: Set[Int] = findUsedOffsets(tail, Index match {
          case IX => MEM_IX_D
          case IY => MEM_IY_D
        })
        val targetSize = usedOffsets.size + usedOffsets.size.&(1)
        if (targetSize == sourceSize)  None else {
          val prologue = if (targetSize == 0) Nil else List(
            ZLine.register(PUSH, Index),
            ZLine.ldImm16(Index, 0x10000 - targetSize),
            ZLine.registers(ADD_16, Index, SP),
            ZLine.ld16(SP, Index))
          val map = usedOffsets.toSeq.sorted.zipWithIndex.toMap
          optimizeContinue(tail, Index, sourceSize, targetSize, map).map { optTail =>
            (name :: prologue ++ optTail, sourceSize, targetSize)
          }
        }
      case _ =>
        None
    }
  }


  def findUsedOffsets(code: List[ZLine], Mem: ZRegister.Value): Set[Int] = {
    code.flatMap {
      case ZLine(_, OneRegisterOffset(Mem, offset), _, _) => Some(offset)
      case ZLine(_, TwoRegistersOffset(_, Mem, offset), _, _) => Some(offset)
      case ZLine(_, TwoRegistersOffset(Mem, _, offset), _, _) => Some(offset)
      case _ => None
    }.toSet
  }

  def optimizeContinue(code: List[ZLine], Index: ZRegister.Value, sourceSize: Int, targetSize: Int, mapping: Map[Int, Int]): Option[List[ZLine]] = {
    import millfork.assembly.z80.ZOpcode._
    import millfork.node.ZRegister._
    val Mem = Index match {
      case IX => MEM_IX_D
      case IY => MEM_IY_D
    }
    code match {
      case (head@ZLine(_, TwoRegistersOffset(reg, Mem, offset), _, _)) :: tail =>
        optimizeContinue(tail, Index, sourceSize, targetSize, mapping).map(
          head.copy(registers = TwoRegistersOffset(reg, Mem, mapping(offset))) :: _)

      case (head@ZLine(_, TwoRegistersOffset(Mem, reg, offset), _, _)) :: tail =>
        optimizeContinue(tail, Index, sourceSize, targetSize, mapping).map(
          head.copy(registers = TwoRegistersOffset(Mem, reg, mapping(offset))) :: _)

      case (head@ZLine(_, OneRegisterOffset(Mem, offset), _, _)) :: tail =>
        optimizeContinue(tail, Index, sourceSize, targetSize, mapping).map(
          head.copy(registers = OneRegisterOffset(Mem, mapping(offset))) :: _)

      case
        ZLine(LD_16, TwoRegisters(Index, IMM_16), NumericConstant(size, _), _) ::
          ZLine(ADD_16, TwoRegisters(Index, SP), _, _) ::
          ZLine(LD_16, TwoRegisters(SP, Index), _, _) ::
          ZLine(POP, OneRegister(Index), _, _) :: tail =>
        if (size != sourceSize) None
        else {
          stripReturn(tail).flatMap {
            case (ret, rest) =>
              val epilogue = if (targetSize == 0) Nil else {
                List(
                  ZLine.ldImm16(Index, targetSize),
                  ZLine.registers(ADD_16, Index, SP),
                  ZLine.ld16(SP, Index),
                  ZLine.register(POP, Index))
              }
              optimizeContinue(rest, Index, sourceSize, targetSize, mapping).map(epilogue ++ ret ++ _)
          }
        }
      case
        ZLine(LD_16, TwoRegisters(HL, IMM_16), NumericConstant(size, _), _) ::
          ZLine(ADD_16, TwoRegisters(HL, SP), _, _) ::
          ZLine(LD_16, TwoRegisters(SP, HL), _, _) ::
          ZLine(POP, OneRegister(Index), _, _) :: tail =>
        if (size != sourceSize) {
          println("Mismatched stack frame sizes")
          None
        } else {
          stripReturn(tail).flatMap {
            case (ret, rest) =>
              val epilogue = if (targetSize == 0) Nil else {
                List(
                  ZLine.ldImm16(HL, targetSize),
                  ZLine.registers(ADD_16, HL, SP),
                  ZLine.ld16(SP, HL),
                  ZLine.register(POP, Index))
              }
              optimizeContinue(rest, Index, sourceSize, targetSize, mapping).map(epilogue ++ ret ++  _)
          }
        }
      case ZLine(RET | RETI | RETN | BYTE, _, _, _) :: _ => None
      case ZLine(JP, _, MemoryAddressConstant(f: NormalFunction), _) :: _ => None
      case x :: _ if x.changesRegister(Index) => None
      case x :: xs => optimizeContinue(xs, Index, sourceSize, targetSize, mapping).map(x :: _)
      case Nil => Some(Nil)
    }
  }

  def stripReturn(code: List[ZLine]): Option[(List[ZLine], List[ZLine])] = {
    val (discards, rest) = code.span(l => ZOpcodeClasses.NoopDiscards(l.opcode))
    if (rest.isEmpty) return None
    import millfork.assembly.z80.ZOpcode._
    val potentialResult = (discards :+ rest.head) -> rest.tail
    rest.head match {
      case ZLine(RET | RETI | RETN, _, _, _) => Some(potentialResult)
      case ZLine(JP, NoRegisters, MemoryAddressConstant(f: NormalFunction), _) => Some(potentialResult)
      case _ => None
    }
  }
}
