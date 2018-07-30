package millfork.assembly.z80.opt

import millfork.assembly.{AssemblyOptimization, OptimizationContext}
import millfork.assembly.z80._
import millfork.env.{MemoryAddressConstant, NormalFunction, NumericConstant}
import millfork.error.ConsoleLogger
import millfork.node.ZRegister

/**
  * @author Karol Stasiak
  */
object CompactStackFrame extends AssemblyOptimization[ZLine] {
  override def name: String = "Compacting the stack frame"

  override def optimize(f: NormalFunction, code: List[ZLine], context: OptimizationContext): List[ZLine] = {
    optimizeStart(code) match {
      case Some((optimized, before, after)) =>
        context.log.debug(s"Optimized stack frame from $before to $after bytes")
        optimized
      case None => code
    }
  }

  def optimizeStart(code: List[ZLine]): Option[(List[ZLine], Int, Int)] = {
    import millfork.assembly.z80.ZOpcode._
    import millfork.node.ZRegister._
    code match {
      case (name@ZLine(LABEL, _, _, _)) ::
        ZLine(PUSH, OneRegister(IX), _, true) ::
        ZLine(LD_16, TwoRegisters(IX, IMM_16), NumericConstant(negativeSize, _), true) ::
        ZLine(ADD_16, TwoRegisters(IX, SP), _, true) ::
        ZLine(LD_16, TwoRegisters(SP, IX), _, true) :: tail =>
        val sourceSize = (-negativeSize).&(0xffff).toInt
        val usedOffsets: Set[Int] = findUsedOffsets(tail)
        val targetSize = usedOffsets.size + usedOffsets.size.&(1)
        if (targetSize == sourceSize)  None else {
          val prologue = if (targetSize == 0) Nil else List(
            ZLine.register(PUSH, IX),
            ZLine.ldImm16(IX, 0x10000 - targetSize),
            ZLine.registers(ADD_16, IX, SP),
            ZLine.ld16(SP, IX))
          val map = usedOffsets.toSeq.sorted.zipWithIndex.toMap
          optimizeContinue(tail, sourceSize, targetSize, map).map { optTail =>
            (name :: prologue ++ optTail, sourceSize, targetSize)
          }
        }
      case _ =>
        None
    }
  }


  def findUsedOffsets(code: List[ZLine]): Set[Int] = {
    code.flatMap {
      case ZLine(_, OneRegisterOffset(ZRegister.MEM_IX_D, offset), _, _) => Some(offset)
      case ZLine(_, TwoRegistersOffset(_, ZRegister.MEM_IX_D, offset), _, _) => Some(offset)
      case ZLine(_, TwoRegistersOffset(ZRegister.MEM_IX_D, _, offset), _, _) => Some(offset)
      case _ => None
    }.toSet
  }

  def optimizeContinue(code: List[ZLine], sourceSize: Int, targetSize: Int, mapping: Map[Int, Int]): Option[List[ZLine]] = {
    import millfork.assembly.z80.ZOpcode._
    import millfork.node.ZRegister._
    code match {
      case (head@ZLine(_, TwoRegistersOffset(reg, MEM_IX_D, offset), _, _)) :: tail =>
        optimizeContinue(tail, sourceSize, targetSize, mapping).map(
          head.copy(registers = TwoRegistersOffset(reg, MEM_IX_D, mapping(offset))) :: _)

      case (head@ZLine(_, TwoRegistersOffset(MEM_IX_D, reg, offset), _, _)) :: tail =>
        optimizeContinue(tail, sourceSize, targetSize, mapping).map(
          head.copy(registers = TwoRegistersOffset(MEM_IX_D, reg, mapping(offset))) :: _)

      case (head@ZLine(_, OneRegisterOffset(MEM_IX_D, offset), _, _)) :: tail =>
        optimizeContinue(tail, sourceSize, targetSize, mapping).map(
          head.copy(registers = OneRegisterOffset(MEM_IX_D, mapping(offset))) :: _)

      case
        ZLine(LD_16, TwoRegisters(IX, IMM_16), NumericConstant(size, _), _) ::
          ZLine(ADD_16, TwoRegisters(IX, SP), _, _) ::
          ZLine(LD_16, TwoRegisters(SP, IX), _, _) ::
          ZLine(POP, OneRegister(IX), _, _) :: tail =>
        if (size != sourceSize) None
        else {
          stripReturn(tail).flatMap {
            case (ret, rest) =>
              val epilogue = if (targetSize == 0) Nil else {
                List(
                  ZLine.ldImm16(IX, targetSize),
                  ZLine.registers(ADD_16, IX, SP),
                  ZLine.ld16(SP, IX),
                  ZLine.register(POP, IX))
              }
              optimizeContinue(rest, sourceSize, targetSize, mapping).map(epilogue ++ ret ++ _)
          }
        }
      case
        ZLine(LD_16, TwoRegisters(HL, IMM_16), NumericConstant(size, _), _) ::
          ZLine(ADD_16, TwoRegisters(HL, SP), _, _) ::
          ZLine(LD_16, TwoRegisters(SP, HL), _, _) ::
          ZLine(POP, OneRegister(IX), _, _) :: tail =>
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
                  ZLine.register(POP, IX))
              }
              optimizeContinue(rest, sourceSize, targetSize, mapping).map(epilogue ++ ret ++  _)
          }
        }
      case ZLine(RET | RETI | RETN | BYTE, _, _, _) :: _ => None
      case ZLine(JP, _, MemoryAddressConstant(f: NormalFunction), _) :: _ => None
      case x :: _ if x.changesRegister(ZRegister.IX) => None
      case x :: xs => optimizeContinue(xs, sourceSize, targetSize, mapping).map(x :: _)
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
