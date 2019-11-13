package millfork.assembly.z80.opt

import millfork.CompilationFlag
import millfork.assembly.{AssemblyOptimization, Elidability, OptimizationContext}
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
      case (name@ZLine0(LABEL, _, _)) ::
        ZLine(PUSH, OneRegister(Index), _, Elidability.Elidable, s1) ::
        ZLine(LD_16, TwoRegisters(Index, IMM_16), NumericConstant(negativeSize, _), Elidability.Elidable, s2) ::
        ZLine(ADD_16, TwoRegisters(Index, SP), _, Elidability.Elidable, s3) ::
        ZLine(LD_16, TwoRegisters(SP, Index), _, Elidability.Elidable, s4) :: tail =>
        val sourceSize = (-negativeSize).&(0xffff).toInt
        if (mayAccessStackViaPointers(tail)) {
          return None
        }
        val usedOffsets: Set[Int] = findUsedOffsets(tail, Index match {
          case IX => MEM_IX_D
          case IY => MEM_IY_D
        })
        val targetSize = usedOffsets.size + usedOffsets.size.&(1)
        if (targetSize == sourceSize)  None else {
          val prologue = if (targetSize == 0) Nil else List(
            ZLine.register(PUSH, Index).pos(s1),
            ZLine.ldImm16(Index, 0x10000 - targetSize).pos(s2),
            ZLine.registers(ADD_16, Index, SP).pos(s3),
            ZLine.ld16(SP, Index).pos(s4))
          val map = usedOffsets.toSeq.sorted.zipWithIndex.toMap
          optimizeContinue(tail, Index, sourceSize, targetSize, map).map { optTail =>
            (name :: prologue ++ optTail, sourceSize, targetSize)
          }
        }
      case _ =>
        None
    }
  }

  def mayAccessStackViaPointers(code: List[ZLine]): Boolean = {
    import millfork.assembly.z80.ZOpcode._
    import millfork.node.ZRegister._
    val mayUsePointer = code.map {
      case ZLine0(_, TwoRegisters(HL, SP), _) => true
      case ZLine0(_, TwoRegisters(SP, HL), _) => true
      case ZLine0(LD_HLSP | LD_DESP, _, _) => true
      case ZLine0(_, TwoRegistersOffset(_, MEM_HL | MEM_BC | MEM_DE, offset), _) => true
      case ZLine0(_, TwoRegistersOffset(MEM_HL | MEM_BC | MEM_DE, _, offset), _) => true
      case ZLine0(CALL, _, _) => true
      case _ => false
    }.toArray
    val range = VariableLifetime.expandRangeToCoverLoops(code, mayUsePointer, stretchBackwards = false)
    if (range.nonEmpty) {
      val criticalCodeSlice = code.slice(range.start, range.end)
      if (criticalCodeSlice.exists {
        case ZLine0(_, TwoRegisters(_, SP), _) => true
        case ZLine0(_, TwoRegisters(SP, _), _) => true
        case ZLine0(LD_HLSP | LD_DESP, _, _) => true
        case ZLine0(EX_SP, _, _) => true
        case _ => false
      }) {
        return true
      }
    }
    false
  }

  def findUsedOffsets(code: List[ZLine], Mem: ZRegister.Value): Set[Int] = {
    code.flatMap {
      case ZLine0(_, OneRegisterOffset(Mem, offset), _) => Some(offset)
      case ZLine0(_, TwoRegistersOffset(_, Mem, offset), _) => Some(offset)
      case ZLine0(_, TwoRegistersOffset(Mem, _, offset), _) => Some(offset)
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
      case (head@ZLine0(_, TwoRegistersOffset(reg, Mem, offset), _)) :: tail =>
        optimizeContinue(tail, Index, sourceSize, targetSize, mapping).map(
          head.copy(registers = TwoRegistersOffset(reg, Mem, mapping(offset))) :: _)

      case (head@ZLine0(_, TwoRegistersOffset(Mem, reg, offset), _)) :: tail =>
        optimizeContinue(tail, Index, sourceSize, targetSize, mapping).map(
          head.copy(registers = TwoRegistersOffset(Mem, reg, mapping(offset))) :: _)

      case (head@ZLine0(_, OneRegisterOffset(Mem, offset), _)) :: tail =>
        optimizeContinue(tail, Index, sourceSize, targetSize, mapping).map(
          head.copy(registers = OneRegisterOffset(Mem, mapping(offset))) :: _)

      case
        ZLine(LD_16, TwoRegisters(Index, IMM_16), NumericConstant(size, _), _, s1) ::
          ZLine(ADD_16, TwoRegisters(Index, SP), _, _, s2) ::
          ZLine(LD_16, TwoRegisters(SP, Index), _, _, s3) ::
          ZLine(POP, OneRegister(Index), _, _, s4) :: tail =>
        if (size != sourceSize) None
        else {
          stripReturn(tail).flatMap {
            case (ret, rest) =>
              val epilogue = if (targetSize == 0) Nil else {
                List(
                  ZLine.ldImm16(Index, targetSize).pos(s1),
                  ZLine.registers(ADD_16, Index, SP).pos(s2),
                  ZLine.ld16(SP, Index).pos(s3),
                  ZLine.register(POP, Index).pos(s4))
              }
              optimizeContinue(rest, Index, sourceSize, targetSize, mapping).map(epilogue ++ ret ++ _)
          }
        }
      case
        ZLine(LD_16, TwoRegisters(HL, IMM_16), NumericConstant(size, _), _, s1) ::
          ZLine(ADD_16, TwoRegisters(HL, SP), _, _, s2) ::
          ZLine(LD_16, TwoRegisters(SP, HL), _, _, s3) ::
          ZLine(POP, OneRegister(Index), _, _, s4) :: tail =>
        if (size != sourceSize) {
          println("Mismatched stack frame sizes")
          None
        } else {
          stripReturn(tail).flatMap {
            case (ret, rest) =>
              val epilogue = if (targetSize == 0) Nil else {
                List(
                  ZLine.ldImm16(HL, targetSize).pos(s1),
                  ZLine.registers(ADD_16, HL, SP).pos(s2),
                  ZLine.ld16(SP, HL).pos(s3),
                  ZLine.register(POP, Index).pos(s4))
              }
              optimizeContinue(rest, Index, sourceSize, targetSize, mapping).map(epilogue ++ ret ++  _)
          }
        }
      case ZLine0(RET | RETI | RETN | BYTE, _, _) :: _ => None
      case ZLine0(JP, _, MemoryAddressConstant(f: NormalFunction)) :: _ => None
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
      case ZLine0(RET | RETI | RETN, _, _) => Some(potentialResult)
      case ZLine0(JP, NoRegisters, MemoryAddressConstant(f: NormalFunction)) => Some(potentialResult)
      case _ => None
    }
  }
}
