package millfork.assembly.z80.opt

import millfork.{CompilationFlag, CompilationOptions}
import millfork.assembly.OptimizationContext
import millfork.assembly.z80.ZOpcode._
import millfork.assembly.z80._
import millfork.env.{Label, MemoryAddressConstant, NormalFunction}

/**
  * @author Karol Stasiak
  */
object JumpShortening {

  def validShortJump(thisOffset: Int, labelOffset: Int): Boolean = {
    val distance = labelOffset - (thisOffset + 2)
    // TODO: I don't trust the instruction size calculations
    // distance.toByte == distance
    distance.abs < 100
  }

  def apply(f: NormalFunction, code: List[ZLine], options: CompilationOptions): List[ZLine] = {
    if (!options.flags(CompilationFlag.EmitExtended80Opcodes)) return code
    val shouldOptimizeAlways = options.flag(CompilationFlag.EmitSharpOpcodes) || options.flag(CompilationFlag.OptimizeForSize)
    val bePessimistic = options.flag(CompilationFlag.OptimizeForSpeed)
    val offsets = new Array[Int](code.length)
    var o = 0
    code.zipWithIndex.foreach{
      case (line, ix) =>
        offsets(ix) = o
        o += line.sizeInBytes
    }
    val labelOffsets = code.zipWithIndex.flatMap {
      case (ZLine(LABEL, _, MemoryAddressConstant(Label(label)), _), ix) => Some(label -> offsets(ix))
      case _ => None
    }.toMap
    code.zipWithIndex.map {
      case (line@ZLine(JP, NoRegisters | IfFlagSet(ZFlag.Z | ZFlag.C) | IfFlagClear(ZFlag.Z | ZFlag.C), MemoryAddressConstant(Label(label)), true), ix) =>
        labelOffsets.get(label).fold(line) { labelOffset =>
          val thisOffset = offsets(ix)
          val willBeTaken  = line.registers == NoRegisters ||
            labelOffset < thisOffset ||  // jumping back means a loopty-loop TODO: better heuristics
            bePessimistic // assume jump forward will happen if compiling with -Of
          val willBeBetter = shouldOptimizeAlways || !willBeTaken
          if (willBeBetter && validShortJump(thisOffset, labelOffset)) {
            val result = line.copy(opcode = JR)
            options.log.debug("Changing branch from long to short")
            if (options.log.traceEnabled) {
              options.log.trace(line.toString)
              options.log.trace("     ↓")
              options.log.trace(result.toString)
            }
            result
          } else line
        }
      case (line, _) => line
    }
  }
}
