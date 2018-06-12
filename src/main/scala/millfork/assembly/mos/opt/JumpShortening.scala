package millfork.assembly.mos.opt

import millfork.assembly.mos.AssemblyLine
import millfork.{CompilationFlag, CompilationOptions}
import millfork.assembly.AddrMode
import millfork.assembly.mos.Opcode._
import millfork.env.{Label, MemoryAddressConstant, NormalFunction}
import millfork.error.ErrorReporting

/**
  * @author Karol Stasiak
  */
object JumpShortening {

  def validShortJump(thisOffset: Int, labelOffset: Int): Boolean = {
    val distance = labelOffset - (thisOffset + 2)
    distance.toByte == distance
  }

  def apply(f: NormalFunction, code: List[AssemblyLine], options: CompilationOptions): List[AssemblyLine] = {
    val offsets = new Array[Int](code.length)
    var o = 0
    code.zipWithIndex.foreach{
      case (line, ix) =>
        offsets(ix) = o
        o += line.sizeInBytes
    }
    val labelOffsets = code.zipWithIndex.flatMap {
      case (AssemblyLine(LABEL, _, MemoryAddressConstant(Label(label)), _), ix) => Some(label -> offsets(ix))
      case _ => None
    }.toMap
    val cmos = options.flags(CompilationFlag.EmitCmosOpcodes)
    if (cmos) {
      code.zipWithIndex.map {
        case (line@AssemblyLine(JMP, AddrMode.Absolute, MemoryAddressConstant(Label(label)), true), ix) =>
          labelOffsets.get(label).fold(line) { labelOffset =>
            val thisOffset = offsets(ix)
            if (validShortJump(thisOffset, labelOffset)) {
              val result = line.copy(opcode = BRA, addrMode = AddrMode.Relative)
              ErrorReporting.debug("Changing branch from long to short")
              ErrorReporting.trace(line.toString)
              ErrorReporting.trace("     ↓")
              ErrorReporting.trace(result.toString)
              result
            } else line
          }
          case (line, _) => line
      }
    } else {
      FlowAnalyzer.analyze(f, code, options, FlowInfoRequirement.ForwardFlow).zipWithIndex.map {
        case ((info, line@AssemblyLine(JMP, AddrMode.Absolute, MemoryAddressConstant(Label(label)), _)), ix) =>
          labelOffsets.get(label).fold(line) { labelOffset =>
            val thisOffset = offsets(ix)
            if (validShortJump(thisOffset, labelOffset)) {
              val bra =
                if (info.statusBefore.z.contains(true)) BEQ
                else if (info.statusBefore.z.contains(false)) BNE
                else if (info.statusBefore.n.contains(true)) BMI
                else if (info.statusBefore.n.contains(false)) BPL
                else if (info.statusBefore.v.contains(true)) BVS
                else if (info.statusBefore.v.contains(false)) BVC
                else if (info.statusBefore.c.contains(true)) BCS
                else if (info.statusBefore.c.contains(false)) BCC
                else JMP
              if (bra != JMP) {
                val result = line.copy(opcode = bra, addrMode = AddrMode.Relative)
                ErrorReporting.debug("Changing branch from long to short")
                ErrorReporting.trace(line.toString)
                ErrorReporting.trace("     ↓")
                ErrorReporting.trace(result.toString)
                result
              } else line
            } else line
          }
        case ((_, line), _) => line
      }
    }
  }
}
