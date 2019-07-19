package millfork.assembly.mos.opt

import java.util.concurrent.atomic.AtomicInteger

import millfork.CompilationOptions
import millfork.assembly.Elidability
import millfork.assembly.mos.Opcode._
import millfork.assembly.mos.{AddrMode, AssemblyLine, AssemblyLine0, Opcode}
import millfork.env.{Label, MemoryAddressConstant, NormalFunction, ThingInMemory}

/**
  * @author Karol Stasiak
  */
object JumpFixing {
  val counter = new AtomicInteger(80000)

  def generateNextLabel() = f".lj${counter.getAndIncrement()}%05d"

  def invalidShortJump(thisOffset: Int, labelOffset: Int): Boolean = {
    val distance = labelOffset - (thisOffset + 2)
    distance.toByte != distance
  }

  private def negate(opcode: Opcode.Value) = opcode match {
    case BEQ => BNE
    case BNE => BEQ
    case BCC => BCS
    case BCS => BCC
    case BVC => BVS
    case BVS => BVC
    case BMI => BPL
    case BPL => BMI
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
      case (AssemblyLine0(LABEL, _, MemoryAddressConstant(Label(label))), ix) => Some(label -> offsets(ix))
      case _ => None
    }.toMap
    var changed = false

    def makeLong(line: AssemblyLine) = {
      changed = true
      val op = line.opcode
      val long: List[AssemblyLine] = op match {
        case BRA => List(line.copy(opcode = JMP, addrMode = AddrMode.Absolute))
        case BSR => List(line.copy(opcode = JSR, addrMode = AddrMode.Absolute))
        case _ =>
          val label = generateNextLabel()
          List(
            AssemblyLine.relative(negate(op), label),
            line.copy(opcode = JMP, addrMode = AddrMode.Absolute),
            AssemblyLine.label(label)
          )
      }
      options.log.debug("Changing branch from short to long")
      if (options.log.traceEnabled) {
        options.log.trace(line.toString)
        options.log.trace("     ↓")
        long.foreach(l => options.log.trace(l.toString))
      }
      long
    }

    val result = code.zipWithIndex.flatMap {
      case (line@AssemblyLine(_, AddrMode.Relative, MemoryAddressConstant(th: ThingInMemory), Elidability.Elidable, _), ix) =>
        labelOffsets.get(th.name) match {
          case None => makeLong(line)
          case Some(labelOffset) =>
            val thisOffset = offsets(ix)
            if (invalidShortJump(thisOffset, labelOffset)) makeLong(line)
            else List(line)
        }
      case (line@AssemblyLine(_, AddrMode.Relative, _, Elidability.Elidable, _), _) =>
        makeLong(line)
      case (line, _) => List(line)
    }
    if (changed) apply(f, result, options) else result
  }


}
