package millfork.assembly.m6809.opt

import millfork.CompilationOptions
import millfork.assembly.m6809.{LongRelative, MLine, MLine0, Relative}
import millfork.env.{Label, MemoryAddressConstant, NormalFunction, ThingInMemory}
import millfork.assembly.m6809.MOpcode._

/**
  * @author Karol Stasiak
  */
object JumpFixing {

  def validShortJump(thisOffset: Int, labelOffset: Int): Boolean = {
    // TODO
    val distance = labelOffset - (thisOffset + 2)
    distance.toByte == distance
  }

  def apply(f: NormalFunction, code: List[MLine], options: CompilationOptions): List[MLine] = {
    val offsets = new Array[Int](code.length)
    var o = 0
    code.zipWithIndex.foreach{
      case (line, ix) =>
        offsets(ix) = o
        o += line.sizeInBytes
    }
    val labelOffsets = code.zipWithIndex.flatMap {
      case (MLine0(LABEL, _, MemoryAddressConstant(Label(label))), ix) => Some(label -> offsets(ix))
      case _ => None
    }.toMap
    var changed = false

    def makeLong(line: MLine): MLine = {
      changed = true
      val newLine = line.copy(addrMode = LongRelative)
      options.log.debug("Changing branch from short to long")
      if (options.log.traceEnabled) {
        options.log.trace(line.toString)
        options.log.trace("     ↓")
        options.log.trace(newLine.toString)
      }
      newLine
    }
    def makeShort(line: MLine): MLine = {
      changed = true
      val newLine = line.copy(addrMode = Relative)
      options.log.debug("Changing branch from long to short")
      if (options.log.traceEnabled) {
        options.log.trace(line.toString)
        options.log.trace("     ↓")
        options.log.trace(newLine.toString)
      }
      newLine
    }

    import millfork.assembly.Elidability.Elidable
    import millfork.assembly.Elidability.Volatile
    val result = code.zipWithIndex.map {
      case (line@MLine(_, Relative, MemoryAddressConstant(th: ThingInMemory), Elidable | Volatile, _), ix) =>
        labelOffsets.get(th.name) match {
          case None =>
            changed = true
            line.copy(addrMode = LongRelative)
          case Some(labelOffset) =>
            val thisOffset = offsets(ix)
            if (!validShortJump(thisOffset, labelOffset)) makeLong(line) else line
        }
      case (line@MLine(_, Relative, _, Elidable | Volatile, _), _) =>
        makeLong(line)
      case (line@MLine(_, LongRelative, MemoryAddressConstant(th: ThingInMemory), Elidable | Volatile, _), ix) =>
        labelOffsets.get(th.name) match {
          case None => line
          case Some(labelOffset) =>
            val thisOffset = offsets(ix)
            if (validShortJump(thisOffset, labelOffset)) makeShort(line) else line
        }
      case (line, _) => line
    }
    if (changed) apply(f, result, options) else result
  }
}
