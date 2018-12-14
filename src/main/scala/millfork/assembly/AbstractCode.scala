package millfork.assembly

import millfork.env.Constant
import millfork.node.Position

/**
  * @author Karol Stasiak
  */
trait AbstractCode {
  def sizeInBytes: Int

  def isPrintable: Boolean

  def parameter: Constant

  def source: Option[SourceLine]
}

object SourceLine {
  val MultipleFiles: SourceLine = SourceLine("", 0)

  def merge(line: Option[SourceLine], lines: Seq[Option[SourceLine]]): Option[SourceLine] = {
    line match {
      case Some(l) =>
        Some(lines.flatten.foldLeft(l)(_ ~ _))
      case None =>
        lines.flatten.reduceOption(_ ~ _)
    }
  }

  def merge(lines: Seq[Option[SourceLine]]): Option[SourceLine] = {
    lines.flatten.reduceOption(_ ~ _)
  }

  def of(pos: Option[Position]): Option[SourceLine] = pos.map(p => SourceLine(p.moduleName, p.line))
}

case class SourceLine(moduleName: String, line: Int) {
  def ~(that: SourceLine): SourceLine = if (this.moduleName == that.moduleName) SourceLine(moduleName, line min that.line) else SourceLine.MultipleFiles

  def ~(that: Option[SourceLine]): SourceLine = that.fold(this)(this ~ _)
}

object Elidability extends Enumeration {
  val Elidable, Volatile, Fixed = Value
}
