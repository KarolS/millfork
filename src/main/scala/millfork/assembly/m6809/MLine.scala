package millfork.assembly.m6809

import millfork.assembly.{AbstractCode, Elidability, SourceLine}
import millfork.env.{Constant, Label}
import millfork.node.Position

/**
  * @author Karol Stasiak
  */

object MLine0 {

  @inline
  def unapply(a: MLine): Some[(MOpcode.Value, MAddrMode, Constant)] = Some(a.opcode, a.addrMode, a.parameter)
}

object MLine {

  import MOpcode._

  def label(label: String): MLine = MLine.label(Label(label))

  def label(label: Label): MLine = MLine(LABEL, NonExistent, label.toAddress)

  def inherent(opcode: MOpcode.Value): MLine = MLine(opcode, Inherent, Constant.Zero)

  def inherentA(opcode: MOpcode.Value): MLine = MLine(opcode, InherentA, Constant.Zero)

  def inherentB(opcode: MOpcode.Value): MLine = MLine(opcode, InherentB, Constant.Zero)
}

case class MLine(opcode: MOpcode.Value, addrMode: MAddrMode, parameter: Constant, elidability: Elidability.Value = Elidability.Elidable, source: Option[SourceLine] = None) extends AbstractCode {

  def pos(s: Option[SourceLine]): MLine = if (s.isEmpty || s == source) this else this.copy(source = s)

  def pos(s1: Option[SourceLine], s2: Option[SourceLine]): MLine = pos(Seq(s1, s2))

  def position(s: Option[Position]): MLine = pos(SourceLine.of(s))

  def positionIfEmpty(s: Option[Position]): MLine = if (s.isEmpty || source.isDefined) this else pos(SourceLine.of(s))

  def pos(s: Seq[Option[SourceLine]]): MLine = pos(SourceLine.merge(s))

  def mergePos(s: Seq[Option[SourceLine]]): MLine = if (s.isEmpty) this else pos(SourceLine.merge(this.source, s))

  def refersTo(name: String): Boolean = parameter.refersTo(name)

  override def sizeInBytes: Int = 1 // TODO

  override def isPrintable: Boolean = true // TODO

  override def toString: String = {
    if (opcode == MOpcode.LABEL) return parameter + ":"
    if (opcode == MOpcode.BYTE) return s"    FCB $parameter"
    val suffix = addrMode match {
      case NonExistent => ""
      case Inherent => ""
      case InherentA => "A"
      case InherentB => "B"
      case Relative => s" $parameter"
      case Absolute(false) => s" $parameter"
      case Indexed(base, false) => s" $parameter,$base"
      case Indexed(base, true) => s" [$parameter,$base]"
      case PreDecremented(base, 1, false) => s" ,-$base"
      case PreDecremented(base, 2, false) => s" ,--$base"
      case PreDecremented(base, 1, true) => s" [-$base]"
      case PreDecremented(base, 2, true) => s" [--$base]"
      case PostIncremented(base, 1, false) => s" ,$base+"
      case PostIncremented(base, 2, false) => s" ,$base++"
      case PostIncremented(base, 1, true) => s" [,$base+]"
      case PostIncremented(base, 2, true) => s" [,$base++]"
    }
    s"    $opcode$suffix"
  }
}

