package millfork.assembly.m6809

import millfork.assembly.{AbstractCode, Elidability, SourceLine}
import millfork.env.{Constant, Label, MemoryVariable, NumericConstant, StackVariable, Variable, VariableInMemory}
import millfork.node.{M6809Register, Position}

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

  def longBranch(opcode: MOpcode.Value, label: Label): MLine = MLine(opcode, LongRelative, label.toAddress)

  def longBranch(opcode: MOpcode.Value, label: String): MLine = longBranch(opcode, Label(label))

  def tfr(source: M6809Register.Value, target: M6809Register.Value): MLine = MLine(TFR, TwoRegisters(source, target), Constant.Zero)

  def pp(opcode: MOpcode.Value, registers: M6809Register.Value*): MLine = MLine(opcode, RegisterSet(registers.toSet), Constant.Zero)

  def indexedS(opcode: MOpcode.Value, offset: Int = 0): MLine =
    MLine(opcode, Indexed(M6809Register.S, indirect = false), NumericConstant(offset, 1))

  def accessAndPullS(opcode: MOpcode.Value): MLine =
    MLine(opcode, PostIncremented(M6809Register.S, 1, indirect = false), Constant.Zero)

  def accessAndPullSTwice(opcode: MOpcode.Value): MLine =
    MLine(opcode, PostIncremented(M6809Register.S, 2, indirect = false), Constant.Zero)

  def inherent(opcode: MOpcode.Value): MLine = MLine(opcode, Inherent, Constant.Zero)

  def inherentA(opcode: MOpcode.Value): MLine = MLine(opcode, InherentA, Constant.Zero)

  def inherentB(opcode: MOpcode.Value): MLine = MLine(opcode, InherentB, Constant.Zero)

  def immediate(opcode: MOpcode.Value, param: Constant): MLine = MLine(opcode, Immediate, param)

  def immediate(opcode: MOpcode.Value, param: Int): MLine = MLine(opcode, Immediate, Constant(param))

  def absolute(opcode: MOpcode.Value, param: Constant): MLine = MLine(opcode, Absolute(false), param)

  def userstack(opcode: MOpcode.Value, offset: Int): MLine = MLine(opcode, Indexed(M6809Register.U, indirect = false), Constant(offset))

  def variable(opcode: MOpcode.Value, variable: Variable, offset: Int = 0): MLine = {
    variable match {
      case v: VariableInMemory => MLine.absolute(opcode, v.toAddress)
      case v: StackVariable => MLine(opcode, Indexed(M6809Register.U, indirect = false), NumericConstant(v.baseOffset, 1))
      case _ => ???
    }
  }
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
    if (addrMode == LongRelative) return s"    L$opcode $parameter"
    val suffix = addrMode match {
      case NonExistent => ""
      case Inherent => ""
      case InherentA => "A"
      case InherentB => "B"
      case Relative => s" $parameter"
      case Immediate => s" #$parameter"
      case DirectPage => s" <$parameter" // TODO: syntax
      case Absolute(false) => s" $parameter"
      case Absolute(true) => s" [$parameter]"
      case Indexed(base, false) => s" $parameter,$base"
      case Indexed(base, true) => s" [$parameter,$base]"
      case DAccumulatorIndexed(base, false) => " D,$base"
      case DAccumulatorIndexed(base, true) => " [D,$base]"
      case AAccumulatorIndexed(base, false) => " A,$base"
      case AAccumulatorIndexed(base, true) => " [A,$base]"
      case BAccumulatorIndexed(base, false) => " B,$base"
      case BAccumulatorIndexed(base, true) => " [B,$base]"
      case PreDecremented(base, 1, false) => s" ,-$base"
      case PreDecremented(base, 2, false) => s" ,--$base"
      case PreDecremented(base, 1, true) => s" [-$base]"
      case PreDecremented(base, 2, true) => s" [--$base]"
      case PostIncremented(base, 1, false) => s" ,$base+"
      case PostIncremented(base, 2, false) => s" ,$base++"
      case PostIncremented(base, 1, true) => s" [,$base+]"
      case PostIncremented(base, 2, true) => s" [,$base++]"
      case TwoRegisters(source, destination) => s" $source,$destination"
      case RegisterSet(set) =>
        import MOpcode._
        import M6809Register._
        set.toSeq
          .filterNot(r => set(D) && (r == A || r == B))
          .sortBy(x => -M6809Register.registerPushMask(x)).map(_.toString).mkString(" ", ",", "")
    }
    s"    $opcode$suffix"
  }
}

