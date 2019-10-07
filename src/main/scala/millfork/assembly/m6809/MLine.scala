package millfork.assembly.m6809

import millfork.CompilationFlag
import millfork.assembly.{AbstractCode, Elidability, SourceLine}
import millfork.compiler.CompilationContext
import millfork.env.{Constant, Label, MemoryVariable, NumericConstant, StackVariable, ThingInMemory, Variable, VariableInMemory}
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

  def indexedX(opcode: MOpcode.Value, offset: Constant): MLine =
    MLine(opcode, Indexed(M6809Register.X, indirect = false), offset)

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

  def absolute(opcode: MOpcode.Value, param: ThingInMemory): MLine = MLine(opcode, Absolute(false), param.toAddress)

  //def userstack(opcode: MOpcode.Value, offset: Int): MLine = MLine(opcode, Indexed(M6809Register.U, indirect = false), Constant(offset))

  def variablestack(ctx: CompilationContext, opcode: MOpcode.Value, offset: Int): MLine = {
    if (ctx.options.flag(CompilationFlag.UseUForStack)) MLine(opcode, Indexed(M6809Register.U, indirect = false), Constant(offset))
    else if (ctx.options.flag(CompilationFlag.UseYForStack)) MLine(opcode, Indexed(M6809Register.Y, indirect = false), Constant(offset))
    else MLine(opcode, Indexed(M6809Register.S, indirect = false), Constant(ctx.extraStackOffset + offset))
  }

  def variable(ctx: CompilationContext, opcode : MOpcode.Value, variable: Variable, offset: Int = 0): MLine = {
    variable match {
      case v: VariableInMemory => MLine.absolute(opcode, v.toAddress)
      case v: StackVariable =>
        if (ctx.options.flag(CompilationFlag.UseUForStack)) MLine(opcode, Indexed(M6809Register.U, indirect = false), NumericConstant(v.baseOffset, 1))
        else if (ctx.options.flag(CompilationFlag.UseYForStack)) MLine(opcode, Indexed(M6809Register.Y, indirect = false), NumericConstant(v.baseOffset, 1))
        else MLine(opcode, Indexed(M6809Register.S, indirect = false), NumericConstant(v.baseOffset + ctx.extraStackOffset, 1))
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
      case DAccumulatorIndexed(base, false) => s" D,$base"
      case DAccumulatorIndexed(base, true) => s" [D,$base]"
      case AAccumulatorIndexed(base, false) => s" A,$base"
      case AAccumulatorIndexed(base, true) => s" [A,$base]"
      case BAccumulatorIndexed(base, false) => s" B,$base"
      case BAccumulatorIndexed(base, true) => s" [B,$base]"
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

  def changesRegister(reg: M6809Register.Value): Boolean = {
    import M6809Register._
    def overlaps(other: M6809Register.Value): Boolean = {
      if (reg == D && (other == A || other == B))  true
      else if (other == D && (reg == A || reg == B))  true
      else reg == other
    }
    import MOpcode._
    (opcode, addrMode) match {
      case (_, InherentA) => overlaps(A)
      case (_, InherentB) => overlaps(B)
      case (PULU, set:RegisterSet) => reg == U || set.contains(reg)
      case (PULS, set:RegisterSet) => reg == S || set.contains(reg)
      case (PSHS, _) => reg == U
      case (PSHU, _) => reg == S
      case (TFR, TwoRegisters(_, dest)) => overlaps(dest)
      case (EXG, TwoRegisters(r1, r2)) => overlaps(r1) || overlaps(r2)
      case (op, _) if MOpcode.ChangesAAlways(op) => overlaps(A) || addrMode.changesRegister(reg)
      case (op, _) if MOpcode.ChangesBAlways(op) => overlaps(B) || addrMode.changesRegister(reg)
      case (LDB, _) => overlaps(B) || addrMode.changesRegister(reg)
      case (LDD, _) => overlaps(D) || addrMode.changesRegister(reg)
      case (LDU | LEAU, _) => reg == U || addrMode.changesRegister(reg)
      case (LDS | LEAS, _) => reg == S || addrMode.changesRegister(reg)
      case (LDX | LEAX, _) => reg == X || addrMode.changesRegister(reg)
      case (LDY | LEAY, _) => reg == Y || addrMode.changesRegister(reg)
      case (MUL, _) => overlaps(D)
      case (ABX, _) => reg == X
      case (NOP | SWI | SWI2 | SWI3 | SYNC, _) => false
      case _ => true // TODO
    }
  }
}

