package millfork.compiler

import millfork.{CompilationFlag, CompilationOptions}
import millfork.assembly._
import millfork.env._
import millfork.node._
import millfork.assembly.Opcode._
import millfork.assembly.AddrMode._
import millfork.error.ErrorReporting

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import millfork.assembly.AssemblyLine
import millfork.env.{NumericConstant, RegisterVariable, Type}
import millfork.error.ErrorReporting
import millfork.node.{Expression, Register}

/**
  * @author Karol Stasiak
  */
object DecimalBuiltIns {
  def compileByteShiftLeft(ctx: CompilationContext, l: Expression, r: Expression, rotate: Boolean): List[AssemblyLine] = {
    val b = ctx.env.get[Type]("byte")
    ctx.env.eval(r) match {
      case Some(NumericConstant(0, _)) =>
        Nil
      case Some(NumericConstant(v, _)) =>
        val addition = BuiltIns.compileAddition(ctx, List.fill(1 << v)(false -> l), decimal = true)
        if (rotate) addition.filterNot(_.opcode == CLC) else addition
      case _ =>
        ErrorReporting.error("Cannot shift by a non-constant amount")
        Nil
    }
  }

  def compileByteShiftRight(ctx: CompilationContext, l: Expression, r: Expression, rotate: Boolean): List[AssemblyLine] = {
    val b = ctx.env.get[Type]("byte")
    MfCompiler.compile(ctx, l, Some((b, RegisterVariable(Register.A, b))), BranchSpec.None) ++ (ctx.env.eval(r) match {
      case Some(NumericConstant(0, _)) =>
        Nil
      case Some(NumericConstant(v, _)) =>
        List.fill(v.toInt) {
          shiftOrRotateAccumulatorRight(ctx, rotate)
        }.flatten
      case _ =>
        ErrorReporting.error("Cannot shift by a non-constant amount")
        Nil
    })
  }

  private def shiftOrRotateAccumulatorRight(ctx: CompilationContext, rotate: Boolean) = {
    val constantLabel = MfCompiler.nextLabel("c8")
    val skipHiDigit = MfCompiler.nextLabel("ds")
    val skipLoDigit = MfCompiler.nextLabel("ds")
    val cmos = ctx.options.flags(CompilationFlag.EmitCmosOpcodes)
    val bit = if (cmos) {
      AssemblyLine.immediate(BIT, 8)
    } else {
      AssemblyLine.absolute(BIT, Label(constantLabel))
    }
    List(
      if (rotate) AssemblyLine.implied(ROR) else AssemblyLine.implied(LSR),
      AssemblyLine(LABEL, DoesNotExist, Label(constantLabel).toAddress, elidable = cmos),
      AssemblyLine(PHP, Implied, Constant.Zero, elidable = cmos),
      AssemblyLine.relative(BPL, skipHiDigit),
      AssemblyLine.implied(SEC),
      AssemblyLine.immediate(SBC, 0x30),
      AssemblyLine.label(skipHiDigit),
      bit,
      AssemblyLine.relative(BCS, skipLoDigit),
      AssemblyLine.implied(SEC),
      AssemblyLine.immediate(SBC, 0x3),
      AssemblyLine.label(skipLoDigit),
      AssemblyLine.implied(PLP))
  }

  def compileInPlaceLongShiftLeft(ctx: CompilationContext, l: LhsExpression, r: Expression): List[AssemblyLine] = {
    ctx.env.eval(r) match {
      case Some(NumericConstant(0, _)) =>
        Nil
      case Some(NumericConstant(v, _)) =>
        List.fill(v.toInt)(BuiltIns.compileInPlaceWordOrLongAddition(ctx, l, l, decimal = true, subtract = false)).flatten
      case _ =>
        ErrorReporting.error("Cannot shift by a non-constant amount")
        Nil
    }
  }

  def compileInPlaceLongShiftRight(ctx: CompilationContext, l: LhsExpression, r: Expression): List[AssemblyLine] = {
    val targetBytes: List[List[AssemblyLine]] = l match {
      case v: VariableExpression =>
        val variable = ctx.env.get[Variable](v.name)
        List.tabulate(variable.typ.size) { i => AssemblyLine.variable(ctx, STA, variable, i) }
      case SeparateBytesExpression(h: VariableExpression, l: VariableExpression) =>
        val lv = ctx.env.get[Variable](l.name)
        val hv = ctx.env.get[Variable](h.name)
        List(
          AssemblyLine.variable(ctx, STA, lv),
          AssemblyLine.variable(ctx, STA, hv))
    }
    ctx.env.eval(r) match {
      case Some(NumericConstant(v, _)) =>
        val size = targetBytes.length
        List.fill(v.toInt) {
          List.tabulate(size) { i =>
            BuiltIns.staTo(LDA, targetBytes(size - 1 - i)) ++
              shiftOrRotateAccumulatorRight(ctx, i != 0) ++
              targetBytes(size - 1 - i)
          }.flatten
        }.flatten
      case _ =>
        ErrorReporting.error("Cannot shift by a non-constant amount")
        Nil
    }
  }

}
