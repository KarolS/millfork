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
        val addition = BuiltIns.compileAddition(ctx, List.fill(1<<v)(false -> l), decimal = true)
        if (rotate) addition.filterNot(_.opcode == CLC) else addition
      case _ =>
        ErrorReporting.error("Cannot shift by a non-constant amount")
        Nil
    }
  }

  def compileByteShiftRight(ctx: CompilationContext, l: Expression, r: Expression, rotate: Boolean): List[AssemblyLine] = {
    val b = ctx.env.get[Type]("byte")
    ctx.env.eval(r) match {
      case Some(NumericConstant(0, _)) =>
        Nil
      case Some(NumericConstant(1, _)) =>
        val constantLabel = MlCompiler.nextLabel("c8")
        val skipHiDigit = MlCompiler.nextLabel("ds")
        val skipLoDigit = MlCompiler.nextLabel("ds")
        val bit = if (ctx.options.flags(CompilationFlag.EmitCmosOpcodes)) {
          AssemblyLine.immediate(BIT, 8)
        } else {
          AssemblyLine.absolute(BIT, Label(constantLabel))
        }
        List(
          if (rotate) AssemblyLine.implied(ROR) else AssemblyLine.implied(LSR),
          AssemblyLine.label(constantLabel),
          AssemblyLine.implied(PHP),
          AssemblyLine.relative(BPL, skipHiDigit),
          AssemblyLine.implied(SEC),
          AssemblyLine.immediate(SBC, 0x30),
          AssemblyLine.label(skipHiDigit),
          bit,
          AssemblyLine.relative(BPL, skipLoDigit),
          AssemblyLine.implied(SEC),
          AssemblyLine.immediate(SBC, 0x3),
          AssemblyLine.label(skipLoDigit),
          AssemblyLine.implied(PLP))
      case _ =>
        ErrorReporting.error("Cannot shift by a non-constant amount")
        Nil
    }
  }

}
