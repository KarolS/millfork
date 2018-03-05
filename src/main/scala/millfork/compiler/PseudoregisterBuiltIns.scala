package millfork.compiler

import millfork.CompilationFlag
import millfork.assembly.AssemblyLine
import millfork.env._
import millfork.error.ErrorReporting
import millfork.node._
import millfork.assembly.Opcode
import millfork.assembly.Opcode._
import millfork.assembly.AddrMode._

/**
  * @author Karol Stasiak
  */
object PseudoregisterBuiltIns {

  def compileWordShiftOps(left: Boolean, ctx: CompilationContext, l: Expression, r: Expression): List[AssemblyLine] = {
    if (!ctx.options.flag(CompilationFlag.ZeropagePseudoregister)) {
      ErrorReporting.error("Word shifting requires the zeropage pseudoregister", l.position)
      return Nil
    }
    val b = ctx.env.get[Type]("byte")
    val w = ctx.env.get[Type]("word")
    val reg = ctx.env.get[VariableInMemory]("__reg")
    val firstParamCompiled = ExpressionCompiler.compile(ctx, l, Some(w -> reg), NoBranching)
    ctx.env.eval(r) match {
      case Some(NumericConstant(0, _)) =>
        Nil
      case Some(NumericConstant(v, _)) if v > 0 =>
        if (ctx.options.flag(CompilationFlag.EmitNative65816Opcodes)) {
          firstParamCompiled ++
            List(AssemblyLine.accu16) ++
            List.fill(v.toInt)(if (left) AssemblyLine.zeropage(ASL, reg) else AssemblyLine.zeropage(LSR, reg)) ++
            List(AssemblyLine.accu8, AssemblyLine.zeropage(LDA, reg), AssemblyLine.zeropage(LDX, reg, 1))
        } else {
          val cycle =
            if (left) List(AssemblyLine.zeropage(ASL, reg), AssemblyLine.zeropage(ROL, reg, 1))
            else List(AssemblyLine.zeropage(LSR, reg, 1), AssemblyLine.zeropage(ROR, reg))
          firstParamCompiled ++ List.fill(v.toInt)(cycle).flatten ++ List(AssemblyLine.zeropage(LDA, reg), AssemblyLine.zeropage(LDX, reg, 1))
        }
      case _ =>
        ErrorReporting.error("Cannot shift by a non-constant amount")
        Nil
    }
  }

  def compileByteMultiplication(ctx: CompilationContext, param1OrRegister: Option[Expression], param2: Expression, storeInRegLo: Boolean): List[AssemblyLine] = {
    if (!ctx.options.flag(CompilationFlag.ZeropagePseudoregister)) {
      ErrorReporting.error("Variable byte multiplication requires the zeropage pseudoregister", param1OrRegister.flatMap(_.position))
      return Nil
    }
    val b = ctx.env.get[Type]("byte")
    val w = ctx.env.get[Type]("word")
    val reg = ctx.env.get[VariableInMemory]("__reg")
    val load: List[AssemblyLine] = param1OrRegister match {
      case Some(param1) =>
        val code1 = ExpressionCompiler.compile(ctx, param1, Some(b -> RegisterVariable(Register.A, b)), BranchSpec.None)
        val code2 = ExpressionCompiler.compile(ctx, param2, Some(b -> RegisterVariable(Register.A, b)), BranchSpec.None)
        if (!usesRegLo(code2)) {
          code1 ++ List(AssemblyLine.zeropage(STA, reg)) ++ code2 ++ List(AssemblyLine.zeropage(STA, reg, 1))
        } else if (!usesRegLo(code1)) {
          code2 ++ List(AssemblyLine.zeropage(STA, reg)) ++ code1 ++ List(AssemblyLine.zeropage(STA, reg, 1))
        } else {
          code1 ++ List(AssemblyLine.implied(PHA)) ++ code2 ++ List(
            AssemblyLine.zeropage(STA, reg),
            AssemblyLine.implied(PLA),
            AssemblyLine.zeropage(STA, reg, 1)
          )
        }
      case None =>
        val code2 = ExpressionCompiler.compile(ctx, param2, Some(b -> RegisterVariable(Register.A, b)), BranchSpec.None)
        if (!usesRegLo(code2)) {
          List(AssemblyLine.zeropage(STA, reg)) ++ code2 ++ List(AssemblyLine.zeropage(STA, reg, 1))
        } else if (!usesRegHi(code2)) {
          List(AssemblyLine.zeropage(STA, reg, 1)) ++ code2 ++ List(AssemblyLine.zeropage(STA, reg))
        } else {
          List(AssemblyLine.implied(PHA)) ++ code2 ++ List(
            AssemblyLine.zeropage(STA, reg),
            AssemblyLine.implied(PLA),
            AssemblyLine.zeropage(STA, reg, 1)
          )
        }
    }
    val calculate = AssemblyLine.absoluteOrLongAbsolute(JSR, ctx.env.get[FunctionInMemory]("__mul_u8u8u8"), ctx.options) ::
      (if (storeInRegLo) List(AssemblyLine.zeropage(STA, reg)) else Nil)
    load ++ calculate
  }

  private def simplicity(env: Environment, expr: Expression): Char = {
    val constPart = env.eval(expr) match {
      case Some(NumericConstant(_, _)) => 'Z'
      case Some(_) => 'Y'
      case None => expr match {
        case VariableExpression(_) => 'V'
        case IndexedExpression(_, LiteralExpression(_, _)) => 'K'
        case IndexedExpression(_, VariableExpression(_)) => 'J'
        case IndexedExpression(_, _) => 'I'
        case _ => 'A'
      }
    }
    constPart
  }

  def usesRegLo(code: List[AssemblyLine]): Boolean = code.forall{
    case AssemblyLine(JSR | BSR | TCD | TDC, _, _, _) => true
    case AssemblyLine(_, _, MemoryAddressConstant(th), _) if th.name == "__reg" => true
    case _ => false
  }

  def usesRegHi(code: List[AssemblyLine]): Boolean = code.forall{
    case AssemblyLine(JSR | BSR | TCD | TDC, _, _, _) => true
    case AssemblyLine(_, _, CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(1, _)), _) if th.name == "__reg" => true
    case _ => false
  }
}
