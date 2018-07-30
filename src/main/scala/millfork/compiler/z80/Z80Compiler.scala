package millfork.compiler.z80

import java.util.concurrent.atomic.AtomicLong

import millfork.CompilationFlag
import millfork.assembly.z80.ZLine
import millfork.compiler.{AbstractCompiler, CompilationContext}
import millfork.env.{Label, NormalParamSignature}
import millfork.error.ErrorReporting
import millfork.node.ZRegister

/**
  * @author Karol Stasiak
  */
object Z80Compiler extends AbstractCompiler[ZLine] {

  private val labelCounter = new AtomicLong

  override def nextLabel(prefix: String): String = "." + prefix + "__" + labelCounter.incrementAndGet().formatted("%05d")

  override def compile(ctx: CompilationContext): List[ZLine] = {
    ctx.env.nameCheck(ctx.function.code)
    val chunk = Z80StatementCompiler.compile(ctx, ctx.function.code)
    val label = ZLine.label(Label(ctx.function.name)).copy(elidable = false)
    val storeParamsFromRegisters = ctx.function.params match {
      case NormalParamSignature(List(param)) if param.typ.size == 1 =>
        List(ZLine.ldAbs8(param.toAddress, ZRegister.A))
      case NormalParamSignature(List(param)) if param.typ.size == 2 =>
        if (ctx.options.flag(CompilationFlag.EmitIntel8080Opcodes)) {
          List(ZLine.ldAbs16(param.toAddress, ZRegister.HL))
        } else {
          List(
            ZLine.ld8(ZRegister.A, ZRegister.L),
            ZLine.ldAbs8(param.toAddress, ZRegister.A),
            ZLine.ld8(ZRegister.A, ZRegister.H),
            ZLine.ldAbs8(param.toAddress + 1, ZRegister.A))
        }
      case NormalParamSignature(List(param)) if param.typ.size == 3 =>
        import ZRegister._
        val p = param.toAddress
        if (ctx.options.flag(CompilationFlag.EmitIntel8080Opcodes)) {
          List(ZLine.ldAbs16(p, HL), ZLine.ld8(A, E), ZLine.ldAbs8(p + 2, A))
        } else {
          List(
            ZLine.ld8(A, L),
            ZLine.ldAbs8(p, A),
            ZLine.ld8(A, H),
            ZLine.ldAbs8(p + 1, A),
            ZLine.ld8(A, E),
            ZLine.ldAbs8(p + 2, A))
        }
      case NormalParamSignature(List(param)) if param.typ.size == 4 =>
        import ZRegister._
        val p = param.toAddress
        if (ctx.options.flag(CompilationFlag.EmitIntel8080Opcodes)) {
          // TODO: is this optimal?
          List(ZLine.ldAbs16(p, HL), ZLine.ld8(A, E), ZLine.ldAbs8(p + 2, A), ZLine.ld8(A, D), ZLine.ldAbs8(p + 3, A))
        } else {
          List(
            ZLine.ld8(A, L),
            ZLine.ldAbs8(p, A),
            ZLine.ld8(A, H),
            ZLine.ldAbs8(p + 1, A),
            ZLine.ld8(A, E),
            ZLine.ldAbs8(p + 2, A),
            ZLine.ld8(A, D),
            ZLine.ldAbs8(p + 3, A))
        }
      case _ => Nil
    }
    label :: (stackPointerFixAtBeginning(ctx) ++ storeParamsFromRegisters ++ chunk)
  }

  def stackPointerFixAtBeginning(ctx: CompilationContext): List[ZLine] = {
    val m = ctx.function
    if (m.stackVariablesSize == 0) return Nil
//    if (!ctx.options.flags(CompilationFlag.EmitZ80Opcodes)) {
//      ErrorReporting.error(s"Target CPU does not support stack variables", m.position)
//      return Nil
//    }
    if (m.stackVariablesSize > 127) {
      ErrorReporting.error(s"Function ${m.name} has too many local stack variables", m.position)
      return Nil
    }
    import millfork.assembly.z80.ZOpcode._
    import ZRegister._
    val localVariableArea = ctx.function.stackVariablesSize.&(1).+(ctx.function.stackVariablesSize)
    if (ctx.options.flag(CompilationFlag.UseIxForStack)) {
      List(
        ZLine.register(PUSH, IX),
        ZLine.ldImm16(IX, 0x10000 - localVariableArea),
        ZLine.registers(ADD_16, IX, SP),
        ZLine.ld16(SP, IX))
    } else if (localVariableArea == 2) {
      // cycles: 11
      // bytes: 1
      List(ZLine.register(PUSH, HL))
    } else if (localVariableArea == 4) {
      // cycles: 22
      // bytes: 2
      List(ZLine.register(PUSH, HL), ZLine.register(PUSH, HL))
    } else {
      val preserveHL = ctx.function.params match {
        case NormalParamSignature(List(param)) => param.typ.size == 2
        case _ => false
      }
      val threshold = if (ctx.options.flag(CompilationFlag.OptimizeForSpeed)) {
        // at LVA=6, PUSH is 33 cycles
        if (preserveHL) 7 else 5
      } else if (ctx.options.flag(CompilationFlag.OptimizeForSize)) {
        // at LVA=10, PUSH is 5 bytes but more cycles
        // at LVA=14, PUSH is 7 bytes but more cycles
        if (preserveHL) 13 else 9
      } else {
        // 44ø 4B is better than 35ø 7B
        // 33ø 3B is better than 27ø 5B
        if (preserveHL) 9 else 7
      }
      if (localVariableArea < threshold) {
        List.fill(localVariableArea >> 1)(ZLine.register(PUSH, HL))
      } else if (preserveHL) {
        // cycles: 4 + 10 + 11 + 6 + 4 = 35
        // bytes: 7
        List(
          ZLine.implied(EX_DE_HL),
          ZLine.ldImm16(HL, 0x10000 - localVariableArea),
          ZLine.registers(ADD_16, HL, SP),
          ZLine.ld16(SP, HL),
          ZLine.implied(EX_DE_HL))
      } else {
        // cycles: 10 + 11 + 6 = 27
        // bytes: 5
        List(
          ZLine.ldImm16(HL, 0x10000 - localVariableArea),
          ZLine.registers(ADD_16, HL, SP),
          ZLine.ld16(SP, HL))
      }
    }
  }
}
