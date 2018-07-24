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
        List(ZLine.ldAbs16(param.toAddress, ZRegister.HL))
      case _ => Nil
    }
    label :: (stackPointerFixAtBeginning(ctx) ++ storeParamsFromRegisters ++ chunk)
  }

  def stackPointerFixAtBeginning(ctx: CompilationContext): List[ZLine] = {
    val m = ctx.function
    if (m.stackVariablesSize == 0) return Nil
    if (!ctx.options.flags(CompilationFlag.EmitZ80Opcodes)) {
      ErrorReporting.error(s"Target CPU does not support stack variables", m.position)
      return Nil
    }
    if (m.stackVariablesSize > 127) {
      ErrorReporting.error(s"Function ${m.name} has too many local stack variables", m.position)
      return Nil
    }
    import millfork.assembly.z80.ZOpcode._
    import ZRegister._
    List(
      ZLine.register(PUSH, IX),
      ZLine.ldImm16(IX, 0x10000 - ctx.function.stackVariablesSize.&(1).+(ctx.function.stackVariablesSize)),
      ZLine.registers(ADD_16, IX, SP),
      ZLine.ld16(SP, IX))
  }
}
