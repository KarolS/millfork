package millfork.compiler.m6809

import millfork.assembly.Elidability
import millfork.assembly.m6809.{Inherent, MLine, MOpcode, NonExistent}
import millfork.compiler.{AbstractCompiler, CompilationContext}
import millfork.env.{Constant, Label, MemoryAddressConstant}

/**
  * @author Karol Stasiak
  */
object M6809Compiler extends AbstractCompiler[MLine] {
  override def compile(ctx: CompilationContext): List[MLine] = {
    ctx.env.nameCheck(ctx.function.code)
    val label = MLine.label(Label(ctx.function.name)).copy(elidability = Elidability.Fixed)
    val chunk = packHalves(M6809StatementCompiler.compile(ctx, new M6809StatementPreprocessor(ctx, ctx.function.code)()))
    // TODO
    label :: chunk
  }
}
