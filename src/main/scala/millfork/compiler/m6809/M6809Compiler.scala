package millfork.compiler.m6809

import millfork.assembly.Elidability
import millfork.assembly.m6809.{Inherent, MLine, MOpcode, NonExistent}
import millfork.compiler.{AbstractCompiler, CompilationContext}
import millfork.env.{Constant, Label, MemoryAddressConstant, NormalParamSignature}

/**
  * @author Karol Stasiak
  */
object M6809Compiler extends AbstractCompiler[MLine] {
  override def compile(ctx: CompilationContext): List[MLine] = {
    ctx.env.nameCheck(ctx.function.code)
    import MOpcode._
    val storeParamsFromRegisters = ctx.function.params match {
      case NormalParamSignature(List(param)) if param.typ.size == 1 =>
        List(MLine.absolute(STB, param.toAddress))
      case NormalParamSignature(List(param)) if param.typ.size == 2 =>
        List(MLine.absolute(STD, param.toAddress))
      case _ => Nil
    }
    val label = MLine.label(Label(ctx.function.name)).copy(elidability = Elidability.Fixed)
    val chunk = packHalves(M6809StatementCompiler.compile(ctx, new M6809StatementPreprocessor(ctx, ctx.function.code)()))
    // TODO: stackframe etc.
    label :: (storeParamsFromRegisters ++ chunk)
  }
}
