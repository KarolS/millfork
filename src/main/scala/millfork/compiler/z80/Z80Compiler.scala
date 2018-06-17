package millfork.compiler.z80

import java.util.concurrent.atomic.AtomicLong

import millfork.assembly.z80.ZLine
import millfork.compiler.{AbstractCompiler, CompilationContext}
import millfork.env.Label

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
    val prefix = Nil // TODO
    label :: (prefix ++ chunk)
  }
}
