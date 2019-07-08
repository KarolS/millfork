package millfork.compiler.m6809

import millfork.assembly.m6809.MLine
import millfork.compiler.{AbstractStatementPreprocessor, CompilationContext}
import millfork.node.{ExecutableStatement, ForStatement}

/**
  * @author Karol Stasiak
  */
class M6809StatementPreprocessor(ctx: CompilationContext, statements: List[ExecutableStatement])
  extends AbstractStatementPreprocessor(ctx, statements) {
  override def maybeOptimizeForStatement(f: ForStatement): Option[(ExecutableStatement, VV)] = None
}
