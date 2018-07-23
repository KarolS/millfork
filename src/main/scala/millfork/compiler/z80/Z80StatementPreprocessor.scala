package millfork.compiler.z80

import millfork.compiler.{AbstractStatementPreprocessor, CompilationContext}
import millfork.node.{ExecutableStatement, ForStatement}

/**
  * @author Karol Stasiak
  */
class Z80StatementPreprocessor(ctx: CompilationContext, statements: List[ExecutableStatement]) extends AbstractStatementPreprocessor(ctx, statements) {

  def maybeOptimizeForStatement(f: ForStatement): Option[(ExecutableStatement, VV)] = None
}
