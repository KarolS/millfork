package millfork.compiler.z80

import millfork.assembly.z80.ZLine
import millfork.compiler.{CompilationContext, MacroExpander}
import millfork.env.AssemblyParam
import millfork.node.{ExecutableStatement, Expression}

/**
  * @author Karol Stasiak
  */
object Z80MacroExpander extends MacroExpander[ZLine] {
  override def nextLabel(prefix: String): String = Z80Compiler.nextLabel(prefix)

  override def prepareAssemblyParams(ctx: CompilationContext, assParams: List[AssemblyParam], params: List[Expression], code: List[ExecutableStatement]): (List[ZLine], List[ExecutableStatement]) = ???
}
