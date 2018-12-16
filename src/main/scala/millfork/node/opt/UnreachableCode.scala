package millfork.node.opt

import millfork.CompilationOptions
import millfork.node._

/**
  * @author Karol Stasiak
  */
object UnreachableCode extends NodeOptimization {

  override def optimize(nodes: List[Node], options: CompilationOptions): List[Node] = nodes match {
    case (x:FunctionDeclarationStatement)::xs =>
      x.copy(statements = x.statements.map(optimizeStatements(_, options))).pos(x.position) :: optimize(xs, options)
    case (x:IfStatement)::xs =>
      x.copy(
        thenBranch = optimizeExecutableStatements(x.thenBranch, options),
        elseBranch = optimizeExecutableStatements(x.elseBranch, options)).pos(x.position) :: optimize(xs, options)
    case (x:WhileStatement)::xs =>
      x.copy(body = optimizeExecutableStatements(x.body, options)).pos(x.position) :: optimize(xs, options)
    case (x:DoWhileStatement)::xs =>
      x.copy(body = optimizeExecutableStatements(x.body, options)).pos(x.position) :: optimize(xs, options)
    case (x:ReturnStatement) :: xs =>
      x :: Nil
    case x :: xs =>
      x :: optimize(xs, options)
    case Nil =>
      Nil
  }

}
