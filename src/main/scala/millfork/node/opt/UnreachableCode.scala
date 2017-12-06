package millfork.node.opt

import millfork.node._

/**
  * @author Karol Stasiak
  */
object UnreachableCode extends NodeOptimization {

  override def optimize(nodes: List[Node]): List[Node] = nodes match {
    case (x:FunctionDeclarationStatement)::xs =>
      x.copy(statements = x.statements.map(optimizeStatements)) :: optimize(xs)
    case (x:IfStatement)::xs =>
      x.copy(
        thenBranch = optimizeExecutableStatements(x.thenBranch),
        elseBranch = optimizeExecutableStatements(x.elseBranch)) :: optimize(xs)
    case (x:WhileStatement)::xs =>
      x.copy(body = optimizeExecutableStatements(x.body)) :: optimize(xs)
    case (x:DoWhileStatement)::xs =>
      x.copy(body = optimizeExecutableStatements(x.body)) :: optimize(xs)
    case (x:ReturnStatement) :: xs =>
      x :: Nil
    case x :: xs =>
      x :: optimize(xs)
    case Nil =>
      Nil
  }

}
