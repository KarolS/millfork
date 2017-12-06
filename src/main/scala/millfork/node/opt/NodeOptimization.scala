package millfork.node.opt

import millfork.node.{ExecutableStatement, Expression, Node, Statement}

/**
  * @author Karol Stasiak
  */
trait NodeOptimization {
  def optimize(nodes: List[Node]): List[Node]

  def optimizeExecutableStatements(nodes: List[ExecutableStatement]): List[ExecutableStatement] =
    optimize(nodes).asInstanceOf[List[ExecutableStatement]]

  def optimizeStatements(nodes: List[Statement]): List[Statement] =
    optimize(nodes).asInstanceOf[List[Statement]]
}
