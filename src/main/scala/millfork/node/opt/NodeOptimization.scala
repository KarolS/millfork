package millfork.node.opt

import millfork.CompilationOptions
import millfork.node.{ExecutableStatement, Expression, Node, Statement}

/**
  * @author Karol Stasiak
  */
trait NodeOptimization {
  def optimize(nodes: List[Node], options: CompilationOptions): List[Node]

  def optimizeExecutableStatements(nodes: List[ExecutableStatement], options: CompilationOptions): List[ExecutableStatement] =
    optimize(nodes, options).asInstanceOf[List[ExecutableStatement]]

  def optimizeStatements(nodes: List[Statement], options: CompilationOptions): List[Statement] =
    optimize(nodes, options).asInstanceOf[List[Statement]]

  def resolveAliases(aliases: Map[String, String], set: Set[String]):Set[String] = {
    var result = set
    var lastSize = set.size
    do {
      lastSize = set.size
      result = result.flatMap(name => aliases.get(name).fold(Set(name))(Set(_, name)))
    } while(lastSize != set.size)
    result
  }
}
