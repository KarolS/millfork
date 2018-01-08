package millfork.node

import millfork.CompilationOptions
import millfork.node.opt.NodeOptimization

/**
  * @author Karol Stasiak
  */
case class Program(declarations: List[DeclarationStatement]) {
  def applyNodeOptimization(o: NodeOptimization, options: CompilationOptions) = Program(o.optimize(declarations, options).asInstanceOf[List[DeclarationStatement]])
  def +(p:Program): Program = Program(this.declarations ++ p.declarations)
}
