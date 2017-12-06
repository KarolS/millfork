package millfork.node

import millfork.node.opt.NodeOptimization

/**
  * @author Karol Stasiak
  */
case class Program(declarations: List[DeclarationStatement]) {
  def applyNodeOptimization(o: NodeOptimization) = Program(o.optimize(declarations).asInstanceOf[List[DeclarationStatement]])
  def +(p:Program): Program = Program(this.declarations ++ p.declarations)
}
