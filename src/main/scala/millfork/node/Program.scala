package millfork.node

import millfork.CompilationOptions
import millfork.node.opt.NodeOptimization

/**
  * @author Karol Stasiak
  */
case class Program(declarations: List[DeclarationStatement]) {
  def applyImportantAliases: Program = {
    val importantAliases = declarations.flatMap{
      case AliasDefinitionStatement(name, _, true) => Some(name)
      case _ => None
    }.toSet
    Program(declarations.filter{
      case AliasDefinitionStatement(_, _, true) => true
      case d => !importantAliases(d.name)
    })
  }

  def applyNodeOptimization(o: NodeOptimization, options: CompilationOptions) = Program(o.optimize(declarations, options).asInstanceOf[List[DeclarationStatement]])
  def +(p:Program): Program = Program(this.declarations ++ p.declarations)
}
