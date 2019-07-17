package millfork.node

import millfork.CompilationOptions
import millfork.error.Logger
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

  def checkSegments(log: Logger, existingBanks: Set[String]): Unit = {
    declarations.foreach {
      case s: BankedDeclarationStatement =>
        s.bank.foreach{ b =>
          if (!existingBanks(b)) {
            log.error(s"Cannot allocate ${s.name}: Segment $b doesn't exist")
          }
        }
      case _ =>
    }
  }
  def applyNodeOptimization(o: NodeOptimization, options: CompilationOptions) = Program(o.optimize(declarations, options).asInstanceOf[List[DeclarationStatement]])
  def +(p:Program): Program = Program(this.declarations ++ p.declarations)
}
