package millfork.compiler.m6809

import millfork.assembly.BranchingOpcodeMapping
import millfork.assembly.m6809.MLine
import millfork.compiler.{AbstractCompiler, AbstractStatementCompiler, BranchSpec, CompilationContext}
import millfork.node.{ExecutableStatement, Expression, M6809AssemblyStatement, ReturnStatement, VariableExpression, WhileStatement}
import millfork.assembly.m6809.MOpcode._
import millfork.env.{Label, ThingInMemory}
/**
  * @author Karol Stasiak
  */
object M6809StatementCompiler extends AbstractStatementCompiler[MLine] {
  def compile(ctx: CompilationContext, statement: ExecutableStatement): (List[MLine], List[MLine]) = {
    val code: (List[MLine], List[MLine]) = statement match {
      case ReturnStatement(None) =>
        // TODO: clean stack
        // TODO: RTI
        List(MLine.inherent(RTS)) -> Nil
      case M6809AssemblyStatement(opcode, addrMode, expression, elidability) =>
        ctx.env.evalForAsm(expression) match {
          case Some(e) => List(MLine(opcode, addrMode, e, elidability)) -> Nil
          case None =>
            println(statement)
            ???
        }
      case WhileStatement(VariableExpression("true"),List(),List(),_) =>
        Nil -> Nil // TODO
      case _ =>
        println(statement)
        ???
    }
    code._1.map(_.positionIfEmpty(statement.position)) -> code._2.map(_.positionIfEmpty(statement.position))
  }

  override def labelChunk(labelName: String): List[MLine] = ???

  override def jmpChunk(label: Label): List[MLine] = ???

  override def branchChunk(opcode: BranchingOpcodeMapping, labelName: String): List[MLine] = ???

  override def compileExpressionForBranching(ctx: CompilationContext, expr: Expression, branching: BranchSpec): List[MLine] = ???

  override def replaceLabel(ctx: CompilationContext, line: MLine, from: String, to: String): MLine = ???

  override def returnAssemblyStatement: ExecutableStatement = ???

  override def callChunk(label: ThingInMemory): List[MLine] = ???

  override def areBlocksLarge(blocks: List[MLine]*): Boolean = ???
}
