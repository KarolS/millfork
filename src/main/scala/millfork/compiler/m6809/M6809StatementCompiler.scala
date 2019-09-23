package millfork.compiler.m6809

import millfork.assembly.BranchingOpcodeMapping
import millfork.assembly.m6809.{MLine, NonExistent}
import millfork.compiler.{AbstractCompiler, AbstractExpressionCompiler, AbstractStatementCompiler, BranchSpec, CompilationContext}
import millfork.node.{Assignment, DoWhileStatement, ExecutableStatement, Expression, ExpressionStatement, ForEachStatement, ForStatement, IfStatement, M6809AssemblyStatement, ReturnDispatchStatement, ReturnStatement, VariableExpression, WhileStatement}
import millfork.assembly.m6809.MOpcode._
import millfork.env.{FatBooleanType, Label, ThingInMemory}

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
      case ReturnStatement(Some(e)) =>
        // TODO: clean stack
        // TODO: RTI
        val rts = List(MLine.inherent(RTS))
        val eval = ctx.function.returnType match {
          case FatBooleanType =>
            M6809ExpressionCompiler.compileToFatBooleanInB(ctx, e)
          case _ =>
            ctx.function.returnType.size match {
              case 0 =>
                ctx.log.error("Cannot return anything from a void function", statement.position)
                M6809ExpressionCompiler.compile(ctx, e, MExpressionTarget.NOTHING)
              case 1 => M6809ExpressionCompiler.compileToB(ctx, e)
              case 2 => M6809ExpressionCompiler.compileToD(ctx, e)
            }
        }
        (eval ++ rts) -> Nil
      case M6809AssemblyStatement(opcode, addrMode, expression, elidability) =>
        ctx.env.evalForAsm(expression) match {
          case Some(e) => List(MLine(opcode, addrMode, e, elidability)) -> Nil
          case None =>
            println(statement)
            ???
        }
      case Assignment(destination, source) =>
        AbstractExpressionCompiler.getExpressionType(ctx, destination).size match {
          case 1 => (M6809ExpressionCompiler.compileToB(ctx, source) ++ M6809ExpressionCompiler.storeB(ctx, destination)) -> Nil
          case 2 => (M6809ExpressionCompiler.compileToD(ctx, source) ++ M6809ExpressionCompiler.storeD(ctx, destination)) -> Nil
        }
      case ExpressionStatement(expression) =>
        M6809ExpressionCompiler.compile(ctx, expression, MExpressionTarget.NOTHING) -> Nil
      case s:IfStatement =>
        compileIfStatement(ctx, s)
      case s:WhileStatement =>
        compileWhileStatement(ctx, s)
      case s:DoWhileStatement =>
        compileDoWhileStatement(ctx, s)
      case s:ForStatement =>
        compileForStatement(ctx, s)
      case s:ForEachStatement =>
        compileForEachStatement(ctx, s)
      case M6809AssemblyStatement(opcode, addrMode, expression, elidability) =>
        ctx.env.evalForAsm(expression) match {
          case Some(param) =>
            List(MLine(opcode, addrMode, param, elidability)) -> Nil
          case None =>
            ctx.log.error("Invalid parameter", expression.position)
            Nil -> Nil
        }
      case _ =>
        println(statement)
        ???
    }
    code._1.map(_.positionIfEmpty(statement.position)) -> code._2.map(_.positionIfEmpty(statement.position))
  }

  override def labelChunk(labelName: String): List[MLine] = List(MLine(LABEL, NonExistent, Label(labelName).toAddress))

  override def jmpChunk(label: Label): List[MLine] = List(MLine.absolute(JMP, label.toAddress))

  override def branchChunk(opcode: BranchingOpcodeMapping, labelName: String): List[MLine] = ???

  override def compileExpressionForBranching(ctx: CompilationContext, expr: Expression, branching: BranchSpec): List[MLine] =
    M6809ExpressionCompiler.compile(ctx, expr, MExpressionTarget.NOTHING, branching)

  override def replaceLabel(ctx: CompilationContext, line: MLine, from: String, to: String): MLine = ???

  override def returnAssemblyStatement: ExecutableStatement = ???

  override def callChunk(label: ThingInMemory): List[MLine] = List(MLine.absolute(JSR, label.toAddress))

  override def areBlocksLarge(blocks: List[MLine]*): Boolean = true // TODO
}
