package millfork.compiler.z80

import millfork.assembly.BranchingOpcodeMapping
import millfork.assembly.z80._
import millfork.compiler.{AbstractExpressionCompiler, AbstractStatementCompiler, BranchSpec, CompilationContext}
import millfork.env._
import millfork.node._
import millfork.assembly.z80.ZOpcode._
import millfork.error.ErrorReporting

/**
  * @author Karol Stasiak
  */
object Z80StatementCompiler extends AbstractStatementCompiler[ZLine] {

  def compile(ctx: CompilationContext, statements: List[ExecutableStatement]): List[ZLine] = {
    statements.flatMap(s => compile(ctx, s))
  }

  def compile(ctx: CompilationContext, statement: ExecutableStatement): List[ZLine] = {
    val options = ctx.options
    statement match {
      case ReturnStatement(None) =>
        fixStackOnReturn(ctx) ++ (ctx.function.returnType match {
          case _: BooleanType =>
            List(ZLine.implied(DISCARD_A), ZLine.implied(DISCARD_HL), ZLine.implied(DISCARD_BCDEIX), ZLine.implied(RET))
          case t => t.size match {
            case 0 =>
              List(ZLine.implied(DISCARD_F), ZLine.implied(DISCARD_A), ZLine.implied(DISCARD_HL), ZLine.implied(DISCARD_BCDEIX), ZLine.implied(RET))
            case 1 =>
              ErrorReporting.warn("Returning without a value", options, statement.position)
              List(ZLine.implied(DISCARD_F), ZLine.implied(DISCARD_A), ZLine.implied(DISCARD_HL), ZLine.implied(DISCARD_BCDEIX), ZLine.implied(RET))
            case 2 =>
              ErrorReporting.warn("Returning without a value", options, statement.position)
              List(ZLine.implied(DISCARD_F), ZLine.implied(DISCARD_A), ZLine.implied(DISCARD_HL), ZLine.implied(DISCARD_BCDEIX), ZLine.implied(RET))
          }
        })
      case ReturnStatement(Some(e)) =>
        ctx.function.returnType match {
          case t: BooleanType => t.size match {
            case 0 =>
              ErrorReporting.error("Cannot return anything from a void function", statement.position)
              fixStackOnReturn(ctx) ++
                List(ZLine.implied(DISCARD_A), ZLine.implied(DISCARD_HL), ZLine.implied(DISCARD_BCDEIX), ZLine.implied(RET))
            case 1 =>
              Z80ExpressionCompiler.compileToA(ctx, e) ++ fixStackOnReturn(ctx) ++
                List(ZLine.implied(DISCARD_HL), ZLine.implied(DISCARD_BCDEIX), ZLine.implied(RET))
            case 2 =>
              Z80ExpressionCompiler.compileToHL(ctx, e) ++ fixStackOnReturn(ctx) ++
                List(ZLine.implied(DISCARD_A), ZLine.implied(DISCARD_BCDEIX), ZLine.implied(RET))
          }
          case t => t.size match {
            case 0 =>
              ErrorReporting.error("Cannot return anything from a void function", statement.position)
              fixStackOnReturn(ctx) ++
                List(ZLine.implied(DISCARD_F), ZLine.implied(DISCARD_A), ZLine.implied(DISCARD_HL), ZLine.implied(DISCARD_BCDEIX), ZLine.implied(RET))
            case 1 =>
              Z80ExpressionCompiler.compileToA(ctx, e) ++ fixStackOnReturn(ctx) ++
                List(ZLine.implied(DISCARD_F), ZLine.implied(DISCARD_HL), ZLine.implied(DISCARD_BCDEIX), ZLine.implied(RET))
            case 2 =>
              Z80ExpressionCompiler.compileToHL(ctx, e) ++ fixStackOnReturn(ctx) ++
                List(ZLine.implied(DISCARD_F), ZLine.implied(DISCARD_A), ZLine.implied(DISCARD_BCDEIX), ZLine.implied(RET))
          }
        }
      case Assignment(destination, source) =>
        val sourceType = AbstractExpressionCompiler.getExpressionType(ctx, source)
        sourceType.size match {
          case 0 => ???
          case 1 => Z80ExpressionCompiler.compileToA(ctx, source) ++ Z80ExpressionCompiler.storeA(ctx, destination, sourceType.isSigned)
          case 2 => Z80ExpressionCompiler.compileToHL(ctx, source) ++ Z80ExpressionCompiler.storeHL(ctx, destination, sourceType.isSigned)
          case _ => ??? // large object copy
        }
      case s: IfStatement =>
        compileIfStatement(ctx, s)
      case s: WhileStatement =>
        compileWhileStatement(ctx, s)
      case s: DoWhileStatement =>
        compileDoWhileStatement(ctx, s)

      case f@ForStatement(_, _, _, _, List(Assignment(target: IndexedExpression, source: IndexedExpression))) =>
        Z80BulkMemoryOperations.compileMemcpy(ctx, target, source, f)

      case f@ForStatement(variable, _, _, _, List(Assignment(target: IndexedExpression, source: Expression))) if !source.containsVariable(variable) =>
        Z80BulkMemoryOperations.compileMemset(ctx, target, source, f)

      case f@ForStatement(variable, _, _, _, List(ExpressionStatement(FunctionCallExpression(
      operator@("+=" | "-=" | "|=" | "&=" | "^=" | "+'=" | "-'=" | "<<=" | ">>="),
      List(target: IndexedExpression, source: Expression)
      )))) =>
        Z80BulkMemoryOperations.compileMemtransform(ctx, target, operator, source, f)

      case f@ForStatement(variable, _, _, _, List(
      ExpressionStatement(FunctionCallExpression(
      operator1@("+=" | "-=" | "|=" | "&=" | "^=" | "+'=" | "-'=" | "<<=" | ">>="),
      List(target1: IndexedExpression, source1: Expression)
      )),
      ExpressionStatement(FunctionCallExpression(
      operator2@("+=" | "-=" | "|=" | "&=" | "^=" | "+'=" | "-'=" | "<<=" | ">>="),
      List(target2: IndexedExpression, source2: Expression)
      ))
      )) =>
        Z80BulkMemoryOperations.compileMemtransform2(ctx, target1, operator1, source1, target2, operator2, source2, f)

      case f@ForStatement(variable, _, _, _, List(
      Assignment(target1: IndexedExpression, source1: Expression),
      ExpressionStatement(FunctionCallExpression(
      operator2@("+=" | "-=" | "|=" | "&=" | "^=" | "+'=" | "-'=" | "<<=" | ">>="),
      List(target2: IndexedExpression, source2: Expression)
      ))
      )) =>
        Z80BulkMemoryOperations.compileMemtransform2(ctx, target1, "=", source1, target2, operator2, source2, f)

      case f@ForStatement(variable, _, _, _, List(
      ExpressionStatement(FunctionCallExpression(
      operator1@("+=" | "-=" | "|=" | "&=" | "^=" | "+'=" | "-'=" | "<<=" | ">>="),
      List(target1: IndexedExpression, source1: Expression)
      )),
      Assignment(target2: IndexedExpression, source2: Expression)
      )) =>
        Z80BulkMemoryOperations.compileMemtransform2(ctx, target1, operator1, source1, target2, "=", source2, f)

      case f@ForStatement(variable, _, _, _, List(
      Assignment(target1: IndexedExpression, source1: Expression),
      Assignment(target2: IndexedExpression, source2: Expression)
      )) =>
        Z80BulkMemoryOperations.compileMemtransform2(ctx, target1, "=", source1, target2, "=", source2, f)

      case f: ForStatement =>
        compileForStatement(ctx, f)
      case s: BreakStatement =>
        compileBreakStatement(ctx, s)
      case s: ContinueStatement =>
        compileContinueStatement(ctx, s)
      case ExpressionStatement(e@FunctionCallExpression(name, params)) =>
        ctx.env.lookupFunction(name, params.map(p => Z80ExpressionCompiler.getExpressionType(ctx, p) -> p)) match {
          case Some(i: MacroFunction) =>
            val (paramPreparation, inlinedStatements) = Z80MacroExpander.inlineFunction(ctx, i, params, e.position)
            paramPreparation ++ compile(ctx.withInlinedEnv(i.environment, Z80Compiler.nextLabel("en")), inlinedStatements)
          case _ =>
            Z80ExpressionCompiler.compile(ctx, e, ZExpressionTarget.NOTHING)
        }
      case ExpressionStatement(e) =>
        Z80ExpressionCompiler.compile(ctx, e, ZExpressionTarget.NOTHING)
      case Z80AssemblyStatement(op, reg, expression, elidable) =>
        val param = ctx.env.evalForAsm(expression) match {
          case Some(v) => v
          case None =>
            ErrorReporting.error("Inlining failed due to non-constant things", expression.position)
            Constant.Zero
        }
        List(ZLine(op, reg, param, elidable))
    }
  }

  private def fixStackOnReturn(ctx: CompilationContext): List[ZLine] = {
    if (ctx.function.stackVariablesSize > 0) {
      import ZRegister._
      val localVariableArea = ctx.function.stackVariablesSize.&(1).+(ctx.function.stackVariablesSize)
      if (ctx.function.returnType.size == 2) {
        List(
          ZLine.ldImm16(IX, localVariableArea), 
          ZLine.registers(ADD_16, IX, SP),
          ZLine.ld16(SP, IX),
          ZLine.register(POP, IX))
      } else {
        List(
          ZLine.ldImm16(HL, localVariableArea),
          ZLine.registers(ADD_16, HL, SP),
          ZLine.ld16(SP, HL),
          ZLine.register(POP, IX))
      }
    } else Nil
  }

  def labelChunk(labelName: String) = List(ZLine.label(Label(labelName)))

  def jmpChunk(label: Label) = List(ZLine.jump(label))

  def branchChunk(opcode: BranchingOpcodeMapping, labelName: String) = List(ZLine.jump(Label(labelName), opcode.z80Flags))

  def areBlocksLarge(blocks: List[ZLine]*): Boolean = false

  override def nextLabel(prefix: String): String = Z80Compiler.nextLabel(prefix)

  override def compileExpressionForBranching(ctx: CompilationContext, expr: Expression, branching: BranchSpec): List[ZLine] =
    Z80ExpressionCompiler.compile(ctx, expr, ZExpressionTarget.NOTHING, branching)
}
