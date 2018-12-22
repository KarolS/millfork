package millfork.compiler

import millfork.CompilationFlag
import millfork.assembly.{AbstractCode, BranchingOpcodeMapping}
import millfork.env._
import millfork.node._

/**
  * @author Karol Stasiak
  */
abstract class AbstractStatementCompiler[T <: AbstractCode] {

  def compile(ctx: CompilationContext, statements: List[ExecutableStatement]): (List[T], List[T]) = {
    val chunks = statements.map(s => compile(ctx, s))
    chunks.flatMap(_._1) -> chunks.flatMap(_._2)
  }

  def compile(ctx: CompilationContext, statement: ExecutableStatement): (List[T], List[T])

  def labelChunk(labelName: String): List[T]

  def jmpChunk(labelName: String): List[T] = jmpChunk(Label(labelName))

  def jmpChunk(label: Label): List[T]

  def branchChunk(opcode: BranchingOpcodeMapping, labelName: String): List[T]

  def compileExpressionForBranching(ctx: CompilationContext, expr: Expression, branching: BranchSpec): List[T]

  def replaceLabel(ctx: CompilationContext, line: T, from: String, to: String): T

  def returnAssemblyStatement: ExecutableStatement

  def callChunk(label: ThingInMemory): List[T]

  def areBlocksLarge(blocks: List[T]*): Boolean

  def compileWhileStatement(ctx: CompilationContext, s: WhileStatement): (List[T], List[T]) = {
    val start = ctx.nextLabel("wh")
    val middle = ctx.nextLabel("he")
    val inc = ctx.nextLabel("fp")
    val end = ctx.nextLabel("ew")
    val condType = AbstractExpressionCompiler.getExpressionType(ctx, s.condition)
    val (bodyBlock, extraBlock) = compile(ctx.addLabels(s.labels, Label(end), Label(inc)), s.body)
    val (incrementBlock, extraBlock2) = compile(ctx.addLabels(s.labels, Label(end), Label(inc)), s.increment)
    val largeBodyBlock = areBlocksLarge(bodyBlock, incrementBlock)
    (condType match {
      case ConstantBooleanType(_, true) =>
        List(labelChunk(start), bodyBlock, labelChunk(inc), incrementBlock, jmpChunk(start), labelChunk(end)).flatten
      case ConstantBooleanType(_, false) => Nil
      case FlagBooleanType(_, jumpIfTrue, jumpIfFalse) =>
        if (largeBodyBlock) {
          val conditionBlock = compileExpressionForBranching(ctx, s.condition, NoBranching)
          List(labelChunk(start), conditionBlock, branchChunk(jumpIfTrue, middle), jmpChunk(end), labelChunk(middle), bodyBlock, labelChunk(inc), incrementBlock, jmpChunk(start), labelChunk(end)).flatten
        } else {
          val conditionBlock = compileExpressionForBranching(ctx, s.condition, NoBranching)
          List(jmpChunk(middle), labelChunk(start), bodyBlock, labelChunk(inc), incrementBlock, labelChunk(middle), conditionBlock, branchChunk(jumpIfTrue, start), labelChunk(end)).flatten
          //              List(labelChunk(start), conditionBlock, branchChunk(jumpIfFalse, end), bodyBlock, labelChunk(inc), incrementBlock, jmpChunk(start), labelChunk(end)).flatten
        }
      case BuiltInBooleanType =>
        if (largeBodyBlock) {
          val conditionBlock = compileExpressionForBranching(ctx, s.condition, BranchIfTrue(middle))
          List(labelChunk(start), conditionBlock, jmpChunk(end), labelChunk(middle), bodyBlock, labelChunk(inc), incrementBlock, jmpChunk(start), labelChunk(end)).flatten
        } else {
          val conditionBlock = compileExpressionForBranching(ctx, s.condition, BranchIfTrue(start))
          List(jmpChunk(middle), labelChunk(start), bodyBlock, labelChunk(inc), incrementBlock, labelChunk(middle), conditionBlock, labelChunk(end)).flatten
          //              List(labelChunk(start), conditionBlock, bodyBlock, labelChunk(inc), incrementBlock, jmpChunk(start), labelChunk(end)).flatten
        }
      case _ =>
        ctx.log.error(s"Illegal type for a condition: `$condType`", s.condition.position)
        Nil
    }) -> (extraBlock ++ extraBlock2)
  }

  def compileDoWhileStatement(ctx: CompilationContext, s: DoWhileStatement): (List[T], List[T]) = {
    val start = ctx.nextLabel("do")
    val inc = ctx.nextLabel("fp")
    val end = ctx.nextLabel("od")
    val condType = AbstractExpressionCompiler.getExpressionType(ctx, s.condition)
    val (bodyBlock, extraBlock) = compile(ctx.addLabels(s.labels, Label(end), Label(inc)), s.body)
    val (incrementBlock, extraBlock2) = compile(ctx.addLabels(s.labels, Label(end), Label(inc)), s.increment)
    val largeBodyBlock = areBlocksLarge(bodyBlock, incrementBlock)
    (condType match {
      case ConstantBooleanType(_, true) =>
        val conditionBlock = compileExpressionForBranching(ctx, s.condition, NoBranching)
        List(labelChunk(start), bodyBlock, labelChunk(inc), incrementBlock, jmpChunk(start), labelChunk(end)).flatten
      case ConstantBooleanType(_, false) =>
        List(bodyBlock, labelChunk(inc), incrementBlock, labelChunk(end)).flatten
      case FlagBooleanType(_, jumpIfTrue, jumpIfFalse) =>
        val conditionBlock = compileExpressionForBranching(ctx, s.condition, NoBranching)
        if (largeBodyBlock) {
          List(labelChunk(start), bodyBlock, labelChunk(inc), incrementBlock, conditionBlock, branchChunk(jumpIfFalse, end), jmpChunk(start), labelChunk(end)).flatten
        } else {
          List(labelChunk(start), bodyBlock, labelChunk(inc), incrementBlock, conditionBlock, branchChunk(jumpIfTrue, start), labelChunk(end)).flatten
        }
      case BuiltInBooleanType =>
        if (largeBodyBlock) {
          val conditionBlock = compileExpressionForBranching(ctx, s.condition, BranchIfFalse(end))
          List(labelChunk(start), bodyBlock, labelChunk(inc), incrementBlock, conditionBlock, jmpChunk(start), labelChunk(end)).flatten
        } else {
          val conditionBlock = compileExpressionForBranching(ctx, s.condition, BranchIfTrue(start))
          List(labelChunk(start), bodyBlock, labelChunk(inc), incrementBlock, conditionBlock, labelChunk(end)).flatten
        }
      case _ =>
        ctx.log.error(s"Illegal type for a condition: `$condType`", s.condition.position)
        Nil
    }) -> (extraBlock ++ extraBlock2)
  }

  def compileForStatement(ctx: CompilationContext, f: ForStatement): (List[T], List[T]) = {
    // TODO: check sizes
    // TODO: special faster cases
    val p = f.position
    val vex = VariableExpression(f.variable)
    val indexType = ctx.env.get[Variable](f.variable).typ
    val arithmetic = indexType.isArithmetic
    if (!arithmetic && f.direction != ForDirection.ParallelUntil) {
      ctx.log.error("Invalid direction for enum iteration", p)
      compile(ctx, f.body)
      return Nil -> Nil
    }
    val one = LiteralExpression(1, 1).pos(p)
    val increment = if (arithmetic) {
      ExpressionStatement(FunctionCallExpression("+=", List(vex, one)).pos(p)).pos(p)
    } else {
      Assignment(vex, FunctionCallExpression(indexType.name, List(
        SumExpression(List(
          false -> FunctionCallExpression("byte", List(vex)).pos(p),
          false -> LiteralExpression(1,1).pos(p),
        ), decimal = false).pos(p)
      )).pos(p)).pos(p)
    }
    val decrement = if (arithmetic) {
          ExpressionStatement(FunctionCallExpression("-=", List(vex, one)).pos(p)).pos(p)
        } else {
          Assignment(vex, FunctionCallExpression(indexType.name, List(
            SumExpression(List(
              false -> FunctionCallExpression("byte", List(vex)).pos(p),
              true -> LiteralExpression(1,1).pos(p),
            ), decimal = false).pos(p)
          )).pos(p)).pos(p)
        }
    val names = Set("", "for", f.variable)

    val startEvaluated = ctx.env.eval(f.start)
    val endEvaluated = ctx.env.eval(f.end)
    val variable = ctx.env.maybeGet[Variable](f.variable)
    variable.foreach{ v=>
      startEvaluated.foreach(value => if (!value.quickSimplify.fitsInto(v.typ)) {
        ctx.log.error(s"Variable `${f.variable}` is too small to hold the initial value in the for loop", f.position)
      })
      endEvaluated.foreach { value =>
        val max = f.direction match {
          case ForDirection.To | ForDirection.ParallelTo | ForDirection.DownTo => value
          case ForDirection.Until | ForDirection.ParallelUntil => value - 1
          case _ => Constant.Zero
        }
        if (!max.quickSimplify.fitsInto(v.typ)) {
          ctx.log.error(s"Variable `${f.variable}` is too small to hold the final value in the for loop", f.position)
        }
      }
    }
    (f.direction, startEvaluated, endEvaluated) match {

      case (ForDirection.Until | ForDirection.ParallelUntil, Some(NumericConstant(s, ssize)), Some(NumericConstant(e, _))) if s == e - 1 =>
        val end = ctx.nextLabel("of")
        val (main, extra) = compile(ctx.addLabels(names, Label(end), Label(end)), Assignment(vex, f.start).pos(p) :: f.body)
        main ++ labelChunk(end) -> extra
      case (ForDirection.Until | ForDirection.ParallelUntil, Some(NumericConstant(s, ssize)), Some(NumericConstant(e, _))) if s >= e =>
        Nil -> Nil

      case (ForDirection.To | ForDirection.ParallelTo, Some(NumericConstant(s, ssize)), Some(NumericConstant(e, _))) if s == e =>
        val end = ctx.nextLabel("of")
        val (main, extra) = compile(ctx.addLabels(names, Label(end), Label(end)), Assignment(vex, f.start).pos(p) :: f.body)
        main ++ labelChunk(end) -> extra
      case (ForDirection.To | ForDirection.ParallelTo, Some(NumericConstant(s, ssize)), Some(NumericConstant(e, _))) if s > e =>
        Nil -> Nil

      case (ForDirection.Until | ForDirection.ParallelUntil, Some(c), Some(NumericConstant(256, _)))
        if variable.map(_.typ.size).contains(1) && c.requiredSize == 1 && c.isProvablyNonnegative =>
        // LDX #s
        // loop:
        // stuff
        // INX
        // BNE loop
        compile(ctx, List(
          Assignment(vex, f.start).pos(p),
          DoWhileStatement(f.body, List(increment), FunctionCallExpression("!=", List(vex, LiteralExpression(0, 1).pos(p))), names).pos(p)
        ))

      case (ForDirection.ParallelUntil, Some(NumericConstant(0, _)), Some(NumericConstant(e, _))) if e > 0 =>
        compile(ctx, List(
          Assignment(vex, f.end),
          DoWhileStatement(Nil, decrement :: f.body, FunctionCallExpression("!=", List(vex, f.start)), names).pos(p)
        ))

      case (ForDirection.DownTo, Some(NumericConstant(s, ssize)), Some(NumericConstant(e, esize))) if s == e =>
        val end = ctx.nextLabel("of")
        val (main, extra) = compile(ctx.addLabels(names, Label(end), Label(end)), Assignment(vex, LiteralExpression(s, ssize)).pos(p) :: f.body)
        main ++ labelChunk(end) -> extra
      case (ForDirection.DownTo, Some(NumericConstant(s, ssize)), Some(NumericConstant(e, esize))) if s < e =>
        Nil -> Nil
      case (ForDirection.DownTo, Some(NumericConstant(s, 1)), Some(NumericConstant(0, _))) if s > 0 =>
        compile(ctx, List(
          Assignment(
            vex,
            FunctionCallExpression("lo", List(
              SumExpression(List(
                false -> f.start,
                false -> LiteralExpression(1, 2).pos(p)),
                decimal = false
              ).pos(p)
            )).pos(p)
          ).pos(p),
          DoWhileStatement(
            decrement :: f.body,
            Nil,
            FunctionCallExpression("!=", List(vex, f.end)).pos(p), names).pos(p)
        ))
      case (ForDirection.DownTo, Some(NumericConstant(s, ssize)), Some(NumericConstant(0, _))) if s > 0 =>
        compile(ctx, List(
          Assignment(
            vex,
            SumExpression(
              List(false -> f.start, false -> LiteralExpression(1, 1).pos(p)),
              decimal = false
            ).pos(p)
          ).pos(p),
          DoWhileStatement(
            decrement :: f.body,
            Nil,
            FunctionCallExpression("!=", List(vex, f.end)).pos(p),
            names
          ).pos(p)
        ))


      case (ForDirection.Until | ForDirection.ParallelUntil, _, _) =>
        compile(ctx, List(
          Assignment(vex, f.start).pos(p),
          WhileStatement(
            FunctionCallExpression("<", List(vex, f.end)).pos(p),
            f.body, List(increment), names).pos(p)
        ))
//          case (ForDirection.To | ForDirection.ParallelTo, _, Some(NumericConstant(n, _))) if n > 0 && n < 255 =>
//            compile(ctx, List(
//              Assignment(vex, f.start),
//              WhileStatement(
//                FunctionCallExpression("<=", List(vex, f.end)),
//                f.body :+ increment),
//            ))
      case (ForDirection.To | ForDirection.ParallelTo, _, _) =>
        compile(ctx, List(
          Assignment(vex, f.start).pos(p),
          WhileStatement(
            VariableExpression("true").pos(p),
            f.body,
            List(IfStatement(
              FunctionCallExpression("==", List(vex, f.end)).pos(p),
              List(BreakStatement(f.variable).pos(p)),
              List(increment)
            )),
            names)
        ))
      case (ForDirection.DownTo, _, _) =>
        // TODO: smarter countdown if end is not a constant
        val endMinusOne = SumExpression(List(true -> f.end, false -> LiteralExpression(1, 1)), decimal = false).pos(p)
        compile(ctx, List(
          Assignment(vex, f.start).pos(p),
          IfStatement(
            FunctionCallExpression(">=", List(vex, f.end)).pos(p),
            List(DoWhileStatement(
              f.body,
              List(decrement),
              FunctionCallExpression("!=", List(vex, endMinusOne)).pos(p),
              names
            ).pos(p)),
            Nil)
        ))
    }
  }

  private def tryExtractForEachBodyToNewFunction(variable: String, stmts: List[ExecutableStatement]): (Boolean, List[ExecutableStatement]) = {
    def inner2(stmt: ExecutableStatement): Option[ExecutableStatement] = stmt match {
      case s: CompoundStatement => s.flatMap(inner2)
      case _: BreakStatement => None
      case _: ReturnStatement => None
      case _: ContinueStatement => None
      case s => Some(s)
    }
    def inner(stmt: ExecutableStatement): Option[ExecutableStatement] = stmt match {
      case s: CompoundStatement => if (s.loopVariable == variable) s.flatMap(inner2) else s.flatMap(inner)
      case _: BreakStatement => None
      case _: ReturnStatement => None
      case s@ContinueStatement(l) if l == variable => Some(returnAssemblyStatement.pos(s.position))
      case _: ContinueStatement => None
      case s => Some(s)
    }
    def toplevel(stmt: ExecutableStatement): Option[ExecutableStatement] = stmt match {
      case s: IfStatement => s.flatMap(toplevel)
      case s: CompoundStatement => s.flatMap(inner)
      case _: BreakStatement => None
      case _: ReturnStatement => None
      case s@ContinueStatement(l) if l == variable => Some(returnAssemblyStatement.pos(s.position))
      case s@ContinueStatement("") => Some(returnAssemblyStatement.pos(s.position))
      case s => Some(s)
    }
    val list = stmts.map(toplevel)
    if (list.forall(_.isDefined)) true -> list.map(_.get)
    else false -> stmts
  }

  def compileForEachStatement(ctx: CompilationContext, f: ForEachStatement): (List[T], List[T]) = {
    val values = f.values match {
      case Left(expr) =>
        expr match {
          case VariableExpression(id) =>
            ctx.env.get[Thing](id) match {
              case EnumType(_, Some(count)) =>
                return compile(ctx, ForStatement(
                  f.variable,
                  FunctionCallExpression(id, List(LiteralExpression(0, 1))),
                  FunctionCallExpression(id, List(LiteralExpression(count, 1))),
                  ForDirection.ParallelUntil,
                  f.body
                ))
              case _ =>
            }
          case _ =>
        }
        ctx.log.error("Not a valid enum type or an inline array", expr.position)
        compile(ctx, f.body)
        return Nil -> Nil
      case Right(vs) => vs
    }
    val endLabel = ctx.nextLabel("fe")
    val continueLabelPlaceholder = ctx.nextLabel("fe")
    val (inlinedBody, extra) = compile(ctx.addLabels(Set("", f.variable), Label(endLabel), Label(continueLabelPlaceholder)), f.body)
    values.size match {
      case 0 => Nil -> Nil
      case 1 =>
        val tuple = compile(ctx,
          Assignment(
            VariableExpression(f.variable).pos(f.position),
            values.head
          ).pos(f.position)
        )
        tuple._1 ++ inlinedBody -> tuple._2
      case valueCount =>
        val (extractable, extracted) = tryExtractForEachBodyToNewFunction(f.variable, f.body)
        val (extractedBody, extra2) = compile(ctx.addStack(2), extracted :+ returnAssemblyStatement)
        val inlinedBodySize = inlinedBody.map(_.sizeInBytes).sum
        val extractedBodySize = extractedBody.map(_.sizeInBytes).sum
        val sizeIfInlined = inlinedBodySize * valueCount
        val sizeIfExtracted = extractedBodySize + 3 * valueCount
        val expectedOptimizationPotentialFromInlining = valueCount * 2
        val shouldExtract = true
          if (ctx.options.flag(CompilationFlag.OptimizeForSonicSpeed)) false
          else sizeIfInlined - expectedOptimizationPotentialFromInlining > sizeIfExtracted
        if (shouldExtract) {
          if (extractable) {
            val callLabel = ctx.nextLabel("fe")
            val calls = values.flatMap(expr => compile(ctx,
              Assignment(
                VariableExpression(f.variable).pos(f.position),
                expr
              )
            )._1 ++ callChunk(Label(callLabel)))
            return calls -> (labelChunk(callLabel) ++ extractedBody ++ extra ++ extra2)
          } else {
            ctx.log.warn("For loop too complex to extract, inlining", f.position)
          }
        }

        val inlinedEverything = values.flatMap { expr =>
          val tuple = compile(ctx,
            Assignment(
              VariableExpression(f.variable).pos(f.position),
              expr
            )
          )
          if (tuple._2.nonEmpty) ???
          val compiled = tuple._1 ++ inlinedBody
          val continueLabel = ctx.nextLabel("fe")
          compiled.map(replaceLabel(ctx, _, continueLabelPlaceholder, continueLabel)) ++ labelChunk(continueLabel)
        } ++ labelChunk(endLabel)

        inlinedEverything -> extra
    }
  }

  def compileBreakStatement(ctx: CompilationContext, s: BreakStatement) :List[T] = {
    ctx.breakLabels.get(s.label) match {
      case None =>
        if (s.label == "") ctx.log.error("`break` outside a loop", s.position)
        else ctx.log.error("Invalid label: " + s.label, s.position)
        Nil
      case Some(label) =>
        jmpChunk(label)
    }
  }

  def compileContinueStatement(ctx: CompilationContext, s: ContinueStatement) :List[T] = {
    ctx.continueLabels.get(s.label) match {
      case None =>
        if (s.label == "") ctx.log.error("`continue` outside a loop", s.position)
        else ctx.log.error("Invalid label: " + s.label, s.position)
        Nil
      case Some(label) =>
        jmpChunk(label)
    }
  }

  def compileIfStatement(ctx: CompilationContext, s: IfStatement): (List[T], List[T]) = {
    val condType = AbstractExpressionCompiler.getExpressionType(ctx, s.condition)
    val (thenBlock, extra1) = compile(ctx, s.thenBranch)
    val (elseBlock, extra2) = compile(ctx, s.elseBranch)
    val largeThenBlock = areBlocksLarge(thenBlock)
    val largeElseBlock = areBlocksLarge(elseBlock)
    val mainCode: List[T] = condType match {
      case ConstantBooleanType(_, true) =>
        compileExpressionForBranching(ctx, s.condition, NoBranching) ++ thenBlock
      case ConstantBooleanType(_, false) =>
        compileExpressionForBranching(ctx, s.condition, NoBranching) ++ elseBlock
      case FlagBooleanType(_, jumpIfTrue, jumpIfFalse) =>
        (s.thenBranch, s.elseBranch) match {
          case (Nil, Nil) =>
            compileExpressionForBranching(ctx, s.condition, NoBranching)
          case (Nil, _) =>
            val conditionBlock = compileExpressionForBranching(ctx, s.condition, NoBranching)
            if (largeElseBlock) {
              val middle = ctx.nextLabel("el")
              val end = ctx.nextLabel("fi")
              List(conditionBlock, branchChunk(jumpIfFalse, middle), jmpChunk(end), labelChunk(middle), elseBlock, labelChunk(end)).flatten
            } else {
              val end = ctx.nextLabel("fi")
              List(conditionBlock, branchChunk(jumpIfTrue, end), elseBlock, labelChunk(end)).flatten
            }
          case (_, Nil) =>
            val conditionBlock = compileExpressionForBranching(ctx, s.condition, NoBranching)
            if (largeThenBlock) {
              val middle = ctx.nextLabel("th")
              val end = ctx.nextLabel("fi")
              List(conditionBlock, branchChunk(jumpIfTrue, middle), jmpChunk(end), labelChunk(middle), thenBlock, labelChunk(end)).flatten
            } else {
              val end = ctx.nextLabel("fi")
              List(conditionBlock, branchChunk(jumpIfFalse, end), thenBlock, labelChunk(end)).flatten
            }
          case _ =>
            val conditionBlock = compileExpressionForBranching(ctx, s.condition, NoBranching)
            if (largeThenBlock) {
              if (largeElseBlock) {
                val middleT = ctx.nextLabel("th")
                val middleE = ctx.nextLabel("el")
                val end = ctx.nextLabel("fi")
                List(conditionBlock, branchChunk(jumpIfTrue, middleT), jmpChunk(middleE), labelChunk(middleT), thenBlock, jmpChunk(end), labelChunk(middleE), elseBlock, labelChunk(end)).flatten
              } else {
                val middle = ctx.nextLabel("th")
                val end = ctx.nextLabel("fi")
                List(conditionBlock, branchChunk(jumpIfTrue, middle), elseBlock, jmpChunk(end), labelChunk(middle), thenBlock, labelChunk(end)).flatten
              }
            } else {
              val middle = ctx.nextLabel("el")
              val end = ctx.nextLabel("fi")
              List(conditionBlock, branchChunk(jumpIfFalse, middle), thenBlock, jmpChunk(end), labelChunk(middle), elseBlock, labelChunk(end)).flatten
            }
        }
      case BuiltInBooleanType =>
        (s.thenBranch, s.elseBranch) match {
          case (Nil, Nil) =>
            compileExpressionForBranching(ctx, s.condition, NoBranching)
          case (Nil, _) =>
            if (largeElseBlock) {
              val middle = ctx.nextLabel("el")
              val end = ctx.nextLabel("fi")
              val conditionBlock = compileExpressionForBranching(ctx, s.condition, BranchIfFalse(middle))
              List(conditionBlock, jmpChunk(end), labelChunk(middle), elseBlock, labelChunk(end)).flatten
            } else {
              val end = ctx.nextLabel("fi")
              val conditionBlock = compileExpressionForBranching(ctx, s.condition, BranchIfTrue(end))
              List(conditionBlock, elseBlock, labelChunk(end)).flatten
            }
          case (_, Nil) =>
            if (largeThenBlock) {
              val middle = ctx.nextLabel("th")
              val end = ctx.nextLabel("fi")
              val conditionBlock = compileExpressionForBranching(ctx, s.condition, BranchIfTrue(middle))
              List(conditionBlock, jmpChunk(end), labelChunk(middle), thenBlock, labelChunk(end)).flatten
            } else {
              val end = ctx.nextLabel("fi")
              val conditionBlock = compileExpressionForBranching(ctx, s.condition, BranchIfFalse(end))
              List(conditionBlock, thenBlock, labelChunk(end)).flatten
            }
          case _ =>
            if (largeThenBlock) {
              if (largeElseBlock) {
                val middleT = ctx.nextLabel("th")
                val middleE = ctx.nextLabel("el")
                val end = ctx.nextLabel("fi")
                val conditionBlock = compileExpressionForBranching(ctx, s.condition, BranchIfTrue(middleT))
                List(conditionBlock, jmpChunk(middleE), labelChunk(middleT), thenBlock, jmpChunk(end), labelChunk(middleE), elseBlock, labelChunk(end)).flatten
              } else {
                val middle = ctx.nextLabel("th")
                val end = ctx.nextLabel("fi")
                val conditionBlock = compileExpressionForBranching(ctx, s.condition, BranchIfTrue(middle))
                List(conditionBlock, elseBlock, jmpChunk(end), labelChunk(middle), thenBlock, labelChunk(end)).flatten
              }
            } else {
              val middle = ctx.nextLabel("el")
              val end = ctx.nextLabel("fi")
              val conditionBlock = compileExpressionForBranching(ctx, s.condition, BranchIfFalse(middle))
              List(conditionBlock, thenBlock, jmpChunk(end), labelChunk(middle), elseBlock, labelChunk(end)).flatten
            }
        }
      case _ =>
        ctx.log.error(s"Illegal type for a condition: `$condType`", s.condition.position)
        Nil
    }
    mainCode -> (extra1 ++ extra2)
  }
}
