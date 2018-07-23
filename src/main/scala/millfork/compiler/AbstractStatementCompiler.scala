package millfork.compiler

import millfork.CpuFamily
import millfork.assembly.{AbstractCode, BranchingOpcodeMapping}
import millfork.env._
import millfork.error.ErrorReporting
import millfork.node._

/**
  * @author Karol Stasiak
  */
abstract class AbstractStatementCompiler[T <: AbstractCode] {

  def compile(ctx: CompilationContext, statements: List[ExecutableStatement]): List[T] = {
    getStatementPreprocessor(ctx, statements)().flatMap(s => compile(ctx, s))
  }

  def getStatementPreprocessor(ctx: CompilationContext, statements: List[ExecutableStatement]): AbstractStatementPreprocessor

  def compile(ctx: CompilationContext, statement: ExecutableStatement): List[T]

  def nextLabel(prefix: String): String

  def labelChunk(labelName: String): List[T]

  def jmpChunk(labelName: String): List[T] = jmpChunk(Label(labelName))

  def jmpChunk(label: Label): List[T]

  def branchChunk(opcode: BranchingOpcodeMapping, labelName: String): List[T]

  def compileExpressionForBranching(ctx: CompilationContext, expr: Expression, branching: BranchSpec): List[T]

  def areBlocksLarge(blocks: List[T]*): Boolean

  def compileWhileStatement(ctx: CompilationContext, s: WhileStatement): List[T] = {
    val start = nextLabel("wh")
    val middle = nextLabel("he")
    val inc = nextLabel("fp")
    val end = nextLabel("ew")
    val condType = AbstractExpressionCompiler.getExpressionType(ctx, s.condition)
    val bodyBlock = compile(ctx.addLabels(s.labels, Label(end), Label(inc)), s.body)
    val incrementBlock = compile(ctx.addLabels(s.labels, Label(end), Label(inc)), s.increment)
    val largeBodyBlock = areBlocksLarge(bodyBlock, incrementBlock)
    condType match {
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
        ErrorReporting.error(s"Illegal type for a condition: `$condType`", s.condition.position)
        Nil
    }
  }

  def compileDoWhileStatement(ctx: CompilationContext, s: DoWhileStatement): List[T] = {
    val start = nextLabel("do")
    val inc = nextLabel("fp")
    val end = nextLabel("od")
    val condType = AbstractExpressionCompiler.getExpressionType(ctx, s.condition)
    val bodyBlock = compile(ctx.addLabels(s.labels, Label(end), Label(inc)), s.body)
    val incrementBlock = compile(ctx.addLabels(s.labels, Label(end), Label(inc)), s.increment)
    val largeBodyBlock = areBlocksLarge(bodyBlock, incrementBlock)
    condType match {
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
        ErrorReporting.error(s"Illegal type for a condition: `$condType`", s.condition.position)
        Nil
    }
  }

  def compileForStatement(ctx: CompilationContext, f: ForStatement): List[T] = {
    // TODO: check sizes
    // TODO: special faster cases
    val p = f.position
    val vex = VariableExpression(f.variable)
    val one = LiteralExpression(1, 1).pos(p)
    val increment = ExpressionStatement(FunctionCallExpression("+=", List(vex, one)).pos(p)).pos(p)
    val decrement = ExpressionStatement(FunctionCallExpression("-=", List(vex, one)).pos(p)).pos(p)
    val names = Set("", "for", f.variable)

    val startEvaluated = ctx.env.eval(f.start)
    val endEvaluated = ctx.env.eval(f.end)
    val variable = ctx.env.maybeGet[Variable](f.variable)
    variable.foreach{ v=>
      startEvaluated.foreach(value => if (!value.quickSimplify.fitsInto(v.typ)) {
        ErrorReporting.error(s"Variable `${f.variable}` is too small to hold the initial value in the for loop", f.position)
      })
      endEvaluated.foreach { value =>
        val max = f.direction match {
          case ForDirection.To | ForDirection.ParallelTo | ForDirection.DownTo => value
          case ForDirection.Until | ForDirection.ParallelUntil => value - 1
          case _ => Constant.Zero
        }
        if (!max.quickSimplify.fitsInto(v.typ)) {
          ErrorReporting.error(s"Variable `${f.variable}` is too small to hold the final value in the for loop", f.position)
        }
      }
    }
    (f.direction, startEvaluated, endEvaluated) match {

      case (ForDirection.Until | ForDirection.ParallelUntil, Some(NumericConstant(s, ssize)), Some(NumericConstant(e, _))) if s == e - 1 =>
        val end = nextLabel("of")
        compile(ctx.addLabels(names, Label(end), Label(end)), Assignment(vex, f.start).pos(p) :: f.body) ++ labelChunk(end)
      case (ForDirection.Until | ForDirection.ParallelUntil, Some(NumericConstant(s, ssize)), Some(NumericConstant(e, _))) if s >= e =>
        Nil

      case (ForDirection.To | ForDirection.ParallelTo, Some(NumericConstant(s, ssize)), Some(NumericConstant(e, _))) if s == e =>
        val end = nextLabel("of")
        compile(ctx.addLabels(names, Label(end), Label(end)), Assignment(vex, f.start).pos(p) :: f.body) ++ labelChunk(end)
      case (ForDirection.To | ForDirection.ParallelTo, Some(NumericConstant(s, ssize)), Some(NumericConstant(e, _))) if s > e =>
        Nil

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
        val end = nextLabel("of")
        compile(ctx.addLabels(names, Label(end), Label(end)), Assignment(vex, LiteralExpression(s, ssize)).pos(p) :: f.body) ++ labelChunk(end)
      case (ForDirection.DownTo, Some(NumericConstant(s, ssize)), Some(NumericConstant(e, esize))) if s < e =>
        Nil
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
        compile(ctx, List(
          Assignment(vex, f.start).pos(p),
          IfStatement(
            FunctionCallExpression(">=", List(vex, f.end)).pos(p),
            List(DoWhileStatement(
              f.body,
              List(decrement),
              FunctionCallExpression("!=", List(vex, f.end)).pos(p),
              names
            ).pos(p)),
            Nil)
        ))
    }
  }

  def compileBreakStatement(ctx: CompilationContext, s: BreakStatement) :List[T] = {
    ctx.breakLabels.get(s.label) match {
      case None =>
        if (s.label == "") ErrorReporting.error("`break` outside a loop", s.position)
        else ErrorReporting.error("Invalid label: " + s.label, s.position)
        Nil
      case Some(label) =>
        jmpChunk(label)
    }
  }

  def compileContinueStatement(ctx: CompilationContext, s: ContinueStatement) :List[T] = {
    ctx.continueLabels.get(s.label) match {
      case None =>
        if (s.label == "") ErrorReporting.error("`continue` outside a loop", s.position)
        else ErrorReporting.error("Invalid label: " + s.label, s.position)
        Nil
      case Some(label) =>
        jmpChunk(label)
    }
  }

  def compileIfStatement(ctx: CompilationContext, s: IfStatement): List[T] = {
    val condType = AbstractExpressionCompiler.getExpressionType(ctx, s.condition)
    val thenBlock = compile(ctx, s.thenBranch)
    val elseBlock = compile(ctx, s.elseBranch)
    val largeThenBlock = areBlocksLarge(thenBlock)
    val largeElseBlock = areBlocksLarge(elseBlock)
    condType match {
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
              val middle = nextLabel("el")
              val end = nextLabel("fi")
              List(conditionBlock, branchChunk(jumpIfFalse, middle), jmpChunk(end), labelChunk(middle), elseBlock, labelChunk(end)).flatten
            } else {
              val end = nextLabel("fi")
              List(conditionBlock, branchChunk(jumpIfTrue, end), elseBlock, labelChunk(end)).flatten
            }
          case (_, Nil) =>
            val conditionBlock = compileExpressionForBranching(ctx, s.condition, NoBranching)
            if (largeThenBlock) {
              val middle = nextLabel("th")
              val end = nextLabel("fi")
              List(conditionBlock, branchChunk(jumpIfTrue, middle), jmpChunk(end), labelChunk(middle), thenBlock, labelChunk(end)).flatten
            } else {
              val end = nextLabel("fi")
              List(conditionBlock, branchChunk(jumpIfFalse, end), thenBlock, labelChunk(end)).flatten
            }
          case _ =>
            val conditionBlock = compileExpressionForBranching(ctx, s.condition, NoBranching)
            if (largeThenBlock) {
              if (largeElseBlock) {
                val middleT = nextLabel("th")
                val middleE = nextLabel("el")
                val end = nextLabel("fi")
                List(conditionBlock, branchChunk(jumpIfTrue, middleT), jmpChunk(middleE), labelChunk(middleT), thenBlock, jmpChunk(end), labelChunk(middleE), elseBlock, labelChunk(end)).flatten
              } else {
                val middle = nextLabel("th")
                val end = nextLabel("fi")
                List(conditionBlock, branchChunk(jumpIfTrue, middle), elseBlock, jmpChunk(end), labelChunk(middle), thenBlock, labelChunk(end)).flatten
              }
            } else {
              val middle = nextLabel("el")
              val end = nextLabel("fi")
              List(conditionBlock, branchChunk(jumpIfFalse, middle), thenBlock, jmpChunk(end), labelChunk(middle), elseBlock, labelChunk(end)).flatten
            }
        }
      case BuiltInBooleanType =>
        (s.thenBranch, s.elseBranch) match {
          case (Nil, Nil) =>
            compileExpressionForBranching(ctx, s.condition, NoBranching)
          case (Nil, _) =>
            if (largeElseBlock) {
              val middle = nextLabel("el")
              val end = nextLabel("fi")
              val conditionBlock = compileExpressionForBranching(ctx, s.condition, BranchIfFalse(middle))
              List(conditionBlock, jmpChunk(end), labelChunk(middle), elseBlock, labelChunk(end)).flatten
            } else {
              val end = nextLabel("fi")
              val conditionBlock = compileExpressionForBranching(ctx, s.condition, BranchIfTrue(end))
              List(conditionBlock, elseBlock, labelChunk(end)).flatten
            }
          case (_, Nil) =>
            if (largeThenBlock) {
              val middle = nextLabel("th")
              val end = nextLabel("fi")
              val conditionBlock = compileExpressionForBranching(ctx, s.condition, BranchIfTrue(middle))
              List(conditionBlock, jmpChunk(end), labelChunk(middle), thenBlock, labelChunk(end)).flatten
            } else {
              val end = nextLabel("fi")
              val conditionBlock = compileExpressionForBranching(ctx, s.condition, BranchIfFalse(end))
              List(conditionBlock, thenBlock, labelChunk(end)).flatten
            }
          case _ =>
            if (largeThenBlock) {
              if (largeElseBlock) {
                val middleT = nextLabel("th")
                val middleE = nextLabel("el")
                val end = nextLabel("fi")
                val conditionBlock = compileExpressionForBranching(ctx, s.condition, BranchIfTrue(middleT))
                List(conditionBlock, jmpChunk(middleE), labelChunk(middleT), thenBlock, jmpChunk(end), labelChunk(middleE), elseBlock, labelChunk(end)).flatten
              } else {
                val middle = nextLabel("th")
                val end = nextLabel("fi")
                val conditionBlock = compileExpressionForBranching(ctx, s.condition, BranchIfTrue(middle))
                List(conditionBlock, elseBlock, jmpChunk(end), labelChunk(middle), thenBlock, labelChunk(end)).flatten
              }
            } else {
              val middle = nextLabel("el")
              val end = nextLabel("fi")
              val conditionBlock = compileExpressionForBranching(ctx, s.condition, BranchIfFalse(middle))
              List(conditionBlock, thenBlock, jmpChunk(end), labelChunk(middle), elseBlock, labelChunk(end)).flatten
            }
        }
      case _ =>
        ErrorReporting.error(s"Illegal type for a condition: `$condType`", s.condition.position)
        Nil
    }

  }
}
