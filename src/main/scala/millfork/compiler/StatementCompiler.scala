package millfork.compiler

import java.util.concurrent.atomic.AtomicLong

import millfork.{CompilationFlag, CompilationOptions}
import millfork.assembly._
import millfork.env._
import millfork.node.{Register, _}
import millfork.assembly.AddrMode._
import millfork.assembly.Opcode._
import millfork.error.ErrorReporting

/**
  * @author Karol Stasiak
  */
object StatementCompiler {

  private def labelChunk(labelName: String) = List(AssemblyLine.label(Label(labelName)))

  private def jmpChunk(labelName: String) =  List(AssemblyLine.absolute(JMP, Label(labelName)))

  private def branchChunk(opcode: Opcode.Value, labelName: String) =  List(AssemblyLine.relative(opcode, Label(labelName)))


  def compile(ctx: CompilationContext, statements: List[ExecutableStatement]): List[AssemblyLine] = {
    statements.flatMap(s => compile(ctx, s))
  }

  def compile(ctx: CompilationContext, statement: ExecutableStatement): List[AssemblyLine] = {
    val env = ctx.env
    val m = ctx.function
    val b = env.get[Type]("byte")
    val w = env.get[Type]("word")
    val someRegisterA = Some(b, RegisterVariable(Register.A, b))
    val someRegisterAX = Some(w, RegisterVariable(Register.AX, w))
    val someRegisterYA = Some(w, RegisterVariable(Register.YA, w))
    val returnInstructions = if (m.interrupt) {
      if (ctx.options.flag(CompilationFlag.EmitNative65816Opcodes)) {
        List(
          AssemblyLine.immediate(REP, 0x30),
          AssemblyLine.implied(PLY),
          AssemblyLine.implied(PLX),
          AssemblyLine.implied(PLA),
          AssemblyLine.implied(PLD),
          AssemblyLine.implied(PLB),
          AssemblyLine.implied(RTI))
      } else
      if (ctx.options.flag(CompilationFlag.EmitEmulation65816Opcodes)) {
        List(
          AssemblyLine.implied(PLY),
          AssemblyLine.implied(PLX),
          AssemblyLine.implied(PLA),
          AssemblyLine.implied(PLD),
          AssemblyLine.implied(PLB),
          AssemblyLine.implied(RTI))
      } else if (ctx.options.flag(CompilationFlag.Emit65CE02Opcodes)) {
        List(
          AssemblyLine.implied(PLZ),
          AssemblyLine.implied(PLY),
          AssemblyLine.implied(PLX),
          AssemblyLine.implied(PLA),
          AssemblyLine.implied(RTI))
      } else if (ctx.options.flag(CompilationFlag.EmitCmosOpcodes)) {
        List(
          AssemblyLine.implied(PLY),
          AssemblyLine.implied(PLX),
          AssemblyLine.implied(PLA),
          AssemblyLine.implied(RTI))
      } else {
        List(
          AssemblyLine.implied(PLA),
          AssemblyLine.implied(TAY),
          AssemblyLine.implied(PLA),
          AssemblyLine.implied(TAX),
          AssemblyLine.implied(PLA),
          AssemblyLine.implied(RTI))
      }
    } else if (m.isFar(ctx.options)) {
      List(AssemblyLine.implied(RTL))
    } else {
      List(AssemblyLine.implied(RTS))
    }
    statement match {
      case AssemblyStatement(o, a, x, e) =>
        val c: Constant = x match {
          // TODO: hmmm
          case VariableExpression(name) =>
            if (OpcodeClasses.ShortBranching(o) || o == JMP || o == LABEL) {
              MemoryAddressConstant(Label(name))
            } else {
              env.evalForAsm(x).getOrElse(env.get[ThingInMemory](name, x.position).toAddress)
            }
          case _ =>
            env.evalForAsm(x).getOrElse(Constant.error(s"`$x` is not a constant", x.position))
        }
        val actualAddrMode = a match {
          case Absolute if OpcodeClasses.ShortBranching(o) => Relative
          case IndexedX if o == JMP => AbsoluteIndexedX
          case Indirect if o != JMP => IndexedZ
          case _ => a
        }
        List(AssemblyLine(o, actualAddrMode, c, e))
      case Assignment(dest, source) =>
        ExpressionCompiler.compileAssignment(ctx, source, dest)
      case ExpressionStatement(e@FunctionCallExpression(name, params)) =>
        env.lookupFunction(name, params.map(p => ExpressionCompiler.getExpressionType(ctx, p) -> p)) match {
          case Some(i: MacroFunction) =>
            val (paramPreparation, inlinedStatements) = MacroExpander.inlineFunction(ctx, i, params, e.position)
            paramPreparation ++ compile(ctx.withInlinedEnv(i.environment), inlinedStatements)
          case _ =>
            ExpressionCompiler.compile(ctx, e, None, NoBranching)
        }
      case ExpressionStatement(e) =>
        ExpressionCompiler.compile(ctx, e, None, NoBranching)
      case BlockStatement(s) =>
        s.flatMap(compile(ctx, _))
      case ReturnStatement(None) =>
        // TODO: return type check
        // TODO: better stackpointer fix
        ctx.function.returnType match {
          case _: BooleanType =>
            stackPointerFixBeforeReturn(ctx) ++ returnInstructions
          case t => t.size match {
            case 0 =>
              stackPointerFixBeforeReturn(ctx) ++
                List(AssemblyLine.discardAF(), AssemblyLine.discardXF(), AssemblyLine.discardYF()) ++ returnInstructions
            case 1 =>
              stackPointerFixBeforeReturn(ctx) ++
                List(AssemblyLine.discardXF(), AssemblyLine.discardYF()) ++ returnInstructions
            case 2 =>
              stackPointerFixBeforeReturn(ctx) ++
                List(AssemblyLine.discardYF()) ++ returnInstructions
          }
        }
      case s : ReturnDispatchStatement =>
        ReturnDispatch.compile(ctx, s)
      case ReturnStatement(Some(e)) =>
        m.returnType match {
          case _: BooleanType =>
            m.returnType.size match {
              case 0 =>
                ErrorReporting.error("Cannot return anything from a void function", statement.position)
                stackPointerFixBeforeReturn(ctx) ++ returnInstructions
              case 1 =>
                ExpressionCompiler.compile(ctx, e, someRegisterA, NoBranching) ++ stackPointerFixBeforeReturn(ctx) ++ returnInstructions
              case 2 =>
                ExpressionCompiler.compile(ctx, e, someRegisterAX, NoBranching) ++ stackPointerFixBeforeReturn(ctx) ++ returnInstructions
            }
          case _ =>
            m.returnType.size match {
              case 0 =>
                ErrorReporting.error("Cannot return anything from a void function", statement.position)
                stackPointerFixBeforeReturn(ctx) ++ List(AssemblyLine.discardAF(), AssemblyLine.discardXF(), AssemblyLine.discardYF()) ++ returnInstructions
              case 1 =>
                ExpressionCompiler.compile(ctx, e, someRegisterA, NoBranching) ++ stackPointerFixBeforeReturn(ctx) ++ List(AssemblyLine.discardXF(), AssemblyLine.discardYF()) ++ returnInstructions
              case 2 =>
                // TODO: ???
                val stackPointerFix = stackPointerFixBeforeReturn(ctx)
                if (stackPointerFix.isEmpty) {
                  ExpressionCompiler.compile(ctx, e, someRegisterAX, NoBranching) ++ List(AssemblyLine.discardYF()) ++ returnInstructions
                } else {
                  ExpressionCompiler.compile(ctx, e, someRegisterYA, NoBranching) ++
                    stackPointerFix ++
                    List(AssemblyLine.implied(TAX), AssemblyLine.implied(TYA), AssemblyLine.discardYF()) ++
                    returnInstructions
                }
            }
        }
      case IfStatement(condition, thenPart, elsePart) =>
        val condType = ExpressionCompiler.getExpressionType(ctx, condition)
        val thenBlock = compile(ctx, thenPart)
        val elseBlock = compile(ctx, elsePart)
        val largeThenBlock = thenBlock.map(_.sizeInBytes).sum > 100
        val largeElseBlock = elseBlock.map(_.sizeInBytes).sum > 100
        condType match {
          case ConstantBooleanType(_, true) => thenBlock
          case ConstantBooleanType(_, false) => elseBlock
          case FlagBooleanType(_, jumpIfTrue, jumpIfFalse) =>
            (thenPart, elsePart) match {
              case (Nil, Nil) => Nil
              case (Nil, _) =>
                val conditionBlock = ExpressionCompiler.compile(ctx, condition, someRegisterA, NoBranching)
                if (largeElseBlock) {
                  val middle = MfCompiler.nextLabel("el")
                  val end = MfCompiler.nextLabel("fi")
                  List(conditionBlock, branchChunk(jumpIfFalse, middle), jmpChunk(end), labelChunk(middle), elseBlock, labelChunk(end)).flatten
                } else {
                  val end = MfCompiler.nextLabel("fi")
                  List(conditionBlock, branchChunk(jumpIfTrue, end), elseBlock, labelChunk(end)).flatten
                }
              case (_, Nil) =>
                val conditionBlock = ExpressionCompiler.compile(ctx, condition, someRegisterA, NoBranching)
                if (largeThenBlock) {
                  val middle = MfCompiler.nextLabel("th")
                  val end = MfCompiler.nextLabel("fi")
                  List(conditionBlock, branchChunk(jumpIfTrue, middle), jmpChunk(end), labelChunk(middle), elseBlock, labelChunk(end)).flatten
                } else {
                  val end = MfCompiler.nextLabel("fi")
                  List(conditionBlock, branchChunk(jumpIfFalse, end), thenBlock, labelChunk(end)).flatten
                }
              case _ =>
                // TODO: large blocks
                if (largeElseBlock || largeThenBlock) ErrorReporting.error("Large blocks in if statement", statement.position)
                val middle = MfCompiler.nextLabel("el")
                val end = MfCompiler.nextLabel("fi")
                val conditionBlock = ExpressionCompiler.compile(ctx, condition, someRegisterA, NoBranching)
                List(conditionBlock, branchChunk(jumpIfFalse, middle), thenBlock, jmpChunk(end), labelChunk(middle), elseBlock, labelChunk(end)).flatten
            }
          case BuiltInBooleanType =>
            (thenPart, elsePart) match {
              case (Nil, Nil) => Nil
              case (Nil, _) =>
                val end = MfCompiler.nextLabel("fi")
                val conditionBlock = ExpressionCompiler.compile(ctx, condition, someRegisterA, BranchIfTrue(end))
                List(conditionBlock, elseBlock, labelChunk(end)).flatten
              case (_, Nil) =>
                val end = MfCompiler.nextLabel("fi")
                val conditionBlock = ExpressionCompiler.compile(ctx, condition, someRegisterA, BranchIfFalse(end))
                List(conditionBlock, thenBlock, labelChunk(end)).flatten
              case _ =>
                val middle = MfCompiler.nextLabel("el")
                val end = MfCompiler.nextLabel("fi")
                val conditionBlock = ExpressionCompiler.compile(ctx, condition, someRegisterA, BranchIfFalse(middle))
                List(conditionBlock, thenBlock, jmpChunk(end), labelChunk(middle), elseBlock, labelChunk(end)).flatten
            }
          case _ =>
            ErrorReporting.error(s"Illegal type for a condition: `$condType`", condition.position)
            Nil
        }
      case WhileStatement(condition, bodyPart) =>
        val condType = ExpressionCompiler.getExpressionType(ctx, condition)
        val bodyBlock = compile(ctx, bodyPart)
        val largeBodyBlock = bodyBlock.map(_.sizeInBytes).sum > 100
        condType match {
          case ConstantBooleanType(_, true) =>
            val conditionBlock = ExpressionCompiler.compile(ctx, condition, someRegisterA, NoBranching)
            val start = MfCompiler.nextLabel("wh")
            List(labelChunk(start), bodyBlock, jmpChunk(start)).flatten
          case ConstantBooleanType(_, false) => Nil
          case FlagBooleanType(_, jumpIfTrue, jumpIfFalse) =>
            if (largeBodyBlock) {
              val start = MfCompiler.nextLabel("wh")
              val middle = MfCompiler.nextLabel("he")
              val end = MfCompiler.nextLabel("ew")
              val conditionBlock = ExpressionCompiler.compile(ctx, condition, someRegisterA, NoBranching)
              List(labelChunk(start), conditionBlock, branchChunk(jumpIfTrue, middle), jmpChunk(end), bodyBlock, jmpChunk(start), labelChunk(end)).flatten
            } else {
              val start = MfCompiler.nextLabel("wh")
              val end = MfCompiler.nextLabel("ew")
              val conditionBlock = ExpressionCompiler.compile(ctx, condition, someRegisterA, NoBranching)
              List(labelChunk(start), conditionBlock, branchChunk(jumpIfFalse, end), bodyBlock, jmpChunk(start), labelChunk(end)).flatten
            }
          case BuiltInBooleanType =>
            if (largeBodyBlock) {
              val start = MfCompiler.nextLabel("wh")
              val middle = MfCompiler.nextLabel("he")
              val end = MfCompiler.nextLabel("ew")
              val conditionBlock = ExpressionCompiler.compile(ctx, condition, someRegisterA, BranchIfTrue(middle))
              List(labelChunk(start), conditionBlock, jmpChunk(end), labelChunk(middle), bodyBlock, jmpChunk(start), labelChunk(end)).flatten
            } else {
              val start = MfCompiler.nextLabel("wh")
              val end = MfCompiler.nextLabel("ew")
              val conditionBlock = ExpressionCompiler.compile(ctx, condition, someRegisterA, BranchIfFalse(end))
              List(labelChunk(start), conditionBlock, bodyBlock, jmpChunk(start), labelChunk(end)).flatten
            }
          case _ =>
            ErrorReporting.error(s"Illegal type for a condition: `$condType`", condition.position)
            Nil
        }
      case DoWhileStatement(bodyPart, condition) =>
        val condType = ExpressionCompiler.getExpressionType(ctx, condition)
        val bodyBlock = compile(ctx, bodyPart)
        val largeBodyBlock = bodyBlock.map(_.sizeInBytes).sum > 100
        condType match {
          case ConstantBooleanType(_, true) =>
            val start = MfCompiler.nextLabel("do")
            val conditionBlock = ExpressionCompiler.compile(ctx, condition, someRegisterA, NoBranching)
            List(labelChunk(start), bodyBlock, jmpChunk(start)).flatten
          case ConstantBooleanType(_, false) => bodyBlock
          case FlagBooleanType(_, jumpIfTrue, jumpIfFalse) =>
            val start = MfCompiler.nextLabel("do")
            val conditionBlock = ExpressionCompiler.compile(ctx, condition, someRegisterA, NoBranching)
            if (largeBodyBlock) {
              val end = MfCompiler.nextLabel("od")
              List(labelChunk(start), bodyBlock, conditionBlock, branchChunk(jumpIfFalse, end), jmpChunk(start), labelChunk(end)).flatten
            } else {
              List(labelChunk(start), bodyBlock, conditionBlock, branchChunk(jumpIfTrue, start)).flatten
            }
          case BuiltInBooleanType =>
            val start = MfCompiler.nextLabel("do")
            if (largeBodyBlock) {
              val end = MfCompiler.nextLabel("od")
              val conditionBlock = ExpressionCompiler.compile(ctx, condition, someRegisterA, BranchIfFalse(end))
              List(labelChunk(start), bodyBlock, conditionBlock, jmpChunk(start), labelChunk(end)).flatten
            } else {
              val conditionBlock = ExpressionCompiler.compile(ctx, condition, someRegisterA, BranchIfTrue(start))
              List(labelChunk(start), bodyBlock, conditionBlock).flatten
            }
          case _ =>
            ErrorReporting.error(s"Illegal type for a condition: `$condType`", condition.position)
            Nil
        }
      case f@ForStatement(variable, start, end, direction, body) =>
        // TODO: check sizes
        // TODO: special faster cases
        val vex = VariableExpression(f.variable)
        val one = LiteralExpression(1, 1)
        val increment = ExpressionStatement(FunctionCallExpression("+=", List(vex, one)))
        val decrement = ExpressionStatement(FunctionCallExpression("-=", List(vex, one)))
        (direction, env.eval(start), env.eval(end)) match {

          case (ForDirection.Until | ForDirection.ParallelUntil, Some(NumericConstant(s, ssize)), Some(NumericConstant(e, _))) if s == e - 1 =>
            compile(ctx, Assignment(vex, f.start) :: f.body)
          case (ForDirection.Until | ForDirection.ParallelUntil, Some(NumericConstant(s, ssize)), Some(NumericConstant(e, _))) if s >= e =>
            Nil

          case (ForDirection.To | ForDirection.ParallelTo, Some(NumericConstant(s, ssize)), Some(NumericConstant(e, _))) if s == e =>
            compile(ctx, Assignment(vex, f.start) :: f.body)
          case (ForDirection.To | ForDirection.ParallelTo, Some(NumericConstant(s, ssize)), Some(NumericConstant(e, _))) if s > e =>
            Nil

          case (ForDirection.ParallelUntil, Some(NumericConstant(0, ssize)), Some(NumericConstant(e, _))) if e > 0 =>
            compile(ctx, List(
              Assignment(vex, f.end),
              DoWhileStatement(decrement :: f.body, FunctionCallExpression("!=", List(vex, f.start)))
            ))

          case (ForDirection.DownTo, Some(NumericConstant(s, ssize)), Some(NumericConstant(e, esize))) if s == e =>
            compile(ctx, Assignment(vex, LiteralExpression(s, ssize)) :: f.body)
          case (ForDirection.DownTo, Some(NumericConstant(s, ssize)), Some(NumericConstant(e, esize))) if s < e =>
            Nil
          case (ForDirection.DownTo, Some(NumericConstant(s, ssize)), Some(NumericConstant(0, esize))) if s > 0 =>
            compile(ctx, List(
              Assignment(vex, f.start),
              DoWhileStatement(f.body :+ decrement, FunctionCallExpression("!=", List(vex, f.end)))
            ))


          case (ForDirection.Until | ForDirection.ParallelUntil, _, _) =>
            compile(ctx, List(
              Assignment(vex, f.start),
              WhileStatement(
                FunctionCallExpression("<", List(vex, f.end)),
                f.body :+ increment),
            ))
//          case (ForDirection.To | ForDirection.ParallelTo, _, Some(NumericConstant(n, _))) if n > 0 && n < 255 =>
//            compile(ctx, List(
//              Assignment(vex, f.start),
//              WhileStatement(
//                FunctionCallExpression("<=", List(vex, f.end)),
//                f.body :+ increment),
//            ))
          case (ForDirection.To | ForDirection.ParallelTo, _, _) =>
            val label = MfCompiler.nextLabel("to")
            compile(ctx, List(
              Assignment(vex, f.start),
              WhileStatement(
                VariableExpression("true"),
                f.body :+ IfStatement(
                  FunctionCallExpression("==", List(vex, f.end)),
                  List(AssemblyStatement(JMP, AddrMode.Absolute, VariableExpression(label), elidable = true)),
                  List(increment))),
              AssemblyStatement(LABEL, AddrMode.DoesNotExist, VariableExpression(label), elidable=true)
            ))
          case (ForDirection.DownTo, _, _) =>
            compile(ctx, List(
              Assignment(vex, f.start),
              IfStatement(
                FunctionCallExpression(">=", List(vex, f.end)),
                List(DoWhileStatement(
                  f.body :+ decrement,
                  FunctionCallExpression("!=", List(vex, f.end))
                )),
                Nil)
            ))
        }
      // TODO
    }
  }

  def stackPointerFixBeforeReturn(ctx: CompilationContext): List[AssemblyLine] = {
    val m = ctx.function
    if (m.stackVariablesSize == 0) return Nil

    if (m.returnType.size == 0 && m.stackVariablesSize <= 2)
      return List.fill(m.stackVariablesSize)(AssemblyLine.implied(PLA))

    if (ctx.options.flag(CompilationFlag.EmitCmosOpcodes)) {
      if (m.returnType.size == 1 && m.stackVariablesSize <= 2) {
        return List.fill(m.stackVariablesSize)(AssemblyLine.implied(PLX))
      }
      if (m.returnType.size == 2 && m.stackVariablesSize <= 2) {
        return List.fill(m.stackVariablesSize)(AssemblyLine.implied(PLY))
      }
    }

    if (ctx.options.flag(CompilationFlag.EmitIllegals)) {
      if (m.returnType.size == 0 && m.stackVariablesSize > 4)
        return List(
          AssemblyLine.implied(TSX),
          AssemblyLine.immediate(LDA, 0xff),
          AssemblyLine.immediate(SBX, 256 - m.stackVariablesSize),
          AssemblyLine.implied(TXS)) // this TXS is fine, it won't appear in 65816 code
      if (m.returnType.size == 1 && m.stackVariablesSize > 6)
        return List(
          AssemblyLine.implied(TAY),
          AssemblyLine.implied(TSX),
          AssemblyLine.immediate(LDA, 0xff),
          AssemblyLine.immediate(SBX, 256 - m.stackVariablesSize),
          AssemblyLine.implied(TXS), // this TXS is fine, it won't appear in 65816 code
          AssemblyLine.implied(TYA))
    }
    AssemblyLine.implied(TSX) :: (List.fill(m.stackVariablesSize)(AssemblyLine.implied(INX)) :+ AssemblyLine.implied(TXS)) // this TXS is fine, it won't appear in 65816 code
  }

}
