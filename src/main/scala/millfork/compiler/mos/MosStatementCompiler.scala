package millfork.compiler.mos

import millfork.CompilationFlag
import millfork.assembly.AddrMode._
import millfork.assembly.mos.Opcode._
import millfork.assembly.mos._
import millfork.env._
import millfork.error.ErrorReporting
import millfork.node.{MosRegister, _}

/**
  * @author Karol Stasiak
  */
object MosStatementCompiler {

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
    val plReg =
      if (ctx.options.flag(CompilationFlag.ZeropagePseudoregister)) {
        val reg = env.get[VariableInMemory]("__reg")
        List(
          AssemblyLine.implied(PLA),
          AssemblyLine.zeropage(STA, reg, 1),
          AssemblyLine.implied(PLA),
          AssemblyLine.zeropage(STA, reg)
        )
      } else Nil
    val someRegisterA = Some(b, RegisterVariable(MosRegister.A, b))
    val someRegisterAX = Some(w, RegisterVariable(MosRegister.AX, w))
    val someRegisterYA = Some(w, RegisterVariable(MosRegister.YA, w))
    val returnInstructions = if (m.interrupt) {
      if (ctx.options.flag(CompilationFlag.EmitNative65816Opcodes)) {
        if (ctx.options.flag(CompilationFlag.ZeropagePseudoregister)) {
          List(
            AssemblyLine.immediate(REP, 0x30),
            AssemblyLine.implied(PLA_W),
            AssemblyLine.zeropage(STA_W, env.get[VariableInMemory]("__reg")),
            AssemblyLine.implied(PLY),
            AssemblyLine.implied(PLX),
            AssemblyLine.implied(PLA),
            AssemblyLine.implied(PLD),
            AssemblyLine.implied(PLB),
            AssemblyLine.implied(RTI))
        } else {
          List(
            AssemblyLine.immediate(REP, 0x30),
            AssemblyLine.implied(PLY),
            AssemblyLine.implied(PLX),
            AssemblyLine.implied(PLA),
            AssemblyLine.implied(PLD),
            AssemblyLine.implied(PLB),
            AssemblyLine.implied(RTI))
        }
      } else
      if (ctx.options.flag(CompilationFlag.EmitEmulation65816Opcodes)) {
        plReg ++ List(
          AssemblyLine.implied(PLY),
          AssemblyLine.implied(PLX),
          AssemblyLine.implied(PLA),
          AssemblyLine.implied(PLD),
          AssemblyLine.implied(PLB),
          AssemblyLine.implied(RTI))
      } else if (ctx.options.flag(CompilationFlag.Emit65CE02Opcodes)) {
        plReg ++ List(
          AssemblyLine.implied(PLZ),
          AssemblyLine.implied(PLY),
          AssemblyLine.implied(PLX),
          AssemblyLine.implied(PLA),
          AssemblyLine.implied(RTI))
      } else if (ctx.options.flag(CompilationFlag.EmitCmosOpcodes)) {
        plReg ++ List(
          AssemblyLine.implied(PLY),
          AssemblyLine.implied(PLX),
          AssemblyLine.implied(PLA),
          AssemblyLine.implied(RTI))
      } else {
        plReg ++ List(
          AssemblyLine.implied(PLA),
          AssemblyLine.implied(TAY),
          AssemblyLine.implied(PLA),
          AssemblyLine.implied(TAX),
          AssemblyLine.implied(PLA),
          AssemblyLine.implied(RTI))
      }
    } else {
      (if (m.kernalInterrupt && ctx.options.flag(CompilationFlag.ZeropagePseudoregister)) {
        if (ctx.options.flag(CompilationFlag.EmitNative65816Opcodes)) {
          List(
            AssemblyLine.accu16,
            AssemblyLine.implied(PLA_W),
            AssemblyLine.zeropage(STA_W, env.get[VariableInMemory]("__reg")),
            AssemblyLine.accu8)
        } else plReg
      } else Nil) ++ (if (m.isFar(ctx.options)) {
        List(AssemblyLine.implied(RTL))
      } else {
        List(AssemblyLine.implied(RTS))
      })
    }
    statement match {
      case MosAssemblyStatement(o, a, x, e) =>
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
      case RawBytesStatement(contents) =>
        env.extractArrayContents(contents).map { expr =>
          env.eval(expr) match {
            case Some(c) => AssemblyLine(BYTE, RawByte, c, elidable = false)
            case None =>
              ErrorReporting.error("Non-constant raw byte", position = statement.position)
              AssemblyLine(BYTE, RawByte, Constant.Zero, elidable = false)
          }
        }
      case Assignment(dest, source) =>
        MosExpressionCompiler.compileAssignment(ctx, source, dest)
      case ExpressionStatement(e@FunctionCallExpression(name, params)) =>
        env.lookupFunction(name, params.map(p => MosExpressionCompiler.getExpressionType(ctx, p) -> p)) match {
          case Some(i: MacroFunction) =>
            val (paramPreparation, inlinedStatements) = MacroExpander.inlineFunction(ctx, i, params, e.position)
            paramPreparation ++ compile(ctx.withInlinedEnv(i.environment), inlinedStatements)
          case _ =>
            MosExpressionCompiler.compile(ctx, e, None, NoBranching)
        }
      case ExpressionStatement(e) =>
        e match {
          case VariableExpression(_) | LiteralExpression(_, _) =>
            ErrorReporting.warn("Pointless expression statement", ctx.options, statement.position)
          case _ =>
        }
        MosExpressionCompiler.compile(ctx, e, None, NoBranching)
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
                MosExpressionCompiler.compile(ctx, e, someRegisterA, NoBranching) ++ stackPointerFixBeforeReturn(ctx) ++ returnInstructions
              case 2 =>
                MosExpressionCompiler.compile(ctx, e, someRegisterAX, NoBranching) ++ stackPointerFixBeforeReturn(ctx) ++ returnInstructions
            }
          case _ =>
            m.returnType.size match {
              case 0 =>
                ErrorReporting.error("Cannot return anything from a void function", statement.position)
                stackPointerFixBeforeReturn(ctx) ++ List(AssemblyLine.discardAF(), AssemblyLine.discardXF(), AssemblyLine.discardYF()) ++ returnInstructions
              case 1 =>
                MosExpressionCompiler.compile(ctx, e, someRegisterA, NoBranching) ++ stackPointerFixBeforeReturn(ctx) ++ List(AssemblyLine.discardXF(), AssemblyLine.discardYF()) ++ returnInstructions
              case 2 =>
                // TODO: ???
                val stackPointerFix = stackPointerFixBeforeReturn(ctx)
                if (stackPointerFix.isEmpty) {
                  MosExpressionCompiler.compile(ctx, e, someRegisterAX, NoBranching) ++ List(AssemblyLine.discardYF()) ++ returnInstructions
                } else {
                  MosExpressionCompiler.compile(ctx, e, someRegisterYA, NoBranching) ++
                    stackPointerFix ++
                    List(AssemblyLine.implied(TAX), AssemblyLine.implied(TYA), AssemblyLine.discardYF()) ++
                    returnInstructions
                }
            }
        }
      case IfStatement(condition, thenPart, elsePart) =>
        val condType = MosExpressionCompiler.getExpressionType(ctx, condition)
        val thenBlock = compile(ctx, thenPart)
        val elseBlock = compile(ctx, elsePart)
        val largeThenBlock = thenBlock.map(_.sizeInBytes).sum > 100
        val largeElseBlock = elseBlock.map(_.sizeInBytes).sum > 100
        condType match {
          case ConstantBooleanType(_, true) =>
            MosExpressionCompiler.compile(ctx, condition, someRegisterA, NoBranching) ++ thenBlock
          case ConstantBooleanType(_, false) =>
            MosExpressionCompiler.compile(ctx, condition, someRegisterA, NoBranching) ++ elseBlock
          case FlagBooleanType(_, jumpIfTrue, jumpIfFalse) =>
            (thenPart, elsePart) match {
              case (Nil, Nil) =>
                MosExpressionCompiler.compile(ctx, condition, someRegisterA, NoBranching)
              case (Nil, _) =>
                val conditionBlock = MosExpressionCompiler.compile(ctx, condition, someRegisterA, NoBranching)
                if (largeElseBlock) {
                  val middle = MosCompiler.nextLabel("el")
                  val end = MosCompiler.nextLabel("fi")
                  List(conditionBlock, branchChunk(jumpIfFalse, middle), jmpChunk(end), labelChunk(middle), elseBlock, labelChunk(end)).flatten
                } else {
                  val end = MosCompiler.nextLabel("fi")
                  List(conditionBlock, branchChunk(jumpIfTrue, end), elseBlock, labelChunk(end)).flatten
                }
              case (_, Nil) =>
                val conditionBlock = MosExpressionCompiler.compile(ctx, condition, someRegisterA, NoBranching)
                if (largeThenBlock) {
                  val middle = MosCompiler.nextLabel("th")
                  val end = MosCompiler.nextLabel("fi")
                  List(conditionBlock, branchChunk(jumpIfTrue, middle), jmpChunk(end), labelChunk(middle), thenBlock, labelChunk(end)).flatten
                } else {
                  val end = MosCompiler.nextLabel("fi")
                  List(conditionBlock, branchChunk(jumpIfFalse, end), thenBlock, labelChunk(end)).flatten
                }
              case _ =>
                val conditionBlock = MosExpressionCompiler.compile(ctx, condition, someRegisterA, NoBranching)
                if (largeThenBlock) {
                  if (largeElseBlock) {
                    val middleT = MosCompiler.nextLabel("th")
                    val middleE = MosCompiler.nextLabel("el")
                    val end = MosCompiler.nextLabel("fi")
                    List(conditionBlock, branchChunk(jumpIfTrue, middleT), jmpChunk(middleE), labelChunk(middleT), thenBlock, jmpChunk(end), labelChunk(middleE), elseBlock, labelChunk(end)).flatten
                  } else {
                    val middle = MosCompiler.nextLabel("th")
                    val end = MosCompiler.nextLabel("fi")
                    List(conditionBlock, branchChunk(jumpIfTrue, middle), elseBlock, jmpChunk(end), labelChunk(middle), thenBlock, labelChunk(end)).flatten
                  }
                } else {
                  val middle = MosCompiler.nextLabel("el")
                  val end = MosCompiler.nextLabel("fi")
                  List(conditionBlock, branchChunk(jumpIfFalse, middle), thenBlock, jmpChunk(end), labelChunk(middle), elseBlock, labelChunk(end)).flatten
                }
            }
          case BuiltInBooleanType =>
            (thenPart, elsePart) match {
              case (Nil, Nil) =>
                MosExpressionCompiler.compile(ctx, condition, someRegisterA, NoBranching)
              case (Nil, _) =>
                if (largeElseBlock) {
                  val middle = MosCompiler.nextLabel("el")
                  val end = MosCompiler.nextLabel("fi")
                  val conditionBlock = MosExpressionCompiler.compile(ctx, condition, someRegisterA, BranchIfFalse(middle))
                  List(conditionBlock, jmpChunk(end), labelChunk(middle), elseBlock, labelChunk(end)).flatten
                } else {
                  val end = MosCompiler.nextLabel("fi")
                  val conditionBlock = MosExpressionCompiler.compile(ctx, condition, someRegisterA, BranchIfTrue(end))
                  List(conditionBlock, elseBlock, labelChunk(end)).flatten
                }
              case (_, Nil) =>
                if (largeThenBlock) {
                  val middle = MosCompiler.nextLabel("th")
                  val end = MosCompiler.nextLabel("fi")
                  val conditionBlock = MosExpressionCompiler.compile(ctx, condition, someRegisterA, BranchIfTrue(middle))
                  List(conditionBlock, jmpChunk(end), labelChunk(middle), thenBlock, labelChunk(end)).flatten
                } else {
                  val end = MosCompiler.nextLabel("fi")
                  val conditionBlock = MosExpressionCompiler.compile(ctx, condition, someRegisterA, BranchIfFalse(end))
                  List(conditionBlock, thenBlock, labelChunk(end)).flatten
                }
              case _ =>
                if (largeThenBlock) {
                  if (largeElseBlock) {
                    val middleT = MosCompiler.nextLabel("th")
                    val middleE = MosCompiler.nextLabel("el")
                    val end = MosCompiler.nextLabel("fi")
                    val conditionBlock = MosExpressionCompiler.compile(ctx, condition, someRegisterA, BranchIfTrue(middleT))
                    List(conditionBlock, jmpChunk(middleE), labelChunk(middleT), thenBlock, jmpChunk(end), labelChunk(middleE), elseBlock, labelChunk(end)).flatten
                  } else {
                    val middle = MosCompiler.nextLabel("th")
                    val end = MosCompiler.nextLabel("fi")
                    val conditionBlock = MosExpressionCompiler.compile(ctx, condition, someRegisterA, BranchIfTrue(middle))
                    List(conditionBlock, elseBlock, jmpChunk(end), labelChunk(middle), thenBlock, labelChunk(end)).flatten
                  }
                } else {
                  val middle = MosCompiler.nextLabel("el")
                  val end = MosCompiler.nextLabel("fi")
                  val conditionBlock = MosExpressionCompiler.compile(ctx, condition, someRegisterA, BranchIfFalse(middle))
                  List(conditionBlock, thenBlock, jmpChunk(end), labelChunk(middle), elseBlock, labelChunk(end)).flatten
                }
            }
          case _ =>
            ErrorReporting.error(s"Illegal type for a condition: `$condType`", condition.position)
            Nil
        }
      case WhileStatement(condition, bodyPart, incrementPart, labels) =>
        val start = MosCompiler.nextLabel("wh")
        val middle = MosCompiler.nextLabel("he")
        val inc = MosCompiler.nextLabel("fp")
        val end = MosCompiler.nextLabel("ew")
        val condType = MosExpressionCompiler.getExpressionType(ctx, condition)
        val bodyBlock = compile(ctx.addLabels(labels, Label(end), Label(inc)), bodyPart)
        val incrementBlock = compile(ctx.addLabels(labels, Label(end), Label(inc)), incrementPart)
        val largeBodyBlock = bodyBlock.map(_.sizeInBytes).sum + incrementBlock.map(_.sizeInBytes).sum > 100
        condType match {
          case ConstantBooleanType(_, true) =>
            List(labelChunk(start), bodyBlock, labelChunk(inc), incrementBlock, jmpChunk(start), labelChunk(end)).flatten
          case ConstantBooleanType(_, false) => Nil
          case FlagBooleanType(_, jumpIfTrue, jumpIfFalse) =>
            if (largeBodyBlock) {
              val conditionBlock = MosExpressionCompiler.compile(ctx, condition, someRegisterA, NoBranching)
              List(labelChunk(start), conditionBlock, branchChunk(jumpIfTrue, middle), jmpChunk(end), labelChunk(middle), bodyBlock, labelChunk(inc), incrementBlock, jmpChunk(start), labelChunk(end)).flatten
            } else {
              val conditionBlock = MosExpressionCompiler.compile(ctx, condition, someRegisterA, NoBranching)
              List(jmpChunk(middle), labelChunk(start), bodyBlock, labelChunk(inc), incrementBlock, labelChunk(middle), conditionBlock,  branchChunk(jumpIfTrue, start), labelChunk(end)).flatten
//              List(labelChunk(start), conditionBlock, branchChunk(jumpIfFalse, end), bodyBlock, labelChunk(inc), incrementBlock, jmpChunk(start), labelChunk(end)).flatten
            }
          case BuiltInBooleanType =>
            if (largeBodyBlock) {
              val conditionBlock = MosExpressionCompiler.compile(ctx, condition, someRegisterA, BranchIfTrue(middle))
              List(labelChunk(start), conditionBlock, jmpChunk(end), labelChunk(middle), bodyBlock, labelChunk(inc), incrementBlock, jmpChunk(start), labelChunk(end)).flatten
            } else {
              val conditionBlock = MosExpressionCompiler.compile(ctx, condition, someRegisterA, BranchIfTrue(start))
              List(jmpChunk(middle), labelChunk(start), bodyBlock, labelChunk(inc), incrementBlock, labelChunk(middle), conditionBlock, labelChunk(end)).flatten
//              List(labelChunk(start), conditionBlock, bodyBlock, labelChunk(inc), incrementBlock, jmpChunk(start), labelChunk(end)).flatten
            }
          case _ =>
            ErrorReporting.error(s"Illegal type for a condition: `$condType`", condition.position)
            Nil
        }
      case DoWhileStatement(bodyPart, incrementPart, condition, labels) =>
        val start = MosCompiler.nextLabel("do")
        val inc = MosCompiler.nextLabel("fp")
        val end = MosCompiler.nextLabel("od")
        val condType = MosExpressionCompiler.getExpressionType(ctx, condition)
        val bodyBlock = compile(ctx.addLabels(labels, Label(end), Label(inc)), bodyPart)
        val incrementBlock = compile(ctx.addLabels(labels, Label(end), Label(inc)), incrementPart)
        val largeBodyBlock = bodyBlock.map(_.sizeInBytes).sum + incrementBlock.map(_.sizeInBytes).sum > 100
        condType match {
          case ConstantBooleanType(_, true) =>
            val conditionBlock = MosExpressionCompiler.compile(ctx, condition, someRegisterA, NoBranching)
            List(labelChunk(start),bodyBlock, labelChunk(inc), incrementBlock, jmpChunk(start), labelChunk(end)).flatten
          case ConstantBooleanType(_, false) =>
            List(bodyBlock, labelChunk(inc), incrementBlock, labelChunk(end)).flatten
          case FlagBooleanType(_, jumpIfTrue, jumpIfFalse) =>
            val conditionBlock = MosExpressionCompiler.compile(ctx, condition, someRegisterA, NoBranching)
            if (largeBodyBlock) {
              List(labelChunk(start), bodyBlock, labelChunk(inc), incrementBlock, conditionBlock, branchChunk(jumpIfFalse, end), jmpChunk(start), labelChunk(end)).flatten
            } else {
              List(labelChunk(start), bodyBlock, labelChunk(inc), incrementBlock, conditionBlock, branchChunk(jumpIfTrue, start), labelChunk(end)).flatten
            }
          case BuiltInBooleanType =>
            if (largeBodyBlock) {
              val conditionBlock = MosExpressionCompiler.compile(ctx, condition, someRegisterA, BranchIfFalse(end))
              List(labelChunk(start), bodyBlock, labelChunk(inc), incrementBlock, conditionBlock, jmpChunk(start), labelChunk(end)).flatten
            } else {
              val conditionBlock = MosExpressionCompiler.compile(ctx, condition, someRegisterA, BranchIfTrue(start))
              List(labelChunk(start), bodyBlock, labelChunk(inc), incrementBlock, conditionBlock, labelChunk(end)).flatten
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
        val names = Set("", "for", variable)
        (direction, env.eval(start), env.eval(end)) match {

          case (ForDirection.Until | ForDirection.ParallelUntil, Some(NumericConstant(s, ssize)), Some(NumericConstant(e, _))) if s == e - 1 =>
            val end = MosCompiler.nextLabel("of")
            compile(ctx.addLabels(names, Label(end), Label(end)), Assignment(vex, f.start) :: f.body) ++ labelChunk(end)
          case (ForDirection.Until | ForDirection.ParallelUntil, Some(NumericConstant(s, ssize)), Some(NumericConstant(e, _))) if s >= e =>
            Nil

          case (ForDirection.To | ForDirection.ParallelTo, Some(NumericConstant(s, ssize)), Some(NumericConstant(e, _))) if s == e =>
            val end = MosCompiler.nextLabel("of")
            compile(ctx.addLabels(names, Label(end), Label(end)), Assignment(vex, f.start) :: f.body) ++ labelChunk(end)
          case (ForDirection.To | ForDirection.ParallelTo, Some(NumericConstant(s, ssize)), Some(NumericConstant(e, _))) if s > e =>
            Nil

          case (ForDirection.ParallelUntil, Some(NumericConstant(0, ssize)), Some(NumericConstant(e, _))) if e > 0 =>
            compile(ctx, List(
              Assignment(vex, f.end),
              DoWhileStatement(Nil, decrement :: f.body, FunctionCallExpression("!=", List(vex, f.start)), names)
            ))

          case (ForDirection.DownTo, Some(NumericConstant(s, ssize)), Some(NumericConstant(e, esize))) if s == e =>
            val end = MosCompiler.nextLabel("of")
            compile(ctx.addLabels(names, Label(end), Label(end)), Assignment(vex, LiteralExpression(s, ssize)) :: f.body) ++ labelChunk(end)
          case (ForDirection.DownTo, Some(NumericConstant(s, ssize)), Some(NumericConstant(e, esize))) if s < e =>
            Nil
          case (ForDirection.DownTo, Some(NumericConstant(s, ssize)), Some(NumericConstant(0, esize))) if s > 0 =>
            compile(ctx, List(
              Assignment(vex, SumExpression(List(false -> f.start, false -> LiteralExpression(1, 1)), decimal = false)),
              DoWhileStatement(decrement :: f.body, Nil, FunctionCallExpression("!=", List(vex, f.end)), names)
            ))


          case (ForDirection.Until | ForDirection.ParallelUntil, _, _) =>
            compile(ctx, List(
              Assignment(vex, f.start),
              WhileStatement(
                FunctionCallExpression("<", List(vex, f.end)),
                f.body, List(increment), names),
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
              Assignment(vex, f.start),
              WhileStatement(
                VariableExpression("true"),
                f.body,
                List(IfStatement(
                  FunctionCallExpression("==", List(vex, f.end)),
                  List(BreakStatement(variable)),
                  List(increment)
                )),
                names),
            ))
          case (ForDirection.DownTo, _, _) =>
            compile(ctx, List(
              Assignment(vex, f.start),
              IfStatement(
                FunctionCallExpression(">=", List(vex, f.end)),
                List(DoWhileStatement(
                  f.body,
                  List(decrement),
                  FunctionCallExpression("!=", List(vex, f.end)),
                  names
                )),
                Nil)
            ))
        }

      case BreakStatement(l) =>
        ctx.breakLabels.get(l) match {
          case None =>
            if (l == "") ErrorReporting.error("`break` outside a loop", statement.position)
            else ErrorReporting.error("Invalid label: " + l, statement.position)
            Nil
          case Some(label) =>
            List(AssemblyLine.absolute(JMP, label))
        }

      case ContinueStatement(l) =>
        ctx.continueLabels.get(l) match {
          case None =>
            if (l == "") ErrorReporting.error("`continue` outside a loop", statement.position)
            else ErrorReporting.error("Invalid label: " + l, statement.position)
            Nil
          case Some(label) =>
            List(AssemblyLine.absolute(JMP, label))
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
