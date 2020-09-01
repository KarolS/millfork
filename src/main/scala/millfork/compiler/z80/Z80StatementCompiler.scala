package millfork.compiler.z80

import millfork.CompilationFlag
import millfork.assembly.{BranchingOpcodeMapping, Elidability}
import millfork.assembly.z80._
import millfork.compiler._
import millfork.env._
import millfork.node._
import millfork.assembly.z80.ZOpcode._
import millfork.error.ConsoleLogger

/**
  * @author Karol Stasiak
  */
object Z80StatementCompiler extends AbstractStatementCompiler[ZLine] {


  def compile(ctx: CompilationContext, statement: ExecutableStatement): (List[ZLine], List[ZLine])= {
    val options = ctx.options
    val env = ctx.env
    val ret = Z80Compiler.restoreRegistersAndReturn(ctx)
    val code: (List[ZLine], List[ZLine]) = statement match {
      case EmptyStatement(stmts) =>
        stmts.foreach(s => compile(ctx, s))
        Nil -> Nil
      case ReturnStatement(None) =>
        fixStackOnReturn(ctx) ++ (ctx.function.returnType match {
          case _: BooleanType =>
            List(ZLine.implied(DISCARD_A), ZLine.implied(DISCARD_HL), ZLine.implied(DISCARD_BC), ZLine.implied(DISCARD_DE)) ++ ret
          case t => t.size match {
            case 0 =>
              List(ZLine.implied(DISCARD_F), ZLine.implied(DISCARD_A), ZLine.implied(DISCARD_HL), ZLine.implied(DISCARD_BC), ZLine.implied(DISCARD_DE)) ++ ret
            case _ =>
              ctx.log.warn("Returning without a value", statement.position)
              List(ZLine.implied(DISCARD_F), ZLine.implied(DISCARD_A), ZLine.implied(DISCARD_HL), ZLine.implied(DISCARD_BC), ZLine.implied(DISCARD_DE)) ++ ret
          }
        }) -> Nil
      case ReturnStatement(Some(e)) =>
        (ctx.function.returnType match {
          case t: BooleanType => t.size match {
            case 0 =>
              ctx.log.error("Cannot return anything from a void function", statement.position)
              fixStackOnReturn(ctx) ++
                List(ZLine.implied(DISCARD_A), ZLine.implied(DISCARD_HL), ZLine.implied(DISCARD_BC), ZLine.implied(DISCARD_DE)) ++ ret
            case 1 =>
              Z80ExpressionCompiler.compileToA(ctx, e) ++ fixStackOnReturn(ctx) ++
                List(ZLine.implied(DISCARD_HL), ZLine.implied(DISCARD_BC), ZLine.implied(DISCARD_DE)) ++ ret
            case 2 =>
              Z80ExpressionCompiler.compileToHL(ctx, e) ++ fixStackOnReturn(ctx) ++
                List(ZLine.implied(DISCARD_A), ZLine.implied(DISCARD_BC), ZLine.implied(DISCARD_DE)) ++ ret
            case _ =>
              Z80ExpressionCompiler.compileToHL(ctx, e) ++ fixStackOnReturn(ctx) ++
                List(ZLine.implied(DISCARD_A), ZLine.implied(DISCARD_BC)) ++ ret
          }
          case FatBooleanType =>
            Z80ExpressionCompiler.compileToFatBooleanInA(ctx, e) ++ fixStackOnReturn(ctx) ++
              List(ZLine.implied(DISCARD_F), ZLine.implied(DISCARD_HL), ZLine.implied(DISCARD_BC), ZLine.implied(DISCARD_DE), ZLine.implied(RET))
          case t =>
            AbstractExpressionCompiler.checkAssignmentType(ctx, e, ctx.function.returnType)
            t.size match {
              case 0 =>
                ctx.log.error("Cannot return anything from a void function", statement.position)
                fixStackOnReturn(ctx) ++
                  List(ZLine.implied(DISCARD_F), ZLine.implied(DISCARD_A), ZLine.implied(DISCARD_HL), ZLine.implied(DISCARD_BC), ZLine.implied(DISCARD_DE), ZLine.implied(RET))
              case 1 =>
                Z80ExpressionCompiler.compileToA(ctx, e) ++ fixStackOnReturn(ctx) ++
                  List(ZLine.implied(DISCARD_F), ZLine.implied(DISCARD_HL), ZLine.implied(DISCARD_BC), ZLine.implied(DISCARD_DE), ZLine.implied(RET))
              case 2 =>
                Z80ExpressionCompiler.compileToHL(ctx, e) ++ fixStackOnReturn(ctx) ++
                  List(ZLine.implied(DISCARD_F), ZLine.implied(DISCARD_A), ZLine.implied(DISCARD_BC), ZLine.implied(DISCARD_DE), ZLine.implied(RET))
              case 3 =>
                Z80ExpressionCompiler.compileToEHL(ctx, e) ++ fixStackOnReturn(ctx) ++
                  List(ZLine.implied(DISCARD_F), ZLine.implied(DISCARD_A), ZLine.implied(DISCARD_BC), ZLine.implied(RET))
              case 4 =>
                Z80ExpressionCompiler.compileToDEHL(ctx, e) ++ fixStackOnReturn(ctx) ++
                  List(ZLine.implied(DISCARD_F), ZLine.implied(DISCARD_A), ZLine.implied(DISCARD_BC), ZLine.implied(RET))
              case _ =>
                if (ctx.function.hasElidedReturnVariable) {
                  fixStackOnReturn(ctx) ++
                    List(ZLine.implied(DISCARD_F), ZLine.implied(DISCARD_A), ZLine.implied(DISCARD_HL), ZLine.implied(DISCARD_BC), ZLine.implied(DISCARD_DE), ZLine.implied(RET))
                } else {
                  Z80ExpressionCompiler.storeLarge(ctx, VariableExpression(ctx.function.name + ".return"), e) ++ fixStackOnReturn(ctx) ++
                    List(ZLine.implied(DISCARD_F), ZLine.implied(DISCARD_A), ZLine.implied(DISCARD_HL), ZLine.implied(DISCARD_BC), ZLine.implied(DISCARD_DE), ZLine.implied(RET))
                }

            }
        }) -> Nil
      case s: GotoStatement =>
        env.eval(s.target) match {
          case Some(e) => List(ZLine(JP, NoRegisters, e)) -> Nil
          case None =>
            Z80ExpressionCompiler.compileToHL(ctx, s.target) ++ List(ZLine(JP, OneRegister(ZRegister.HL), Constant.Zero)) -> Nil
        }
      case s: LabelStatement =>
        List(ZLine.label(env.prefix + s.name)) -> Nil
      case Assignment(destination, source) =>
        if (destination == BlackHoleExpression) return Z80ExpressionCompiler.compile(ctx, source, ZExpressionTarget.NOTHING, NoBranching) -> Nil
        val sourceType = AbstractExpressionCompiler.getExpressionType(ctx, source)
        val targetType = AbstractExpressionCompiler.getExpressionType(ctx, destination)
        AbstractExpressionCompiler.checkAssignmentType(ctx, source, targetType)
        (sourceType.size match {
          case 0 =>
            sourceType match {
              case _:ConstantBooleanType =>
                Z80ExpressionCompiler.compileToA(ctx, source) ++ Z80ExpressionCompiler.storeA(ctx, destination, signedSource = false)
              case _:BooleanType =>
                  // TODO: optimize
                Z80ExpressionCompiler.compileToFatBooleanInA(ctx, source) ++ Z80ExpressionCompiler.storeA(ctx, destination, signedSource = false)
              case _ =>
                ctx.log.error("Cannot assign a void expression", statement.position)
                Z80ExpressionCompiler.compile(ctx, source, ZExpressionTarget.NOTHING, BranchSpec.None) ++
                  Z80ExpressionCompiler.compile(ctx, destination, ZExpressionTarget.NOTHING, BranchSpec.None)
            }
          case 1 => Z80ExpressionCompiler.compileToA(ctx, source) ++ Z80ExpressionCompiler.storeA(ctx, destination, sourceType.isSigned)
          case 2 =>
            ctx.env.eval(source) match {
              case Some(constantWord) =>
                Z80ExpressionCompiler.storeConstantWord(ctx, destination, constantWord, sourceType.isSigned)
              case _ =>
                val load = Z80ExpressionCompiler.compileToHL(ctx, source)
                val store = Z80ExpressionCompiler.storeHL(ctx, destination, sourceType.isSigned)
                load ++ store
            }
          case 3 =>
            val load = Z80ExpressionCompiler.compile(ctx, source, ZExpressionTarget.EHL, BranchSpec.None)
            val store = Z80ExpressionCompiler.storeEHL(ctx, destination, sourceType.isSigned)
            load ++ store
          case 4 =>
            val load = Z80ExpressionCompiler.compile(ctx, source, ZExpressionTarget.DEHL, BranchSpec.None)
            val store = Z80ExpressionCompiler.storeDEHL(ctx, destination, sourceType.isSigned)
            load ++ store
          case s => Z80ExpressionCompiler.storeLarge(ctx, destination, source)
        }) -> Nil
      case s: IfStatement =>
        compileIfStatement(ctx, s)
      case s: WhileStatement =>
        compileWhileStatement(ctx, s)
      case s: DoWhileStatement =>
        compileDoWhileStatement(ctx, s)
      case s: ReturnDispatchStatement =>
        Z80ReturnDispatch.compile(ctx, s) -> Nil

      case f:MemsetStatement =>
        Z80BulkMemoryOperations.compileMemset(ctx, f) -> Nil

      case f@ForStatement(_, _, _, _, List(Assignment(target: IndexedExpression, source: IndexedExpression)), Nil) =>
        Z80BulkMemoryOperations.compileMemcpy(ctx, target, source, f) -> Nil

      case f@ForStatement(variable, _, _, _, List(Assignment(target: IndexedExpression, source: Expression)), Nil) if ctx.env.overlapsVariable(variable, source) =>
        Z80BulkMemoryOperations.compileMemset(ctx, target, source, f) -> Nil

      case f@ForStatement(variable, _, _, _, List(ExpressionStatement(FunctionCallExpression(
      operator@("+=" | "-=" | "|=" | "&=" | "^=" | "+'=" | "-'=" | "<<=" | ">>="),
      List(target: IndexedExpression, source: Expression)
      ))), Nil) =>
        Z80BulkMemoryOperations.compileMemtransform(ctx, target, operator, source, f) -> Nil

      case f@ForStatement(variable, _, _, _, List(
      ExpressionStatement(FunctionCallExpression(
      operator1@("+=" | "-=" | "|=" | "&=" | "^=" | "+'=" | "-'=" | "<<=" | ">>="),
      List(target1: IndexedExpression, source1: Expression)
      )),
      ExpressionStatement(FunctionCallExpression(
      operator2@("+=" | "-=" | "|=" | "&=" | "^=" | "+'=" | "-'=" | "<<=" | ">>="),
      List(target2: IndexedExpression, source2: Expression)
      ))
      ), Nil) =>
        Z80BulkMemoryOperations.compileMemtransform2(ctx, target1, operator1, source1, target2, operator2, source2, f) -> Nil

      case f@ForStatement(variable, _, _, _, List(
      Assignment(target1: IndexedExpression, source1: Expression),
      ExpressionStatement(FunctionCallExpression(
      operator2@("+=" | "-=" | "|=" | "&=" | "^=" | "+'=" | "-'=" | "<<=" | ">>="),
      List(target2: IndexedExpression, source2: Expression)
      ))
      ), Nil) =>
        Z80BulkMemoryOperations.compileMemtransform2(ctx, target1, "=", source1, target2, operator2, source2, f) -> Nil

      case f@ForStatement(variable, _, _, _, List(
      ExpressionStatement(FunctionCallExpression(
      operator1@("+=" | "-=" | "|=" | "&=" | "^=" | "+'=" | "-'=" | "<<=" | ">>="),
      List(target1: IndexedExpression, source1: Expression)
      )),
      Assignment(target2: IndexedExpression, source2: Expression)
      ), Nil) =>
        Z80BulkMemoryOperations.compileMemtransform2(ctx, target1, operator1, source1, target2, "=", source2, f) -> Nil

      case f@ForStatement(variable, _, _, _, List(
      Assignment(target1: IndexedExpression, source1: Expression),
      Assignment(target2: IndexedExpression, source2: Expression)
      ), Nil) =>
        Z80BulkMemoryOperations.compileMemtransform2(ctx, target1, "=", source1, target2, "=", source2, f) -> Nil

      case f: ForStatement =>
        compileForStatement(ctx, f)
      case f:ForEachStatement =>
        compileForEachStatement(ctx, f)
      case s: BreakStatement =>
        compileBreakStatement(ctx, s) -> Nil
      case s: ContinueStatement =>
        compileContinueStatement(ctx, s) -> Nil
      case ExpressionStatement(e@FunctionCallExpression(name, params)) =>
        env.maybeGet[Type](name) match {
          case Some(_) =>
            params.flatMap(p => Z80ExpressionCompiler.compile(ctx, p, ZExpressionTarget.NOTHING)) -> Nil
          case _ =>
            env.lookupFunction(name, params.map(p => Z80ExpressionCompiler.getExpressionType(ctx, p) -> p)) match {
              case Some(i: MacroFunction) =>
                val (paramPreparation, inlinedStatements) = Z80MacroExpander.inlineFunction(ctx, i, params, e.position)
                val (main, extra) = compile(ctx.withInlinedEnv(i.environment, ctx.nextLabel("en")), inlinedStatements)
                paramPreparation ++ main -> extra
              case _ =>
                Z80ExpressionCompiler.compile(ctx, e, ZExpressionTarget.NOTHING) -> Nil
            }
        }
      case ExpressionStatement(e) =>
        Z80ExpressionCompiler.compile(ctx, e, ZExpressionTarget.NOTHING) -> Nil
      case RawBytesStatement(contents, _) =>
        env.extractArrayContents(contents).map { expr =>
          env.eval(expr) match {
            case Some(c) => ZLine(BYTE, NoRegisters, c, elidability = Elidability.Fixed)
            case None =>
              ctx.log.error("Non-constant raw byte", position = statement.position)
              ZLine(BYTE, NoRegisters, Constant.Zero, elidability = Elidability.Fixed)
          }
        } -> Nil
      case Z80AssemblyStatement(op, reg, offset, expression, elidability) =>
        val param: Constant = expression match {
          // TODO: hmmm
          case VariableExpression(name) =>
            if (Seq(JP, JR, DJNZ, LABEL, CHANGED_MEM).contains(op)) {
              val fqName = if (name.startsWith(".")) env.prefix + name else name
              MemoryAddressConstant(Label(fqName))
            } else {
              env.evalForAsm(expression).orElse(env.maybeGet[ThingInMemory](name).map(_.toAddress)).getOrElse(MemoryAddressConstant(Label(name)))
            }
          case _ =>
            env.evalForAsm(expression).getOrElse(env.errorConstant(s"`$expression` is not a constant", Some(expression), expression.position))
        }
        val registers = (reg, offset) match {
          case (OneRegister(r), Some(o)) => env.evalForAsm(expression) match {
            case Some(NumericConstant(v, _)) => OneRegisterOffset(r, v.toInt)
            case Some(_) =>
              ctx.log.error("Non-numeric constant", o.position)
              reg
            case None =>
              ctx.log.error("Inlining failed due to non-constant things", o.position)
              reg
          }
          case (TwoRegisters(t, s), Some(o)) => env.evalForAsm(expression) match {
            case Some(NumericConstant(v, _)) => TwoRegistersOffset(t, s, v.toInt)
            case Some(_) =>
              ctx.log.error("Non-numeric constant", o.position)
              reg
            case None =>
              ctx.log.error("Inlining failed due to non-constant things", o.position)
              reg
          }
          case _ => reg
        }
        List(ZLine(op, registers, param, elidability)) -> Nil
    }
    code._1.map(_.positionIfEmpty(statement.position)) -> code._2.map(_.positionIfEmpty(statement.position))
  }

  private def fixStackOnReturn(ctx: CompilationContext): List[ZLine] = {
    if (ctx.function.stackVariablesSize > 0) {
      import ZRegister._
      val localVariableArea = ctx.function.stackVariablesSize.&(1).+(ctx.function.stackVariablesSize)
      if (ctx.options.flags(CompilationFlag.UseIxForStack)) {
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
      } else if (ctx.options.flags(CompilationFlag.UseIyForStack)) {
        if (ctx.function.returnType.size == 2) {
          List(
            ZLine.ldImm16(IY, localVariableArea),
            ZLine.registers(ADD_16, IY, SP),
            ZLine.ld16(SP, IY),
            ZLine.register(POP, IY))
        } else {
          List(
            ZLine.ldImm16(HL, localVariableArea),
            ZLine.registers(ADD_16, HL, SP),
            ZLine.ld16(SP, HL),
            ZLine.register(POP, IY))
        }
      } else {
        if (ctx.function.returnType.size == 2) {
          if (ctx.options.flags(CompilationFlag.EmitSharpOpcodes)) {
            List(ZLine.imm8(ADD_SP, localVariableArea))
          } else if (localVariableArea == 2) {
            List(ZLine.register(INC_16, SP), ZLine.register(INC_16, SP))
          } else if (ctx.options.flags(CompilationFlag.EmitIntel8080Opcodes)) {
            List(
              ZLine.implied(EX_DE_HL),
              ZLine.ldImm16(HL, localVariableArea),
              ZLine.registers(ADD_16, HL, SP),
              ZLine.ld16(SP, HL),
              ZLine.implied(EX_DE_HL))
          } else {
            ???
          }
        } else if (localVariableArea == 2) {
          List(ZLine.register(POP, HL))
        } else {
          List(
            ZLine.ldImm16(HL, localVariableArea),
            ZLine.registers(ADD_16, HL, SP),
            ZLine.ld16(SP, HL))
        }
      }
    } else Nil
  }

  def labelChunk(labelName: String) = List(ZLine.label(Label(labelName)))

  def jmpChunk(label: Label) = List(ZLine.jump(label))

  def branchChunk(opcode: BranchingOpcodeMapping, labelName: String) = List(ZLine.jump(Label(labelName), opcode.z80Flags))

  def areBlocksLarge(blocks: List[ZLine]*): Boolean = false

  override def compileExpressionForBranching(ctx: CompilationContext, expr: Expression, branching: BranchSpec): List[ZLine] = {
    if (AbstractExpressionCompiler.getExpressionType(ctx, expr) == FatBooleanType) {
      val prepareA = Z80ExpressionCompiler.compile(ctx, expr, ZExpressionTarget.A, branching)
      if (Z80ExpressionCompiler.areSZFlagsBasedOnA(prepareA)) prepareA
      else prepareA :+ ZLine.register(OR, ZRegister.A)
    } else {
      Z80ExpressionCompiler.compile(ctx, expr, ZExpressionTarget.NOTHING, branching)
    }
  }

  override def replaceLabel(ctx: CompilationContext, line: ZLine, from: String, to: String): ZLine = line.parameter match {
    case MemoryAddressConstant(Label(l)) if l == from => line.copy(parameter = MemoryAddressConstant(Label(to)))
    case _ => line
  }

  override def returnAssemblyStatement: ExecutableStatement = Z80AssemblyStatement(RET, NoRegisters, None, LiteralExpression(0,1), Elidability.Elidable)

  override def callChunk(label: ThingInMemory): List[ZLine] = List(ZLine(CALL, NoRegisters, label.toAddress))
}
