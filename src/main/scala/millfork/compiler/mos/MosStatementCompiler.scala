package millfork.compiler.mos

import millfork.CompilationFlag
import millfork.assembly.{BranchingOpcodeMapping, Elidability}
import millfork.assembly.mos.AddrMode._
import millfork.assembly.mos.Opcode._
import millfork.assembly.mos._
import millfork.compiler._
import millfork.env._
import millfork.error.ConsoleLogger
import millfork.node.{MosRegister, _}

/**
  * @author Karol Stasiak
  */
object MosStatementCompiler extends AbstractStatementCompiler[AssemblyLine] {

  def labelChunk(labelName: String) = List(AssemblyLine.label(Label(labelName)))

  def jmpChunk(label: Label) =  List(AssemblyLine.absolute(JMP, label))

  def branchChunk(opcode: BranchingOpcodeMapping, labelName: String) =  List(AssemblyLine.relative(opcode.mosOpcode, Label(labelName)))

  def areBlocksLarge(blocks: List[AssemblyLine]*): Boolean = blocks.map(_.map(_.sizeInBytes).sum).sum > 170

  def compileExpressionForBranching(ctx: CompilationContext, expr: Expression, branching: BranchSpec): List[AssemblyLine] = {
    val b = ctx.env.get[Type]("byte")
    val prepareA = MosExpressionCompiler.compile(ctx, expr, Some(b, RegisterVariable(MosRegister.A, b)), branching)
    if (AbstractExpressionCompiler.getExpressionType(ctx, expr) == FatBooleanType) {
      if (MosExpressionCompiler.areNZFlagsBasedOnA(prepareA)) prepareA
      else prepareA :+ AssemblyLine.immediate(CMP, 0)
    } else prepareA
  }

  override def replaceLabel(ctx: CompilationContext, line: AssemblyLine, from: String, to: String): AssemblyLine = line.parameter match {
    case MemoryAddressConstant(Label(l)) if l == from => line.copy(parameter = MemoryAddressConstant(Label(to)))
    case _ => line
  }

  override def returnAssemblyStatement: ExecutableStatement = MosAssemblyStatement(RTS, AddrMode.Implied, LiteralExpression(0,1), Elidability.Elidable)

  override def callChunk(label: ThingInMemory): List[AssemblyLine] = List(AssemblyLine.absolute(JSR, label.toAddress))

  def compile(ctx: CompilationContext, statement: ExecutableStatement): (List[AssemblyLine], List[AssemblyLine]) = {
    val env = ctx.env
    val m = ctx.function
    val b = env.get[Type]("byte")
    val w = env.get[Type]("word")
    val zpRegisterSize = ctx.options.zpRegisterSize
    lazy val plReg =
      (if (ctx.options.flag(CompilationFlag.SoftwareStack)) {
        List(
          AssemblyLine.implied(PLA),
          AssemblyLine.absolute(STA, ctx.env.get[ThingInMemory]("__sp")).copy(elidability = Elidability.Volatile))
      } else Nil) ++ (if (zpRegisterSize > 0) {
        val reg = env.get[VariableInMemory]("__reg")
        (zpRegisterSize.-(1) to 0 by (-1)).flatMap { i =>
          List(
            AssemblyLine.implied(PLA),
            AssemblyLine.zeropage(STA, reg,i).copy(elidability = Elidability.Volatile))
        }.toList
      } else Nil)
    val someRegisterA = Some(b, RegisterVariable(MosRegister.A, b))
    val someRegisterAX = Some(w, RegisterVariable(MosRegister.AX, w))
    val someRegisterYA = Some(w, RegisterVariable(MosRegister.YA, w))
    lazy val returnInstructions = if (m.interrupt) {
      if (ctx.options.flag(CompilationFlag.EmitNative65816Opcodes)) {
        if (zpRegisterSize > 0) {
          val reg = env.get[VariableInMemory]("__reg")
          val lastByte = if (zpRegisterSize % 2 != 0) {
            List(
              AssemblyLine.implied(PLA),
              AssemblyLine.zeropage(STA, reg, zpRegisterSize - 1).copy(elidability = Elidability.Volatile),
              AssemblyLine.immediate(REP, 0x30))
          } else {
            List(AssemblyLine.immediate(REP, 0x30))
          }
          val remainingBytes = (zpRegisterSize.&(0xfe).-(2) to 0 by (-2)).flatMap { i =>
            List(
              AssemblyLine.implied(PLA_W),
              AssemblyLine.zeropage(STA_W, reg, i).copy(elidability = Elidability.Volatile))
          }
          lastByte ++ remainingBytes ++
            List(
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
          AssemblyLine.implied(TAY).copy(elidability = Elidability.Fixed),
          AssemblyLine.implied(PLA),
          AssemblyLine.implied(TAX).copy(elidability = Elidability.Fixed),
          AssemblyLine.implied(PLA),
          AssemblyLine.implied(RTI))
      }
    } else {
      (if (m.kernalInterrupt && zpRegisterSize > 0) {
        if (ctx.options.flag(CompilationFlag.EmitNative65816Opcodes)) {
          val reg = env.get[VariableInMemory]("__reg")
          val lastByte = if (zpRegisterSize % 2 != 0) {
            List(
              AssemblyLine.implied(PLA),
              AssemblyLine.zeropage(STA, reg, zpRegisterSize - 1),
              AssemblyLine.accu16)
          } else {
            List(AssemblyLine.accu16)
          }
          val remainingBytes = (zpRegisterSize.&(0xfe).-(2) to 0 by (-2)).flatMap { i =>
            List(
              AssemblyLine.implied(PLA_W),
              AssemblyLine.zeropage(STA_W, reg, i),
              AssemblyLine.accu8)
          }
          lastByte ++ remainingBytes
        } else plReg
      } else Nil) ++ (if (m.isFar(ctx.options)) {
        List(AssemblyLine.implied(RTL))
      } else {
        List(AssemblyLine.implied(RTS))
      })
    }
    val code: (List[AssemblyLine], List[AssemblyLine]) = statement match {
      case EmptyStatement(stmts) =>
        stmts.foreach(s => compile(ctx, s))
        Nil -> Nil
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
            env.evalForAsm(x).getOrElse(env.errorConstant(s"`$x` is not a constant", x.position))
        }
        val actualAddrMode = a match {
          case Absolute if OpcodeClasses.ShortBranching(o) => Relative
          case Absolute if OpcodeClasses.SupportsZeropage(o) && c.fitsProvablyIntoByte => ZeroPage
          case IndexedX if o == JMP => AbsoluteIndexedX
          case Indirect if o != JMP => IndexedZ
          case _ => a
        }
        List(AssemblyLine(o, actualAddrMode, c, e)) -> Nil
      case RawBytesStatement(contents, _) =>
        env.extractArrayContents(contents).map { expr =>
          env.eval(expr) match {
            case Some(c) => AssemblyLine(BYTE, RawByte, c, elidability = Elidability.Fixed)
            case None =>
              ctx.log.error("Non-constant raw byte", position = statement.position)
              AssemblyLine(BYTE, RawByte, Constant.Zero, elidability = Elidability.Fixed)
          }
        } -> Nil
      case Assignment(dest, source) =>
        MosExpressionCompiler.compileAssignment(ctx, source, dest) -> Nil
      case ExpressionStatement(e@FunctionCallExpression(name, params)) =>
        env.maybeGet[Type](name) match {
          case Some(_) =>
            params.flatMap(p => MosExpressionCompiler.compile(ctx, p, None, NoBranching))-> Nil
          case None =>
            env.lookupFunction(name, params.map(p => MosExpressionCompiler.getExpressionType(ctx, p) -> p)) match {
              case Some(i: MacroFunction) =>
                val (paramPreparation, inlinedStatements) = MosMacroExpander.inlineFunction(ctx, i, params, e.position)
                paramPreparation ++  compile(ctx.withInlinedEnv(i.environment, ctx.nextLabel("en")), inlinedStatements)._1 -> Nil
              case _ =>
                MosExpressionCompiler.compile(ctx, e, None, NoBranching) -> Nil
            }
        }
      case ExpressionStatement(e) =>
        e match {
          case VariableExpression(_) | LiteralExpression(_, _) | _:GeneratedConstantExpression =>
            ctx.log.warn("Pointless expression statement", statement.position)
          case _ =>
        }
        MosExpressionCompiler.compile(ctx, e, None, NoBranching) -> Nil
      case ReturnStatement(None) =>
        // TODO: return type check
        // TODO: better stackpointer fix
        (ctx.function.returnType match {
          case _: BooleanType =>
            stackPointerFixBeforeReturn(ctx) ++ returnInstructions
          case t => t.size match {
            case 0 =>
              stackPointerFixBeforeReturn(ctx) ++
                List(AssemblyLine.discardAF(), AssemblyLine.discardXF(), AssemblyLine.discardYF()) ++ returnInstructions
            case 1 =>
              if (statement.position.isDefined){
                ctx.log.warn("Returning without a value", statement.position)
              }
              stackPointerFixBeforeReturn(ctx) ++
                List(AssemblyLine.discardXF(), AssemblyLine.discardYF()) ++ returnInstructions
            case 2 =>
              if (statement.position.isDefined){
                ctx.log.warn("Returning without a value", statement.position)
              }
              stackPointerFixBeforeReturn(ctx) ++
                List(AssemblyLine.discardYF()) ++ returnInstructions
            case _ =>
              if (statement.position.isDefined){
                ctx.log.warn("Returning without a value", statement.position)
              }
              stackPointerFixBeforeReturn(ctx) ++
                List(AssemblyLine.discardAF(), AssemblyLine.discardXF(), AssemblyLine.discardYF()) ++ returnInstructions
          }
        }) -> Nil
      case s : ReturnDispatchStatement =>
        MosReturnDispatch.compile(ctx, s) -> Nil
      case ReturnStatement(Some(e)) =>
        (m.returnType match {
          case _: BooleanType =>
            m.returnType.size match {
              case 0 =>
                ctx.log.error("Cannot return anything from a void function", statement.position)
                stackPointerFixBeforeReturn(ctx) ++ returnInstructions
              case 1 =>
                MosExpressionCompiler.compile(ctx, e, someRegisterA, NoBranching) ++ stackPointerFixBeforeReturn(ctx, preserveA = true) ++ returnInstructions
              case 2 =>
                MosExpressionCompiler.compile(ctx, e, someRegisterAX, NoBranching) ++ stackPointerFixBeforeReturn(ctx, preserveA = true, preserveX = true) ++ returnInstructions
              case _ =>
                // TODO: is this case ever used?
                MosExpressionCompiler.compileAssignment(ctx, e, VariableExpression(ctx.function.name + "`return")) ++
                  stackPointerFixBeforeReturn(ctx) ++ returnInstructions
            }
          case FatBooleanType =>
            MosExpressionCompiler.compileToFatBooleanInA(ctx, e) ++
              stackPointerFixBeforeReturn(ctx, preserveA = true) ++
              List(AssemblyLine.discardXF(), AssemblyLine.discardYF()) ++ returnInstructions
          case _ =>
            AbstractExpressionCompiler.checkAssignmentType(ctx, e, m.returnType)
            m.returnType.size match {
              case 0 =>
                ctx.log.error("Cannot return anything from a void function", statement.position)
                stackPointerFixBeforeReturn(ctx) ++ List(AssemblyLine.discardAF(), AssemblyLine.discardXF(), AssemblyLine.discardYF()) ++ returnInstructions
              case 1 =>
                MosExpressionCompiler.compile(ctx, e, someRegisterA, NoBranching) ++ stackPointerFixBeforeReturn(ctx, preserveA = true) ++ List(AssemblyLine.discardXF(), AssemblyLine.discardYF()) ++ returnInstructions
              case 2 =>
                // TODO: ???
                val stackPointerFix = stackPointerFixBeforeReturn(ctx, preserveA = true, preserveY = true)
                if (stackPointerFix.isEmpty) {
                  MosExpressionCompiler.compile(ctx, e, someRegisterAX, NoBranching) ++ List(AssemblyLine.discardYF()) ++ returnInstructions
                } else {
                  MosExpressionCompiler.compile(ctx, e, someRegisterYA, NoBranching) ++
                    stackPointerFix ++
                    List(AssemblyLine.implied(TAX), AssemblyLine.implied(TYA), AssemblyLine.discardYF()) ++
                    returnInstructions
                }
              case _ =>
                if (ctx.function.hasElidedReturnVariable) {
                    stackPointerFixBeforeReturn(ctx) ++ List(AssemblyLine.discardAF(), AssemblyLine.discardXF(), AssemblyLine.discardYF()) ++ returnInstructions
                } else {
                  MosExpressionCompiler.compileAssignment(ctx, e, VariableExpression(ctx.function.name + ".return")) ++
                    stackPointerFixBeforeReturn(ctx) ++ List(AssemblyLine.discardAF(), AssemblyLine.discardXF(), AssemblyLine.discardYF()) ++ returnInstructions
                }
            }
        }) -> Nil
      case s: GotoStatement =>
        env.eval(s.target) match {
          case Some(e) => List(AssemblyLine.absolute(JMP, e)) -> Nil
          case None =>
            MosExpressionCompiler.compileToZReg(ctx, s.target) ++ List(AssemblyLine(JMP, Indirect, env.get[ThingInMemory]("__reg.loword").toAddress)) -> Nil
        }
      case s: LabelStatement =>
        List(AssemblyLine.label(env.prefix + s.name)) -> Nil
      case s: IfStatement =>
        compileIfStatement(ctx, s)
      case s: WhileStatement =>
        compileWhileStatement(ctx, s)
      case s: DoWhileStatement =>
        compileDoWhileStatement(ctx, s)
      case f@ForStatement(variable, _, _, _, List(Assignment(target: IndexedExpression, source: Expression))) if !source.containsVariable(variable) =>
        MosBulkMemoryOperations.compileMemset(ctx, target, source, f) -> Nil
      case f@ForStatement(variable, start, end, _, List(ExpressionStatement(
        FunctionCallExpression(operator@("+=" | "-=" | "+'=" | "-'=" | "|=" | "^=" | "&="), List(target: VariableExpression, source))
      ))) if !target.containsVariable(variable) && !source.containsVariable(variable) && !start.containsVariable(target.name) && !end.containsVariable(target.name) =>
        MosBulkMemoryOperations.compileFold(ctx, target, operator, source, f) match {
          case Some(x) => x -> Nil
          case None => compileForStatement(ctx, f)
        }
      case f@ForStatement(variable, start, end, _, List(ExpressionStatement(
        FunctionCallExpression(operator@("+=" | "-=" | "<<=" | ">>="), List(target: IndexedExpression, source))
      ))) if !source.containsVariable(variable) && !start.containsVariable(target.name) && !end.containsVariable(target.name) && target.name != variable =>
        MosBulkMemoryOperations.compileMemmodify(ctx, target, operator, source, f) match {
          case Some(x) => x -> Nil
          case None => compileForStatement(ctx, f)
        }
      case f:ForStatement =>
        compileForStatement(ctx,f)
      case f:ForEachStatement =>
        compileForEachStatement(ctx, f)
      case s:BreakStatement =>
        compileBreakStatement(ctx, s) -> Nil
      case s:ContinueStatement =>
        compileContinueStatement(ctx, s) -> Nil
    }
    code._1.map(_.positionIfEmpty(statement.position)) -> code._2.map(_.positionIfEmpty(statement.position))
  }

  private def stackPointerFixBeforeReturn(ctx: CompilationContext, preserveA: Boolean = false, preserveX: Boolean = false, preserveY: Boolean = false): List[AssemblyLine] = {
    val m = ctx.function
    if (m.stackVariablesSize == 0 && m.name != "main") return Nil
    if (ctx.options.flag(CompilationFlag.SoftwareStack)) {
      // TODO
      val stackPointer = ctx.env.get[ThingInMemory]("__sp")
      if (m.name == "main") {
        List(
          AssemblyLine.immediate(LDY, 0xff),
          AssemblyLine.absolute(STY, stackPointer))
      } else if (m.stackVariablesSize < 3) {
        List.fill(m.stackVariablesSize)(AssemblyLine.absolute(INC, stackPointer))
      } else if (!preserveA) {
        List(AssemblyLine.absolute(LDA, stackPointer),
          AssemblyLine.implied(CLC),
          AssemblyLine.immediate(ADC, m.stackVariablesSize),
          AssemblyLine.absolute(STA, stackPointer))
      } else if (!preserveY) {
        List(AssemblyLine.implied(TAY),
          AssemblyLine.absolute(LDA, stackPointer),
          AssemblyLine.implied(CLC),
          AssemblyLine.immediate(ADC, m.stackVariablesSize),
          AssemblyLine.absolute(STA, stackPointer),
          AssemblyLine.implied(TYA))
      } else if (!preserveX) {
        List(AssemblyLine.implied(TAY),
          AssemblyLine.absolute(LDA, stackPointer),
          AssemblyLine.implied(CLC),
          AssemblyLine.immediate(ADC, m.stackVariablesSize),
          AssemblyLine.absolute(STA, stackPointer),
          AssemblyLine.implied(TYA))
      } else ???
    } else {
      if (!preserveA && m.stackVariablesSize <= 2)
        return List.fill(m.stackVariablesSize)(AssemblyLine.implied(PLA))

      if (ctx.options.flag(CompilationFlag.EmitCmosOpcodes)) {
        if (!preserveX && m.stackVariablesSize <= 2) {
          return List.fill(m.stackVariablesSize)(AssemblyLine.implied(PLX))
        }
        if (!preserveY && m.stackVariablesSize <= 2) {
          return List.fill(m.stackVariablesSize)(AssemblyLine.implied(PLY))
        }
      }

      if (ctx.options.flag(CompilationFlag.EmitIllegals) && !preserveX) {
        // TODO
        if (!preserveA && m.stackVariablesSize > 4)
          return List(
            AssemblyLine.implied(TSX),
            AssemblyLine.immediate(LDA, 0xff),
            AssemblyLine.immediate(SBX, 256 - m.stackVariablesSize),
            AssemblyLine.implied(TXS)) // this TXS is fine, it won't appear in 65816 code
        if (!preserveY && m.stackVariablesSize > 6)
          return List(
            AssemblyLine.implied(TAY),
            AssemblyLine.implied(TSX),
            AssemblyLine.immediate(LDA, 0xff),
            AssemblyLine.immediate(SBX, 256 - m.stackVariablesSize),
            AssemblyLine.implied(TXS), // this TXS is fine, it won't appear in 65816 code
            AssemblyLine.implied(TYA))
      }
      if (!preserveX) {
        AssemblyLine.implied(TSX) :: (List.fill(m.stackVariablesSize)(AssemblyLine.implied(INX)) :+ AssemblyLine.implied(TXS)) // this TXS is fine, it won't appear in 65816 code
      } else {
        // TODO: figure out if there's nothing better
        if (!preserveA) {
          List.fill(m.stackVariablesSize)(AssemblyLine.implied(PLA))
        } else if (ctx.options.flag(CompilationFlag.EmitCmosOpcodes) && !preserveY) {
          List.fill(m.stackVariablesSize)(AssemblyLine.implied(PLY))
        } else if (!preserveY) {
          AssemblyLine.implied(TAY) :: (List.fill(m.stackVariablesSize)(AssemblyLine.implied(PLA)) :+ AssemblyLine.implied(TYA))
        } else ???
      }
    }
  }
}
