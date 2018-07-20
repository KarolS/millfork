package millfork.compiler.mos

import millfork.CompilationFlag
import millfork.assembly.BranchingOpcodeMapping
import millfork.assembly.mos.AddrMode._
import millfork.assembly.mos.Opcode._
import millfork.assembly.mos._
import millfork.compiler._
import millfork.env._
import millfork.error.ErrorReporting
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
    MosExpressionCompiler.compile(ctx, expr, Some(b, RegisterVariable(MosRegister.A, b)), branching)
  }

  def compile(ctx: CompilationContext, statements: List[ExecutableStatement]): List[AssemblyLine] = {
    statements.flatMap(s => compile(ctx, s))
  }

  def compile(ctx: CompilationContext, statement: ExecutableStatement): List[AssemblyLine] = {
    val env = ctx.env
    val m = ctx.function
    val b = env.get[Type]("byte")
    val w = env.get[Type]("word")
    val zpRegisterSize = ctx.options.zpRegisterSize
    lazy val plReg =
      if (zpRegisterSize > 0) {
        val reg = env.get[VariableInMemory]("__reg")
        (zpRegisterSize.-(1) to 0 by (-1)).flatMap{ i=>
          List(
            AssemblyLine.implied(PLA),
            AssemblyLine.zeropage(STA, reg,i))
        }.toList
      } else Nil
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
              AssemblyLine.zeropage(STA, reg, zpRegisterSize - 1),
              AssemblyLine.immediate(REP, 0x30))
          } else {
            List(AssemblyLine.immediate(REP, 0x30))
          }
          val remainingBytes = (zpRegisterSize.&(0xfe).-(2) to 0 by (-2)).flatMap { i =>
            List(
              AssemblyLine.implied(PLA_W),
              AssemblyLine.zeropage(STA_W, reg, i))
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
          AssemblyLine.implied(TAY),
          AssemblyLine.implied(PLA),
          AssemblyLine.implied(TAX),
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
            val (paramPreparation, inlinedStatements) = MosMacroExpander.inlineFunction(ctx, i, params, e.position)
            paramPreparation ++ compile(ctx.withInlinedEnv(i.environment, MosCompiler.nextLabel("en")), inlinedStatements)
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
              ErrorReporting.warn("Returning without a value", ctx.options, statement.position)
              stackPointerFixBeforeReturn(ctx) ++
                List(AssemblyLine.discardXF(), AssemblyLine.discardYF()) ++ returnInstructions
            case 2 =>
              ErrorReporting.warn("Returning without a value", ctx.options, statement.position)
              stackPointerFixBeforeReturn(ctx) ++
                List(AssemblyLine.discardYF()) ++ returnInstructions
          }
        }
      case s : ReturnDispatchStatement =>
        MosReturnDispatch.compile(ctx, s)
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
            AbstractExpressionCompiler.checkAssignmentType(ctx, e, m.returnType)
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
      case s: IfStatement =>
        compileIfStatement(ctx, s)
      case s: WhileStatement =>
        compileWhileStatement(ctx, s)
      case s: DoWhileStatement =>
        compileDoWhileStatement(ctx, s)
      case f:ForStatement =>
        compileForStatement(ctx,f)
      case s:BreakStatement =>
        compileBreakStatement(ctx, s)
      case s:ContinueStatement =>
        compileContinueStatement(ctx, s)
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

  override def nextLabel(prefix: String): String = MosCompiler.nextLabel(prefix)
}
