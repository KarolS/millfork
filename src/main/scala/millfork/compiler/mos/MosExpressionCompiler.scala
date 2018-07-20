package millfork.compiler.mos

import millfork.CompilationFlag
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
object MosExpressionCompiler extends AbstractExpressionCompiler[AssemblyLine] {

  def compileConstant(ctx: CompilationContext, expr: Constant, target: Variable): List[AssemblyLine] = {
    target match {
      case RegisterVariable(MosRegister.A, _) => List(AssemblyLine(LDA, Immediate, expr))
      case RegisterVariable(MosRegister.AW, _) =>
        List(
          AssemblyLine.accu16,
          AssemblyLine(LDA_W, WordImmediate, expr),
          AssemblyLine.accu8)
      case RegisterVariable(MosRegister.X, _) => List(AssemblyLine(LDX, Immediate, expr))
      case RegisterVariable(MosRegister.Y, _) => List(AssemblyLine(LDY, Immediate, expr))
      case RegisterVariable(MosRegister.AX, _) => List(
        AssemblyLine(LDA, Immediate, expr.loByte),
        AssemblyLine(LDX, Immediate, expr.hiByte))
      case RegisterVariable(MosRegister.AY, _) => List(
        AssemblyLine(LDA, Immediate, expr.loByte),
        AssemblyLine(LDY, Immediate, expr.hiByte))
      case RegisterVariable(MosRegister.XA, _) => List(
        AssemblyLine(LDA, Immediate, expr.hiByte),
        AssemblyLine(LDX, Immediate, expr.loByte))
      case RegisterVariable(MosRegister.YA, _) => List(
        AssemblyLine(LDA, Immediate, expr.hiByte),
        AssemblyLine(LDY, Immediate, expr.loByte))
      case m: VariableInMemory =>
        val addrMode = if (m.zeropage) ZeroPage else Absolute
        val addr = m.toAddress
        m.typ.size match {
          case 0 => Nil
          case 1 => List(
            AssemblyLine(LDA, Immediate, expr.loByte),
            AssemblyLine(STA, addrMode, addr))
          case 2 => List(
            AssemblyLine(LDA, Immediate, expr.loByte),
            AssemblyLine(STA, addrMode, addr),
            AssemblyLine(LDA, Immediate, expr.hiByte),
            AssemblyLine(STA, addrMode, addr + 1))
          case s => List.tabulate(s)(i => List(
            AssemblyLine(LDA, Immediate, expr.subbyte(i)),
            AssemblyLine(STA, addrMode, addr + i))).flatten
        }
      case StackVariable(_, t, offset) =>
        t.size match {
          case 0 => Nil
          case 1 => List(
            AssemblyLine.implied(TSX),
            AssemblyLine.immediate(LDA, expr.loByte),
            AssemblyLine.absoluteX(STA, offset + ctx.extraStackOffset))
          case 2 => List(
            AssemblyLine.implied(TSX),
            AssemblyLine.immediate(LDA, expr.loByte),
            AssemblyLine.absoluteX(STA, offset + ctx.extraStackOffset),
            AssemblyLine.immediate(LDA, expr.hiByte),
            AssemblyLine.absoluteX(STA, offset + ctx.extraStackOffset + 1))
          case s => AssemblyLine.implied(TSX) :: List.tabulate(s)(i => List(
            AssemblyLine.immediate(LDA, expr.subbyte(i)),
            AssemblyLine.absoluteX(STA, offset + ctx.extraStackOffset + i))).flatten
        }
    }
  }

  def fixTsx(code: List[AssemblyLine]): List[AssemblyLine] = code match {
    case (tsx@AssemblyLine(TSX, _, _, _)) :: xs => tsx :: AssemblyLine.implied(INX) :: fixTsx(xs)
    case (txs@AssemblyLine(TXS, _, _, _)) :: xs => ???
    case x :: xs => x :: fixTsx(xs)
    case Nil => Nil
  }

  def preserveRegisterIfNeeded(ctx: CompilationContext, register: MosRegister.Value, code: List[AssemblyLine]): List[AssemblyLine] = {
    val state = register match {
      case MosRegister.A => State.A
      case MosRegister.X => State.X
      case MosRegister.Y => State.Y
    }

    val cmos = ctx.options.flag(CompilationFlag.EmitCmosOpcodes)
    if (AssemblyLine.treatment(code, state) != Treatment.Unchanged) {
      register match {
        case MosRegister.A => AssemblyLine.implied(PHA) +: fixTsx(code) :+ AssemblyLine.implied(PLA)
        case MosRegister.X => if (cmos) {
          List(
            AssemblyLine.implied(PHA),
            AssemblyLine.implied(PHX),
          ) ++ fixTsx(fixTsx(code)) ++ List(
            AssemblyLine.implied(PLX),
            AssemblyLine.implied(PLA),
          )
        } else {
          List(
            AssemblyLine.implied(PHA),
            AssemblyLine.implied(TXA),
            AssemblyLine.implied(PHA),
          ) ++ fixTsx(fixTsx(code)) ++ List(
            AssemblyLine.implied(PLA),
            AssemblyLine.implied(TAX),
            AssemblyLine.implied(PLA),
          )
        }
        case MosRegister.Y => if (cmos) {
          List(
            AssemblyLine.implied(PHA),
            AssemblyLine.implied(PHY),
          ) ++ fixTsx(fixTsx(code)) ++ List(
            AssemblyLine.implied(PLY),
            AssemblyLine.implied(PLA),
          )
        } else {
          List(
            AssemblyLine.implied(PHA),
            AssemblyLine.implied(TYA),
            AssemblyLine.implied(PHA),
          ) ++ fixTsx(fixTsx(code)) ++ List(
            AssemblyLine.implied(PLA),
            AssemblyLine.implied(TAY),
            AssemblyLine.implied(PLA),
          )
        }
      }
    } else {
      code
    }
  }

  def prepareWordIndexing(ctx: CompilationContext, pointy: Pointy, indexExpression: Expression): List[AssemblyLine] = {
    val w = ctx.env.get[Type]("word")
    if (ctx.options.zpRegisterSize < 2) {
      ErrorReporting.error("16-bit indexing requires a zeropage pseudoregister")
      return Nil
    }
    val reg = ctx.env.get[VariableInMemory]("__reg")
    val compileIndex = compile(ctx, indexExpression, Some(MosExpressionCompiler.getExpressionType(ctx, indexExpression) -> RegisterVariable(MosRegister.YA, w)), BranchSpec.None)
    val prepareRegister = pointy match {
      case ConstantPointy(addr, _, _, _, _) =>
        List(
          AssemblyLine.implied(CLC),
          AssemblyLine.immediate(ADC, addr.hiByte),
          AssemblyLine.zeropage(STA, reg, 1),
          AssemblyLine.immediate(LDA, addr.loByte),
          AssemblyLine.zeropage(STA, reg))
      case VariablePointy(addr, _, _) =>
        List(
          AssemblyLine.implied(CLC),
          AssemblyLine.zeropage(ADC, addr + 1),
          AssemblyLine.zeropage(STA, reg, 1),
          AssemblyLine.zeropage(LDA, addr),
          AssemblyLine.zeropage(STA, reg))
    }
    compileIndex ++ prepareRegister
  }

  def compileByteStorage(ctx: CompilationContext, register: MosRegister.Value, target: LhsExpression): List[AssemblyLine] = {
    val env = ctx.env
    val b = env.get[Type]("byte")
    val store = register match {
      case MosRegister.A => STA
      case MosRegister.X => STX
      case MosRegister.Y => STY
    }
    val transferToA = register match {
      case MosRegister.A => NOP
      case MosRegister.X => TXA
      case MosRegister.Y => TYA
    }
    target match {
      case VariableExpression(name) =>
        val v = env.get[Variable](name)
        v.typ.size match {
          case 0 => ???
          case 1 =>
            v match {
              case mv: VariableInMemory => AssemblyLine.variable(ctx, store, mv)
              case sv@StackVariable(_, _, offset) =>
                if (ctx.options.flags(CompilationFlag.EmitEmulation65816Opcodes)) {
                  AssemblyLine.implied(transferToA) :: AssemblyLine.stackRelative(STA, offset + ctx.extraStackOffset) :: Nil
                } else {
                  AssemblyLine.implied(transferToA) :: AssemblyLine.implied(TSX) :: AssemblyLine.absoluteX(STA, offset + ctx.extraStackOffset) :: Nil
                }
            }
          case s if s > 1 =>
            v match {
              case mv: VariableInMemory =>
                AssemblyLine.variable(ctx, store, mv) ++
                  List(AssemblyLine.immediate(LDA, 0)) ++
                  List.tabulate(s - 1)(i => AssemblyLine.variable(ctx, STA, mv, i + 1)).flatten
              case sv@StackVariable(_, _, offset) =>
                AssemblyLine.implied(transferToA) ::
                  AssemblyLine.implied(TSX) ::
                  AssemblyLine.absoluteX(STA, offset + ctx.extraStackOffset) ::
                  List.tabulate(s - 1)(i => AssemblyLine.absoluteX(STA, offset + ctx.extraStackOffset + i + 1))
            }
        }
      case IndexedExpression(arrayName, indexExpr) =>
        val pointy = env.getPointy(arrayName)
        val (variableIndex, constIndex) = env.evalVariableAndConstantSubParts(indexExpr)
        val variableIndexSize = variableIndex.map(v => getExpressionType(ctx, v).size).getOrElse(0)
        val totalIndexSize = getExpressionType(ctx, indexExpr).size

        def storeToArrayAtUnknownIndex(variableIndex: Expression, arrayAddr: Constant) = {
          // TODO check typ
          val indexRegister = if (register == MosRegister.Y) MosRegister.X else MosRegister.Y
          val calculatingIndex = preserveRegisterIfNeeded(ctx, register, compile(ctx, variableIndex, Some(b, RegisterVariable(indexRegister, b)), NoBranching))
          if (register == MosRegister.A) {
            indexRegister match {
              case MosRegister.Y =>
                calculatingIndex ++ arrayBoundsCheck(ctx, pointy, MosRegister.Y, indexExpr) ++ List(AssemblyLine.absoluteY(STA, arrayAddr + constIndex))
              case MosRegister.X =>
                calculatingIndex ++ arrayBoundsCheck(ctx, pointy, MosRegister.X, indexExpr) ++ List(AssemblyLine.absoluteX(STA, arrayAddr + constIndex))
            }
          } else {
            indexRegister match {
              case MosRegister.Y =>
                calculatingIndex ++ arrayBoundsCheck(ctx, pointy, MosRegister.Y, indexExpr) ++ List(AssemblyLine.implied(transferToA), AssemblyLine.absoluteY(STA, arrayAddr + constIndex))
              case MosRegister.X =>
                calculatingIndex ++ arrayBoundsCheck(ctx, pointy, MosRegister.X, indexExpr) ++ List(AssemblyLine.implied(transferToA), AssemblyLine.absoluteX(STA, arrayAddr + constIndex))
            }
          }
        }
        def wrapWordIndexingStorage(code: List[AssemblyLine]) = {
          val reg = ctx.env.get[VariableInMemory]("__reg")
          val cmos = ctx.options.flag(CompilationFlag.EmitCmosOpcodes)
          register match {
            case MosRegister.A =>
              List(AssemblyLine.implied(PHA)) ++ code ++ List(AssemblyLine.implied(PLA), AssemblyLine.indexedY(STA, reg))
            case MosRegister.X =>
              if (code.exists(l => OpcodeClasses.ChangesX(l.opcode))) {
                if (cmos)
                  List(AssemblyLine.implied(PHX)) ++ code ++ List(AssemblyLine.implied(PLA), AssemblyLine.indexedY(STA, reg))
                else
                  List(AssemblyLine.implied(TXA), AssemblyLine.implied(PHA)) ++ code ++ List(AssemblyLine.implied(PLA), AssemblyLine.indexedY(STA, reg))
              } else {
                code ++ List(AssemblyLine.implied(TXA), AssemblyLine.indexedY(STA, reg))
              }
            case MosRegister.Y =>
              if (cmos)
                List(AssemblyLine.implied(PHY)) ++ code ++ List(AssemblyLine.implied(PLA), AssemblyLine.indexedY(STA, reg))
              else
                List(AssemblyLine.implied(TYA), AssemblyLine.implied(PHA)) ++ code ++ List(AssemblyLine.implied(PLA), AssemblyLine.indexedY(STA, reg))
          }
        }

        (pointy, variableIndex, variableIndexSize, totalIndexSize) match {
          case (p: ConstantPointy, None, _, _) =>
            List(AssemblyLine.absolute(store, env.genRelativeVariable(p.value + constIndex, b, zeropage = false)))
          case (p: VariablePointy, _, _, 2) =>
            wrapWordIndexingStorage(prepareWordIndexing(ctx, p, indexExpr))
          case (p: ConstantPointy, Some(v), 2, _) =>
            val w = env.get[VariableType]("word")
            wrapWordIndexingStorage(prepareWordIndexing(ctx, ConstantPointy(p.value + constIndex, None, if (constIndex.isProvablyZero) p.size else None, w, p.elementType), v))
          case (p: ConstantPointy, Some(v), 1, _) =>
            storeToArrayAtUnknownIndex(v, p.value)
          //TODO: should there be a type check or a zeropage check?
          case (pointerVariable:VariablePointy, None, _, 0 | 1) =>
            register match {
              case MosRegister.A =>
                List(AssemblyLine.immediate(LDY, constIndex), AssemblyLine.indexedY(STA, pointerVariable.addr))
              case MosRegister.Y =>
                List(AssemblyLine.implied(TYA), AssemblyLine.immediate(LDY, constIndex), AssemblyLine.indexedY(STA, pointerVariable.addr), AssemblyLine.implied(TAY))
              case MosRegister.X =>
                List(AssemblyLine.immediate(LDY, constIndex), AssemblyLine.implied(TXA), AssemblyLine.indexedY(STA, pointerVariable.addr))
              case _ =>
                ErrorReporting.error("Cannot store a word in an array", target.position)
                Nil
            }
          case (pointerVariable:VariablePointy, Some(_), _, 0 | 1) =>
            val calculatingIndex = compile(ctx, indexExpr, Some(b, RegisterVariable(MosRegister.Y, b)), NoBranching)
            register match {
              case MosRegister.A =>
                preserveRegisterIfNeeded(ctx, MosRegister.A, calculatingIndex) :+ AssemblyLine.indexedY(STA, pointerVariable.addr)
              case MosRegister.X =>
                preserveRegisterIfNeeded(ctx, MosRegister.X, calculatingIndex) ++ List(AssemblyLine.implied(TXA), AssemblyLine.indexedY(STA, pointerVariable.addr))
              case MosRegister.Y =>
                AssemblyLine.implied(TYA) :: preserveRegisterIfNeeded(ctx, MosRegister.A, calculatingIndex) ++ List(
                  AssemblyLine.indexedY(STA, pointerVariable.addr), AssemblyLine.implied(TAY)
                )
              case _ =>
                ErrorReporting.error("Cannot store a word in an array", target.position)
                Nil
            }
          case _ =>
            ErrorReporting.error("Invalid index for writing", indexExpr.position)
            Nil
        }
    }
  }
  val noop: List[AssemblyLine] = Nil


  def compile(ctx: CompilationContext, expr: Expression, exprTypeAndVariable: Option[(Type, Variable)], branches: BranchSpec): List[AssemblyLine] = {
    val env = ctx.env
    val b = env.get[Type]("byte")
    val w = env.get[Type]("word")
    expr match {
      case HalfWordExpression(expression, _) => ??? // TODO
      case LiteralExpression(value, size) =>
        exprTypeAndVariable.fold(noop) { case (exprType, target) =>
          assertCompatible(exprType, target.typ)
          compileConstant(ctx, NumericConstant(value, size), target)
        }
      case VariableExpression(name) =>
        exprTypeAndVariable.fold(noop) { case (exprType, target) =>
          assertCompatible(exprType, target.typ)
          env.eval(expr).map(c => compileConstant(ctx, c, target)).getOrElse {
            env.get[TypedThing](name) match {
              case source: VariableInMemory =>
                target match {
                  case RegisterVariable(MosRegister.A, _) => AssemblyLine.variable(ctx, LDA, source)
                  case RegisterVariable(MosRegister.AW, _) =>
                    exprType.size match {
                      case 1 => if (exprType.isSigned) {
                        AssemblyLine.variable(ctx, LDA, source) ++ List(
                          AssemblyLine.implied(PHA)) ++ signExtendA() ++ List(
                          AssemblyLine.implied(XBA),
                          AssemblyLine.implied(PLA))
                      } else List(AssemblyLine.immediate(LDX, 0), AssemblyLine.implied(XBA)) ++ AssemblyLine.variable(ctx, LDA, source) :+ AssemblyLine.immediate(LDX, 0)
                      case 2 =>
                        // TODO: use LDA_W
                        AssemblyLine.variable(ctx, LDA, source, 1) ++ List(AssemblyLine.implied(XBA)) ++ AssemblyLine.variable(ctx, LDA, source)
                    }
                  case RegisterVariable(MosRegister.X, _) => AssemblyLine.variable(ctx, LDX, source)
                  case RegisterVariable(MosRegister.Y, _) => AssemblyLine.variable(ctx, LDY, source)
                  case RegisterVariable(MosRegister.AX, _) =>
                    exprType.size match {
                      case 1 => if (exprType.isSigned) {
                        AssemblyLine.variable(ctx, LDA, source) ++ List(
                          AssemblyLine.implied(PHA)) ++ signExtendA() ++ List(
                          AssemblyLine.implied(TAX),
                          AssemblyLine.implied(PLA))
                      } else AssemblyLine.variable(ctx, LDA, source) :+ AssemblyLine.immediate(LDX, 0)
                      case 2 =>
                        AssemblyLine.variable(ctx, LDA, source) ++ AssemblyLine.variable(ctx, LDX, source, 1)
                    }
                  case RegisterVariable(MosRegister.AY, _) =>
                    exprType.size match {
                      case 1 => if (exprType.isSigned) {
                        AssemblyLine.variable(ctx, LDA, source) ++ List(
                          AssemblyLine.implied(PHA)) ++ signExtendA() ++ List(
                          AssemblyLine.implied(TAY),
                          AssemblyLine.implied(PLA))
                      } else {
                        AssemblyLine.variable(ctx, LDA, source) :+ AssemblyLine.immediate(LDY, 0)
                      }
                      case 2 =>
                        AssemblyLine.variable(ctx, LDA, source) ++ AssemblyLine.variable(ctx, LDY, source, 1)
                    }
                  case RegisterVariable(MosRegister.XA, _) =>
                    exprType.size match {
                      case 1 => if (exprType.isSigned) {
                        AssemblyLine.variable(ctx, LDX, source) ++ List(AssemblyLine.implied(TXA)) ++ signExtendA()
                      } else
                        AssemblyLine.variable(ctx, LDX, source) :+ AssemblyLine.immediate(LDA, 0)
                      case 2 =>
                        AssemblyLine.variable(ctx, LDX, source) ++ AssemblyLine.variable(ctx,LDA, source, 1)
                    }
                  case RegisterVariable(MosRegister.YA, _) =>
                    exprType.size match {
                      case 1 => if (exprType.isSigned) {
                        AssemblyLine.variable(ctx, LDY, source) ++ List(AssemblyLine.implied(TYA)) ++ signExtendA()
                      } else
                        AssemblyLine.variable(ctx, LDY, source) :+ AssemblyLine.immediate(LDA, 0)
                      case 2 =>
                        AssemblyLine.variable(ctx, LDY, source) ++ AssemblyLine.variable(ctx, LDA, source, 1)
                    }
                  case target: VariableInMemory =>
                    if (exprType.size > target.typ.size) {
                      ErrorReporting.error(s"Variable `$target.name` is too small", expr.position)
                      Nil
                    } else {
                      val copyFromLo  = List.tabulate(exprType.size)(i => AssemblyLine.variable(ctx, LDA, source, i) ++ AssemblyLine.variable(ctx, STA, target, i))
                      val copy = if (shouldCopyFromHiToLo(source.toAddress, target.toAddress)) copyFromLo.reverse else copyFromLo
                      val extend = if (exprType.size == target.typ.size) Nil else if (exprType.isSigned) {
                        signExtendA() ++ List.tabulate(target.typ.size - exprType.size)(i => AssemblyLine.variable(ctx, STA, target, i + exprType.size)).flatten
                      } else {
                        AssemblyLine.immediate(LDA, 0) ::
                          List.tabulate(target.typ.size - exprType.size)(i => AssemblyLine.variable(ctx, STA, target, i + exprType.size)).flatten
                      }
                      copy.flatten ++ extend
                    }
                  case target: StackVariable =>
                    if (exprType.size > target.typ.size) {
                      ErrorReporting.error(s"Variable `$target.name` is too small", expr.position)
                      Nil
                    } else {
                      val copy = List.tabulate(exprType.size)(i => AssemblyLine.variable(ctx, LDA, source, i) :+ AssemblyLine.absoluteX(STA, target.baseOffset + ctx.extraStackOffset + i))
                      val extend = if (exprType.size == target.typ.size) Nil else if (exprType.isSigned) {
                        signExtendA() ++ List.tabulate(target.typ.size - exprType.size)(i => AssemblyLine.absoluteX(STA, target.baseOffset + ctx.extraStackOffset + i + exprType.size))
                      } else {
                        AssemblyLine.immediate(LDA, 0) ::
                          List.tabulate(target.typ.size - exprType.size)(i => AssemblyLine.absoluteX(STA, target.baseOffset + ctx.extraStackOffset + i + exprType.size))
                      }
                      AssemblyLine.implied(TSX) :: (copy.flatten ++ extend)
                    }
                }
              case source@StackVariable(_, sourceType, offset) =>
                target match {
                  case RegisterVariable(MosRegister.A, _) => List(AssemblyLine.implied(TSX), AssemblyLine.absoluteX(LDA, offset + ctx.extraStackOffset))
                  case RegisterVariable(MosRegister.X, _) => List(AssemblyLine.implied(TSX), AssemblyLine.absoluteX(LDA, offset + ctx.extraStackOffset), AssemblyLine.implied(TAX))
                  case RegisterVariable(MosRegister.Y, _) => List(AssemblyLine.implied(TSX), AssemblyLine.absoluteX(LDY, offset + ctx.extraStackOffset))
                  case RegisterVariable(MosRegister.AX, _) =>
                    exprType.size match {
                      case 1 => if (exprType.isSigned) {
                        List(
                          AssemblyLine.implied(TSX),
                          AssemblyLine.absoluteX(LDA, offset + ctx.extraStackOffset),
                          AssemblyLine.implied(PHA)) ++ signExtendA() ++ List(
                          AssemblyLine.implied(TAX),
                          AssemblyLine.implied(PLA))
                      } else List(
                        AssemblyLine.implied(TSX),
                        AssemblyLine.absoluteX(LDA, offset + ctx.extraStackOffset),
                        AssemblyLine.immediate(LDX, 0))
                      case 2 => List(
                        AssemblyLine.implied(TSX),
                        AssemblyLine.absoluteX(LDA, offset + ctx.extraStackOffset),
                        AssemblyLine.implied(PHA),
                        AssemblyLine.absoluteX(LDA, offset + ctx.extraStackOffset + 1),
                        AssemblyLine.implied(TAX),
                        AssemblyLine.implied(PLA))
                    }
                  case RegisterVariable(MosRegister.AY, _) =>
                    exprType.size match {
                      case 1 => if (exprType.isSigned) {
                        val label = MosCompiler.nextLabel("sx")
                        ??? // TODO
                      } else {
                        List(
                          AssemblyLine.implied(TSX),
                          AssemblyLine.absoluteX(LDA, offset + ctx.extraStackOffset),
                          AssemblyLine.immediate(LDY, 0))
                      }
                      case 2 => List(
                        AssemblyLine.implied(TSX),
                        AssemblyLine.absoluteX(LDA, offset + ctx.extraStackOffset),
                        AssemblyLine.absoluteX(LDY, offset + ctx.extraStackOffset + 1))
                    }
                  case RegisterVariable(MosRegister.XA, _) =>
                    ??? // TODO
                  case RegisterVariable(MosRegister.YA, _) =>
                    exprType.size match {
                      case 1 => if (exprType.isSigned) {
                        val label = MosCompiler.nextLabel("sx")
                        ??? // TODO
                      } else {
                        List(
                          AssemblyLine.implied(TSX),
                          AssemblyLine.absoluteX(LDY, offset + ctx.extraStackOffset),
                          AssemblyLine.immediate(LDA, 0))
                      }
                      case 2 => List(
                        AssemblyLine.implied(TSX),
                        AssemblyLine.absoluteX(LDY, offset + ctx.extraStackOffset),
                        AssemblyLine.absoluteX(LDA, offset + ctx.extraStackOffset + 1))
                    }
                  case target: VariableInMemory =>
                    if (exprType.size > target.typ.size) {
                      ErrorReporting.error(s"Variable `$target.name` is too small", expr.position)
                      Nil
                    } else {
                      val copy = List.tabulate(exprType.size)(i => AssemblyLine.absoluteX(LDA, offset + ctx.extraStackOffset + i) :: AssemblyLine.variable(ctx, STA, target, i))
                      val extend = if (exprType.size == target.typ.size) Nil else if (exprType.isSigned) {
                        signExtendA() ++ List.tabulate(target.typ.size - exprType.size)(i => AssemblyLine.variable(ctx, STA, target, i + exprType.size)).flatten
                      } else {
                        AssemblyLine.immediate(LDA, 0) ::
                          List.tabulate(target.typ.size - exprType.size)(i => AssemblyLine.variable(ctx, STA, target, i + exprType.size)).flatten
                      }
                      AssemblyLine.implied(TSX) :: (copy.flatten ++ extend)
                    }
                  case target: StackVariable =>
                    if (exprType.size > target.typ.size) {
                      ErrorReporting.error(s"Variable `$target.name` is too small", expr.position)
                      Nil
                    } else {
                      val copyFromLo = List.tabulate(exprType.size)(i => List(AssemblyLine.absoluteX(LDA, offset + ctx.extraStackOffset + i), AssemblyLine.absoluteX(STA, target.baseOffset + i)))
                      val copy = if (shouldCopyFromHiToLo(NumericConstant(source.baseOffset, 2), NumericConstant(target.baseOffset, 2))) copyFromLo.reverse else copyFromLo
                      val extend = if (exprType.size == target.typ.size) Nil else if (exprType.isSigned) {
                        signExtendA() ++ List.tabulate(target.typ.size - exprType.size)(i => AssemblyLine.absoluteX(STA, target.baseOffset + ctx.extraStackOffset + i + exprType.size))
                      } else {
                        AssemblyLine.immediate(LDA, 0) ::
                          List.tabulate(target.typ.size - exprType.size)(i => AssemblyLine.absoluteX(STA, target.baseOffset + ctx.extraStackOffset + i + exprType.size))
                      }
                      AssemblyLine.implied(TSX) :: (copy.flatten ++ extend)
                    }
                }
              case source@ConstantThing(_, value, _) =>
                compileConstant(ctx, value, target)
            }
          }
        }
      case IndexedExpression(arrayName, indexExpr) =>
        val pointy = env.getPointy(arrayName)
        AbstractExpressionCompiler.checkIndexType(ctx, pointy, indexExpr)
        // TODO: check
        val (variableIndex, constantIndex) = env.evalVariableAndConstantSubParts(indexExpr)
        val variableIndexSize = variableIndex.map(v => getExpressionType(ctx, v).size).getOrElse(0)
        val totalIndexSize = getExpressionType(ctx, indexExpr).size
        exprTypeAndVariable.fold(compile(ctx, indexExpr, None, BranchSpec.None)) { case (exprType, target) =>

          val register = target match {
            case RegisterVariable(r, _) => r
            case _ => MosRegister.A
          }
          val suffix = target match {
            case RegisterVariable(_, _) => Nil
            case target: VariableInMemory =>
              if (target.typ.size == 1) {
                AssemblyLine.variable(ctx, STA, target)
              }
              else if (target.typ.isSigned) {
                AssemblyLine.variable(ctx, STA, target) ++ signExtendA() ++
                  List.tabulate(target.typ.size - 1)(i => AssemblyLine.variable(ctx, STA, target, i + 1)).flatten
              } else {
                AssemblyLine.variable(ctx, STA, target) ++
                  List(AssemblyLine.immediate(LDA, 0)) ++
                  List.tabulate(target.typ.size - 1)(i => AssemblyLine.variable(ctx, STA, target, i + 1)).flatten
              }
          }
          val load = register match {
            case MosRegister.A | MosRegister.AX | MosRegister.AY => LDA
            case MosRegister.X => LDX
            case MosRegister.Y => LDY
          }

          def loadFromArrayAtUnknownIndex(variableIndex: Expression, arrayAddr: Constant) = {
            // TODO check typ
            val indexRegister = if (register == MosRegister.Y) MosRegister.X else MosRegister.Y
            val calculatingIndex = compile(ctx, variableIndex, Some(b, RegisterVariable(indexRegister, b)), NoBranching)
            indexRegister match {
              case MosRegister.Y =>
                calculatingIndex ++ arrayBoundsCheck(ctx, pointy, MosRegister.Y, indexExpr) ++ List(AssemblyLine.absoluteY(load, arrayAddr + constantIndex))
              case MosRegister.X =>
                calculatingIndex ++ arrayBoundsCheck(ctx, pointy, MosRegister.X, indexExpr) ++ List(AssemblyLine.absoluteX(load, arrayAddr + constantIndex))
            }
          }

          def loadFromReg() = {
            val reg = ctx.env.get[VariableInMemory]("__reg")
            register match {
              case MosRegister.A =>
                List(AssemblyLine.indexedY(LDA, reg))
              case MosRegister.X =>
                List(AssemblyLine.indexedY(LDA, reg), AssemblyLine.implied(TAX))
              case MosRegister.Y =>
                List(AssemblyLine.indexedY(LDA, reg), AssemblyLine.implied(TAY))
            }
          }
          val result = (pointy, variableIndex, totalIndexSize, variableIndexSize) match {
            case (a: ConstantPointy, None, _, _) =>
              List(AssemblyLine.absolute(load, env.genRelativeVariable(a.value + constantIndex, b, zeropage = false)))
            case (a: ConstantPointy, Some(v), _, 1) =>
              loadFromArrayAtUnknownIndex(v, a.value)
            case (a: ConstantPointy, Some(v), _, 2) =>
              prepareWordIndexing(ctx, ConstantPointy(
                a.value + constantIndex,
                None,
                if (constantIndex.isProvablyZero) a.size else None,
                env.get[VariableType]("word"),
                a.elementType), v) ++ loadFromReg()
            case (a: VariablePointy, _, 2, _) =>
              prepareWordIndexing(ctx, a, indexExpr) ++ loadFromReg()
            case (p:VariablePointy, None, 0 | 1, _) =>
              register match {
                case MosRegister.A =>
                  List(AssemblyLine.immediate(LDY, constantIndex), AssemblyLine.indexedY(LDA, p.addr))
                case MosRegister.Y =>
                  List(AssemblyLine.immediate(LDY, constantIndex), AssemblyLine.indexedY(LDA, p.addr), AssemblyLine.implied(TAY))
                case MosRegister.X =>
                  List(AssemblyLine.immediate(LDY, constantIndex), AssemblyLine.indexedY(LDA, p.addr), AssemblyLine.implied(TAX))
              }
            case (p:VariablePointy, Some(_), 0 | 1, _) =>
              val calculatingIndex = compile(ctx, indexExpr, Some(b, RegisterVariable(MosRegister.Y, b)), NoBranching)
              register match {
                case MosRegister.A =>
                  calculatingIndex :+ AssemblyLine.indexedY(LDA, p.addr)
                case MosRegister.X =>
                  calculatingIndex ++ List(AssemblyLine.indexedY(LDA, p.addr), AssemblyLine.implied(TAX))
                case MosRegister.Y =>
                  calculatingIndex ++ List(AssemblyLine.indexedY(LDA, p.addr), AssemblyLine.implied(TAY))
              }
            case _ =>
              ErrorReporting.error("Invalid index for reading", indexExpr.position)
              Nil
          }
          register match {
            case MosRegister.A | MosRegister.X | MosRegister.Y => result ++ suffix
            case MosRegister.AX => result :+ AssemblyLine.immediate(LDX, 0)
            case MosRegister.AY => result :+ AssemblyLine.immediate(LDY, 0)
          }
        }
      case SumExpression(params, decimal) =>
        assertAllArithmetic(ctx, params.map(_._2))
        val a = params.map{case (n, p) => env.eval(p).map(n -> _)}
        if (a.forall(_.isDefined)) {
            val value = a.foldLeft(Constant.Zero){(c, pair) =>
              val Some((neg, v)) = pair
              CompoundConstant(if (decimal) {
                if (neg) MathOperator.DecimalMinus else MathOperator.DecimalPlus
              } else {
                if (neg) MathOperator.Minus else MathOperator.Plus
              }, c, v).quickSimplify
            }
          exprTypeAndVariable.map(x => compileConstant(ctx, value.quickSimplify, x._2)).getOrElse(Nil)
        } else {
          getSumSize(ctx, params) match {
            case 1 =>
              val calculate = BuiltIns.compileAddition(ctx, params, decimal = decimal)
              val store = expressionStorageFromAX(ctx, exprTypeAndVariable, expr.position)
              if (exprTypeAndVariable.exists(_._1.size >= 2)) {
                calculate ++ List(AssemblyLine.immediate(LDX, 0)) ++ store
              } else {
                calculate ++ store
              }
            case 2 =>
              val calculate = PseudoregisterBuiltIns.compileWordAdditionToAX(ctx, params, decimal = decimal)
              val store = expressionStorageFromAX(ctx, exprTypeAndVariable, expr.position)
              calculate ++ store
          }
        }
      case SeparateBytesExpression(h, l) =>
        exprTypeAndVariable.fold {
          // TODO: order?
          compile(ctx, l, None, branches) ++ compile(ctx, h, None, branches)
        } { case (exprType, target) =>
          assertCompatible(exprType, target.typ)
          target match {
            // TODO: some more complex ones may not work correctly
            case RegisterVariable(MosRegister.A | MosRegister.X | MosRegister.Y, _) => compile(ctx, l, exprTypeAndVariable, branches)
            case RegisterVariable(MosRegister.AX, _) =>
              compile(ctx, l, Some(b -> RegisterVariable(MosRegister.A, b)), branches) ++
                preserveRegisterIfNeeded(ctx, MosRegister.A, compile(ctx, h, Some(b -> RegisterVariable(MosRegister.X, b)), branches))
            case RegisterVariable(MosRegister.AY, _) =>
              compile(ctx, l, Some(b -> RegisterVariable(MosRegister.A, b)), branches) ++
                preserveRegisterIfNeeded(ctx, MosRegister.A, compile(ctx, h, Some(b -> RegisterVariable(MosRegister.Y, b)), branches))
            case RegisterVariable(MosRegister.XA, _) =>
              compile(ctx, l, Some(b -> RegisterVariable(MosRegister.X, b)), branches) ++
                compile(ctx, h, Some(b -> RegisterVariable(MosRegister.A, b)), branches)
            case RegisterVariable(MosRegister.YA, _) =>
              compile(ctx, l, Some(b -> RegisterVariable(MosRegister.Y, b)), branches) ++
                compile(ctx, h, Some(b -> RegisterVariable(MosRegister.A, b)), branches)
            case target: VariableInMemory =>
              target.typ.size match {
                case 1 =>
                  ErrorReporting.error(s"Variable `$target.name` cannot hold a word", expr.position)
                  Nil
                case 2 =>
                  compile(ctx, l, Some(b -> env.genRelativeVariable(target.toAddress, b, zeropage = target.zeropage)), branches) ++
                    compile(ctx, h, Some(b -> env.genRelativeVariable(target.toAddress + 1, b, zeropage = target.zeropage)), branches)
              }
            case target: StackVariable =>
              target.typ.size match {
                case 1 =>
                  ErrorReporting.error(s"Variable `$target.name` cannot hold a word", expr.position)
                  Nil
                case 2 =>
                  compile(ctx, l, Some(b -> StackVariable("", b, target.baseOffset + ctx.extraStackOffset)), branches) ++
                    compile(ctx, h, Some(b -> StackVariable("", b, target.baseOffset + ctx.extraStackOffset + 1)), branches)
              }
          }
        }

      case f@FunctionCallExpression(name, params) =>
        var zeroExtend = false
        val calculate: List[AssemblyLine] = name match {
          case "not" =>
            assertBool(ctx, "not", params, 1)
            compile(ctx, params.head, exprTypeAndVariable, branches.flip)
          case "hi" | "lo" =>
            zeroExtend = true
            val hi = name == "hi"
            if (params.length != 1) {
              ErrorReporting.error("Too many parameters for hi/lo", f.position)
              Nil
            } else {
              val param = params.head
              val typ = getExpressionType(ctx, param)
              if (typ.size < 1 || typ.size > 2) {
                ErrorReporting.error("Invalid parameter type for hi/lo", param.position)
                compile(ctx, param, None, BranchSpec.None)
              } else {
                val compilation = compile(ctx, param, Some(MosExpressionCompiler.getExpressionType(ctx, param) -> RegisterVariable(MosRegister.AX, w)), BranchSpec.None)
                if (hi) {
                  if (typ.size == 2) compilation :+ AssemblyLine.implied(TXA)
                  else if (typ.isSigned) compilation ++ signExtendA()
                  else List(AssemblyLine.immediate(LDA, 0))
                } else compilation
              }
            }
          case "nonet" =>
            if (params.length != 1) {
              ErrorReporting.error("Invalid number of parameters", f.position)
              Nil
            } else {
              assertAllArithmeticBytes("Nonet argument has to be a byte", ctx, params)
              params.head match {
                case SumExpression(addends, _) =>
                  if (addends.exists(a => !a._1)) {
                    ErrorReporting.warn("Nonet subtraction may not work as expected", ctx.options, expr.position)
                  }
                  if (addends.size > 2) {
                    ErrorReporting.warn("Nonet addition works correctly only for two operands", ctx.options, expr.position)
                  }
                case FunctionCallExpression("+" | "+'" | "<<" | "<<'" | "nonet", _) => // ok
                case _ =>
                  ErrorReporting.warn("Unspecified nonet operation, results might be unpredictable", ctx.options, expr.position)
              }
              val label = MosCompiler.nextLabel("no")
              compile(ctx, params.head, Some(b -> RegisterVariable(MosRegister.A, b)), BranchSpec.None) ++ List(
                AssemblyLine.immediate(LDX, 0),
                AssemblyLine.relative(BCC, label),
                AssemblyLine.implied(INX),
                AssemblyLine.label(label)
              )
            }
          case "&&" =>
            assertBool(ctx, "&&", params)
            branches match {
              case BranchIfFalse(_) =>
                params.flatMap(compile(ctx, _, exprTypeAndVariable, branches))
              case _ =>
                val skip = MosCompiler.nextLabel("an")
                params.init.flatMap(compile(ctx, _, exprTypeAndVariable, BranchIfFalse(skip))) ++
                  compile(ctx, params.last, exprTypeAndVariable, branches) ++
                  List(AssemblyLine.label(skip))
            }
          case "||" =>
            assertBool(ctx, "||", params)
            branches match {
              case BranchIfTrue(_) =>
                params.flatMap(compile(ctx, _, exprTypeAndVariable, branches))
              case _ =>
                val skip = MosCompiler.nextLabel("or")
                params.init.flatMap(compile(ctx, _, exprTypeAndVariable, BranchIfTrue(skip))) ++
                  compile(ctx, params.last, exprTypeAndVariable, branches) ++
                  List(AssemblyLine.label(skip))
            }
          case "^^" => ???
          case "&" =>
            getArithmeticParamMaxSize(ctx, params) match {
              case 1 =>
                zeroExtend = true
                BuiltIns.compileBitOps(AND, ctx, params)
              case 2 => PseudoregisterBuiltIns.compileWordBitOpsToAX(ctx, params, AND)
            }
          case "*" =>
            zeroExtend = true
            assertAllArithmeticBytes("Long multiplication not supported", ctx, params)
            BuiltIns.compileByteMultiplication(ctx, params)
          case "|" =>
            getArithmeticParamMaxSize(ctx, params) match {
              case 1 =>
                zeroExtend = true
                BuiltIns.compileBitOps(ORA, ctx, params)
              case 2 => PseudoregisterBuiltIns.compileWordBitOpsToAX(ctx, params, ORA)
            }
          case "^" =>
            getArithmeticParamMaxSize(ctx, params) match {
              case 1 =>
                zeroExtend = true
                BuiltIns.compileBitOps(EOR, ctx, params)
              case 2 => PseudoregisterBuiltIns.compileWordBitOpsToAX(ctx, params, EOR)
            }
          case ">>>>" =>
            val (l, r, 2) = assertArithmeticBinary(ctx, params)
            l match {
              case v: LhsExpression =>
                zeroExtend = true
                BuiltIns.compileNonetOps(ctx, v, r)
            }
          case "<<" =>
            val (l, r, size) = assertArithmeticBinary(ctx, params)
            size match {
              case 1 =>
                zeroExtend = true
                BuiltIns.compileShiftOps(ASL, ctx, l, r)
              case 2 =>
                PseudoregisterBuiltIns.compileWordShiftOps(left = true, ctx, l, r)
              case _ =>
                ErrorReporting.error("Long shift ops not supported", l.position)
                Nil
            }
          case ">>" =>
            val (l, r, size) = assertArithmeticBinary(ctx, params)
            size match {
              case 1 =>
                zeroExtend = true
                BuiltIns.compileShiftOps(LSR, ctx, l, r)
              case 2 =>
                PseudoregisterBuiltIns.compileWordShiftOps(left = false, ctx, l, r)
              case _ =>
                ErrorReporting.error("Long shift ops not supported", l.position)
                Nil
            }
          case "<<'" =>
            zeroExtend = true
            assertAllArithmeticBytes("Long shift ops not supported", ctx, params)
            val (l, r, 1) = assertArithmeticBinary(ctx, params)
            DecimalBuiltIns.compileByteShiftLeft(ctx, l, r, rotate = false)
          case ">>'" =>
            zeroExtend = true
            assertAllArithmeticBytes("Long shift ops not supported", ctx, params)
            val (l, r, 1) = assertArithmeticBinary(ctx, params)
            DecimalBuiltIns.compileByteShiftRight(ctx, l, r, rotate = false)
          case "<" =>
            // TODO: signed
            val (size, signed) = assertArithmeticComparison(ctx, params)
            compileTransitiveRelation(ctx, "<", params, exprTypeAndVariable, branches) { (l, r) =>
              size match {
                case 1 => BuiltIns.compileByteComparison(ctx, if (signed) ComparisonType.LessSigned else ComparisonType.LessUnsigned, l, r, branches)
                case 2 => BuiltIns.compileWordComparison(ctx, if (signed) ComparisonType.LessSigned else ComparisonType.LessUnsigned, l, r, branches)
                case _ => BuiltIns.compileLongComparison(ctx, if (signed) ComparisonType.LessSigned else ComparisonType.LessUnsigned, l, r, size, branches)
              }
            }
          case ">=" =>
            // TODO: signed
            val (size, signed) = assertArithmeticComparison(ctx, params)
            compileTransitiveRelation(ctx, ">=", params, exprTypeAndVariable, branches) { (l, r) =>
              size match {
                case 1 => BuiltIns.compileByteComparison(ctx, if (signed) ComparisonType.GreaterOrEqualSigned else ComparisonType.GreaterOrEqualUnsigned, l, r, branches)
                case 2 => BuiltIns.compileWordComparison(ctx, if (signed) ComparisonType.GreaterOrEqualSigned else ComparisonType.GreaterOrEqualUnsigned, l, r, branches)
                case _ => BuiltIns.compileLongComparison(ctx, if (signed) ComparisonType.GreaterOrEqualSigned else ComparisonType.GreaterOrEqualUnsigned, l, r, size, branches)
              }
            }
          case ">" =>
            // TODO: signed
            val (size, signed) = assertArithmeticComparison(ctx, params)
            compileTransitiveRelation(ctx, ">", params, exprTypeAndVariable, branches) { (l, r) =>
              size match {
                case 1 => BuiltIns.compileByteComparison(ctx, if (signed) ComparisonType.GreaterSigned else ComparisonType.GreaterUnsigned, l, r, branches)
                case 2 => BuiltIns.compileWordComparison(ctx, if (signed) ComparisonType.GreaterSigned else ComparisonType.GreaterUnsigned, l, r, branches)
                case _ => BuiltIns.compileLongComparison(ctx, if (signed) ComparisonType.GreaterSigned else ComparisonType.GreaterUnsigned, l, r, size, branches)
              }
            }
          case "<=" =>
            // TODO: signed
            val (size, signed) = assertArithmeticComparison(ctx, params)
            compileTransitiveRelation(ctx, "<=", params, exprTypeAndVariable, branches) { (l, r) =>
              size match {
                case 1 => BuiltIns.compileByteComparison(ctx, if (signed) ComparisonType.LessOrEqualSigned else ComparisonType.LessOrEqualUnsigned, l, r, branches)
                case 2 => BuiltIns.compileWordComparison(ctx, if (signed) ComparisonType.LessOrEqualSigned else ComparisonType.LessOrEqualUnsigned, l, r, branches)
                case _ => BuiltIns.compileLongComparison(ctx, if (signed) ComparisonType.LessOrEqualSigned else ComparisonType.LessOrEqualUnsigned, l, r, size, branches)
              }
            }
          case "==" =>
            val size = params.map(p => getExpressionType(ctx, p).size).max
            compileTransitiveRelation(ctx, "==", params, exprTypeAndVariable, branches) { (l, r) =>
              size match {
                case 1 => BuiltIns.compileByteComparison(ctx, ComparisonType.Equal, l, r, branches)
                case 2 => BuiltIns.compileWordComparison(ctx, ComparisonType.Equal, l, r, branches)
                case _ => BuiltIns.compileLongComparison(ctx, ComparisonType.Equal, l, r, size, branches)
              }
            }
          case "!=" =>
            val (l, r, size) = assertBinary(ctx, params)
            size match {
              case 1 => BuiltIns.compileByteComparison(ctx, ComparisonType.NotEqual, l, r, branches)
              case 2 => BuiltIns.compileWordComparison(ctx, ComparisonType.NotEqual, l, r, branches)
              case _ => BuiltIns.compileLongComparison(ctx, ComparisonType.NotEqual, l, r, size, branches)
            }
          case "+=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 =>
                BuiltIns.compileInPlaceByteAddition(ctx, l, r, subtract = false, decimal = false)
              case 2 =>
                l match {
                  case v: LhsExpression =>
                    BuiltIns.compileInPlaceWordOrLongAddition(ctx, v, r, subtract = false, decimal = false)
                }
              case i if i > 2 =>
                l match {
                  case v: VariableExpression =>
                    BuiltIns.compileInPlaceWordOrLongAddition(ctx, v, r, subtract = false, decimal = false)
                }
            }
          case "-=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 =>
                BuiltIns.compileInPlaceByteAddition(ctx, l, r, subtract = true, decimal = false)
              case 2 =>
                l match {
                  case v: LhsExpression =>
                    BuiltIns.compileInPlaceWordOrLongAddition(ctx, v, r, subtract = true, decimal = false)
                }
              case i if i > 2 =>
                l match {
                  case v: VariableExpression =>
                    BuiltIns.compileInPlaceWordOrLongAddition(ctx, v, r, subtract = true, decimal = false)
                }
            }
          case "+'=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 =>
                BuiltIns.compileInPlaceByteAddition(ctx, l, r, subtract = false, decimal = true)
              case 2 =>
                l match {
                  case v: LhsExpression =>
                    BuiltIns.compileInPlaceWordOrLongAddition(ctx, v, r, subtract = false, decimal = true)
                }
              case i if i > 2 =>
                l match {
                  case v: VariableExpression =>
                    BuiltIns.compileInPlaceWordOrLongAddition(ctx, v, r, subtract = false, decimal = true)
                }
            }
          case "-'=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 =>
                BuiltIns.compileInPlaceByteAddition(ctx, l, r, subtract = true, decimal = true)
              case 2 =>
                l match {
                  case v: LhsExpression =>
                    BuiltIns.compileInPlaceWordOrLongAddition(ctx, v, r, subtract = true, decimal = true)
                }
              case i if i > 2 =>
                l match {
                  case v: VariableExpression =>
                    BuiltIns.compileInPlaceWordOrLongAddition(ctx, v, r, subtract = true, decimal = true)
                }
            }
          case "<<=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 =>
                BuiltIns.compileInPlaceByteShiftOps(ASL, ctx, l, r)
              case i if i >= 2 =>
                l match {
                  case v: LhsExpression =>
                    BuiltIns.compileInPlaceWordOrLongShiftOps(ctx, v, r, aslRatherThanLsr = true)
                }
            }
          case ">>=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 =>
                BuiltIns.compileInPlaceByteShiftOps(LSR, ctx, l, r)
              case i if i >= 2 =>
                l match {
                  case v: LhsExpression =>
                    BuiltIns.compileInPlaceWordOrLongShiftOps(ctx, v, r, aslRatherThanLsr = false)
                }
            }
          case "<<'=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 =>
                DecimalBuiltIns.compileByteShiftLeft(ctx, l, r, rotate = false) ++ compileByteStorage(ctx, MosRegister.A, l)
              case i if i >= 2 =>
                l match {
                  case v: LhsExpression =>
                    DecimalBuiltIns.compileInPlaceLongShiftLeft(ctx, v, r)
                }
            }
          case ">>'=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 =>
                DecimalBuiltIns.compileByteShiftRight(ctx, l, r, rotate = false) ++ compileByteStorage(ctx, MosRegister.A, l)
              case i if i >= 2 =>
                l match {
                  case v: LhsExpression =>
                    DecimalBuiltIns.compileInPlaceLongShiftRight(ctx, v, r)
                }
            }
          case "*=" =>
            assertAllArithmeticBytes("Long multiplication not supported", ctx, params)
            val (l, r, 1) = assertArithmeticAssignmentLike(ctx, params)
            BuiltIns.compileInPlaceByteMultiplication(ctx, l, r)
          case "*'=" =>
            assertAllArithmeticBytes("Long multiplication not supported", ctx, params)
            val (l, r, 1) = assertArithmeticAssignmentLike(ctx, params)
            DecimalBuiltIns.compileInPlaceByteMultiplication(ctx, l, r)
          case "&=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 =>
                BuiltIns.compileInPlaceByteBitOp(ctx, l, r, AND)
              case i if i >= 2 =>
                l match {
                  case v: LhsExpression =>
                    BuiltIns.compileInPlaceWordOrLongBitOp(ctx, l, r, AND)
                }
            }
          case "^=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 =>
                BuiltIns.compileInPlaceByteBitOp(ctx, l, r, EOR)
              case i if i >= 2 =>
                l match {
                  case v: LhsExpression =>
                    BuiltIns.compileInPlaceWordOrLongBitOp(ctx, l, r, EOR)
                }
            }
          case "|=" =>
            val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
            size match {
              case 1 =>
                BuiltIns.compileInPlaceByteBitOp(ctx, l, r, ORA)
              case i if i >= 2 =>
                l match {
                  case v: LhsExpression =>
                    BuiltIns.compileInPlaceWordOrLongBitOp(ctx, l, r, ORA)
                }
            }
          case _ =>
            env.maybeGet[Type](f.functionName) match {
              case Some(typ) =>
                var failed = false
                if (typ.name == "pointer") {
                  ErrorReporting.error("Cannot cast into pointer")
                  failed = true
                }
                if (params.length != 1) {
                  ErrorReporting.error("Type casting should have exactly one argument")
                  failed = true
                }
                val sourceType = getExpressionType(ctx, params.head)
                if (typ.size != sourceType.size){
                  ErrorReporting.error("Cannot cast a type to a type of different size")
                  failed = true
                }
                val newExprTypeAndVariable = exprTypeAndVariable.map(i => sourceType -> i._2)
                return if (failed) Nil else compile(ctx, params.head, newExprTypeAndVariable, branches)
              case None =>
                // fallthrough to the lookup below
            }
            lookupFunction(ctx, f) match {
              case function: MacroFunction =>
                val (paramPreparation, statements) = MosMacroExpander.inlineFunction(ctx, function, params, expr.position)
                paramPreparation ++ statements.map {
                  case MosAssemblyStatement(opcode, addrMode, expression, elidable) =>
                    val param = env.evalForAsm(expression).getOrElse {
                      ErrorReporting.error("Inlining failed due to non-constant things", expression.position)
                      Constant.Zero
                    }
                    AssemblyLine(opcode, addrMode, param, elidable)
                }
              case function: EmptyFunction =>
                ??? // TODO: type conversion?
              case function: FunctionInMemory =>
                function match {
                  case nf: NormalFunction =>
                    if (nf.interrupt) {
                      ErrorReporting.error(s"Calling an interrupt function `${f.functionName}`", expr.position)
                    }
                  case _ => ()
                }
                val result = function.params match {
                  case AssemblyParamSignature(paramConvs) =>
                    val pairs = params.zip(paramConvs)
                    val secondViaMemory = pairs.flatMap {
                      case (paramExpr, AssemblyParam(typ, paramVar: VariableInMemory, AssemblyParameterPassingBehaviour.Copy)) =>
                        compile(ctx, paramExpr, Some(typ -> paramVar), NoBranching)
                      case _ => Nil
                    }
                    val thirdViaRegisters = pairs.flatMap {
                      case (paramExpr, AssemblyParam(typ, paramVar@RegisterVariable(register, _), AssemblyParameterPassingBehaviour.Copy)) =>
                        compile(ctx, paramExpr, Some(typ -> paramVar), NoBranching)

                      // TODO: fix
                      case _ => Nil
                    }
                    secondViaMemory ++ thirdViaRegisters :+ AssemblyLine.absoluteOrLongAbsolute(JSR, function, ctx.options)
                  case NormalParamSignature(List(MemoryVariable(_, typ, _))) if typ.size == 1 =>
                    compile(ctx, params.head, Some(b -> RegisterVariable(MosRegister.A, b)), BranchSpec.None) ++ List(AssemblyLine.absoluteOrLongAbsolute(JSR, function, ctx.options))
                  case NormalParamSignature(paramVars) =>
                    params.zip(paramVars).flatMap {
                      case (paramExpr, paramVar) =>
                        val callCtx = callingContext(ctx, paramVar)
                        compileAssignment(callCtx, paramExpr, VariableExpression(paramVar.name))
                    } ++ List(AssemblyLine.absoluteOrLongAbsolute(JSR, function, ctx.options))
                }
                result
            }
        }
        val store: List[AssemblyLine] = expressionStorageFromAX(ctx, exprTypeAndVariable, expr.position)
        if (zeroExtend && exprTypeAndVariable.exists(_._1.size >= 2)) {
          calculate ++ List(AssemblyLine.immediate(LDX, 0)) ++ store
        } else {
          calculate ++ store
        }
    }
  }

  private def compileTransitiveRelation(ctx: CompilationContext,
                                        operator: String,
                                        params: List[Expression],
                                        exprTypeAndVariable: Option[(Type, Variable)],
                                        branches: BranchSpec)(binary: (Expression, Expression) => List[AssemblyLine]): List[AssemblyLine] = {
    params match {
      case List(l, r) => binary(l, r)
      case List(_) | Nil =>
        ErrorReporting.fatal("")
      case _ =>
        params.tail.init.foreach { e =>
          if (ctx.env.eval(e).isEmpty) e match {
            case VariableExpression(_) =>
            case LiteralExpression(_, _) =>
            case IndexedExpression(_, VariableExpression(_)) =>
            case IndexedExpression(_, LiteralExpression(_, _)) =>
            case IndexedExpression(_, SumExpression(List(
            (_, LiteralExpression(_, _)),
            (false, VariableExpression(_))
            ), false)) =>
            case IndexedExpression(_, SumExpression(List(
            (false, VariableExpression(_)),
            (_, LiteralExpression(_, _))
            ), false)) =>
            case _ =>
              ErrorReporting.warn("A complex expression may be evaluated multiple times", ctx.options, e.position)
          }
        }
        val conjunction = params.init.zip(params.tail).map {
          case (l, r) => FunctionCallExpression(operator, List(l, r))
        }.reduceLeft((a, b) => FunctionCallExpression("&&", List(a, b)))
        compile(ctx, conjunction, exprTypeAndVariable, branches)
    }
  }

  def expressionStorageFromAX(ctx: CompilationContext, exprTypeAndVariable: Option[(Type, Variable)], position: Option[Position]): List[AssemblyLine] = {
    exprTypeAndVariable.fold(noop) {
      case (VoidType, _) => ErrorReporting.fatal("Cannot assign word to void", position)
      case (_, RegisterVariable(MosRegister.A, _)) => noop
      case (_, RegisterVariable(MosRegister.AW, _)) => List(AssemblyLine.implied(XBA), AssemblyLine.implied(TXA), AssemblyLine.implied(XBA))
      case (_, RegisterVariable(MosRegister.X, _)) => List(AssemblyLine.implied(TAX))
      case (_, RegisterVariable(MosRegister.Y, _)) => List(AssemblyLine.implied(TAY))
      case (_, RegisterVariable(MosRegister.AX, _)) =>
        // TODO: sign extension
        noop
      case (_, RegisterVariable(MosRegister.XA, _)) =>
        // TODO: sign extension
        if (ctx.options.flag(CompilationFlag.EmitHudsonOpcodes)) {
          List(AssemblyLine.implied(HuSAX))
        } else if (ctx.options.flag(CompilationFlag.EmitCmosOpcodes)) {
          List(
            AssemblyLine.implied(PHA),
            AssemblyLine.implied(PHX),
            AssemblyLine.implied(PLA),
            AssemblyLine.implied(PLX))
        } else {
          List(
            AssemblyLine.implied(TAY),
            AssemblyLine.implied(TXA),
            AssemblyLine.implied(PHA),
            AssemblyLine.implied(TYA),
            AssemblyLine.implied(TAX),
            AssemblyLine.implied(PLA)) // fuck this shit
        }
      case (_, RegisterVariable(MosRegister.YA, _)) =>
        // TODO: sign extension
        List(
          AssemblyLine.implied(TAY),
          AssemblyLine.implied(TXA))
      case (_, RegisterVariable(MosRegister.AY, _)) =>
        // TODO: sign extension
        if (ctx.options.flag(CompilationFlag.EmitHudsonOpcodes)) {
          List(AssemblyLine.implied(SXY))
        } else if (ctx.options.flag(CompilationFlag.EmitEmulation65816Opcodes)) {
          List(AssemblyLine.implied(TXY))
        } else {
          List(
            AssemblyLine.implied(PHA),
            AssemblyLine.implied(TXA),
            AssemblyLine.implied(TAY),
            AssemblyLine.implied(PLA))
        }
      case (t, v: VariableInMemory) => t.size match {
        case 1 => v.typ.size match {
          case 1 =>
            AssemblyLine.variable(ctx, STA, v)
          case s if s > 1 =>
            if (t.isSigned) {
              AssemblyLine.variable(ctx, STA, v) ++ signExtendA() ++ List.tabulate(s - 1)(i => AssemblyLine.variable(ctx, STA, v, i + 1)).flatten
            } else {
              AssemblyLine.variable(ctx, STA, v) ++ List(AssemblyLine.immediate(LDA, 0)) ++
                List.tabulate(s - 1)(i => AssemblyLine.variable(ctx, STA, v, i + 1)).flatten
            }
        }
        case 2 => v.typ.size match {
          case 1 =>
            ErrorReporting.error(s"Variable `${v.name}` cannot hold a word", position)
            Nil
          case 2 =>
            AssemblyLine.variable(ctx, STA, v) ++ AssemblyLine.variable(ctx, STX, v, 1)
          case s if s > 2 =>
            if (t.isSigned) {
              AssemblyLine.variable(ctx, STA, v) ++
                AssemblyLine.variable(ctx, STX, v, 1) ++
                List(AssemblyLine.implied(TXA)) ++
                signExtendA() ++
                List.tabulate(s - 2)(i => AssemblyLine.variable(ctx, STA, v, i + 2)).flatten
            } else {
              AssemblyLine.variable(ctx, STA, v) ++ AssemblyLine.variable(ctx, STX, v, 1) ++ List(
                AssemblyLine.immediate(LDA, 0)) ++
                List.tabulate(s - 2)(i => AssemblyLine.variable(ctx, STA, v, i + 2)).flatten
            }
        }
      }
      case (t, v: StackVariable) => t.size match {
        case 1 => v.typ.size match {
          case 1 =>
            List(AssemblyLine.implied(TSX), AssemblyLine.absoluteX(STA, v.baseOffset + ctx.extraStackOffset))
          case s if s > 1 =>
            if (t.isSigned) {
              List(
                AssemblyLine.implied(TSX),
                AssemblyLine.absoluteX(STA, v.baseOffset + ctx.extraStackOffset)) ++
                signExtendA() ++
                List.tabulate(s - 1)(i => AssemblyLine.absoluteX(STA, v.baseOffset + ctx.extraStackOffset + i + 1))
            } else {
              List(
                AssemblyLine.implied(TSX),
                AssemblyLine.absoluteX(STA, v.baseOffset + ctx.extraStackOffset),
                AssemblyLine.immediate(LDA, 0)) ++
                List.tabulate(s - 1)(i => AssemblyLine.absoluteX(STA, v.baseOffset + ctx.extraStackOffset + i + 1))
            }
        }
        case 2 => v.typ.size match {
          case 1 =>
            ErrorReporting.error(s"Variable `${v.name}` cannot hold a word", position)
            Nil
          case 2 =>
            List(
              AssemblyLine.implied(TAY),
              AssemblyLine.implied(TXA),
              AssemblyLine.implied(TSX),
              AssemblyLine.absoluteX(STA, v.baseOffset + ctx.extraStackOffset + 1),
              AssemblyLine.implied(TYA),
              AssemblyLine.absoluteX(STA, v.baseOffset + ctx.extraStackOffset))
          case s if s > 2 => ???
        }
      }
    }
  }

  def compileAssignment(ctx: CompilationContext, source: Expression, target: LhsExpression): List[AssemblyLine] = {
    val env = ctx.env
    val sourceType = AbstractExpressionCompiler.checkAssignmentTypeAndGetSourceType(ctx, source, target)
    val b = env.get[Type]("byte")
    val w = env.get[Type]("word")
    target match {
      case VariableExpression(name) =>
        val v = env.get[Variable](name, target.position)
        // TODO check v.typ
        compile(ctx, source, Some((sourceType, v)), NoBranching)
      case SeparateBytesExpression(h: LhsExpression, l: LhsExpression) =>
        compile(ctx, source, Some(w, RegisterVariable(MosRegister.AX, w)), NoBranching) ++
          compileByteStorage(ctx, MosRegister.A, l) ++ compileByteStorage(ctx, MosRegister.X, h)
      case SeparateBytesExpression(_, _) =>
        ErrorReporting.error("Invalid left-hand-side use of `:`")
        Nil
      case _ =>
        compile(ctx, source, Some(b, RegisterVariable(MosRegister.A, b)), NoBranching) ++ compileByteStorage(ctx, MosRegister.A, target)
    }
  }

  def arrayBoundsCheck(ctx: CompilationContext, pointy: Pointy, register: MosRegister.Value, index: Expression): List[AssemblyLine] = {
    if (!ctx.options.flags(CompilationFlag.CheckIndexOutOfBounds)) return Nil
    val arrayLength:Int = pointy match {
      case _: VariablePointy => return Nil
      case p: ConstantPointy => p.size match {
        case None => return Nil
        case Some(s) => s
      }
    }
    ctx.env.eval(index) match {
      case Some(NumericConstant(i, _)) =>
        if (i >= 0) {
          if (i < arrayLength) return Nil
          if (i >= arrayLength) return List(
            AssemblyLine.implied(PHP),
            AssemblyLine.absoluteOrLongAbsolute(JSR, ctx.env.get[ThingInMemory]("_panic"), ctx.options))
        }
      case _ =>
    }
    if (arrayLength > 0 && arrayLength < 255) {
      val label = MosCompiler.nextLabel("bc")
      val compare = register match {
        case MosRegister.A => CMP
        case MosRegister.X => CPX
        case MosRegister.Y => CPY
      }
      List(
        AssemblyLine.implied(PHP),
        AssemblyLine.immediate(compare, arrayLength),
        AssemblyLine.relative(BCC, label),
        AssemblyLine.absoluteOrLongAbsolute(JSR, ctx.env.get[ThingInMemory]("_panic"), ctx.options),
        AssemblyLine.label(label),
        AssemblyLine.implied(PLP))
    } else {
      Nil
    }
  }

  private def signExtendA(): List[AssemblyLine] = {
    val label = MosCompiler.nextLabel("sx")
    List(
      AssemblyLine.immediate(ORA, 0x7F),
      AssemblyLine.relative(BMI, label),
      AssemblyLine.immediate(LDA, 0),
      AssemblyLine.label(label))
  }

  private def shouldCopyFromHiToLo(srcAddress: Constant, destAddress: Constant): Boolean = (srcAddress, destAddress) match {
    case (
      CompoundConstant(MathOperator.Plus, a: MemoryAddressConstant, NumericConstant(s, _)),
      CompoundConstant(MathOperator.Plus, b: MemoryAddressConstant, NumericConstant(d, _))
      ) if a == b => s < d
    case (
     a: MemoryAddressConstant,
      CompoundConstant(MathOperator.Plus, b: MemoryAddressConstant, NumericConstant(d, _))
      ) if a == b => 0 < d
    case (NumericConstant(s, _), NumericConstant(d, _)) => s < d
    case _ => false
  }
}
