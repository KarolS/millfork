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
object ExpressionCompiler {

  def getExpressionType(ctx: CompilationContext, expr: Expression): Type = {
    val env = ctx.env
    val b = env.get[Type]("byte")
    val bool = env.get[Type]("bool$")
    val v = env.get[Type]("void")
    val w = env.get[Type]("word")
    val l = env.get[Type]("long")
    expr match {
      case LiteralExpression(value, size) =>
        size match {
          case 1 => b
          case 2 => w
          case 3 | 4 => l
        }
      case VariableExpression(name) =>
        env.get[TypedThing](name, expr.position).typ
      case HalfWordExpression(param, _) =>
        getExpressionType(ctx, param)
        b
      case IndexedExpression(_, _) => b
      case SeparateBytesExpression(hi, lo) =>
        if (getExpressionType(ctx, hi).size > 1) ErrorReporting.error("Hi byte too large", hi.position)
        if (getExpressionType(ctx, lo).size > 1) ErrorReporting.error("Lo byte too large", lo.position)
        w
      case SumExpression(params, _) => params.map { case (_, e) => getExpressionType(ctx, e).size }.max match {
        case 1 => b
        case 2 => w
        case _ => ErrorReporting.error("Adding values bigger than words", expr.position); w
      }
      case FunctionCallExpression("nonet", params) => w
      case FunctionCallExpression("not", params) => bool
      case FunctionCallExpression("hi", params) => b
      case FunctionCallExpression("lo", params) => b
      case FunctionCallExpression("*", params) => b
      case FunctionCallExpression("|" | "&" | "^", params) => params.map { e => getExpressionType(ctx, e).size }.max match {
        case 1 => b
        case 2 => w
        case _ => ErrorReporting.error("Adding values bigger than words", expr.position); w
      }
      case FunctionCallExpression("<<", List(a1, a2)) =>
        if (getExpressionType(ctx, a2).size > 1) ErrorReporting.error("Shift amount too large", a2.position)
        getExpressionType(ctx, a1)
      case FunctionCallExpression(">>", List(a1, a2)) =>
        if (getExpressionType(ctx, a2).size > 1) ErrorReporting.error("Shift amount too large", a2.position)
        getExpressionType(ctx, a1)
      case FunctionCallExpression("<<'", params) => b
      case FunctionCallExpression(">>'", params) => b
      case FunctionCallExpression(">>>>", params) => b
      case FunctionCallExpression("&&", params) => bool
      case FunctionCallExpression("||", params) => bool
      case FunctionCallExpression("^^", params) => bool
      case FunctionCallExpression("==", params) => bool
      case FunctionCallExpression("!=", params) => bool
      case FunctionCallExpression("<", params) => bool
      case FunctionCallExpression(">", params) => bool
      case FunctionCallExpression("<=", params) => bool
      case FunctionCallExpression(">=", params) => bool
      case FunctionCallExpression("+=", params) => v
      case FunctionCallExpression("-=", params) => v
      case FunctionCallExpression("*=", params) => v
      case FunctionCallExpression("+'=", params) => v
      case FunctionCallExpression("-'=", params) => v
      case FunctionCallExpression("*'=", params) => v
      case FunctionCallExpression("|=", params) => v
      case FunctionCallExpression("&=", params) => v
      case FunctionCallExpression("^=", params) => v
      case FunctionCallExpression("<<=", params) => v
      case FunctionCallExpression(">>=", params) => v
      case FunctionCallExpression("<<'=", params) => v
      case FunctionCallExpression(">>'=", params) => v
      case f@FunctionCallExpression(name, params) =>
        ctx.env.maybeGet[Type](name) match {
          case Some(typ) =>
            typ
          case None =>
            lookupFunction(ctx, f).returnType
        }
    }
  }

  def compileConstant(ctx: CompilationContext, expr: Constant, target: Variable): List[AssemblyLine] = {
    target match {
      case RegisterVariable(Register.A, _) => List(AssemblyLine(LDA, Immediate, expr))
      case RegisterVariable(Register.AW, _) =>
        List(
          AssemblyLine.accu16,
          AssemblyLine(LDA_W, WordImmediate, expr),
          AssemblyLine.accu8)
      case RegisterVariable(Register.X, _) => List(AssemblyLine(LDX, Immediate, expr))
      case RegisterVariable(Register.Y, _) => List(AssemblyLine(LDY, Immediate, expr))
      case RegisterVariable(Register.AX, _) => List(
        AssemblyLine(LDA, Immediate, expr.loByte),
        AssemblyLine(LDX, Immediate, expr.hiByte))
      case RegisterVariable(Register.AY, _) => List(
        AssemblyLine(LDA, Immediate, expr.loByte),
        AssemblyLine(LDY, Immediate, expr.hiByte))
      case RegisterVariable(Register.XA, _) => List(
        AssemblyLine(LDA, Immediate, expr.hiByte),
        AssemblyLine(LDX, Immediate, expr.loByte))
      case RegisterVariable(Register.YA, _) => List(
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

  def preserveRegisterIfNeeded(ctx: CompilationContext, register: Register.Value, code: List[AssemblyLine]): List[AssemblyLine] = {
    val state = register match {
      case Register.A => State.A
      case Register.X => State.X
      case Register.Y => State.Y
    }

    val cmos = ctx.options.flag(CompilationFlag.EmitCmosOpcodes)
    if (AssemblyLine.treatment(code, state) != Treatment.Unchanged) {
      register match {
        case Register.A => AssemblyLine.implied(PHA) +: fixTsx(code) :+ AssemblyLine.implied(PLA)
        case Register.X => if (cmos) {
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
        case Register.Y => if (cmos) {
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
    if (!ctx.options.flag(CompilationFlag.ZeropagePseudoregister)) {
      ErrorReporting.error("16-bit indexing requires a zeropage pseudoregister")
      compile(ctx, indexExpression, Some(w -> RegisterVariable(Register.YA, w)), BranchSpec.None)
      return Nil
    }
    val reg = ctx.env.get[VariableInMemory]("__reg")
    val compileIndex = compile(ctx, indexExpression, Some(w -> RegisterVariable(Register.YA, w)), BranchSpec.None)
    val prepareRegister = pointy match {
      case ConstantPointy(addr, _) =>
        List(
          AssemblyLine.implied(CLC),
          AssemblyLine.immediate(ADC, addr.hiByte),
          AssemblyLine.zeropage(STA, reg, 1),
          AssemblyLine.immediate(LDA, addr.loByte),
          AssemblyLine.zeropage(STA, reg))
      case VariablePointy(addr) =>
        List(
          AssemblyLine.implied(CLC),
          AssemblyLine.zeropage(ADC, addr + 1),
          AssemblyLine.zeropage(STA, reg, 1),
          AssemblyLine.zeropage(LDA, addr),
          AssemblyLine.zeropage(STA, reg))
    }
    compileIndex ++ prepareRegister
  }

  def compileByteStorage(ctx: CompilationContext, register: Register.Value, target: LhsExpression): List[AssemblyLine] = {
    val env = ctx.env
    val b = env.get[Type]("byte")
    val store = register match {
      case Register.A => STA
      case Register.X => STX
      case Register.Y => STY
    }
    val transferToA = register match {
      case Register.A => NOP
      case Register.X => TXA
      case Register.Y => TYA
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
          val indexRegister = if (register == Register.Y) Register.X else Register.Y
          val calculatingIndex = preserveRegisterIfNeeded(ctx, register, compile(ctx, variableIndex, Some(b, RegisterVariable(indexRegister, b)), NoBranching))
          if (register == Register.A) {
            indexRegister match {
              case Register.Y =>
                calculatingIndex ++ arrayBoundsCheck(ctx, pointy, Register.Y, indexExpr) ++ List(AssemblyLine.absoluteY(STA, arrayAddr + constIndex))
              case Register.X =>
                calculatingIndex ++ arrayBoundsCheck(ctx, pointy, Register.X, indexExpr) ++ List(AssemblyLine.absoluteX(STA, arrayAddr + constIndex))
            }
          } else {
            indexRegister match {
              case Register.Y =>
                calculatingIndex ++ arrayBoundsCheck(ctx, pointy, Register.Y, indexExpr) ++ List(AssemblyLine.implied(transferToA), AssemblyLine.absoluteY(STA, arrayAddr + constIndex))
              case Register.X =>
                calculatingIndex ++ arrayBoundsCheck(ctx, pointy, Register.X, indexExpr) ++ List(AssemblyLine.implied(transferToA), AssemblyLine.absoluteX(STA, arrayAddr + constIndex))
            }
          }
        }
        def wrapWordIndexingStorage(code: List[AssemblyLine]) = {
          val reg = ctx.env.get[VariableInMemory]("__reg")
          val cmos = ctx.options.flag(CompilationFlag.EmitCmosOpcodes)
          register match {
            case Register.A =>
              List(AssemblyLine.implied(PHA)) ++ code ++ List(AssemblyLine.implied(PLA), AssemblyLine.indexedY(STA, reg))
            case Register.X =>
              if (code.exists(l => OpcodeClasses.ChangesX(l.opcode))) {
                if (cmos)
                  List(AssemblyLine.implied(PHX)) ++ code ++ List(AssemblyLine.implied(PLA), AssemblyLine.indexedY(STA, reg))
                else
                  List(AssemblyLine.implied(TXA), AssemblyLine.implied(PHA)) ++ code ++ List(AssemblyLine.implied(PLA), AssemblyLine.indexedY(STA, reg))
              } else {
                code ++ List(AssemblyLine.implied(TXA), AssemblyLine.indexedY(STA, reg))
              }
            case Register.Y =>
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
            wrapWordIndexingStorage(prepareWordIndexing(ctx, ConstantPointy(p.value + constIndex, if (constIndex.isProvablyZero) p.size else None), v))
          case (p: ConstantPointy, Some(v), 1, _) =>
            storeToArrayAtUnknownIndex(v, p.value)
          //TODO: should there be a type check or a zeropage check?
          case (pointerVariable:VariablePointy, None, _, 0 | 1) =>
            register match {
              case Register.A =>
                List(AssemblyLine.immediate(LDY, constIndex), AssemblyLine.indexedY(STA, pointerVariable.addr))
              case Register.Y =>
                List(AssemblyLine.implied(TYA), AssemblyLine.immediate(LDY, constIndex), AssemblyLine.indexedY(STA, pointerVariable.addr), AssemblyLine.implied(TAY))
              case Register.X =>
                List(AssemblyLine.immediate(LDY, constIndex), AssemblyLine.implied(TXA), AssemblyLine.indexedY(STA, pointerVariable.addr))
              case _ =>
                ErrorReporting.error("Cannot store a word in an array", target.position)
                Nil
            }
          case (pointerVariable:VariablePointy, Some(_), _, 0 | 1) =>
            val calculatingIndex = compile(ctx, indexExpr, Some(b, RegisterVariable(Register.Y, b)), NoBranching)
            register match {
              case Register.A =>
                preserveRegisterIfNeeded(ctx, Register.A, calculatingIndex) :+ AssemblyLine.indexedY(STA, pointerVariable.addr)
              case Register.X =>
                preserveRegisterIfNeeded(ctx, Register.X, calculatingIndex) ++ List(AssemblyLine.implied(TXA), AssemblyLine.indexedY(STA, pointerVariable.addr))
              case Register.Y =>
                AssemblyLine.implied(TYA) :: preserveRegisterIfNeeded(ctx, Register.A, calculatingIndex) ++ List(
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

  def assertCompatible(exprType: Type, variableType: Type): Unit = {
    // TODO
  }

  val noop: List[AssemblyLine] = Nil

  def callingContext(ctx: CompilationContext, v: MemoryVariable): CompilationContext = {
    val result = new Environment(Some(ctx.env), "")
    result.registerVariable(VariableDeclarationStatement(v.name, v.typ.name, stack = false, global = false, constant = false, volatile = false, register = false, initialValue = None, address = None, bank = v.declaredBank), ctx.options)
    ctx.copy(env = result)
  }

  def assertBinary(ctx: CompilationContext, params: List[Expression]): (Expression, Expression, Int) = {
    if (params.length != 2) {
      ErrorReporting.fatal("sfgdgfsd", None)
    }
    (params.head, params(1)) match {
      case (l: Expression, r: Expression) => (l, r, getExpressionType(ctx, l).size max getExpressionType(ctx, r).size)
    }
  }

  def assertComparison(ctx: CompilationContext, params: List[Expression]): (Int, Boolean) = {
    (params.head, params(1)) match {
      case (l: Expression, r: Expression) =>
        val lt = getExpressionType(ctx, l)
        val rt = getExpressionType(ctx, r)
        (lt.size max rt.size, lt.isSigned || rt.isSigned)
    }
  }

  def assertBool(ctx: CompilationContext, params: List[Expression], expectedParamCount: Int): Unit = {
    if (params.length != expectedParamCount) {
      ErrorReporting.error("Invalid number of parameters", params.headOption.flatMap(_.position))
      return
    }
    params.foreach { param =>
      if (!getExpressionType(ctx, param).isInstanceOf[BooleanType])
        ErrorReporting.fatal("Parameter should be boolean", param.position)
    }
  }

  def assertAssignmentLike(ctx: CompilationContext, params: List[Expression]): (LhsExpression, Expression, Int) = {
    if (params.length != 2) {
      ErrorReporting.fatal("sfgdgfsd", None)
    }
    (params.head, params(1)) match {
      case (l: LhsExpression, r: Expression) =>
        val lsize = getExpressionType(ctx, l).size
        val rsize = getExpressionType(ctx, r).size
        if (lsize < rsize) {
          ErrorReporting.error("Left-hand-side expression is of smaller type than the right-hand-side expression", l.position)
        }
        (l, r, lsize)
      case (err: Expression, _) => ErrorReporting.fatal("Invalid left-hand-side expression", err.position)
    }
  }

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
                  case RegisterVariable(Register.A, _) => AssemblyLine.variable(ctx, LDA, source)
                  case RegisterVariable(Register.AW, _) =>
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
                  case RegisterVariable(Register.X, _) => AssemblyLine.variable(ctx, LDX, source)
                  case RegisterVariable(Register.Y, _) => AssemblyLine.variable(ctx, LDY, source)
                  case RegisterVariable(Register.AX, _) =>
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
                  case RegisterVariable(Register.AY, _) =>
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
                  case RegisterVariable(Register.XA, _) =>
                    exprType.size match {
                      case 1 => if (exprType.isSigned) {
                        AssemblyLine.variable(ctx, LDX, source) ++ List(AssemblyLine.implied(TXA)) ++ signExtendA()
                      } else
                        AssemblyLine.variable(ctx, LDX, source) :+ AssemblyLine.immediate(LDA, 0)
                      case 2 =>
                        AssemblyLine.variable(ctx, LDX, source) ++ AssemblyLine.variable(ctx,LDA, source, 1)
                    }
                  case RegisterVariable(Register.YA, _) =>
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
                      val copy = List.tabulate(exprType.size)(i => AssemblyLine.variable(ctx, LDA, source, i) ++ AssemblyLine.variable(ctx, STA, target, i))
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
                  case RegisterVariable(Register.A, _) => List(AssemblyLine.implied(TSX), AssemblyLine.absoluteX(LDA, offset + ctx.extraStackOffset))
                  case RegisterVariable(Register.X, _) => List(AssemblyLine.implied(TSX), AssemblyLine.absoluteX(LDA, offset + ctx.extraStackOffset), AssemblyLine.implied(TAX))
                  case RegisterVariable(Register.Y, _) => List(AssemblyLine.implied(TSX), AssemblyLine.absoluteX(LDY, offset + ctx.extraStackOffset))
                  case RegisterVariable(Register.AX, _) =>
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
                  case RegisterVariable(Register.AY, _) =>
                    exprType.size match {
                      case 1 => if (exprType.isSigned) {
                        val label = MfCompiler.nextLabel("sx")
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
                  case RegisterVariable(Register.XA, _) =>
                    ??? // TODO
                  case RegisterVariable(Register.YA, _) =>
                    exprType.size match {
                      case 1 => if (exprType.isSigned) {
                        val label = MfCompiler.nextLabel("sx")
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
                      val copy = List.tabulate(exprType.size)(i => List(AssemblyLine.absoluteX(LDA, offset + ctx.extraStackOffset + i), AssemblyLine.absoluteX(STA, target.baseOffset + i)))
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
        // TODO: check
        val (variableIndex, constantIndex) = env.evalVariableAndConstantSubParts(indexExpr)
        val variableIndexSize = variableIndex.map(v => getExpressionType(ctx, v).size).getOrElse(0)
        val totalIndexSize = getExpressionType(ctx, indexExpr).size
        exprTypeAndVariable.fold(compile(ctx, indexExpr, None, BranchSpec.None)) { case (exprType, target) =>

          val register = target match {
            case RegisterVariable(r, _) => r
            case _ => Register.A
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
            case Register.A | Register.AX | Register.AY => LDA
            case Register.X => LDX
            case Register.Y => LDY
          }

          def loadFromArrayAtUnknownIndex(variableIndex: Expression, arrayAddr: Constant) = {
            // TODO check typ
            val indexRegister = if (register == Register.Y) Register.X else Register.Y
            val calculatingIndex = compile(ctx, variableIndex, Some(b, RegisterVariable(indexRegister, b)), NoBranching)
            indexRegister match {
              case Register.Y =>
                calculatingIndex ++ arrayBoundsCheck(ctx, pointy, Register.Y, indexExpr) ++ List(AssemblyLine.absoluteY(load, arrayAddr + constantIndex))
              case Register.X =>
                calculatingIndex ++ arrayBoundsCheck(ctx, pointy, Register.X, indexExpr) ++ List(AssemblyLine.absoluteX(load, arrayAddr + constantIndex))
            }
          }

          def loadFromReg() = {
            val reg = ctx.env.get[VariableInMemory]("__reg")
            register match {
              case Register.A =>
                List(AssemblyLine.indexedY(LDA, reg))
              case Register.X =>
                List(AssemblyLine.indexedY(LDX, reg))
              case Register.Y =>
                List(AssemblyLine.indexedY(LDA, reg), AssemblyLine.implied(TAY))
            }
          }
          val result = (pointy, variableIndex, totalIndexSize, variableIndexSize) match {
            case (a: ConstantPointy, None, _, _) =>
              List(AssemblyLine.absolute(load, env.genRelativeVariable(a.value + constantIndex, b, zeropage = false)))
            case (a: ConstantPointy, Some(v), _, 1) =>
              loadFromArrayAtUnknownIndex(v, a.value)
            case (a: ConstantPointy, Some(v), _, 2) =>
              prepareWordIndexing(ctx, ConstantPointy(a.value + constantIndex, if (constantIndex.isProvablyZero) a.size else None), v) ++ loadFromReg()
            case (a: VariablePointy, _, 2, _) =>
              prepareWordIndexing(ctx, a, indexExpr) ++ loadFromReg()
            case (p:VariablePointy, None, 0 | 1, _) =>
              register match {
                case Register.A =>
                  List(AssemblyLine.immediate(LDY, constantIndex), AssemblyLine.indexedY(LDA, p.addr))
                case Register.Y =>
                  List(AssemblyLine.immediate(LDY, constantIndex), AssemblyLine.indexedY(LDA, p.addr), AssemblyLine.implied(TAY))
                case Register.X =>
                  List(AssemblyLine.immediate(LDY, constantIndex), AssemblyLine.indexedY(LDX, p.addr))
              }
            case (p:VariablePointy, Some(_), 0 | 1, _) =>
              val calculatingIndex = compile(ctx, indexExpr, Some(b, RegisterVariable(Register.Y, b)), NoBranching)
              register match {
                case Register.A =>
                  calculatingIndex :+ AssemblyLine.indexedY(LDA, p.addr)
                case Register.X =>
                  calculatingIndex :+ AssemblyLine.indexedY(LDX, p.addr)
                case Register.Y =>
                  calculatingIndex ++ List(AssemblyLine.indexedY(LDA, p.addr), AssemblyLine.implied(TAY))
              }
            case _ =>
              ErrorReporting.error("Invalid index for reading", indexExpr.position)
              Nil
          }
          register match {
            case Register.A | Register.X | Register.Y => result ++ suffix
            case Register.AX => result :+ AssemblyLine.immediate(LDX, 0)
            case Register.AY => result :+ AssemblyLine.immediate(LDY, 0)
          }
        }
      case SumExpression(params, decimal) =>
        val a = params.map{case (n, p) => env.eval(p).map(n -> _)}
        if (a.forall(_.isDefined)) {
            val value = a.foldLeft(Constant.Zero){(c, pair) =>
              val Some((neg, v)) = pair
              CompoundConstant(if (decimal) {
                if (neg) MathOperator.DecimalMinus else MathOperator.DecimalPlus
              } else {
                if (neg) MathOperator.Minus else MathOperator.Plus
              }, c, v)
            }
          exprTypeAndVariable.map(x => compileConstant(ctx, value.quickSimplify, x._2)).getOrElse(Nil)
        } else {
          getSumSize(ctx, params) match {
            case 1 =>
              val calculate = BuiltIns.compileAddition(ctx, params, decimal = decimal)
              val store = expressionStorageFromAX(ctx, exprTypeAndVariable, expr.position)
              calculate ++ store
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
            case RegisterVariable(Register.A | Register.X | Register.Y, _) => compile(ctx, l, exprTypeAndVariable, branches)
            case RegisterVariable(Register.AX, _) =>
              compile(ctx, l, Some(b -> RegisterVariable(Register.A, b)), branches) ++
                preserveRegisterIfNeeded(ctx, Register.A, compile(ctx, h, Some(b -> RegisterVariable(Register.X, b)), branches))
            case RegisterVariable(Register.AY, _) =>
              compile(ctx, l, Some(b -> RegisterVariable(Register.A, b)), branches) ++
                preserveRegisterIfNeeded(ctx, Register.A, compile(ctx, h, Some(b -> RegisterVariable(Register.Y, b)), branches))
            case RegisterVariable(Register.XA, _) =>
              compile(ctx, l, Some(b -> RegisterVariable(Register.X, b)), branches) ++
                compile(ctx, h, Some(b -> RegisterVariable(Register.A, b)), branches)
            case RegisterVariable(Register.YA, _) =>
              compile(ctx, l, Some(b -> RegisterVariable(Register.Y, b)), branches) ++
                compile(ctx, h, Some(b -> RegisterVariable(Register.A, b)), branches)
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
        val calculate = name match {
          case "not" =>
            assertBool(ctx, params, 1)
            compile(ctx, params.head, exprTypeAndVariable, branches.flip)
          case "hi" | "lo" =>
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
                val compilation = compile(ctx, param, Some(w -> RegisterVariable(Register.AX, w)), BranchSpec.None)
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
              assertAllBytes("Nonet argument has to be a byte", ctx, params)
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
              val label = MfCompiler.nextLabel("no")
              compile(ctx, params.head, Some(b -> RegisterVariable(Register.A, b)), BranchSpec.None) ++ List(
                AssemblyLine.immediate(LDX, 0),
                AssemblyLine.relative(BCC, label),
                AssemblyLine.implied(INX),
                AssemblyLine.label(label)
              )
            }
          case "&&" =>
            assertBool(ctx, params, 2)
            val a = params.head
            val b = params(1)
            branches match {
              case BranchIfFalse(_) =>
                compile(ctx, a, exprTypeAndVariable, branches) ++ compile(ctx, b, exprTypeAndVariable, branches)
              case _ =>
                val skip = MfCompiler.nextLabel("an")
                compile(ctx, a, exprTypeAndVariable, BranchIfFalse(skip)) ++
                  compile(ctx, b, exprTypeAndVariable, branches) ++
                  List(AssemblyLine.label(skip))
            }
          case "||" =>
            assertBool(ctx, params, 2)
            val a = params.head
            val b = params(1)
            branches match {
              case BranchIfTrue(_) =>
                compile(ctx, a, exprTypeAndVariable, branches) ++ compile(ctx, b, exprTypeAndVariable, branches)
              case _ =>
                val skip = MfCompiler.nextLabel("or")
                compile(ctx, a, exprTypeAndVariable, BranchIfTrue(skip)) ++
                  compile(ctx, b, exprTypeAndVariable, branches) ++
                  List(AssemblyLine.label(skip))
            }
          case "^^" => ???
          case "&" =>
            getParamMaxSize(ctx, params) match {
              case 1 => BuiltIns.compileBitOps(AND, ctx, params)
              case 2 => PseudoregisterBuiltIns.compileWordBitOpsToAX(ctx, params, AND)
            }
          case "*" =>
            assertAllBytes("Long multiplication not supported", ctx, params)
            BuiltIns.compileByteMultiplication(ctx, params)
          case "|" =>
            getParamMaxSize(ctx, params) match {
              case 1 => BuiltIns.compileBitOps(ORA, ctx, params)
              case 2 => PseudoregisterBuiltIns.compileWordBitOpsToAX(ctx, params, ORA)
            }
          case "^" =>
            getParamMaxSize(ctx, params) match {
              case 1 => BuiltIns.compileBitOps(EOR, ctx, params)
              case 2 => PseudoregisterBuiltIns.compileWordBitOpsToAX(ctx, params, EOR)
            }
          case ">>>>" =>
            val (l, r, 2) = assertBinary(ctx, params)
            l match {
              case v: LhsExpression =>
                BuiltIns.compileNonetOps(ctx, v, r)
            }
          case "<<" =>
            val (l, r, size) = assertBinary(ctx, params)
            size match {
              case 1 =>
                BuiltIns.compileShiftOps(ASL, ctx, l, r)
              case 2 =>
                PseudoregisterBuiltIns.compileWordShiftOps(left = true, ctx, l, r)
              case _ =>
                ErrorReporting.error("Long shift ops not supported", l.position)
                Nil
            }
          case ">>" =>
            val (l, r, size) = assertBinary(ctx, params)
            size match {
              case 1 =>
                BuiltIns.compileShiftOps(LSR, ctx, l, r)
              case 2 =>
                PseudoregisterBuiltIns.compileWordShiftOps(left = false, ctx, l, r)
              case _ =>
                ErrorReporting.error("Long shift ops not supported", l.position)
                Nil
            }
          case "<<'" =>
            assertAllBytes("Long shift ops not supported", ctx, params)
            val (l, r, 1) = assertBinary(ctx, params)
            DecimalBuiltIns.compileByteShiftLeft(ctx, l, r, rotate = false)
          case ">>'" =>
            assertAllBytes("Long shift ops not supported", ctx, params)
            val (l, r, 1) = assertBinary(ctx, params)
            DecimalBuiltIns.compileByteShiftRight(ctx, l, r, rotate = false)
          case "<" =>
            // TODO: signed
            val (size, signed) = assertComparison(ctx, params)
            compileTransitiveRelation(ctx, "<", params, exprTypeAndVariable, branches) { (l, r) =>
              size match {
                case 1 => BuiltIns.compileByteComparison(ctx, if (signed) ComparisonType.LessSigned else ComparisonType.LessUnsigned, l, r, branches)
                case 2 => BuiltIns.compileWordComparison(ctx, if (signed) ComparisonType.LessSigned else ComparisonType.LessUnsigned, l, r, branches)
              }
            }
          case ">=" =>
            // TODO: signed
            val (size, signed) = assertComparison(ctx, params)
            compileTransitiveRelation(ctx, ">=", params, exprTypeAndVariable, branches) { (l, r) =>
              size match {
                case 1 => BuiltIns.compileByteComparison(ctx, if (signed) ComparisonType.GreaterOrEqualSigned else ComparisonType.GreaterOrEqualUnsigned, l, r, branches)
                case 2 => BuiltIns.compileWordComparison(ctx, if (signed) ComparisonType.GreaterOrEqualSigned else ComparisonType.GreaterOrEqualUnsigned, l, r, branches)
              }
            }
          case ">" =>
            // TODO: signed
            val (size, signed) = assertComparison(ctx, params)
            compileTransitiveRelation(ctx, ">", params, exprTypeAndVariable, branches) { (l, r) =>
              size match {
                case 1 => BuiltIns.compileByteComparison(ctx, if (signed) ComparisonType.GreaterSigned else ComparisonType.GreaterUnsigned, l, r, branches)
                case 2 => BuiltIns.compileWordComparison(ctx, if (signed) ComparisonType.GreaterSigned else ComparisonType.GreaterUnsigned, l, r, branches)
              }
            }
          case "<=" =>
            // TODO: signed
            val (size, signed) = assertComparison(ctx, params)
            compileTransitiveRelation(ctx, "<=", params, exprTypeAndVariable, branches) { (l, r) =>
              size match {
                case 1 => BuiltIns.compileByteComparison(ctx, if (signed) ComparisonType.LessOrEqualSigned else ComparisonType.LessOrEqualUnsigned, l, r, branches)
                case 2 => BuiltIns.compileWordComparison(ctx, if (signed) ComparisonType.LessOrEqualSigned else ComparisonType.LessOrEqualUnsigned, l, r, branches)
              }
            }
          case "==" =>
            val size = params.map(p => getExpressionType(ctx, p).size).max
            compileTransitiveRelation(ctx, "==", params, exprTypeAndVariable, branches) { (l, r) =>
              size match {
                case 1 => BuiltIns.compileByteComparison(ctx, ComparisonType.Equal, l, r, branches)
                case 2 => BuiltIns.compileWordComparison(ctx, ComparisonType.Equal, l, r, branches)
              }
            }
          case "!=" =>
            val (l, r, size) = assertBinary(ctx, params)
            size match {
              case 1 => BuiltIns.compileByteComparison(ctx, ComparisonType.NotEqual, l, r, branches)
              case 2 => BuiltIns.compileWordComparison(ctx, ComparisonType.NotEqual, l, r, branches)
            }
          case "+=" =>
            val (l, r, size) = assertAssignmentLike(ctx, params)
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
            val (l, r, size) = assertAssignmentLike(ctx, params)
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
            val (l, r, size) = assertAssignmentLike(ctx, params)
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
            val (l, r, size) = assertAssignmentLike(ctx, params)
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
            val (l, r, size) = assertAssignmentLike(ctx, params)
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
            val (l, r, size) = assertAssignmentLike(ctx, params)
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
            val (l, r, size) = assertAssignmentLike(ctx, params)
            size match {
              case 1 =>
                DecimalBuiltIns.compileByteShiftLeft(ctx, l, r, rotate = false) ++ compileByteStorage(ctx, Register.A, l)
              case i if i >= 2 =>
                l match {
                  case v: LhsExpression =>
                    DecimalBuiltIns.compileInPlaceLongShiftLeft(ctx, v, r)
                }
            }
          case ">>'=" =>
            val (l, r, size) = assertAssignmentLike(ctx, params)
            size match {
              case 1 =>
                DecimalBuiltIns.compileByteShiftRight(ctx, l, r, rotate = false) ++ compileByteStorage(ctx, Register.A, l)
              case i if i >= 2 =>
                l match {
                  case v: LhsExpression =>
                    DecimalBuiltIns.compileInPlaceLongShiftRight(ctx, v, r)
                }
            }
          case "*=" =>
            assertAllBytes("Long multiplication not supported", ctx, params)
            val (l, r, 1) = assertAssignmentLike(ctx, params)
            BuiltIns.compileInPlaceByteMultiplication(ctx, l, r)
          case "*'=" =>
            assertAllBytes("Long multiplication not supported", ctx, params)
            val (l, r, 1) = assertAssignmentLike(ctx, params)
            DecimalBuiltIns.compileInPlaceByteMultiplication(ctx, l, r)
          case "&=" =>
            val (l, r, size) = assertAssignmentLike(ctx, params)
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
            val (l, r, size) = assertAssignmentLike(ctx, params)
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
            val (l, r, size) = assertAssignmentLike(ctx, params)
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
                val (paramPreparation, statements) = MacroExpander.inlineFunction(ctx, function, params, expr.position)
                paramPreparation ++ statements.map {
                  case AssemblyStatement(opcode, addrMode, expression, elidable) =>
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
        val store = expressionStorageFromAX(ctx, exprTypeAndVariable, expr.position)
        calculate ++ store
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
      case (VoidType, _) => ???
      case (_, RegisterVariable(Register.A, _)) => noop
      case (_, RegisterVariable(Register.AW, _)) => List(AssemblyLine.implied(XBA), AssemblyLine.implied(TXA), AssemblyLine.implied(XBA))
      case (_, RegisterVariable(Register.X, _)) => List(AssemblyLine.implied(TAX))
      case (_, RegisterVariable(Register.Y, _)) => List(AssemblyLine.implied(TAY))
      case (_, RegisterVariable(Register.AX, _)) =>
        // TODO: sign extension
        noop
      case (_, RegisterVariable(Register.XA, _)) =>
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
      case (_, RegisterVariable(Register.YA, _)) =>
        // TODO: sign extension
        List(
          AssemblyLine.implied(TAY),
          AssemblyLine.implied(TXA))
      case (_, RegisterVariable(Register.AY, _)) =>
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

  private def getParamMaxSize(ctx: CompilationContext, params: List[Expression]): Int = {
    params.map { case expr => getExpressionType(ctx, expr).size}.max
  }

  private def getSumSize(ctx: CompilationContext, params: List[(Boolean, Expression)]): Int = {
    params.map { case (_, expr) => getExpressionType(ctx, expr).size}.max
  }

  private def assertAllBytes(msg: String, ctx: CompilationContext, params: List[Expression]): Unit = {
    if (params.exists { expr => getExpressionType(ctx, expr).size != 1 }) {
      ErrorReporting.fatal(msg, params.head.position)
    }
  }

  def compileAssignment(ctx: CompilationContext, source: Expression, target: LhsExpression): List[AssemblyLine] = {
    val env = ctx.env
    val b = env.get[Type]("byte")
    val w = env.get[Type]("word")
    target match {
      case VariableExpression(name) =>
        val v = env.get[Variable](name, target.position)
        // TODO check v.typ
        compile(ctx, source, Some((getExpressionType(ctx, source), v)), NoBranching)
      case SeparateBytesExpression(h: LhsExpression, l: LhsExpression) =>
        compile(ctx, source, Some(w, RegisterVariable(Register.AX, w)), NoBranching) ++
          compileByteStorage(ctx, Register.A, l) ++ compileByteStorage(ctx, Register.X, h)
      case SeparateBytesExpression(_, _) =>
        ErrorReporting.error("Invalid left-hand-side use of `:`")
        Nil
      case _ =>
        compile(ctx, source, Some(b, RegisterVariable(Register.A, b)), NoBranching) ++ compileByteStorage(ctx, Register.A, target)
    }
  }

  def lookupFunction(ctx: CompilationContext, f: FunctionCallExpression): MangledFunction = {
    val paramsWithTypes = f.expressions.map(x => getExpressionType(ctx, x) -> x)
    ctx.env.lookupFunction(f.functionName, paramsWithTypes).getOrElse(
      ErrorReporting.fatal(s"Cannot find function `${f.functionName}` with given params `${paramsWithTypes.map(_._1)}`", f.position))
  }

  def arrayBoundsCheck(ctx: CompilationContext, pointy: Pointy, register: Register.Value, index: Expression): List[AssemblyLine] = {
    if (!ctx.options.flags(CompilationFlag.CheckIndexOutOfBounds)) return Nil
    val arrayLength = pointy match {
      case _: VariablePointy => return Nil
      case ConstantPointy(_, None) => return Nil
      case ConstantPointy(_, Some(s)) => s
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
      val label = MfCompiler.nextLabel("bc")
      val compare = register match {
        case Register.A => CMP
        case Register.X => CPX
        case Register.Y => CPY
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
    val label = MfCompiler.nextLabel("sx")
    List(
      AssemblyLine.immediate(ORA, 0x7F),
      AssemblyLine.relative(BMI, label),
      AssemblyLine.immediate(LDA, 0),
      AssemblyLine.label(label))
  }
}
