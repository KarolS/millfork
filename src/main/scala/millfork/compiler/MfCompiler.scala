package millfork.compiler

import java.util.concurrent.atomic.AtomicLong

import millfork.{CompilationFlag, CompilationOptions}
import millfork.assembly._
import millfork.env._
import millfork.node.{Register, _}
import millfork.assembly.AddrMode._
import millfork.assembly.Opcode._
import millfork.error.ErrorReporting

import scala.collection.JavaConverters._

/**
  * @author Karol Stasiak
  */

sealed trait BranchSpec {
  def flip: BranchSpec
}

case object NoBranching extends BranchSpec {
  override def flip: BranchSpec = this
}

case class BranchIfTrue(label: String) extends BranchSpec {
  override def flip: BranchSpec = BranchIfFalse(label)
}

case class BranchIfFalse(label: String) extends BranchSpec {
  override def flip: BranchSpec = BranchIfTrue(label)
}

object BranchSpec {
  val None: BranchSpec = NoBranching
}

//noinspection NotImplementedCode,ScalaUnusedSymbol
object MlCompiler {


  private val labelCounter = new AtomicLong

  def nextLabel(prefix: String): String = "." + prefix + "__" + labelCounter.incrementAndGet().formatted("%05d")

  def compile(ctx: CompilationContext): Chunk = {
    val chunk = compile(ctx, ctx.function.code)
    val prefix = (if (ctx.function.interrupt) {
      if (ctx.options.flag(CompilationFlag.EmitCmosOpcodes)) {
        List(
          AssemblyLine.implied(SEI),
          AssemblyLine.implied(PHA),
          AssemblyLine.implied(PHX),
          AssemblyLine.implied(PHY),
          AssemblyLine.implied(CLD))
      } else {
        List(
          AssemblyLine.implied(SEI),
          AssemblyLine.implied(PHA),
          AssemblyLine.implied(TXA),
          AssemblyLine.implied(PHA),
          AssemblyLine.implied(TYA),
          AssemblyLine.implied(PHA),
          AssemblyLine.implied(CLD))
      }
    } else Nil) ++ stackPointerFixAtBeginning(ctx)
    if (prefix.nonEmpty) {
      LabelledChunk(ctx.function.name, SequenceChunk(List(LinearChunk(prefix), chunk)))
    } else {
      LabelledChunk(ctx.function.name, chunk)
    }
  }

  def lookupFunction(ctx: CompilationContext, f: FunctionCallExpression): MangledFunction = {
    val paramsWithTypes = f.expressions.map(x => getExpressionType(ctx, x) -> x)
    ctx.env.lookupFunction(f.functionName, paramsWithTypes).getOrElse(
      ErrorReporting.fatal(s"Cannot find function `${f.functionName}` with given params `${paramsWithTypes.map(_._1)}`", f.position))
  }

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
      case SeparateBytesExpression(h, l) =>
        if (getExpressionType(ctx, h).size > 1) ErrorReporting.error("Hi byte too large", h.position)
        if (getExpressionType(ctx, l).size > 1) ErrorReporting.error("Lo byte too large", l.position)
        w
      case SumExpression(params, _) => b
      case FunctionCallExpression("not", params) => bool
      case FunctionCallExpression("*", params) => b
      case FunctionCallExpression("|", params) => b
      case FunctionCallExpression("&", params) => b
      case FunctionCallExpression("^", params) => b
      case FunctionCallExpression("<<", params) => b
      case FunctionCallExpression(">>", params) => b
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
        lookupFunction(ctx, f).returnType
    }
  }

  def compileConstant(ctx: CompilationContext, expr: Constant, target: Variable): List[AssemblyLine] = {
    target match {
      case RegisterVariable(Register.A, _) => List(AssemblyLine(LDA, Immediate, expr))
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
        val addrMode = if(m.zeropage) ZeroPage else Absolute
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
              case mv: VariableInMemory => AssemblyLine.absolute(store, mv) :: Nil
              case sv@StackVariable(_, _, offset) => AssemblyLine.implied(transferToA) :: AssemblyLine.implied(TSX) :: AssemblyLine.absoluteX(STA, offset + ctx.extraStackOffset) :: Nil
            }
          case s if s > 1 =>
            v match {
              case mv: VariableInMemory =>
                if (mv.zeropage) {
                  AssemblyLine.zeropage(store, mv) ::
                    AssemblyLine.immediate(LDA, 0) ::
                    List.tabulate(s - 1)(i => AssemblyLine.zeropage(STA, mv.toAddress + (i + 1)))
                } else {
                  AssemblyLine.absolute(store, mv) ::
                    AssemblyLine.immediate(LDA, 0) ::
                    List.tabulate(s - 1)(i => AssemblyLine.absolute(STA, mv.toAddress + (i + 1)))
                }
              case sv@StackVariable(_, _, offset) =>
                AssemblyLine.implied(transferToA) ::
                  AssemblyLine.implied(TSX) ::
                  AssemblyLine.absoluteX(STA, offset + ctx.extraStackOffset) ::
                  List.tabulate(s - 1)(i => AssemblyLine.absoluteX(STA, offset + ctx.extraStackOffset + i + 1))
            }
        }
      case IndexedExpression(arrayName, indexExpr) =>
        val array = env.getArrayOrPointer(arrayName)
        val (variableIndex, constIndex) = env.evalVariableAndConstantSubParts(indexExpr)

        def storeToArrayAtUnknownIndex(variableIndex: Expression, arrayAddr: Constant) = {
          // TODO check typ
          val indexRegister = if (register == Register.Y) Register.X else Register.Y
          val calculatingIndex = preserveRegisterIfNeeded(ctx, register, compile(ctx, variableIndex, Some(b, RegisterVariable(indexRegister, b)), NoBranching))
          if (register == Register.A) {
            indexRegister match {
              case Register.Y =>
                calculatingIndex ++ List(AssemblyLine.absoluteY(STA, arrayAddr + constIndex))
              case Register.X =>
                calculatingIndex ++ List(AssemblyLine.absoluteX(STA, arrayAddr + constIndex))
            }
          } else {
            indexRegister match {
              case Register.Y =>
                calculatingIndex ++ List(AssemblyLine.implied(transferToA), AssemblyLine.absoluteY(STA, arrayAddr + constIndex))
              case Register.X =>
                calculatingIndex ++ List(AssemblyLine.implied(transferToA), AssemblyLine.absoluteX(STA, arrayAddr + constIndex))
            }
          }
        }

        (array, variableIndex) match {

          case (p: ConstantThing, None) =>
            List(AssemblyLine.absolute(store, env.genRelativeVariable(p.value + constIndex, b, zeropage = false)))
          case (p: ConstantThing, Some(v)) =>
            storeToArrayAtUnknownIndex(v, p.value)

          case (a@InitializedArray(_, _, _), None) =>
            List(AssemblyLine.absolute(store, env.genRelativeVariable(a.toAddress + constIndex, b, zeropage = false)))
          case (a@InitializedArray(_, _, _), Some(v)) =>
            storeToArrayAtUnknownIndex(v, a.toAddress)

          case (a@UninitializedArray(_, _), None) =>
            List(AssemblyLine.absolute(store, env.genRelativeVariable(a.toAddress + constIndex, b, zeropage = false)))
          case (a@UninitializedArray(_, _), Some(v)) =>
            storeToArrayAtUnknownIndex(v, a.toAddress)

          case (RelativeArray(_, arrayAddr, _), None) =>
            List(AssemblyLine.absolute(store, env.genRelativeVariable(arrayAddr + constIndex, b, zeropage = false)))
          case (RelativeArray(_, arrayAddr, _), Some(v)) =>
            storeToArrayAtUnknownIndex(v, arrayAddr)

          // TODO: are those two below okay?
          case (RelativeVariable(_, arrayAddr, typ, _), None) =>
            List(AssemblyLine.absolute(store, env.genRelativeVariable(arrayAddr + constIndex, b, zeropage = false)))
          case (RelativeVariable(_, arrayAddr, typ, _), Some(v)) =>
            storeToArrayAtUnknownIndex(v, arrayAddr)

          //TODO: should there be a type check or a zeropage check?
          case (pointerVariable@MemoryVariable(_, typ, _), None) =>
            register match {
              case Register.A =>
                List(AssemblyLine.immediate(LDY, constIndex), AssemblyLine.indexedY(STA, pointerVariable))
              case Register.Y =>
                List(AssemblyLine.implied(TYA), AssemblyLine.immediate(LDY, constIndex), AssemblyLine.indexedY(STA, pointerVariable), AssemblyLine.implied(TAY))
              case Register.X =>
                List(AssemblyLine.immediate(LDY, constIndex), AssemblyLine.implied(TXA), AssemblyLine.indexedY(STA, pointerVariable))
              case _ =>
                ErrorReporting.error("Cannot store a word in an array", target.position)
                Nil
            }
          case (pointerVariable@MemoryVariable(_, typ, _), Some(_)) =>
            val calculatingIndex = compile(ctx, indexExpr, Some(b, RegisterVariable(Register.Y, b)), NoBranching)
            register match {
              case Register.A =>
                preserveRegisterIfNeeded(ctx, Register.A, calculatingIndex) :+ AssemblyLine.indexedY(STA, pointerVariable)
              case Register.X =>
                preserveRegisterIfNeeded(ctx, Register.X, calculatingIndex) ++ List(AssemblyLine.implied(TXA), AssemblyLine.indexedY(STA, pointerVariable))
              case Register.Y =>
                AssemblyLine.implied(TYA) :: preserveRegisterIfNeeded(ctx, Register.A, calculatingIndex) ++ List(
                  AssemblyLine.indexedY(STA, pointerVariable), AssemblyLine.implied(TAY)
                )
              case _ =>
                ErrorReporting.error("Cannot store a word in an array", target.position)
                Nil
            }
        }

    }
  }

  def assertCompatible(exprType: Type, variableType: Type): Unit = {
    // TODO
  }

  val noop: List[AssemblyLine] = Nil

  def callingContext(ctx: CompilationContext, v: MemoryVariable): CompilationContext = {
    val result = new Environment(Some(ctx.env), "")
    result.registerVariable(VariableDeclarationStatement(v.name, v.typ.name, stack = false, global = false, constant = false, volatile = false, initialValue = None, address = None), ctx.options)
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

  def assertComparison(ctx: CompilationContext, params: List[Expression]): (Expression, Expression, Int, Boolean) = {
    if (params.length != 2) {
      ErrorReporting.fatal("sfgdgfsd", None)
    }
    (params.head, params(1)) match {
      case (l: Expression, r: Expression) =>
        val lt = getExpressionType(ctx, l)
        val rt = getExpressionType(ctx, r)
        (l, r, lt.size max rt.size, lt.isSigned || rt.isSigned)
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
                  case RegisterVariable(Register.A, _) => List(AssemblyLine.absolute(LDA, source))
                  case RegisterVariable(Register.X, _) => List(AssemblyLine.absolute(LDX, source))
                  case RegisterVariable(Register.Y, _) => List(AssemblyLine.absolute(LDY, source))
                  case RegisterVariable(Register.AX, _) =>
                    exprType.size match {
                      case 1 => if (exprType.isSigned) {
                        val label = nextLabel("sx")
                        List(
                          AssemblyLine.absolute(LDA, source),
                          AssemblyLine.implied(PHA),
                          AssemblyLine.immediate(ORA, 0x7F),
                          AssemblyLine.relative(BMI, label),
                          AssemblyLine.immediate(LDA, 0),
                          AssemblyLine.label(label),
                          AssemblyLine.implied(TAX),
                          AssemblyLine.implied(PLA))
                      } else List(
                        AssemblyLine.absolute(LDA, source),
                        AssemblyLine.immediate(LDX, 0))
                      case 2 => List(
                        AssemblyLine.absolute(LDA, source),
                        AssemblyLine.absolute(LDX, source, 1))
                    }
                  case RegisterVariable(Register.AY, _) =>
                    exprType.size match {
                      case 1 => if (exprType.isSigned) {
                        val label = nextLabel("sx")
                        List(
                          AssemblyLine.absolute(LDA, source),
                          AssemblyLine.implied(PHA),
                          AssemblyLine.immediate(ORA, 0x7F),
                          AssemblyLine.relative(BMI, label),
                          AssemblyLine.immediate(LDA, 0),
                          AssemblyLine.label(label),
                          AssemblyLine.implied(TAY),
                          AssemblyLine.implied(PLA))
                      } else {
                        List(
                          AssemblyLine.absolute(LDA, source),
                          AssemblyLine.immediate(LDY, 0))
                      }
                      case 2 => List(
                        AssemblyLine.absolute(LDA, source),
                        AssemblyLine.absolute(LDY, source, 1))
                    }
                  case RegisterVariable(Register.XA, _) =>
                    exprType.size match {
                      case 1 => if (exprType.isSigned) {
                        val label = nextLabel("sx")
                        List(
                          AssemblyLine.absolute(LDX, source),
                          AssemblyLine.implied(TXA),
                          AssemblyLine.immediate(ORA, 0x7F),
                          AssemblyLine.relative(BMI, label),
                          AssemblyLine.immediate(LDA, 0),
                          AssemblyLine.label(label))
                      } else List(
                        AssemblyLine.absolute(LDX, source),
                        AssemblyLine.immediate(LDA, 0))
                      case 2 => List(
                        AssemblyLine.absolute(LDX, source),
                        AssemblyLine.absolute(LDA, source, 1))
                    }
                  case RegisterVariable(Register.YA, _) =>
                    exprType.size match {
                      case 1 => if (exprType.isSigned) {
                        val label = nextLabel("sx")
                        List(
                          AssemblyLine.absolute(LDY, source),
                          AssemblyLine.implied(TYA),
                          AssemblyLine.immediate(ORA, 0x7F),
                          AssemblyLine.relative(BMI, label),
                          AssemblyLine.immediate(LDA, 0),
                          AssemblyLine.label(label))
                      } else List(
                        AssemblyLine.absolute(LDY, source),
                        AssemblyLine.immediate(LDA, 0))
                      case 2 => List(
                        AssemblyLine.absolute(LDY, source),
                        AssemblyLine.absolute(LDA, source, 1))
                    }
                  case target: VariableInMemory =>
                    if (exprType.size > target.typ.size) {
                      ErrorReporting.error(s"Variable `$target.name` is too small", expr.position)
                      Nil
                    } else {
                      val copy = List.tabulate(exprType.size)(i => List(AssemblyLine.absolute(LDA, source, i), AssemblyLine.absolute(STA, target, i)))
                      val extend = if (exprType.size == target.typ.size) Nil else if (exprType.isSigned) {
                        val label = nextLabel("sx")
                        List(
                          AssemblyLine.immediate(ORA, 0x7F),
                          AssemblyLine.relative(BMI, label),
                          AssemblyLine.immediate(LDA, 0),
                          AssemblyLine.label(label)) ++
                          List.tabulate(target.typ.size - exprType.size)(i => AssemblyLine.absolute(STA, target, i + exprType.size))
                      } else {
                        AssemblyLine.immediate(LDA, 0) ::
                          List.tabulate(target.typ.size - exprType.size)(i => AssemblyLine.absolute(STA, target, i + exprType.size))
                      }
                      copy.flatten ++ extend
                    }
                  case target: StackVariable =>
                    if (exprType.size > target.typ.size) {
                      ErrorReporting.error(s"Variable `$target.name` is too small", expr.position)
                      Nil
                    } else {
                      val copy = List.tabulate(exprType.size)(i => List(AssemblyLine.absolute(LDA, source, i), AssemblyLine.absoluteX(STA, target.baseOffset + ctx.extraStackOffset + i)))
                      val extend = if (exprType.size == target.typ.size) Nil else if (exprType.isSigned) {
                        val label = nextLabel("sx")
                        List(
                          AssemblyLine.immediate(ORA, 0x7F),
                          AssemblyLine.relative(BMI, label),
                          AssemblyLine.immediate(LDA, 0),
                          AssemblyLine.label(label)) ++
                          List.tabulate(target.typ.size - exprType.size)(i => AssemblyLine.absoluteX(STA, target.baseOffset + ctx.extraStackOffset + i + exprType.size))
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
                        val label = nextLabel("sx")
                        List(
                          AssemblyLine.implied(TSX),
                          AssemblyLine.absoluteX(LDA, offset + ctx.extraStackOffset),
                          AssemblyLine.implied(PHA),
                          AssemblyLine.immediate(ORA, 0x7F),
                          AssemblyLine.relative(BMI, label),
                          AssemblyLine.immediate(LDA, 0),
                          AssemblyLine.label(label),
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
                        val label = nextLabel("sx")
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
                        val label = nextLabel("sx")
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
                      val copy = List.tabulate(exprType.size)(i => List(AssemblyLine.absoluteX(LDA, offset + ctx.extraStackOffset + i), AssemblyLine.absolute(STA, target, i)))
                      val extend = if (exprType.size == target.typ.size) Nil else if (exprType.isSigned) {
                        val label = nextLabel("sx")
                        List(
                          AssemblyLine.immediate(ORA, 0x7F),
                          AssemblyLine.relative(BMI, label),
                          AssemblyLine.immediate(LDA, 0),
                          AssemblyLine.label(label)) ++
                          List.tabulate(target.typ.size - exprType.size)(i => AssemblyLine.absolute(STA, target, i + exprType.size))
                      } else {
                        AssemblyLine.immediate(LDA, 0) ::
                          List.tabulate(target.typ.size - exprType.size)(i => AssemblyLine.absolute(STA, target, i + exprType.size))
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
                        val label = nextLabel("sx")
                        List(
                          AssemblyLine.immediate(ORA, 0x7F),
                          AssemblyLine.relative(BMI, label),
                          AssemblyLine.immediate(LDA, 0),
                          AssemblyLine.label(label)) ++
                          List.tabulate(target.typ.size - exprType.size)(i => AssemblyLine.absoluteX(STA, target.baseOffset + ctx.extraStackOffset + i + exprType.size))
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
        val array = env.getArrayOrPointer(arrayName)
        // TODO: check
        val (variableIndex, constantIndex) = env.evalVariableAndConstantSubParts(indexExpr)
        exprTypeAndVariable.fold(noop) { case (exprType, target) =>

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
                val label = nextLabel("sx")
                AssemblyLine.variable(ctx, STA, target) ++
                List(
                  AssemblyLine.immediate(ORA, 0x7f),
                  AssemblyLine.relative(BMI, label),
                  AssemblyLine.immediate(LDA, 0),
                  AssemblyLine.label(label)) ++
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
                calculatingIndex ++ List(AssemblyLine.absoluteY(load, arrayAddr + constantIndex))
              case Register.X =>
                calculatingIndex ++ List(AssemblyLine.absoluteX(load, arrayAddr + constantIndex))
            }
          }

          val result = (array, variableIndex) match {
            case (a: ConstantThing, None) =>
              List(AssemblyLine.absolute(load, env.genRelativeVariable(a.value + constantIndex, b, zeropage = false)))
            case (a: ConstantThing, Some(v)) =>
              loadFromArrayAtUnknownIndex(v, a.value)

            case (a: MlArray, None) =>
              List(AssemblyLine.absolute(load, env.genRelativeVariable(a.toAddress + constantIndex, b, zeropage = false)))
            case (a: MlArray, Some(v)) =>
              loadFromArrayAtUnknownIndex(v, a.toAddress)

            // TODO: see above
            case (RelativeVariable(_, arrayAddr, typ, _), None) =>
              List(AssemblyLine.absolute(load, env.genRelativeVariable(arrayAddr + constantIndex, b, zeropage = false)))
            case (RelativeVariable(_, arrayAddr, typ, _), Some(v)) =>
              loadFromArrayAtUnknownIndex(v, arrayAddr)

            // TODO: see above
            case (pointerVariable@MemoryVariable(_, typ, _), None) =>
              register match {
                case Register.A =>
                  List(AssemblyLine.immediate(LDY, constantIndex), AssemblyLine.indexedY(LDA, pointerVariable))
                case Register.Y =>
                  List(AssemblyLine.immediate(LDY, constantIndex), AssemblyLine.indexedY(LDA, pointerVariable), AssemblyLine.implied(TAY))
                case Register.X =>
                  List(AssemblyLine.immediate(LDY, constantIndex), AssemblyLine.indexedY(LDX, pointerVariable))
              }
            case (pointerVariable@MemoryVariable(_, typ, _), Some(_)) =>
              val calculatingIndex = compile(ctx, indexExpr, Some(b, RegisterVariable(Register.Y, b)), NoBranching)
              register match {
                case Register.A =>
                  calculatingIndex :+ AssemblyLine.indexedY(LDA, pointerVariable)
                case Register.X =>
                  calculatingIndex :+ AssemblyLine.indexedY(LDX, pointerVariable)
                case Register.Y =>
                  calculatingIndex ++ List(AssemblyLine.indexedY(LDA, pointerVariable), AssemblyLine.implied(TAY))
              }
          }
          register match {
            case Register.A | Register.X | Register.Y => result ++ suffix
            case Register.AX => result :+ AssemblyLine.immediate(LDX, 0)
            case Register.AY => result :+ AssemblyLine.immediate(LDY, 0)
          }
        }
      case SumExpression(params, decimal) =>
        assertAllBytesForSum("Long addition not supported", ctx, params)
        val calculate = BuiltIns.compileAddition(ctx, params, decimal = decimal)
        val store = expressionStorageFromAX(ctx, exprTypeAndVariable, expr.position)
        calculate ++ store
      case SeparateBytesExpression(h, l) =>
        exprTypeAndVariable.fold {
          // TODO: order?
          compile(ctx, l, None, branches) ++ compile(ctx, h, None, branches)
        } { case (exprType, target) =>
          assertCompatible(exprType, target.typ)
          target match {
            case RegisterVariable(Register.A | Register.X | Register.Y, _) => compile(ctx, l, exprTypeAndVariable, branches)
            case RegisterVariable(Register.AX, _) =>
              compile(ctx, l, Some(b -> RegisterVariable(Register.A, b)), branches) ++
                compile(ctx, h, Some(b -> RegisterVariable(Register.X, b)), branches)
            case RegisterVariable(Register.AY, _) =>
              compile(ctx, l, Some(b -> RegisterVariable(Register.A, b)), branches) ++
                compile(ctx, h, Some(b -> RegisterVariable(Register.Y, b)), branches)
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
          case "&&" =>
            assertBool(ctx, params, 2)
            val a = params.head
            val b = params(1)
            branches match {
              case BranchIfFalse(_) =>
                compile(ctx, a, exprTypeAndVariable, branches) ++ compile(ctx, b, exprTypeAndVariable, branches)
              case _ =>
                val skip = nextLabel("an")
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
                val skip = nextLabel("or")
                compile(ctx, a, exprTypeAndVariable, BranchIfTrue(skip)) ++
                  compile(ctx, b, exprTypeAndVariable, branches) ++
                  List(AssemblyLine.label(skip))
            }
          case "^^" => ???
          case "&" =>
            assertAllBytes("Long bit ops not supported", ctx, params)
            BuiltIns.compileBitOps(AND, ctx, params)
          case "*" =>
            assertAllBytes("Long multiplication not supported", ctx, params)
            BuiltIns.compileByteMultiplication(ctx, params)
          case "|" =>
            assertAllBytes("Long bit ops not supported", ctx, params)
            BuiltIns.compileBitOps(ORA, ctx, params)
          case "^" =>
            assertAllBytes("Long bit ops not supported", ctx, params)
            BuiltIns.compileBitOps(EOR, ctx, params)
          case ">>>>" =>
            val (l, r, 2) = assertBinary(ctx, params)
            l match {
              case v: LhsExpression =>
                BuiltIns.compileNonetOps(ctx, v, r)
            }
          case "<<" =>
            assertAllBytes("Long shift ops not supported", ctx, params)
            val (l, r, 1) = assertBinary(ctx, params)
            BuiltIns.compileShiftOps(ASL, ctx, l, r)
          case ">>" =>
            assertAllBytes("Long shift ops not supported", ctx, params)
            val (l, r, 1) = assertBinary(ctx, params)
            BuiltIns.compileShiftOps(LSR, ctx, l, r)
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
            val (l, r, size, signed) = assertComparison(ctx, params)
            size match {
              case 1 => BuiltIns.compileByteComparison(ctx, if (signed) ComparisonType.LessSigned else ComparisonType.LessUnsigned, l, r, branches)
              case 2 => BuiltIns.compileWordComparison(ctx, if (signed) ComparisonType.LessSigned else ComparisonType.LessUnsigned, l, r, branches)
            }
          case ">=" =>
            // TODO: signed
            val (l, r, size, signed) = assertComparison(ctx, params)
            size match {
              case 1 => BuiltIns.compileByteComparison(ctx, if (signed) ComparisonType.GreaterOrEqualSigned else ComparisonType.GreaterOrEqualUnsigned, l, r, branches)
              case 2 => BuiltIns.compileWordComparison(ctx, if (signed) ComparisonType.GreaterOrEqualSigned else ComparisonType.GreaterOrEqualUnsigned, l, r, branches)
            }
          case ">" =>
            // TODO: signed
            val (l, r, size, signed) = assertComparison(ctx, params)
            size match {
              case 1 => BuiltIns.compileByteComparison(ctx, if (signed) ComparisonType.GreaterSigned else ComparisonType.GreaterUnsigned, l, r, branches)
              case 2 => BuiltIns.compileWordComparison(ctx, if (signed) ComparisonType.GreaterSigned else ComparisonType.GreaterUnsigned, l, r, branches)
            }
          case "<=" =>
            // TODO: signed
            val (l, r, size, signed) = assertComparison(ctx, params)
            size match {
              case 1 => BuiltIns.compileByteComparison(ctx, if (signed) ComparisonType.LessOrEqualSigned else ComparisonType.LessOrEqualUnsigned, l, r, branches)
              case 2 => BuiltIns.compileWordComparison(ctx, if (signed) ComparisonType.LessOrEqualSigned else ComparisonType.LessOrEqualUnsigned, l, r, branches)
            }
          case "==" =>
            val (l, r, size) = assertBinary(ctx, params)
            size match {
              case 1 => BuiltIns.compileByteComparison(ctx, ComparisonType.Equal, l, r, branches)
              case 2 => BuiltIns.compileWordComparison(ctx, ComparisonType.Equal, l, r, branches)
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
            lookupFunction(ctx, f) match {
              case function: InlinedFunction =>
                inlineFunction(function, params, Some(ctx)).map {
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
                    secondViaMemory ++ thirdViaRegisters :+ AssemblyLine.absolute(JSR, function)
                  case NormalParamSignature(paramVars) =>
                    params.zip(paramVars).flatMap {
                      case (paramExpr, paramVar) =>
                        val callCtx = callingContext(ctx, paramVar)
                        compileAssignment(callCtx, paramExpr, VariableExpression(paramVar.name))
                    } ++ List(AssemblyLine.absolute(JSR, function))
                }
                result
            }
        }
        val store = expressionStorageFromAX(ctx, exprTypeAndVariable, expr.position)
        calculate ++ store
    }
  }

  def expressionStorageFromAX(ctx: CompilationContext, exprTypeAndVariable: Option[(Type, Variable)], position: Option[Position]): List[AssemblyLine] = {
    exprTypeAndVariable.fold(noop) {
      case (VoidType, _) => ???
      case (_, RegisterVariable(Register.A, _)) => noop
      case (_, RegisterVariable(Register.X, _)) => List(AssemblyLine.implied(TAX))
      case (_, RegisterVariable(Register.Y, _)) => List(AssemblyLine.implied(TAY))
      case (_, RegisterVariable(Register.AX, _)) =>
        // TODO: sign extension
        noop
      case (_, RegisterVariable(Register.XA, _)) =>
        // TODO: sign extension
        if (ctx.options.flag(CompilationFlag.EmitCmosOpcodes)) {
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
        List(
          AssemblyLine.implied(PHA),
          AssemblyLine.implied(TXA),
          AssemblyLine.implied(TAY),
          AssemblyLine.implied(PLA))
      case (t, v: VariableInMemory) => t.size match {
        case 1 => v.typ.size match {
          case 1 =>
            List(AssemblyLine.absolute(STA, v))
          case s if s > 1 =>
            if (t.isSigned) {
              val label = nextLabel("sx")
              List(
                AssemblyLine.absolute(STA, v),
                AssemblyLine.immediate(ORA, 0x7f),
                AssemblyLine.relative(BMI, label),
                AssemblyLine.immediate(LDA, 0),
                AssemblyLine.label(label)) ++ List.tabulate(s - 1)(i => AssemblyLine.absolute(STA, v, i + 1))
            } else {
              List(
                AssemblyLine.absolute(STA, v),
                AssemblyLine.immediate(LDA, 0)) ++
                List.tabulate(s - 1)(i => AssemblyLine.absolute(STA, v, i + 1))
            }
        }
        case 2 => v.typ.size match {
          case 1 =>
            ErrorReporting.error(s"Variable `${v.name}` cannot hold a word", position)
            Nil
          case 2 =>
            List(AssemblyLine.absolute(STA, v), AssemblyLine.absolute(STX, v, 1))
          case s if s > 2 =>
            if (t.isSigned) {
              val label = nextLabel("sx")
              List(
                AssemblyLine.absolute(STA, v),
                AssemblyLine.absolute(STX, v, 1),
                AssemblyLine.implied(TXA),
                AssemblyLine.immediate(ORA, 0x7f),
                AssemblyLine.relative(BMI, label),
                AssemblyLine.immediate(LDA, 0),
                AssemblyLine.label(label)) ++ List.tabulate(s - 2)(i => AssemblyLine.absolute(STA, v, i + 2))
            } else {
              List(
                AssemblyLine.absolute(STA, v),
                AssemblyLine.absolute(STX, v, 1),
                AssemblyLine.immediate(LDA, 0)) ++
                List.tabulate(s - 2)(i => AssemblyLine.absolute(STA, v, i + 2))
            }
        }
      }
      case (t, v: StackVariable) => t.size match {
        case 1 => v.typ.size match {
          case 1 =>
            List(AssemblyLine.implied(TSX), AssemblyLine.absoluteX(STA, v.baseOffset + ctx.extraStackOffset))
          case s if s > 1 =>
            if (t.isSigned) {
              val label = nextLabel("sx")
              List(
                AssemblyLine.implied(TSX),
                AssemblyLine.absoluteX(STA, v.baseOffset + ctx.extraStackOffset),
                AssemblyLine.immediate(ORA, 0x7f),
                AssemblyLine.relative(BMI, label),
                AssemblyLine.immediate(LDA, 0),
                AssemblyLine.label(label)) ++ List.tabulate(s - 1)(i => AssemblyLine.absoluteX(STA, v.baseOffset + ctx.extraStackOffset + i + 1))
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

  private def assertAllBytesForSum(msg: String, ctx: CompilationContext, params: List[(Boolean, Expression)]): Unit = {
    if (params.exists { case (_, expr) => getExpressionType(ctx, expr).size != 1 }) {
      ErrorReporting.fatal(msg, params.head._2.position)
    }
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


  def compile(ctx: CompilationContext, statements: List[ExecutableStatement]): Chunk = {
    SequenceChunk(statements.map(s => compile(ctx, s)))
  }

  def inlineFunction(i: InlinedFunction, params: List[Expression], cc: Option[CompilationContext]): List[ExecutableStatement] = {
    var actualCode = i.code
    i.params match {
      case AssemblyParamSignature(assParams) =>
        assParams.zip(params).foreach {
          case (AssemblyParam(typ, Placeholder(ph, phType), AssemblyParameterPassingBehaviour.ByReference), actualParam) =>
            actualParam match {
              case VariableExpression(vname) =>
                cc.foreach(_.env.get[ThingInMemory](vname))
              case l: LhsExpression =>
                // TODO: ??
                cc.foreach(c => compileByteStorage(c, Register.A, l))
              case _ =>
                ErrorReporting.error("A non-assignable expression was passed to an inlineable function as a `ref` parameter", actualParam.position)
            }
            actualCode = actualCode.map {
              case a@AssemblyStatement(_, _, expr, _) =>
                a.copy(expression = expr.replaceVariable(ph, actualParam))
              case x => x
            }
          case (AssemblyParam(typ, Placeholder(ph, phType), AssemblyParameterPassingBehaviour.ByConstant), actualParam) =>
            cc.foreach(_.env.eval(actualParam).getOrElse(Constant.error("Non-constant expression was passed to an inlineable function as a `const` parameter", actualParam.position)))
            actualCode = actualCode.map {
              case a@AssemblyStatement(_, _, expr, _) =>
                a.copy(expression = expr.replaceVariable(ph, actualParam))
              case x => x
            }
          case (AssemblyParam(_, _, AssemblyParameterPassingBehaviour.Copy), actualParam) =>
            ???
          case (_, actualParam) =>
        }
      case NormalParamSignature(Nil) => i.code
      case NormalParamSignature(normalParams) => ???
    }
    actualCode
  }

  def stackPointerFixAtBeginning(ctx: CompilationContext): List[AssemblyLine] = {
    val m = ctx.function
    if (m.stackVariablesSize == 0) return Nil
    if (ctx.options.flag(CompilationFlag.EmitIllegals)) {
      if (m.stackVariablesSize > 4)
        return List(
          AssemblyLine.implied(TSX),
          AssemblyLine.immediate(LDA, 0xff),
          AssemblyLine.immediate(SBX, m.stackVariablesSize),
          AssemblyLine.implied(TXS))
    }
    List.fill(m.stackVariablesSize)(AssemblyLine.implied(PHA))
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
          AssemblyLine.implied(TXS))
      if (m.returnType.size == 1 && m.stackVariablesSize > 6)
        return List(
          AssemblyLine.implied(TAY),
          AssemblyLine.implied(TSX),
          AssemblyLine.immediate(LDA, 0xff),
          AssemblyLine.immediate(SBX, 256 - m.stackVariablesSize),
          AssemblyLine.implied(TXS),
          AssemblyLine.implied(TYA))
    }

    AssemblyLine.implied(TSX) :: (List.fill(m.stackVariablesSize)(AssemblyLine.implied(INX)) :+ AssemblyLine.implied(TXS))
  }

  def compile(ctx: CompilationContext, statement: ExecutableStatement): Chunk = {
    val env = ctx.env
    val m = ctx.function
    val b = env.get[Type]("byte")
    val w = env.get[Type]("word")
    val someRegisterA = Some(b, RegisterVariable(Register.A, b))
    val someRegisterAX = Some(w, RegisterVariable(Register.AX, w))
    val someRegisterYA = Some(w, RegisterVariable(Register.YA, w))
    val returnInstructions = if (m.interrupt) {
      if (ctx.options.flag(CompilationFlag.EmitCmosOpcodes)) {
        List(
          AssemblyLine.implied(PLY),
          AssemblyLine.implied(PLX),
          AssemblyLine.implied(PLA),
          AssemblyLine.implied(CLI),
          AssemblyLine.implied(RTI))
      } else {
        List(
          AssemblyLine.implied(PLA),
          AssemblyLine.implied(TAY),
          AssemblyLine.implied(PLA),
          AssemblyLine.implied(TAX),
          AssemblyLine.implied(PLA),
          AssemblyLine.implied(CLI),
          AssemblyLine.implied(RTI))
      }
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
            } else{
              env.evalForAsm(x).getOrElse(env.get[ThingInMemory](name, x.position).toAddress)
            }
          case _ =>
            env.evalForAsm(x).getOrElse(Constant.error(s"`$x` is not a constant", x.position))
        }
        val actualAddrMode = a match {
          case Absolute if OpcodeClasses.ShortBranching(o) => Relative
          case IndexedX if o == JMP => AbsoluteIndexedX
          case Indirect if o != JMP => ZeroPageIndirect
          case _ => a
        }
        LinearChunk(List(AssemblyLine(o, actualAddrMode, c, e)))
      case Assignment(dest, source) =>
        LinearChunk(compileAssignment(ctx, source, dest))
      case ExpressionStatement(e@FunctionCallExpression(name, params)) =>
        env.lookupFunction(name, params.map(p => getExpressionType(ctx, p) -> p)) match {
          case Some(i: InlinedFunction) =>
            compile(ctx, inlineFunction(i, params, Some(ctx)))
          case _ =>
            LinearChunk(compile(ctx, e, None, NoBranching))
        }
      case ExpressionStatement(e) =>
        LinearChunk(compile(ctx, e, None, NoBranching))
      case BlockStatement(s) =>
        SequenceChunk(s.map(compile(ctx, _)))
      case ReturnStatement(None) =>
        // TODO: return type check
        // TODO: better stackpointer fix
        ctx.function.returnType match {
          case _: BooleanType =>
            LinearChunk(stackPointerFixBeforeReturn(ctx) ++ returnInstructions)
          case t => t.size match {
            case 0 =>
              LinearChunk(stackPointerFixBeforeReturn(ctx) ++
                List(AssemblyLine.discardAF(), AssemblyLine.discardXF(), AssemblyLine.discardYF()) ++ returnInstructions)
            case 1 =>
              LinearChunk(stackPointerFixBeforeReturn(ctx) ++
                List(AssemblyLine.discardXF(), AssemblyLine.discardYF()) ++ returnInstructions)
            case 2 =>
              LinearChunk(stackPointerFixBeforeReturn(ctx) ++
                List(AssemblyLine.discardYF()) ++ returnInstructions)
          }
        }
      case ReturnStatement(Some(e)) =>
        m.returnType match {
          case _: BooleanType =>
            m.returnType.size match {
              case 0 =>
                ErrorReporting.error("Cannot return anything from a void function", statement.position)
                LinearChunk(stackPointerFixBeforeReturn(ctx) ++ returnInstructions)
              case 1 =>
                LinearChunk(compile(ctx, e, someRegisterA, NoBranching) ++ stackPointerFixBeforeReturn(ctx) ++ returnInstructions)
              case 2 =>
                LinearChunk(compile(ctx, e, someRegisterAX, NoBranching) ++ stackPointerFixBeforeReturn(ctx) ++ returnInstructions)
            }
          case _ =>
            m.returnType.size match {
              case 0 =>
                ErrorReporting.error("Cannot return anything from a void function", statement.position)
                LinearChunk(stackPointerFixBeforeReturn(ctx) ++ List(AssemblyLine.discardAF(), AssemblyLine.discardXF(), AssemblyLine.discardYF()) ++ returnInstructions)
              case 1 =>
                LinearChunk(compile(ctx, e, someRegisterA, NoBranching) ++ stackPointerFixBeforeReturn(ctx) ++ List(AssemblyLine.discardXF(), AssemblyLine.discardYF()) ++ returnInstructions)
              case 2 =>
                // TODO: ???
                val stackPointerFix = stackPointerFixBeforeReturn(ctx)
                if (stackPointerFix.isEmpty) {
                  LinearChunk(compile(ctx, e, someRegisterAX, NoBranching) ++ List(AssemblyLine.discardYF()) ++ returnInstructions)
                } else {
                  LinearChunk(compile(ctx, e, someRegisterYA, NoBranching) ++
                    stackPointerFix ++
                    List(AssemblyLine.implied(TAX), AssemblyLine.implied(TYA), AssemblyLine.discardYF()) ++
                    returnInstructions)
                }
            }
        }
      case IfStatement(condition, thenPart, elsePart) =>
        val condType = getExpressionType(ctx, condition)
        val thenBlock = compile(ctx, thenPart)
        val elseBlock = compile(ctx, elsePart)
        val largeThenBlock = thenBlock.sizeInBytes > 100
        val largeElseBlock = elseBlock.sizeInBytes > 100
        condType match {
          case ConstantBooleanType(_, true) => thenBlock
          case ConstantBooleanType(_, false) => elseBlock
          case FlagBooleanType(_, jumpIfTrue, jumpIfFalse) =>
            (thenPart, elsePart) match {
              case (Nil, Nil) => EmptyChunk
              case (Nil, _) =>
                val conditionBlock = LinearChunk(compile(ctx, condition, someRegisterA, NoBranching))
                if (largeElseBlock) {
                  val middle = nextLabel("el")
                  val end = nextLabel("fi")
                  SequenceChunk(List(conditionBlock, branchChunk(jumpIfFalse, middle), jmpChunk(end), labelChunk(middle), elseBlock, labelChunk(end)))
                } else {
                  val end = nextLabel("fi")
                  SequenceChunk(List(conditionBlock, branchChunk(jumpIfTrue, end), elseBlock, labelChunk(end)))
                }
              case (_, Nil) =>
                val conditionBlock = LinearChunk(compile(ctx, condition, someRegisterA, NoBranching))
                if (largeThenBlock) {
                  val middle = nextLabel("th")
                  val end = nextLabel("fi")
                  SequenceChunk(List(conditionBlock, branchChunk(jumpIfTrue, middle), jmpChunk(end), labelChunk(middle), elseBlock, labelChunk(end)))
                } else {
                  val end = nextLabel("fi")
                  SequenceChunk(List(conditionBlock, branchChunk(jumpIfFalse, end), thenBlock, labelChunk(end)))
                }
              case _ =>
                // TODO: large blocks
                if (largeElseBlock || largeThenBlock) ErrorReporting.error("Large blocks in if statement", statement.position)
                val middle = nextLabel("el")
                val end = nextLabel("fi")
                val conditionBlock = LinearChunk(compile(ctx, condition, someRegisterA, NoBranching))
                SequenceChunk(List(conditionBlock, branchChunk(jumpIfFalse, middle), thenBlock, jmpChunk(end), labelChunk(middle), elseBlock, labelChunk(end)))
            }
          case BuiltInBooleanType =>
            (thenPart, elsePart) match {
              case (Nil, Nil) => EmptyChunk
              case (Nil, _) =>
                val end = nextLabel("fi")
                val conditionBlock = LinearChunk(compile(ctx, condition, someRegisterA, BranchIfTrue(end)))
                SequenceChunk(List(conditionBlock, elseBlock, labelChunk(end)))
              case (_, Nil) =>
                val end = nextLabel("fi")
                val conditionBlock = LinearChunk(compile(ctx, condition, someRegisterA, BranchIfFalse(end)))
                SequenceChunk(List(conditionBlock, thenBlock, labelChunk(end)))
              case _ =>
                val middle = nextLabel("el")
                val end = nextLabel("fi")
                val conditionBlock = LinearChunk(compile(ctx, condition, someRegisterA, BranchIfFalse(middle)))
                SequenceChunk(List(conditionBlock, thenBlock, jmpChunk(end), labelChunk(middle), elseBlock, labelChunk(end)))
            }
          case _ =>
            ErrorReporting.error(s"Illegal type for a condition: `$condType`", condition.position)
            EmptyChunk
        }
      case WhileStatement(condition, bodyPart) =>
        val condType = getExpressionType(ctx, condition)
        val bodyBlock = compile(ctx, bodyPart)
        val largeBodyBlock = bodyBlock.sizeInBytes > 100
        condType match {
          case ConstantBooleanType(_, true) =>
            val conditionBlock = LinearChunk(compile(ctx, condition, someRegisterA, NoBranching))
            val start = nextLabel("wh")
            SequenceChunk(List(labelChunk(start), bodyBlock, jmpChunk(start)))
          case ConstantBooleanType(_, false) => EmptyChunk
          case FlagBooleanType(_, jumpIfTrue, jumpIfFalse) =>
            if (largeBodyBlock) {
              val start = nextLabel("wh")
              val middle = nextLabel("he")
              val end = nextLabel("ew")
              val conditionBlock = LinearChunk(compile(ctx, condition, someRegisterA, NoBranching))
              SequenceChunk(List(labelChunk(start), conditionBlock, branchChunk(jumpIfTrue, middle), jmpChunk(end), bodyBlock, jmpChunk(start), labelChunk(end)))
            } else {
              val start = nextLabel("wh")
              val end = nextLabel("ew")
              val conditionBlock = LinearChunk(compile(ctx, condition, someRegisterA, NoBranching))
              SequenceChunk(List(labelChunk(start), conditionBlock, branchChunk(jumpIfFalse, end), bodyBlock, jmpChunk(start), labelChunk(end)))
            }
          case BuiltInBooleanType =>
            if (largeBodyBlock) {
              val start = nextLabel("wh")
              val middle = nextLabel("he")
              val end = nextLabel("ew")
              val conditionBlock = LinearChunk(compile(ctx, condition, someRegisterA, BranchIfTrue(middle)))
              SequenceChunk(List(labelChunk(start), conditionBlock, jmpChunk(end), labelChunk(middle), bodyBlock, jmpChunk(start), labelChunk(end)))
            } else {
              val start = nextLabel("wh")
              val end = nextLabel("ew")
              val conditionBlock = LinearChunk(compile(ctx, condition, someRegisterA, BranchIfFalse(end)))
              SequenceChunk(List(labelChunk(start), conditionBlock, bodyBlock, jmpChunk(start), labelChunk(end)))
            }
          case _ =>
            ErrorReporting.error(s"Illegal type for a condition: `$condType`", condition.position)
            EmptyChunk
        }
      case DoWhileStatement(bodyPart, condition) =>
        val condType = getExpressionType(ctx, condition)
        val bodyBlock = compile(ctx, bodyPart)
        val largeBodyBlock = bodyBlock.sizeInBytes > 100
        condType match {
          case ConstantBooleanType(_, true) =>
            val start = nextLabel("do")
            val conditionBlock = LinearChunk(compile(ctx, condition, someRegisterA, NoBranching))
            SequenceChunk(List(labelChunk(start), bodyBlock, jmpChunk(start)))
          case ConstantBooleanType(_, false) => bodyBlock
          case FlagBooleanType(_, jumpIfTrue, jumpIfFalse) =>
            val start = nextLabel("do")
            val conditionBlock = LinearChunk(compile(ctx, condition, someRegisterA, NoBranching))
            if (largeBodyBlock) {
              val end = nextLabel("od")
              SequenceChunk(List(labelChunk(start), bodyBlock, conditionBlock, branchChunk(jumpIfFalse, end), jmpChunk(start), labelChunk(end)))
            } else {
              SequenceChunk(List(labelChunk(start), bodyBlock, conditionBlock, branchChunk(jumpIfTrue, start)))
            }
          case BuiltInBooleanType =>
            val start = nextLabel("do")
            if (largeBodyBlock) {
              val end = nextLabel("od")
              val conditionBlock = LinearChunk(compile(ctx, condition, someRegisterA, BranchIfFalse(end)))
              SequenceChunk(List(labelChunk(start), bodyBlock, conditionBlock, jmpChunk(start), labelChunk(end)))
            } else {
              val conditionBlock = LinearChunk(compile(ctx, condition, someRegisterA, BranchIfTrue(start)))
              SequenceChunk(List(labelChunk(start), bodyBlock, conditionBlock))
            }
          case _ =>
            ErrorReporting.error(s"Illegal type for a condition: `$condType`", condition.position)
            EmptyChunk
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
            EmptyChunk

          case (ForDirection.To | ForDirection.ParallelTo, Some(NumericConstant(s, ssize)), Some(NumericConstant(e, _))) if s == e =>
            compile(ctx, Assignment(vex, f.start) :: f.body)
          case (ForDirection.To | ForDirection.ParallelTo, Some(NumericConstant(s, ssize)), Some(NumericConstant(e, _))) if s > e =>
            EmptyChunk

          case (ForDirection.ParallelUntil, Some(NumericConstant(0, ssize)), Some(NumericConstant(e, _))) if e > 0 =>
            compile(ctx, List(
              Assignment(vex, f.end),
              DoWhileStatement(decrement :: f.body, FunctionCallExpression("!=", List(vex, f.start)))
            ))

          case (ForDirection.DownTo, Some(NumericConstant(s, ssize)), Some(NumericConstant(e, esize))) if s == e =>
            compile(ctx, Assignment(vex, LiteralExpression(s, ssize)) :: f.body)
          case (ForDirection.DownTo, Some(NumericConstant(s, ssize)), Some(NumericConstant(e, esize))) if s < e =>
            EmptyChunk
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
          case (ForDirection.To | ForDirection.ParallelTo,_,_) =>
            compile(ctx, List(
              Assignment(vex, f.start),
              WhileStatement(
                FunctionCallExpression("<=", List(vex, f.end)),
                f.body :+ increment),
            ))
          case (ForDirection.DownTo,_,_) =>
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

  private def labelChunk(labelName: String) = {
    LinearChunk(List(AssemblyLine.label(Label(labelName))))
  }

  private def jmpChunk(labelName: String) = {
    LinearChunk(List(AssemblyLine.absolute(JMP, Label(labelName))))
  }

  private def branchChunk(opcode: Opcode.Value, labelName: String) = {
    LinearChunk(List(AssemblyLine.relative(opcode, Label(labelName))))
  }
}
