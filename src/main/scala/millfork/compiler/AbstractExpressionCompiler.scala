package millfork.compiler

import millfork.CompilationFlag
import millfork.env._
import millfork.node._
import millfork.error.{ConsoleLogger, Logger}
import millfork.assembly.AbstractCode
import millfork.output.NoAlignment

import scala.collection.mutable.ListBuffer

/**
  * @author Karol Stasiak
  */
class AbstractExpressionCompiler[T <: AbstractCode] {

  def getExpressionType(ctx: CompilationContext, expr: Expression): Type = AbstractExpressionCompiler.getExpressionType(ctx, expr)

  def extractWordExpandedBytes(ctx: CompilationContext, params:List[Expression]): Option[List[Expression]] = {
    val result = ListBuffer[Expression]()
    for(param <- params) {
      if (ctx.env.eval(param).isDefined) return None
      AbstractExpressionCompiler.getExpressionType(ctx, param) match {
        case t: PlainType if t.size == 1 && !t.isSigned =>
          result += param
        case t: PlainType if t.size == 2 =>
          param match {
            case FunctionCallExpression(functionName, List(inner)) =>
              AbstractExpressionCompiler.getExpressionType(ctx, inner) match {
                case t: PlainType if t.size == 1 && !t.isSigned =>
                  ctx.env.maybeGet[Type](functionName) match {
                    case Some(tw: PlainType) if tw.size == 2 =>
                      result += inner
                    case _ => return None
                  }
                case _ => return None
              }
            case _ => return None
          }
        case _ => return None
      }
    }
    Some(result.toList)
  }

  def assertAllArithmetic(ctx: CompilationContext,expressions: List[Expression], booleanHint: String = ""): Unit = {
     for(e <- expressions) {
       val typ = getExpressionType(ctx, e)
       if (!typ.isArithmetic) {
         ctx.log.error(s"Cannot perform arithmetic operations on type `$typ`", e.position)
         if (booleanHint != "" && typ.isBoollike) {
           ctx.log.info(s"Did you mean: $booleanHint")
         }
       }
     }
  }

  def lookupFunction(ctx: CompilationContext, f: FunctionCallExpression): MangledFunction = AbstractExpressionCompiler.lookupFunction(ctx.env, ctx.log, f)

  def assertCompatible(exprType: Type, variableType: Type): Unit = {
    // TODO
  }

  def callingContext(ctx: CompilationContext, callee: String, v: VariableInMemory): CompilationContext = {
    val result = new Environment(Some(ctx.env), "", ctx.options.platform.cpuFamily, ctx.options)
    val localName = v.name + "`aa"
    result.addVariable(ctx.options, localName, v, None)
    ctx.copy(env = result)
  }

  def getArithmeticParamMaxSize(ctx: CompilationContext, params: List[Expression], booleanHint: String = ""): Int = {
    assertAllArithmetic(ctx, params, booleanHint = booleanHint)
    params.map(expr => getExpressionType(ctx, expr).size).max
  }

  def getSumSize(ctx: CompilationContext, params: List[(Boolean, Expression)]): Int = {
    params.map { case (_, expr) => getExpressionType(ctx, expr).size}.max
  }

  def assertSizesForMultiplication(ctx: CompilationContext, params: List[Expression], inPlace: Boolean): Unit = {
    assertAllArithmetic(ctx, params)
    //noinspection ZeroIndexToHead
    val lType = getExpressionType(ctx, params(0))
    val lSize = lType.size
    val rType = getExpressionType(ctx, params(1))
    val rSize = rType.size
    if (lSize != 1 && lSize != 2) {
      ctx.log.error("Long multiplication not supported", params.head.position)
    }
    if (rSize != 1 && rSize != 2) {
      ctx.log.error("Long multiplication not supported", params.head.position)
    }
    if (inPlace) {
      if (params.size > 2) {
        ctx.log.error("Too many arguments for *=", params.head.position)
      }
//      if (lSize == 2 && rSize == 1 && rType.isSigned) {
//        ctx.log.error("Signed multiplication not supported", params.head.position)
//      }
    } else {
//      if (lSize == 2 && rSize == 1 && rType.isSigned) {
//        ctx.log.error("Signed multiplication not supported", params.head.position)
//      }
//      if (rSize == 2 && lSize == 1 && lType.isSigned) {
//        ctx.log.error("Signed multiplication not supported", params.head.position)
//      }
    }
  }

  def assertSizesForDivision(ctx: CompilationContext, params: List[Expression], inPlace: Boolean): Unit = {
    assertAllArithmetic(ctx, params)
    //noinspection ZeroIndexToHead
    val lType = getExpressionType(ctx, params(0))
    val lSize = lType.size
    val rType = getExpressionType(ctx, params(1))
    val rSize = rType.size
    if (lSize != 1 && lSize != 2) {
      ctx.log.error("Long division not supported", params.head.position)
    }
    if (rSize != 1 && rSize != 2) {
      ctx.log.error("Long division not supported", params.head.position)
    }
    if (inPlace) {
      if (lSize == 2 && rSize == 1 && rType.isSigned) {
        ctx.log.error("Signed division not supported", params.head.position)
      }
    } else {
      if (lSize == 2 && rSize == 1 && rType.isSigned) {
        ctx.log.error("Signed division not supported", params.head.position)
      }
      if (rSize == 2 && lSize == 1 && lType.isSigned) {
        ctx.log.error("Signed division not supported", params.head.position)
      }
    }
  }

  def assertAllArithmeticBytes(msg: String, ctx: CompilationContext, params: List[Expression]): Unit = {
    assertAllArithmetic(ctx, params)
    if (params.exists { expr => getExpressionType(ctx, expr).size != 1 }) {
      ctx.log.fatal(msg, params.head.position)
    }
  }

  @inline
  def assertArithmeticBinary(ctx: CompilationContext, params: List[Expression]): (Expression, Expression, Int) = {
    assertAllArithmetic(ctx, params)
    assertBinary(ctx, params)
  }

  def assertBinary(ctx: CompilationContext, params: List[Expression]): (Expression, Expression, Int) = {
    if (params.length != 2) {
      ctx.log.fatal("sfgdgfsd", None)
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

  def assertArithmeticComparison(ctx: CompilationContext, params: List[Expression]): (Int, Boolean) = {
    assertAllArithmetic(ctx, params)
    assertComparison(ctx, params)
  }

  def assertBool(ctx: CompilationContext, fname: String, params: List[Expression], expectedParamCount: Int): Unit = {
    if (params.length != expectedParamCount) {
      ctx.log.error("Invalid number of parameters for " + fname, params.headOption.flatMap(_.position))
      return
    }
    params.foreach { param =>
      getExpressionType(ctx, param) match {
        case _: BooleanType =>
        case FatBooleanType =>
        case _=>
          ctx.log.fatal("Parameter should be boolean", param.position)
      }
    }
  }

  def assertBool(ctx: CompilationContext, fname: String, params: List[Expression]): Unit = {
    if (params.length < 2) {
      ctx.log.error("Invalid number of parameters for " + fname, params.headOption.flatMap(_.position))
      return
    }
    params.foreach { param =>
      getExpressionType(ctx, param) match {
        case _: BooleanType =>
        case FatBooleanType =>
        case t =>
          ctx.log.fatal(s"Parameter has type `${t.name}`, but it should be boolean", param.position)
      }
    }
  }

  def assertArithmeticAssignmentLike(ctx: CompilationContext, params: List[Expression]): (LhsExpression, Expression, Int) = {
    if (params.length != 2) {
      ctx.log.fatal("sfgdgfsd", None)
    }
    assertAllArithmetic(ctx, params)
    (params.head, params(1)) match {
      case (l: LhsExpression, r: Expression) =>
        val lsize = getExpressionType(ctx, l).size
        val rsize = getExpressionType(ctx, r).size
        if (lsize < rsize) {
          ctx.log.error("Left-hand-side expression is of smaller type than the right-hand-side expression", l.position)
        }
        (l, r, lsize)
      case (err: Expression, _) => ctx.log.fatal("Invalid left-hand-side expression", err.position)
    }
  }

  def isUpToOneVar(params: List[(Boolean, Expression)]): Boolean = {
    var count = 0
    params.foreach {
      case (false, VariableExpression(_)) => count += 1
      case (_, _: LiteralExpression) =>
      case (_, _: GeneratedConstantExpression) =>
      case _ => return false
    }
    count <= 1
  }

  def validateTypeCastAndGetSourceExpressionType(ctx: CompilationContext, typ: Type, params: List[Expression]): Type = {
    var failed = false
    if (typ.name == "pointer" && typ.name !="pointer" && !typ.isInstanceOf[PointerType]) {
      ctx.log.error("Cannot cast into pointer", params.headOption.flatMap(_.position))
      failed = true
    }
    if (params.length != 1) {
      ctx.log.error("Type casting should have exactly one argument", params.headOption.flatMap(_.position))
      failed = true
    }
    val sourceType = getExpressionType(ctx, params.head)
    if (typ.size != sourceType.size && !sourceType.isExplicitlyCastableTo(typ)) {
      ctx.log.error(s"Cannot cast a type ${sourceType.name} to an incompatible type ${typ.name} of different size", params.head.position)
      failed = true
    }
    sourceType
  }
}

object AbstractExpressionCompiler {
  @inline
  def getExpressionType(ctx: CompilationContext, expr: Expression): Type = {
    getExpressionTypeImpl(ctx.env, ctx.log, expr, loosely = false)
  }
  @inline
  def getExpressionTypeLoosely(ctx: CompilationContext, expr: Expression): Type = {
    getExpressionTypeImpl(ctx.env, ctx.log, expr, loosely = true)
  }

  @inline
  def getExpressionType(env: Environment, log: Logger, expr: Expression): Type = getExpressionTypeImpl(env, log, expr, loosely = false)

  @inline
  def getExpressionTypeLoosely(env: Environment, log: Logger, expr: Expression): Type = getExpressionTypeImpl(env, log, expr, loosely = true)

  def getExpressionTypeImpl(env: Environment, log: Logger, expr: Expression, loosely: Boolean): Type = {
    if (expr.typeCache ne null) return expr.typeCache
    val b = env.get[Type]("byte")
    val bool = env.get[Type]("bool$")
    val boolTrue = env.get[Type]("true$")
    val boolFalse= env.get[Type]("false$")
    def toType(x: Boolean): Type = if (x) boolTrue else boolFalse

    def toAllNumericConstants(exprs: List[Expression]): Option[List[Long]] = {
      for {
        maybeConstants <- Some(exprs.map(env.eval))
        if maybeConstants.forall(_.isDefined)
        constants = maybeConstants.map(_.get)
        if constants.forall(_.isInstanceOf[NumericConstant])
        numericConstants = constants.map(_.asInstanceOf[NumericConstant])
        maxSize = numericConstants.map(_.requiredSize).fold(1)(_ max _)
        mask = (1L << (8 * maxSize)) - 1
        signMask = ~mask
        signTest = if (signMask == 0) Long.MaxValue else signMask >> 1
        types = exprs.map(e => getExpressionTypeImpl(env, log, e, loosely))
        if types.forall(_.size.<(8))
        signednesses = types.flatMap {
          case d: DerivedPlainType => Some(d.isSigned)
          case _ => None
        }.toSet
        if signednesses.size < 2
        preserveSigns = signednesses.contains(true)
        values = numericConstants.map(n => if (preserveSigns && n.value.&(signTest) != 0) n.value.&(mask).|(signMask) else n.value.&(mask))
//        _ = println(s"$constants $types $signednesses $preserveSigns $mask $signMask $signTest $values")
      } yield values
    }
    def toAllBooleanConstants(exprs: List[Expression]): Option[List[Boolean]] = {
      for {
        types <- Some(exprs.map(e => getExpressionTypeImpl(env, log, e, loosely)))
        if types.forall(_.isInstanceOf[ConstantBooleanType])
        bools = types.map(_.asInstanceOf[ConstantBooleanType].value)
        if bools.nonEmpty
      } yield bools
    }

    def monotonous(values: List[Long], pred: (Long, Long) => Boolean): Boolean = values.init.zip(values.tail).forall(pred.tupled)

    val v = env.get[Type]("void")
    val w = env.get[Type]("word")
    val t: Type = expr match {
      case LiteralExpression(_, size) =>
        size match {
          case 1 => b
          case 2 => w
          case 3 => env.get[Type]("int24")
          case 4 => env.get[Type]("int32")
          case 5 => env.get[Type]("int40")
          case 6 => env.get[Type]("int48")
          case 7 => env.get[Type]("int56")
          case 8 => env.get[Type]("int64")
        }
      case ConstantArrayElementExpression(constant) =>
        (constant.quickSimplify match {
          case SubbyteConstant(_, _) => false -> 1
          case NumericConstant(v, s) => (v < 0) -> s
          case CompoundConstant(MathOperator.Minus, NumericConstant(_, ls), NumericConstant(_, rs)) => true -> (ls max rs)
        }) match {
          case (false, 1) => b
          case (true, 1) => env.get[Type]("sbyte")
          case (_, 2) => b
          case (_, 3) => env.get[Type]("int24")
          case (_, 4) => env.get[Type]("int32")
          case (_, 5) => env.get[Type]("int40")
          case (_, 6) => env.get[Type]("int48")
          case (_, 7) => env.get[Type]("int56")
          case (_, 8) => env.get[Type]("int64")
        }
      case GeneratedConstantExpression(_, typ) => typ
      case TextLiteralExpression(_) => env.get[Type]("pointer")
      case VariableExpression(name) =>
        if (loosely) {
          env.maybeGet[TypedThing](name) match {
            case Some(t) => t.typ
            case None =>
              if (name.endsWith(".lo") || name.endsWith(".hi")) {
                b
              } else if (name.endsWith(".addr")) {
                env.get[Type]("pointer")
              } else {
                log.error(s"TypedThing `$name` is not defined", expr.position)
                b
              }
          }
        } else {
          env.get[TypedThing](name, expr.position).typ
        }
      case HalfWordExpression(param, _) =>
        getExpressionTypeImpl(env, log, param, loosely)
        b
      case IndexedExpression(name, _) =>
        env.getPointy(name).elementType
      case DerefDebuggingExpression(_, 1) => b
      case DerefDebuggingExpression(_, 2) => w
      case DerefExpression(_, _, _, typ) => typ
      case IndirectFieldExpression(inner, firstIndices, fieldPath) =>
        var currentType = inner match {
          case VariableExpression(arrName) =>
            env.maybeGet[Thing](arrName + ".array") match {
              case Some(a: MfArray) =>
                env.get[Type]("pointer." + a.elementType)
              case _ =>
                getExpressionTypeImpl(env, log, inner, loosely)
            }
          case _ => getExpressionTypeImpl(env, log, inner, loosely)
        }
        var ok = true
        for(_ <- firstIndices) {
          currentType match {
            case PointerType(_, _, Some(targetType)) =>
              currentType = targetType
            case x if x.isPointy =>
              currentType = b
            case _ =>
              log.error(s"Type `$currentType` is not a pointer type", expr.position)
              ok = false
          }
        }
        for ((dot, fieldName, indices) <- fieldPath) {
          if (dot && ok) {
            fieldName match {
              case "addr" => currentType = env.get[Type]("pointer")
              case "pointer" => currentType = env.get[Type]("pointer." + currentType.name)
              case "addr.hi" => currentType = b
              case "addr.lo" => currentType = b
              case "pointer.hi" => currentType = b
              case "pointer.lo" => currentType = b
              case _ =>
                log.error(s"Unexpected subfield `$fieldName`", expr.position)
                ok = false
            }
          } else if (ok) {
            val (actualFieldName, pointerWrap): (String, Int) = getActualFieldNameAndPointerWrap(fieldName)
            currentType match {
              case PointerType(_, _, Some(targetType)) =>
                val tuples = env.getSubvariables(targetType).filter(x => x.suffix == "." + actualFieldName)
                if (tuples.isEmpty) {
                  log.error(s"Type `$targetType` doesn't have field named `$actualFieldName`", expr.position)
                  ok = false
                } else {
                  val subv = tuples.head
                  pointerWrap match {
                    case 0 =>
                      currentType = if (subv.arrayIndexTypeAndSize.isDefined) env.get[Type]("pointer." + subv.typ) else subv.typ
                    case 1 =>
                      currentType = env.get[Type]("pointer." + subv.typ)
                    case 2 =>
                      currentType = env.get[Type]("pointer")
                    case 10 | 11 =>
                      currentType = b
                    case _ => throw new IllegalStateException
                  }
                }
              case _ =>
                log.error(s"Type `$currentType` is not a pointer type", expr.position)
                ok = false
            }
          }
          if (ok) {
            for (_ <- indices) {
              currentType match {
                case PointerType(_, _, Some(targetType)) =>
                  currentType = targetType
                case x if x.isPointy =>
                  currentType = b
                case _ =>
                  log.error(s"Type `$currentType` is not a pointer type", expr.position)
                  ok = false
              }
            }
          }
        }
        if (ok) currentType else b
      case SeparateBytesExpression(hi, lo) =>
        if (getExpressionTypeImpl(env, log, hi, loosely).size > 1) log.error("Hi byte too large", hi.position)
        if (getExpressionTypeImpl(env, log, lo, loosely).size > 1) log.error("Lo byte too large", lo.position)
        w
      case SumExpression(params, _) =>
        val paramTypes = params.map { case (_, e) => getExpressionTypeImpl(env, log, e, loosely) }
        val anySigned = paramTypes.exists(_.isSigned)
        val anyUnsigned = paramTypes.exists {
          case p: DerivedPlainType => !p.isSigned
          case _ => false
        }
        paramTypes.map(_.size).max match {
          case 1 =>
            params match {
              case List((false, LiteralExpression(0, _)), (true, _)) if !anyUnsigned => env.get[Type]("sbyte")
              case _ => if (anySigned && !anyUnsigned) env.get[Type]("sbyte") else b
            }
          case 2 =>
            params match {
              case List((false, LiteralExpression(0, _)), (true, _)) if !anyUnsigned => env.get[Type]("signed16")
              case _ => if (anySigned && !anyUnsigned) env.get[Type]("signed16") else w
            }
          case 3 => env.get[Type]("int24")
          case 4 => env.get[Type]("int32")
          case 0 => b
          case _ => log.error("Adding values bigger than longs", expr.position); env.get[Type]("int32")
        }
      case FunctionCallExpression("nonet", _) => w
      case FunctionCallExpression("not", params) =>
        toAllBooleanConstants(params) match {
          case Some(List(x)) => toType(!x)
          case _ => bool
        }
      case f@FunctionCallExpression("call", params) =>
        params match {
          case List(fp) =>
            getExpressionTypeImpl(env, log, fp, loosely) match {
              case fpt@FunctionPointerType(_, _, _, Some(v), Some(r)) =>
                if (v.name != "void"){
                  log.error(s"Invalid function pointer type: $fpt", fp.position)
                }
                r
              case KernalInterruptPointerType =>
                v
              case fpt =>
                log.error(s"Not a function pointer type: $fpt", fp.position)
                v
            }
          case List(fp, pp) =>
            getExpressionTypeImpl(env, log, fp, loosely) match {
              case fpt@FunctionPointerType(_, _, _, Some(p), Some(r)) =>
                if (!getExpressionTypeImpl(env, log, pp, loosely).isAssignableTo(p)){
                  log.error(s"Invalid function pointer type: $fpt", fp.position)
                }
                r
              case fpt@KernalInterruptPointerType =>
                log.error(s"Invalid function pointer type: $fpt", fp.position)
                v
              case fpt =>
                log.error(s"Not a function pointer type: $fpt", fp.position)
                v
            }
          case _ =>
            log.error("Invalid call(...) syntax; use either 1 or 2 arguments", f.position)
            v
        }
      case FunctionCallExpression("hi", _) => b
      case FunctionCallExpression("lo", _) => b
      case FunctionCallExpression("sin", params) => if (params.size < 2) b else getExpressionTypeImpl(env, log, params(1), loosely)
      case FunctionCallExpression("cos", params) => if (params.size < 2) b else getExpressionTypeImpl(env, log, params(1), loosely)
      case FunctionCallExpression("tan", params) => if (params.size < 2) b else getExpressionTypeImpl(env, log, params(1), loosely)
      case FunctionCallExpression(name@("min" | "max"), params) => if (params.isEmpty) b else params.map { e => getExpressionTypeImpl(env, log, e, loosely).size }.max match {
        case 1 => b
        case 2 => w
        case 0 => log.error(s"Invalid parameters to $name", expr.position); b
        case n if n >= 3 => env.get[Type]("int" + n * 8)
      } // TODO: ?
      case FunctionCallExpression("if", params) => if (params.length < 3) b else params.tail.map { e => getExpressionTypeImpl(env, log, e, loosely).size }.max match {
        case 1 => b
        case 2 => w
        case 0 => log.error(s"Invalid parameters to if", expr.position); b
        case n if n >= 3 => env.get[Type]("int" + n * 8)
      } // TODO: ?
      case FunctionCallExpression("sizeof", params) => env.evalSizeof(params.head).requiredSize match {
        case 1 => b
        case 2 => w
      }
      case FunctionCallExpression("typeof", params) => w
      case FunctionCallExpression("%%", params) => params.map { e => getExpressionTypeImpl(env, log, e, loosely).size } match {
        case List(1, 1) | List(2, 1) => b
        case List(1, 2) | List(2, 2) => w
        case List(0, _) | List(_, 0) => b
        case List(n, _) if n >= 3 => env.get[Type]("int" + n * 8)
        case _ => log.error(s"Invalid parameters to %%", expr.position); w
      }
      case FunctionCallExpression(op@("*"), params) =>
        val paramTypes = params.map { e => getExpressionTypeImpl(env, log, e, loosely) }
        val signed = paramTypes.exists(_.isSigned)
        val unsigned = paramTypes.exists{
          case t: DerivedPlainType => !t.isSigned
          case _ => false
        }
        if (signed && unsigned) {
          log.error("Mixing signed and explicitly unsigned types in multiplication", expr.position)
        }
        paramTypes.map(_.size).max match {
          case 0 | 1 => if (signed) env.get[Type]("sbyte") else b
          case 2 => if (signed) env.get[Type]("signed16") else w
          case n if n >= 3 => env.get[Type]("int" + n * 8)
          case _ => log.error(s"Invalid parameters to " + op, expr.position); w
        }
      case FunctionCallExpression(op@("|" | "&" | "^" | "/"), params) => params.map { e => getExpressionTypeImpl(env, log, e, loosely).size }.max match {
        case 0 | 1 => b
        case 2 => w
        case n if n >= 3 => env.get[Type]("int" + n * 8)
        case _ => log.error(s"Invalid parameters to " + op, expr.position); w
      }
      case FunctionCallExpression("<<", List(a1, a2)) =>
        if (getExpressionTypeImpl(env, log, a2, loosely).size > 1) log.error("Shift amount too large", a2.position)
        getExpressionTypeImpl(env, log, a1, loosely)
      case FunctionCallExpression(">>", List(a1, a2)) =>
        if (getExpressionTypeImpl(env, log, a2, loosely).size > 1) log.error("Shift amount too large", a2.position)
        getExpressionTypeImpl(env, log, a1, loosely)
      case FunctionCallExpression("*'", _) => b
      case FunctionCallExpression("<<'", _) => b
      case FunctionCallExpression(">>'", _) => b
      case FunctionCallExpression(">>>>", _) => b
      case FunctionCallExpression("&&", params) =>
        toAllBooleanConstants(params).fold(bool)(list => toType(list.reduce(_ && _)))
      case FunctionCallExpression("||", params) =>
        toAllBooleanConstants(params).fold(bool)(list => toType(list.reduce(_ || _)))
      case FunctionCallExpression("^^", params) =>
        toAllBooleanConstants(params).fold(bool)(list => toType(list.reduce(_ != _)))
      case FunctionCallExpression("==", params) =>
        toAllNumericConstants(params).fold(bool)(list => toType(monotonous(list, _ == _)))
      case FunctionCallExpression("!=", params) =>
        toAllNumericConstants(params) match {
          case Some(List(x, y)) => toType(x != y)
          case _ => bool
        }
      case FunctionCallExpression("<", params) =>
        toAllNumericConstants(params).fold(bool)(list => toType(monotonous(list, _ < _)))
      case FunctionCallExpression(">", params) =>
        toAllNumericConstants(params).fold(bool)(list => toType(monotonous(list, _ > _)))
      case FunctionCallExpression("<=", params) =>
        toAllNumericConstants(params).fold(bool)(list => toType(monotonous(list, _ <= _)))
      case FunctionCallExpression(">=", params) =>
        toAllNumericConstants(params).fold(bool)(list => toType(monotonous(list, _ >= _)))
      case FunctionCallExpression("+=", _) => v
      case FunctionCallExpression("-=", _) => v
      case FunctionCallExpression("*=", _) => v
      case FunctionCallExpression("/=", _) => v
      case FunctionCallExpression("%%=", _) => v
      case FunctionCallExpression("+'=", _) => v
      case FunctionCallExpression("-'=", _) => v
      case FunctionCallExpression("*'=", _) => v
      case FunctionCallExpression("|=", _) => v
      case FunctionCallExpression("&=", _) => v
      case FunctionCallExpression("^=", _) => v
      case FunctionCallExpression("<<=", _) => v
      case FunctionCallExpression(">>=", _) => v
      case FunctionCallExpression("<<'=", _) => v
      case FunctionCallExpression(">>'=", _) => v
      case f@FunctionCallExpression(name, _) =>
        env.maybeGet[Type](name) match {
          case Some(typ) =>
            typ
          case None =>
            lookupFunction(env, log, f).returnType
        }
    }
    expr.typeCache = t
    t
  }

  def getActualFieldNameAndPointerWrap(fieldName: String): (String, Int) = {
    if (fieldName.endsWith(".pointer")) {
      fieldName.stripSuffix(".pointer") -> 1
    } else if (fieldName.endsWith(".addr")) {
      fieldName.stripSuffix(".addr") -> 2
    } else if (fieldName.endsWith(".addr.hi")) {
      fieldName.stripSuffix(".addr.hi") -> 11
    } else if (fieldName.endsWith(".pointer.hi")) {
      fieldName.stripSuffix(".pointer.hi") -> 11
    } else if (fieldName.endsWith(".addr.lo")) {
      fieldName.stripSuffix(".addr.lo") -> 10
    } else if (fieldName.endsWith(".pointer.lo")) {
      fieldName.stripSuffix(".pointer.lo") -> 10
    } else {
      fieldName -> 0
    }
  }

  def checkIndexType(ctx: CompilationContext, pointy: Pointy, index: Expression): Unit = {
    val indexerType = getExpressionType(ctx, index)
    if (!indexerType.isAssignableTo(pointy.indexType)) {
      ctx.log.error(s"Invalid type for index${pointy.name.fold("")(" for `" + _ + "`")}: expected `${pointy.indexType}`, got `$indexerType`", index.position)
    }
  }

  def checkAssignmentTypeAndGetSourceType(ctx: CompilationContext, source: Expression, target: LhsExpression): Type = {
    val sourceType = getExpressionType(ctx, source)
    if (target == BlackHoleExpression) return sourceType
    val targetType = getExpressionType(ctx, target)
    if (!sourceType.isAssignableTo(targetType)) {
      ctx.log.error(s"Cannot assign `$sourceType` to `$targetType`", target.position.orElse(source.position))
    }
    sourceType
  }

  def checkAssignmentType(ctx: CompilationContext, source: Expression, targetType: Type): Unit = {
    val sourceType = getExpressionType(ctx, source)
    if (!sourceType.isAssignableTo(targetType)) {
      ctx.log.error(s"Cannot assign `$sourceType` to `$targetType`", source.position)
    }
  }

  def checkAssignmentTypeLoosely(env: Environment, source: Expression, targetType: Type): Unit = {
    val sourceType = getExpressionTypeLoosely(env, env.log, source)
    if (!sourceType.isAssignableTo(targetType)) {
      env.log.error(s"Cannot assign `$sourceType` to `$targetType`", source.position)
    }
  }

  def checkAssignmentType(env: Environment, source: Expression, targetType: Type): Unit = {
    val sourceType = getExpressionType(env, env.log, source)
    if (!sourceType.isAssignableTo(targetType)) {
      env.log.error(s"Cannot assign `$sourceType` to `$targetType`", source.position)
    }
  }

  def lookupFunction(env: Environment, log: Logger, f: FunctionCallExpression): MangledFunction = {
    val paramsWithTypes = f.expressions.map(x => getExpressionType(env, log, x) -> x)
    env.lookupFunction(f.functionName, paramsWithTypes).getOrElse {
      if (Environment.constOnlyBuiltinFunction(f.functionName)){
        log.error(s"Cannot use function `${f.functionName}` with non-constant params `${paramsWithTypes.map(_._1).mkString("(", ",", ")")}`", f.position)
      } else {
        log.error(s"Cannot find function `${f.functionName}` with given params `${paramsWithTypes.map(_._1).mkString("(", ",", ")")}`", f.position)
      }
      val signature = NormalParamSignature(paramsWithTypes.map { case (t, _) =>
        UninitializedMemoryVariable("?", t, VariableAllocationMethod.Auto, None, Set.empty, NoAlignment, isVolatile = false)
      })
      ExternFunction(f.functionName, NullType, signature, Constant.Zero, env, Set.empty, None)
    }
  }
}
