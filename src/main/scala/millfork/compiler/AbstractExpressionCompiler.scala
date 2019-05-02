package millfork.compiler

import millfork.env._
import millfork.node._
import millfork.error.{ConsoleLogger, Logger}
import millfork.assembly.AbstractCode

/**
  * @author Karol Stasiak
  */
class AbstractExpressionCompiler[T <: AbstractCode] {

  def getExpressionType(ctx: CompilationContext, expr: Expression): Type = AbstractExpressionCompiler.getExpressionType(ctx, expr)

  def assertAllArithmetic(ctx: CompilationContext,expressions: List[Expression]) = {
     for(e <- expressions) {
       val typ = getExpressionType(ctx, e)
       if (!typ.isArithmetic) {
         ctx.log.error(s"Cannot perform arithmetic operations on type `$typ`", e.position)
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

  def getArithmeticParamMaxSize(ctx: CompilationContext, params: List[Expression]): Int = {
    assertAllArithmetic(ctx, params)
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
    if (inPlace) {
      if (lSize != 1 && lSize != 2) {
        ctx.log.error("Long multiplication not supported", params.head.position)
      }
      if (rSize != 1) {
        ctx.log.error("Long multiplication not supported", params.head.position)
      }
      if (lSize == 2 && rType.isSigned) {
        ctx.log.error("Signed multiplication not supported", params.head.position)
      }
    } else {
      if (lSize > 2 || rSize > 2 || lSize + rSize > 3) {
        ctx.log.error("Signed multiplication not supported", params.head.position)
      }
      if (lSize == 2 && rType.isSigned) {
        ctx.log.error("Signed multiplication not supported", params.head.position)
      }
      if (rSize == 2 && lType.isSigned) {
        ctx.log.error("Signed multiplication not supported", params.head.position)
      }
      if (lSize + rSize > 2) {
        if (params.size != 2) {
          ctx.log.error("Cannot multiply more than 2 large numbers at once", params.headOption.flatMap(_.position))
          return
        }
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
      if (!getExpressionType(ctx, param).isInstanceOf[BooleanType])
        ctx.log.fatal("Parameter should be boolean", param.position)
    }
  }

  def assertBool(ctx: CompilationContext, fname: String, params: List[Expression]): Unit = {
    if (params.length < 2) {
      ctx.log.error("Invalid number of parameters for " + fname, params.headOption.flatMap(_.position))
      return
    }
    params.foreach { param =>
      if (!getExpressionType(ctx, param).isInstanceOf[BooleanType])
        ctx.log.fatal("Parameter should be boolean", param.position)
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
    if (typ.name == "pointer") {
      ctx.log.error("Cannot cast into pointer")
      failed = true
    }
    if (params.length != 1) {
      ctx.log.error("Type casting should have exactly one argument")
      failed = true
    }
    val sourceType = getExpressionType(ctx, params.head)
    if (typ.size != sourceType.size && !sourceType.isAssignableTo(typ)) {
      ctx.log.error("Cannot cast a type to an incompatible type of different size")
      failed = true
    }
    sourceType
  }
}

object AbstractExpressionCompiler {
  @inline
  def getExpressionType(ctx: CompilationContext, expr: Expression): Type = {
    getExpressionType(ctx.env, ctx.log, expr)
  }

  def getExpressionType(env: Environment, log: Logger, expr: Expression): Type = {
    if (expr.typeCache ne null) expr.typeCache
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
        types = exprs.map(e => getExpressionType(env, log, e))
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
        types <- Some(exprs.map(e => getExpressionType(env, log, e)))
        if types.forall(_.isInstanceOf[ConstantBooleanType])
        bools = types.map(_.asInstanceOf[ConstantBooleanType].value)
        if bools.nonEmpty
      } yield bools
    }

    def monotonous(values: List[Long], pred: (Long, Long) => Boolean): Boolean = values.init.zip(values.tail).forall(pred.tupled)

    val v = env.get[Type]("void")
    val w = env.get[Type]("word")
    val t = expr match {
      case LiteralExpression(value, size) =>
        size match {
          case 1 => b
          case 2 => w
          case 3 => env.get[Type]("farword")
          case 4 => env.get[Type]("long")
        }
      case GeneratedConstantExpression(c, t) => t
      case TextLiteralExpression(_) => env.get[Type]("pointer")
      case VariableExpression(name) =>
        env.get[TypedThing](name, expr.position).typ
      case HalfWordExpression(param, _) =>
        getExpressionType(env, log, param)
        b
      case IndexedExpression(name, _) =>
        env.getPointy(name).elementType
      case DerefDebuggingExpression(_, 1) => b
      case DerefDebuggingExpression(_, 2) => w
      case DerefExpression(_, _, typ) => typ
      case IndirectFieldExpression(inner, firstIndices, fieldPath) =>
        var currentType = getExpressionType(env, log, inner)
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
        for ((fieldName, indices) <- fieldPath) {
          if (ok) {
            currentType match {
              case PointerType(_, _, Some(targetType)) =>
                val tuples = env.getSubvariables(targetType).filter(x => x._1 == "." + fieldName)
                if (tuples.isEmpty) {
                  log.error(s"Type `$targetType` doesn't have field named `$fieldName`", expr.position)
                  ok = false
                } else {
                  currentType = tuples.head._3
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
        if (getExpressionType(env, log, hi).size > 1) log.error("Hi byte too large", hi.position)
        if (getExpressionType(env, log, lo).size > 1) log.error("Lo byte too large", lo.position)
        w
      case SumExpression(params, _) => params.map { case (_, e) => getExpressionType(env, log, e).size }.max match {
        case 1 => b
        case 2 => w
        case _ => log.error("Adding values bigger than words", expr.position); w
      }
      case FunctionCallExpression("nonet", params) => w
      case FunctionCallExpression("not", params) =>
        toAllBooleanConstants(params) match {
          case Some(List(x)) => toType(!x)
          case _ => bool
        }
      case FunctionCallExpression("hi", params) => b
      case FunctionCallExpression("lo", params) => b
      case FunctionCallExpression("sin", params) => if (params.size < 2) b else getExpressionType(env, log, params(1))
      case FunctionCallExpression("cos", params) => if (params.size < 2) b else getExpressionType(env, log, params(1))
      case FunctionCallExpression("tan", params) => if (params.size < 2) b else getExpressionType(env, log, params(1))
      case FunctionCallExpression("sizeof", params) => env.evalSizeof(params.head).requiredSize match {
        case 1 => b
        case 2 => w
      }
      case FunctionCallExpression("*" | "|" | "&" | "^", params) => params.map { e => getExpressionType(env, log, e).size }.max match {
        case 1 => b
        case 2 => w
        case _ => log.error("Combining values bigger than words", expr.position); w
      }
      case FunctionCallExpression("<<", List(a1, a2)) =>
        if (getExpressionType(env, log, a2).size > 1) log.error("Shift amount too large", a2.position)
        getExpressionType(env, log, a1)
      case FunctionCallExpression(">>", List(a1, a2)) =>
        if (getExpressionType(env, log, a2).size > 1) log.error("Shift amount too large", a2.position)
        getExpressionType(env, log, a1)
      case FunctionCallExpression("<<'", params) => b
      case FunctionCallExpression(">>'", params) => b
      case FunctionCallExpression(">>>>", params) => b
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

  def checkIndexType(ctx: CompilationContext, pointy: Pointy, index: Expression): Unit = {
    val indexerType = getExpressionType(ctx, index)
    if (!indexerType.isAssignableTo(pointy.indexType)) {
      ctx.log.error(s"Invalid type for index${pointy.name.fold("")(" for `" + _ + "`")}: expected `${pointy.indexType}`, got `$indexerType`", index.position)
    }
  }

  def checkAssignmentTypeAndGetSourceType(ctx: CompilationContext, source: Expression, target: LhsExpression): Type = {
    val sourceType = getExpressionType(ctx, source)
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

  def checkAssignmentType(env: Environment, source: Expression, targetType: Type): Unit = {
    val sourceType = getExpressionType(env, env.log, source)
    if (!sourceType.isAssignableTo(targetType)) {
      env.log.error(s"Cannot assign `$sourceType` to `$targetType`", source.position)
    }
  }

  def lookupFunction(env: Environment, log: Logger, f: FunctionCallExpression): MangledFunction = {
    val paramsWithTypes = f.expressions.map(x => getExpressionType(env, log, x) -> x)
    env.lookupFunction(f.functionName, paramsWithTypes).getOrElse(
      log.fatal(s"Cannot find function `${f.functionName}` with given params `${paramsWithTypes.map(_._1).mkString("(", ",", ")")}`", f.position))
  }
}
