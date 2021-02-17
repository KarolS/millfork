package millfork.compiler

import millfork.{CompilationFlag, CpuFamily, node}
import millfork.env._
import millfork.node._
import AbstractExpressionCompiler.getExpressionType
import millfork.compiler.AbstractStatementPreprocessor.hiddenEffectFreeFunctions

import scala.collection.immutable
import scala.collection.mutable.ListBuffer

/**
  * @author Karol Stasiak
  */
abstract class AbstractStatementPreprocessor(protected val ctx: CompilationContext, statements: List[ExecutableStatement]) {
  type VV = Map[String, Constant]
  protected val optimize = true // TODO
  protected val env: Environment = ctx.env
  protected val localPrefix: String = ctx.function.name + "$"
  protected val usedIdentifiers: immutable.Iterable[String] with (Nothing => Any) = if (optimize) statements.flatMap(_.getAllExpressions).flatMap(_.getAllIdentifiers) else Set()
  protected val trackableVars: Set[String] = if (optimize) {
    env.getAllLocalVariables
      .filterNot(_.typ.isSigned) // sadly, tracking loses signedness
      .map(_.name.stripPrefix(localPrefix))
      .filterNot(_.contains("."))
      .filterNot(_.contains("$"))
      .filterNot { vname =>
        val prefix = vname + "."
        usedIdentifiers.exists(_.startsWith(prefix))
      }.toSet
  } else Set() // TODO
  if (ctx.log.traceEnabled && trackableVars.nonEmpty) {
    ctx.log.trace("Tracking local variables: " + trackableVars.mkString(", "))
  }
  protected val reentrantVars: Set[String] = trackableVars.filter(v => env.get[Variable](v) match {
    case _: StackVariable => true
    case v:UninitializedMemoryVariable if v.alloc == VariableAllocationMethod.Auto => ctx.options.flag(CompilationFlag.DangerousOptimizations)
    case _ => false
  })
  protected val nonreentrantVars: Set[String] = trackableVars -- reentrantVars

  protected val optimizeStdlib: Boolean = ctx.options.flag(CompilationFlag.OptimizeStdlib)

  def apply(): List[ExecutableStatement] = {
    optimizeStmts(statements, Map())._1
  }

  def optimizeStmts(stmts: Seq[ExecutableStatement], currentVarValues: VV): (List[ExecutableStatement], VV) = {
    val result = ListBuffer[ExecutableStatement]()
    var cv = currentVarValues
    for(stmt <- stmts){
      val p = optimizeStmt(stmt, cv)
      result += p._1
      cv = p._2
    }
    result.toList -> cv
  }

  def maybeOptimizeForStatement(f: ForStatement): Option[(ExecutableStatement, VV)]

  def isNonzero(index: Expression): Boolean = env.eval(index) match {
    case Some(c) => !c.isProvablyZero
    case _ => true
  }

  def isWordPointy(name: String): Boolean = {
    env.getPointy(name).elementType.alignedSize == 2
  }

  def optimizeStmt(stmt: ExecutableStatement, currentVarValues: VV): (ExecutableStatement, VV) = {
    var cv = currentVarValues
    val pos = stmt.position
    // stdlib:
    if (optimizeStdlib) {
      stmt match {
        case ExpressionStatement(FunctionCallExpression("putstrz", List(TextLiteralExpression(text))))
          if StdLibOptUtils.isValidNulTerminated(ctx.options.platform.defaultCodec, text) =>
            text.size match {
              case 1 =>
                ctx.log.debug("Removing putstrz with empty argument", stmt.position)
                return EmptyStatement(Nil) -> currentVarValues
              case 2 =>
                ctx.log.debug("Replacing putstrz with putchar", stmt.position)
                return ExpressionStatement(FunctionCallExpression("putchar", List(text.head))) -> currentVarValues
              case 3 =>
                if (ctx.options.platform.cpuFamily == CpuFamily.M6502) {
                  ctx.log.debug("Replacing putstrz with putchar", stmt.position)
                  return IfStatement(FunctionCallExpression("==", List(LiteralExpression(1, 1), LiteralExpression(1, 1))), List(
                    ExpressionStatement(FunctionCallExpression("putchar", List(text.head))),
                    ExpressionStatement(FunctionCallExpression("putchar", List(text(1))))
                  ), Nil) -> currentVarValues
                }
              case _ =>
            }
        case ExpressionStatement(FunctionCallExpression("putpstr", List(TextLiteralExpression(text))))
          if StdLibOptUtils.isValidPascal(text) =>
            text.length match {
              case 1 =>
                ctx.log.debug("Removing putpstr with empty argument", stmt.position)
                return EmptyStatement(Nil) -> currentVarValues
              case 2 =>
                ctx.log.debug("Replacing putpstr with putchar", stmt.position)
                return ExpressionStatement(FunctionCallExpression("putchar", List(text(1)))) -> currentVarValues
              case _ =>
            }
        case _ =>
      }
    }
    // generic warnings:
    stmt match {
      case ExpressionStatement(expr@FunctionCallExpression("strzlen" | "putstrz" | "strzcmp" | "strzcopy" | "strzpaste", params)) =>
        for (param <- params) checkIfNullTerminated(ctx, stmt, param)
      case ExpressionStatement(expr@FunctionCallExpression("pstrlen" | "putpstr" | "pstrcmp" | "pstrcopy" | "pstrpaste", params)) =>
        for (param <- params) checkIfLengthPrefixed(ctx, stmt, param)

      case ExpressionStatement(expr@FunctionCallExpression(f, List(VariableExpression(v)))) if hiddenEffectFreeFunctions(f)=>
        val volatile = ctx.env.maybeGet[ThingInMemory](v).fold(false)(_.isVolatile)
        if (!volatile && ctx.options.flag(CompilationFlag.UselessCodeWarning)) {
          ctx.log.warn("Pointless expression.", stmt.position)
        }

      case ExpressionStatement(expr@FunctionCallExpression(f, List(_: LiteralExpression))) if hiddenEffectFreeFunctions(f) =>
        if (ctx.options.flag(CompilationFlag.UselessCodeWarning)) {
          ctx.log.warn("Pointless expression.", stmt.position)
        }

      case ExpressionStatement(VariableExpression(v)) =>
        val volatile = ctx.env.maybeGet[ThingInMemory](v).fold(false)(_.isVolatile)
        if (!volatile && ctx.options.flag(CompilationFlag.UselessCodeWarning)) {
          ctx.log.warn("Pointless expression.", stmt.position)
        }

      case ExpressionStatement(_: LiteralExpression) =>
        if (ctx.options.flag(CompilationFlag.UselessCodeWarning)) {
          ctx.log.warn("Pointless expression.", stmt.position)
        }

      case WhileStatement(condition, Nil, Nil, _) =>
        if (!env.isGoodEmptyLoopCondition(condition)) {
          ctx.log.warn("Empty loop with a non-volatile condition may be optimized into a no-op or an infinite loop.", stmt.position)
        }

      case DoWhileStatement(Nil, Nil, condition, _) =>
        if (!env.isGoodEmptyLoopCondition(condition)) {
          ctx.log.warn("Empty loop with a non-volatile condition may be optimized into a no-op or an infinite loop.", stmt.position)
        }

      case _ =>
    }
    stmt match {
      case Assignment(ve@VariableExpression(v), arg) if trackableVars(v) =>
        cv = search(arg, cv)
        Assignment(ve, optimizeExpr(arg, cv)).pos(pos) -> (env.eval(optimizeExpr(arg, Map()), currentVarValues) match {
          case Some(c) => cv + (v -> c)
          case None => cv - v
        })
      case Assignment(target:DerefDebuggingExpression, arg) =>
        cv = search(arg, cv)
        cv = search(target, cv)
        Assignment(optimizeExpr(target, cv).asInstanceOf[LhsExpression], optimizeExpr(arg, cv)).pos(pos) -> cv
      case Assignment(target:DerefExpression, arg) =>
        cv = search(arg, cv)
        cv = search(target, cv)
        Assignment(optimizeExpr(target, cv).asInstanceOf[LhsExpression], optimizeExpr(arg, cv)).pos(pos) -> cv
      case Assignment(target:IndirectFieldExpression, arg) =>
        cv = search(arg, cv)
        cv = search(target, cv)
        Assignment(optimizeExpr(target, cv).asInstanceOf[LhsExpression], optimizeExpr(arg, cv)).pos(pos) -> cv
      case Assignment(target:IndexedExpression, arg) if isWordPointy(target.name) =>
        cv = search(arg, cv)
        cv = search(target, cv)
        Assignment(DerefExpression(
          FunctionCallExpression("pointer", List(VariableExpression(target.name).pos(pos))).pos(pos) #+#
            FunctionCallExpression("<<", List(optimizeExpr(target.index, cv), LiteralExpression(1, 1))).pos(pos),
          0, env.getPointy(target.name).elementType).pos(pos), optimizeExpr(arg, cv)).pos(pos) -> cv
      case Assignment(target:IndexedExpression, arg) =>
        cv = search(arg, cv)
        cv = search(target, cv)
        Assignment(optimizeExpr(target, cv).asInstanceOf[LhsExpression], optimizeExpr(arg, cv)).pos(pos) -> cv
      case Assignment(ve, arg) =>
        cv = search(arg, cv)
        cv = search(ve, cv)
        Assignment(ve, optimizeExpr(arg, cv)).pos(pos) -> cv
      case ExpressionStatement(expr@FunctionCallExpression(fname, _))
        if ctx.env.maybeGet[Thing](fname).exists(i => i.isInstanceOf[MacroFunction]) =>
        ExpressionStatement(optimizeExpr(expr, Map())).pos(pos) -> Map()
      case ExpressionStatement(expr@FunctionCallExpression("+=", List(VariableExpression(v), arg)))
        if currentVarValues.contains(v) =>
        cv = search(arg, cv)
        ExpressionStatement(optimizeExpr(expr, cv - v)).pos(pos) -> (env.eval(expr, currentVarValues) match {
          case Some(c) => if (cv.contains(v)) cv + (v -> (cv(v) + c)) else cv
          case None => cv - v
        })
      case ExpressionStatement(expr@FunctionCallExpression(op, List(VariableExpression(v), arg)))
        if op.endsWith("=") && op != ">=" && op != "<=" && op != ":=" =>
        cv = search(arg, cv)
        ExpressionStatement(optimizeExpr(expr, cv - v)).pos(pos) -> (cv - v)
      case ExpressionStatement(expr) =>
        cv = search(expr, cv)
        ExpressionStatement(optimizeExpr(expr, cv)).pos(pos) -> cv
      case ReturnStatement(Some(expr)) =>
        cv = search(expr, cv)
        ReturnStatement(Some(optimizeExpr(expr, cv))).pos(pos) -> cv
      case IfStatement(cond, th, el) =>
        cv = search(cond, cv)
        val c = optimizeExpr(cond, cv)
        val (t, vt) = optimizeStmts(th, cv)
        val (e, ve) = optimizeStmts(el, cv)
        IfStatement(c, t, e).pos(pos) -> commonVV(vt, ve)
      case WhileStatement(cond, body, inc, labels) =>
        cv = search(cond, cv)
        val c = optimizeExpr(cond, Map())
        val (b, _) = optimizeStmts(body, Map())
        val (i, _) = optimizeStmts(inc, Map())
        WhileStatement(c, b, i, labels).pos(pos) -> Map()
      case DoWhileStatement(body, inc, cond, labels) =>
        val c = optimizeExpr(cond, Map())
        val (b, _) = optimizeStmts(body, Map())
        val (i, _) = optimizeStmts(inc, Map())
        DoWhileStatement(b, i, c, labels).pos(pos) -> Map()
      case f@ForEachStatement(v, pv, arr, body) =>
        for (a <- arr.right.getOrElse(Nil)) cv = search(a, cv)
        val a = arr.map(_.map(optimizeExpr(_, Map())))
        val (b, _) = optimizeStmts(body, Map())
        ForEachStatement(v, pv, a, b).pos(pos) -> Map()
      case f@ForStatement(v, st, en, dir, body, Nil) =>

        // detect a memset
        f.body match {
          case List(Assignment(target@IndexedExpression(pointy, index), source)) =>
            val sourceType = AbstractExpressionCompiler.getExpressionType(ctx, source)
            val targetType = AbstractExpressionCompiler.getExpressionType(ctx, target)
            if (
              !env.isVolatile(VariableExpression(pointy)) &&
              !env.isVolatile(index) &&
              !env.isVolatile(source) &&
              !env.isVolatile(f.end) &&
              index.getAllIdentifiers.forall(iv => !ctx.env.overlapsVariable(iv, source)) &&
              !env.overlapsVariable(pointy, source) &&
              !env.overlapsVariable(pointy, index) &&
              !env.overlapsVariable(f.variable, source) &&
              !env.overlapsVariable(f.variable, f.start) &&
              !env.overlapsVariable(f.variable, f.end) &&
              ctx.isConstant(source) &&
              sourceType.size <= 1 &&
              targetType.size == 1 &&
              sourceType.isAssignableTo(targetType)
            ) {
              val sizeExpr = f.direction match {
                case ForDirection.DownTo =>
                  f.start #-# f.end #+# LiteralExpression(1, 2)
                case ForDirection.To | ForDirection.ParallelTo =>
                  f.end #-# f.start #+# LiteralExpression(1, 2)
                case ForDirection.Until | ForDirection.ParallelUntil =>
                  f.end #-# f.start
              }
              val w = env.get[Type]("word")
              env.eval(sizeExpr) match {
                case Some(size) =>
                  val startOpt = optimizeExpr(f.start, Map())
                  val sourceOpt = optimizeExpr(source, Map())
                  (env.getPointy(pointy), env.evalVariableAndConstantSubParts(index)) match {
                    case (array: ConstantPointy, (Some(VariableExpression(i)), offset)) if i == f.variable =>
                      // for i,start,until,end { array[i+offset] = source }
                      // println(s"Detected memset via array $array and index $i")
                      return MemsetStatement(startOpt #+# GeneratedConstantExpression(array.value + offset, w), size, sourceOpt, f.direction, Some(f)).pos(pos) -> Map()
                    case (pointer, (Some(VariableExpression(i)), offset)) if i == f.variable =>
                      // for i,start,until,end { array[i+offset] = source }
                      // println(s"Detected memset via pointer $pointer and index $i")
                      return MemsetStatement(startOpt #+# VariableExpression(pointy) #+# GeneratedConstantExpression(offset, w), size, sourceOpt, f.direction, Some(f)).pos(pos) -> Map()
                    case (_, (None, offset)) if pointy == f.variable =>
                      // for pointy,start,until,end { pointy[offset] = source }
                      // println(s"Detected memset via pointer $pointy alone")
                      return MemsetStatement(startOpt #+# GeneratedConstantExpression(offset, w), size, sourceOpt, f.direction, Some(f)).pos(pos) -> Map()
                    case _ =>
                  }
                case _ =>
              }
            }
          case _ =>
        }

        maybeOptimizeForStatement(f) match {
          case Some(x) => x
          case None =>
            val s = optimizeExpr(st, Map())
            val e = optimizeExpr(en, Map())
            val (b, _) = optimizeStmts(body, Map())
            ForStatement(v, s, e, dir, b).pos(pos) -> Map()
        }
      case f@ForStatement(v, st, en, dir, body, increment) =>
        val s = optimizeExpr(st, Map())
        val e = optimizeExpr(en, Map())
        val (b, _) = optimizeStmts(body, Map())
        val (i, _) = optimizeStmts(increment, Map())
        ForStatement(v, s, e, dir, b, i).pos(pos) -> Map()
      case _ => stmt -> Map()
    }
  }

  private def checkIfNullTerminated(ctx: CompilationContext, stmt: ExecutableStatement, param: Expression): Unit = {
    if (!ctx.options.flag(CompilationFlag.BuggyCodeWarning)) return
    val TERMINATOR = ctx.options.platform.defaultCodec.stringTerminator.head
    param match {
      case TextLiteralExpression(ch) =>
        ch.lastOption match {
          case Some(LiteralExpression(TERMINATOR, _)) => //ok
          case _ => ctx.log.warn("Passing a non-null-terminated string to a function that expects a null-terminated string.", stmt.position)
        }
      case _ =>
    }
  }

  private def checkIfLengthPrefixed(ctx: CompilationContext, stmt: ExecutableStatement, param: Expression): Unit = {
    if (!ctx.options.flag(CompilationFlag.BuggyCodeWarning)) return
    val TERMINATOR = ctx.options.platform.defaultCodec.stringTerminator.head
    param match {
      case TextLiteralExpression(ch) =>
        if (ch.headOption match {
          case Some(LiteralExpression(length, _)) if length == ch.size - 1 => false
          case Some(LiteralExpression(length, _)) if length == ch.size + 2 =>
            ch.lastOption match {
              case Some(LiteralExpression(TERMINATOR, _)) => false
              case _ => true
            }
          case _ => true
        }) {
          ctx.log.warn("Passing a non-length-prefixed string to a function that expects a length-prefixed string.", stmt.position)
        }
      case _ =>
    }
  }

  def search(expr: Expression, cv: VV): VV = {
    expr match {
      case FunctionCallExpression(op, List(VariableExpression(v), arg)) if op.endsWith("=") && op != "<=" && op != ">=" =>
        search(arg, cv - v)
      case FunctionCallExpression(name, params)
        if hiddenEffectFreeFunctions(name) || env.maybeGet[Type](name).isDefined =>
        if (params.isEmpty) cv // to handle compilation errors
        else params.map(p => search(p, cv)).reduce(commonVV)
      case FunctionCallExpression(_, _) => cv -- nonreentrantVars
      case SumExpression(params, _) => params.map(p => search(p._2, cv)).reduce(commonVV)
      case HalfWordExpression(arg, _) => search(arg, cv)
      case IndexedExpression(_, arg) => search(arg, cv)
      case DerefDebuggingExpression(arg, _) => search(arg, cv)
      case DerefExpression(arg, _, _) => search(arg, cv)
      case _ => cv // TODO
    }
  }

  def commonVV(a: VV, b: VV): VV = {
    if (a.isEmpty) return a
    if (b.isEmpty) return b
    val keys = a.keySet & b.keySet
    if (keys.isEmpty) return Map()
    keys.flatMap{ k =>
      val aa = a(k)
      val bb = b(k)
      if (aa == bb) Some(k -> aa) else None
    }.toMap
  }

  def isHiddenEffectFree(expr: Expression): Boolean = {
    expr match {
      case _: VariableExpression => true
      case _: LiteralExpression => true
      case _: ConstantArrayElementExpression => true
      case _: GeneratedConstantExpression => true
      case FunctionCallExpression(name, params) =>
        hiddenEffectFreeFunctions(name) && params.forall(isHiddenEffectFree)
      case _ => false // TODO
    }
  }

  def optimizeExpr(expr: Expression, currentVarValues: VV, optimizeSum: Boolean = false): Expression = {
    val pos = expr.position
    // stdlib:
    if (optimizeStdlib) {
      expr match {
        case FunctionCallExpression("strzlen", List(TextLiteralExpression(text))) =>
          StdLibOptUtils.evalStrzLen(ctx, expr, ctx.options.platform.defaultCodec , text).foreach(return _)
        case FunctionCallExpression("scrstrzlen", List(TextLiteralExpression(text))) =>
          StdLibOptUtils.evalStrzLen(ctx, expr, ctx.options.platform.defaultCodec, text).foreach(return _)
        case FunctionCallExpression("pstrlen", List(TextLiteralExpression(text))) =>
          StdLibOptUtils.evalPStrLen(ctx, expr, text).foreach(return _)
        case _ =>
      }
    }
    implicit class StringToFunctionNameOps(val functionName: String) {
      def <|(exprs: Expression*): Expression = FunctionCallExpression(functionName, exprs.toList).pos(exprs.head.position)
    }
    // generic warnings:
    expr match {
      case FunctionCallExpression("*" | "*=", params) =>
        if (ctx.options.flag(CompilationFlag.UselessCodeWarning) && params.exists {
          case LiteralExpression(0, _) => true
          case _ => false
        }) ctx.log.warn("Multiplication by zero.", params.head.position)
      case FunctionCallExpression("/" | "/=" | "%%" | "%%=", params) =>
        if (ctx.options.flag(CompilationFlag.BuggyCodeWarning) && params.tail.exists {
          case LiteralExpression(0, _) => true
          case _ => false
        }) ctx.log.warn("Division by zero.", params.head.position)
      case FunctionCallExpression("<<" | ">>" | "<<'" | "<<=" | ">>=" | "<<'=" | ">>>>", List(lhs@_, LiteralExpression(0, _))) =>
        if (ctx.options.flag(CompilationFlag.UselessCodeWarning)) ctx.log.warn("Shift by zero.", lhs.position)
      case _ =>
    }
    expr match {
      case IndirectFieldExpression(root, firstIndices, fieldPath) =>
        val b = env.get[Type]("byte")
        var ok = true
        var result = optimizeExpr(root, currentVarValues).pos(pos)
        def applyIndex(result: Expression, index: Expression): Expression = {
          AbstractExpressionCompiler.getExpressionType(env, env.log, result) match {
            case pt@PointerType(_, _, Some(target)) =>
              env.eval(index) match {
                case Some(NumericConstant(0, _)) => //ok
                case _ =>
                  // TODO: should we keep this?
                  env.log.error(s"Type `$pt` can be only indexed with 0")
              }
              DerefExpression(result, 0, target)
            case x if x.isPointy =>
              val (targetType, arraySizeInBytes) = result match {
                case VariableExpression(maybePointy) =>
                  val pointy = env.getPointy(maybePointy)
                  pointy.elementType -> (pointy match {
                    case p:ConstantPointy => p.sizeInBytes
                    case _ => None
                  })
                case _ => env.get[Type](x.pointerTargetName) -> None
              }
              ctx.log.trace(s"$result is $x and targets $targetType")
              env.eval(index) match {
                case Some(NumericConstant(n, _)) if n >= 0 && (targetType.alignedSize * n) <= 127 =>
                  x match {
                    case _: PointerType =>
                      DerefExpression(result, targetType.alignedSize * n.toInt, targetType)
                    case _ =>
                      DerefExpression(
                        ("pointer." + targetType.name) <| result,
                        targetType.alignedSize * n.toInt, targetType)
                  }
                case _ =>
                  val shifts = Integer.numberOfTrailingZeros(targetType.alignedSize)
                  val shrunkElementSize = targetType.alignedSize >> shifts
                  val shrunkArraySize = arraySizeInBytes.fold(9999)(_.>>(shifts))
                  val scaledIndex = arraySizeInBytes match {
                    // "n > targetType.alignedSize" means
                    // "don't do optimizations on arrays size 0 or 1"
                    case Some(n) if n > targetType.alignedSize && n <= 256 => targetType.alignedSize match {
                      case 1 => "byte" <| index
                      case 2 => "<<" <| ("byte" <| index, LiteralExpression(1, 1))
                      case 4 => "<<" <| ("byte" <| index, LiteralExpression(2, 1))
                      case 8 => "<<" <| ("byte" <| index, LiteralExpression(3, 1))
                      case _ => "*" <| ("byte" <| index, LiteralExpression(targetType.alignedSize, 1))
                    }
                    case Some(n) if n > targetType.alignedSize && n <= 512 && targetType.alignedSize == 2 =>
                      "nonet" <| ("<<" <| ("byte" <| index, LiteralExpression(1, 1)))
                    case Some(n) if n > targetType.alignedSize && n <= 512 && targetType.alignedSize == 2 =>
                      "nonet" <| ("<<" <| ("byte" <| index, LiteralExpression(1, 1)))
                    case Some(n) if n > targetType.alignedSize && shrunkArraySize <= 256 =>
                      "<<" <| ("word" <| ("*" <| ("byte" <| index, LiteralExpression(shrunkElementSize, 1))), LiteralExpression(shifts, 1))
                    case _ => targetType.alignedSize match {
                      case 1 => "word" <| index
                      case 2 => "<<" <| ("word" <| index, LiteralExpression(1, 1))
                      case 4 => "<<" <| ("word" <| index, LiteralExpression(2, 1))
                      case 8 => "<<" <| ("word" <| index, LiteralExpression(3, 1))
                      case _ => "*" <| ("word" <| index, LiteralExpression(targetType.alignedSize, 1))
                    }
                  }
                  // TODO: re-cast pointer type
                  DerefExpression(("pointer." + targetType.name) <| (
                    result #+# optimizeExpr(scaledIndex, Map())
                  ), 0, targetType)
              }
            case _ =>
              ctx.log.error("Not a pointer type on the left-hand side of `[`", pos)
              ok = false
              result
          }
        }

        for (index <- firstIndices) {
          result = applyIndex(result, index)
        }
        for ((dot, fieldName, indices) <- fieldPath) {
          if (dot && ok) {
            val pointer = result match {
              case DerefExpression(inner, 0, _) =>
                optimizeExpr(inner, currentVarValues).pos(pos)
              case DerefExpression(inner, offset, targetType) =>
                if (offset == 0) {
                  ("pointer." + targetType.name) <| ("pointer" <| optimizeExpr(inner, currentVarValues).pos(pos))
                } else {
                  ("pointer." + targetType.name) <| (
                    ("pointer" <| optimizeExpr(inner, currentVarValues).pos(pos)) #+# LiteralExpression(offset, 2)
                  )
                }
              case IndexedExpression(name, index) =>
                ctx.log.fatal("Oops!")
              case _ =>
                ok = false
                ctx.log.error(s"Not a left-hand-side expression", result.position)
                result
            }
            fieldName match {
              case "pointer" => result = pointer
              case "pointer.hi" => result = "hi" <| pointer
              case "pointer.lo" => result = "lo" <| pointer
              case "addr" => result = "pointer" <| pointer
              case "addr.hi" => result = "hi" <| pointer
              case "addr.lo" => result = "lo" <| pointer
              case _ =>
                ctx.log.error(s"Unexpected subfield `$fieldName`", result.position)
                ok = false
            }
          } else if (ok) {
            val (actualFieldName, pointerWrap): (String, Int) = AbstractExpressionCompiler.getActualFieldNameAndPointerWrap(fieldName)
            val currentResultType = AbstractExpressionCompiler.getExpressionType(env, env.log, result)
            result = currentResultType match {
              case PointerType(_, _, Some(target)) =>
                val subvariables = env.getSubvariables(target).filter(x => x.suffix == "." + actualFieldName)
                if (subvariables.isEmpty) {
                  ctx.log.error(s"Type `${target.name}` does not contain field `$actualFieldName`", result.position)
                  ok = false
                  LiteralExpression(0, 1)
                } else {
                  if (subvariables.head.arraySize.isDefined) ??? // TODO
                  val inner = optimizeExpr(result, currentVarValues, optimizeSum = true).pos(pos)
                  val fieldOffset = subvariables.head.offset
                  val fieldType = subvariables.head.typ
                  pointerWrap match {
                    case 0 =>
                      DerefExpression(inner, fieldOffset, fieldType)
                    case 1 =>
                      if (fieldOffset == 0) {
                        ("pointer." + fieldType.name) <| ("pointer" <| inner)
                      } else {
                        ("pointer." + fieldType.name) <| (
                          ("pointer" <| inner) #+# LiteralExpression(fieldOffset, 2)
                        )
                      }
                    case 2 =>
                      if (fieldOffset == 0) {
                        "pointer" <| inner
                      } else {
                        ("pointer" <| inner) #+# LiteralExpression(fieldOffset, 2)
                      }
                    case 10 =>
                      if (fieldOffset == 0) {
                        "lo" <| ("pointer" <| inner)
                      } else {
                        "lo" <| (
                          ("pointer" <| inner) #+# LiteralExpression(fieldOffset, 2)
                        )
                      }
                    case 11 =>
                      if (fieldOffset == 0) {
                        "hi" <| ("pointer" <| inner)
                      } else {
                        "hi" <| (
                           ("pointer" <| inner) #+# LiteralExpression(fieldOffset, 2)
                        )
                      }

                    case _ => throw new IllegalStateException
                  }
                }
              case _ =>
                ctx.log.error("Invalid pointer type on the left-hand side of `->`", result.position)
                ctx.log.debug(currentResultType.toString)
                LiteralExpression(0, 1)
            }
          }
          if (ok) {
            for (index <- indices) {
              result = applyIndex(result, index)
            }
          }
        }
        result
      case DerefDebuggingExpression(inner, 1) =>
        DerefExpression(optimizeExpr(inner, currentVarValues, optimizeSum = true), 0, env.get[VariableType]("byte")).pos(pos)
      case DerefDebuggingExpression(inner, 2) =>
        DerefExpression(optimizeExpr(inner, currentVarValues, optimizeSum = true), 0, env.get[VariableType]("word")).pos(pos)
      case e@TextLiteralExpression(characters) =>
        val name = ctx.env.getTextLiteralArrayName(e)
        VariableExpression(name).pos(pos)
      case VariableExpression(v) if currentVarValues.contains(v) =>
        val constant = currentVarValues(v)
        ctx.log.debug(s"Using node flow to replace $v with $constant", pos)
        GeneratedConstantExpression(constant, getExpressionType(ctx, expr)).pos(pos)
      case FunctionCallExpression(t1, List(FunctionCallExpression(t2, List(arg))))
        if optimize && pointlessDoubleCast(t1, t2, arg) =>
        ctx.log.debug(s"Pointless double cast $t1($t2(...))", pos)
        optimizeExpr(FunctionCallExpression(t1, List(arg)).pos(pos), currentVarValues)
      case FunctionCallExpression(t1, List(arg))
        if optimize && pointlessCast(t1, arg) =>
        ctx.log.debug(s"Pointless cast $t1(...)", pos)
        optimizeExpr(arg, currentVarValues)
      case FunctionCallExpression(op@("<<" | "<<'"), List(l, r)) =>
        env.eval(l) match {
          case Some(c) if c.isProvablyZero =>
            env.eval(r) match {
              case Some(rc) =>
                LiteralExpression(0, c.requiredSize)
              case _ =>
                FunctionCallExpression(op, List(optimizeExpr(l, currentVarValues), optimizeExpr(r, currentVarValues)))
            }
          case _ =>
            FunctionCallExpression(op, List(optimizeExpr(l, currentVarValues), optimizeExpr(r, currentVarValues)))
        }
      case FunctionCallExpression("nonet", args) =>
        // Eliminating variables may eliminate carry
        FunctionCallExpression("nonet", args.map(arg => optimizeExpr(arg, Map()))).pos(pos)
      case FunctionCallExpression(name, args) =>
        if (Environment.constOnlyBuiltinFunction(name)) {
          if (ctx.env.eval(expr).isEmpty) {
            ctx.log.error(s"`$name` should be only used with constant expressions", expr.position)
          }
        }
        ctx.env.maybeGet[Thing](name) match {
          case Some(_: MacroFunction) =>
            FunctionCallExpression(name, args.map(arg => optimizeExpr(arg, Map()))).pos(pos)
          case a =>
            if (ctx.options.flag(CompilationFlag.CallToOverlappingBankWarning)) {
              a match {
                case Some(f: FunctionInMemory) =>
                  val thisBank = ctx.function.bank(ctx.options)
                  val targetBank = f.bank(ctx.options)
                  if (ctx.options.platform.isUnsafeToJump(targetBank, thisBank)) {
                    ctx.log.warn(s"Unsafe call to function `${f.name}` in segment `$targetBank` from function `${ctx.function.name}` in overlapping segment `$thisBank`", expr.position)
                  }
                case _ =>
              }
            }
            FunctionCallExpression(name, args.map(arg => optimizeExpr(arg, currentVarValues))).pos(pos)
        }
      case SumExpression(expressions, false) if optimizeSum =>
        SumExpression(expressions.map{
          case (minus, arg) => minus -> optimizeExpr(arg, currentVarValues)
        }.filterNot{
          case (_, e) => env.eval(e).exists(_.isProvablyZero)
        }, decimal = false).pos(pos)
      case SumExpression(expressions, decimal) =>
        // don't collapse additions, let the later stages deal with it
        // expecially important when inside a nonet operation
        SumExpression(expressions.map{case (minus, arg) => minus -> optimizeExpr(arg, currentVarValues)}, decimal).pos(pos)
      case IndexedExpression(name, index) =>
        val pointy = env.getPointy(name)
        val targetType = pointy.elementType
        targetType.alignedSize match {
          case 1 => IndexedExpression(name, optimizeExpr(index, Map())).pos(pos)
          case _ =>
            val constantOffset: Option[Long] = env.eval(index) match {
              case Some(z) if z.isProvablyZero => Some(0L)
              case Some(NumericConstant(n, _)) =>
                if (targetType.alignedSize * (n+1) <= 256) Some(targetType.alignedSize * n) else None
              case _ => None
            }
            constantOffset match {
              case Some(o) if o >= 0 && o <= 256 - targetType.size =>
                if (pointy.isArray) {
                  DerefExpression(
                    "pointer" <| VariableExpression(name).pos(pos),
                    o.toInt, pointy.elementType).pos(pos)
                } else {
                  DerefExpression(
                    VariableExpression(name).pos(pos),
                    o.toInt, pointy.elementType).pos(pos)
                }
              case _ =>
                val arraySizeInBytes = pointy match {
                  case p: ConstantPointy => p.sizeInBytes
                  case _ => None
                }
                val shifts = Integer.numberOfTrailingZeros(targetType.alignedSize)
                val shrunkElementSize = targetType.alignedSize >> shifts
                val shrunkArraySize = arraySizeInBytes.fold(9999)(_.>>(shifts))
                val scaledIndex = arraySizeInBytes match {
                  // "n > targetType.alignedSize" means
                  // "don't do optimizations on arrays size 0 or 1"
                  case Some(n) if n > targetType.alignedSize && n <= 256 => targetType.alignedSize match {
                    case 1 => "byte" <| index
                    case 2 => "<<" <| ("byte" <| index, LiteralExpression(1, 1))
                    case 4 => "<<" <| ("byte" <| index, LiteralExpression(2, 1))
                    case 8 => "<<" <| ("byte" <| index, LiteralExpression(3, 1))
                    case _ => "*" <| ("byte" <| index, LiteralExpression(targetType.alignedSize, 1))
                  }
                  case Some(n) if n > targetType.alignedSize && n <= 512 && targetType.alignedSize == 2 =>
                    "nonet" <| ("<<" <| ("byte" <| index, LiteralExpression(1, 1)))
                  case Some(n) if n > targetType.alignedSize && shrunkArraySize <= 256 =>
                    "<<" <| ("word" <| ("*" <| ("byte" <| index, LiteralExpression(shrunkElementSize, 1))), LiteralExpression(shifts, 1))
                  case _ => targetType.alignedSize match {
                    case 1 => "word" <| index
                    case 2 => "<<" <| ("word" <| index, LiteralExpression(1, 1))
                    case 4 => "<<" <| ("word" <| index, LiteralExpression(2, 1))
                    case 8 => "<<" <| ("word" <| index, LiteralExpression(3, 1))
                    case _ => "*" <| ("word" <| index, LiteralExpression(targetType.alignedSize, 1))
                  }
                }
                DerefExpression(
                  ("pointer" <| VariableExpression(name).pos(pos)) #+# optimizeExpr(scaledIndex, Map()),
                  0, pointy.elementType).pos(pos)
            }
        }
      case _ => expr // TODO
    }
  }

  def pointlessCast(t1: String, expr: Expression): Boolean = {
    val typ1 = env.maybeGet[Type](t1).getOrElse(return false)
    val typ2 = getExpressionType(ctx, expr)
    if (!typ1.isSigned) {
      expr match {
        case SumExpression(params, false) => if (params.exists(_._1)) return false
        case _ =>
      }
    }
    typ1.name == typ2.name
  }

  def pointlessDoubleCast(t1: String, t2: String, expr: Expression): Boolean = {
    val s1 = env.maybeGet[Type](t1).getOrElse(return false).size
    val s2 = env.maybeGet[Type](t2).getOrElse(return false).size
    if (s1 != s2) return false
    val s3 = AbstractExpressionCompiler.getExpressionType(ctx, expr).size
    s1 == s3
  }
}

object AbstractStatementPreprocessor {
  val hiddenEffectFreeFunctions: Set[String] = Set(
    "+", "+'", "-", "-'",
    "*", "*'",
    "<<", "<<'", ">>", ">>'", ">>>>",
    "&", "&&", "||", "|", "^",
    "==", "!=", "<", ">", ">=", "<=",
    "not", "hi", "lo", "nonet", "sizeof", "typeof"
  )

  def mightBeMemset(ctx: CompilationContext, f: ForStatement): Boolean = {
    val env = ctx.env
    f.body match {
      case List(Assignment(target@IndexedExpression(pointy, index), source)) =>
        val sourceType = AbstractExpressionCompiler.getExpressionType(ctx, source)
        val targetType = AbstractExpressionCompiler.getExpressionType(ctx, target)
        if (
          ctx.isConstant(source) &&
          ctx.isConstant(index) &&
          sourceType.size <= 1 &&
          targetType.size == 1  &&
          sourceType.isAssignableTo(targetType)&&
          !env.isVolatile(VariableExpression(pointy)) &&
          !env.isVolatile(index) &&
          !env.isVolatile(source) &&
          !env.isVolatile(f.end) &&
          index.getAllIdentifiers.forall(iv => !ctx.env.overlapsVariable(iv, source)) &&
          !env.overlapsVariable(pointy, source) &&
          !env.overlapsVariable(pointy, index) &&
          !env.overlapsVariable(f.variable, source) &&
          !env.overlapsVariable(f.variable, f.start) &&
          !env.overlapsVariable(f.variable, f.end)
        ) env.eval(f.end #-# f.start).isDefined else false
      case _ => false
    }
  }
}