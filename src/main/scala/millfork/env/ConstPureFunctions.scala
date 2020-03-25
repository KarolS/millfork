package millfork.env

import millfork.node.{Expression, FunctionCallExpression, GeneratedConstantExpression, IfStatement, IndexedExpression, LiteralExpression, ReturnStatement, Statement, SumExpression, VariableExpression}

/**
  * @author Karol Stasiak
  */
object ConstPureFunctions {

  def checkConstPure(env: Environment, function: NormalFunction): Unit = {
    if (!function.isConstPure) return
    val params = function.params match {
      case NormalParamSignature(ps) => ps.map(p => p.name.stripPrefix(function.name + "$")).toSet
    }
    checkConstPure(env, function.code, params)
  }

  private def checkConstPure(env: Environment, s: List[Statement], params: Set[String]): Unit = {
    s match {
      case List(ReturnStatement(Some(expr))) => checkConstPure(env, expr, params)
      case IfStatement(c, t, Nil) :: e =>
        checkConstPure(env, c, params)
        checkConstPure(env, t, params)
        checkConstPure(env, e, params)
      case List(IfStatement(c, t, e)) =>
        checkConstPure(env, c, params)
        checkConstPure(env, t, params)
        checkConstPure(env, e, params)
      case List(IfStatement(c, t, e), bad) =>
        checkConstPure(env, c, params)
        checkConstPure(env, t, params)
        checkConstPure(env, e, params)
        bad match {
          case ReturnStatement(None) =>
          case _ =>
            env.log.error(s"Statement ${bad} not allowed in const-pure functions", bad.position)
        }
      case ReturnStatement(Some(_)) :: bad :: xs =>
        bad match {
          case ReturnStatement(None) =>
          case _ =>
            env.log.error(s"Statement ${bad} not allowed in const-pure functions", bad.position)
        }
        checkConstPure(env, xs, params)
      case (bad@ReturnStatement(None)) :: xs =>
        env.log.error("Returning without value not allowed in const-pure functions",
          bad.position.orElse(xs.headOption.flatMap(_.position)))
        checkConstPure(env, xs, params)
      case bad :: xs =>
        env.log.error(s"Statement $bad not allowed in const-pure functions", bad.position)
        checkConstPure(env, xs, params)
      case _ =>
    }
  }

  private def checkConstPure(env: Environment, expr: Expression, params: Set[String]): Unit = {
    expr match {
      case VariableExpression(vname) =>
        if (params(vname)) return
        if (env.eval(expr).isDefined) return
        env.log.error(s"Refering to `$vname` not allowed in const-pure functions", expr.position)
      case LiteralExpression(_, _) =>
      case GeneratedConstantExpression(_, _) =>
      case SumExpression(expressions, _) =>
        for((_, e) <- expressions) checkConstPure(env, e, params)
      case FunctionCallExpression(functionName, expressions) =>
        functionName match {
          case "/" | "%%" | "*" | "*'" | "<<" | ">>" | "<<'" | ">>'" | ">>>>" =>
          case ">" | ">=" | "<=" | "<" | "!=" | "==" =>
          case "|" | "||" | "&" | "&&" | "^" =>
          case f if Environment.constOnlyBuiltinFunction(f) =>
          case f if Environment.predefinedFunctions(f) =>
          case _ =>
            env.maybeGet[Thing](functionName) match {
              case Some(n: NormalFunction) if n.isConstPure =>
              case Some(_: Type) =>
              case Some(_: ConstOnlyCallable) =>
              case Some(th) =>
                env.log.error(s"Calling `${th.name}` not allowed in const-pure functions", expr.position)
              case None =>
                if (functionName.exists(c => Character.isAlphabetic(c.toInt))) {
                  env.log.error(s"Calling undefined thing `$functionName` not allowed in const-pure functions", expr.position)
                } else {
                  env.log.error(s"Operator `$functionName` not allowed in const-pure functions", expr.position)
                }
            }
        }
        for (e <- expressions) checkConstPure(env, e, params)
      case IndexedExpression(vname, index) =>
        env.getPointy(vname) match {
          case p: ConstantPointy if p.isArray && p.readOnly =>
          case _ =>
            env.log.error(s"Calling `${vname}` not allowed in const-pure functions", expr.position)
        }
        checkConstPure(env, index, params)
      case _ =>
        env.log.error(s"Expression not allowed in const-pure functions", expr.position)

    }
  }


  def eval(env: Environment, function: NormalFunction, args: List[Constant]): Option[Constant] = {
    val fitArgs = args.zip(function.params.types).map { case (arg, typ) => arg.fitInto(typ) }
    val params = function.params match {
      case NormalParamSignature(ps) => ps.zip(fitArgs).map { case (p, arg) => p.name.stripPrefix(function.name + "$") -> arg }.toMap
    }
    eval(env, function.code, params).map(_.fitInto(function.returnType))
  }

  @scala.annotation.tailrec
  private def eval(env: Environment, code: List[Statement], params: Map[String, Constant]): Option[Constant] = {
    code match {
      case List(ReturnStatement(Some(expr))) =>
        env.eval(expr, params)
      case IfStatement(cond, t, Nil) :: xs =>
        env.eval(cond, params) match {
          case Some(c) if c.isProvablyZero => eval(env, xs, params)
          case Some(NumericConstant(value, _)) => eval(env, if (value != 0) t else xs, params)
          case _ => None
        }
      case List(IfStatement(cond, t, e)) =>
        env.eval(cond, params) match {
          case Some(c) if c.isProvablyZero => eval(env, e, params)
          case Some(NumericConstant(value, _)) => eval(env, if (value != 0) t else e, params)
          case _ => None
        }
      case _ => None
    }
  }

}
