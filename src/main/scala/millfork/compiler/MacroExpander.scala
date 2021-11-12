package millfork.compiler

import millfork.assembly.AbstractCode
import millfork.assembly.m6809.MOpcode
import millfork.assembly.mos._
import millfork.assembly.z80.ZOpcode
import millfork.env
import millfork.env._
import millfork.node._

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
abstract class MacroExpander[T <: AbstractCode] {

  def stmtPreprocess(ctx: CompilationContext, stmts: List[ExecutableStatement]): List[ExecutableStatement]

  def prepareAssemblyParams(ctx: CompilationContext, assParams: List[AssemblyOrMacroParam], params: List[Expression], code: List[ExecutableStatement]): (List[T], List[ExecutableStatement])

  def replaceVariableX(stmt: Statement, paramName: String, target: Expression): ExecutableStatement = {
    replaceVariable(stmt, paramName, target).asInstanceOf[ExecutableStatement]
  }
  def replaceVariable(stmt: Statement, paramName: String, target: Expression): Statement = {
    val paramNamePeriod = paramName + "."
    def f[S <: Expression](e: S) = e.replaceVariable(paramName, target)

    def fx[S <: Expression](e: S) = e.replaceVariable(paramName, target).asInstanceOf[LhsExpression]

    def g[S <: Statement](s: S) = replaceVariable(s, paramName, target)

    def gx[S <: ExecutableStatement](s: S) = replaceVariable(s, paramName, target).asInstanceOf[ExecutableStatement]

    def h(s: String): String =
      if (s == paramName) target.asInstanceOf[VariableExpression].name
      else if (s.startsWith(paramNamePeriod)) target.asInstanceOf[VariableExpression].name + s.stripPrefix(paramName)
      else s

    (stmt match {
      case RawBytesStatement(contents, be) => RawBytesStatement(contents.replaceVariable(paramName, target), be)
      case ExpressionStatement(e) => ExpressionStatement(e.replaceVariable(paramName, target))
      case ReturnStatement(e) => ReturnStatement(e.map(f))
      case ReturnDispatchStatement(i, ps, bs) => ReturnDispatchStatement(i.replaceVariable(paramName, target), ps.map(fx), bs.map {
        case ReturnDispatchBranch(l, fu, pps) => ReturnDispatchBranch(l, f(fu), pps.map(f))
      })
      case WhileStatement(c, b, i, n) => WhileStatement(f(c), b.map(gx), i.map(gx), n)
      case DoWhileStatement(b, i, c, n) => DoWhileStatement(b.map(gx), i.map(gx), f(c), n)
      case ForStatement(v, start, end, dir, body, increment) => ForStatement(h(v), f(start), f(end), dir, body.map(gx), increment.map(gx))
      case MemsetStatement(start, size, value, dir, original) => MemsetStatement(f(start), size, f(value), dir, original.map(gx).asInstanceOf[Option[ForStatement]])
      case IfStatement(c, t, e) => IfStatement(f(c), t.map(gx), e.map(gx))
      case s: Z80AssemblyStatement => s.copy(expression = f(s.expression), offsetExpression = s.offsetExpression.map(f))
      case s: MosAssemblyStatement => s.copy(expression = f(s.expression))
      case Assignment(d, s) => Assignment(fx(d), f(s))
      case BreakStatement(s) => if (s == paramName) BreakStatement(target.toString) else stmt
      case ContinueStatement(s) => if (s == paramName) ContinueStatement(target.toString) else stmt
      case s: EmptyStatement => s.copy(toTypecheck = s.toTypecheck.map(gx))
      case _ =>
        println(stmt)
        ???
    }).pos(stmt.position)
  }

  def renameVariableX(stmt: Statement, paramName: String, target: String): ExecutableStatement = {
    renameVariable(stmt, paramName, target).asInstanceOf[ExecutableStatement]
  }

  def renameVariable(stmt: Statement, paramName: String, target: String): Statement = {
    val paramNamePeriod = paramName + "."
    def f[S <: Expression](e: S) = e.renameVariable(paramName, target)

    def fx[S <: Expression](e: S) = e.renameVariable(paramName, target).asInstanceOf[LhsExpression]

    def g[S <: Statement](s: S) = renameVariable(s, paramName, target)

    def gx[S <: ExecutableStatement](s: S) = renameVariable(s, paramName, target).asInstanceOf[ExecutableStatement]

    def h(s: String): String =
      if (s == paramName) target.asInstanceOf[VariableExpression].name
      else if (s.startsWith(paramNamePeriod)) target.asInstanceOf[VariableExpression].name + s.stripPrefix(paramName)
      else s

    (stmt match {
      case RawBytesStatement(contents, be) => RawBytesStatement(contents.renameVariable(paramName, target), be)
      case ExpressionStatement(e) => ExpressionStatement(e.renameVariable(paramName, target))
      case ReturnStatement(e) => ReturnStatement(e.map(f))
      case ReturnDispatchStatement(i, ps, bs) => ReturnDispatchStatement(i.renameVariable(paramName, target), ps.map(fx), bs.map {
        case ReturnDispatchBranch(l, fu, pps) => ReturnDispatchBranch(l, f(fu), pps.map(f))
      })
      case WhileStatement(c, b, i, n) => WhileStatement(f(c), b.map(gx), i.map(gx), n)
      case DoWhileStatement(b, i, c, n) => DoWhileStatement(b.map(gx), i.map(gx), f(c), n)
      case ForStatement(v, start, end, dir, body, increment) => ForStatement(h(v), f(start), f(end), dir, body.map(gx), increment.map(gx))
      case MemsetStatement(start, size, value, dir, original) => MemsetStatement(f(start), size, f(value), dir, original.map(gx).asInstanceOf[Option[ForStatement]])
      case IfStatement(c, t, e) => IfStatement(f(c), t.map(gx), e.map(gx))
      case s: Z80AssemblyStatement => s.copy(expression = f(s.expression), offsetExpression = s.offsetExpression.map(f))
      case s: MosAssemblyStatement => s.copy(expression = f(s.expression))
      case Assignment(d, s) => Assignment(fx(d), f(s))
      case BreakStatement(s) => if (s == paramName) BreakStatement(target.toString) else stmt
      case ContinueStatement(s) => if (s == paramName) ContinueStatement(target.toString) else stmt
      case s: EmptyStatement => s.copy(toTypecheck = s.toTypecheck.map(gx))
      case _ =>
        println(stmt)
        ???
    }).pos(stmt.position)
  }

  def inlineFunction(ctx: CompilationContext, i: MacroFunction, actualParams: List[Expression], position: Option[Position]): (List[T], List[ExecutableStatement]) = {
    var paramPreparation = List[T]()
    var actualCode = i.code
    var actualConstants = i.constants
    i.params match {
      case AssemblyOrMacroParamSignature(params) =>
        params.foreach{ param =>
          val vName = param.variable.name
          i.environment.removeVariable(vName)
          ctx.env.maybeGet[Thing](vName) match {
            case Some(thing) => i.environment.things += vName -> thing
            case None =>
          }
        }
        if (actualParams.length != params.length) {
          ctx.log.error(s"Invalid number of params for macro function ${i.name}", position)
        } else if (i.isInAssembly) {
          val pair = prepareAssemblyParams(ctx, params, actualParams, i.code)
          paramPreparation = pair._1
          actualCode = pair._2
        } else {
          actualParams.zip(params).foreach {
            case (v@VariableExpression(_), AssemblyOrMacroParam(_, paramVariable, AssemblyParameterPassingBehaviour.ByReference)) =>
              actualCode = actualCode.map(stmt => renameVariableX(stmt, paramVariable.name.stripPrefix(i.environment.prefix), v.name))
            case (v@IndexedExpression(_, _), AssemblyOrMacroParam(_, paramVariable, AssemblyParameterPassingBehaviour.ByReference)) =>
              actualCode = actualCode.map(stmt => replaceVariableX(stmt, paramVariable.name.stripPrefix(i.environment.prefix), v))
            case (expr, AssemblyOrMacroParam(_, _, AssemblyParameterPassingBehaviour.ByReference)) =>
              ctx.log.error("By-reference parameters to macro functions have to be variables", expr.position)
            case (expr, AssemblyOrMacroParam(_, paramVariable, AssemblyParameterPassingBehaviour.ByConstant)) =>
              if (ctx.env.eval(expr).isEmpty) {
                ctx.log.error("Const parameters to macro functions have to be constants", expr.position)
              }
              actualCode = actualCode.map(stmt => replaceVariableX(stmt, paramVariable.name.stripPrefix(i.environment.prefix), expr))
              actualConstants = actualConstants.map(_.replaceVariableInInitialValue(paramVariable.name.stripPrefix(i.environment.prefix), expr))
            case (expr, AssemblyOrMacroParam(paramType, paramVariable, AssemblyParameterPassingBehaviour.Eval)) =>
              val castParam = FunctionCallExpression(paramType.name, List(expr))
              actualCode = actualCode.map(stmt => replaceVariableX(stmt, paramVariable.name.stripPrefix(i.environment.prefix), castParam))
            case _ =>
              ctx.log.error(s"Parameters to macro functions have to be variables", position)
          }
        }

    }
    var flattenedConstants = mutable.MutableList[VariableDeclarationStatement]()
    while(actualConstants.nonEmpty) {
      val constant = actualConstants.head
      flattenedConstants += constant
      actualConstants = actualConstants.tail.map(_.replaceVariableInInitialValue(constant.name.stripPrefix(i.environment.prefix), constant.initialValue.get))
    }
    for (constant <- flattenedConstants) {
      val valueExpr = constant.initialValue.get
      ctx.env.eval(valueExpr) match {
        case Some(c) =>
          actualCode = actualCode.map(stmt => replaceVariableX(stmt, constant.name.stripPrefix(i.environment.prefix), valueExpr))
        case None =>
          ctx.log.error("Not a constant", constant.position)
      }
    }
    // fix local labels:
    // TODO: do it even if the labels are in an inline assembly block inside a Millfork function
    val localLabels = actualCode.flatMap {
      case MosAssemblyStatement(Opcode.LABEL, _, VariableExpression(l), _) => Some(l)
      case Z80AssemblyStatement(ZOpcode.LABEL, _, _, VariableExpression(l), _) => Some(l)
      case M6809AssemblyStatement(MOpcode.LABEL, _, VariableExpression(l), _) => Some(l)
      case _ => None
    }.toSet
    val labelPrefix = ctx.nextLabel("il")
    actualCode = stmtPreprocess(ctx, actualCode)
    paramPreparation -> actualCode.map {
      case s@MosAssemblyStatement(_, _, VariableExpression(v), _) if localLabels(v) =>
        s.copy(expression = VariableExpression(labelPrefix + v))
      case s@Z80AssemblyStatement(_, _, _, VariableExpression(v), _) if localLabels(v) =>
        s.copy(expression = VariableExpression(labelPrefix + v))
      case s@M6809AssemblyStatement(_, _, VariableExpression(v), _) if localLabels(v) =>
        s.copy(expression = VariableExpression(labelPrefix + v))
      case s => s
    }
  }
}
