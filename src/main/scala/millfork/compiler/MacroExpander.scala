package millfork.compiler

import millfork.assembly.AbstractCode
import millfork.assembly.mos._
import millfork.assembly.z80.ZOpcode
import millfork.env._
import millfork.node._

/**
  * @author Karol Stasiak
  */
abstract class MacroExpander[T <: AbstractCode] {

  def prepareAssemblyParams(ctx: CompilationContext, assParams: List[AssemblyParam], params: List[Expression], code: List[ExecutableStatement]): (List[T], List[ExecutableStatement])

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
      case ForStatement(v, start, end, dir, body) => ForStatement(h(v), f(start), f(end), dir, body.map(gx))
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
      case ForStatement(v, start, end, dir, body) => ForStatement(h(v), f(start), f(end), dir, body.map(gx))
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

  def inlineFunction(ctx: CompilationContext, i: MacroFunction, params: List[Expression], position: Option[Position]): (List[T], List[ExecutableStatement]) = {
    var paramPreparation = List[T]()
    var actualCode = i.code
    i.params match {
      case AssemblyParamSignature(assParams) =>
        val pair = prepareAssemblyParams(ctx, assParams, params, i.code)
        paramPreparation = pair._1
        actualCode = pair._2
      case NormalParamSignature(normalParams) =>
        if (params.length != normalParams.length) {
          ctx.log.error(s"Invalid number of params for macro function ${i.name}", position)
        } else {
          normalParams.foreach(param => i.environment.removeVariable(param.name))
          params.zip(normalParams).foreach {
            case (v@VariableExpression(_), MemoryVariable(paramName, paramType, _)) =>
              actualCode = actualCode.map(stmt => renameVariable(stmt, paramName.stripPrefix(i.environment.prefix), v.name).asInstanceOf[ExecutableStatement])
            case (v@IndexedExpression(_, _), MemoryVariable(paramName, paramType, _)) =>
              actualCode = actualCode.map(stmt => replaceVariable(stmt, paramName.stripPrefix(i.environment.prefix), v).asInstanceOf[ExecutableStatement])
            case _ =>
              ctx.log.error(s"Parameters to macro functions have to be variables", position)
          }
        }

    }
    // fix local labels:
    // TODO: do it even if the labels are in an inline assembly block inside a Millfork function
    val localLabels = actualCode.flatMap {
      case MosAssemblyStatement(Opcode.LABEL, _, VariableExpression(l), _) => Some(l)
      case Z80AssemblyStatement(ZOpcode.LABEL, _, _, VariableExpression(l), _) => Some(l)
      case _ => None
    }.toSet
    val labelPrefix = ctx.nextLabel("il")
    paramPreparation -> actualCode.map {
      case s@MosAssemblyStatement(_, _, VariableExpression(v), _) if localLabels(v) =>
        s.copy(expression = VariableExpression(labelPrefix + v))
      case s@Z80AssemblyStatement(_, _, _, VariableExpression(v), _) if localLabels(v) =>
        s.copy(expression = VariableExpression(labelPrefix + v))
      case s => s
    }
  }
}
