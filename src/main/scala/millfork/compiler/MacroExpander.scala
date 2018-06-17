package millfork.compiler

import millfork.assembly.AbstractCode
import millfork.assembly.mos.Opcode._
import millfork.assembly.mos._
import millfork.env._
import millfork.error.ErrorReporting
import millfork.node.{MosRegister, _}

/**
  * @author Karol Stasiak
  */
abstract class MacroExpander[T <: AbstractCode] {

  def nextLabel(prefix: String): String

  def prepareAssemblyParams(ctx: CompilationContext, assParams: List[AssemblyParam], params: List[Expression], code: List[ExecutableStatement]): (List[T], List[ExecutableStatement])

  def replaceVariable(stmt: Statement, paramName: String, target: Expression): Statement = {
    def f[T <: Expression](e: T) = e.replaceVariable(paramName, target)

    def fx[T <: Expression](e: T) = e.replaceVariable(paramName, target).asInstanceOf[LhsExpression]

    def g[T <: Statement](s: T) = replaceVariable(s, paramName, target)

    def gx[T <: ExecutableStatement](s: T) = replaceVariable(s, paramName, target).asInstanceOf[ExecutableStatement]

    def h(s: String) = if (s == paramName) target.asInstanceOf[VariableExpression].name else s

    stmt match {
      case RawBytesStatement(contents) => RawBytesStatement(contents.replaceVariable(paramName, target))
      case ExpressionStatement(e) => ExpressionStatement(e.replaceVariable(paramName, target))
      case ReturnStatement(e) => ReturnStatement(e.map(f))
      case ReturnDispatchStatement(i, ps, bs) => ReturnDispatchStatement(i.replaceVariable(paramName, target), ps.map(fx), bs.map {
        case ReturnDispatchBranch(l, fu, pps) => ReturnDispatchBranch(l, f(fu), pps.map(f))
      })
      case WhileStatement(c, b, i, n) => WhileStatement(f(c), b.map(gx), i.map(gx), n)
      case DoWhileStatement(b, i, c, n) => DoWhileStatement(b.map(gx), i.map(gx), f(c), n)
      case ForStatement(v, start, end, dir, body) => ForStatement(h(v), f(start), f(end), dir, body.map(gx))
      case IfStatement(c, t, e) => IfStatement(f(c), t.map(gx), e.map(gx))
      case s: MosAssemblyStatement => s.copy(expression = f(s.expression))
      case Assignment(d, s) => Assignment(fx(d), f(s))
      case BreakStatement(s) => if (s == paramName) BreakStatement(target.toString) else stmt
      case ContinueStatement(s) => if (s == paramName) ContinueStatement(target.toString) else stmt
      case _ =>
        println(stmt)
        ???
    }
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
          ErrorReporting.error(s"Invalid number of params for macro function ${i.name}", position)
        } else {
          params.zip(normalParams).foreach {
            case (v@VariableExpression(_), MemoryVariable(paramName, paramType, _)) =>
              actualCode = actualCode.map(stmt => replaceVariable(stmt, paramName.stripPrefix(i.environment.prefix), v).asInstanceOf[ExecutableStatement])
            case (v@IndexedExpression(_, _), MemoryVariable(paramName, paramType, _)) =>
              actualCode = actualCode.map(stmt => replaceVariable(stmt, paramName.stripPrefix(i.environment.prefix), v).asInstanceOf[ExecutableStatement])
            case _ =>
              ErrorReporting.error(s"Parameters to macro functions have to be variables", position)
          }
        }

    }
    // fix local labels:
    // TODO: do it even if the labels are in an inline assembly block inside a Millfork function
    val localLabels = actualCode.flatMap {
      case MosAssemblyStatement(LABEL, _, VariableExpression(l), _) => Some(l)
      case _ => None
    }.toSet
    val labelPrefix = nextLabel("il")
    paramPreparation -> actualCode.map {
      case s@MosAssemblyStatement(_, _, VariableExpression(v), _) if localLabels(v) =>
        s.copy(expression = VariableExpression(labelPrefix + v))
      case s => s
    }
  }
}
