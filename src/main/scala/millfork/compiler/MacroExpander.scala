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
object MacroExpander {


  def replaceVariable(stmt: Statement, paramName: String, target: Expression): Statement = {
    def f[T <: Expression](e:T) = e.replaceVariable(paramName, target)
    def fx[T <: Expression](e:T) = e.replaceVariable(paramName, target).asInstanceOf[LhsExpression]
    def g[T <: Statement](s:T) = replaceVariable(s, paramName, target)
    def gx[T <: ExecutableStatement](s:T) = replaceVariable(s, paramName, target).asInstanceOf[ExecutableStatement]
    def h(s:String) = if (s == paramName) target.asInstanceOf[VariableExpression].name else s
    stmt match {
      case ExpressionStatement(e) => ExpressionStatement(e.replaceVariable(paramName, target))
      case ReturnStatement(e) => ReturnStatement(e.map(f))
      case ReturnDispatchStatement(i,ps, bs) => ReturnDispatchStatement(i.replaceVariable(paramName, target), ps.map(fx), bs.map{
        case ReturnDispatchBranch(l, fu, pps) => ReturnDispatchBranch(l, f(fu), pps.map(f))
      })
      case WhileStatement(c, b, i, n) => WhileStatement(f(c), b.map(gx), i.map(gx), n)
      case DoWhileStatement(b, i, c, n) => DoWhileStatement(b.map(gx), i.map(gx), f(c), n)
      case ForStatement(v, start, end, dir, body) => ForStatement(h(v), f(start), f(end), dir, body.map(gx))
      case IfStatement(c, t, e) => IfStatement(f(c), t.map(gx), e.map(gx))
      case s:AssemblyStatement => s.copy(expression =  f(s.expression))
      case Assignment(d,s) => Assignment(fx(d), f(s))
      case BlockStatement(s) => BlockStatement(s.map(gx))
      case BreakStatement(s) => if (s == paramName) BreakStatement(target.toString) else stmt
      case ContinueStatement(s) => if (s == paramName) ContinueStatement(target.toString) else stmt
      case _ =>
        println(stmt)
        ???
    }
  }

  def inlineFunction(ctx: CompilationContext, i: MacroFunction, params: List[Expression], position: Option[Position]): (List[AssemblyLine], List[ExecutableStatement]) = {
    var paramPreparation = List[AssemblyLine]()
    var actualCode = i.code
    i.params match {
      case AssemblyParamSignature(assParams) =>
        var hadRegisterParam = false
        assParams.zip(params).foreach {
          case (AssemblyParam(typ, Placeholder(ph, phType), AssemblyParameterPassingBehaviour.ByReference), actualParam) =>
            actualParam match {
              case VariableExpression(vname) =>
                ctx.env.get[ThingInMemory](vname)
              case l: LhsExpression =>
                // TODO: ??
                ExpressionCompiler.compileByteStorage(ctx, Register.A, l)
              case _ =>
                ErrorReporting.error("A non-assignable expression was passed to an inlineable function as a `ref` parameter", actualParam.position)
            }
            actualCode = actualCode.map {
              case a@AssemblyStatement(_, _, expr, _) =>
                a.copy(expression = expr.replaceVariable(ph, actualParam))
              case x => x
            }
          case (AssemblyParam(typ, Placeholder(ph, phType), AssemblyParameterPassingBehaviour.ByConstant), actualParam) =>
            ctx.env.eval(actualParam).getOrElse(Constant.error("Non-constant expression was passed to an inlineable function as a `const` parameter", actualParam.position))
            actualCode = actualCode.map {
              case a@AssemblyStatement(_, _, expr, _) =>
                a.copy(expression = expr.replaceVariable(ph, actualParam))
              case x => x
            }
          case (AssemblyParam(typ, v@RegisterVariable(register, _), AssemblyParameterPassingBehaviour.Copy), actualParam) =>
            if (hadRegisterParam) {
              ErrorReporting.error("Only one macro assembly function parameter can be passed via a register", position)
            }
            hadRegisterParam = true
            paramPreparation = ExpressionCompiler.compile(ctx, actualParam, Some(typ, v), BranchSpec.None)
          case (AssemblyParam(_, _, AssemblyParameterPassingBehaviour.Copy), actualParam) =>
            ???
          case (_, actualParam) =>
        }
      case NormalParamSignature(normalParams) =>
        if (params.length != normalParams.length) {
          ErrorReporting.error(s"Invalid number of params for macro function ${i.name}", position)
        } else {
          params.zip(normalParams).foreach{
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
    val localLabels = actualCode.flatMap{
      case AssemblyStatement(LABEL, _, VariableExpression(l), _) => Some(l)
      case _ => None
    }.toSet
    val labelPrefix = MfCompiler.nextLabel("il")
    paramPreparation -> actualCode.map{
      case s@AssemblyStatement(_, _, VariableExpression(v), _) if localLabels(v) =>
        s.copy(expression =  VariableExpression(labelPrefix + v))
      case s => s
    }
  }
}
