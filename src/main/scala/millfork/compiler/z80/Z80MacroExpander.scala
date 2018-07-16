package millfork.compiler.z80

import java.util.Locale

import millfork.assembly.z80.ZLine
import millfork.compiler.{CompilationContext, MacroExpander}
import millfork.env._
import millfork.error.ErrorReporting
import millfork.node._

/**
  * @author Karol Stasiak
  */
object Z80MacroExpander extends MacroExpander[ZLine] {
  override def nextLabel(prefix: String): String = Z80Compiler.nextLabel(prefix)

  override def prepareAssemblyParams(ctx: CompilationContext, assParams: List[AssemblyParam], params: List[Expression], code: List[ExecutableStatement]): (List[ZLine], List[ExecutableStatement]) =  {
      var paramPreparation = List[ZLine]()
      var actualCode = code
      var hadRegisterParam = false
      assParams.zip(params).foreach {
        case (AssemblyParam(typ, Placeholder(ph, phType), AssemblyParameterPassingBehaviour.ByReference), actualParam) =>
          actualParam match {
            case VariableExpression(vname) =>
              ctx.env.get[ThingInMemory](vname)
            case l: LhsExpression =>
              // TODO: ??
              Z80ExpressionCompiler.compileToA(ctx, l)
            case _ =>
              ErrorReporting.error("A non-assignable expression was passed to an inlineable function as a `ref` parameter", actualParam.position)
          }
          actualCode = actualCode.map {
            case a@MosAssemblyStatement(_, _, expr, _) =>
              a.copy(expression = expr.replaceVariable(ph, actualParam))
            case x => x
          }
        case (AssemblyParam(typ, Placeholder(ph, phType), AssemblyParameterPassingBehaviour.ByConstant), actualParam) =>
          ctx.env.eval(actualParam).getOrElse(Constant.error("Non-constant expression was passed to an inlineable function as a `const` parameter", actualParam.position))
          actualCode = actualCode.map {
            case a@Z80AssemblyStatement(_, _, _, expr, _) =>
              a.copy(expression = expr.replaceVariable(ph, actualParam))
            case x => x
          }
        case (AssemblyParam(typ, v@ZRegisterVariable(register, _), AssemblyParameterPassingBehaviour.Copy), actualParam) =>
          if (hadRegisterParam) {
            ErrorReporting.error("Only one macro assembly function parameter can be passed via a register", actualParam.position)
          }
          hadRegisterParam = true
          paramPreparation = (register, typ.size) match {
            case (ZRegister.A, 1) => Z80ExpressionCompiler.compileToA(ctx, actualParam)
            case (r@(ZRegister.B | ZRegister.C | ZRegister.D | ZRegister.E | ZRegister.H | ZRegister.L), 1) => Z80ExpressionCompiler.compileToA(ctx, actualParam) :+ ZLine.ld8(r, ZRegister.A)
            case (ZRegister.HL, 2) => Z80ExpressionCompiler.compileToHL(ctx, actualParam)
            case (ZRegister.BC, 2) => Z80ExpressionCompiler.compileToBC(ctx, actualParam)
            case (ZRegister.DE, 2) => Z80ExpressionCompiler.compileToDE(ctx, actualParam)
            case _ =>
              ErrorReporting.error(s"Invalid parameter for macro: ${typ.name} ${register.toString.toLowerCase(Locale.ROOT)}", actualParam.position)
              Nil
          }
        case (AssemblyParam(_, _, AssemblyParameterPassingBehaviour.Copy), actualParam) =>
          ???
        case (_, actualParam) =>
      }
      paramPreparation -> actualCode
    }
  }
