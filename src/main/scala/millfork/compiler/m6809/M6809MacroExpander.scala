package millfork.compiler.m6809

import java.util.Locale

import millfork.assembly.m6809.MLine
import millfork.assembly.z80.ZLine
import millfork.compiler.{CompilationContext, MacroExpander}
import millfork.env._
import millfork.node._

/**
  * @author Karol Stasiak
  */
object M6809MacroExpander extends MacroExpander[MLine] {

  override def stmtPreprocess(ctx: CompilationContext, stmts: List[ExecutableStatement]): List[ExecutableStatement] = new M6809StatementPreprocessor(ctx, stmts)()

  override def prepareAssemblyParams(ctx: CompilationContext, assParams: List[AssemblyOrMacroParam], params: List[Expression], code: List[ExecutableStatement]): (List[MLine], List[ExecutableStatement]) =  {
      var paramPreparation = List[MLine]()
      var actualCode = code
      var hadRegisterParam = false
      assParams.zip(params).foreach {
        case (AssemblyOrMacroParam(typ, Placeholder(ph, phType), AssemblyParameterPassingBehaviour.ByReference), actualParam) =>
          actualParam match {
            case VariableExpression(vname) =>
              ctx.env.get[ThingInMemory](vname)
            case l: LhsExpression =>
              // TODO: ??
              M6809ExpressionCompiler.compileToB(ctx, l)
            case _ =>
              ctx.log.error("A non-assignable expression was passed to an inlineable function as a `ref` parameter", actualParam.position)
          }
          actualCode = actualCode.map {
            case a@MosAssemblyStatement(_, _, expr, _) =>
              a.copy(expression = expr.replaceVariable(ph, actualParam))
            case x => x
          }
        case (AssemblyOrMacroParam(typ, Placeholder(ph, phType), AssemblyParameterPassingBehaviour.ByConstant), actualParam) =>
          ctx.env.eval(actualParam).getOrElse(ctx.env.errorConstant("Non-constant expression was passed to an inlineable function as a `const` parameter", actualParam.position))
          actualCode = actualCode.map {
            case a@Z80AssemblyStatement(_, _, _, expr, _) =>
              a.copy(expression = expr.replaceVariable(ph, actualParam))
            case x => x
          }
        case (AssemblyOrMacroParam(typ, v@M6809RegisterVariable(register, _), AssemblyParameterPassingBehaviour.Copy), actualParam) =>
          if (hadRegisterParam) {
            ctx.log.error("Only one macro assembly function parameter can be passed via a register", actualParam.position)
          }
          hadRegisterParam = true
          paramPreparation = (register, typ.size) match {
            case (M6809Register.A, 1) => M6809ExpressionCompiler.compileToA(ctx, actualParam)
            case (M6809Register.B, 1) => M6809ExpressionCompiler.compileToB(ctx, actualParam)
            case (M6809Register.D, 2) => M6809ExpressionCompiler.compileToD(ctx, actualParam)
            case (M6809Register.X, 2) => M6809ExpressionCompiler.compileToX(ctx, actualParam)
            case (M6809Register.Y, 2) => M6809ExpressionCompiler.compileToY(ctx, actualParam)
            case (M6809Register.U, 2) => M6809ExpressionCompiler.compileToU(ctx, actualParam)
            case _ =>
              ctx.log.error(s"Invalid parameter for macro: ${typ.name} ${register.toString.toLowerCase(Locale.ROOT)}", actualParam.position)
              Nil
          }
        case (AssemblyOrMacroParam(_, _, AssemblyParameterPassingBehaviour.Copy), actualParam) =>
          ???
        case (_, actualParam) =>
      }
      paramPreparation -> stmtPreprocess(ctx, actualCode)
    }
  }
