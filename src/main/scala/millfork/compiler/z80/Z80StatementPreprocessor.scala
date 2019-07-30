package millfork.compiler.z80

import millfork.CompilationFlag
import millfork.compiler.{AbstractStatementPreprocessor, CompilationContext}
import millfork.env.{MemoryVariable, MfArray, PointerType, Thing, Variable, VariableAllocationMethod}
import millfork.node.{Assignment, DerefDebuggingExpression, DoWhileStatement, ExecutableStatement, Expression, ExpressionStatement, ForDirection, ForEachStatement, ForStatement, FunctionCallExpression, IfStatement, IndexedExpression, IndirectFieldExpression, LhsExpression, LiteralExpression, Node, Statement, SumExpression, VariableDeclarationStatement, VariableExpression, WhileStatement}

/**
  * @author Karol Stasiak
  */
class Z80StatementPreprocessor(ctx: CompilationContext, statements: List[ExecutableStatement]) extends AbstractStatementPreprocessor(ctx, statements) {

  def findIndexedArrays(nodes: Seq[Node], variable: String): Seq[String] = nodes.flatMap(n => findIndexedArrays(n, variable))

  def findIndexedArrays(node: Node, variable: String): Seq[String] = node match {
    case f: ForStatement =>
      if (f.variable == variable) Nil else f.getChildStatements.flatMap(s => findIndexedArrays(s, variable))
    case f: Assignment => findIndexedArrays(f.destination, variable) ++ findIndexedArrays(f.source, variable)
    case f: IfStatement =>
      findIndexedArrays(f.condition, variable) ++ findIndexedArrays(f.thenBranch, variable) ++ findIndexedArrays(f.elseBranch, variable)
    case f: WhileStatement => findIndexedArrays(f.condition, variable) ++ findIndexedArrays(f.body, variable) ++ findIndexedArrays(f.increment, variable)
    case f: DoWhileStatement => findIndexedArrays(f.condition, variable) ++ findIndexedArrays(f.body, variable) ++ findIndexedArrays(f.increment, variable)
    case f: ForEachStatement => findIndexedArrays(f.values.right.toOption.getOrElse(Nil), variable) ++ findIndexedArrays(f.body, variable)
    case f: ExpressionStatement => findIndexedArrays(f.expression, variable)
    case f: FunctionCallExpression => findIndexedArrays(f.expressions, variable)
    case f: SumExpression => findIndexedArrays(f.expressions.map(_._2), variable)
    case f: VariableExpression => Nil
    case f: LiteralExpression => Nil
    case f: DerefDebuggingExpression => Nil
    case IndexedExpression(a, VariableExpression(v)) => if (v == variable) {
      ctx.env.maybeGet[Thing](a + ".array") match {
        case Some(array: MfArray) if array.elementType.size == 1 => Seq(a)
        case _ => Nil
      }
    } else Nil
    case IndexedExpression(_, e) => findIndexedArrays(e, variable)
    case _ => Nil
  }


  def maybeOptimizeForStatement(f: ForStatement): Option[(ExecutableStatement, VV)] = {
    if (!ctx.options.flag(CompilationFlag.DangerousOptimizations)) return None
    // TODO: figure out when this is useful
    // Currently all instances of arr[i] are replaced with arr`popt##`i[0], where arr`popt`i is a new pointer variable.
    // This breaks the main Millfork promise of not using hidden variables!
    // This may be increase code size or runtime in certain circumstances, more experimentation is needed.
    if (!optimize) return None
    if (ctx.env.eval(f.start).isEmpty) return None
    if (f.variable.contains(".")) return None
    if (f.start.containsVariable(f.variable)) return None
    if (f.end.containsVariable(f.variable)) return None
    val indexVariable = env.get[Variable](f.variable)
    if (indexVariable.typ.size != 1) return None
    if (indexVariable.isVolatile) return None
    indexVariable match {
      case v: MemoryVariable =>
        if (v.alloc == VariableAllocationMethod.Static) return None
      case _ => return None
    }
    val indexedArrays = findIndexedArrays(f.body, f.variable).toSet
    if (indexedArrays.isEmpty) return None
    if (indexedArrays.size > 2) return None // TODO: is this the optimal limit?
    val newVariables: Map[(String, String), String] = (for (a <- indexedArrays) yield {
      val array = ctx.env.get[MfArray](a + ".array")
      val infix = "`popt" + ctx.nextLabel.asNumber() + "`"
      // Evil hidden memory usage:
      val newVariable = a + infix + f.variable
      env.registerVariable(VariableDeclarationStatement(
        newVariable,
        "pointer",
        None,
        global = false,
        stack = false,
        constant = false,
        volatile = false,
        register = false,
        None,
        None,
        None
      ), ctx.options, isPointy = true)
      (a -> f.variable) -> (a + infix + f.variable)
    }).toMap

    def replaceArrayIndexingsE(node: Expression): Expression = node.replaceIndexedExpression(
      i => i.index match {
        case VariableExpression(vn) => vn == f.variable && indexedArrays(i.name)
        case _ => false
      },
      i => {
        val array = ctx.env.get[MfArray](i.name + ".array")
        optimizeExpr(IndirectFieldExpression(
          FunctionCallExpression("pointer." + array.elementType.name, List(VariableExpression(newVariables(i.name, f.variable)))),
          Seq(LiteralExpression(0, 1)),
          Seq()), Map())
      }
    )

    def replaceArrayIndexingsL(node: LhsExpression): LhsExpression = replaceArrayIndexingsE(node.asInstanceOf[Expression]).asInstanceOf[LhsExpression]

    def replaceArrayIndexingsS(node: ExecutableStatement): ExecutableStatement = node match {
      case Assignment(t, s) => Assignment(replaceArrayIndexingsL(t), replaceArrayIndexingsE(s)).pos(node.position)
      case ExpressionStatement(e) => ExpressionStatement(replaceArrayIndexingsE(e)).pos(node.position)
      case IfStatement(c, t, e) => IfStatement(replaceArrayIndexingsE(c), replaceArrayIndexings(t), replaceArrayIndexings(e)).pos(node.position)
      case WhileStatement(c, b, i, l) => WhileStatement(replaceArrayIndexingsE(c), replaceArrayIndexings(b), replaceArrayIndexings(i), l).pos(node.position)
      case DoWhileStatement(b, i, c, l) => DoWhileStatement(replaceArrayIndexings(b), replaceArrayIndexings(i), replaceArrayIndexingsE(c), l).pos(node.position)
      case _ => throw new ArrayIndexOutOfBoundsException // TODO
    }

    def replaceArrayIndexings(nodes: List[ExecutableStatement]): List[ExecutableStatement] = nodes.map(replaceArrayIndexingsS)

    val newDirection = f.direction match {
      case ForDirection.ParallelUntil => ForDirection.Until
      case ForDirection.ParallelTo => ForDirection.To
      case d => d
    }
    val operator = newDirection match {
      case ForDirection.DownTo => "-="
      case _ => "+="
    }
    try {
      val newBody = replaceArrayIndexings(f.body) ++ indexedArrays.map(name => {
        val array = ctx.env.get[MfArray](name + ".array")
        ExpressionStatement(FunctionCallExpression(operator, List(
          VariableExpression(newVariables(name, f.variable)),
          LiteralExpression(1, 1))))
      })
      val optStart = optimizeExpr(f.start, Map())
      Some(IfStatement(VariableExpression("true"),
        indexedArrays.map(name => {
          val array = ctx.env.get[MfArray](name + ".array")
          Assignment(
            VariableExpression(newVariables(name, f.variable)),
            FunctionCallExpression("pointer", List(
              VariableExpression(name + ".addr") #+# optStart
            )))
        }).toList :+ ForStatement(f.variable, optStart, optimizeExpr(f.end, Map()), newDirection, optimizeStmts(newBody, Map())._1),
        Nil
      ) -> Map())
    } catch {
      // too complex, give up:
      case _: ArrayIndexOutOfBoundsException => None
    }
  }
}
