package millfork.node

import millfork.Tarjan
import millfork.error.ErrorReporting

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */

sealed trait VariableVertex {
  def function: String
}

case class ParamVertex(function: String) extends VariableVertex

case class LocalVertex(function: String) extends VariableVertex

case object GlobalVertex extends VariableVertex {
  override def function = ""
}

abstract class CallGraph(program: Program) {

  def canOverlap(a: VariableVertex, b: VariableVertex): Boolean

  protected val entryPoints = mutable.Set[String]()
  // (F,G) means function F calls function G
  protected val callEdges = mutable.Set[(String, String)]()
  // (F,G) means function G is called when building parameters for function F
  protected val paramEdges = mutable.Set[(String, String)]()
  protected val multiaccessibleFunctions = mutable.Set[String]()
  protected val everCalledFunctions = mutable.Set[String]()
  protected val allFunctions = mutable.Set[String]()

  entryPoints += "main"
  program.declarations.foreach(s => add(None, Nil, s))
  everCalledFunctions.retain(allFunctions)
  fillOut()

  def add(currentFunction: Option[String], callingFunctions: List[String], node: Node): Unit = {
    node match {
      case f: FunctionDeclarationStatement =>
        allFunctions += f.name
        if (f.address.isDefined || f.interrupt) entryPoints += f.name
        f.statements.getOrElse(Nil).foreach(s => this.add(Some(f.name), Nil, s))
      case s: Statement =>
        s.getAllExpressions.foreach(e => add(currentFunction, callingFunctions, e))
      case g: FunctionCallExpression =>
        everCalledFunctions += g.functionName
        currentFunction.foreach(f => callEdges += f -> g.functionName)
        callingFunctions.foreach(f => paramEdges += f -> g.functionName)
        g.expressions.foreach(expr => add(currentFunction, g.functionName :: callingFunctions, expr))
      case s: SumExpression =>
        s.expressions.foreach(expr => add(currentFunction, callingFunctions, expr._2))
      case x: VariableExpression =>
        val varName = x.name.stripSuffix(".hi").stripSuffix(".lo").stripSuffix(".addr")
        everCalledFunctions += varName
      case i: IndexedExpression =>
        val varName = i.name.stripSuffix(".hi").stripSuffix(".lo").stripSuffix(".addr")
        everCalledFunctions += varName
        add(currentFunction, callingFunctions, i.index)
      case _ => ()
    }
  }


  def fillOut(): Unit = {
    var changed = true
    while (changed) {
      changed = false
      val toAdd = for {
        (a, b) <- callEdges
        (c, d) <- callEdges
        if b == c
        if !callEdges.contains(a -> d)
      } yield (a, d)
      if (toAdd.nonEmpty) {
        callEdges ++= toAdd
        changed = true
      }
    }

    changed = true
    while (changed) {
      changed = false
      val toAdd = for {
        (a, b) <- paramEdges
        (c, d) <- callEdges
        if b == c
        if !paramEdges.contains(a -> d)
      } yield (a, d)
      if (toAdd.nonEmpty) {
        paramEdges ++= toAdd
        changed = true
      }
    }
    multiaccessibleFunctions ++= entryPoints
    everCalledFunctions ++= entryPoints
    callEdges.filter(e => entryPoints.contains(e._1)).foreach(e => everCalledFunctions += e._2)
    multiaccessibleFunctions ++= callEdges.filter(e => entryPoints.contains(e._1)).map(_._2).groupBy(identity).filter(p => p._2.size > 1).keys

    ErrorReporting.trace("Call edges:")
    callEdges.toList.sorted.foreach(s => ErrorReporting.trace(s.toString))

    ErrorReporting.trace("Param edges:")
    paramEdges.toList.sorted.foreach(s => ErrorReporting.trace(s.toString))

    ErrorReporting.trace("Entry points:")
    entryPoints.toList.sorted.foreach(ErrorReporting.trace(_))

    ErrorReporting.trace("Multiaccessible functions:")
    multiaccessibleFunctions.toList.sorted.foreach(ErrorReporting.trace(_))

    ErrorReporting.trace("Ever called functions:")
    everCalledFunctions.toList.sorted.foreach(ErrorReporting.trace(_))
  }

  def isEverCalled(function: String): Boolean = {
    everCalledFunctions(function)
  }

  def recommendedCompilationOrder: List[String] = Tarjan.sort(allFunctions, callEdges)
}

class RestrictiveCallGraph(program: Program) extends CallGraph(program) {

  def canOverlap(a: VariableVertex, b: VariableVertex): Boolean = false
}

class StandardCallGraph(program: Program) extends CallGraph(program) {

  def canOverlap(a: VariableVertex, b: VariableVertex): Boolean = {
    if (a.function == b.function) {
      return false
    }
    if (a == GlobalVertex || b == GlobalVertex) {
      return false
    }
    if (multiaccessibleFunctions(a.function) || multiaccessibleFunctions(b.function)) {
      return false
    }
    if (callEdges(a.function -> b.function) || callEdges(b.function -> a.function)) {
      return false
    }
    a match {
      case ParamVertex(af) =>
        if (paramEdges(af -> b.function)) return false
      case _ =>
    }
    b match {
      case ParamVertex(bf) =>
        if (paramEdges(bf -> a.function)) return false
      case _ =>
    }
    ErrorReporting.trace(s"$a and $b can overlap")
    true
  }


}
