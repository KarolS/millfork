package millfork.compiler

import millfork.env.{Environment, Label, NormalFunction, NormalParamSignature}
import millfork.error.Logger
import millfork.node.NiceFunctionProperty
import millfork.{CompilationFlag, CompilationOptions, JobContext}

/**
  * @author Karol Stasiak
  */
case class CompilationContext(env: Environment,
                              function: NormalFunction,
                              extraStackOffset: Int,
                              options: CompilationOptions,
                              niceFunctionProperties: Set[(NiceFunctionProperty, String)],
                              breakLabels: Map[String, Label] = Map(),
                              continueLabels: Map[String, Label] = Map()){
  def withInlinedEnv(environment: Environment, newLabel: String): CompilationContext = {
    val newEnv = new Environment(Some(env), newLabel, environment.cpuFamily, jobContext)
    newEnv.things ++= environment.things
    copy(env = newEnv)
  }

  def addLabels(names: Set[String], breakLabel: Label, continueLabel: Label): CompilationContext = {
    var b = breakLabels
    var c = continueLabels
    names.foreach{name =>
      b += (name -> breakLabel)
      c += (name -> continueLabel)
    }
    this.copy(breakLabels = b, continueLabels = c)
  }

  def addStack(i: Int): CompilationContext = this.copy(extraStackOffset = extraStackOffset + i)

  def neverCheckArrayBounds: CompilationContext =
    this.copy(options = options.copy(commandLineFlags = options.commandLineFlags + (CompilationFlag.CheckIndexOutOfBounds -> false)))

  @inline
  def jobContext: JobContext = options.jobContext
  @inline
  def log: Logger = options.log
  @inline
  def nextLabel: LabelGenerator = options.nextLabel

  def prologueShouldAvoidA: Boolean = {
    function.params match {
      case NormalParamSignature(List(param)) => param.typ.size == 1
      case _ => false
    }
  }
}
