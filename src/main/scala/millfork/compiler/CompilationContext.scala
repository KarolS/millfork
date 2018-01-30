package millfork.compiler

import millfork.{CompilationFlag, CompilationOptions}
import millfork.env.{Environment, MangledFunction, NormalFunction}

/**
  * @author Karol Stasiak
  */
case class CompilationContext(env: Environment, function: NormalFunction, extraStackOffset: Int, options: CompilationOptions){

  def addStack(i: Int): CompilationContext = this.copy(extraStackOffset = extraStackOffset + i)

  def neverCheckArrayBounds: CompilationContext =
    this.copy(options = options.copy(commandLineFlags = options.commandLineFlags + (CompilationFlag.CheckIndexOutOfBounds -> false)))
}
