package millfork.assembly

import millfork.CompilationOptions
import millfork.env.{NormalFunction, ThingInMemory}
import millfork.error.Logger
import millfork.node.NiceFunctionProperty

/**
  * @author Karol Stasiak
  */
case class OptimizationContext(options: CompilationOptions,
                               labelMap: Map[String, Int],
                               zreg: Option[ThingInMemory],
                               niceFunctionProperties: Set[(NiceFunctionProperty, String)]) {
  def log: Logger = options.log
}

trait AssemblyOptimization[T <: AbstractCode] {
  def name: String

  def optimize(f: NormalFunction, code: List[T], context: OptimizationContext): List[T]
}
