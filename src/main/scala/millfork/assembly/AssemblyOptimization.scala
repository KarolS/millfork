package millfork.assembly

import millfork.CompilationOptions
import millfork.env.{NormalFunction, ThingInMemory}
import millfork.node.NiceFunctionProperty

/**
  * @author Karol Stasiak
  */
case class OptimizationContext(options: CompilationOptions,
                               labelMap: Map[String, Int],
                               zreg: Option[ThingInMemory],
                               niceFunctionProperties: Set[(NiceFunctionProperty, String)])

trait AssemblyOptimization[T <: AbstractCode] {
  def name: String

  def optimize(f: NormalFunction, code: List[T], context: OptimizationContext): List[T]
}
