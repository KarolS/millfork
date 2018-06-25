package millfork.assembly

import millfork.CompilationOptions
import millfork.env.NormalFunction
import millfork.node.NiceFunctionProperty

/**
  * @author Karol Stasiak
  */
case class OptimizationContext(options: CompilationOptions,
                               labelMap: Map[String, Int],
                               niceFunctionProperties: Set[(NiceFunctionProperty, String)])

trait AssemblyOptimization[T <: AbstractCode] {
  def name: String

  def optimize(f: NormalFunction, code: List[T], context: OptimizationContext): List[T] = optimize(f, code, context.options)

  def optimize(f: NormalFunction, code: List[T], options: CompilationOptions): List[T] = optimize(f, code, OptimizationContext(options, Map(), Set()))
}
