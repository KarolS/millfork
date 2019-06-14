package millfork.assembly

import millfork.{CompilationFlag, CompilationOptions}
import millfork.compiler.LabelGenerator
import millfork.env.{NormalFunction, ThingInMemory}
import millfork.error.Logger
import millfork.node.NiceFunctionProperty

/**
  * @author Karol Stasiak
  */
case class OptimizationContext(options: CompilationOptions,
                               labelMap: Map[String, (Int, Int)],
                               zreg: Option[ThingInMemory],
                               niceFunctionProperties: Set[(NiceFunctionProperty, String)]) {
  @inline
  def log: Logger = options.log
  @inline
  def nextLabel: LabelGenerator = options.nextLabel
}

trait AssemblyOptimization[T <: AbstractCode] {
  def name: String

  def optimize(f: NormalFunction, code: List[T], context: OptimizationContext): List[T]

  def requiredFlags: Set[CompilationFlag.Value] = Set.empty
}
