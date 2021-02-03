package millfork.assembly.z80.opt

import millfork.CompilationOptions
import millfork.assembly.OptimizationContext
import millfork.assembly.z80.{ZLine, ZLine0, ZOpcode}
import millfork.env.{Label, MemoryAddressConstant, NormalFunction}

/**
  * @author Karol Stasiak
  */

class FlowHolder(_statusBefore: () => List[CpuStatus], _importanceAfter: () => List[CpuImportance]) {
  lazy val statusBefore: List[CpuStatus] = _statusBefore()
  lazy val importanceAfter: List[CpuImportance] = _importanceAfter()

  def toString(index: Int): String = statusBefore(index).toString ++ " -> " ++ importanceAfter(index).toString
}

case class FlowInfo(holder: FlowHolder, index: Int, _labelUseCountMap: () => Option[Map[String, Int]]) {

  lazy val statusBefore: CpuStatus = holder.statusBefore(index)
  lazy val importanceAfter: CpuImportance = holder.importanceAfter(index)
  lazy val labelUseCountMap: Option[Map[String, Int]] = _labelUseCountMap()

  def labelUseCount(label: String): Int = labelUseCountMap.map(_.getOrElse(label, 0)).getOrElse(-1)

  override def toString: String = holder.toString(index)
}

object FlowAnalyzer {

  private val EmptyCpuStatus = CpuStatus()
  private val EmptyCpuImportance = CpuImportance()

  def analyze(f: NormalFunction, code: List[ZLine], optimizationContext: OptimizationContext, req: FlowInfoRequirement.Value): List[(FlowInfo, ZLine)] = {
    val forwardFlow = req match {
      case FlowInfoRequirement.BothFlows | FlowInfoRequirement.ForwardFlow =>
        () => CoarseFlowAnalyzer.analyze(f, code, optimizationContext)
      case FlowInfoRequirement.BackwardFlow | FlowInfoRequirement.JustLabels | FlowInfoRequirement.NoRequirement =>
        () => List.fill(code.size)(EmptyCpuStatus)
    }
    val reverseFlow = req match {
      case FlowInfoRequirement.BothFlows | FlowInfoRequirement.BackwardFlow =>
        () => ReverseFlowAnalyzer.analyze(f, code)
      case FlowInfoRequirement.ForwardFlow | FlowInfoRequirement.JustLabels | FlowInfoRequirement.NoRequirement =>
        () => List.fill(code.size)(EmptyCpuImportance)
    }
    val labelMap: (() => Option[Map[String, Int]]) = () => req match {
      case FlowInfoRequirement.NoRequirement => None
      case _ => Some(code.flatMap(_.parameter.extractLabels).groupBy(identity).mapValues(_.size).view.force)
    }
    val holder = new FlowHolder(forwardFlow, reverseFlow)
    code.zipWithIndex.map{ case (line, i) => FlowInfo(holder, i, labelMap) -> line}
  }
}
