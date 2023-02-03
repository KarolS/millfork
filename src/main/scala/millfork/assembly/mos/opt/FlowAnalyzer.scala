package millfork.assembly.mos.opt

import millfork.assembly.OptimizationContext
import millfork.assembly.mos.{AssemblyLine, AssemblyLine0, Opcode, State}
import millfork.env.{Label, MemoryAddressConstant, NormalFunction, StructureConstant}

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

  @inline
  def hasClear(state: State.Value): Boolean = statusBefore.hasClear(state)

  @inline
  def hasSet(state: State.Value): Boolean = statusBefore.hasSet(state)

  @inline
  def isUnimportant(state: State.Value): Boolean = importanceAfter.isUnimportant(state)

  @inline
  def labelUseCount(label: String): Int = labelUseCountMap.map(_.getOrElse(label, 0)).getOrElse(-1)

  override def toString: String = holder.toString(index)
}

object FlowAnalyzer {

  private val EmptyCpuStatus = CpuStatus()
  private val EmptyCpuImportance = CpuImportance()

  def analyze(f: NormalFunction, code: List[AssemblyLine], optimizationContext: OptimizationContext, req: FlowInfoRequirement.Value): List[(FlowInfo, AssemblyLine)] = {
    val forwardFlow = req match {
      case FlowInfoRequirement.BothFlows | FlowInfoRequirement.ForwardFlow =>
        () => CoarseFlowAnalyzer.analyze(f, code, optimizationContext)
      case FlowInfoRequirement.BackwardFlow | FlowInfoRequirement.JustLabels | FlowInfoRequirement.NoRequirement =>
        () => List.fill(code.size)(EmptyCpuStatus)
    }
    val reverseFlow = req match {
      case FlowInfoRequirement.BothFlows | FlowInfoRequirement.BackwardFlow =>
        () => ReverseFlowAnalyzer.analyze(f, code, optimizationContext)
      case FlowInfoRequirement.ForwardFlow | FlowInfoRequirement.JustLabels | FlowInfoRequirement.NoRequirement =>
        () => List.fill(code.size)(EmptyCpuImportance)
    }
    val labelMap: () => Option[Map[String, Int]] = () => req match {
      case FlowInfoRequirement.NoRequirement => None
      case _ => Some(code.filter(m => m.opcode != Opcode.LABEL).flatMap(_.parameter.extractLabels).groupBy(identity).mapValues(_.size).view.force)
    }
    val holder = new FlowHolder(forwardFlow, reverseFlow)
    code.zipWithIndex.map{ case (line, i) => FlowInfo(holder, i, labelMap) -> line}
  }
}
