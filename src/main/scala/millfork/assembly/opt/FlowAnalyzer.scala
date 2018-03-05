package millfork.assembly.opt

import millfork.{CompilationFlag, CompilationOptions}
import millfork.assembly.{AssemblyLine, Opcode, State}
import millfork.env.{Label, MemoryAddressConstant, NormalFunction}

/**
  * @author Karol Stasiak
  */

class FlowHolder(_statusBefore: () => List[CpuStatus], _importanceAfter: () => List[CpuImportance]) {
  lazy val statusBefore: List[CpuStatus] = _statusBefore()
  lazy val importanceAfter: List[CpuImportance] = _importanceAfter()
}

case class FlowInfo(holder: FlowHolder, index: Int, _labelUseCountMap: () => Option[Map[String, Int]]) {

  lazy val statusBefore: CpuStatus = holder.statusBefore(index)
  lazy val importanceAfter = holder.importanceAfter(index)
  lazy val labelUseCountMap: Option[Map[String, Int]] = _labelUseCountMap()

  def hasClear(state: State.Value): Boolean = statusBefore.hasClear(state)

  def hasSet(state: State.Value): Boolean = statusBefore.hasSet(state)

  def isUnimportant(state: State.Value): Boolean = importanceAfter.isUnimportant(state)

  def labelUseCount(label: String): Int = labelUseCountMap.map(_.getOrElse(label, 0)).getOrElse(-1)
}

object FlowAnalyzer {

  private val EmptyCpuStatus = CpuStatus()
  private val EmptyCpuImportance = CpuImportance()

  def analyze(f: NormalFunction, code: List[AssemblyLine], options: CompilationOptions, req: FlowInfoRequirement.Value): List[(FlowInfo, AssemblyLine)] = {
    val forwardFlow = req match {
      case FlowInfoRequirement.BothFlows | FlowInfoRequirement.ForwardFlow =>
        () => CoarseFlowAnalyzer.analyze(f, code, options)
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
      case _ => Some(code.flatMap {
        case AssemblyLine(op, _, MemoryAddressConstant(Label(l)), _) if op != Opcode.LABEL => Some(l)
        case _ => None
      }.groupBy(identity).mapValues(_.size))
    }
    val holder = new FlowHolder(forwardFlow, reverseFlow)
    code.zipWithIndex.map{ case (line, i) => FlowInfo(holder, i, labelMap) -> line}
  }
}
