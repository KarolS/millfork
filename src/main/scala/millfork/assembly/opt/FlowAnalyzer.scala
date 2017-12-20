package millfork.assembly.opt

import millfork.{CompilationFlag, CompilationOptions}
import millfork.assembly.{AssemblyLine, Opcode, State}
import millfork.env.{Label, MemoryAddressConstant, NormalFunction}

/**
  * @author Karol Stasiak
  */

case class FlowInfo(statusBefore: CpuStatus, importanceAfter: CpuImportance, labelUseCountMap: Option[Map[String, Int]]) {

  def hasClear(state: State.Value): Boolean = statusBefore.hasClear(state)

  def hasSet(state: State.Value): Boolean = statusBefore.hasSet(state)

  def isUnimportant(state: State.Value): Boolean = importanceAfter.isUnimportant(state)

  def labelUseCount(label: String): Int = labelUseCountMap.map(_.getOrElse(label, 0)).getOrElse(-1)
}

object FlowInfo {
  val Default = FlowInfo(CpuStatus(), CpuImportance(), None)
}

object FlowAnalyzer {
  def analyze(f: NormalFunction, code: List[AssemblyLine], options: CompilationOptions, req: FlowInfoRequirement.Value): List[(FlowInfo, AssemblyLine)] = {
    val forwardFlow = req match {
      case FlowInfoRequirement.BothFlows | FlowInfoRequirement.ForwardFlow =>
        if (options.flag(CompilationFlag.DetailedFlowAnalysis)) {
          QuantumFlowAnalyzer.analyze(f, code).map(_.collapse)
        } else {
          CoarseFlowAnalyzer.analyze(f, code)
        }
      case FlowInfoRequirement.BackwardFlow | FlowInfoRequirement.JustLabels | FlowInfoRequirement.NoRequirement =>
        List.fill(code.size)(CpuStatus())
    }
    val reverseFlow = req match {
      case FlowInfoRequirement.BothFlows | FlowInfoRequirement.BackwardFlow =>
        ReverseFlowAnalyzer.analyze(f, code)
      case FlowInfoRequirement.ForwardFlow | FlowInfoRequirement.JustLabels | FlowInfoRequirement.NoRequirement =>
        List.fill(code.size)(CpuImportance())
    }
    val labelMap = req match {
      case FlowInfoRequirement.NoRequirement => None
      case _ => Some(code.flatMap {
        case AssemblyLine(op, _, MemoryAddressConstant(Label(l)), _) if op != Opcode.LABEL => Some(l)
        case _ => None
      }.groupBy(identity).mapValues(_.size))
    }
    forwardFlow.zip(reverseFlow).map { case (s, i) => FlowInfo(s, i, labelMap) }.zip(code)
  }
}
