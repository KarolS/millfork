package millfork.assembly.opt

import millfork.{CompilationFlag, CompilationOptions}
import millfork.assembly.{AssemblyLine, State}
import millfork.env.NormalFunction

/**
  * @author Karol Stasiak
  */

case class FlowInfo(statusBefore: CpuStatus, importanceAfter: CpuImportance) {

  def hasClear(state: State.Value): Boolean = statusBefore.hasClear(state)

  def hasSet(state: State.Value): Boolean = statusBefore.hasSet(state)

  def isUnimportant(state: State.Value): Boolean = importanceAfter.isUnimportant(state)
}

object FlowInfo {
  val Default = FlowInfo(CpuStatus(), CpuImportance())
}

object FlowAnalyzer {
  def analyze(f: NormalFunction, code: List[AssemblyLine], options: CompilationOptions): List[(FlowInfo, AssemblyLine)] = {
    val forwardFlow = if (options.flag(CompilationFlag.DetailedFlowAnalysis)) {
      QuantumFlowAnalyzer.analyze(f, code).map(_.collapse)
    } else {
      CoarseFlowAnalyzer.analyze(f, code)
    }
    val reverseFlow = ReverseFlowAnalyzer.analyze(f, code)
    forwardFlow.zip(reverseFlow).map{case (s,i) => FlowInfo(s,i)}.zip(code)
  }
}
