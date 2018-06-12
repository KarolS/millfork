package millfork.assembly.mos.opt

import millfork.assembly.AssemblyOptimization
import millfork.assembly.mos.AssemblyLine
import millfork.assembly.mos.Opcode._

/**
  * @author Karol Stasiak
  */
object CE02Optimizations {

  val UseAsr = new RuleBasedAssemblyOptimization("Use 65CE02 instruction ASR",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (Elidable & HasOpcode(CMP) & HasImmediate(0x80)) ~
      (Elidable & HasOpcode(ROR)) ~~> (_ => List(AssemblyLine.implied(ASR))),
  )

  val All: List[AssemblyOptimization[AssemblyLine]] = List(UseAsr)
}
