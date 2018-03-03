package millfork.assembly.opt

import millfork.assembly.AddrMode._
import millfork.assembly.AssemblyLine
import millfork.assembly.Opcode._
import millfork.assembly.OpcodeClasses._

/**
  * @author Karol Stasiak
  */
object CE02Optimizations {

  val UseAsr = new RuleBasedAssemblyOptimization("Use 65CE02 instruction ASR",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (Elidable & HasOpcode(CMP) & HasImmediate(0x80)) ~
      (Elidable & HasOpcode(ROR)) ~~> (_ => List(AssemblyLine.implied(ASR))),
  )

  val All: List[AssemblyOptimization] = List(UseAsr)
}
