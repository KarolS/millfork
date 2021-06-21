package millfork.assembly.m6809.opt

import millfork.assembly.m6809.MOpcode
import millfork.assembly.m6809.MOpcode._
import millfork.assembly.opt.Status.SingleFalse
import millfork.assembly.opt.{AnyStatus, SingleStatus, Status}
import millfork.env.Constant

/**
  * @author Karol Stasiak
  */
object ForwardFlowAnalysisForInherent {
  private val map: Map[MOpcode.Value, CpuStatus => CpuStatus] = Map(
    NOP -> identity,
    MUL -> { currentStatus =>
      val newD = (currentStatus.a <*> currentStatus.b) { (a, b) => ((a & 0xff) * (b & 0xff)) & 0xff }
      currentStatus.copy(c = AnyStatus, z = AnyStatus, a = newD.hi, b = newD.hi)
    },
    SEX -> { currentStatus =>
      val newA = currentStatus.b.map{ n => if (n.&(0x80) == 0) 0 else 0xff }
      currentStatus.copy(v = Status.SingleFalse, n = AnyStatus, z = AnyStatus, a = newA)
    },
  )

  def hasDefinition(opcode: MOpcode.Value): Boolean = map.contains(opcode)

  def get(opcode: MOpcode.Value): CpuStatus => CpuStatus = map(opcode)
}
