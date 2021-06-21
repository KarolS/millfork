package millfork.assembly.m6809.opt

import millfork.assembly.m6809.MOpcode
import millfork.assembly.m6809.MOpcode._
import millfork.assembly.opt.{AnyStatus, Status}

/**
  * @author Karol Stasiak
  */
object ForwardFlowAnalysisForInherentB {
  private val map: Map[MOpcode.Value, CpuStatus => CpuStatus] = Map(
    ASL -> { currentStatus =>
      val newValue = currentStatus.b.map(n => n.<<(1).&(0xff))
      currentStatus.copy(b = newValue, c = currentStatus.b.bit7, v = AnyStatus).nzB(newValue)
    },
    LSR -> { currentStatus =>
      val newValue = currentStatus.b.map(n => n.>>(1).&(0x7f))
      currentStatus.copy(b = newValue, c = currentStatus.b.bit0, v = AnyStatus).nzB(newValue)
    },
    CLR -> { currentStatus =>
      currentStatus.copy(b = Status.SingleZero, c = Status.SingleFalse, v = Status.SingleFalse, n = Status.SingleFalse, z = Status.SingleFalse)
    },
    COM -> { currentStatus =>
      val newValue = currentStatus.b.map(n => n.^(0xff).&(0xff))
      currentStatus.copy(b = newValue, c = Status.SingleTrue, v = Status.SingleFalse).nzB(newValue)
    },
    DEC -> { currentStatus =>
      val newValue = currentStatus.b.map(n => n.+(1).&(0xff))
      currentStatus.copy(b = newValue, v = AnyStatus).nzB(newValue)
    },
    INC -> { currentStatus =>
      val newValue = currentStatus.b.map(n => n.-(1).&(0xff))
      currentStatus.copy(b = newValue, v = AnyStatus).nzB(newValue)
    },
    NEG -> { currentStatus =>
      val newValue = currentStatus.b.map(n => (-n).&(0xff))
      currentStatus.copy(b = newValue, c = AnyStatus, v = AnyStatus).nzB(newValue)
    },
    TST -> { currentStatus =>
      currentStatus.copy(v = Status.SingleFalse).nzB(currentStatus.b)
    },
  )

  def hasDefinition(opcode: MOpcode.Value): Boolean = map.contains(opcode)

  def get(opcode: MOpcode.Value): CpuStatus => CpuStatus = map(opcode)
}
