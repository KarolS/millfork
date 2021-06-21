package millfork.assembly.m6809.opt

import millfork.assembly.m6809.MOpcode
import millfork.assembly.m6809.MOpcode._
import millfork.assembly.opt.{AnyStatus, SingleStatus, Status}
import millfork.assembly.opt.Status.SingleFalse
import millfork.env.Constant

/**
  * @author Karol Stasiak
  */
object ForwardFlowAnalysisForImmediate {
  private val map: Map[MOpcode.Value, (Constant, CpuStatus) => CpuStatus] = Map(
    LDX -> {(c, currentStatus) =>
      currentStatus.copy(x = SingleStatus(c), v = SingleFalse).nzW(c)
    },
    LDY -> {(c, currentStatus) =>
      currentStatus.copy(y = SingleStatus(c), v = SingleFalse).nzW(c)
    },
    LDU -> {(c, currentStatus) =>
      currentStatus.copy(u = SingleStatus(c), v = SingleFalse).nzW(c)
    },
    LDA -> {(c, currentStatus) =>
      val value = Status.fromByte(c)
      currentStatus.copy(a = value, v = SingleFalse).nzB(value)
    },
    LDB -> {(c, currentStatus) =>
      val value = Status.fromByte(c)
      currentStatus.copy(a = value, v = SingleFalse).nzB(value)
    },
    LDD -> {(c, currentStatus) =>
      val value = Status.fromByte(c)
      currentStatus.copy(a = value.hi, b = value.lo, v = SingleFalse).nzW(value)
    },
    ORA -> { (c, currentStatus) =>
      val newValue = (currentStatus.a <*> Status.fromByte(c)) { (x, y) => (x | y) & 0xff }
      currentStatus.copy(a = newValue, v = SingleFalse).nzB(newValue)
    },
    ORB -> { (c, currentStatus) =>
      val newValue = (currentStatus.b <*> Status.fromByte(c)) { (x, y) => (x | y) & 0xff }
      currentStatus.copy(b = newValue, v = SingleFalse).nzB(newValue)
    },
    EORA -> { (c, currentStatus) =>
      val newValue = (currentStatus.a <*> Status.fromByte(c)) { (x, y) => (x ^ y) & 0xff }
      currentStatus.copy(a = newValue, v = SingleFalse).nzB(newValue)
    },
    EORB -> { (c, currentStatus) =>
      val newValue = (currentStatus.b <*> Status.fromByte(c)) { (x, y) => (x ^ y) & 0xff }
      currentStatus.copy(b = newValue, v = SingleFalse).nzB(newValue)
    },
    ANDA -> { (c, currentStatus) =>
      val newValue = (currentStatus.a <*> Status.fromByte(c)) { (x, y) => (x & y) & 0xff }
      currentStatus.copy(a = newValue, v = SingleFalse).nzB(newValue)
    },
    ANDB -> { (c, currentStatus) =>
      val newValue = (currentStatus.b <*> Status.fromByte(c)) { (x, y) => (x & y) & 0xff }
      currentStatus.copy(b = newValue, v = SingleFalse).nzB(newValue)
    },
    ADDA -> { (c, currentStatus) =>
      val (newValue, newCarry) = currentStatus.a.adc(Status.fromByte(c), SingleFalse)
      currentStatus.copy(a = newValue, v = AnyStatus, c = newCarry).nzB(newValue)
    },
    ADDB -> { (c, currentStatus) =>
      val (newValue, newCarry) = currentStatus.b.adc(Status.fromByte(c), SingleFalse)
      currentStatus.copy(b = newValue, v = AnyStatus, c = newCarry).nzB(newValue)
    },
    ADCA -> { (c, currentStatus) =>
      val (newValue, newCarry) = currentStatus.a.adc(Status.fromByte(c), currentStatus.c)
      currentStatus.copy(a = newValue, v = AnyStatus, c = newCarry).nzB(newValue)
    },
    ADCB -> { (c, currentStatus) =>
      val (newValue, newCarry) = currentStatus.b.adc(Status.fromByte(c), currentStatus.c)
      currentStatus.copy(b = newValue, v = AnyStatus, c = newCarry).nzB(newValue)
    },
  )

  def hasDefinition(opcode: MOpcode.Value): Boolean = map.contains(opcode)

  def get(opcode: MOpcode.Value): (Constant, CpuStatus) => CpuStatus = map(opcode)
}
