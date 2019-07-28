package millfork.assembly.m6809

import millfork.node.{M6809Register, Position}

/**
  * @author Karol Stasiak
  */
sealed trait MAddrMode {
  def makeIndirect(position: Position): MAddrMode
}

case object Inherent extends MAddrMode {
  def makeIndirect(position: Position): MAddrMode = ???
}

case object InherentA extends MAddrMode {
  def makeIndirect(position: Position): MAddrMode = ???
}

case object InherentB extends MAddrMode {
  def makeIndirect(position: Position): MAddrMode = ???
}

case class TwoRegisters(source: M6809Register.Value, target: M6809Register.Value) extends MAddrMode {
  def makeIndirect(position: Position): MAddrMode = ???
}

case class RegisterSet(registers: Set[M6809Register.Value]) extends MAddrMode {
  def makeIndirect(position: Position): MAddrMode = ???
  def contains(register: M6809Register.Value): Boolean = {
    import M6809Register._
    register match {
      case A => registers(A) || registers(D)
      case B => registers(B) || registers(D)
      case D => registers(D) || (registers(A) && registers(B))
      case _ => registers(register)
    }
  }
}

object RegisterSet {
  def cleaned(registers: Set[M6809Register.Value]): RegisterSet = {
    import M6809Register._
    if (registers(A) && registers(B)) RegisterSet(registers - A - B + D)
    else if (registers(D) && registers(B)) RegisterSet(registers - A - B + D)
    else if (registers(D) && registers(A)) RegisterSet(registers - A - B + D)
    else RegisterSet(registers)

  }
}

case class Absolute(indirect: Boolean) extends MAddrMode {
  def makeIndirect(position: Position): MAddrMode = copy(indirect = true)
}

case class Indexed(base: M6809Register.Value, indirect: Boolean) extends MAddrMode {
  def makeIndirect(position: Position): MAddrMode = copy(indirect = true)
}

case class AAccumulatorIndexed(base: M6809Register.Value, indirect: Boolean) extends MAddrMode {
  def makeIndirect(position: Position): MAddrMode = copy(indirect = true)
}

case class BAccumulatorIndexed(base: M6809Register.Value, indirect: Boolean) extends MAddrMode {
  def makeIndirect(position: Position): MAddrMode = copy(indirect = true)
}

case class DAccumulatorIndexed(base: M6809Register.Value, indirect: Boolean) extends MAddrMode {
  def makeIndirect(position: Position): MAddrMode = copy(indirect = true)
}

case class PostIncremented(base: M6809Register.Value, amount: Int, indirect: Boolean) extends MAddrMode {
  def makeIndirect(position: Position): MAddrMode = copy(indirect = true)
}

case class PreDecremented(base: M6809Register.Value, amount: Int, indirect: Boolean) extends MAddrMode {
  def makeIndirect(position: Position): MAddrMode = copy(indirect = true)
}

case object Relative extends MAddrMode {
  def makeIndirect(position: Position): MAddrMode = ???
}

case object LongRelative extends MAddrMode {
  def makeIndirect(position: Position): MAddrMode = ???
}

case object Immediate extends MAddrMode {
  def makeIndirect(position: Position): MAddrMode = ???
}

case object DirectPage extends MAddrMode {
  def makeIndirect(position: Position): MAddrMode = ???
}

case object NonExistent extends MAddrMode {
  def makeIndirect(position: Position): MAddrMode = ???
}

case object RawByte extends MAddrMode {
  def makeIndirect(position: Position): MAddrMode = ???
}
