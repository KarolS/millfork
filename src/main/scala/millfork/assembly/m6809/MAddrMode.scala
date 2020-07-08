package millfork.assembly.m6809

import millfork.node.{M6809Register, Position}

/**
  * @author Karol Stasiak
  */
sealed trait MAddrMode {
  def changesRegister(reg: M6809Register.Value): Boolean = false

  def makeIndirect(position: Position): MAddrMode
  
  def isDeferenceable: Boolean
  def dereference(): MAddrMode
}

case object Inherent extends MAddrMode {
  def makeIndirect(position: Position): MAddrMode = ???
  
  def isDeferenceable: Boolean = false
  def dereference(): MAddrMode = ???
}

case object InherentA extends MAddrMode {
  override def changesRegister(reg: M6809Register.Value): Boolean = reg == M6809Register.A || reg == M6809Register.D

  def makeIndirect(position: Position): MAddrMode = ???
  
  def isDeferenceable: Boolean = false
  def dereference(): MAddrMode = ???
}

case object InherentB extends MAddrMode {
  override def changesRegister(reg: M6809Register.Value): Boolean = reg == M6809Register.B || reg == M6809Register.D

  def makeIndirect(position: Position): MAddrMode = ???
  
  def isDeferenceable: Boolean = false
  def dereference(): MAddrMode = ???
}

case class TwoRegisters(source: M6809Register.Value, target: M6809Register.Value) extends MAddrMode {
  def makeIndirect(position: Position): MAddrMode = ???
  
  def isDeferenceable: Boolean = false
  def dereference(): MAddrMode = ???
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
  
  def isDeferenceable: Boolean = false
  def dereference(): MAddrMode = ???
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
  
  def isDeferenceable: Boolean = !indirect
  def dereference(): MAddrMode = copy(indirect = true)
}

case class Indexed(base: M6809Register.Value, indirect: Boolean) extends MAddrMode {
  def makeIndirect(position: Position): MAddrMode = copy(indirect = true)
  
  def isDeferenceable: Boolean = !indirect
  def dereference(): MAddrMode = copy(indirect = true)
}

case class AAccumulatorIndexed(base: M6809Register.Value, indirect: Boolean) extends MAddrMode {
  def makeIndirect(position: Position): MAddrMode = copy(indirect = true)
  
  def isDeferenceable: Boolean = !indirect
  def dereference(): MAddrMode = copy(indirect = true)
}

case class BAccumulatorIndexed(base: M6809Register.Value, indirect: Boolean) extends MAddrMode {
  def makeIndirect(position: Position): MAddrMode = copy(indirect = true)
  
  def isDeferenceable: Boolean = !indirect
  def dereference(): MAddrMode = copy(indirect = true)
}

case class DAccumulatorIndexed(base: M6809Register.Value, indirect: Boolean) extends MAddrMode {
  def makeIndirect(position: Position): MAddrMode = copy(indirect = true)
  
  def isDeferenceable: Boolean = !indirect
  def dereference(): MAddrMode = copy(indirect = true)
}

case class PostIncremented(base: M6809Register.Value, amount: Int, indirect: Boolean) extends MAddrMode {
  override def changesRegister(reg: M6809Register.Value): Boolean = reg == base

  def makeIndirect(position: Position): MAddrMode = copy(indirect = true)
  def isDeferenceable: Boolean = false
  def dereference(): MAddrMode = ???
}

case class PreDecremented(base: M6809Register.Value, amount: Int, indirect: Boolean) extends MAddrMode {
  override def changesRegister(reg: M6809Register.Value): Boolean = reg == base

  def makeIndirect(position: Position): MAddrMode = copy(indirect = true)
  def isDeferenceable: Boolean = false
  def dereference(): MAddrMode = ???
}

case object Relative extends MAddrMode {
  def makeIndirect(position: Position): MAddrMode = ???
  def isDeferenceable: Boolean = false
  def dereference(): MAddrMode = ???
}

case object LongRelative extends MAddrMode {
  def makeIndirect(position: Position): MAddrMode = ???
  def isDeferenceable: Boolean = false
  def dereference(): MAddrMode = ???
}

case object Immediate extends MAddrMode {
  def makeIndirect(position: Position): MAddrMode = ???
  
  def isDeferenceable: Boolean = true
  def dereference(): MAddrMode = Absolute(false)
}

case object DirectPage extends MAddrMode {
  def makeIndirect(position: Position): MAddrMode = ???
  def isDeferenceable: Boolean = false
  def dereference(): MAddrMode = ???
}

case object NonExistent extends MAddrMode {
  def makeIndirect(position: Position): MAddrMode = ???
  def isDeferenceable: Boolean = false
  def dereference(): MAddrMode = ???
}

case object RawByte extends MAddrMode {
  def makeIndirect(position: Position): MAddrMode = ???
  def isDeferenceable: Boolean = false
  def dereference(): MAddrMode = ???
}
