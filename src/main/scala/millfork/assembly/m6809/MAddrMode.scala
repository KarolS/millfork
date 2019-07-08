package millfork.assembly.m6809

import millfork.node.M6809Register

/**
  * @author Karol Stasiak
  */
sealed trait MAddrMode

case object Inherent extends MAddrMode

case object InherentA extends MAddrMode

case object InherentB extends MAddrMode

case class TwoRegisters(source: M6809Register.Value, target: M6809Register.Value) extends MAddrMode

case class RegisterSet(registers: Set[M6809Register.Value]) extends MAddrMode

case class Absolute(indirect: Boolean) extends MAddrMode

case class Indexed(base: M6809Register.Value, indirect: Boolean) extends MAddrMode

case class AccumulatorIndexed(base: M6809Register.Value, indirect: Boolean) extends MAddrMode

case class PostIncremented(base: M6809Register.Value, amount: Int, indirect: Boolean) extends MAddrMode

case class PreDecremented(base: M6809Register.Value, amount: Int, indirect: Boolean) extends MAddrMode

case object Relative extends MAddrMode

case object Immediate extends MAddrMode

case object NonExistent extends MAddrMode

case object RawByte extends MAddrMode
