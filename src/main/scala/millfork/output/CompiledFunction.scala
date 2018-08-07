package millfork.output

import millfork.assembly.AbstractCode

/**
  * @author Karol Stasiak
  */
sealed trait CompiledFunction[T <: AbstractCode] {
  def orderKey : (Int, String)
}

case class NormalCompiledFunction[T <: AbstractCode](segment: String, code: List[T], hasFixedAddress: Boolean, alignment: MemoryAlignment) extends CompiledFunction[T] {
  override def orderKey: (Int, String) = (if (hasFixedAddress) 1 else 2) -> ""
}

case class RedirectedFunction[T <: AbstractCode](segment: String, redirect: String, offset: Int) extends CompiledFunction[T] {
  override def orderKey: (Int, String) = 3 -> redirect
}

case class NonexistentFunction[T <: AbstractCode]() extends CompiledFunction[T] {
  override def orderKey: (Int, String) = 4 -> ""
}
