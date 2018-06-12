package millfork.assembly

import millfork.env.Constant

/**
  * @author Karol Stasiak
  */
trait AbstractCode {
  def sizeInBytes: Int
  def isPrintable: Boolean
  def parameter: Constant
}
