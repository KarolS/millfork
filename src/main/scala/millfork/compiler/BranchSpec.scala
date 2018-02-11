package millfork.compiler

/**
  * @author Karol Stasiak
  */

sealed trait BranchSpec {
  def flip: BranchSpec
}

case object NoBranching extends BranchSpec {
  override def flip: BranchSpec = this
}

case class BranchIfTrue(label: String) extends BranchSpec {
  override def flip: BranchSpec = BranchIfFalse(label)
}

case class BranchIfFalse(label: String) extends BranchSpec {
  override def flip: BranchSpec = BranchIfTrue(label)
}

object BranchSpec {
  val None: BranchSpec = NoBranching
}
