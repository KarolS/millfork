package millfork

import millfork.compiler.LabelGenerator
import millfork.error.Logger

/**
  * @author Karol Stasiak
  */
case class JobContext(log: Logger, nextLabel: LabelGenerator)
