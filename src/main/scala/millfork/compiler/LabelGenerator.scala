package millfork.compiler

import java.util.concurrent.atomic.AtomicLong

/**
  * @author Karol Stasiak
  */
class LabelGenerator {

  private val labelCounter = new AtomicLong

  def apply(prefix: String): String = "." + prefix + "__" + labelCounter.incrementAndGet().formatted("%05d")
}
