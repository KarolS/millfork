package millfork.test.emu

import millfork.output.MemoryBank

/**
  * @author Karol Stasiak
  */
object EmuUltraBenchmarkRun {
  def apply(source:String)(verifier: MemoryBank=>Unit): Unit = {
    val (Timings(t0, _), m0) = EmuUnoptimizedRun.apply2(source)
    val (Timings(t1, _), m1) = EmuOptimizedRun.apply2(source)
    val (Timings(ti, _), mi) = EmuOptimizedInlinedRun.apply2(source)
    val (Timings(t2, _), m2) = EmuSuperOptimizedRun.apply2(source)
    val (Timings(t5, _), m5) = EmuSuperOptimizedInliningRun.apply2(source)
    println(f"Before optimization: $t0%7d")
    println(f"After optimization:  $t1%7d")
    println(f"After inlining:      $ti%7d")
    println(f"After superopt.:     $t2%7d")
    println(f"After super. + inl.: $t5%7d")
    println(f"Gain:               ${(100L*(t0-t1)/t0.toDouble).round}%7d%%")
    println(f"Inlining gain:      ${(100L*(t0-ti)/t0.toDouble).round}%7d%%")
    println(f"Superopt. gain:     ${(100L*(t0-t2)/t0.toDouble).round}%7d%%")
    println(f"Super. + inl. gain: ${(100L*(t0-t5)/t0.toDouble).round}%7d%%")
    println(f"Running unoptimized")
    verifier(m0)
    println(f"Running optimized")
    verifier(m1)
    println(f"Running optimized inlined")
    verifier(mi)
    println(f"Running superoptimized")
    verifier(m2)
    println(f"Running superoptimized inlined")
    verifier(m5)
  }
}
