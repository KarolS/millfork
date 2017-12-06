package millfork.test.emu

import millfork.output.MemoryBank

/**
  * @author Karol Stasiak
  */
object EmuCmosBenchmarkRun {
  def apply(source:String)(verifier: MemoryBank=>Unit) = {
    val (Timings(_, t0), m0) = EmuUnoptimizedRun.apply2(source)
    val (Timings(_, t1), m1) = EmuOptimizedRun.apply2(source)
    val (Timings(_, t2), m2) = EmuOptimizedCmosRun.apply2(source)
    println(f"Before optimization:      $t0%7d")
    println(f"After NMOS optimization:  $t1%7d")
    println(f"After CMOS optimization:  $t2%7d")
    println(f"Gain unopt->NMOS:        ${(100L*(t0-t1)/t0.toDouble).round}%7d%%")
    println(f"Gain unopt->CMOS:        ${(100L*(t0-t2)/t0.toDouble).round}%7d%%")
    println(f"Gain NMOS->CMOS:         ${(100L*(t1-t2)/t1.toDouble).round}%7d%%")
    println(f"Running unoptimized")
    verifier(m0)
    println(f"Running NMOS-optimized")
    verifier(m1)
    println(f"Running CMOS-optimized")
    verifier(m2)
  }
}
