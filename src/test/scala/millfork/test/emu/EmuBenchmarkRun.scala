package millfork.test.emu

import millfork.output.MemoryBank

/**
  * @author Karol Stasiak
  */
object EmuBenchmarkRun {
  def apply(source:String)(verifier: MemoryBank=>Unit) = {
    val (Timings(t0, _), m0) = EmuUnoptimizedRun.apply2(source)
    val (Timings(t1, _), m1) = EmuOptimizedRun.apply2(source)
//    val (Timings(t2, _), m2) = SymonSuperOptimizedRun.apply2(source)
//val (Timings(t3, _), m3) = SymonQuantumOptimizedRun.apply2(source)
    println(f"Before optimization: $t0%7d")
    println(f"After optimization:  $t1%7d")
//    println(f"After quantum:       $t3%7d")
//    println(f"After superopt.:     $t2%7d")
    println(f"Gain:               ${(100L*(t0-t1)/t0.toDouble).round}%7d%%")
//    println(f"Quantum gain:       ${(100L*(t0-t3)/t0.toDouble).round}%7d%%")
//    println(f"Superopt. gain:     ${(100L*(t0-t2)/t0.toDouble).round}%7d%%")
    println(f"Running unoptimized")
    verifier(m0)
    println(f"Running optimized")
    verifier(m1)
//    println(f"Running quantum optimized")
//    verifier(m3)
//    println(f"Running superoptimized")
//    verifier(m2)
  }
}
