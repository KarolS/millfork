package millfork.test.emu

import millfork.output.MemoryBank

/**
  * @author Karol Stasiak
  */
object EmuNative65816BenchmarkRun {
  def apply(source:String)(verifier: MemoryBank=>Unit): Unit = {
    println(f"Compiling for 65816 (unoptimized)")
    val (Timings(_, t0), m0) = EmuUnoptimizedNative65816Run.apply2(source)
    println(f"Compiling for 65816 (optimized)")
    val (Timings(_, t2), m2) = EmuOptimizedNative65816Run.apply2(source)
    println(f"Before optimization:  $t0%7d")
    println(f"After optimization:   $t2%7d")
    println(f"Gain:                 ${(100L*(t0-t2)/t0.toDouble).round}%7d%%")
    println(f"Running 65816 unoptimized")
    verifier(m0)
    println(f"Running 65816 optimized")
    verifier(m2)
  }
}
