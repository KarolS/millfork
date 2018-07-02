package millfork.test.emu

import millfork.CpuFamily
import millfork.output.MemoryBank

/**
  * @author Karol Stasiak
  */
object EmuBenchmarkRun {
  def apply(source: String)(verifier: MemoryBank => Unit) = {
    val (Timings(t0, _), m0) = EmuUnoptimizedRun.apply2(source)
    val (Timings(t1, _), m1) = EmuOptimizedRun.apply2(source)
    val (Timings(t2, _), m2) = EmuOptimizedInlinedRun.apply2(source)
    println(f"Before optimization: $t0%7d")
    println(f"After optimization:  $t1%7d")
    println(f"After inlining:      $t2%7d")
    println(f"Gain:               ${(100L * (t0 - t1) / t0.toDouble).round}%7d%%")
    println(f"Gain with inlining: ${(100L * (t0 - t2) / t0.toDouble).round}%7d%%")
    println(f"Running unoptimized")
    verifier(m0)
    println(f"Running optimized")
    verifier(m1)
    println(f"Running optimized inlined")
    verifier(m2)
  }
}

object EmuI80BenchmarkRun {
  def apply(source: String)(verifier: MemoryBank => Unit) = {
    val (Timings(t0, _), m0) = EmuUnoptimizedZ80Run.apply2(source)
    val (Timings(t1, _), m1) = EmuOptimizedZ80Run.apply2(source)
    println(f"Before optimization: $t0%7d")
    println(f"After optimization:  $t1%7d")
    println(f"Gain:               ${(100L * (t0 - t1) / t0.toDouble).round}%7d%%")
    println(f"Running unoptimized")
    verifier(m0)
    println(f"Running optimized")
    verifier(m1)
  }
}

object EmuCrossPlatformBenchmarkRun {
  def apply(platforms: CpuFamily.Value*)(source: String)(verifier: MemoryBank => Unit): Unit = {
    if (platforms.isEmpty) {
      throw new RuntimeException("Dude, test at least one platform")
    }
    if (platforms.contains(CpuFamily.M6502)) {
      EmuBenchmarkRun.apply(source)(verifier)
    }
    if (platforms.contains(CpuFamily.I80)) {
      EmuI80BenchmarkRun.apply(source)(verifier)
    }
  }
}
