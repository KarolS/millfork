package millfork.test.emu

import millfork.CpuFamily
import millfork.output.MemoryBank

/**
  * @author Karol Stasiak
  */
object EmuBenchmarkRun {
  def apply(source: String)(verifier: MemoryBank => Unit): Unit = {
    val (Timings(t0, _), m0) = EmuUnoptimizedRun.apply2(source)
    val (Timings(t1, _), m1) = EmuOptimizedRun.apply2(source)
    val (Timings(t2, _), m2) = EmuOptimizedInlinedRun.apply2(source)
    println(f"Before optimization: $t0%7d")
    println(f"After optimization:  $t1%7d")
    println(f"After inlining:      $t2%7d")
    println(f"Gain:               ${(100L * (t0 - t1) / t0.toDouble).round}%7d%%")
    println(f"Gain with inlining: ${(100L * (t0 - t2) / t0.toDouble).round}%7d%%")
    println(f"Running 6502 unoptimized")
    verifier(m0)
    println(f"Running 6502 optimized")
    verifier(m1)
    println(f"Running 6502 optimized inlined")
    verifier(m2)
  }
}

object EmuSoftwareStackBenchmarkRun {
  def apply(source: String)(verifier: MemoryBank => Unit): Unit = {
    val (Timings(t0, _), m0) = EmuUnoptimizedRun.apply2(source)
    val (Timings(t1, _), m1) = EmuOptimizedRun.apply2(source)
    val (Timings(t2, _), m2) = EmuOptimizedInlinedRun.apply2(source)
    val (Timings(t3, _), m3) = EmuOptimizedSoftwareStackRun.apply2(source)
    println(f"Before optimization:  $t0%7d")
    println(f"After optimization:   $t1%7d")
    println(f"After inlining:       $t2%7d")
    println(f"After software stack: $t3%7d")
    println(f"Gain:               ${(100L * (t0 - t1) / t0.toDouble).round}%7d%%")
    println(f"Gain with inlining: ${(100L * (t0 - t2) / t0.toDouble).round}%7d%%")
    println(f"Gain with SW stack: ${(100L * (t0 - t3) / t0.toDouble).round}%7d%%")
    println(f"Running 6502 unoptimized")
    verifier(m0)
    println(f"Running 6502 optimized")
    verifier(m1)
    println(f"Running 6502 optimized inlined")
    verifier(m2)
    println(f"Running 6502 optimized with software stack")
    verifier(m3)
  }
}

object EmuZ80BenchmarkRun {
  def apply(source: String)(verifier: MemoryBank => Unit): Unit = {
    val (Timings(t0, _), m0) = EmuUnoptimizedZ80Run.apply2(source)
    val (Timings(t1, _), m1) = EmuOptimizedZ80Run.apply2(source)
    val (Timings(t2, _), m2) = EmuOptimizedInlinedZ80Run.apply2(source)
    println(f"Before optimization: $t0%7d")
    println(f"After optimization:  $t1%7d")
    println(f"After inlining:      $t2%7d")
    println(f"Gain:               ${(100L * (t0 - t1) / t0.toDouble).round}%7d%%")
    println(f"Gain with inlining: ${(100L * (t0 - t2) / t0.toDouble).round}%7d%%")
    println(f"Running Z80 unoptimized")
    verifier(m0)
    println(f"Running Z80 optimized")
    verifier(m1)
    println(f"Running Z80 optimized inlined")
    verifier(m2)
  }
}

object EmuIntel8080BenchmarkRun {
  def apply(source: String)(verifier: MemoryBank => Unit): Unit = {
    val (Timings(t0, _), m0) = EmuUnoptimizedIntel8080Run.apply2(source)
    val (Timings(t1, _), m1) = EmuOptimizedIntel8080Run.apply2(source)
    val (Timings(t2, _), m2) = EmuOptimizedInlinedIntel8080Run.apply2(source)
    println(f"Before optimization: $t0%7d")
    println(f"After optimization:  $t1%7d")
    println(f"After inlining:      $t2%7d")
    println(f"Gain:               ${(100L * (t0 - t1) / t0.toDouble).round}%7d%%")
    println(f"Gain with inlining: ${(100L * (t0 - t2) / t0.toDouble).round}%7d%%")
    println(f"Running 8080 unoptimized")
    verifier(m0)
    println(f"Running 8080 optimized")
    verifier(m1)
    println(f"Running 8080 optimized inlined")
    verifier(m2)
  }
}

object EmuSharpBenchmarkRun {
  def apply(source: String)(verifier: MemoryBank => Unit): Unit = {
    val (Timings(t0, _), m0) = EmuUnoptimizedSharpRun.apply2(source)
    val (Timings(t1, _), m1) = EmuOptimizedSharpRun.apply2(source)
    val (Timings(t2, _), m2) = EmuOptimizedInlinedSharpRun.apply2(source)
    println(f"Before optimization: $t0%7d")
    println(f"After optimization:  $t1%7d")
    println(f"After inlining:      $t2%7d")
    println(f"Gain:               ${(100L * (t0 - t1) / t0.toDouble).round}%7d%%")
    println(f"Gain with inlining: ${(100L * (t0 - t2) / t0.toDouble).round}%7d%%")
    println(f"Running LR35902 unoptimized")
    verifier(m0)
    println(f"Running LR35902 optimized")
    verifier(m1)
    println(f"Running LR35902 optimized inlined")
    verifier(m2)
  }
}

object EmuR800BenchmarkRun {
  def apply(source: String)(verifier: MemoryBank => Unit): Unit = {
    val (Timings(t0, _), m0) = EmuUnoptimizedR800Run.apply2(source)
    val (Timings(t1, _), m1) = EmuOptimizedR800Run.apply2(source)
    val (Timings(t2, _), m2) = EmuOptimizedInlinedR800Run.apply2(source)
    if (t0 > 0)println(f"Before optimization: $t0%7d")
    if (t1 > 0)println(f"After optimization:  $t1%7d")
    if (t2 > 0)println(f"After inlining:      $t2%7d")
    if (t0 > 0 && t1 > 0) println(f"Gain:               ${(100L * (t0 - t1) / t0.toDouble).round}%7d%%")
    if (t0 > 0 && t2 > 0) println(f"Gain with inlining: ${(100L * (t0 - t2) / t0.toDouble).round}%7d%%")
    if (t0 > 0) {
      println(f"Running R800 unoptimized")
      verifier(m0)
    }
    if (t1 > 0) {
      println(f"Running R800 optimized")
      verifier(m1)
    }
    if (t2 > 0) {
      println(f"Running R800 optimized inlined")
      verifier(m2)
    }
  }
}

object EmuIntel8086BenchmarkRun {
  def apply(source: String)(verifier: MemoryBank => Unit): Unit = {
    val (Timings(t0, _), m0) = EmuUnoptimizedIntel8086Run.apply2(source)
    val (Timings(t1, _), m1) = EmuOptimizedIntel8086Run.apply2(source)
    val (Timings(t2, _), m2) = EmuOptimizedInlinedIntel8086Run.apply2(source)
    println(f"Before optimization: $t0%7d")
    println(f"After optimization:  $t1%7d")
    println(f"After inlining:      $t2%7d")
    println(f"Gain:               ${(100L * (t0 - t1) / t0.toDouble).round}%7d%%")
    println(f"Gain with inlining: ${(100L * (t0 - t2) / t0.toDouble).round}%7d%%")
    println(f"Running 8086 unoptimized")
    verifier(m0)
    println(f"Running 8086 optimized")
    verifier(m1)
    println(f"Running 8086 optimized inlined")
    verifier(m2)
  }
}

object EmuMotorola6809BenchmarkRun {
  def apply(source: String)(verifier: MemoryBank => Unit): Unit = {
    val (Timings(t0, _), m0) = EmuUnoptimizedM6809Run.apply2(source)
    val (Timings(t1, _), m1) = EmuOptimizedM6809Run.apply2(source)
    val (Timings(t2, _), m2) = EmuOptimizedInlinedM6809Run.apply2(source)
    println(f"Before optimization: $t0%7d")
    println(f"After optimization:  $t1%7d")
    println(f"After inlining:      $t2%7d")
    println(f"Gain:               ${(100L * (t0 - t1) / t0.toDouble).round}%7d%%")
    println(f"Gain with inlining: ${(100L * (t0 - t2) / t0.toDouble).round}%7d%%")
    println(f"Running 6809 unoptimized")
    verifier(m0)
    println(f"Running 6809 optimized")
    verifier(m1)
    println(f"Running 6809 optimized inlined")
    verifier(m2)
  }
}

object EmuCrossPlatformBenchmarkRun {
  def apply(platforms: millfork.Cpu.Value*)(source: String)(verifier: MemoryBank => Unit): Unit = {
    if (platforms.isEmpty) {
      throw new RuntimeException("Dude, test at least one platform")
    }
    if (Settings.enable6502Tests && platforms.contains(millfork.Cpu.Mos)) {
      EmuBenchmarkRun.apply(source)(verifier)
    }
    if (Settings.enable6502Tests && platforms.contains(millfork.Cpu.StrictMos)) {
      EmuBenchmarkRun.apply(source)(verifier)
    }
    if (Settings.enableRicohTests && platforms.contains(millfork.Cpu.Ricoh)) {
      verifier(EmuUndocumentedRun.apply(source))
    }
    if (Settings.enable65C02Tests && platforms.contains(millfork.Cpu.Cmos)) {
      EmuCmosBenchmarkRun.apply(source)(verifier)
    }
    if (Settings.enableWdc85816Tests && platforms.contains(millfork.Cpu.Sixteen)) {
      EmuNative65816BenchmarkRun.apply(source)(verifier)
    }
    if (Settings.enableZ80Tests && platforms.contains(millfork.Cpu.Z80)) {
      EmuZ80BenchmarkRun.apply(source)(verifier)
    }
    if (Settings.enableIntel8080Tests && platforms.contains(millfork.Cpu.Intel8080)) {
      EmuIntel8080BenchmarkRun.apply(source)(verifier)
    }
    if (Settings.enableZ80Tests && platforms.contains(millfork.Cpu.R800)) {
      EmuR800BenchmarkRun.apply(source)(verifier)
    }
    if (Settings.enableUnemulatedTests && platforms.contains(millfork.Cpu.Intel8085)) {
      EmuUnoptimizedIntel8085Run.apply(source)
    }
    if (Settings.enableGameboyTests && platforms.contains(millfork.Cpu.Sharp)) {
      EmuSharpBenchmarkRun.apply(source)(verifier)
    }
    if (Settings.enableIntel8086Tests && platforms.contains(millfork.Cpu.Intel8086)) {
      EmuIntel8086BenchmarkRun.apply(source)(verifier)
    }
    if (Settings.enableMotorola6809Tests && platforms.contains(millfork.Cpu.Motorola6809)) {
      EmuMotorola6809BenchmarkRun.apply(source)(verifier)
    }
  }
}
