package millfork.test.emu

import millfork.Cpu
import millfork.output.MemoryBank

/**
  * @author Karol Stasiak
  */
object EmuSizeOptimizedCrossPlatformRun {
  def apply(platforms: Cpu.Value*)(source: String)(verifier: MemoryBank => Unit): Unit = {
    val (_, mm) = if (platforms.contains(Cpu.Mos)) EmuSizeOptimizedRun.apply2(source) else Timings(-1, -1) -> null
    val (_, mz) = if (platforms.contains(Cpu.Z80)) EmuSizeOptimizedZ80Run.apply2(source) else Timings(-1, -1) -> null
    val (_, mi) = if (platforms.contains(Cpu.Intel8080)) EmuSizeOptimizedIntel8080Run.apply2(source) else Timings(-1, -1) -> null
    val (_, mo) = if (platforms.contains(Cpu.Motorola6809)) EmuOptimizedM6809Run.apply2(source) else Timings(-1, -1) -> null // TODO
    if (platforms.contains(Cpu.Mos)) {
      println(f"Running 6502")
      verifier(mm)
    }
    if (platforms.contains(Cpu.Z80)) {
      println(f"Running Z80")
      verifier(mz)
    }
    if (platforms.contains(Cpu.Intel8080)) {
      println(f"Running 8080")
      verifier(mi)
    }
    if (platforms.contains(Cpu.Motorola6809)) {
      println(f"Running 6809")
      verifier(mo)
    }
  }
}
