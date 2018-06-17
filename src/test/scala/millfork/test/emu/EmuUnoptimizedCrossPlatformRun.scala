package millfork.test.emu

import millfork.CpuFamily
import millfork.output.MemoryBank

/**
  * @author Karol Stasiak
  */
object EmuUnoptimizedCrossPlatformRun {
  def apply(platforms: CpuFamily.Value*)(source: String)(verifier: MemoryBank => Unit): Unit = {
    val (_, mm) = if (platforms.contains(CpuFamily.M6502)) EmuUnoptimizedRun.apply2(source) else Timings(-1, -1) -> null
    val (_, mz) = if (platforms.contains(CpuFamily.I80)) EmuUnoptimizedZ80Run.apply2(source) else Timings(-1, -1) -> null
    if (platforms.contains(CpuFamily.M6502)) {
      println(f"Running MOS")
      verifier(mm)
    }
  }
}
