package millfork.test.emu

import millfork.Cpu
import millfork.output.MemoryBank

/**
  * @author Karol Stasiak
  */
object EmuUnoptimizedCrossPlatformRun {
  def apply(platforms: Cpu.Value*)(source: String)(verifier: MemoryBank => Unit): Unit = {
    val (_, mm) = if (platforms.contains(Cpu.Mos)) EmuUnoptimizedRun.apply2(source) else Timings(-1, -1) -> null
    val (_, mc) = if (platforms.contains(Cpu.Cmos)) EmuUnoptimizedCmosRun.apply2(source) else Timings(-1, -1) -> null
    val (_, mn) = if (platforms.contains(Cpu.Ricoh)) EmuUnoptimizedRicohRun.apply2(source) else Timings(-1, -1) -> null
    val (_, mz) = if (platforms.contains(Cpu.Z80)) EmuUnoptimizedZ80Run.apply2(source) else Timings(-1, -1) -> null
    val (_, mi) = if (platforms.contains(Cpu.Intel8080)) EmuUnoptimizedIntel8080Run.apply2(source) else Timings(-1, -1) -> null
    val (_, ms) = if (platforms.contains(Cpu.Sharp)) EmuUnoptimizedSharpRun.apply2(source) else Timings(-1, -1) -> null
    if (platforms.contains(Cpu.Mos)) {
      println(f"Running 6502")
      verifier(mm)
    }
    if (platforms.contains(millfork.Cpu.Ricoh)) {
      println(f"Running Ricoh")
      verifier(mn)
    }
    if (platforms.contains(Cpu.Cmos)) {
      println(f"Running 65C02")
      verifier(mc)
    }
    if (platforms.contains(Cpu.Z80)) {
      println(f"Running Z80")
      verifier(mz)
    }
    if (platforms.contains(Cpu.Intel8080)) {
      println(f"Running 8080")
      verifier(mi)
    }
    if (platforms.contains(Cpu.Sharp)) {
      println(f"Running LR35902")
      verifier(ms)
    }
  }
}
