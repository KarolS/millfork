package millfork.test.emu

import millfork.Cpu
import millfork.output.MemoryBank

/**
  * @author Karol Stasiak
  */
object EmuUnoptimizedCrossPlatformRun {
  def apply(platforms: Cpu.Value*)(source: String)(verifier: MemoryBank => Unit): Unit = {
    val (_, mm) = if (platforms.contains(Cpu.Mos) || platforms.contains(Cpu.StrictMos)) EmuUnoptimizedRun.apply2(source) else Timings(-1, -1) -> null
    val (_, mc) = if (platforms.contains(Cpu.Cmos)) EmuUnoptimizedCmosRun.apply2(source) else Timings(-1, -1) -> null
    val (_, mn) = if (platforms.contains(Cpu.Ricoh)) EmuUnoptimizedRicohRun.apply2(source) else Timings(-1, -1) -> null
    val (_, mz) = if (platforms.contains(Cpu.Z80)) EmuUnoptimizedZ80Run.apply2(source) else Timings(-1, -1) -> null
    val (_, mi) = if (platforms.contains(Cpu.Intel8080)) EmuUnoptimizedIntel8080Run.apply2(source) else Timings(-1, -1) -> null
    val (_, ms) = if (platforms.contains(Cpu.Sharp)) EmuUnoptimizedSharpRun.apply2(source) else Timings(-1, -1) -> null
    val (_, mo) = if (platforms.contains(Cpu.Motorola6809)) EmuUnoptimizedM6809Run.apply2(source) else Timings(-1, -1) -> null
    val (_, mx) = if (Settings.enableIntel8086Tests && platforms.contains(Cpu.Intel8086)) EmuUnoptimizedIntel8086Run.apply2(source) else Timings(-1, -1) -> null
    if (Settings.enable6502Tests && platforms.contains(Cpu.Mos)) {
      println(f"Running 6502")
      verifier(mm)
    }
    if (Settings.enableRicohTests && platforms.contains(millfork.Cpu.Ricoh)) {
      println(f"Running Ricoh")
      verifier(mn)
    }
    if (Settings.enable65C02Tests && platforms.contains(Cpu.Cmos)) {
      println(f"Running 65C02")
      verifier(mc)
    }
    if (Settings.enableZ80Tests && platforms.contains(Cpu.Z80)) {
      println(f"Running Z80")
      verifier(mz)
    }
    if (Settings.enableIntel8080Tests && platforms.contains(Cpu.Intel8080)) {
      println(f"Running 8080")
      verifier(mi)
    }
    if (Settings.enableGameboyTests && platforms.contains(Cpu.Sharp)) {
      println(f"Running LR35902")
      verifier(ms)
    }
    if (Settings.enableIntel8086Tests && platforms.contains(Cpu.Intel8086)) {
      println(f"Running 8086")
      verifier(mx)
    }
    if (Settings.enableMotorola6809Tests && platforms.contains(Cpu.Motorola6809)) {
      println(f"Running 6809")
      verifier(mo)
    }
  }
}
