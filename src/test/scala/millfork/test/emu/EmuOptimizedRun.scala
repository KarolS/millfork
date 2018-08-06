package millfork.test.emu

import millfork.assembly.mos.opt.{LaterOptimizations, ZeropageRegisterOptimizations}
import millfork.assembly.z80.opt.{AlwaysGoodZ80Optimizations, Z80OptimizationPresets}
import millfork.{Cpu, OptimizationPresets}

/**
  * @author Karol Stasiak
  */
object EmuOptimizedRun extends EmuRun(
  Cpu.StrictMos,
  OptimizationPresets.NodeOpt,
  OptimizationPresets.AssOpt ++
    ZeropageRegisterOptimizations.All ++
    OptimizationPresets.Good ++
    OptimizationPresets.Good ++
    OptimizationPresets.Good ++ LaterOptimizations.Nmos ++
    OptimizationPresets.Good ++ LaterOptimizations.Nmos ++
    ZeropageRegisterOptimizations.All ++
    OptimizationPresets.Good ++
    OptimizationPresets.Good)

object EmuSizeOptimizedRun extends EmuRun(
  Cpu.StrictMos,
  OptimizationPresets.NodeOpt,
  OptimizationPresets.AssOpt ++
    ZeropageRegisterOptimizations.All ++
    OptimizationPresets.Good ++
    OptimizationPresets.Good ++
    OptimizationPresets.Good ++ LaterOptimizations.Nmos ++
    OptimizationPresets.Good ++ LaterOptimizations.Nmos ++
    ZeropageRegisterOptimizations.All ++
    OptimizationPresets.Good ++
    OptimizationPresets.Good) {
  override def optimizeForSize = true
}


object EmuOptimizedZ80Run extends EmuZ80Run(Cpu.Z80, OptimizationPresets.NodeOpt, Z80OptimizationPresets.GoodForZ80)

object EmuSizeOptimizedZ80Run extends EmuZ80Run(Cpu.Z80, OptimizationPresets.NodeOpt, Z80OptimizationPresets.GoodForZ80) {
  override def optimizeForSize = true
}

object EmuOptimizedIntel8080Run extends EmuZ80Run(Cpu.Intel8080, OptimizationPresets.NodeOpt, Z80OptimizationPresets.GoodForIntel8080)

object EmuSizeOptimizedIntel8080Run extends EmuZ80Run(Cpu.Intel8080, OptimizationPresets.NodeOpt, Z80OptimizationPresets.GoodForIntel8080) {
  override def optimizeForSize = true
}

object EmuOptimizedSharpRun extends EmuZ80Run(Cpu.Sharp, OptimizationPresets.NodeOpt, Z80OptimizationPresets.GoodForSharp)
