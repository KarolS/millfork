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
    OptimizationPresets.Good ++ LaterOptimizations.Nmos ++
    OptimizationPresets.Good ++ LaterOptimizations.Nmos ++
    ZeropageRegisterOptimizations.All ++
    OptimizationPresets.Good)



object EmuOptimizedZ80Run extends EmuZ80Run(Cpu.Z80, OptimizationPresets.NodeOpt, Z80OptimizationPresets.Good)