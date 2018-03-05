package millfork.test.emu

import millfork.assembly.opt.{LaterOptimizations, ZeropageRegisterOptimizations}
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
    OptimizationPresets.Good,
  false)



