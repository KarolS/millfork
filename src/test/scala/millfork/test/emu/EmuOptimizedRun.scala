package millfork.test.emu

import millfork.assembly.opt.LaterOptimizations
import millfork.{Cpu, OptimizationPresets}

/**
  * @author Karol Stasiak
  */
object EmuOptimizedRun extends EmuRun(
  Cpu.StrictMos,
  OptimizationPresets.NodeOpt,
  OptimizationPresets.AssOpt ++
    OptimizationPresets.Good ++ LaterOptimizations.Nmos ++
    OptimizationPresets.Good ++ LaterOptimizations.Nmos ++
    OptimizationPresets.Good,
  false)



