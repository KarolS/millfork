package millfork.test.emu

import millfork.assembly.opt.{CmosOptimizations, HudsonOptimizations}
import millfork.{Cpu, OptimizationPresets}

/**
  * @author Karol Stasiak
  */
object EmuOptimizedHudsonRun extends EmuRun(
  Cpu.HuC6280,
  OptimizationPresets.NodeOpt,
  OptimizationPresets.AssOpt ++
    CmosOptimizations.All ++ HudsonOptimizations.All ++ OptimizationPresets.Good ++
    CmosOptimizations.All ++ HudsonOptimizations.All ++ OptimizationPresets.Good ++
    CmosOptimizations.All ++ HudsonOptimizations.All ++ OptimizationPresets.Good,
  false)



