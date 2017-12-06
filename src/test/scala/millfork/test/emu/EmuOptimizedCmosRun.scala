package millfork.test.emu

import millfork.assembly.opt.CmosOptimizations
import millfork.{Cpu, OptimizationPresets}

/**
  * @author Karol Stasiak
  */
object EmuOptimizedCmosRun extends EmuRun(
  Cpu.Cmos,
  OptimizationPresets.NodeOpt,
  OptimizationPresets.AssOpt ++
    CmosOptimizations.All ++ OptimizationPresets.Good ++
    CmosOptimizations.All ++ OptimizationPresets.Good ++
    CmosOptimizations.All ++ OptimizationPresets.Good,
  false)



