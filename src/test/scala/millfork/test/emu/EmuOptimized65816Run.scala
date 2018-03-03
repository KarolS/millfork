package millfork.test.emu

import millfork.assembly.opt.{CmosOptimizations, SixteenOptimizations}
import millfork.{Cpu, OptimizationPresets}

/**
  * @author Karol Stasiak
  */
object EmuOptimized65816Run extends EmuRun(
  Cpu.Sixteen,
  OptimizationPresets.NodeOpt,
  OptimizationPresets.AssOpt ++
    CmosOptimizations.All ++ SixteenOptimizations.All ++ OptimizationPresets.Good ++
    CmosOptimizations.All ++ SixteenOptimizations.All ++ OptimizationPresets.Good ++
    CmosOptimizations.All ++ SixteenOptimizations.All ++ OptimizationPresets.Good ++
    CmosOptimizations.All ++ SixteenOptimizations.All ++ OptimizationPresets.Good ++
    CmosOptimizations.All ++ SixteenOptimizations.All ++ OptimizationPresets.Good,
  false)



