package millfork.test.emu

import millfork.assembly.opt.{CE02Optimizations, CmosOptimizations}
import millfork.{Cpu, OptimizationPresets}

/**
  * @author Karol Stasiak
  */
object EmuOptimized65CE02Run extends EmuRun(
  Cpu.CE02,
  OptimizationPresets.NodeOpt,
  OptimizationPresets.AssOpt ++
    CmosOptimizations.All ++ CE02Optimizations.All ++ OptimizationPresets.Good ++
    CmosOptimizations.All ++ CE02Optimizations.All ++ OptimizationPresets.Good ++
    CmosOptimizations.All ++ CE02Optimizations.All ++ OptimizationPresets.Good)



