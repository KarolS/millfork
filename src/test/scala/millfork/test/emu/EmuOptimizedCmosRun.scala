package millfork.test.emu

import millfork.assembly.mos.opt.{CmosOptimizations, ZeropageRegisterOptimizations}
import millfork.{Cpu, OptimizationPresets}

/**
  * @author Karol Stasiak
  */
object EmuOptimizedCmosRun extends EmuRun(
  Cpu.Cmos,
  OptimizationPresets.NodeOpt,
  OptimizationPresets.AssOpt ++
    ZeropageRegisterOptimizations.All ++
    CmosOptimizations.All ++ OptimizationPresets.Good ++
    CmosOptimizations.All ++ OptimizationPresets.Good ++
    ZeropageRegisterOptimizations.All ++
    CmosOptimizations.All ++ OptimizationPresets.Good)



