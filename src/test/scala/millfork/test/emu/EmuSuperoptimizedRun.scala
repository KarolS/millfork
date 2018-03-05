package millfork.test.emu

import millfork.assembly.opt.SuperOptimizer
import millfork.{Cpu, OptimizationPresets}

/**
  * @author Karol Stasiak
  */
object EmuSuperOptimizedRun extends EmuRun(
  Cpu.StrictMos,
  OptimizationPresets.NodeOpt,
  List(SuperOptimizer))



