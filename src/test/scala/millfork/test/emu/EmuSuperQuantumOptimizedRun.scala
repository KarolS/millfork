package millfork.test.emu

import millfork.assembly.opt.SuperOptimizer
import millfork.{Cpu, OptimizationPresets}

/**
  * @author Karol Stasiak
  */
// TODO : it doesn't work
object EmuSuperQuantumOptimizedRun extends EmuRun(
  Cpu.StrictMos,
  OptimizationPresets.NodeOpt,
  List(SuperOptimizer),
  true)



