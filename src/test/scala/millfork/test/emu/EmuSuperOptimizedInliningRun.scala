package millfork.test.emu

import millfork.assembly.opt.SuperOptimizer
import millfork.{Cpu, OptimizationPresets}

/**
  * @author Karol Stasiak
  */
object EmuSuperQuantumOptimizedInliningRun extends EmuRun(
  Cpu.StrictMos,
  OptimizationPresets.NodeOpt,
  List(SuperOptimizer),
  true) {
  override def inline = true
}



