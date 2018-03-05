package millfork.test.emu

import millfork.assembly.opt.SuperOptimizer
import millfork.{Cpu, OptimizationPresets}

/**
  * @author Karol Stasiak
  */
object EmuSuperOptimizedInliningRun extends EmuRun(
  Cpu.StrictMos,
  OptimizationPresets.NodeOpt,
  List(SuperOptimizer)) {
  override def inline = true
}



