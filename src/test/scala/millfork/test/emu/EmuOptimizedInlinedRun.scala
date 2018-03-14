package millfork.test.emu

import millfork.assembly.opt.{LaterOptimizations, ZeropageRegisterOptimizations}
import millfork.{Cpu, OptimizationPresets}

/**
  * @author Karol Stasiak
  */
object EmuOptimizedInlinedRun extends EmuRun(
  Cpu.StrictMos,
  OptimizationPresets.NodeOpt,
  OptimizationPresets.AssOpt ++
    ZeropageRegisterOptimizations.All ++
    OptimizationPresets.Good ++ LaterOptimizations.Nmos ++
    OptimizationPresets.Good ++ LaterOptimizations.Nmos ++
    ZeropageRegisterOptimizations.All ++
    OptimizationPresets.Good) {
  override def inline: Boolean = true
  override def blastProcessing: Boolean = true
}



