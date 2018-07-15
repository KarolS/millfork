package millfork.test.emu

import millfork.assembly.mos.opt.{LaterOptimizations, ZeropageRegisterOptimizations}
import millfork.assembly.z80.opt.Z80OptimizationPresets
import millfork.{Cpu, OptimizationPresets}

/**
  * @author Karol Stasiak
  */
object EmuOptimizedInlinedRun extends EmuRun(
  Cpu.StrictMos,
  OptimizationPresets.NodeOpt,
  OptimizationPresets.AssOpt ++
    ZeropageRegisterOptimizations.All ++
    OptimizationPresets.Good ++
    OptimizationPresets.Good ++
    OptimizationPresets.Good ++ LaterOptimizations.Nmos ++
    OptimizationPresets.Good ++ LaterOptimizations.Nmos ++
    ZeropageRegisterOptimizations.All ++
    OptimizationPresets.Good) {
  override def inline: Boolean = true
  override def blastProcessing: Boolean = true
}


object EmuOptimizedInlinedZ80Run extends EmuZ80Run(Cpu.Z80, OptimizationPresets.NodeOpt, Z80OptimizationPresets.Good) {
  override def inline: Boolean = true
}


