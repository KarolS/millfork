package millfork.test.emu

import millfork.assembly.m6809.opt.M6809OptimizationPresets
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
    OptimizationPresets.AssOpt ++
    OptimizationPresets.Good ++ LaterOptimizations.Nmos ++
    ZeropageRegisterOptimizations.All ++
    OptimizationPresets.AssOpt ++
    OptimizationPresets.Good ++ LaterOptimizations.Nmos ++
    ZeropageRegisterOptimizations.All ++
    OptimizationPresets.AssOpt ++
    OptimizationPresets.AssOpt ++
    OptimizationPresets.AssOpt ++
    OptimizationPresets.Good ++
    ZeropageRegisterOptimizations.All ++
    OptimizationPresets.Good ++
    ZeropageRegisterOptimizations.All ++
    OptimizationPresets.AssOpt) {
  override def inline: Boolean = true
  override def blastProcessing: Boolean = true
}


object EmuOptimizedInlinedZ80Run extends EmuZ80Run(Cpu.Z80, OptimizationPresets.NodeOpt, Z80OptimizationPresets.GoodForZ80) {
  override def inline: Boolean = true
}

object EmuOptimizedInlinedIntel8080Run extends EmuZ80Run(Cpu.Intel8080, OptimizationPresets.NodeOpt, Z80OptimizationPresets.GoodForIntel8080) {
  override def inline: Boolean = true
}

object EmuOptimizedInlinedIntel8086Run extends EmuI86Run(OptimizationPresets.NodeOpt, Z80OptimizationPresets.GoodForIntel8080) {
  override def inline: Boolean = true
}

object EmuOptimizedInlinedSharpRun extends EmuZ80Run(Cpu.Sharp, OptimizationPresets.NodeOpt, Z80OptimizationPresets.GoodForSharp) {
  override def inline: Boolean = true
}

object EmuOptimizedInlinedM6809Run extends EmuM6809Run(Cpu.Motorola6809, OptimizationPresets.NodeOpt, M6809OptimizationPresets.Default) {
  override def inline: Boolean = true
}

