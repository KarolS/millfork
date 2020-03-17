package millfork.test.emu

import millfork.assembly.m6809.opt.M6809OptimizationPresets
import millfork.assembly.mos.opt.{LaterOptimizations, NmosOptimizations, ZeropageRegisterOptimizations}
import millfork.assembly.z80.opt.{AlwaysGoodZ80Optimizations, Z80OptimizationPresets}
import millfork.{Cpu, OptimizationPresets}

/**
  * @author Karol Stasiak
  */
object EmuOptimizedRun extends EmuRun(
  Cpu.StrictMos,
  OptimizationPresets.NodeOpt,
  OptimizationPresets.AssOpt ++
    ZeropageRegisterOptimizations.All ++
    OptimizationPresets.Good ++
    OptimizationPresets.Good ++
    OptimizationPresets.Good ++ NmosOptimizations.All ++
    OptimizationPresets.Good ++ NmosOptimizations.All ++
    ZeropageRegisterOptimizations.All ++
    OptimizationPresets.Good ++
    ZeropageRegisterOptimizations.All ++
    OptimizationPresets.Good ++
    ZeropageRegisterOptimizations.All ++
    OptimizationPresets.Good ++
    OptimizationPresets.Good)

case class EmuOptimizedAccordingToLevelRun(optLevel: Int) extends EmuRun(
  Cpu.Ricoh,
  OptimizationPresets.NodeOpt,
  optLevel match {
    case 0 => Nil
    case 1 => OptimizationPresets.QuickPreset
    case _ =>
      val goodExtras = ZeropageRegisterOptimizations.All
      val extras = Nil
      val goodCycle = List.fill(optLevel - 2)(OptimizationPresets.Good ++ goodExtras).flatten
      val mainCycle = List.fill(optLevel - 1)(OptimizationPresets.AssOpt ++ extras).flatten
      goodCycle ++ mainCycle ++ goodCycle
  }
)

object EmuSizeOptimizedRun extends EmuRun(
  Cpu.StrictMos,
  OptimizationPresets.NodeOpt,
  OptimizationPresets.AssOpt ++
    ZeropageRegisterOptimizations.All ++
    OptimizationPresets.Good ++
    OptimizationPresets.Good ++
    OptimizationPresets.Good ++ NmosOptimizations.All ++
    OptimizationPresets.Good ++ NmosOptimizations.All ++
    ZeropageRegisterOptimizations.All ++
    OptimizationPresets.Good ++
    ZeropageRegisterOptimizations.All ++
    OptimizationPresets.Good ++
    ZeropageRegisterOptimizations.All ++
    OptimizationPresets.Good ++
    OptimizationPresets.Good) {
  override def optimizeForSize = true
}

object EmuOptimizedSoftwareStackRun extends EmuRun(
  Cpu.StrictMos,
  OptimizationPresets.NodeOpt,
  OptimizationPresets.AssOpt ++
    ZeropageRegisterOptimizations.All ++
    OptimizationPresets.Good ++
    OptimizationPresets.Good ++
    OptimizationPresets.Good ++ NmosOptimizations.All ++
    OptimizationPresets.Good ++ NmosOptimizations.All ++
    ZeropageRegisterOptimizations.All ++
    OptimizationPresets.Good ++
    ZeropageRegisterOptimizations.All ++
    OptimizationPresets.Good ++
    ZeropageRegisterOptimizations.All ++
    OptimizationPresets.Good ++
    OptimizationPresets.Good) {
  override def softwareStack = true
}


object EmuOptimizedZ80Run extends EmuZ80Run(Cpu.Z80, OptimizationPresets.NodeOpt, Z80OptimizationPresets.GoodForZ80)

object EmuSizeOptimizedZ80Run extends EmuZ80Run(Cpu.Z80, OptimizationPresets.NodeOpt, Z80OptimizationPresets.GoodForZ80) {
  override def optimizeForSize = true
}

object EmuOptimizedIntel8080Run extends EmuZ80Run(Cpu.Intel8080, OptimizationPresets.NodeOpt, Z80OptimizationPresets.GoodForIntel8080)

object EmuOptimizedIntel8086Run extends EmuI86Run(OptimizationPresets.NodeOpt, Z80OptimizationPresets.GoodForIntel8080)

object EmuSizeOptimizedIntel8080Run extends EmuZ80Run(Cpu.Intel8080, OptimizationPresets.NodeOpt, Z80OptimizationPresets.GoodForIntel8080) {
  override def optimizeForSize = true
}

object EmuOptimizedSharpRun extends EmuZ80Run(Cpu.Sharp, OptimizationPresets.NodeOpt, Z80OptimizationPresets.GoodForSharp)

object EmuOptimizedM6809Run extends EmuM6809Run(Cpu.Motorola6809, OptimizationPresets.NodeOpt, M6809OptimizationPresets.Default)
