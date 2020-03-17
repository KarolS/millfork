package millfork.test.emu

import millfork.assembly.mos.opt.{NmosOptimizations, UndocumentedOptimizations}
import millfork.{Cpu, OptimizationPresets}

/**
  * @author Karol Stasiak
  */
object EmuUndocumentedRun extends EmuRun(
  Cpu.Ricoh, // not Cpu.Mos, because I haven't found an emulator that supports both illegals and decimal mode yet
  OptimizationPresets.NodeOpt,
  OptimizationPresets.AssOpt ++ NmosOptimizations.All ++
    UndocumentedOptimizations.All ++
    OptimizationPresets.Good ++ NmosOptimizations.All ++
    UndocumentedOptimizations.All ++
    OptimizationPresets.AssOpt ++ NmosOptimizations.All ++
    UndocumentedOptimizations.All ++
    OptimizationPresets.AssOpt ++ NmosOptimizations.All ++
    UndocumentedOptimizations.All ++
    OptimizationPresets.Good) {

  override def emitIllegals = true
}



