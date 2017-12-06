package millfork.test.emu

import millfork.assembly.opt.UndocumentedOptimizations
import millfork.{Cpu, OptimizationPresets}

/**
  * @author Karol Stasiak
  */
object EmuUndocumentedRun extends EmuRun(
  Cpu.Ricoh, // not Cpu.Mos, because I haven't found an emulator that supports both illegals and decimal mode yet
  OptimizationPresets.NodeOpt,
  OptimizationPresets.AssOpt ++ UndocumentedOptimizations.All ++ OptimizationPresets.Good ++ UndocumentedOptimizations.All ++ OptimizationPresets.Good,
  false) {

  override def emitIllegals = true
}



