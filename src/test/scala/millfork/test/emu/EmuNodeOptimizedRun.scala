package millfork.test.emu

import millfork.Cpu
import millfork.node.opt.{UnreachableCode, UnusedLocalVariables}

/**
  * @author Karol Stasiak
  */
object EmuNodeOptimizedRun extends EmuRun(
  Cpu.StrictMos,
  List(
    UnreachableCode,
    UnusedLocalVariables),
  Nil)