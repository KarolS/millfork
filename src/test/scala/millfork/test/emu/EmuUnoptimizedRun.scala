package millfork.test.emu

import millfork.Cpu


/**
  * @author Karol Stasiak
  */
object EmuUnoptimizedRun extends EmuRun(Cpu.StrictMos, Nil, Nil)

object EmuUnoptimizedZ80Run extends EmuZ80Run(Cpu.Z80, Nil, Nil)