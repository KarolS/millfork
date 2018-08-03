package millfork.test.emu

import millfork.Cpu


/**
  * @author Karol Stasiak
  */
object EmuUnoptimizedRun extends EmuRun(Cpu.StrictMos, Nil, Nil)

object EmuUnoptimizedRicohRun extends EmuRun(Cpu.Ricoh, Nil, Nil)

object EmuUnoptimizedCmosRun extends EmuRun(Cpu.Cmos, Nil, Nil)

object EmuUnoptimizedZ80Run extends EmuZ80Run(Cpu.Z80, Nil, Nil)

object EmuUnoptimizedIntel8080Run extends EmuZ80Run(Cpu.Intel8080, Nil, Nil)

object EmuUnoptimizedSharpRun extends EmuZ80Run(Cpu.Sharp, Nil, Nil)
