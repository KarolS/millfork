package millfork.test.emu

import millfork.Cpu


/**
  * @author Karol Stasiak
  */
object EmuUnoptimizedRun extends EmuRun(Cpu.StrictMos, Nil, Nil)

object EmuUnoptimizedRicohRun extends EmuRun(Cpu.Ricoh, Nil, Nil)

object EmuUnoptimizedCmosRun extends EmuRun(Cpu.Cmos, Nil, Nil)

object EmuUnoptimizedHudsonRun extends EmuRun(Cpu.HuC6280, Nil, Nil)

object EmuUnoptimizedNative65816Run extends EmuRun(Cpu.Sixteen, Nil, Nil) {
  override def native16: Boolean = true
}

object EmuUnoptimizedZ80Run extends EmuZ80Run(Cpu.Z80, Nil, Nil)

object EmuUnoptimizedIntel8080Run extends EmuZ80Run(Cpu.Intel8080, Nil, Nil)

object EmuUnoptimizedIntel8085Run extends EmuZ80Run(Cpu.Intel8085, Nil, Nil)

object EmuUnoptimizedZ80NextRun extends EmuZ80Run(Cpu.Z80Next, Nil, Nil)

object EmuUnoptimizedIntel8086Run extends EmuI86Run(Nil, Nil)

object EmuUnoptimizedSharpRun extends EmuZ80Run(Cpu.Sharp, Nil, Nil)

object EmuUnoptimizedM6809Run extends EmuM6809Run(Cpu.Motorola6809, Nil, Nil)
