package millfork.test.emu

import eu.rekawek.coffeegb.AddressSpace
import eu.rekawek.coffeegb.cpu.{Cpu, InterruptManager, SpeedMode}
import eu.rekawek.coffeegb.gpu.{Display, Gpu}
import eu.rekawek.coffeegb.memory.{Dma, Ram}
import millfork.output.MemoryBank

/**
  * @author Karol Stasiak
  */
case class GameboyStubs(memoryBank: MemoryBank) {
  val display: Display = Display.NULL_DISPLAY

  val interruptManager: InterruptManager = new InterruptManager(false)
  interruptManager.disableInterrupts(false)

  val addressSpace: AddressSpace = GameboyMemory(memoryBank)

  val oamRam: Ram = new Ram(0, 0)

  val speedMode: SpeedMode = new SpeedMode

  val dma: Dma = new Dma(addressSpace, oamRam, speedMode)

  val gpu: Gpu = new Gpu(display, interruptManager, dma, oamRam, false)

  val cpu: Cpu = new Cpu(addressSpace, interruptManager, gpu, display, speedMode)
}
