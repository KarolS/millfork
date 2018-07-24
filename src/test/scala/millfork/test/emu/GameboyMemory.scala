package millfork.test.emu

import eu.rekawek.coffeegb.AddressSpace
import millfork.output.MemoryBank

/**
  * @author Karol Stasiak
  */
case class GameboyMemory(memoryBank: MemoryBank) extends AddressSpace {
  override def accepts(i: Int): Boolean = true

  override def setByte(address: Int, data: Int): Unit = {
    //    if (!memoryBank.writeable(address)) throw new RuntimeException("Can't write to $" + address.toHexString)
        memoryBank.output(address) = data.toByte
  }

  override def getByte(address: Int): Int = memoryBank.readByte(address)
}
