package millfork.test.emu

import com.codingrodent.microprocessor.IMemory
import millfork.output.MemoryBank

/**
  * @author Karol Stasiak
  */
case class Z80Memory(memoryBank: MemoryBank) extends IMemory {

  override def readByte(address: Int): Int = memoryBank.readByte(address)

  override def readWord(address: Int): Int = memoryBank.readWord(address)

  override def writeByte(address: Int, data: Int): Unit = {
//    if (!memoryBank.writeable(address)) throw new RuntimeException("Can't write to $" + address.toHexString)
    memoryBank.output(address) = data.toByte
  }

  override def writeWord(address: Int, data: Int): Unit = {
    writeByte(address, data)
    writeByte(address + 1, data >> 8)
  }
}
