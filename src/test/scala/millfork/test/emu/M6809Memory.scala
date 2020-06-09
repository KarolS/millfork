package millfork.test.emu

import millfork.output.MemoryBank
import org.roug.osnine.{Bus8Motorola, MemorySegment}

/**
  * @author Karol Stasiak
  */
class M6809Memory(memoryBank: MemoryBank, resetVector: Int) extends MemorySegment(0, 0xffff) {

  for(i <- 0xc000 to 0xffff) {
    memoryBank.readable(i) = true
    memoryBank.writeable(i) = true
  }

  for(i <- 0 to 9) {
    memoryBank.readable(i) = true
    memoryBank.writeable(i) = true
  }
  memoryBank.output(0xfffe) = resetVector.>>(8).toByte
  memoryBank.output(0xffff) = resetVector.toByte

  override def load(addr: Int): Int = {
    if (!memoryBank.readable(addr)) {
      println(s"Accessing memory for read at $$${addr.toHexString}")
      ???
    }
    memoryBank.readByte(addr)
  }

  override def store(addr: Int, `val`: Int): Unit = {
    if (!memoryBank.writeable(addr)) {
      val start = addr & 0xff00
      (0 until 0x100).grouped(16).map(range => (start + range.head).toHexString + range.map(i => memoryBank.output(start + i)).map(v => f" $v%02X").mkString("")).foreach(println)
      println(s"Accessing memory for write at $$${addr.toHexString}, writing $$${`val`.toHexString}")
      ???
    }
    memoryBank.output(addr) = `val`.toByte
  }
}
