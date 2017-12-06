package millfork.test.emu

import com.loomcom.symon.devices.Device
import millfork.output.MemoryBank

/**
  * @author Karol Stasiak
  */
class SymonTestRam(mem: MemoryBank) extends Device(0x0000, 0xffff, "RAM") {

  mem.readable(1) = true
  mem.readable(2) = true

  (0x100 to 0x1ff).foreach { stack =>
    mem.writeable(stack) = true
    mem.readable(stack) = true
  }

  (0xc000 to 0xcfff).foreach { himem =>
    mem.writeable(himem) = true
    mem.readable(himem) = true
  }

  override def write(i: Int, i1: Int): Unit = {
    if (!mem.writeable(i)) {
      throw new RuntimeException(s"Can't write to $$${i.toHexString}")
    }
    mem.output(i) = i1.toByte
  }

  override def read(i: Int, b: Boolean): Int = {
    if (!mem.readable(i)) {
      throw new RuntimeException(s"Can't read from $$${i.toHexString}")
    }
    mem.output(i)
  }

  override def toString: String = "TestRam"
}
