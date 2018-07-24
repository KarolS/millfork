package millfork.output

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
class CompiledMemory(bankNames: List[String]) {
  var programName = "MILLFORK"
  val banks = mutable.Map(bankNames.map(_ -> new MemoryBank): _*)
}

class MemoryBank {
  def readByte(addr: Int) = output(addr) & 0xff

  def readWord(addr: Int) = readByte(addr) + (readByte(addr + 1) << 8)

  def readMedium(addr: Int) = readByte(addr) + (readByte(addr + 1) << 8) + (readByte(addr + 2) << 16)

  def readLong(addr: Int) = readByte(addr) + (readByte(addr + 1) << 8) + (readByte(addr + 2) << 16) + (readByte(addr + 3) << 24)

  def readWord(addrHi: Int, addrLo: Int) = readByte(addrLo) + (readByte(addrHi) << 8)

  val output = Array.fill[Byte](1 << 16)(0)
  val occupied = Array.fill(1 << 16)(false)
  val initialized = Array.fill(1 << 16)(false)
  val readable = Array.fill(1 << 16)(false)
  val writeable = Array.fill(1 << 16)(false)
  var start: Int = 0
  var end: Int = 0

  def dump(startAddr: Int, count: Int)(dumper: String => Any): Unit = {
    (0 until count).map(i => (i + startAddr) -> output(i + startAddr)).grouped(16).zipWithIndex.map { case (c, i) => f"${c.head._1}%04X: " + c.map(i => f"${i._2}%02x").mkString(" ") }.foreach(dumper)
  }
}
