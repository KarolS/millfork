package millfork.output

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
class CompiledMemory(bankNames: List[(String, Int)], bankFills: Map[String, Int], bigEndian: Boolean) {
  var programName = "MILLFORK"
  val banks: mutable.Map[String, MemoryBank] = mutable.Map(bankNames.map{p =>
    val bank = new MemoryBank(p._2, bigEndian)
    bank.fill(bankFills.getOrElse(p._1, 0))
    p._1 -> bank
  }: _*)
}

class MemoryBank(val index: Int, val isBigEndian: Boolean) {

  def fill(value: Int): Unit = {
    output.indices.foreach(i => output(i) = value.toByte)
  }

  def readByte(addr: Int): Int = output(addr) & 0xff

  def readWord(addr: Int): Int =
    if (isBigEndian) readByte(addr + 1) + (readByte(addr) << 8)
    else readByte(addr) + (readByte(addr + 1) << 8)

  def readMedium(addr: Int): Int =
    if (isBigEndian) readByte(addr + 2) + (readByte(addr + 1) << 8) + (readByte(addr) << 16)
    else readByte(addr) + (readByte(addr + 1) << 8) + (readByte(addr + 2) << 16)

  def readLong(addr: Int): Int =
    if (isBigEndian) readByte(addr + 3) + (readByte(addr + 2) << 8) + (readByte(addr + 1) << 16) + (readByte(addr) << 24)
    else readByte(addr) + (readByte(addr + 1) << 8) + (readByte(addr + 2) << 16) + (readByte(addr + 3) << 24)

  def readWord(addrHi: Int, addrLo: Int): Int = readByte(addrLo) + (readByte(addrHi) << 8)

  val output: Array[Byte] = Array.fill[Byte](1 << 16)(0)
  val occupied: Array[Boolean] = Array.fill(1 << 16)(false)
  val initialized: Array[Boolean] = Array.fill(1 << 16)(false)
  val readable: Array[Boolean] = Array.fill(1 << 16)(false)
  val writeable: Array[Boolean] = Array.fill(1 << 16)(false)
  var start: Int = 0
  var end: Int = 0

  def dump(startAddr: Int, count: Int)(dumper: String => Any): Unit = {
    (0 until count).map(i => (i + startAddr) -> output(i + startAddr)).grouped(16).zipWithIndex.map { case (c, i) => f"${c.head._1}%04X: " + c.map(i => f"${i._2}%02x").mkString(" ") }.foreach(dumper)
  }
}
