package millfork.output

import java.nio.charset.StandardCharsets

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
object D88Output extends OutputPackager {

  private def bootloader(targetAddress: Int, sectorCount: Int): Array[Byte] = Array(
    0x21, targetAddress, targetAddress >> 8, 0x01, 0x00, sectorCount, 0x11, 0x02, 0x00, 0xCD, 0x0F, 0xC0, 0xC3, targetAddress, targetAddress >> 8, 0x3E,
    0x02, 0xCD, 0x41, 0xC0, 0x3E, 0x11, 0x93, 0xB8, 0x38, 0x01, 0x78, 0xCD, 0x52, 0xC0, 0x79, 0xCD,
    0x52, 0xC0, 0x7A, 0xCD, 0x52, 0xC0, 0x7B, 0xCD, 0x52, 0xC0, 0x3E, 0x12, 0xCD, 0x41, 0xC0, 0xCD,
    0x71, 0xC0, 0x1C, 0x7B, 0xFE, 0x11, 0x28, 0x03, 0x10, 0xF5, 0xC9, 0x14, 0x1E, 0x01, 0x10, 0xCF,
    0xC9, 0xF5, 0x3E, 0x0F, 0xD3, 0xFF, 0xDB, 0xFE, 0xE6, 0x02, 0x28, 0xFA, 0x3E, 0x0E, 0xD3, 0xFF,
    0x18, 0x07, 0xF5, 0xDB, 0xFE, 0xE6, 0x02, 0x28, 0xFA, 0xF1, 0xD3, 0xFD, 0x3E, 0x09, 0xD3, 0xFF,
    0xDB, 0xFE, 0xE6, 0x04, 0x28, 0xFA, 0x3E, 0x08, 0xD3, 0xFF, 0xDB, 0xFE, 0xE6, 0x04, 0x20, 0xFA,
    0xC9, 0xC5, 0xD5, 0x01, 0xFC, 0x00, 0x11, 0x0C, 0x0A, 0x3E, 0x0B, 0xD3, 0xFF, 0xDB, 0xFE, 0x0F,
    0x30, 0xFB, 0x7A, 0xD3, 0xFF, 0xED, 0xA2, 0x3E, 0x0D, 0xD3, 0xFF, 0xDB, 0xFE, 0x0F, 0x38, 0xFB,
    0xED, 0xA2, 0x7B, 0xD3, 0xFF, 0xC2, 0x79, 0xC0, 0xD1, 0xC1, 0xC9, 0xC9
  ).map(_.toByte)


  override def packageOutput(mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(bank)
    val start = b.start
    val header = new D88Header(mem.programName.take(16))
    val trackList = new D88TrackList
    val sectors = mutable.ListBuffer[D88Sector]()
    var trackOffset = 688
    var cylinder = 0
    var head = 0
    var sector = 1
    var trackCount = 0

    @inline def addSector(data: Array[Byte]): Unit = {
      val s = D88Sector(cylinder, head, sector, data)
      if (sector == 1) {
        trackList.offsets(trackCount) = trackOffset
        trackCount += 1
      }
      sectors += s
      trackOffset += s.length
      sector += 1
      if (sector == 17) {
        sector = 1
        head += 1
        if (head == 2) {
          cylinder += 1
          head = 0
        }
      }
    }

    val size = b.end + 1 - b.start
    val sizeInPages = size.|(0xff).+(1).>>(8)
    addSector(bootloader(b.start, sizeInPages))
    for (page <- 0 until sizeInPages) {
      val pageStart = b.start + (page << 8)
      val pagePastEnd = pageStart + 0x100
      b.markAsOutputted(pageStart, pagePastEnd)
      addSector(b.output.slice(pageStart, pagePastEnd))
    }
    header.totalSize = trackOffset
    sectors.map(_.toArray).foldLeft(header.toArray ++ trackList.toArray)(_ ++ _)
  }
}

sealed trait D88Part {
  def toArray: Array[Byte]
}

class D88Header(programName: String) extends D88Part {
  var totalSize: Long = 0

  def isAlphanum(c: Char): Boolean = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')

  def toArray: Array[Byte] = {
    programName.map(c =>
      if (c == 0 || isAlphanum(c)) c.toByte else '_'.toByte
    ).padTo(16, 0.toByte).toArray ++ Array(
      0, // NUL
      0, 0, 0, 0, 0, 0, 0, 0, 0, // reserved
      0, // not write protected
      0, // 2D
      totalSize,
      totalSize >> 8,
      totalSize >> 16,
      totalSize >> 24
    ).map(_.toByte)
  }
}

class D88TrackList extends D88Part {
  val offsets: Array[Int] = Array.fill[Int](164)(0)

  def toArray: Array[Byte] = {
    offsets.flatMap(i => Array[Byte](
      i.toByte,
      i.>>(8).toByte,
      i.>>(16).toByte,
      i.>>(24).toByte
    ))
  }
}

case class D88Sector(cylinder: Int, head: Int, sector: Int, data: Array[Byte]) extends D88Part {
  def length: Int = 16 + 256

  override def toArray: Array[Byte] = {
    val header = Array(
      cylinder, head, sector,
      1, // 256B
      16, 0, // sectors per track
      0, // double density
      0, // not deleted
      0, // no crc error
      0, 0, 0, 0, 0, // reserved
      0, 1 // data size
    ).map(_.toByte)
    val padding = Array.fill[Byte]((256 - data.length.&(0xff)).&(0xff))(-1)
    header ++ data ++ padding
  }
}