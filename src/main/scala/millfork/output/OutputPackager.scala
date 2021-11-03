package millfork.output

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.util.Locale
import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
trait OutputPackager {

  def packageOutputAndLayout(mem: CompiledMemory, bank: String): (Array[Byte], BankLayoutInFile) = {
    val startInFile = mutable.Map[String, Int]()
    val firstAddress = mutable.Map[String, Int]()
    val fileLayoutCollector = new FileLayoutCollector(startInFile, firstAddress)
    val output = packageOutput(fileLayoutCollector, mem, bank)
    output -> fileLayoutCollector.toBankLayoutInFile
  }

  def packageOutput(flc: FileLayoutCollector, mem: CompiledMemory, bank: String): Array[Byte]

  def writes(b: String): Boolean = false
}

class FileLayoutCollector(startInFile: mutable.Map[String, Int],
                          firstAddress: mutable.Map[String, Int],
                          offset: Int = 0) {
  def reportBankChunk(bank: String, extraOffset: Int, addr: Int): Unit = {
    if (!startInFile.contains(bank)) {
      println(s"bank $bank starts at $offset + $extraOffset")
      startInFile(bank) = offset + extraOffset
    }
    if (!firstAddress.contains(bank)) {
      firstAddress(bank) = addr
    }
  }

  def +(deltaOffset: Int) = new FileLayoutCollector(startInFile, firstAddress, offset + deltaOffset)

  def toBankLayoutInFile = new BankLayoutInFile(startInFile.toMap, firstAddress.toMap)
}

case class SequenceOutput(children: List[OutputPackager]) extends OutputPackager {
  def packageOutput(flc: FileLayoutCollector, mem: CompiledMemory, bank: String): Array[Byte] = {
    val baos = new ByteArrayOutputStream
    var f = flc
    for (c <- children) {
      val a = c.packageOutput(f, mem, bank)
      baos.write(a, 0, a.length)
      f += a.length
    }
    baos.toByteArray
  }

  override def writes(b: String): Boolean = children.exists(_.writes(b))
}

case class ConstOutput(byte: Byte) extends OutputPackager {
  def packageOutput(flc: FileLayoutCollector, mem: CompiledMemory, bank: String): Array[Byte] = Array(byte)
}

case class CurrentBankFragmentOutput(start: Int, end: Int) extends OutputPackager {
  def packageOutput(flc: FileLayoutCollector, mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(bank)
    b.markAsOutputted(start, end + 1)
    flc.reportBankChunk(bank, 0, start)
    b.output.slice(start, end + 1)
  }
}

case class BankFragmentOutput(alwaysBank: String, start: Int, end: Int) extends OutputPackager {
  def packageOutput(flc: FileLayoutCollector, mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(alwaysBank)
    b.markAsOutputted(start, end + 1)
    flc.reportBankChunk(alwaysBank, 0, start)
    b.output.slice(start, end + 1)
  }

  override def writes(b: String): Boolean = b == alwaysBank
}

case class ProgramNameOutput(length: Int) extends OutputPackager {
  def isAlphanum(c: Char): Boolean = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')
  def packageOutput(flc: FileLayoutCollector, mem: CompiledMemory, bank: String): Array[Byte] = {
    mem.programName.toUpperCase(Locale.ROOT).filter(isAlphanum).take(length).padTo(length, ' ').getBytes(StandardCharsets.US_ASCII)
  }
}

case class StringOutput(string: String) extends OutputPackager {
  def packageOutput(flc: FileLayoutCollector, mem: CompiledMemory, bank: String): Array[Byte] = {
    string.getBytes(StandardCharsets.US_ASCII)
  }
}

case class StartAddressOutput(bonus: Int) extends OutputPackager {
  def packageOutput(flc: FileLayoutCollector, mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(bank)
    val x = b.start + bonus
    Array(x.toByte, x.>>(8).toByte)
  }
}

case class StartAddressOutputBe(bonus: Int) extends OutputPackager {
  def packageOutput(flc: FileLayoutCollector, mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(bank)
    val x = b.start + bonus
    Array(x.>>(8).toByte, x.toByte)
  }
}

object StartPageOutput extends OutputPackager {
  def packageOutput(flc: FileLayoutCollector, mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(bank)
    Array(b.start.>>(8).toByte)
  }
}

case class EndAddressOutput(bonus: Int) extends OutputPackager {
  def packageOutput(flc: FileLayoutCollector, mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(bank)
    val x = b.end + bonus
    Array(x.toByte, x.>>(8).toByte)
  }
}

case class EndAddressOutputBe(bonus: Int) extends OutputPackager {
  def packageOutput(flc: FileLayoutCollector, mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(bank)
    val x = b.end + bonus
    Array(x.>>(8).toByte, x.toByte)
  }
}


case class SymbolAddressOutput(symbol: String, bonus: Int) extends OutputPackager {
  def packageOutput(flc: FileLayoutCollector, mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(bank)
    val x = mem.getAddress(symbol) + bonus
    Array(b.end.toByte, b.end.>>(8).toByte)
  }
}

case class SymbolAddressOutputBe(symbol: String, bonus: Int) extends OutputPackager {
  def packageOutput(flc: FileLayoutCollector, mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(bank)
    val x = mem.getAddress(symbol) + bonus
    Array(x.>>(8).toByte, x.toByte)
  }
}

object PageCountOutput extends OutputPackager {
  def packageOutput(flc: FileLayoutCollector, mem: CompiledMemory, bank: String): Array[Byte] = {
    val e = mem.banks(bank).end.>>(8)
    val s = mem.banks(bank).start.>>(8)
    Array((e - s + 1).toByte)
  }
}

object AllocatedDataOutput extends OutputPackager {
  def packageOutput(flc: FileLayoutCollector, mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(bank)
    b.markAsOutputted(b.start, b.end + 1)
    flc.reportBankChunk(bank, 0, b.start)
    b.output.slice(b.start, b.end + 1)
  }
}

case class AllocatedDataLength(bonus: Int) extends OutputPackager {
  def packageOutput(flc: FileLayoutCollector, mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(bank)
    val size = b.end - b.start + 1 + bonus
    Array(size.toByte, size.>>(8).toByte)
  }
}

case class AllocatedDataLengthBe(bonus: Int) extends OutputPackager {
  def packageOutput(flc: FileLayoutCollector, mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(bank)
    val size = b.end - b.start + 1 + bonus
    Array(size.>>(8).toByte, size.toByte)
  }
}
