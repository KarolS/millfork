package millfork.output

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.util.Locale

/**
  * @author Karol Stasiak
  */
trait OutputPackager {
  def packageOutput(mem: CompiledMemory, bank: String): Array[Byte]
}

case class SequenceOutput(children: List[OutputPackager]) extends OutputPackager {
  def packageOutput(mem: CompiledMemory, bank: String): Array[Byte] = {
    val baos = new ByteArrayOutputStream
    children.foreach { c =>
      val a = c.packageOutput(mem, bank)
      baos.write(a, 0, a.length)
    }
    baos.toByteArray
  }
}

case class ConstOutput(byte: Byte) extends OutputPackager {
  def packageOutput(mem: CompiledMemory, bank: String): Array[Byte] = Array(byte)
}

case class CurrentBankFragmentOutput(start: Int, end: Int) extends OutputPackager {
  def packageOutput(mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(bank)
    b.output.slice(start, end + 1)
  }
}

case class BankFragmentOutput(alwaysBank: String, start: Int, end: Int) extends OutputPackager {
  def packageOutput(mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(alwaysBank)
    b.output.slice(start, end + 1)
  }
}

case class ProgramNameOutput(length: Int) extends OutputPackager {
  def isAlphanum(c: Char): Boolean = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')
  def packageOutput(mem: CompiledMemory, bank: String): Array[Byte] = {
    mem.programName.toUpperCase(Locale.ROOT).filter(isAlphanum).take(length).padTo(length, ' ').getBytes(StandardCharsets.US_ASCII)
  }
}

case class StringOutput(string: String) extends OutputPackager {
  def packageOutput(mem: CompiledMemory, bank: String): Array[Byte] = {
    string.getBytes(StandardCharsets.US_ASCII)
  }
}

case class StartAddressOutput(bonus: Int) extends OutputPackager {
  def packageOutput(mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(bank)
    val x = b.start + bonus
    Array(x.toByte, x.>>(8).toByte)
  }
}

case class StartAddressOutputBe(bonus: Int) extends OutputPackager {
  def packageOutput(mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(bank)
    val x = b.start + bonus
    Array(x.>>(8).toByte, x.toByte)
  }
}

object StartPageOutput extends OutputPackager {
  def packageOutput(mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(bank)
    Array(b.start.>>(8).toByte)
  }
}

case class EndAddressOutput(bonus: Int) extends OutputPackager {
  def packageOutput(mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(bank)
    val x = b.end + bonus
    Array(x.toByte, x.>>(8).toByte)
  }
}

case class EndAddressOutputBe(bonus: Int) extends OutputPackager {
  def packageOutput(mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(bank)
    val x = b.end + bonus
    Array(x.>>(8).toByte, x.toByte)
  }
}

object PageCountOutput extends OutputPackager {
  def packageOutput(mem: CompiledMemory, bank: String): Array[Byte] = {
    val e = mem.banks(bank).end.>>(8)
    val s = mem.banks(bank).start.>>(8)
    Array((e - s + 1).toByte)
  }
}

object AllocatedDataOutput extends OutputPackager {
  def packageOutput(mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(bank)
    b.output.slice(b.start, b.end + 1)
  }
}

case class AllocatedDataLength(bonus: Int) extends OutputPackager {
  def packageOutput(mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(bank)
    val size = b.end - b.start + 1 + bonus
    Array(size.toByte, size.>>(8).toByte)
  }
}

case class AllocatedDataLengthBe(bonus: Int) extends OutputPackager {
  def packageOutput(mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(bank)
    val size = b.end - b.start + 1 + bonus
    Array(size.>>(8).toByte, size.toByte)
  }
}
