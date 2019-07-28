package millfork.output

import java.io.ByteArrayOutputStream

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

object StartAddressOutput extends OutputPackager {
  def packageOutput(mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(bank)
    Array(b.start.toByte, b.start.>>(8).toByte)
  }
}

object StartAddressOutputBe extends OutputPackager {
  def packageOutput(mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(bank)
    Array(b.start.>>(8).toByte, b.start.toByte)
  }
}

object StartPageOutput extends OutputPackager {
  def packageOutput(mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(bank)
    Array(b.start.>>(8).toByte)
  }
}

object EndAddressOutput extends OutputPackager {
  def packageOutput(mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(bank)
    Array(b.end.toByte, b.end.>>(8).toByte)
  }
}

object EndAddressOutputBe extends OutputPackager {
  def packageOutput(mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(bank)
    Array(b.end.>>(8).toByte, b.end.toByte)
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

object AllocatedDataLength extends OutputPackager {
  def packageOutput(mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(bank)
    val size = b.end - b.start + 1
    Array(size.toByte, size.>>(8).toByte)
  }
}

object AllocatedDataLengthBe extends OutputPackager {
  def packageOutput(mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(bank)
    val size = b.end - b.start + 1
    Array(size.>>(8).toByte, size.toByte)
  }
}
