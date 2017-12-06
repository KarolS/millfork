package millfork.output

import java.io.ByteArrayOutputStream

/**
  * @author Karol Stasiak
  */
trait OutputPackager {
  def packageOutput(mem: CompiledMemory, bank: Int): Array[Byte]
}

case class SequenceOutput(children: List[OutputPackager]) extends OutputPackager {
  def packageOutput(mem: CompiledMemory, bank: Int): Array[Byte] = {
    val baos = new ByteArrayOutputStream
    children.foreach { c =>
      val a = c.packageOutput(mem, bank)
      baos.write(a, 0, a.length)
    }
    baos.toByteArray
  }
}

case class ConstOutput(byte: Byte) extends OutputPackager {
  def packageOutput(mem: CompiledMemory, bank: Int): Array[Byte] = Array(byte)
}

case class CurrentBankFragmentOutput(start: Int, end: Int) extends OutputPackager {
  def packageOutput(mem: CompiledMemory, bank: Int): Array[Byte] = {
    val b = mem.banks(bank)
    b.output.slice(start, end + 1)
  }
}

case class BankFragmentOutput(alwaysBank: Int, start: Int, end: Int) extends OutputPackager {
  def packageOutput(mem: CompiledMemory, bank: Int): Array[Byte] = {
    val b = mem.banks(alwaysBank)
    b.output.slice(start, end + 1)
  }
}

object StartAddressOutput extends OutputPackager {
  def packageOutput(mem: CompiledMemory, bank: Int): Array[Byte] = {
    val b = mem.banks(bank)
    Array(b.start.toByte, b.start.>>(8).toByte)
  }
}

object EndAddressOutput extends OutputPackager {
  def packageOutput(mem: CompiledMemory, bank: Int): Array[Byte] = {
    val b = mem.banks(bank)
    Array(b.end.toByte, b.end.>>(8).toByte)
  }
}

object AllocatedDataOutput extends OutputPackager {
  def packageOutput(mem: CompiledMemory, bank: Int): Array[Byte] = {
    val b = mem.banks(bank)
    b.output.slice(b.start, b.end + 1)
  }
}