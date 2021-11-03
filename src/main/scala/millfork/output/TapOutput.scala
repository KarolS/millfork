package millfork.output

import java.nio.charset.StandardCharsets

/**
  * @author Karol Stasiak
  */
class TapOutput(val symbol: String) extends OutputPackager {

  def isAlphanum(c: Char): Boolean = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')

  override def packageOutput(flc: FileLayoutCollector, mem: CompiledMemory, bank: String): Array[Byte] = {
    val filteredName: String = mem.programName.filter(isAlphanum)
    val b = mem.banks(bank)
    val code = b.output.slice(b.start, b.end + 1)
    b.markAsOutputted(b.start, b.end + 1)
    val codeDataBlock = new DataBlock(code)
    val codeHeaderBlock = new HeaderBlock(3, "CODE", code.length, b.start, 32768)
    val loaderDataBlock = new DataBlock(ZxSpectrumBasic.loader("CODE", filteredName, b.start, mem.getAddress(symbol)))
    val loaderHeaderBlock = new HeaderBlock(0, "LOADER", loaderDataBlock.inputData.length, 10, loaderDataBlock.inputData.length)
    val result = Array(loaderHeaderBlock, loaderDataBlock, codeHeaderBlock, codeDataBlock).map(_.toArray)
    result.flatten
  }
}

abstract class TapBlock {
  def rawData: Array[Byte]

  def checksum: Byte = rawData.foldLeft(0)(_ ^ _).toByte

  def toArray: Array[Byte] = Array[Byte](
    rawData.length.+(1).toByte,
    rawData.length.+(1).>>(8).toByte
  ) ++ rawData :+ checksum
}

class HeaderBlock(typ: Int, name: String, lengthOfData: Int, param1: Int, param2: Int) extends TapBlock {
  val rawData: Array[Byte] = Array[Byte](0, typ.toByte) ++ name.take(10).padTo(10, ' ').getBytes(StandardCharsets.US_ASCII) ++ Array[Byte](
    lengthOfData.toByte,
    lengthOfData.>>(8).toByte,
    param1.toByte,
    param1.>>(8).toByte,
    param2.toByte,
    param2.>>(8).toByte,
  )
}

class DataBlock(val inputData: Array[Byte]) extends TapBlock {
  val rawData: Array[Byte] = 0xff.toByte +: inputData
}

object ZxSpectrumBasic {

  import scala.language.implicitConversions

  class Snippet(val array: Array[Byte]) extends AnyVal

  implicit def _implicit_String_to_Snippet(s: String): Snippet = new Snippet(s.getBytes(StandardCharsets.US_ASCII))

  private def token(i: Int) = new Snippet(Array(i.toByte))

  val PI: Snippet = token(167)
  val SCREEN$: Snippet = token(170)
  val AT: Snippet = token(172)
  val CODE: Snippet = token(175)
  val VAL: Snippet = token(176)
  val USR: Snippet = token(192)
  val NOT: Snippet = token(195)
  val INK: Snippet = token(217)
  val PAPER: Snippet = token(218)
  val BORDER: Snippet = token(231)
  val REM: Snippet = token(234)
  val LOAD: Snippet = token(239)
  val POKE: Snippet = token(244)
  val PRINT: Snippet = token(245)
  val RUN: Snippet = token(247)
  val RANDOMIZE: Snippet = token(249)
  val CLS: Snippet = token(251)
  val CLEAR: Snippet = token(253)

  val colon: Snippet = token(':')

  def line(number: Int, tokens: Snippet*): Array[Byte] = {
    val content = tokens.flatMap(_.array).toArray
    Array[Byte](number.>>(8).toByte, number.toByte, (content.length + 1).toByte, (content.length + 1).>>(8).toByte) ++ content :+ 13.toByte
  }

  private def quoted(a: Any): Snippet = "\"" + a + "\""

  def loader(filename: String, rem: String, loadAddress: Int, runAddress: Int): Array[Byte] = {
    Array(
      line(10, REM, rem),
      line(20, BORDER, VAL, quoted(7), colon, INK, NOT, PI, colon, PAPER, VAL, quoted(7), colon, CLS),
      line(30, CLEAR, VAL, quoted(loadAddress - 1)),
      line(40, LOAD, quoted(filename), CODE),
      line(50, CLS, colon, PRINT, AT, NOT, PI, ",", NOT, PI, ";", colon, RANDOMIZE, USR, VAL, quoted(runAddress))
    ).flatten
  }
}