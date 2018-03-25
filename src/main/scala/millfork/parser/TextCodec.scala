package millfork.parser

import millfork.error.ErrorReporting
import millfork.node.Position

/**
  * @author Karol Stasiak
  */
class TextCodec(val name: String, private val map: String, private val extra: Map[Char, Int]) {
  def decode(position: Option[Position], c: Char): Int = {
    if (extra.contains(c)) extra(c) else {
      val index = map.indexOf(c)
      if (index >= 0) {
        index
      } else {
        ErrorReporting.fatal("Invalid character in string in ")
      }
    }
  }
}

object TextCodec {
  val NotAChar = '\ufffd'

  val Ascii = new TextCodec("ASCII", 0.until(127).map { i => if (i < 32) NotAChar else i.toChar }.mkString, Map.empty)

  val CbmScreencodes = new TextCodec("CBM-Screen",
    "@abcdefghijklmnopqrstuvwxyz[£]↑←" +
      0x20.to(0x3f).map(_.toChar).mkString +
      "–ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    Map('^' -> 0x3E, 'π' -> 0x5E, '♥' -> 0x53, '♡' -> 0x53, '♠' -> 0x41, '♣' -> 0x58, '♢' -> 0x5A, '•' -> 0x51))

  val Petscii = new TextCodec("PETSCII",
    "\ufffd" * 32 +
      0x20.to(0x3f).map(_.toChar).mkString +
      "@abcdefghijklmnopqrstuvwxyz[£]↑←" +
      "–ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    Map('^' -> 0x5E, 'π' -> 0x7E, '♥' -> 0x73, '♡' -> 0x73, '♠' -> 0x61, '♣' -> 0x78, '♢' -> 0x7A, '•' -> 0x71)
  )

  val Atascii = new TextCodec("ATASCII",
    "♡" +
    "\ufffd" * 15 +
      "♣\ufffd–\ufffd•" +
    "\ufffd" * 11 +
      0x20.to(0x5f).map(_.toChar).mkString +
      "♢abcdefghijklmnopqrstuvwxyz♠|",
    Map('♥' -> 0, '·' -> 0x14)
  )

}
