package millfork.parser

import millfork.error.ErrorReporting
import millfork.node.Position

/**
  * @author Karol Stasiak
  */
class TextCodec(val name: String, private val map: String, private val extra: Map[Char, Int], private val decompositions: Map[Char, String]) {
  def encode(position: Option[Position], c: Char): List[Int] = {
    if (decompositions.contains(c)) {
      decompositions(c).toList.flatMap(x => encode(position, x))
    } else if (extra.contains(c)) List(extra(c)) else {
      val index = map.indexOf(c)
      if (index >= 0) {
        List(index)
      } else {
        ErrorReporting.fatal("Invalid character in string")
      }
    }
  }

  def decode(by: Int): Char = {
    val index = by & 0xff
    if (index < map.length) map(index) else TextCodec.NotAChar
  }
}

object TextCodec {
  val NotAChar = '\ufffd'

  val Ascii = new TextCodec("ASCII", 0.until(127).map { i => if (i < 32) NotAChar else i.toChar }.mkString, Map.empty, Map.empty)

  val Apple2 = new TextCodec("APPLE-II", 0.until(255).map { i => if (i < 160) NotAChar else (i - 128).toChar }.mkString, Map.empty, Map.empty)

  val IsoIec646De = new TextCodec("ISO-IEC-646-DE",
    "\ufffd" * 32 +
      " !\"#$%^'()*+,-./0123456789:;<=>?" +
      "§ABCDEFGHIJKLMNOPQRSTUVWXYZÄÖÜ^_" +
      "`abcdefghijklmnopqrstuvwxyzäöüß",
    Map.empty, Map.empty
  )

  val IsoIec646Se = new TextCodec("ISO-IEC-646-SE",
    "\ufffd" * 32 +
      " !\"#¤%^'()*+,-./0123456789:;<=>?" +
      "@ABCDEFGHIJKLMNOPQRSTUVWXYZÄÖÅ^_" +
      "`abcdefghijklmnopqrstuvwxyzäöå~",
    Map('¯' -> '~'.toInt,
      '‾' -> '~'.toInt,
      'É' -> '@'.toInt,
      'é' -> '`'.toInt,
      'Ü' -> '^'.toInt,
      'ü' -> '~'.toInt,
      '$' -> '¤'.toInt),
    Map.empty
  )

  val IsoIec646No = new TextCodec("ISO-IEC-646-NO",
    "\ufffd" * 32 +
      " !\"#$%^'()*+,-./0123456789:;<=>?" +
      "@ABCDEFGHIJKLMNOPQRSTUVWXYZÆØÅ^_" +
      "`abcdefghijklmnopqrstuvwxyzæøå~",
    Map('¯' -> '~'.toInt,
      '‾' -> '~'.toInt,
      '|' -> '~'.toInt,
      '¤' -> '$'.toInt,
      'Ä' -> '@'.toInt,
      'ä' -> '`'.toInt,
      'Ü' -> '^'.toInt,
      'ü' -> '~'.toInt,
      '«' -> '"'.toInt,
      '»' -> '#'.toInt,
      '§' -> '#'.toInt),
    Map.empty
  )

  val IsoIec646Yu = new TextCodec("ISO-IEC-646-YU",
    "\ufffd" * 32 +
      " !\"#$%^'()*+,-./0123456789:;<=>?" +
      "ŽABCDEFGHIJKLMNOPQRSTUVWXYZŠĐĆČ_" +
      "žabcdefghijklmnopqrstuvwxyzšđćč",
    Map('Ë' -> '$'.toInt, 'ë' -> '_'.toInt),
    Map.empty
  )

  val CbmScreencodes = new TextCodec("CBM-Screen",
    "@abcdefghijklmnopqrstuvwxyz[£]↑←" +
      0x20.to(0x3f).map(_.toChar).mkString +
      "–ABCDEFGHIJKLMNOPQRSTUVWXYZ\ufffd\ufffd\ufffdπ",
    Map('^' -> 0x3E, '♥' -> 0x53, '♡' -> 0x53, '♠' -> 0x41, '♣' -> 0x58, '♢' -> 0x5A, '•' -> 0x51), Map.empty
  )

  val Petscii = new TextCodec("PETSCII",
    "\ufffd" * 32 +
      0x20.to(0x3f).map(_.toChar).mkString +
      "@abcdefghijklmnopqrstuvwxyz[£]↑←" +
      "–ABCDEFGHIJKLMNOPQRSTUVWXYZ\ufffd\ufffd\ufffdπ",
    Map('^' -> 0x5E, '♥' -> 0x73, '♡' -> 0x73, '♠' -> 0x61, '♣' -> 0x78, '♢' -> 0x7A, '•' -> 0x71), Map.empty
  )

  val Atascii = new TextCodec("ATASCII",
    "♡" +
    "\ufffd" * 15 +
      "♣\ufffd–\ufffd•" +
    "\ufffd" * 11 +
      0x20.to(0x5f).map(_.toChar).mkString +
      "♢abcdefghijklmnopqrstuvwxyz♠|",
    Map('♥' -> 0, '·' -> 0x14), Map.empty
  )

  val Bbc = new TextCodec("BBC",
    "\ufffd" * 32 +
      0x20.to(0x5f).map(_.toChar).mkString +
    "£" + 0x61.to(0x7E).map(_.toChar).mkString + "©",
    Map('↑' -> '^'.toInt), Map.empty
  )

  //noinspection ScalaUnnecessaryParentheses
  val Jis = new TextCodec("JIS-X-0201",
    "\ufffd" * 32 +
      ' '.to('Z').mkString +
      "[¥]^_" +
      "`" + 'a'.to('z').mkString + "{|}~\ufffd" +
      "\ufffd" * 32 +
      "\ufffd。「」、・ヲァィゥェォャュョッ" +
      "ーアイウエオカキクケコサシスセソ" +
      "タチツテトナニヌネノハヒフヘホマ" +
      "ミムメモヤユヨラリルレロワン゛゜" +
      "\ufffd" * 8 +
      "♠♡♢♣" +
      "\ufffd" * 4 +
      "円年月日時分秒" +
      "\ufffd" * 3 + "\\",
    Map('¯' -> '~'.toInt, '‾' -> '~'.toInt, '♥' -> 0xE9) ++
      1.to(0x3F).map(i => (i + 0xff60).toChar -> (i + 0xA1)).toMap,
      (("カキクケコサシスセソタチツテトハヒフヘホ")).zip(
        "ガギグゲゴザジズゼゾダヂヅデドバビブベボ").map { case (u, v) => v -> (u + "゛") }.toMap ++
      "ハヒフヘホ".zip("パピプペポ").map { case (h, p) => p -> (h + "゜") }.toMap
  )

}
