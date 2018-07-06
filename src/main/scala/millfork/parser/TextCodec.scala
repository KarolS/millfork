package millfork.parser

import java.util.Locale

import millfork.CompilationOptions
import millfork.error.ErrorReporting
import millfork.node.Position

/**
  * @author Karol Stasiak
  */
class TextCodec(val name: String, private val map: String, private val extra: Map[Char, Int], private val decompositions: Map[Char, String]) {

  private def isPrintable(c: Char) = {
    c.getType match {
      case Character.LOWERCASE_LETTER => true
      case Character.UPPERCASE_LETTER => true
      case Character.TITLECASE_LETTER => true
      case Character.OTHER_LETTER => true
      case Character.LETTER_NUMBER => true
      case Character.DECIMAL_DIGIT_NUMBER => true
      case Character.OTHER_NUMBER => true
      case Character.DASH_PUNCTUATION => true
      case Character.START_PUNCTUATION => true
      case Character.END_PUNCTUATION => true
      case Character.INITIAL_QUOTE_PUNCTUATION => true
      case Character.FINAL_QUOTE_PUNCTUATION => true
      case Character.OTHER_PUNCTUATION => true
      case Character.CURRENCY_SYMBOL => true
      case Character.OTHER_SYMBOL => true
      case Character.MATH_SYMBOL => true
      case Character.SPACE_SEPARATOR => true
      case Character.PARAGRAPH_SEPARATOR => false
      case Character.LINE_SEPARATOR => false
      case Character.CONTROL => false
      case Character.MODIFIER_SYMBOL => false
      case Character.SURROGATE => false
      case Character.NON_SPACING_MARK => false
      case Character.COMBINING_SPACING_MARK => false
      case _ => false
    }
  }

  private def format(c:Char):String = {
    val u = f"U+${c.toInt}%04X"
    if (isPrintable(c)) f"`$c%c` ($u%s)"
    else u
  }

  private def format(s:String) = {
    val u = s.map(c => f"U+${c.toInt}%04X").mkString(",")
    if (s.forall(isPrintable)) f"`$s%s` ($u%s)"
    else u
  }

  private def encodeImpl(options: CompilationOptions, position: Option[Position], c: Char, lenient: Boolean): Option[List[Int]] = {
    if (decompositions.contains(c)) {
      Some(decompositions(c).toList.flatMap(x => encodeImpl(options, position, x, lenient).getOrElse(Nil)))
    } else if (extra.contains(c)) Some(List(extra(c))) else {
      val index = map.indexOf(c)
      if (index >= 0) {
        Some(List(index))
      } else if (lenient) {
        val alternative = TextCodec.lossyAlternatives.getOrElse(c, Nil).:+("?").find(alts => alts.forall(alt => encodeImpl(options, position, alt, lenient = false).isDefined)).getOrElse("")
        ErrorReporting.warn(s"Cannot encode ${format(c)} in encoding `$name`, replaced it with ${format(alternative)}", options, position)
        Some(alternative.toList.flatMap(encodeImpl(options, position, _, lenient = false).get))
      } else {
        None
      }
    }
  }

  def encode(options: CompilationOptions, position: Option[Position], c: Char, lenient: Boolean): List[Int] = {
    encodeImpl(options, position, c, lenient) match {
      case Some(x) => x
      case None =>
        ErrorReporting.error(f"Invalid character ${format(c)} in string", position)
        Nil
    }
  }

  def decode(by: Int): Char = {
    val index = by & 0xff
    if (index < map.length) map(index) else TextCodec.NotAChar
  }
}

object TextCodec {

  def forName(name: String, position: Option[Position]): TextCodec = (position, name) match {
    case (_, "ascii") => TextCodec.Ascii
    case (_, "petscii") => TextCodec.Petscii
    case (_, "pet") => TextCodec.Petscii
    case (_, "cbmscr") => TextCodec.CbmScreencodes
    case (_, "petscr") => TextCodec.CbmScreencodes
    case (_, "atascii") => TextCodec.Atascii
    case (_, "atari") => TextCodec.Atascii
    case (_, "bbc") => TextCodec.Bbc
    case (_, "apple2") => TextCodec.Apple2
    case (_, "jis") => TextCodec.Jis
    case (_, "jisx") => TextCodec.Jis
    case (_, "iso_de") => TextCodec.IsoIec646De
    case (_, "iso_no") => TextCodec.IsoIec646No
    case (_, "iso_dk") => TextCodec.IsoIec646No
    case (_, "iso_se") => TextCodec.IsoIec646Se
    case (_, "iso_fi") => TextCodec.IsoIec646Se
    case (_, "iso_yu") => TextCodec.IsoIec646Yu
    case (p, x) =>
      ErrorReporting.error(s"Unknown string encoding: `$x`", p)
      TextCodec.Ascii
  }

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
      '»' -> '"'.toInt,
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

  private val jisHalfwidthKatakanaOrder: String =
    "\ufffd。「」、・ヲァィゥェォャュョッ" +
    "ーアイウエオカキクケコサシスセソ" +
    "タチツテトナニヌネノハヒフヘホマ" +
    "ミムメモヤユヨラリルレロワン゛゜"

  //noinspection ScalaUnnecessaryParentheses
  val Jis = new TextCodec("JIS-X-0201",
    "\ufffd" * 32 +
      ' '.to('Z').mkString +
      "[¥]^_" +
      "`" + 'a'.to('z').mkString + "{|}~\ufffd" +
      "\ufffd" * 32 +
      jisHalfwidthKatakanaOrder +
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

  val lossyAlternatives: Map[Char, List[String]] = {
    val allowLowercase: Map[Char, List[String]] = ('A' to 'Z').map(c => c -> List(c.toString.toLowerCase(Locale.ROOT))).toMap
    val allowUppercase: Map[Char, List[String]] = ('a' to 'z').map(c => c -> List(c.toString.toUpperCase(Locale.ROOT))).toMap
     val ligaturesAndSymbols: Map[Char, List[String]] = Map(
       '¦' -> List("|"),
       '|' -> List("¦"),
       'ß' -> List("ss", "SS"),
       'ﬀ' -> List("ff", "FF"),
       'ﬂ' -> List("fl", "FL"),
       'ﬁ' -> List("fi", "FI"),
       'ﬃ' -> List("ffi", "FFI"),
       'ﬄ' -> List("ffl", "FFL"),
       '½' -> List("1/2"),
       '¼' -> List("1/4"),
       '¾' -> List("3/4"),
       '¥' -> List("Y", "y"),
       '円' -> List("¥", "Y", "y"),
       '年' -> List("Y", "y"),
       '月' -> List("M", "m"),
       '日' -> List("D", "d"),
       '時' -> List("h", "H"),
       '分' -> List("m", "M"),
       '秒' -> List("s", "S"),
       '♥' -> List("H", "h"),
       '♠' -> List("S", "s"),
       '♡' -> List("H", "h"),
       '♢' -> List("D", "d"),
       '♣' -> List("C", "c"),
       '。' -> List("."),
       '、' -> List(","),
       '・' -> List("-"),
       '•' -> List("・", "*"),
       '「' -> List("[", "("),
       '」' -> List("]", ")"),
       '。' -> List("."),
       '。' -> List("."),
       '^' -> List("↑"),
       '↑' -> List("^"),
       '‾' -> List("~"),
       '¯' -> List("~"),
       '«' -> List("\""),
       '»' -> List("\""),
       '§' -> List("#"),
       '[' -> List("("),
       ']' -> List(")"),
       '{' -> List("("),
       '}' -> List(")"),
       '§' -> List("#"),
       '§' -> List("#"),
       '©' -> List("(C)"),
       'İ' -> List("I", "i"),
       'ª' -> List("a", "A"),
       'º' -> List("o", "O"),
       '‰' -> List("%."),
       '÷' -> List("/"),
       'ĳ' -> List("ij", "IJ"),
       'Ĳ' -> List("IJ", "ij"),
     )
    val accentedLetters: Map[Char, List[String]] = List(
      "áàäãåąāǎă" -> "a",
      "çčċćĉ" -> "c",
      "đď" -> "d",
      "ð" -> "dh",
      "éèêëęēėě" -> "e",
      "ğǧĝģġ" -> "g",
      "ħĥ" -> "h",
      "íıìîïįīǐĭĩ" -> "i",
      "ĵ" -> "j",
      "ķ" -> "k",
      "ĺľłļŀ" -> "l",
      "ñńňņŋ" -> "n",
      "óòöôőõøōǒ" -> "o",
      "řŗŕ" -> "r",
      "śšŝșşſ" -> "s",
      "ţțťŧ" -> "t",
      "þ" -> "th",
      "úùũûüűųūǔůǘǜǚǖ" -> "u",
      "ẃẁŵ" -> "w",
      "ýÿỳŷȳ" -> "y",
      "žźż" -> "z",
      "æ" -> "ae",
      "œ" -> "oe",
    ).flatMap{case (acc, plain) => acc.toList.flatMap(letter => List(
      letter -> List(plain, plain.toUpperCase(Locale.ROOT)),
      letter.toUpper -> List(plain.toUpperCase(Locale.ROOT), plain)
    ))}.toMap
    val hiragana: Map[Char, List[String]] = (0x3041 to 0x3096).map{ kana => kana.toChar -> List(kana.+(0x60).toChar.toString)}.toMap
    val fullWidth: Map[Char, List[String]] = (0xff01 to 0xff5e).map{ i =>
      val fw = i.toChar
      val hw = i.-(0xfee0).toChar
      if (hw.isUpper) fw -> List(hw.toString, hw.toString.toLowerCase(Locale.ROOT))
      else if (hw.isLower) fw -> List(hw.toString, hw.toString.toUpperCase(Locale.ROOT))
      else fw -> List(hw.toString)
    }.toMap
    val halfWidth = (0xff61 to 0xff9f).map{ c => c.toChar -> List(jisHalfwidthKatakanaOrder(c - 0xff60).toString)}.toMap
    allowLowercase ++ allowUppercase ++ ligaturesAndSymbols ++ accentedLetters ++ hiragana ++ fullWidth ++ halfWidth
  }

}
