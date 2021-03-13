package millfork.parser

import java.nio.charset.{Charset, StandardCharsets}
import java.time.LocalDate
import java.util.Locale

import millfork.{CompilationFlag, CompilationOptions}
import millfork.error.{ConsoleLogger, Logger}
import millfork.node.Position

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */

final case class TextCodecWithFlags(codec: TextCodec, nullTerminated: Boolean, lengthPrefixed: Boolean, lenient: Boolean)

sealed trait TextCodec {
  val supportsLowercase: Boolean

  def name: String

  def stringTerminator: List[Int]

  def encode(log: Logger, position: Option[Position], s: List[Int], options: CompilationOptions, lenient: Boolean): List[Int]

  def decode(by: Int): Char

  // encodes one decimal digit to one byte
  def encodeDigit(digit: Int): List[Int]

  def dump(): Unit = {
    (0 until 256).map(decode).zipWithIndex.grouped(32).map(row => row.head._2.toHexString + "\t" + row.map(_._1).mkString("")).foreach(println(_))
  }

  {
    // sanity check:
    (0 to 9) foreach encodeDigit
  }
}

abstract class MappedTextCodec(override val name: String, inner: TextCodec) extends TextCodec {
  override val supportsLowercase: Boolean = inner.supportsLowercase

  override val stringTerminator: List[Int] = inner.stringTerminator.flatMap(this.mapWithEscaping)

  override def encode(log: Logger, position: Option[Position], s: List[Int], options: CompilationOptions, lenient: Boolean): List[Int] =
    inner.encode(log, position, s, options, lenient).flatMap(this.mapWithEscaping)

  override def decode(by: Int): Char = TextCodec.NotAChar

  override def encodeDigit(digit: Int): List[Int] = inner.encodeDigit(digit).flatMap(this.mapWithEscaping)

  private def mapWithEscaping(byte: Int): List[Int] = {
    if (byte < 0) List(-1 - byte)
    else map(byte)
  }

  def map(byte: Int): List[Int]
}

class UnicodeTextCodec(override val name: String, val optionalCharset: Option[Charset], override val stringTerminator: List[Int], val escapeRawBytes: Boolean = false) extends TextCodec {
  private val escapeSequences: Map[String, Char] = Map(
    "n" -> '\n',
    "r" -> '\r',
    "t" -> '\t',
    "b" -> '\b',
    "null" -> '\0',
    "nullchar" -> '\0',
    "nbsp" -> '\u00a0',
    "shy" -> '\u00ad',
    "apos" -> '\'',
    "q" -> '\"',
    "lbrace" -> '{',
    "rbrace" -> '}',
    "cent" -> '¢',
    "pound" -> '£',
    "euro" -> '€',
    "yen" -> '¥',
    "pi" -> 'π',
    "copy" -> '©',
    "ss" -> 'ß'
  )

  private def encodeEscapeSequence(log: Logger, escSeq: String, position: Option[Position], options: CompilationOptions, lenient: Boolean): List[Int] = {
    if (escSeq.length == 3 && (escSeq(0) == 'X' || escSeq(0) == 'x' || escSeq(0) == '$')){
      try {
        var rawByte = Integer.parseInt(escSeq.tail, 16)
        if (escapeRawBytes) {
          rawByte = -1 - rawByte
        }
        return List(rawByte)
      } catch {
        case _: NumberFormatException =>
      }
    }
    if (escSeq.length > 1 && (escSeq(0) == 'U' || escSeq(0) == 'u')) {
      try {
        return encode(log, position, List(Integer.parseInt(escSeq.tail, 16)), options, lenient)
      } catch {
        case _: NumberFormatException =>
      }
    }
    if (escSeq == "program_name_upper") {
      return encode(log, position, options.outputFileName.getOrElse("MILLFORK").toUpperCase(Locale.ROOT).codePoints().toArray.toList, options, lenient)
    }
    if (escSeq == "program_name") {
      return encode(log, position, options.outputFileName.getOrElse("MILLFORK").codePoints().toArray.toList, options, lenient)
    }
    if (escSeq == "copyright_year") {
      return encode(log, position, LocalDate.now.getYear.toString.map(_.toInt).toList, options, lenient)
    }
    if (escSeq == "null" || escSeq == "nullchar") {
      return stringTerminator
    }
    escapeSequences.get(escSeq) match {
      case Some(c) =>
        encode(log, position, List(c), options, lenient)
      case None =>
        if (lenient) {
          if (options.flag(CompilationFlag.FallbackValueUseWarning)) {
            log.warn(s"Cannot encode escape sequence {$escSeq} in encoding `$name`, skipped it", position)
          }
        } else {
          log.error(s"Invalid escape sequence {$escSeq} for encoding `$name`", position)
        }
        Nil
    }
  }

  override def encode(log: Logger, position: Option[Position], s: List[Int], options: CompilationOptions, lenient: Boolean): List[Int] = {
    val LBRACE = '{'.toInt
    s match {
      case LBRACE :: tail =>
        val (escSeq, closingBrace) = tail.span(_ != '}')
        closingBrace match {
          case '}' :: xs =>
            encodeEscapeSequence(log, escSeq.map(_.toChar).mkString(""), position, options, lenient) ++ encode(log, position, xs, options, lenient)
          case _ =>
            log.error(f"Unclosed escape sequence", position)
            Nil
        }
      case head :: tail =>
        optionalCharset match {
          case Some(charset) =>
            Character.toChars(head).mkString("").getBytes(charset).map(_.&(0xff)).toList ++ encode(log, position, tail, options, lenient)
          case None =>
            head :: encode(log, position, tail, options, lenient)
        }
      case Nil => Nil
    }
  }

  def encodeDigit(digit: Int): List[Int] =
    optionalCharset match {
      case Some(charset) =>
        digit.toString.getBytes(charset).map(_.toInt.&(0xff)).toList
      case None => List('0'.toInt + digit)
    }

  override def decode(by: Int): Char = {
    if (by >= 0x20 && by <= 0x7E) by.toChar
    else if (by == 0) '.'
    else '?'
  }

  override val supportsLowercase: Boolean = true
}

class TableTextCodec(override val name: String,
                     val stringTerminatorChar: Int,
                val map: String,
                val extra: Map[Char, Int],
                val decompositions: Map[Char, String],
                val directDecompositions: Map[Char, List[Int]],
                val escapeSequences: Map[String, List[Int]]) extends TextCodec {

  private val alreadyWarned = mutable.Set[Char]()

  override val stringTerminator: List[Int] = List(stringTerminatorChar)

  private def isPrintable(c: Int) = {
    Character.getType(c) match {
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

  private def format(c:Int):String = {
    val u = f"U+${c.toInt}%04X"
    if (isPrintable(c)) f"`${Character.toChars(c).mkString}%s` ($u%s)"
    else u
  }

  private def format(s:String) = {
    val codePoints = s.codePoints().toArray
    val u = codePoints.map(c => f"U+${c}%04X").mkString(",")
    if (codePoints.forall(isPrintable)) f"`$s%s` ($u%s)"
    else u
  }

  private def supportsChar(c: Char): Boolean = {
    decompositions.contains(c) || directDecompositions.contains(c) || map.indexOf(c) >= 0
  }

  private def encodeChar(log: Logger, position: Option[Position], c: Char, options: CompilationOptions, lenient: Boolean): Option[List[Int]] = {
      if (decompositions.contains(c)) {
        Some(decompositions(c).toList.flatMap(x => encodeChar(log, position, x, options, lenient).getOrElse(List(x.toInt))))
      } else if (directDecompositions.contains(c)) {
        Some(directDecompositions(c))
      } else if (extra.contains(c)) Some(List(extra(c))) else {
        val index = map.indexOf(c)
        if (index >= 0) {
          Some(List(index))
        } else if (lenient) {
          val alternative = TextCodec.lossyAlternatives.getOrElse(c, Nil).:+("?").find(alts => alts.forall(alt => encodeChar(log, position, alt, options, lenient = false).isDefined)).getOrElse("")
          if (options.flag(CompilationFlag.FallbackValueUseWarning) && !alreadyWarned(c)) {
            log.warn(s"Cannot encode ${format(c)} in encoding `$name`, replaced it with ${format(alternative)}", position)
            alreadyWarned += c
          }
          Some(alternative.toList.flatMap(encodeChar(log, position, _, options, lenient = false).get))
        } else {
          None
        }
      }
    }


  def encode(log: Logger, position: Option[Position], s: List[Int], options: CompilationOptions, lenient: Boolean): List[Int] = {
    val LBRACE = '{'.toInt
    val lenient = options.flag(CompilationFlag.LenientTextEncoding)
    s match {
      case LBRACE :: tail =>
        val (escSeq, closingBrace) = tail.span(_ != '}')
        closingBrace match {
          case '}' :: xs =>
            encodeEscapeSequence(log, escSeq.map(_.toChar).mkString(""), position, options, lenient) ++ encode(log, position, xs, options, lenient)
          case _ =>
            log.error(f"Unclosed escape sequence", position)
            Nil
        }
      case head :: tail if head >= Char.MinValue && head <= Char.MaxValue =>
        (encodeChar(log, position, head.toChar, options, lenient) match {
          case Some(x) => x
          case None =>
            log.error(f"Invalid character ${format(head)} in string", position)
            Nil
        }) ++ encode(log, position, tail, options, lenient)
      case head :: tail =>
        log.error(f"Invalid character ${format(head)} in string", position)
        encode(log, position, tail, options, lenient)
      case Nil => Nil
    }
  }

  private def encodeEscapeSequence(log: Logger, escSeq: String, position: Option[Position], options: CompilationOptions, lenient: Boolean): List[Int] = {
    if (escSeq.length == 3 && (escSeq(0) == 'X' || escSeq(0) == 'x' || escSeq(0) == '$')){
      try {
        return List(Integer.parseInt(escSeq.tail, 16))
      } catch {
        case _: NumberFormatException =>
      }
    }
    if (escSeq == "program_name_upper") {
      return encode(log, position, options.outputFileName.getOrElse("MILLFORK").toUpperCase(Locale.ROOT).codePoints().toArray.toList, options, lenient)
    }
    if (escSeq == "program_name") {
      return encode(log, position, options.outputFileName.getOrElse("MILLFORK").codePoints().toArray.toList, options, lenient)
    }
    if (escSeq == "copyright_year") {
      return encode(log, position, LocalDate.now.getYear.toString.map(_.toInt).toList, options, lenient)
    }
    if (escSeq == "null" || escSeq == "nullchar") {
      return stringTerminator
    }
    escapeSequences.getOrElse(escSeq, {
      if (lenient) {
        if (options.flag(CompilationFlag.FallbackValueUseWarning)) {
          log.warn(s"Cannot encode escape sequence {$escSeq} in encoding `$name`, skipped it", position)
        }
      } else {
        log.error(s"Invalid escape sequence {$escSeq} for encoding `$name`", position)
      }
      Nil
    })
  }

  override def decode(by: Int): Char = {
    val index = by & 0xff
    if (index < map.length) map(index) else TextCodec.NotAChar
  }

  override def encodeDigit(digit: Int): List[Int] = {
    val i = map.indexOf(digit + '0'.toInt)
    if (i < 0) throw new IllegalStateException(s"For some reason, there is no digit $digit in the $name encoding?")
    List(i)
  }

  override val supportsLowercase: Boolean = 'a' to 'z' forall(c => supportsChar(c))
}

object TextCodec {

  val NotAChar = '\ufffd'

  lazy val DefaultOverrides: Map[Char, Int] = ('\u2400' to '\u2420').map(c => c->(c.toInt - 0x2400)).toMap + ('\u2421' -> 127)

  //noinspection ScalaUnusedSymbol
  private lazy val AsciiEscapeSequences: Map[String, List[Int]] = Map(
    "n" -> List(13, 10),
    "t" -> List(9),
    "b" -> List(8),
    "q" -> List('\"'.toInt),
    "apos" -> List('\''.toInt),
    "lbrace" -> List('{'.toInt),
    "rbrace" -> List('}'.toInt))

  //noinspection ScalaUnusedSymbol
  private lazy val MinimalEscapeSequencesWithoutBraces: Map[String, List[Int]] = Map(
    "apos" -> List('\''.toInt),
    "q" -> List('\"'.toInt))

  //noinspection ScalaUnusedSymbol
  private lazy val MinimalEscapeSequencesWithBraces: Map[String, List[Int]] = Map(
    "apos" -> List('\''.toInt),
    "q" -> List('\"'.toInt),
    "lbrace" -> List('{'.toInt),
    "rbrace" -> List('}'.toInt))

  lazy val StandardKatakanaDecompositions: Map[Char, String] = {
    (("カキクケコサシスセソタチツテトハヒフヘホ")).zip(
      "ガギグゲゴザジズゼゾダヂヅデドバビブベボ").map { case (u, v) => v -> (u + "゛") }.toMap ++
      "ハヒフヘホ".zip("パピプペポ").map { case (h, p) => p -> (h + "゜") }.toMap
  }
  lazy val StandardHiraganaDecompositions: Map[Char, String] = {
    (("かきくけこさしすせそたちつてとはひふへほ")).zip(
      "がぎぐげござじずぜぞだぢづでどばびぶべぼ").map { case (u, v) => v -> (u + "゛") }.toMap ++
      "はひふへほ".zip("ぱぴぷぺぽ").map { case (h, p) => p -> (h + "゜") }.toMap
  }

  // The only built-in encoding:
  lazy val Ascii = new TableTextCodec("ASCII", 0, 0.until(127).map { i => if (i < 32) NotAChar else i.toChar }.mkString, Map.empty, Map.empty, Map.empty, AsciiEscapeSequences)

  private val jisHalfwidthKatakanaOrder: String =
    "\ufffd。「」、・ヲァィゥェォャュョッ" +
    "ーアイウエオカキクケコサシスセソ" +
    "タチツテトナニヌネノハヒフヘホマ" +
    "ミムメモヤユヨラリルレロワン゛゜"

  lazy val lossyAlternatives: Map[Char, List[String]] = {
    val allowLowercase: Map[Char, List[String]] = ('A' to 'Z').map(c => c -> List(c.toString.toLowerCase(Locale.ROOT))).toMap
    val allowUppercase: Map[Char, List[String]] = ('a' to 'z').map(c => c -> List(c.toString.toUpperCase(Locale.ROOT))).toMap
    val allowLowercaseCyr: Map[Char, List[String]] = ('а' to 'я').map(c => c -> List(c.toString.toUpperCase(Locale.ROOT))).toMap
    val allowUppercaseCyr: Map[Char, List[String]] = ('а' to 'я').map(c => c -> List(c.toString.toUpperCase(Locale.ROOT))).toMap
     val ligaturesAndSymbols: Map[Char, List[String]] = Map(
       // commonly used alternative forms:
       '¦' -> List("|"),
       '|' -> List("¦"),
       // Eszett:
       'ß' -> List("ss", "SS"),
       'β' -> List("ß"),
       // various ligatures:
       'ﬀ' -> List("ff", "FF"),
       'ﬂ' -> List("fl", "FL"),
       'ﬁ' -> List("fi", "FI"),
       'ﬃ' -> List("ffi", "FFI"),
       'ﬄ' -> List("ffl", "FFL"),
       'ĳ' -> List("ij", "IJ"),
       'Ĳ' -> List("IJ", "ij"),
       // fractions:
       '½' -> List("1/2"),
       '¼' -> List("1/4"),
       '¾' -> List("3/4"),
       // currencies:
       '₧' -> List("Pt", "PT"),
       '¢' -> List("c", "C"),
       '$' -> List("¤"),
       '¥' -> List("Y", "y"),
       // kanji:
       '円' -> List("¥", "Y", "y"),
       '年' -> List("Y", "y"),
       '月' -> List("M", "m"),
       '日' -> List("D", "d"),
       '時' -> List("h", "H"),
       '分' -> List("m", "M"),
       '秒' -> List("s", "S"),
       // card suits:
       '♥' -> List("H", "h"),
       '♠' -> List("S", "s"),
       '♡' -> List("H", "h"),
       '♢' -> List("D", "d"),
       '♣' -> List("C", "c"),
       // Eastern punctuation:
       '。' -> List("."),
       '、' -> List(","),
       '・' -> List("-"),
       '•' -> List("・", "*"),
       '「' -> List("[", "("),
       '」' -> List("]", ")"),
       '。' -> List("."),
       '。' -> List("."),
       // quote marks:
       '«' -> List("\""),
       '»' -> List("\""),
       '‟' -> List("\""),
       '”' -> List("\""),
       '„' -> List("\""),
       '’' -> List("\'"),
       '‘' -> List("\'"),
       // pi:
       'π' -> List("Π"),
       'Π' -> List("π"),
       // alternative symbols:
       '^' -> List("↑"),
       '↑' -> List("^"),
       '‾' -> List("~"),
       '¯' -> List("~"),
       '§' -> List("#"),
       '[' -> List("("),
       ']' -> List(")"),
       '{' -> List("("),
       '}' -> List(")"),
       '©' -> List("(C)", "(c)"),
       '®' -> List("(R)", "(r)"),
       '‰' -> List("%."),
       '×' -> List("x"),
       '÷' -> List("/"),
       'ª' -> List("a", "A"),
       'º' -> List("o", "O"),
       // Turkish I with dot:
       'İ' -> List("I", "i"),
       // partially supported Russian letters:
       'ё' -> List("е", "Ё", "Е"),
       'Ё' -> List("Е", "ё", "е"),
       'Ъ' -> List("ъ"),
       'ъ' -> List("Ъ"),
       // Latin lookalikes for Cyrillic:
       'і' -> List("i", "I"),
       'І' -> List("I", "i"),
       'ј' -> List("j", "J"),
       'Ј' -> List("J", "j"),
     )
    val accentedLetters: Map[Char, List[String]] = List(
      "áàäãåąāǎă" -> "a",
      "çčċćĉ" -> "c",
      "ðď" -> "d",
      "đ" -> "dj",
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
    allowLowercase ++ allowUppercase ++ allowLowercaseCyr ++ allowUppercaseCyr ++ ligaturesAndSymbols ++ accentedLetters ++ hiragana ++ fullWidth ++ halfWidth
  }

}
