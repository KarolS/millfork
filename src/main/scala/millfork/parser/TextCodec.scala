package millfork.parser

import java.nio.charset.{Charset, StandardCharsets}
import java.time.LocalDate
import java.util.Locale

import millfork.{CompilationFlag, CompilationOptions}
import millfork.error.{ConsoleLogger, Logger}
import millfork.node.Position

/**
  * @author Karol Stasiak
  */

final case class TextCodecWithFlags(code: TextCodec, nullTerminated: Boolean, lengthPrefixed: Boolean, lenient: Boolean)

sealed trait TextCodec {
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

class UnicodeTextCodec(override val name: String, val charset: Charset, override val stringTerminator: List[Int]) extends TextCodec {
  private val escapeSequences: Map[String, Char] = Map(
    "n" -> '\n',
    "r" -> '\r',
    "t" -> '\t',
    "b" -> '\b',
    "null" -> '\0',
    "nullchar" -> '\0',
    "apos" -> '\'',
    "q" -> '\"',
    "lbrace" -> '{',
    "rbrace" -> '}',
    "pound" -> '£',
    "euro" -> '€',
    "yen" -> '¥',
    "pi" -> 'π',
    "copy" -> '©'
  )

  private def encodeEscapeSequence(log: Logger, escSeq: String, position: Option[Position], options: CompilationOptions, lenient: Boolean): List[Int] = {
    if (escSeq.length == 3 && (escSeq(0) == 'X' || escSeq(0) == 'x' || escSeq(0) == '$')){
      try {
        return List(Integer.parseInt(escSeq.tail, 16))
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
            encodeEscapeSequence(log, escSeq.mkString(""), position, options, lenient) ++ encode(log, position, xs, options, lenient)
          case _ =>
            log.error(f"Unclosed escape sequence", position)
            Nil
        }
      case head :: tail =>
        Character.toChars(head).mkString("").getBytes(charset).map(_.&(0xff)).toList ++ encode(log, position, tail, options, lenient)
      case Nil => Nil
    }
  }

  def encodeDigit(digit: Int): List[Int] =  digit.toString.getBytes(charset).map(_.toInt.&(0xff)).toList

  override def decode(by: Int): Char = {
    if (by >= 0x20 && by <= 0x7E) by.toChar
    else if (by == 0) '.'
    else '?'
  }
}

class TableTextCodec(override val name: String,
                     val stringTerminatorChar: Int,
                private val map: String,
                private val extra: Map[Char, Int],
                private val decompositions: Map[Char, String],
                private val escapeSequences: Map[String, List[Int]]) extends TextCodec {

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
  private def encodeChar(log: Logger, position: Option[Position], c: Char, options: CompilationOptions, lenient: Boolean): Option[List[Int]] = {
      if (decompositions.contains(c)) {
        Some(decompositions(c).toList.flatMap(x => encodeChar(log, position, x, options, lenient).getOrElse(List(x.toInt))))
      } else if (extra.contains(c)) Some(List(extra(c))) else {
        val index = map.indexOf(c)
        if (index >= 0) {
          Some(List(index))
        } else if (lenient) {
          val alternative = TextCodec.lossyAlternatives.getOrElse(c, Nil).:+("?").find(alts => alts.forall(alt => encodeChar(log, position, alt, options, lenient = false).isDefined)).getOrElse("")
          if (options.flag(CompilationFlag.FallbackValueUseWarning)) {
            log.warn(s"Cannot encode ${format(c)} in encoding `$name`, replaced it with ${format(alternative)}", position)
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
}

object TextCodec {
  lazy val allCodecs = Map(
    "ascii" -> TextCodec.Ascii,
    "petscii" -> TextCodec.Petscii,
    "pet" -> TextCodec.Petscii,
    "petsciijp" -> TextCodec.PetsciiJp,
    "petjp" -> TextCodec.PetsciiJp,
    "oldpetscii" -> TextCodec.OldPetscii,
    "oldpet" -> TextCodec.OldPetscii,
    "origpetscii" -> TextCodec.OriginalPetscii,
    "origpet" -> TextCodec.OriginalPetscii,
    "cbmscr" -> TextCodec.CbmScreencodes,
    "petscr" -> TextCodec.CbmScreencodes,
    "cbmscrjp" -> TextCodec.CbmScreencodesJp,
    "petscrjp" -> TextCodec.CbmScreencodesJp,
    "atascii" -> TextCodec.Atascii,
    "atari" -> TextCodec.Atascii,
    "atasciiscr" -> TextCodec.AtasciiScreencodes,
    "atariscr" -> TextCodec.AtasciiScreencodes,
    "bbc" -> TextCodec.Bbc,
    "sinclair" -> TextCodec.Sinclair,
    "apple2" -> TextCodec.Apple2,
    "jis" -> TextCodec.Jis,
    "jisx" -> TextCodec.Jis,
    "iso_de" -> TextCodec.IsoIec646De,
    "iso_no" -> TextCodec.IsoIec646No,
    "iso_dk" -> TextCodec.IsoIec646No,
    "iso_se" -> TextCodec.IsoIec646Se,
    "iso_fi" -> TextCodec.IsoIec646Se,
    "iso_yu" -> TextCodec.IsoIec646Yu,
    "msx_intl" -> TextCodec.MsxWest,
    "msx_us" -> TextCodec.MsxWest,
    "msx_uk" -> TextCodec.MsxWest,
    "msx_de" -> TextCodec.MsxWest,
    "msx_fr" -> TextCodec.MsxWest,
    "msx_es" -> TextCodec.MsxWest,
    "msx_ru" -> TextCodec.MsxRu,
    "msx_jp" -> TextCodec.MsxJp,
    "msx_br" -> TextCodec.MsxBr,
    "vectrex" -> TextCodec.Vectrex,
    "koi7n2" -> TextCodec.Koi7N2,
    "short_koi" -> TextCodec.Koi7N2,
    "zx80" -> TextCodec.Zx80,
    "zx81" -> TextCodec.Zx81,
    "iso8859_15" -> TextCodec.Iso8859_15,
    "latin0" -> TextCodec.Iso8859_15,
    "latin9" -> TextCodec.Iso8859_15,
    "iso15" -> TextCodec.Iso8859_15,
    "utf8" -> TextCodec.Utf8,
    "utf16be" -> TextCodec.Utf16Be,
    "utf16le" -> TextCodec.Utf16Le,
  )

  def forName(name: String, position: Option[Position], log: Logger): TextCodecWithFlags = {
    if (allCodecs.contains(name)) return TextCodecWithFlags(allCodecs(name), nullTerminated = false, lengthPrefixed = false, lenient = false)
    if (name.endsWith("z")) {
      val cleanName = name.stripSuffix("z")
      if (allCodecs.contains(cleanName)) return TextCodecWithFlags(allCodecs(cleanName), nullTerminated = true, lengthPrefixed = false, lenient = false)
    }
    val lengthPrefixed = name.startsWith("p")
    if (name.startsWith("p")) {
      val cleanName = name.stripPrefix("p")
      if (allCodecs.contains(cleanName)) return TextCodecWithFlags(allCodecs(cleanName), nullTerminated = false, lengthPrefixed = true, lenient = false)

      if (cleanName.endsWith("z")) {
        val cleanName2 = cleanName.stripSuffix("z")
        if (allCodecs.contains(cleanName2)) return TextCodecWithFlags(allCodecs(cleanName2), nullTerminated = true, lengthPrefixed = true, lenient = false)
      }
    }
    log.error(s"Unknown string encoding: `$name`", position)
    TextCodecWithFlags(TextCodec.Ascii, nullTerminated = false, lengthPrefixed = false, lenient = false)
  }

  private val Utf8 = new UnicodeTextCodec("UTF-8", StandardCharsets.UTF_8, List(0))

  private val Utf16Be = new UnicodeTextCodec("UTF-16BE", StandardCharsets.UTF_16BE, List(0, 0))

  private val Utf16Le = new UnicodeTextCodec("UTF-16LE", StandardCharsets.UTF_16LE, List(0, 0))

  val NotAChar = '\ufffd'

  private lazy val DefaultOverrides: Map[Char, Int] = ('\u2400' to '\u2420').map(c => c->(c.toInt - 0x2400)).toMap + ('\u2421' -> 127)

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

  private lazy val StandardKatakanaDecompositions: Map[Char, String] = {
    (("カキクケコサシスセソタチツテトハヒフヘホ")).zip(
      "ガギグゲゴザジズゼゾダヂヅデドバビブベボ").map { case (u, v) => v -> (u + "゛") }.toMap ++
      "ハヒフヘホ".zip("パピプペポ").map { case (h, p) => p -> (h + "゜") }.toMap
  }
  private lazy val StandardHiraganaDecompositions: Map[Char, String] = {
    (("かきくけこさしすせそたちつてとはひふへほ")).zip(
      "がぎぐげござじずぜぞだぢづでどばびぶべぼ").map { case (u, v) => v -> (u + "゛") }.toMap ++
      "はひふへほ".zip("ぱぴぷぺぽ").map { case (h, p) => p -> (h + "゜") }.toMap
  }

  lazy val Ascii = new TableTextCodec("ASCII", 0, 0.until(127).map { i => if (i < 32) NotAChar else i.toChar }.mkString, Map.empty, Map.empty, AsciiEscapeSequences)

  lazy val Iso8859_15 = new TableTextCodec("ISO 8859-15", 0,
    "\ufffd" * 32 +
      32.until(127).map { i => i.toChar }.mkString +
      "\ufffd" +
      "\ufffd" * 32 +
      "\ufffd¡¢£€¥Š§š©ª«¬\ufffd®¯" +
      "°±²³Žµ¶·ž¹º»ŒœŸ¿" +
      "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏ" +
      "ÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞß" +
      "àáâãäåæçèéêëìíîï" +
      "ðñòóôõö÷øùúûüýþÿ",
    Map.empty, Map.empty, AsciiEscapeSequences ++ Map(
      "cent" -> List(0xA2),
      "pound" -> List(0xA3),
      "euro" -> List(0xA4),
      "yen" -> List(0xA5),
      "copy" -> List(0xA9),
    )
  )

  lazy val Apple2 = new TableTextCodec("APPLE-II", 0, 0.until(255).map { i =>
    if (i < 0xa0) NotAChar
    else if (i < 0xe0) (i - 128).toChar
    else NotAChar
  }.mkString,
    ('a' to 'z').map(l => l -> (l - 'a' + 0xC1)).toMap, Map.empty, MinimalEscapeSequencesWithBraces)

  lazy val IsoIec646De = new TableTextCodec("ISO-IEC-646-DE", 0,
    "\ufffd" * 32 +
      " !\"#$%^'()*+,-./0123456789:;<=>?" +
      "§ABCDEFGHIJKLMNOPQRSTUVWXYZÄÖÜ^_" +
      "`abcdefghijklmnopqrstuvwxyzäöüß",
    DefaultOverrides, Map.empty, AsciiEscapeSequences ++ Map(
      "UE" -> List('['.toInt),
      "OE" -> List('\\'.toInt),
      "AE" -> List(']'.toInt),
      "ue" -> List('{'.toInt),
      "oe" -> List('|'.toInt),
      "ae" -> List('}'.toInt),
      "ss" -> List('~'.toInt)
    )
  )

  lazy val IsoIec646Se = new TableTextCodec("ISO-IEC-646-SE", 0,
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
    Map.empty, AsciiEscapeSequences ++ Map(
      "AE" -> List('['.toInt),
      "OE" -> List('\\'.toInt),
      "AA" -> List(']'.toInt),
      "ae" -> List('{'.toInt),
      "oe" -> List('|'.toInt),
      "aa" -> List('}'.toInt)
    )
  )

  lazy val IsoIec646No = new TableTextCodec("ISO-IEC-646-NO", 0,
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
    Map.empty, AsciiEscapeSequences ++ Map(
      "AE" -> List('['.toInt),
      "OE" -> List('\\'.toInt),
      "AA" -> List(']'.toInt),
      "ae" -> List('{'.toInt),
      "oe" -> List('|'.toInt),
      "aa" -> List('}'.toInt)
    )
  )


  lazy val IsoIec646Yu = new TableTextCodec("ISO-IEC-646-YU", 0,
    "\ufffd" * 32 +
      " !\"#$%^'()*+,-./0123456789:;<=>?" +
      "ŽABCDEFGHIJKLMNOPQRSTUVWXYZŠĐĆČ_" +
      "žabcdefghijklmnopqrstuvwxyzšđćč",
    Map('Ë' -> '$'.toInt, 'ë' -> '_'.toInt),
    Map.empty, AsciiEscapeSequences)

  val CbmScreencodes = new TableTextCodec("CBM-Screen", 0xE0,
    "@abcdefghijklmnopqrstuvwxyz[£]↑←" +
      0x20.to(0x3f).map(_.toChar).mkString +
      "–ABCDEFGHIJKLMNOPQRSTUVWXYZ\ufffd\ufffd\ufffdπ",
    Map('^' -> 0x1E, '♥' -> 0x53, '♡' -> 0x53, '♠' -> 0x41, '♣' -> 0x58, '♢' -> 0x5A, '•' -> 0x51),
    Map.empty, MinimalEscapeSequencesWithoutBraces ++ Map(
      "pound" -> List(0x1c),
      "pi" -> List(0x5f),
    )
  )

  lazy val CbmScreencodesJp = new TableTextCodec("CBM-Screen-JP", 0xE0,
    "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[¥]↑←" + // 00-1f
      0x20.to(0x3f).map(_.toChar).mkString +
      "タチツテトナニヌネノハヒフヘホマ" + // 40-4f
      "ミムメモヤユヨラリルレロワン゛゜" + // 50-5f
      "\ufffd円年月\ufffd\ufffdヲ\ufffd" + // 60-67
      "πアイウエオカキクケコサシスセソ" + // 70-7f
      "",
    Map('^' -> 0x1E, '\\' -> 0x1C,
      '♥' -> 0x44, '♡' -> 0x44, '♠' -> 0x41, '♣' -> 0x7B, '♢' -> 0x42, '•' -> 0x5D,
      'ー' -> '-'.toInt, 0xff70.toChar -> '-'.toInt, 0xff66.toChar -> 0x66,
      'ヮ' -> 0x5C, 'ヵ' -> 0x76, 'ヶ' -> 0x79,
      'ァ' -> 0x71, 0xff67.toChar -> 0x71,
      'ィ' -> 0x72, 0xff68.toChar -> 0x72,
      'ゥ' -> 0x73, 0xff69.toChar -> 0x73,
      'ェ' -> 0x74, 0xff6a.toChar -> 0x74,
      'ォ' -> 0x75, 0xff6b.toChar -> 0x75,
      'ャ' -> 0x54, 0xff6c.toChar -> 0x54,
      'ュ' -> 0x55, 0xff6d.toChar -> 0x55,
      'ョ' -> 0x56, 0xff6e.toChar -> 0x56,
      'ッ' -> 0x42, 0xff6f.toChar -> 0x42
    ) ++
      ('a' to 'z').map(l => l -> (l - 'a' + 1)) ++
      (1 to 0xf).map(i => (i + 0xff70).toChar -> (i + 0x70)) ++
      (0x10 to 0x2f).map(i => (i + 0xff70).toChar -> (i + 0x40)),
    StandardKatakanaDecompositions, MinimalEscapeSequencesWithoutBraces ++ Map(
      "pi" -> List(0x70),
      "yen" -> List(0x1c),
    )
  )

  lazy val Petscii = new TableTextCodec("PETSCII", 0,
    "\ufffd" * 32 +
      0x20.to(0x3f).map(_.toChar).mkString +
      "@abcdefghijklmnopqrstuvwxyz[£]↑←" +
      "\ufffd" * 32 + // 60-7f
      "\ufffd" * 32 + // 80-9f
      "\ufffd" * 32 + // a0-bf
      "–ABCDEFGHIJKLMNOPQRSTUVWXYZ\ufffd\ufffd\ufffdπ", // c0-df
    Map('^' -> 0x5E, '♥' -> 0xD3, '♡' -> 0xD3, '♠' -> 0xC1, '♣' -> 0xD8, '♢' -> 0xDA, '•' -> 0xD1), Map.empty, Map(
      "n" -> List(13),
      "q" -> List('\"'.toInt),
      "pound" -> List(0x5c),
      "pi" -> List(0xdf),
      "apos" -> List('\''.toInt),
      "up" -> List(0x91),
      "down" -> List(0x11),
      "left" -> List(0x9d),
      "right" -> List(0x1d),
      "white" -> List(5),
      "black" -> List(0x90),
      "red" -> List(0x1c),
      "blue" -> List(0x1f),
      "green" -> List(0x1e),
      "cyan" -> List(0x9f),
      "purple" -> List(0x9c),
      "yellow" -> List(0x9e),
      "reverse" -> List(0x12),
      "reverseoff" -> List(0x92)
    )
  )

  lazy val PetsciiJp = new TableTextCodec("PETSCII-JP", 0,
    "\ufffd" * 32 +
      0x20.to(0x3f).map(_.toChar).mkString +
      "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[¥]↑←" +
      "\ufffd" * 32 + // 60-7f
      "\ufffd" * 32 + // 80-9f
      "\ufffd円年月\ufffd\ufffdヲ\ufffd" + // a0-a7
      "\ufffd" * 8 + // a8-af
      "πアイウエオカキクケコサシスセソ" + // b0-bf
      "タチツテトナニヌネノハヒフヘホマ" + // c0-cf
      "ミムメモヤユヨラリルレロワン゛゜", // d0-df
    Map('^' -> 0x5E, '\\' -> 0x5C,
      '♥' -> 0xC4, '♡' -> 0x73, '♠' -> 0xC1, '♣' -> 0xBB, '♢' -> 0xC2, '•' -> 0xDD,
      'ー' -> '-'.toInt, 0xff70.toChar -> '-'.toInt, 0xff66.toChar -> 0xa6,
      'ヮ' -> 0xDC, 'ヵ' -> 0xB6, 'ヶ' -> 0xB9,
      'ァ' -> 0xB1, 0xff67.toChar -> 0xB1,
      'ィ' -> 0xB2, 0xff68.toChar -> 0xB2,
      'ゥ' -> 0xB3, 0xff69.toChar -> 0xB3,
      'ェ' -> 0xB4, 0xff6a.toChar -> 0xB4,
      'ォ' -> 0xB5, 0xff6b.toChar -> 0xB5,
      'ャ' -> 0xD4, 0xff6c.toChar -> 0xD4,
      'ュ' -> 0xD5, 0xff6d.toChar -> 0xD5,
      'ョ' -> 0xD6, 0xff6e.toChar -> 0xD6,
      'ッ' -> 0xC2, 0xff6f.toChar -> 0xC2) ++
      ('a' to 'z').map(l => l -> l.toUpper.toInt) ++
      (1 to 0x2f).map(i => (i+0xff70).toChar -> (i+0xb0)),
    StandardKatakanaDecompositions, Map(
      "n" -> List(13),
      "q" -> List('\"'.toInt),
      "apos" -> List('\''.toInt),
      "yen" -> List(0x5c),
      "pi" -> List(0xb0),
      "up" -> List(0x91),
      "down" -> List(0x11),
      "left" -> List(0x9d),
      "right" -> List(0x1d),
      "white" -> List(5),
      "black" -> List(0x90),
      "red" -> List(0x1c),
      "blue" -> List(0x1f),
      "green" -> List(0x1e),
      "cyan" -> List(0x9f),
      "purple" -> List(0x9c),
      "yellow" -> List(0x9e),
      "reverse" -> List(0x12),
      "reverseoff" -> List(0x92)
    )
  )

  lazy val Vectrex = new TableTextCodec("Vectrex", 0x80,
    "\ufffd" * 32 +
      0x20.to(0x3f).map(_.toChar).mkString +
      "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_" +
      "\ufffd↑\ufffd↓\ufffd\ufffd\ufffd©\ufffd\ufffd\ufffd\ufffd∞",
      ('a' to 'z').map(l => l -> l.toUpper.toInt).toMap,
    Map.empty, Map(
      "copy" -> List('g'.toInt)
    )
  )

  lazy val Koi7N2 = new TableTextCodec("KOI-7 N2", 0,
    "\ufffd" * 32 +
      " !\"#¤%&'()*+,-./" +
      "0123456789:;<=>?" +
      "@ABCDEFGHIJKLMNO" +
      "PQRSTUVWXYZ[\\]^_" +
      "ЮАБЦДЕФГХИЙКЛМНО" +
      "ПЯРСТУЖВЬЫЗШЭЩЧ",
    Map('↑' -> 0x5E, '$' -> 0x24) ++
      ('a' to 'z').map(l => l -> l.toUpper.toInt).toMap ++
      ('а' to 'я').filter(_ != 'ъ').map(l => l -> l.toUpper.toInt).toMap,
    Map.empty, Map(
      "n" -> List(13), // TODO: ?
      "b" -> List(8), // TODO: ?
      "q" -> List('\"'.toInt),
      "apos" -> List('\''.toInt)
    )
  )

  lazy val OldPetscii = new TableTextCodec("Old PETSCII", 0,
    "\ufffd" * 32 +
      0x20.to(0x3f).map(_.toChar).mkString +
      "@abcdefghijklmnopqrstuvwxyz[\\]↑←" +
      "\ufffd" * 32 +
      "\ufffd" * 32 +
      "\ufffd" * 32 +
      "–ABCDEFGHIJKLMNOPQRSTUVWXYZ\ufffd\ufffd\ufffdπ",
    Map('^' -> 0x5E, '♥' -> 0xD3, '♡' -> 0xD3, '♠' -> 0xC1, '♣' -> 0xC8, '♢' -> 0xDA, '•' -> 0xD1), Map.empty, Map(
      "n" -> List(13),
      "q" -> List('\"'.toInt),
      "pi" -> List(0xdf),
      "apos" -> List('\''.toInt),
      "up" -> List(0x91),
      "down" -> List(0x11),
      "left" -> List(0x9d),
      "right" -> List(0x1d),
      "reverse" -> List(0x12),
      "reverseoff" -> List(0x92)
    )
  )

  lazy val OriginalPetscii = new TableTextCodec("Original PETSCII", 0,
    "\ufffd" * 32 +
      0x20.to(0x3f).map(_.toChar).mkString +
      "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]↑←" +
      "\ufffd" * 32 +
      "\ufffd" * 32 +
      "\ufffd" * 32 +
      "–abcdefghijklmnopqrstuvwxyz\ufffd\ufffd\ufffdπ",
    Map('^' -> 0x5E, '♥' -> 0xD3, '♡' -> 0xD3, '♠' -> 0xC1, '♣' -> 0xC8, '♢' -> 0xDA, '•' -> 0xD1), Map.empty, Map(
      "n" -> List(13),
      "q" -> List('\"'.toInt),
      "apos" -> List('\''.toInt),
      "pi" -> List(0xdf),
      "up" -> List(0x91),
      "down" -> List(0x11),
      "left" -> List(0x9d),
      "right" -> List(0x1d),
      "reverse" -> List(0x12),
      "reverseoff" -> List(0x92)
    )
  )

  lazy val Atascii = new TableTextCodec("ATASCII", 0,
    "♡" +
    "\ufffd" * 15 +
      "♣\ufffd–\ufffd•" +
    "\ufffd" * 11 +
      0x20.to(0x5f).map(_.toChar).mkString +
      "♢abcdefghijklmnopqrstuvwxyz♠|",
    Map('♥' -> 0, '·' -> 0x14), Map.empty, MinimalEscapeSequencesWithoutBraces ++ Seq(
      "n" -> List(0x9b),
      "up" -> List(0x1c),
      "down" -> List(0x1d),
      "left" -> List(0x1e),
      "right" -> List(0x1f),
      "b" -> List(0x7e),
    )
  )

  lazy val AtasciiScreencodes = new TableTextCodec("ATASCII-Screen", 0xDB,
    0x20.to(0x3f).map(_.toChar).mkString +
      0x40.to(0x5f).map(_.toChar).mkString +
      "♡" +
      "\ufffd" * 15 +
      "♣\ufffd–\ufffd•" +
      "\ufffd" * 7 + "↑↓←→"+
      "♢abcdefghijklmnopqrstuvwxyz♠|",
    Map('♥' -> 0x40, '·' -> 0x54), Map.empty, MinimalEscapeSequencesWithoutBraces
  )

  lazy val Bbc = new TableTextCodec("BBC", 0,
    "\ufffd" * 32 +
      0x20.to(0x5f).map(_.toChar).mkString +
      "£" + 0x61.to(0x7E).map(_.toChar).mkString + "©",
    Map('↑' -> '^'.toInt), Map.empty, MinimalEscapeSequencesWithBraces ++ Map(
      "n" -> List(13),
      "pound" -> List(0x60),
      "copy" -> List(0x7f),
    )
  )

  lazy val Sinclair = new TableTextCodec("Sinclair", 0,
    "\ufffd" * 32 +
      0x20.to(0x5f).map(_.toChar).mkString +
      "£" + 0x61.to(0x7E).map(_.toChar).mkString + "©",
    Map('↑' -> '^'.toInt), Map.empty, Map(
      "n" -> List(13),
      "q" -> List('\"'.toInt),
      "apos" -> List('\''.toInt),
      "pound" -> List(0x60),
      "copy" -> List(0x7f),
      "lbrace" -> List('{'.toInt),
      "rbrace" -> List('}'.toInt),
      "up" -> List(11),
      "down" -> List(10),
      "left" -> List(8),
      "right" -> List(9),
      "white" -> List(0x10, 7),
      "black" -> List(0x10, 8),
      "red" -> List(0x10, 2),
      "blue" -> List(0x10, 1),
      "green" -> List(0x10, 4),
      "cyan" -> List(0x10, 5),
      "purple" -> List(0x10, 3),
      "yellow" -> List(0x10, 6),
      "bgwhite" -> List(0x11, 7),
      "bgblack" -> List(0x11, 8),
      "bgred" -> List(0x11, 2),
      "bgblue" -> List(0x11, 1),
      "bggreen" -> List(0x11, 4),
      "bgcyan" -> List(0x11, 5),
      "bgpurple" -> List(0x11, 3),
      "bgyellow" -> List(0x11, 6),
      "reverse" -> List(0x14, 1),
      "reverseoff" -> List(0x14, 0)
    )
  )

  lazy val Zx80 = new TableTextCodec("ZX80", 1,
    " \ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd" +
      "£$:?()-+*/=><;,." +
      "0123456789" +
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
      "\ufffd" * (9 * 16) +
      "\ufffd\ufffd\ufffd\ufffd\"",
    ('a' to 'z').map(l => l -> (l - 'a' + 0x26)).toMap,
    Map.empty, Map(
      "pound" -> List(0x0c),
      "q" -> List(0xd4),
      "apos" -> List(212),
      "n" -> List(0x76),
      "b" -> List(0x77),
      "up" -> List(0x70),
      "down" -> List(0x71),
      "left" -> List(0x72),
      "right" -> List(0x73),
    )
  )

  lazy val Zx81 = new TableTextCodec("ZX81", 11,
    " \ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd" +
      "£$:?()><=+-*/;,." +
      "0123456789" +
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
      "\ufffd" * (8 * 16) +
      "\"",
    ('a' to 'z').map(l => l -> (l - 'a' + 0x26)).toMap,
    Map.empty, Map(
      "pound" -> List(0x0c),
      "q" -> List(0xc0),
      "n" -> List(0x76),
      "b" -> List(0x77),
      "up" -> List(0x70),
      "down" -> List(0x71),
      "left" -> List(0x72),
      "right" -> List(0x73),
    )
  )

  private val jisHalfwidthKatakanaOrder: String =
    "\ufffd。「」、・ヲァィゥェォャュョッ" +
    "ーアイウエオカキクケコサシスセソ" +
    "タチツテトナニヌネノハヒフヘホマ" +
    "ミムメモヤユヨラリルレロワン゛゜"

  //noinspection ScalaUnnecessaryParentheses
  lazy val Jis = new TableTextCodec("JIS-X-0201", 0,
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
      1.to(0x3F).map(i => (i + 0xff60).toChar -> (i + 0xA0)).toMap,
      StandardKatakanaDecompositions, MinimalEscapeSequencesWithBraces ++ Map(
      "n" -> List(13, 10),
      "yen" -> List(0x5c)
    )
  )

  lazy val MsxWest = new TableTextCodec("MSX-International", 0,
    "\ufffd" * 32 +
      (0x20 to 0x7e).map(_.toChar).mkString("") +
      "\ufffd" +
      "ÇüéâäàåçêëèïîìÄÅ" +
      "ÉæÆôöòûùÿÖÜ¢£¥₧ƒ" +
      "áíóúñÑªº¿⌐¬½¼¡«»" +
      "ÃãĨĩÕõŨũĲĳ¾\ufffd\ufffd‰¶§" +
      "\ufffd" * 24 +
      "Δ\ufffdω\ufffd\ufffd\ufffd\ufffd\ufffd" +
      "αβΓΠΣσµγΦθΩδ∞∅∈∩" +
      "≡±≥≤\ufffd\ufffd÷\ufffd\ufffd\ufffd\ufffd\ufffdⁿ²",
    Map('ß' -> 0xE1, '¦' -> 0x7C, 'Ő' -> 0xB4, 'ő' -> 0xB5, 'Ű' -> 0xB6, 'ű' -> 0xB7),
    Map('♥' -> "\u0001C", '♡' -> "\u0001C", '♢' -> "\u0001D", '♢' -> "\u0001D", '♣' -> "\u0001E", '♠' -> "\u0001F", '·' -> "\u0001G") ,
    MinimalEscapeSequencesWithBraces ++ Map(
      "right" -> List(0x1c),
      "left" -> List(0x1d),
      "up" -> List(0x1e),
      "down" -> List(0x1f),
      "b" -> List(8),
      "n" -> List(13, 10),
      "pound" -> List(0x9c),
      "yen" -> List(0x9d),
    )
  )

  lazy val MsxBr = new TableTextCodec("MSX-BR", 0,
    "\ufffd" * 32 +
      (0x20 to 0x7e).map(_.toChar).mkString("") +
      "\ufffd" +
      "ÇüéâÁà\ufffdçêÍÓÚÂÊÔÀ" +
      "ÉæÆôöòûùÿÖÜ¢£¥₧ƒ" +
      "áíóúñÑªº¿⌐¬½¼¡«»" +
      "ÃãĨĩÕõŨũĲĳ¾\ufffd\ufffd‰¶§" +
      "\ufffd" * 24 +
      "Δ\ufffdω\ufffd\ufffd\ufffd\ufffd\ufffd" +
      "αβΓΠΣσµγΦθΩδ∞∅∈∩" +
      "≡±≥≤\ufffd\ufffd÷\ufffd\ufffd\ufffd\ufffd\ufffdⁿ²",
    Map('ß' -> 0xE1, '¦' -> 0x7C, 'Ő' -> 0xB4, 'ő' -> 0xB5, 'Ű' -> 0xB6, 'ű' -> 0xB7),
    Map('♥' -> "\u0001C", '♡' -> "\u0001C", '♢' -> "\u0001D", '♢' -> "\u0001D", '♣' -> "\u0001E", '♠' -> "\u0001F", '·' -> "\u0001G") ,
    MinimalEscapeSequencesWithBraces ++ Map(
      "right" -> List(0x1c),
      "left" -> List(0x1d),
      "up" -> List(0x1e),
      "down" -> List(0x1f),
      "n" -> List(13, 10),
      "b" -> List(8),
      "pound" -> List(0x9c),
      "yen" -> List(0x9d),
    )
  )

  lazy val MsxRu = new TableTextCodec("MSX-RU", 0,
    "\ufffd" * 32 +
      (0x20 to 0x7e).map(_.toChar).mkString("") +
      "\ufffd" +
      "\ufffd" * 16 +
      "\ufffd" * 8 +
      "Δ\ufffdω\ufffd\ufffd\ufffd\ufffd\ufffd" +
      "αβΓΠΣσµγΦθΩδ∞∅∈∩" +
      "≡±≥≤\ufffd\ufffd÷\ufffd\ufffd\ufffd\ufffd\ufffdⁿ²\ufffd¤" +
      "юабцдефгхийклмнопярстужвьызшэщчъ" +
      "ЮАБЦДЕФГХИЙКЛМНОПЯРСТУЖВЬЫЗШЭЩ",
    Map('ß' -> 0xA1, '¦' -> 0x7C),
    Map('♥' -> "\u0001C", '♡' -> "\u0001C", '♢' -> "\u0001D", '♢' -> "\u0001D", '♣' -> "\u0001E", '♠' -> "\u0001F", '·' -> "\u0001G"),
    MinimalEscapeSequencesWithBraces ++ Map(
      "right" -> List(0x1c),
      "left" -> List(0x1d),
      "up" -> List(0x1e),
      "down" -> List(0x1f),
      "b" -> List(8),
      "n" -> List(13, 10)
    )
  )

  lazy val MsxJp = new TableTextCodec("MSX-JP", 0,
    "\ufffd" * 32 +
      (0x20 to 0x7e).map(c => if (c == 0x5c) '¥' else c.toChar).mkString("") +
      "\ufffd" +
      "♠♡♣♢\uffdd·をぁぃぅぇぉゃゅょっ" +
      "　あいうえおかきくけこさしすせそ" +
      jisHalfwidthKatakanaOrder +
      "たちつてとなにぬねのはひふへほま" +
      "みむめもやゆよらりるれろわん" +
      "" +
      "",
    Map('♥' -> 0x81, '¦' -> 0x7C) ++
      1.to(0x3F).map(i => (i + 0xff60).toChar -> (i + 0xA0)).toMap,
    Map(
      '月' -> "\u0001A",
      '火' -> "\u0001B",
      '水' -> "\u0001C",
      '木' -> "\u0001D",
      '金' -> "\u0001E",
      '土' -> "\u0001F",
      '日' -> "\u0001G",
      '年' -> "\u0001H",
      '円' -> "\u0001I",
      '時' -> "\u0001J",
      '分' -> "\u0001K",
      '秒' -> "\u0001L",
      '百' -> "\u0001M",
      '千' -> "\u0001N",
      '万' -> "\u0001O",
      '大' -> "\u0001]",
      '中' -> "\u0001^",
      '小' -> "\u0001_"
    ) ++
      StandardHiraganaDecompositions ++ StandardKatakanaDecompositions,
    MinimalEscapeSequencesWithBraces ++ Map(
      "right" -> List(0x1c),
      "left" -> List(0x1d),
      "up" -> List(0x1e),
      "down" -> List(0x1f),
      "b" -> List(8),
      "n" -> List(13, 10),
      "yen" -> List(0x5c)
    )
  )

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
