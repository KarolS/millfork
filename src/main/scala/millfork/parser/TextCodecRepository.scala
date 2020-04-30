package millfork.parser

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.Locale

import millfork.error.Logger
import millfork.node.Position

import scala.collection.mutable
import scala.collection.convert.ImplicitConversionsToScala._
import scala.util.matching.Regex

/**
  * @author Karol Stasiak
  */
class TextCodecRepository(val includePath: List[String]) {
  private var cache: mutable.Map[String, Option[TextCodec]] = mutable.Map()

  private def parse(shortname: String, lines: Seq[String], log: Logger): Option[TextCodec] = {
    import TextCodecRepository.{COMMENT, LINE, SINGLEHEX, HEXRANGE, UNICODECODEPOINT, CHAR, ESCAPE, HEXSTRING, DEPRECATED}
    val kvs = lines.flatMap{
      case LINE(k,v) => Some(k, v)
      case COMMENT() => None
      case DEPRECATED(msg) =>
        log.warn(s"Encoding $shortname is deprecated: $msg")
        None
      case line =>
        log.error(s"Unexpected line in encoding $shortname: $line")
        None
    }
    if (kvs.length == 1 && kvs.head._1 == "ALIAS") {
      val actualName = kvs.head._2
      load(actualName, log)
      return cache(actualName)
    }
    var name = ""
    var builtin = ""
    var terminator = -1
    val map = Array.fill[Char](256)('\ufffd')
    val extras = mutable.Map[Char, Int]()
    val decompositions = mutable.Map[Char, String]()
    val directDecompositions = mutable.Map[Char, List[Int]]()
    val escapeSequences = mutable.Map[String, List[Int]]()

    def hexToInt(h: String): Int = Integer.parseInt(h, 16)

    def hexToInts(h: String): List[Int] = if (h.length % 2 == 0) {
      h.grouped(2).map(b => Integer.parseInt(b, 16)).toList
    } else {
      log.error(s"Odd number of hex digits in encoding $shortname: $h")
      Nil
    }

    def putToMap(ix: Int, c: Char): Unit = {
      if (map(ix) != '\ufffd') log.error(s"Multiple characters in encoding $shortname defined for 0x${ix.toHexString}")
      map(ix) = c
    }

    kvs.foreach{
      case ("NAME", n) =>
        if (name != "") log.error(s"Encoding $shortname has multiple names")
        name = n
      case ("BUILTIN", b) =>
        if (builtin != "") log.error(s"Encoding $shortname refers to multiple built-ins")
        builtin = b.toUpperCase(Locale.ROOT)
      case ("ALIAS", n) => log.error(s"ALIAS encoding $shortname cannot contain any other entries")
      case ("EOT", SINGLEHEX(h)) =>
        if (terminator != -1) log.error(s"Encoding $shortname has multiple string terminators")
        terminator = hexToInt(h)
      case ("EOT", s) => log.error(s"Invalid string terminator in encoding $shortname: $s")
      case (SINGLEHEX(h), UNICODECODEPOINT(u)) =>
        val c = hexToInt(u)
        if (c > 0xffff) log.error(s"Invalid astral character $h=U+$u in encoding $shortname")
        else putToMap(hexToInt(h), c.toChar)
      case (SINGLEHEX(h), CHAR(c)) => putToMap(hexToInt(h), c.head)
      case (SINGLEHEX(h), s) => log.error(s"Invalid character $h=$s in encoding $shortname")
      case (range@HEXRANGE(f, t), cs) =>
        val from = hexToInt(f)
        val to = hexToInt(t)
        if (cs.length != to - from + 1) log.error(s"Mismatched range length and character count for $range in encoding $shortname")
        for (i <- 0 until ((to - from + 1) min cs.length)) {
          putToMap(from + i, cs(i))
        }
      case (range@HEXRANGE(_, _), s) => log.error(s"Invalid character range $range=$s in encoding $shortname")
      case (ESCAPE(e), HEXSTRING(h)) => escapeSequences(e) = hexToInts(h)
      case (ESCAPE(e), s) => log.error(s"Invalid escape sequence {$e}=$s in encoding $shortname")

      case (CHAR(e), HEXSTRING(h)) => directDecompositions(e.head) = hexToInts(h)
      case (UNICODECODEPOINT(u), HEXSTRING(h)) =>
        val c = hexToInt(u)
        if (c > 0xffff) log.error(s"Invalid astral character U+$u=$h in encoding $shortname")
        else directDecompositions(c.toChar) = hexToInts(h)

      case (CHAR(e), SINGLEHEX(h)) => extras(e.head) = hexToInt(h)
      case (UNICODECODEPOINT(u), SINGLEHEX(h)) =>
        val c = hexToInt(u)
        if (c > 0xffff) log.error(s"Invalid astral character U+$u=$h in encoding $shortname")
        else extras(c.toChar) = hexToInt(h)

      case (CHAR(e), s) if (s.startsWith(">")) =>
        if (s.length == 1) log.error(s"Empty decomposition $e=$s in encoding $shortname")
        decompositions(e.head) = s.tail
      case (UNICODECODEPOINT(u), s) if (s.startsWith(">")) =>
        if (s.length == 1) log.error(s"Empty decomposition U+$u=$s in encoding $shortname")
        val c = hexToInt(u)
        if (c > 0xffff) log.error(s"Invalid astral character U+$u=$s in encoding $shortname")
        else decompositions(c.toChar) = s.tail
      case ("KATAKANA", ">DECOMPOSE" | "DECOMPOSE") =>
        decompositions ++= TextCodec.StandardKatakanaDecompositions
      case ("HIRAGANA", ">DECOMPOSE" | "DECOMPOSE") =>
        decompositions ++= TextCodec.StandardHiraganaDecompositions
      case ("a-z", SINGLEHEX(h)) =>
        val start = hexToInt(h)
        for(c <- 'a' to 'z') {
          extras(c) = start + c.toInt - 'a'.toInt
        }
      case ("ю-ч", SINGLEHEX(h)) =>
        val start = hexToInt(h)
        val koi="юабцдефгхийклмнопярстужвьызшэщч"
        for(ix <- 0 until koi.length) {
          extras(koi(ix)) = start + ix
        }
      case ("ｱ-ﾝ", SINGLEHEX(h)) =>
        val start = hexToInt(h)
        for(c <- 'ｱ' to 'ﾝ') {
          extras(c) = start + c.toInt - 'ｱ'.toInt
        }
      case ("ｱ-ｿ", SINGLEHEX(h)) =>
        val start = hexToInt(h)
        for(c <- 'ｱ' to 'ｿ') {
          extras(c) = start + c.toInt - 'ｱ'.toInt
        }
      case ("ﾀ-ﾝ", SINGLEHEX(h)) =>
        val start = hexToInt(h)
        for(c <- 'ﾀ' to 'ﾝ') {
          extras(c) = start + c.toInt - 'ﾀ'.toInt
        }
      case (k, v) => log.error(s"Invalid command $k=$v in encoding $shortname")
    }
    if (name == "") {
      log.error(s"Nameless encoding $shortname")
    }
    if (builtin != "") {
      if (terminator != -1) log.error(s"Cannot redefine EOT for built-in encoding $builtin in encoding $shortname")
      if (decompositions.nonEmpty) log.error(s"Cannot redefine decompositions for built-in encoding $builtin in encoding $shortname")
      if (extras.nonEmpty) log.error(s"Cannot redefine extras for built-in encoding $builtin in encoding $shortname")
      if (escapeSequences.nonEmpty) log.error(s"Cannot redefine escape sequences for built-in encoding $builtin in encoding $shortname")
      if (!map.forall(_ == '\ufffd')) log.error(s"Cannot redefine characters for built-in encoding $builtin in encoding $shortname")
    }
    builtin match {
      case "" =>
        if (terminator == -1) {
          log.error(s"Undefined EOT for encoding $shortname")
        }
        for (d <- '0' to '9') {
          if (map.indexOf(d) < 0) log.warn(s"Missing digit $d in encoding $shortname")
        }
        Some(new TableTextCodec(name, terminator, map.mkString, extras.toMap, decompositions.toMap, directDecompositions.toMap, escapeSequences.toMap))
      case "UTF-8" => Some(TextCodecRepository.Utf8)
      case "UTF-16LE" => Some(TextCodecRepository.Utf16Le)
      case "UTF-16BE" => Some(TextCodecRepository.Utf16Be)
      case _ =>
        log.error(s"Unknown built-in encoding $builtin for encoding $shortname")
        None
    }
  }

  private def lookupFile(filename: String, log: Logger): Option[String] = {
    includePath.foreach { dir =>
      val file = Paths.get(dir, filename + ".tbl").toFile
      log.trace("Checking " + file)
      if (file.exists()) {
        return Some(file.getAbsolutePath)
      }
      val file2 = Paths.get(dir, "encoding", filename + ".tbl").toFile
      log.trace("Checking " + file2)
      if (file2.exists()) {
        return Some(file2.getAbsolutePath)
      }
    }
    log.trace(s"Encoding `$filename` not found")
    None
  }

  private def load(name: String, log: Logger): Unit = {
    if (cache.contains(name)) return
    cache(name) = lookupFile(name, log).flatMap(f => {
      val lines = Files.readAllLines(Paths.get(f), StandardCharsets.UTF_8).toIndexedSeq
      parse(name, lines, log)
    })
  }

  def forName(name: String, position: Option[Position], log: Logger): TextCodecWithFlags = {
    load(name, log)
    load(name.stripSuffix("z"), log)
    load(name.stripPrefix("p"), log)
    load(name.stripSuffix("z").stripPrefix("p"), log)
    cache(name) foreach(c =>  return TextCodecWithFlags(c, nullTerminated = false, lengthPrefixed = false, lenient = false))
    if (name.endsWith("z")) {
      val cleanName = name.stripSuffix("z")
      cache(cleanName) foreach(c =>  return TextCodecWithFlags(c, nullTerminated = true, lengthPrefixed = false, lenient = false))
    }
    val lengthPrefixed = name.startsWith("p")
    if (name.startsWith("p")) {
      val cleanName = name.stripPrefix("p")
      cache(cleanName) foreach(c =>  return TextCodecWithFlags(c, nullTerminated = false, lengthPrefixed = true, lenient = false))
      if (cleanName.endsWith("z")) {
        val cleanName2 = cleanName.stripSuffix("z")
        cache(cleanName2) foreach(c =>  return TextCodecWithFlags(c, nullTerminated = true, lengthPrefixed = true, lenient = false))
      }
    }
    log.error(s"Unknown string encoding: `$name`", position)
    TextCodecWithFlags(TextCodec.Ascii, nullTerminated = false, lengthPrefixed = false, lenient = false)
  }

}
object TextCodecRepository {
  val LINE: Regex = "\\A\\s*([^=\\s]+)\\s*=\\s*(.+?)\\s*\\z".r
  val COMMENT: Regex = "\\A\\s*(?:(?:;|#|//).*)?\\z".r
  val DEPRECATED: Regex = "\\A\\s*!\\s*[Dd][Ee][Pp][Rr][Ee][Cc][Aa][Tt][Ee][Dd]?(.*)\\z".r
  val SINGLEHEX: Regex = "\\A([0-9A-F-a-f]{2})\\z".r
  val HEXSTRING: Regex = "\\A([0-9A-F-a-f]+)\\z".r
  val HEXRANGE: Regex = "\\A([0-9A-F-a-f]{2})-([0-9A-F-a-f]{2})\\z".r
  val UNICODECODEPOINT: Regex = "\\A[Uu][-+]([0-9A-F-a-f]{1,5})\\z".r
  val ESCAPE: Regex = "\\A\\{([\\w.'\\p{L}]+)}\\z".r
  val CHAR: Regex = "\\A(\\S)\\z".r

  val Utf8 = new UnicodeTextCodec("UTF-8", StandardCharsets.UTF_8, List(0))

  val Utf16Be = new UnicodeTextCodec("UTF-16BE", StandardCharsets.UTF_16BE, List(0, 0))

  val Utf16Le = new UnicodeTextCodec("UTF-16LE", StandardCharsets.UTF_16LE, List(0, 0))
}