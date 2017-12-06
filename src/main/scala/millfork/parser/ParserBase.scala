package millfork.parser

import millfork.node.Position

/**
  * @author Karol Stasiak
  */
case class ParseException(msg: String, position: Option[Position]) extends Exception

class ParserBase(filename: String, input: String) {

  def reset(): Unit = {
    cursor = 0
    line = 1
    column = FirstColumn
  }

  private val FirstColumn = 0
  private val length = input.length
  private var cursor = 0
  private var line = 1
  private var column = FirstColumn

  def position = Position(filename, line, column, cursor)

  def restorePosition(p: Position): Unit = {
    cursor = p.cursor
    column = p.column
    line = p.line
  }

  def error(msg: String, pos: Option[Position]): Nothing = throw ParseException(msg, pos)

  def error(msg: String, pos: Position): Nothing = throw ParseException(msg, Some(pos))

  def error(msg: String): Nothing = throw ParseException(msg, Some(position))

  def error() = throw ParseException("Syntax error", Some(position))

  def nextChar() = {
    if (cursor >= length) error("Unexpected end of input")
    val c = input(cursor)
    cursor += 1
    if (c == '\n') {
      line += 1
      column = FirstColumn
    } else {
      column += 1
    }
    c
  }

  def peekChar(): Char = {
    if (cursor >= length) '\0' else input(cursor)
  }

  def require(char: Char): Char = {
    val pos = position
    val c = nextChar()
    if (c != char) error(s"Expected `$char`", pos)
    c
  }
  def require(p: Char=>Boolean, errorMsg: String = "Unexpected character"): Char = {
    val pos = position
    val c = nextChar()
    if (!p(c)) error(errorMsg, pos)
    c
  }

  def require(s: String): String = {
    val c = peekChars(s.length)
    if (c != s) error(s"Expected `$s`")
    1 to s.length foreach (_=>nextChar())
    s
  }

  def requireAny(s: String, errorMsg: String = "Unexpected character"): Char = {
    val c = nextChar()
    if (s.contains(c)) c
    else error(errorMsg)
  }

  def peek2Chars(): String = {
    peekChars(2)
  }

  def peekChars(n: Int): String = {
    if (cursor > length - n) input.substring(cursor) else input.substring(cursor, cursor + n)
  }

  def charsWhile(pred: Char => Boolean, min: Int = 0, errorMsg: String = "Unexpected character"): String = {
    val sb = new StringBuilder()
    while (pred(peekChar())) {
      sb += nextChar()
    }
    val s = sb.toString
    if (s.length < min) error(errorMsg)
    else s
  }

  def skipNextIfMatches(c: Char): Boolean = {
    if (peekChar() == c) {
      nextChar()
      true
    } else {
      false
    }
  }

  def either(c: Char, s: String): Unit = {
    if (peekChar() == c) {
      nextChar()
    } else if (peekChars(s.length) == s) {
      require(s)
    } else {
      error(s"Expected either `$c` or `$s`")
    }
  }

  def sepOrEnd(sep: Char, end: Char): Boolean = {
    val p = position
    val c = nextChar()
    if (c == sep) true
    else if  (c==end) false
    else error(s"Expected `$sep` or `$end`", p)
  }

  def anyOf[T](errorMsg: String, alternatives: (()=> T)*): T = {
    alternatives.foreach { t =>
      val p = position
      try {
        return t()
      } catch {
        case _: ParseException => restorePosition(p)
      }
    }
    error(errorMsg)
  }

  def surrounded[T](left: => Any, content: => T, right: => Any): T = {
    left
    val result = content
    right
    content
  }

  def followed[T](content: => T, right: => Any): T = {
    val result = content
    right
    content
  }

  def attempt[T](content: => T): Option[T] = {
    val p = position
    try {
      Some(content)
    } catch {
      case _: ParseException => None
    }
  }

  def opaque[T](errorMsg: String)(block: =>T) :T={
   try {
     block
   } catch{
     case p:ParseException => error(errorMsg, p.position)
   }
  }
}
