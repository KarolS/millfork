package millfork.parser

import fastparse.all._
import fastparse.core

object MinimalTestCase {
  def AWS: P[Unit] = "\n".rep(min = 0).opaque("<any whitespace>").log()

  def EOL: P[Unit] = "\n".rep(min = 1).opaque("<line break>").log()

  def identifier: P[String] = CharPred(_.isLetter).rep(min = 1).!.opaque("<identifier>").log()

  def identifierWithSpace: P[String] = (identifier ~/ AWS ~/ Pass).opaque("<identifier with space>").log()

  def separator: P[Unit] = ("," ~/ AWS ~/ Pass).opaque("<comma>").log()

  def identifiers: P[Seq[String]] = identifierWithSpace.rep(min = 0, sep = separator)//.opaque("<separated identifiers>").log()

  def array: P[Seq[String]] = ("[" ~/ AWS ~/ identifiers ~/ "]" ~/ Pass)//.opaque("<array>").log()

  def arrays: Parser[Seq[Seq[String]]] = (array ~/ EOL).rep(min = 0, sep = !End ~/ Pass)//.opaque("<arrays>").log()

  def program: Parser[Seq[Seq[String]]] = Start ~/ AWS ~/ arrays ~/ End
}
