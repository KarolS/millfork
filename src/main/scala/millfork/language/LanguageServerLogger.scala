package millfork.language

import millfork.error.Logger
import millfork.node.Position
import millfork.assembly.SourceLine

class LanguageServerLogger extends Logger {
  // TODO: Complete stub to send diagnostics to client
  override def setFatalWarnings(fatalWarnings: Boolean): Unit = {}

  override def info(msg: String, position: Option[Position]): Unit = {}

  override def debug(msg: String, position: Option[Position]): Unit = {}

  override def trace(msg: String, position: Option[Position]): Unit = {}

  override def traceEnabled: Boolean = false

  override def debugEnabled: Boolean = false

  override def warn(msg: String, position: Option[Position]): Unit = {}

  override def error(msg: String, position: Option[Position]): Unit = {}

  override def fatal(msg: String, position: Option[Position]): Nothing = ???

  override def fatalQuit(msg: String, position: Option[Position]): Nothing = ???

  override def assertNoErrors(msg: String): Unit = {}

  override def addSource(filename: String, lines: IndexedSeq[String]): Unit = {}

  override def getLine(line: SourceLine): Option[String] = None

}
