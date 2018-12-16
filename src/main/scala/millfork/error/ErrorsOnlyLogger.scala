package millfork.error

import millfork.assembly.SourceLine
import millfork.node.Position

/**
  * @author Karol Stasiak
  */
class ErrorsOnlyLogger(inner: Logger) extends Logger {
  override def setFatalWarnings(fatalWarnings: Boolean): Unit = ()

  override def info(msg: String, position: Option[Position]): Unit = ()

  override def debug(msg: String, position: Option[Position]): Unit = ()

  override def trace(msg: String, position: Option[Position]): Unit = ()

  override def traceEnabled: Boolean = false

  override def warn(msg: String, position: Option[Position]): Unit = ()

  override def error(msg: String, position: Option[Position]): Unit = inner.error(msg, position)

  override def fatal(msg: String, position: Option[Position]): Nothing = inner.fatal(msg, position)

  override def fatalQuit(msg: String, position: Option[Position]): Nothing = inner.fatalQuit(msg, position)

  override def assertNoErrors(msg: String): Unit = inner.assertNoErrors(msg)

  override def addSource(filename: String, lines: IndexedSeq[String]): Unit = ()

  override def getLine(line: SourceLine): Option[String] = None
}
