package millfork.error

import millfork.node.Position

/**
  * @author Karol Stasiak
  */
trait Logger {

  def setFatalWarnings(fatalWarnings: Boolean): Unit

  def info(msg: String, position: Option[Position] = None): Unit

  def debug(msg: String, position: Option[Position] = None): Unit

  def trace(msg: String, position: Option[Position] = None): Unit

  def traceEnabled: Boolean

  def warn(msg: String, position: Option[Position] = None): Unit

  def error(msg: String, position: Option[Position] = None): Unit

  def fatal(msg: String, position: Option[Position] = None): Nothing

  def fatalQuit(msg: String, position: Option[Position] = None): Nothing

  def assertNoErrors(msg: String): Unit

  def addSource(filename: String, lines: IndexedSeq[String]): Unit
}