package millfork.error

import millfork.assembly.SourceLine
import millfork.node.Position

import scala.collection.mutable

class ConsoleLogger extends Logger {
  FatalErrorReporting.considerAsGlobal(this)

  var verbosity = 0
  var fatalWarnings = false

  override def setFatalWarnings(fatalWarnings: Boolean): Unit = {
    this.fatalWarnings = fatalWarnings
  }

  var hasErrors = false

  private val sourceLines: mutable.Map[String, IndexedSeq[String]] = mutable.Map()

  private def printErrorContext(pos: Option[Position]): Unit = synchronized {
    pos.foreach { position =>
        sourceLines.get(position.moduleName).foreach { lines =>
          val line = lines.apply(pos.get.line - 1)
          val column = pos.get.column - 1
          val margin = "       "
          print(margin)
          println(line)
          print(margin)
          print(" " * column)
          println("^")
        }
    }
  }

  @inline
  def f(position: Option[Position]): String = position.fold("")(p => s"(${p.moduleName}:${p.line}:${p.column}) ")

  override def info(msg: String, position: Option[Position] = None): Unit = {
    if (verbosity < 0) return
    println("INFO:  " + f(position) + msg)
    flushOutput()
  }

  override def debug(msg: String, position: Option[Position] = None): Unit = {
    if (verbosity < 1) return
    println("DEBUG: " + f(position) + msg)
    flushOutput()
  }

  override def traceEnabled: Boolean = verbosity >= 2

  override def trace(msg: String, position: Option[Position] = None): Unit = {
    if (verbosity < 2) return
    println("TRACE: " + f(position) + msg)
    flushOutput()
  }

  @inline
  private def flushOutput(): Unit = {
    System.out.flush()
    System.err.flush()
  }

  override def warn(msg: String, position: Option[Position] = None): Unit = {
    if (verbosity < 0) return
    println("WARN:  " + f(position) + msg)
    printErrorContext(position)
    flushOutput()
    if (fatalWarnings) {
      hasErrors = true
    }
  }

  override def error(msg: String, position: Option[Position] = None): Unit = {
    hasErrors = true
    println("ERROR: " + f(position) + msg)
    printErrorContext(position)
    flushOutput()
  }

  override def fatal(msg: String, position: Option[Position] = None): Nothing = {
    hasErrors = true
    println("FATAL: " + f(position) + msg)
    printErrorContext(position)
    flushOutput()
    throw new AssertionError(msg)
  }

  override def fatalQuit(msg: String, position: Option[Position] = None): Nothing = {
    hasErrors = true
    println("FATAL: " + f(position) + msg)
    printErrorContext(position)
    flushOutput()
    System.exit(1)
    throw new RuntimeException(msg)
  }

  def assertNoErrors(msg: String): Unit = {
    if (hasErrors) {
      error(msg)
      fatal("Build halted due to previous errors")
    }
  }

  def clearErrors(): Unit = synchronized {
    hasErrors = false
    sourceLines.clear()
  }

  def setSource(source: Option[IndexedSeq[String]]): Unit = synchronized {
    sourceLines.clear()
    source.foreach(sourceLines("") = _)
  }

  def addSource(filename: String, lines: IndexedSeq[String]): Unit = synchronized {
    sourceLines(filename) = lines
  }

  override def getLine(line: SourceLine): Option[String] = for {
    file <- sourceLines.get(line.moduleName)
    line <- file.lift(line.line - 1)
  } yield line
}