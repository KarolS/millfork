package millfork.error

import millfork.node.Position

/**
  * Logger for fatal errors due to bugs in the compiler.
  * Should never (hopefully) be called.
  * @author Karol Stasiak
  */
object FatalErrorReporting {

  private var globalLogger: Logger = _

  def considerAsGlobal(logger: Logger): Unit = {
    if (globalLogger eq null) {
      globalLogger = logger
    }
  }

  def reportFlyingPig(msg: String, position: Option[Position] = None): Nothing = globalLogger.fatalQuit(msg, position)
}
