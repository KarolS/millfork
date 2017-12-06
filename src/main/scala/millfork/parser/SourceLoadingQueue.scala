package millfork.parser

import java.nio.file.{Files, Paths}

import fastparse.core.Parsed.{Failure, Success}
import millfork.CompilationOptions
import millfork.error.ErrorReporting
import millfork.node.{ImportStatement, Position, Program}

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
class SourceLoadingQueue(val initialFilenames: List[String], val includePath: List[String], val options: CompilationOptions) {

  private val parsedModules = mutable.Map[String, Program]()
  private val moduleQueue = mutable.Queue[() => Unit]()
  val extension: String = ".ml"


  def run(): Program = {
    initialFilenames.foreach { i =>
      parseModule(extractName(i), includePath, Right(i), options)
    }
    options.platform.startingModules.foreach {m =>
      moduleQueue.enqueue(() => parseModule(m, includePath, Left(None), options))
    }
    while (moduleQueue.nonEmpty) {
      moduleQueue.dequeueAll(_ => true).par.foreach(_())
    }
    ErrorReporting.assertNoErrors("Parse failed")
    parsedModules.values.reduce(_ + _)
  }

  def lookupModuleFile(includePath: List[String], moduleName: String, position: Option[Position]): String = {
    includePath.foreach { dir =>
      val file = Paths.get(dir, moduleName + extension).toFile
      ErrorReporting.debug("Checking " + file)
      if (file.exists()) {
        return file.getAbsolutePath
      }
    }
    ErrorReporting.fatal(s"Module `$moduleName` not found", position)
  }

  def parseModule(moduleName: String, includePath: List[String], why: Either[Option[Position], String], options: CompilationOptions): Unit = {
    val filename: String = why.fold(p => lookupModuleFile(includePath, moduleName, p), s => s)
    ErrorReporting.debug(s"Parsing $filename")
    val path = Paths.get(filename)
    val parentDir = path.toFile.getAbsoluteFile.getParent
    val src = new String(Files.readAllBytes(path))
    val parser = MfParser(filename, src, parentDir, options)
    parser.toAst match {
      case Success(prog, _) =>
        parsedModules.synchronized {
          parsedModules.put(moduleName, prog)
          prog.declarations.foreach {
            case s@ImportStatement(m) =>
              if (!parsedModules.contains(m)) {
                moduleQueue.enqueue(() => parseModule(m, parentDir :: includePath, Left(s.position), options))
              }
            case _ => ()
          }
        }
      case f@Failure(a, b, d) =>
        ErrorReporting.error(s"Failed to parse the module `$moduleName` in $filename", Some(parser.indexToPosition(f.index, parser.lastLabel)))
//        ErrorReporting.error(a.toString)
//        ErrorReporting.error(b.toString)
//        ErrorReporting.error(d.toString)
//        ErrorReporting.error(d.traced.expected)
//        ErrorReporting.error(d.traced.stack.toString)
//        ErrorReporting.error(d.traced.traceParsers.toString)
//        ErrorReporting.error(d.traced.fullStack.toString)
//        ErrorReporting.error(f.toString)
        if (parser.lastLabel != "") {
          ErrorReporting.error(s"Syntax error: ${parser.lastLabel} expected", Some(parser.lastPosition))
        } else {
          ErrorReporting.error("Syntax error", Some(parser.lastPosition))
        }
    }
  }

  def extractName(i: String): String = {
    val noExt = i.stripSuffix(extension)
    val lastSlash = noExt.lastIndexOf('/') max noExt.lastIndexOf('\\')
    if (lastSlash >= 0) i.substring(lastSlash + 1) else i
  }
}
