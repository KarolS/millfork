package millfork.parser

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import fastparse.core.Parsed.{Failure, Success}
import millfork.{CompilationFlag, CompilationOptions}
import millfork.error.ConsoleLogger
import millfork.node.{ImportStatement, Position, Program}

import scala.collection.mutable
import scala.collection.convert.ImplicitConversionsToScala._

abstract class AbstractSourceLoadingQueue[T](val initialFilenames: List[String],
                                             val includePath: List[String],
                                             val options: CompilationOptions) {

  protected val parsedModules: mutable.Map[String, Program] = mutable.Map[String, Program]()
  protected val moduleQueue: mutable.Queue[() => Unit] = mutable.Queue[() => Unit]()
  val extension: String = ".mfk"

  def enqueueStandardModules(): Unit

  def run(): Program = {
    initialFilenames.foreach { i =>
      parseModule(extractName(i), includePath, Right(i))
    }
    options.platform.startingModules.foreach {m =>
      moduleQueue.enqueue(() => parseModule(m, includePath, Left(None)))
    }
    enqueueStandardModules()
    while (moduleQueue.nonEmpty) {
      if (options.flag(CompilationFlag.SingleThreaded)) {
        moduleQueue.dequeueAll(_ => true).foreach(_())
      } else {
        moduleQueue.dequeueAll(_ => true).par.foreach(_())
      }
    }
    options.log.assertNoErrors("Parse failed")
    parsedModules.values.reduce(_ + _).applyImportantAliases
  }

  def lookupModuleFile(includePath: List[String], moduleName: String, position: Option[Position]): String = {
    includePath.foreach { dir =>
      val file = Paths.get(dir, moduleName + extension).toFile
      options.log.debug("Checking " + file)
      if (file.exists()) {
        return file.getAbsolutePath
      }
    }
    options.log.fatal(s"Module `$moduleName` not found", position)
  }

  def supportedPragmas: Set[String]

  def createParser(filename: String, src: String, parentDir: String, featureConstants: Map[String, Long], pragmas: Set[String]) : MfParser[T]

  def parseModule(moduleName: String, includePath: List[String], why: Either[Option[Position], String]): Unit = {
    val filename: String = why.fold(p => lookupModuleFile(includePath, moduleName, p), s => s)
    options.log.debug(s"Parsing $filename")
    val path = Paths.get(filename)
    val parentDir = path.toFile.getAbsoluteFile.getParent
    val PreprocessingResult(src, featureConstants, pragmas) = Preprocessor(options, Files.readAllLines(path, StandardCharsets.UTF_8).toIndexedSeq)
    for (pragma <- pragmas) {
      if (!supportedPragmas(pragma._1)) {
        options.log.warn(s"Unsupported pragma: #pragma $pragma", Some(Position(moduleName, pragma._2, 1, 0)))
      }
    }
    val shortFileName = path.getFileName.toString
    val parser = createParser(shortFileName, src, parentDir, featureConstants, pragmas.keySet)
    options.log.addSource(shortFileName, src.lines.toIndexedSeq)
    parser.toAst match {
      case Success(prog, _) =>
        parsedModules.synchronized {
          parsedModules.put(moduleName, prog)
          prog.declarations.foreach {
            case s@ImportStatement(m) =>
              if (!parsedModules.contains(m)) {
                moduleQueue.enqueue(() => parseModule(m, parentDir :: includePath, Left(s.position)))
              }
            case _ => ()
          }
        }
      case f@Failure(a, b, d) =>
        options.log.error(s"Failed to parse the module `$moduleName` in $filename", Some(parser.indexToPosition(f.index, parser.lastLabel)))
        if (parser.lastLabel != "") {
          options.log.error(s"Syntax error: ${parser.lastLabel} expected", Some(parser.lastPosition))
        } else {
          options.log.error("Syntax error", Some(parser.lastPosition))
        }
    }
  }

  def extractName(i: String): String = {
    val noExt = i.stripSuffix(extension)
    val lastSlash = noExt.lastIndexOf('/') max noExt.lastIndexOf('\\')
    if (lastSlash >= 0) i.substring(lastSlash + 1) else i
  }
}
