package millfork.parser

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import fastparse.core.Parsed.{Failure, Success}
import millfork.{CompilationFlag, CompilationOptions, Tarjan}
import millfork.node.{AliasDefinitionStatement, DeclarationStatement, ImportStatement, Position, Program}

import scala.collection.mutable
import scala.collection.convert.ImplicitConversionsToScala._

case class ParsedProgram(compilationOrderProgram: Program, parsedModules: Map[String, Program], modulePaths: Map[String, Path])

abstract class AbstractSourceLoadingQueue[T](val initialFilenames: List[String],
                                             val includePath: List[String],
                                             val options: CompilationOptions) {

  protected val parsedModules: mutable.Map[String, Program] = mutable.Map[String, Program]()
  protected val modulePaths: mutable.Map[String, Path] = mutable.Map[String, Path]()
  protected val moduleDependecies: mutable.Set[(String, String)] = mutable.Set[(String, String)]()
  protected val moduleQueue: mutable.Queue[() => Unit] = mutable.Queue[() => Unit]()
  val extension: String = ".mfk"

  def standardModules: IndexedSeq[String]

  def enqueueStandardModules(): Unit

  def pseudoModules: List[DeclarationStatement] = {
    val encodingConversionAliases = (options.platform.defaultCodec.name, options.platform.screenCodec.name) match {
        // TODO: don't rely on names!
      case ("PETSCII", "CBM-Screen") |
           ("PETSCII-JP", "CBM-Screen-JP") =>
        List(AliasDefinitionStatement("__from_screencode", "petscr_to_petscii", important = false),
          AliasDefinitionStatement("__to_screencode", "petscii_to_petscr", important = false))
      case ("ATASCII", "ATASCII-Screen") =>
        List(AliasDefinitionStatement("__from_screencode", "atasciiscr_to_atascii", important = false),
          AliasDefinitionStatement("__to_screencode", "atascii_to_atasciiscr", important = false))
      case ("Color-Computer", "Color-Computer-Screen") =>
        List(AliasDefinitionStatement("__from_screencode", "cocoscr_to_coco", important = false),
          AliasDefinitionStatement("__to_screencode", "coco_to_cocoscr", important = false))
      case _ => Nil
    }
    encodingConversionAliases
  }

  /**
    * Tokenizes and parses the configured source file and modules
    *
    * @return A ParsedProgram containing an ordered set of statements in order of compilation dependencies, and each individual parsed module
    */
  def run(): ParsedProgram = {
    for {
      initialFilename <- initialFilenames
      startingModule <- options.platform.startingModules
    } {
      val initialModule = extractName(initialFilename)
      moduleDependecies += initialModule -> startingModule
      for (standardModule <- standardModules) {
        moduleDependecies += initialModule -> standardModule
        moduleDependecies += startingModule -> standardModule
      }
    }
    for {
      earlier <- standardModules.indices
      later <- (earlier + 1) until standardModules.length
    } {
      moduleDependecies += standardModules(later) -> standardModules(earlier)
    }
    initialFilenames.foreach { i =>
      parseModule(extractName(i), includePath, Right(i), Nil)
    }
    options.platform.startingModules.foreach {m =>
      moduleQueue.enqueue(() => parseModule(m, includePath, Left(None), Nil))
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
    val compilationOrder = Tarjan.sort(parsedModules.keys, moduleDependecies)
    options.log.debug("Compilation order: " + compilationOrder.mkString(", "))

    ParsedProgram(compilationOrder.filter(parsedModules.contains).map(parsedModules).reduce(_ + _).applyImportantAliases, parsedModules.toMap, modulePaths.toMap)
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

  def fullModuleName(moduleNameBase: String, templateParams: List[String]): String = {
    if (templateParams.isEmpty) moduleNameBase else moduleNameBase + templateParams.mkString("<", ",", ">")
  }

  def parseModule(moduleName: String, includePath: List[String], why: Either[Option[Position], String], templateParams: List[String]): Unit = {
    val filename: String = why.fold(p => lookupModuleFile(includePath, moduleName, p), s => s)
    options.log.debug(s"Parsing $filename")
    val path = Paths.get(filename)
    modulePaths.put(moduleName, path)
    val parentDir = path.toFile.getAbsoluteFile.getParent
    val shortFileName = path.getFileName.toString
    val PreprocessingResult(src, featureConstants, pragmas) = Preprocessor(options, shortFileName, Files.readAllLines(path, StandardCharsets.UTF_8).toIndexedSeq, templateParams)
    for (pragma <- pragmas) {
      if (!supportedPragmas(pragma._1) && options.flag(CompilationFlag.BuggyCodeWarning)) {
        options.log.warn(s"Unsupported pragma: #pragma ${pragma._1}", Some(Position(moduleName, pragma._2, 1, 0)))
      }
    }
    val parser = createParser(shortFileName, src, parentDir, featureConstants, pragmas.keySet)
    options.log.addSource(shortFileName, src.linesIterator.toIndexedSeq)
    parsedModules.put("pseudomodule\u0000", Program(pseudoModules))
    parser.toAst match {
      case Success(prog, _) =>
        parsedModules.synchronized {
          parsedModules.put(fullModuleName(moduleName, templateParams), prog)
          prog.declarations.foreach {
            case s@ImportStatement(m, ps) =>
              moduleDependecies += moduleName -> m
              if (!parsedModules.contains(fullModuleName(m, ps))) {
                moduleQueue.enqueue(() => parseModule(m, parentDir :: includePath, Left(s.position), ps))
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
    if (lastSlash >= 0) noExt.substring(lastSlash + 1) else noExt
  }
}
