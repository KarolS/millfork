package millfork.cli

import net.liftweb.json._
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.charset.StandardCharsets
import scala.collection.mutable
import scala.collection.convert.ImplicitConversionsToScala._
import java.io.InputStreamReader
import millfork.Context
import millfork.error.ConsoleLogger

case class JsonConfig(
    include: Option[List[String]],
    platform: Option[String],
    inputFiles: Option[List[String]]
)

object JsonConfigParser {
  implicit val formats = DefaultFormats

  def parseConfig(context: Context, logger: ConsoleLogger): Context = {
    var newContext = context

    var defaultConfig = false
    val filePath = context.configFilePath.getOrElse({
      defaultConfig = true
      ".millforkrc.json"
    })

    val path = Paths.get(filePath)

    try {
      val jsonString =
        Files
          .readAllLines(path, StandardCharsets.UTF_8)
          .toIndexedSeq
          .mkString("")

      val result = parse(jsonString).extract[JsonConfig]

      if (context.inputFileNames.length < 1 && result.inputFiles.isDefined) {
        newContext = newContext.copy(inputFileNames = result.inputFiles.get)
      }

      if (context.includePath.length < 1 && result.include.isDefined) {
        newContext =
          newContext.copy(extraIncludePath = result.include.get.toSeq)
      }

      if (context.platform.isEmpty && result.platform.isDefined) {
        newContext = newContext.copy(platform = Some(result.platform.get))
      }
    } catch {
      case default: Throwable => {
        if (!defaultConfig) {
          // Only throw error if not default config
          logger.fatalQuit("Invalid config file")
        }
      }
    }

    newContext
  }
}
