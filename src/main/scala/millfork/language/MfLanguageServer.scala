package millfork.language
import millfork.CompilationOptions
import millfork.parser.MosSourceLoadingQueue
import millfork.Context
import millfork.parser.ParsedProgram

import millfork.node.{
  FunctionDeclarationStatement,
  ParameterDeclaration,
  Position,
  Node
}

import org.eclipse.lsp4j.services.{
  LanguageServer,
  TextDocumentService,
  WorkspaceService
}
import org.eclipse.lsp4j.{
  InitializeParams,
  InitializeResult,
  ServerCapabilities,
  Range
}
import org.eclipse.lsp4j.jsonrpc.services.JsonRequest

import java.util.concurrent.CompletableFuture
import org.eclipse.lsp4j.TextDocumentPositionParams
import org.eclipse.lsp4j.Hover
import org.eclipse.lsp4j.jsonrpc.messages.Either
import java.{util => ju}
import scala.collection.mutable
import org.eclipse.lsp4j.MarkedString
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification
import org.eclipse.lsp4j.InitializedParams
import org.eclipse.lsp4j.MarkupContent
import org.eclipse.lsp4j.DefinitionParams
import org.eclipse.lsp4j.Location
import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}
import org.eclipse.lsp4j.MessageParams
import org.eclipse.lsp4j.MessageType
import org.eclipse.lsp4j.DidOpenTextDocumentParams
import org.eclipse.lsp4j.TextDocumentSyncKind
import org.eclipse.lsp4j.DidChangeTextDocumentParams
import millfork.parser.AbstractSourceLoadingQueue
import java.nio.file.Path
import java.nio.file.Paths
import org.eclipse.lsp4j.VersionedTextDocumentIdentifier
import millfork.node.Program
import millfork.node.ImportStatement
import org.eclipse.lsp4j.ReferenceParams
import millfork.node.DeclarationStatement
import scala.collection.JavaConverters._

class MfLanguageServer(context: Context, options: CompilationOptions) {
  var client: Option[MfLanguageClient] = None

  val cachedModules: mutable.Map[String, Program] = mutable.Map()
  private var cachedProgram: Option[ParsedProgram] = None
  private val moduleNames: mutable.Map[String, String] = mutable.Map()
  private val modulePaths: mutable.Map[String, Path] = mutable.Map()

  @JsonRequest("initialize")
  def initialize(
      params: InitializeParams
  ): CompletableFuture[
    InitializeResult
  ] =
    CompletableFuture.completedFuture {
      val capabilities = new ServerCapabilities()
      capabilities.setHoverProvider(true)
      capabilities.setDefinitionProvider(true)
      capabilities.setTextDocumentSync(TextDocumentSyncKind.Full)
      capabilities.setReferencesProvider(true)

      new InitializeResult(capabilities)
    }

  @JsonNotification("initialized")
  def initialized(params: InitializedParams): CompletableFuture[Unit] =
    CompletableFuture.completedFuture {
      populateProgramForPath()
    }

  // @JsonRequest("getTextDocumentService")
  // def getTextDocumentService(): CompletableFuture[TextDocumentService] = {
  //   val completableFuture = new CompletableFuture[InitializeResult]()
  //   completableFuture.complete(new TextDocumentService())
  //   completableFuture
  // }

  // @JsonRequest("getWorkspaceService")
  // def getWorkspaceService(): CompletableFuture[WorkspaceService] = ???

  @JsonRequest("exit")
  def exit(): CompletableFuture[Unit] = ???

  @JsonRequest("shutdown")
  def shutdown(): CompletableFuture[Object] = ???

  @JsonRequest("textDocument/didOpen")
  def textDocumentDidOpen(
      params: DidOpenTextDocumentParams
  ): CompletableFuture[Unit] =
    CompletableFuture.completedFuture {
      val textDocument = params.getTextDocument()
      val pathString = trimDocumentUri(textDocument.getUri())

      val documentText = textDocument.getText().split("\n").toSeq

      rebuildASTForFile(pathString, documentText)
    }

  @JsonRequest("textDocument/didChange")
  def textDocumentDidChange(
      params: DidChangeTextDocumentParams
  ): CompletableFuture[Unit] =
    CompletableFuture.completedFuture {
      val pathString = trimDocumentUri(params.getTextDocument().getUri())

      val documentText =
        params.getContentChanges().get(0).getText().split("\n").toSeq

      rebuildASTForFile(pathString, documentText)
    }

  def rebuildASTForFile(pathString: String, text: Seq[String]) = {
    logEvent(TelemetryEvent("Rebuilding AST for module at path", pathString))

    val queue = new MosSourceLoadingQueue(
      initialFilenames = context.inputFileNames,
      includePath = context.includePath,
      options = options
    )

    val path = Paths.get(pathString)

    logEvent(TelemetryEvent("Path", path.toString()))

    val moduleName = queue.extractName(pathString)
    val newProgram = queue.parseModuleWithLines(
      moduleName,
      path,
      text,
      context.includePath,
      Left(None),
      Nil
    )

    if (newProgram.isDefined) {
      cachedModules.put(moduleName, newProgram.get)

      moduleNames.put(pathString, moduleName)

      logEvent(
        TelemetryEvent(
          "Finished rebuilding AST for module at path",
          pathString
        )
      )
    } else {
      logEvent(
        TelemetryEvent("Failed to rebuild AST for module at path", pathString)
      )
    }
  }

  @JsonRequest("textDocument/definition")
  def textDocumentDefinition(
      params: DefinitionParams
  ): CompletableFuture[Location] =
    CompletableFuture.completedFuture {
      val activePosition = params.getPosition()

      val statement = findExpressionAtPosition(
        trimDocumentUri(params.getTextDocument().getUri()),
        Position(
          "",
          activePosition.getLine() + 1,
          activePosition.getCharacter() + 2,
          0
        )
      )

      if (statement.isDefined) {
        val (module, declaration) = statement.get
        locationForExpression(declaration, module)
      } else null
    }

  @JsonRequest("textDocument/references")
  def textDocumentReferences(
      params: ReferenceParams
  ): CompletableFuture[ju.List[Location]] =
    CompletableFuture.completedFuture {
      val activePosition = params.getPosition()

      val statement = findExpressionAtPosition(
        trimDocumentUri(params.getTextDocument().getUri()),
        Position(
          "",
          activePosition.getLine() + 1,
          activePosition.getCharacter() + 2,
          0
        )
      )

      if (statement.isDefined) {
        val (declarationModule, declarationContent) = statement.get

        logEvent(
          TelemetryEvent("Attempting to find references")
        )

        if (
          declarationContent
            .isInstanceOf[DeclarationStatement] || declarationContent
            .isInstanceOf[ParameterDeclaration]
        ) {
          val matchingExpressions =
            // Only include declaration if params specify it
            (if (params.getContext().isIncludeDeclaration())
               List((declarationModule, declarationContent))
             else List()) ++ NodeFinder
              .matchingExpressionsForDeclaration(
                cachedModules.toStream,
                declarationContent
              )

          logEvent(
            TelemetryEvent("Prepping references", matchingExpressions)
          )

          matchingExpressions
            .sortBy {
              case (_, expression) =>
                expression.position match {
                  case Some(value) => value.line
                  case None        => 0
                }
            }
            .map {
              case (module, expression) => {
                try {
                  locationForExpression(expression, module)
                } catch {
                  case _: Throwable => null
                }
              }
            }
            .filter(e => e != null)
            .asJava
        } else {
          null
        }
      } else null
    }

  @JsonRequest("textDocument/hover")
  def textDocumentHover(
      params: TextDocumentPositionParams
  ): CompletableFuture[Hover] =
    CompletableFuture.completedFuture {
      val hoverPosition = params.getPosition()

      val statement = findExpressionAtPosition(
        trimDocumentUri(params.getTextDocument().getUri()),
        Position(
          "",
          // Millfork positions start at 1,2, rather than 0,0, so add to each coord
          hoverPosition.getLine() + 1,
          hoverPosition.getCharacter() + 2,
          0
        )
      )

      if (statement.isDefined) {
        val (_, declarationContent) = statement.get
        val formatting = NodeFormatter.symbol(declarationContent)

        if (formatting.isDefined)
          new Hover(
            new MarkupContent(
              "markdown",
              NodeFormatter.hover(
                formatting.get,
                ""
              )
            )
          )
        else null
      } else null
    }

  /**
    * Builds the AST for the entire program, based on the configured "inputFileNames"
    */
  private def populateProgramForPath() = {
    logEvent(
      TelemetryEvent("Building program AST")
    )

    val queue = new MosSourceLoadingQueue(
      initialFilenames = context.inputFileNames,
      includePath = context.includePath,
      options = options
    )

    var program = queue.run()

    logEvent(
      TelemetryEvent("Finished building AST")
    )

    cachedProgram = Some(program)
    program.parsedModules.foreach {
      case (moduleName, program) =>
        cachedModules.put(moduleName, program)
    }
    program.modulePaths.foreach {
      case (moduleName, path) => modulePaths.put(moduleName, path)
    }
  }

  private def moduleNameForPath(documentPath: String) =
    moduleNames.get(documentPath).getOrElse {
      throw new Exception("Cannot find module at " + documentPath)
    }

  private def findExpressionAtPosition(
      documentPath: String,
      position: Position
  ): Option[(String, Node)] = {
    val moduleName = moduleNameForPath(documentPath)

    val currentModuleDeclarations = cachedModules.get(moduleName)

    if (currentModuleDeclarations.isEmpty) {
      return None
    }

    val (node, enclosingDeclarations) = NodeFinder.findNodeAtPosition(
      currentModuleDeclarations.get,
      position
    )

    if (node.isDefined) {
      logEvent(TelemetryEvent("Found node at position", node))

      // Build ordered scopes to search through
      // First, our current enclosing scope, then the current module (which contains the current scope), then all other modules
      val orderedScopes = List(
        (moduleName, enclosingDeclarations.get),
        (moduleName, currentModuleDeclarations.get.declarations)
      ) ++ cachedModules.toList
        .filter {
          case (cachedModuleName, program) => cachedModuleName != moduleName
        }
        .map {
          case (cachedModuleName, program) =>
            (cachedModuleName, program.declarations)
        }

      val usage =
        NodeFinder.findDeclarationForUsage(orderedScopes, node.get)

      if (usage.isDefined) {
        logEvent(TelemetryEvent("Found original declaration", usage))
        usage
      } else Some((moduleName, node.get))
    } else {
      logEvent(TelemetryEvent("Cannot find node for position", position))
      None
    }
  }

  /**
    * Builds highlighted `Location` of a declaration or usage
    */
  private def locationForExpression(
      expression: Node,
      module: String
  ): Location = {
    val name = NodeFinder.extractNodeName(expression)
    val position = expression.position.get
    val modulePath = modulePaths.getOrElse(
      module, {
        logEvent(
          TelemetryEvent(
            "Could not find path for module",
            module
          )
        )
        null
      }
    )

    if (expression.isInstanceOf[ImportStatement]) {
      // ImportStatement declaration is the entire "file". Set position to 1,1
      val importPosition = Position(module, 1, 1, 0)
      return new Location(
        modulePath.toUri().toString(),
        new Range(
          mfPositionToLSP4j(importPosition),
          mfPositionToLSP4j(importPosition)
        )
      )
    }

    val endPosition = if (name.isDefined) {
      Position(
        module,
        position.line,
        position.column + name.get.length,
        0
      )
    } else position

    new Location(
      modulePath.toUri().toString(),
      new Range(
        mfPositionToLSP4j(position),
        mfPositionToLSP4j(endPosition)
      )
    )
  }

  private def mfPositionToLSP4j(
      position: Position
  ): org.eclipse.lsp4j.Position =
    new org.eclipse.lsp4j.Position(
      position.line - 1,
      // If subtracting 1 would be < 0, set to 0
      if (position.column < 1) 0 else position.column - 1
    )

  private def logEvent(event: TelemetryEvent): Unit = {
    val languageClient = client.getOrElse {
      // Language client not registered
      return
    }

    implicit val formats = Serialization.formats(NoTypeHints)
    val serializedEvent = write(event)

    languageClient.logMessage(
      new MessageParams(MessageType.Log, serializedEvent)
    )
  }

  private def trimDocumentUri(uri: String): String = uri.stripPrefix("file:")
}

case class TelemetryEvent(message: String, data: Any = None)
