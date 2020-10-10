package millfork.language
import millfork.CompilationOptions
import millfork.parser.MosSourceLoadingQueue
import millfork.Context

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
import org.eclipse.lsp4j.MarkedString
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification
import org.eclipse.lsp4j.InitializedParams
import org.eclipse.lsp4j.MarkupContent
import org.eclipse.lsp4j.DefinitionParams
import org.eclipse.lsp4j.Location

class MfLanguageServer(context: Context, options: CompilationOptions) {
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

      new InitializeResult(capabilities)
    }

  @JsonNotification("initialized")
  def initialized(params: InitializedParams): CompletableFuture[Unit] = {
    val completableFuture = new CompletableFuture[Unit]()

    completableFuture.complete()
    completableFuture
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

  @JsonRequest("textDocument/definition")
  def textDocumentDefinition(
      params: DefinitionParams
  ): CompletableFuture[Location] =
    CompletableFuture.completedFuture {
      val activePosition = params.getPosition()

      val statement = findExpressionAtPosition(
        params.getTextDocument().getUri().stripPrefix("file:"),
        Position(
          "",
          activePosition.getLine() + 1,
          activePosition.getCharacter() + 2,
          0
        )
      )

      if (statement.isDefined) {
        val declarationContent = statement.get
        val formatting = NodeFormatter.symbol(declarationContent)

        if (declarationContent.position.isDefined)
          new Location(
            params.getTextDocument().getUri(),
            new Range(
              mfPositionToLSP4j(declarationContent.position.get),
              mfPositionToLSP4j(declarationContent.position.get)
            )
          )
        else null
      } else null
    }

  @JsonRequest("textDocument/hover")
  def textDocumentHover(
      params: TextDocumentPositionParams
  ): CompletableFuture[Hover] =
    CompletableFuture.completedFuture {
      val hoverPosition = params.getPosition()

      val statement = findExpressionAtPosition(
        params.getTextDocument().getUri().stripPrefix("file:"),
        Position(
          "",
          // Millfork positions start at 1,2, rather than 0,0, so add to each coord
          hoverPosition.getLine() + 1,
          hoverPosition.getCharacter() + 2,
          0
        )
      )

      if (statement.isDefined) {
        val declarationContent = statement.get
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

  private def findExpressionAtPosition(
      documentPath: String,
      position: Position
  ): Option[Node] = {
    val queue = new MosSourceLoadingQueue(
      initialFilenames = List(documentPath),
      includePath = context.includePath,
      options = options
    )
    val unoptimizedProgram = queue.run()

    val node = NodeFinder.findNodeAtPosition(
      queue.extractName(documentPath),
      unoptimizedProgram,
      position
    )

    if (node.isDefined) {
      val usage =
        NodeFinder.findDeclarationForUsage(unoptimizedProgram, node.get)

      if (usage.isDefined) usage else node
    } else None
  }

  private def mfPositionToLSP4j(
      position: Position
  ): org.eclipse.lsp4j.Position =
    new org.eclipse.lsp4j.Position(position.line - 1, position.column - 1)
}
