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
  ServerCapabilities
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

  @JsonRequest("textDocument/hover")
  def textDocumentHover(
      params: TextDocumentPositionParams
  ): CompletableFuture[Hover] =
    CompletableFuture.completedFuture {
      val hoverPosition = params.getPosition()

      val statement = findExpressionAtPosition(
        params.getTextDocument().getUri().stripPrefix("file:"),
        new Position(
          "",
          params.getPosition().getLine() + 1,
          params.getPosition().getCharacter(),
          0
        )
      )

      if (statement.isDefined) {
        val declarationContent = statement.get
        var formattedHover = declarationContent.toString()

        if (declarationContent.isInstanceOf[FunctionDeclarationStatement]) {
          val funcDeclaration: FunctionDeclarationStatement =
            declarationContent.asInstanceOf[FunctionDeclarationStatement]
          formattedHover += " Params: " + funcDeclaration.params
            .map(p => p.typ)
            .mkString
        }

        new Hover(
          new MarkupContent("plaintext", formattedHover)
        )
      } else new Hover(new MarkupContent("plaintext", "No statement found"))
    }

  private def findExpressionAtPosition(
      documentPath: String,
      position: Position
  ): Option[Node] = {
    val unoptimizedProgram = new MosSourceLoadingQueue(
      initialFilenames = List(documentPath),
      includePath = context.includePath,
      options = options
    ).run()

    NodeFinder.findNodeAtPosition(unoptimizedProgram, position)
  }
}
