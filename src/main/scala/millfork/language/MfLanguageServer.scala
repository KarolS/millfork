package millfork.language
import millfork.CompilationOptions
import millfork.parser.MosSourceLoadingQueue
import millfork.Context

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
import millfork.node.FunctionDeclarationStatement
import millfork.node.ParameterDeclaration

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

      // Console.printf("Initializing Millfork language server")

      new InitializeResult(capabilities)
    }
  // ] = {
  //   val completableFuture = new CompletableFuture[InitializeResult]()

  //   val capabilities = new ServerCapabilities()
  //   capabilities.setHoverProvider(true)

  //   Console.printf("Initializing Millfork language server")

  //   completableFuture.complete(new InitializeResult(capabilities))
  //   completableFuture
  // }

  @JsonNotification("initialized")
  def initialized(params: InitializedParams): CompletableFuture[Unit] = {
    val completableFuture = new CompletableFuture[Unit]()

    // Console.printf("Millfork language server Initialization Finished")

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

      val unoptimized = new MosSourceLoadingQueue(
        initialFilenames =
          List(params.getTextDocument().getUri().stripPrefix("file:")),
        includePath = context.includePath,
        options = options
      ).run()

      val declaration = unoptimized.declarations.find(s => {
        if (s.position.isDefined) {
          val position = s.position.get
          position.line - 1 == hoverPosition.getLine()
          // .getLine() && position.column == hoverPosition.getCharacter()
        } else false
      })

      // Console.printf("Hovering")
      if (declaration.isDefined) {
        val declarationContent = declaration.get
        var formattedHover = declaration.get.name

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
      } else null
    }
}
