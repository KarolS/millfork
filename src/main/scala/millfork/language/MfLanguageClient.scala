package millfork.language

import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification
import org.eclipse.lsp4j.MessageType
import org.eclipse.lsp4j.MessageParams

trait MfLanguageClient extends LanguageClient {

  /**
    * Display message in the editor "status bar", which should be displayed somewhere alongside the buffer.
    *
    * The status bar should always be visible to the user.
    *
    * - VS Code: https://code.visualstudio.com/docs/extensionAPI/vscode-api#StatusBarItem
    */
  // @JsonNotification("metals/status")
  // def metalsStatus(params: MetalsStatusParams): Unit

  /**
    * Starts a long running task with no estimate for how long it will take to complete.
    *
    * - request cancellation from the server indicates that the task has completed
    * - response with cancel=true indicates the client wishes to cancel the slow task
    */
  // @JsonRequest("metals/slowTask")
  // def metalsSlowTask(
  //     params: MetalsSlowTaskParams
  // ): CompletableFuture[MetalsSlowTaskResult]

  // @JsonNotification("metals/executeClientCommand")
  // def metalsExecuteClientCommand(params: ExecuteCommandParams): Unit

  final def refreshModel(): Unit = {
    // val command = ClientCommands.RefreshModel.id
    // val params = new ExecuteCommandParams(command, Nil.asJava)
    // metalsExecuteClientCommand(params)
  }

  /**
    * Opens an input box to ask the user for input.
    *
    * @return the user provided input. The future can be cancelled, meaning
    *         the input box should be dismissed in the editor.
    */
  // @JsonRequest("metals/inputBox")
  // def metalsInputBox(
  //     params: MetalsInputBoxParams
  // ): CompletableFuture[MetalsInputBoxResult]

  /**
    * Opens an menu to ask the user to pick one of the suggested options.
    *
    * @return the user provided pick. The future can be cancelled, meaning
    *         the input box should be dismissed in the editor.
    */
  // @JsonRequest("metals/quickPick")
  // def metalsQuickPick(
  //     params: MetalsQuickPickParams
  // ): CompletableFuture[MetalsQuickPickResult]

  final def showMessage(messageType: MessageType, message: String): Unit = {
    val params = new MessageParams(messageType, message)
    showMessage(params)
  }

  def shutdown(): Unit = {}

}
