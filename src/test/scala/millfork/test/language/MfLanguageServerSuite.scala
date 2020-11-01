package millfork.test.language

import millfork.error.Logger
import millfork.{NullLogger, Platform}
import org.scalatest.{AppendedClues, FunSuite, Matchers}
import millfork.language.MfLanguageServer
import millfork.Context
import millfork.CompilationOptions
import millfork.test.emu.{EmuPlatform, TestErrorReporting}
import millfork.Cpu
import millfork.JobContext
import millfork.compiler.LabelGenerator
import org.eclipse.lsp4j.DidOpenTextDocumentParams
import org.eclipse.lsp4j.TextDocumentItem
import org.eclipse.lsp4j.HoverParams
import org.eclipse.lsp4j.TextDocumentIdentifier
import org.eclipse.lsp4j.Position
import millfork.language.MfLanguageClient

/**
  * @author Karol Stasiak
  */
class MfLanguageServerSuite extends FunSuite with Matchers with AppendedClues {
  test("hover should find node under cursor, and its root declaration") {
    implicit val logger: Logger = new NullLogger()
    val platform = EmuPlatform.get(Cpu.Mos)
    val jobContext = JobContext(TestErrorReporting.log, new LabelGenerator)
    val server = new MfLanguageServer(
      new Context(logger, List()),
      new CompilationOptions(
        platform,
        Map(),
        None,
        0,
        Map(),
        EmuPlatform.textCodecRepository,
        jobContext
      )
    )

    val textDocument = new TextDocumentItem("file.mfk", "millfork", 1, """
      | byte test
      | void main() {
      |  test = test + 1
      | }
    """.stripMargin)
    val openParams = new DidOpenTextDocumentParams(textDocument)
    server.textDocumentDidOpen(openParams)

    val hoverParams = new HoverParams(
      new TextDocumentIdentifier("file.mfk"),
      new Position(3, 3)
    )
    val response = server.textDocumentHover(hoverParams)

    val hover = response.get

    val contents = hover.getContents().getRight()
    contents should not equal (null)
    contents.getValue() should equal("```mfk\nbyte test\n```")
  }

}
