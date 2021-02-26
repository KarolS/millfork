package millfork.test.language.util

import millfork.error.Logger
import millfork.{NullLogger, Platform}
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

object LanguageHelper {
  def createServer(): MfLanguageServer = {
    implicit val logger: Logger = new NullLogger()
    val platform = EmuPlatform.get(Cpu.Mos)
    val jobContext = JobContext(TestErrorReporting.log, new LabelGenerator)
    new MfLanguageServer(
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
  }

  def openDocument(server: MfLanguageServer, name: String, text: String) = {
    val textDocument =
      new TextDocumentItem(name, "millfork", 1, text.stripMargin)
    val openParams = new DidOpenTextDocumentParams(textDocument)
    server.textDocumentDidOpen(openParams)
  }

  def formatHover(text: String): String = s"""```mfk\n${text}\n```"""
}
