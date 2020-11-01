package millfork.test.language

import org.scalatest.{AppendedClues, FunSuite, Matchers}
import millfork.test.language.util._
import org.eclipse.lsp4j.DidOpenTextDocumentParams
import org.eclipse.lsp4j.TextDocumentItem
import org.eclipse.lsp4j.HoverParams
import org.eclipse.lsp4j.TextDocumentIdentifier
import org.eclipse.lsp4j.Position

/**
  * @author Karol Stasiak
  */
class MfLanguageServerSuite extends FunSuite with Matchers with AppendedClues {
  test("hover should find node under cursor, and its root declaration") {
    val server = LanguageHelper.createServer

    LanguageHelper.openDocument(
      server,
      "file.mfk",
      """
      | byte test
      | array(byte) foo[4]
      | void main() {
      |  test = test + 1
      |  foo[1] = test
      | }
    """
    )

    {
      // Select `test` variable usage
      val hoverParams = new HoverParams(
        new TextDocumentIdentifier("file.mfk"),
        new Position(4, 3)
      )
      val response = server.textDocumentHover(hoverParams)
      val hover = response.get

      val contents = hover.getContents().getRight()
      contents should not equal (null)
      contents.getValue() should equal(LanguageHelper.formatHover("byte test"))
    }
    {
      // Select `main` function
      val hoverParams = new HoverParams(
        new TextDocumentIdentifier("file.mfk"),
        new Position(3, 3)
      )
      val response = server.textDocumentHover(hoverParams)
      val hover = response.get

      val contents = hover.getContents().getRight()
      contents should not equal (null)
      contents.getValue() should equal(
        LanguageHelper.formatHover("void main()")
      )
    }
    {
      // Select `foo` array usage
      val hoverParams = new HoverParams(
        new TextDocumentIdentifier("file.mfk"),
        new Position(5, 6)
      )
      val response = server.textDocumentHover(hoverParams)
      val hover = response.get

      val contents = hover.getContents().getRight()
      contents should not equal (null)
      contents.getValue() should equal(
        LanguageHelper.formatHover("array(byte) foo [4]")
      )
    }
  }

}
