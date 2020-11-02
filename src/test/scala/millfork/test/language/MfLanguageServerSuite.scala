package millfork.test.language

import org.scalatest.{AppendedClues, FunSpec, Matchers}
import millfork.test.language.util._
import org.eclipse.lsp4j.DidOpenTextDocumentParams
import org.eclipse.lsp4j.TextDocumentItem
import org.eclipse.lsp4j.HoverParams
import org.eclipse.lsp4j.TextDocumentIdentifier
import org.eclipse.lsp4j.Position
import java.util.regex.Pattern
import scala.collection.mutable

class MfLanguageServerSuite extends FunSpec with Matchers with AppendedClues {
  describe("hover") {
    it("should find node under cursor, and its root declaration") {
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
        contents.getValue() should equal(
          LanguageHelper.formatHover("byte test")
        )
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

    describe("should always produce value") {
      val server = LanguageHelper.createServer

      val text = """
      | byte test
      | array(byte) foo[4]
      | void main() {
      |  test += test
      |  foo[1] = test
      |  func()
      | }
      | byte func() {
      |  byte i
      |  byte innerValue
      |  innerValue = 2
      |  innerValue += innerValue
      |  return innerValue
      | }
    """.stripMargin

      LanguageHelper.openDocument(
        server,
        "file.mfk",
        text
      )

      val lines = text.split("\n")

      val pattern = Pattern.compile("(return|byte)")

      for ((line, i) <- lines.zipWithIndex) {
        val matcher = pattern.matcher(line)

        val ignoreRanges = mutable.MutableList[Range]()

        while (matcher.find()) {
          ignoreRanges += Range(matcher.start(), matcher.end())
        }

        for ((character, column) <- line.toCharArray().zipWithIndex) {
          if (
            Character.isLetter(character) &&
            // Ignore sections of string matching pattern
            ignoreRanges.filter(r => r.contains(column)).length == 0
          ) {
            it(s"""should work on ${i}, ${column} contents "${line}" """) {
              val hoverParams = new HoverParams(
                new TextDocumentIdentifier("file.mfk"),
                new Position(i, column + 2)
              )
              val response = server.textDocumentHover(hoverParams)
              val hover = response.get

              hover should not equal (null)

              val contents = hover.getContents().getRight()
              info(contents.toString())
              contents should not equal (null)
            }
          }
        }
      }
    }

  }
}
