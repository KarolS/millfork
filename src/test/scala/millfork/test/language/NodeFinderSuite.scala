package millfork.test.language

import org.scalatest.{AppendedClues, FunSpec, Matchers}
import millfork.test.language.util._
import org.eclipse.lsp4j.DidOpenTextDocumentParams
import org.eclipse.lsp4j.TextDocumentItem
import org.eclipse.lsp4j.HoverParams
import org.eclipse.lsp4j.TextDocumentIdentifier
import millfork.language.NodeFinder
import millfork.node.Position
import millfork.node.FunctionDeclarationStatement
import millfork.node.ExpressionStatement
import millfork.node.FunctionCallExpression
import millfork.node.Assignment
import java.util.regex.Pattern

class NodeFinderSuite extends FunSpec with Matchers with AppendedClues {
  describe("nodeAtPosition") {
    val text = """
             | 
             | byte test
             | array(byte) foo[4]
             | void main() {
             |  test += test
             |  foo[1] = test
             |  func()
             | }
             | byte func(byte arg) {
             |  byte i
             |  byte innerValue
             |  innerValue = 2
             |  innerValue += innerValue
             |  innerValue += arg
             |  return innerValue
             | }
    """.stripMargin

    val server = LanguageHelper.createServer

    LanguageHelper
      .openDocument(
        server,
        "file.mfk",
        text
      )

    val program = server.cachedModules.get("file").get

    def findRangeOfString(
        textMatch: String,
        afterLine: Int = 0
    ): (Int, Range) = {
      val pattern = Pattern.compile(s"(${Pattern.quote(textMatch)})")

      val lines = text.split("\n")
      for ((line, i) <- lines.zipWithIndex) {
        if (i >= afterLine) {
          val matcher = pattern.matcher(line)

          if (matcher.find()) {
            return (i + 1, Range(matcher.start() + 2, matcher.end() + 2))
          }
        }
      }

      throw new Error(s"Cound not find pattern ${textMatch}")
    }

    it("should find root variable declarations") {
      val (line, range) = findRangeOfString("test")

      for (column <- range) {
        NodeFinder
          .findNodeAtPosition(program, Position("", line, column, 0))
          ._2
          .get(0) should equal(
          program.declarations(0)
        )
      }
    }

    it("should find root array declarations") {
      val (line, range) = findRangeOfString("foo[4]")

      for (column <- range) {
        NodeFinder
          .findNodeAtPosition(program, Position("", line, column, 0))
          ._2
          .get(0) should equal(
          program.declarations(1)
        )
      }
    }

    it("should find function declarations") {
      val (line, range) = findRangeOfString("main()")

      for (column <- range) {
        NodeFinder
          .findNodeAtPosition(program, Position("", line, column, 0))
          ._2
          .get(0) should equal(
          program.declarations(2)
        )
      }
    }

    it("should find variable expression within function") {
      val (line, range) = findRangeOfString("test", 4)

      for (column <- range) {
        NodeFinder
          .findNodeAtPosition(program, Position("", line, column, 0))
          ._1
          .get should equal(
          program
            .declarations(2)
            .asInstanceOf[FunctionDeclarationStatement]
            .statements
            .get(0)
            .asInstanceOf[ExpressionStatement]
            .expression
            .asInstanceOf[FunctionCallExpression]
            .expressions(0)
        )
      }
    }

    it("should find array expression within function") {
      val (line, range) = findRangeOfString("foo", 4)

      for (column <- range) {
        NodeFinder
          .findNodeAtPosition(program, Position("", line, column, 0))
          ._1
          .get should equal(
          program
            .declarations(2)
            .asInstanceOf[FunctionDeclarationStatement]
            .statements
            .get(1)
            .asInstanceOf[Assignment]
            .destination
        )
      }
    }

    it("should find right hand side of assignment") {
      val (line, range) = findRangeOfString("test", 5)

      for (column <- range) {
        NodeFinder
          .findNodeAtPosition(program, Position("", line, column, 0))
          ._1
          .get should equal(
          program
            .declarations(2)
            .asInstanceOf[FunctionDeclarationStatement]
            .statements
            .get(1)
            .asInstanceOf[Assignment]
            .source
        )
      }
    }

    it("should find function call") {
      val (line, range) = findRangeOfString("func()")

      for (column <- range) {
        NodeFinder
          .findNodeAtPosition(program, Position("", line, column, 0))
          ._1
          .get should equal(
          program
            .declarations(2)
            .asInstanceOf[FunctionDeclarationStatement]
            .statements
            .get(2)
            .asInstanceOf[ExpressionStatement]
            .expression
        )
      }
    }

    it("should find function argument") {
      val (line, range) = findRangeOfString("arg")

      for (column <- range) {
        NodeFinder
          .findNodeAtPosition(program, Position("", line, column, 0))
          ._1
          .get should equal(
          program
            .declarations(3)
            .asInstanceOf[FunctionDeclarationStatement]
            .params(0)
        )
      }
    }

    it("should find function nested variable declarations") {
      val (line, range) = findRangeOfString("i", 7)

      for (column <- range) {
        NodeFinder
          .findNodeAtPosition(program, Position("", line, column, 0))
          ._1
          .get should equal(
          program
            .declarations(3)
            .asInstanceOf[FunctionDeclarationStatement]
            .statements
            .get(0)
        )
      }
    }
  }
}
