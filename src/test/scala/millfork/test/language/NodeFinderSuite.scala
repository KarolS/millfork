package millfork.test.language

import org.scalatest.{AppendedClues, FunSpec, Matchers}
import millfork.test.language.util._
import org.eclipse.lsp4j.DidOpenTextDocumentParams
import org.eclipse.lsp4j.TextDocumentItem
import org.eclipse.lsp4j.HoverParams
import org.eclipse.lsp4j.TextDocumentIdentifier
import java.util.regex.Pattern
import millfork.language.NodeFinder
import millfork.node.Position
import millfork.node.FunctionDeclarationStatement
import millfork.node.ExpressionStatement
import millfork.node.FunctionCallExpression
import millfork.node.Assignment

class NodeFinderSuite extends FunSpec with Matchers with AppendedClues {
  describe("nodeAtPosition") {
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

    val server = LanguageHelper.createServer

    LanguageHelper
      .openDocument(
        server,
        "file.mfk",
        text
      )

    val program = server.cachedModules.get("file").get

    it("should find root variable declarations") {
      val startIndex = 7
      val length = 4

      for (column <- startIndex to startIndex + length) {
        NodeFinder
          .findNodeAtPosition(program, Position("", 2, column, 0))
          ._2
          .get(0) should equal(
          program.declarations(0)
        )
      }
    }

    it("should find root array declarations") {
      val startIndex = 13
      val length = 16

      for (column <- startIndex to startIndex + length) {
        NodeFinder
          .findNodeAtPosition(program, Position("", 3, column, 0))
          ._2
          .get(0) should equal(
          program.declarations(1)
        )
      }
    }

    it("should find function declarations") {
      val startIndex = 7
      val length = 4

      for (column <- startIndex to startIndex + length) {
        NodeFinder
          .findNodeAtPosition(program, Position("", 4, column, 0))
          ._2
          .get(0) should equal(
          program.declarations(2)
        )
      }
    }

    it("should find variable expression within function") {
      val startIndex = 4
      val length = 4

      for (column <- startIndex to startIndex + length) {
        NodeFinder
          .findNodeAtPosition(program, Position("", 5, column, 0))
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
      val startIndex = 4
      val length = 3

      for (column <- startIndex to startIndex + length) {
        NodeFinder
          .findNodeAtPosition(program, Position("", 6, column, 0))
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
      val startIndex = 13
      val length = 4

      for (column <- startIndex to startIndex + length) {
        NodeFinder
          .findNodeAtPosition(program, Position("", 6, column, 0))
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
      val startIndex = 4
      val length = 6

      for (column <- startIndex to startIndex + length) {
        NodeFinder
          .findNodeAtPosition(program, Position("", 7, column, 0))
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

    it("should find function nested variable declarations") {
      val startIndex = 9
      val length = 1

      for (column <- startIndex to startIndex + length) {
        NodeFinder
          .findNodeAtPosition(program, Position("", 10, column, 0))
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
