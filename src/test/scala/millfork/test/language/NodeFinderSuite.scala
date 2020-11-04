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
import millfork.node.Program
import millfork.node.IndexedExpression
import millfork.node.SumExpression

class NodeFinderSuite extends FunSpec with Matchers with AppendedClues {
  def createProgram(text: String): Program = {
    val server = LanguageHelper.createServer

    LanguageHelper
      .openDocument(
        server,
        "file.mfk",
        text
      )

    server.cachedModules.get("file").get
  }

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

    val program = createProgram(text)

    def findRangeOfString(
        text: String,
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
      val (line, range) = findRangeOfString(text, "test")

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
      val (line, range) = findRangeOfString(text, "foo[4]")

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
      val (line, range) = findRangeOfString(text, "main()")

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
      val (line, range) = findRangeOfString(text, "test", 4)

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
      val (line, range) = findRangeOfString(text, "foo", 4)

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
      val (line, range) = findRangeOfString(text, "test", 5)

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
      val (line, range) = findRangeOfString(text, "func()")

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
      val (line, range) = findRangeOfString(text, "arg")

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
      val (line, range) = findRangeOfString(text, "i", 7)

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

    it("should find variable used to index array") {
      val innerText = """
        | byte root
        | array(byte) anArray[10]
        | void main() {
        |  byte index
        |  index = 4
        |  root = anArray[index]
        |  index = anArray[root+1]
        | }
      """.stripMargin

      val program = createProgram(innerText)

      {
        // Standard indexing
        val (line, range) = findRangeOfString(innerText, "index", 6)

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
              .asInstanceOf[Assignment]
              .source
              .asInstanceOf[IndexedExpression]
              .index
          )
        }
      }
      {
        // Indexing within sum expression
        val (line, range) = findRangeOfString(innerText, "root", 7)

        for (column <- range) {
          NodeFinder
            .findNodeAtPosition(program, Position("", line, column, 0))
            ._1
            .get should equal(
            program
              .declarations(2)
              .asInstanceOf[FunctionDeclarationStatement]
              .statements
              .get(3)
              .asInstanceOf[Assignment]
              .source
              .asInstanceOf[IndexedExpression]
              .index
              .asInstanceOf[SumExpression]
              .expressions(0)
              ._2
          )
        }
      }
    }

    it("should find variables within a sum") {
      val innerText = """
        | byte valA
        | byte valB
        | byte valC
        | byte output
        | void main() {
        |  output = valB + valA - 102 + valC
        | }
      """.stripMargin

      val program = createProgram(innerText)

      val sumExpression = program
        .declarations(4)
        .asInstanceOf[FunctionDeclarationStatement]
        .statements
        .get(0)
        .asInstanceOf[Assignment]
        .source
        .asInstanceOf[SumExpression]

      {
        val (line, range) = findRangeOfString(innerText, "valA", 5)

        for (column <- range) {
          NodeFinder
            .findNodeAtPosition(program, Position("", line, column, 0))
            ._1
            .get should equal(
            sumExpression.expressions(1)._2
          )
        }
      }
      {
        val (line, range) = findRangeOfString(innerText, "valB", 5)

        for (column <- range) {
          NodeFinder
            .findNodeAtPosition(program, Position("", line, column, 0))
            ._1
            .get should equal(
            sumExpression.expressions(0)._2
          )
        }
      }
      {
        val (line, range) = findRangeOfString(innerText, "valC", 5)

        for (column <- range) {
          NodeFinder
            .findNodeAtPosition(program, Position("", line, column, 0))
            ._1
            .get should equal(
            sumExpression.expressions(3)._2
          )
        }
      }
    }

    // TODO: Additional tests:
    // Fields on array indexing: spawn_info[index].hi
    // Struct type: Player player
    // Struct fields: player1.pos
    // Messed up hover positions (in `nes_reset_joy.mfk`, each variable assignment to 0)
    // Alias references
    // Pointers: obj_ptr->xvel
  }
}
