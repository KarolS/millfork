package millfork.language

import millfork.node.{
  DeclarationStatement,
  Expression,
  FunctionCallExpression,
  Node,
  Program,
  Position
}
import scala.collection.mutable

object NodeFinder {
  def findNodeAtPosition(program: Program, position: Position): Option[Node] = {
    val line = position.line
    val column = position.column

    val declarations =
      findDeclarationsAtLine(program.declarations, line)

    if (declarations.isEmpty) {
      return None
    }

    if (lineOrNegOne(declarations.get.head.position) == line) {
      // All declarations are current line, find matching node for column
      findNodeAtColumn(
        declarations.get.flatMap(d => List(d) ++ d.getAllExpressions),
        line,
        column
      )
    } else {
      // Declaration is a function or similar wrapper
      // Find inner expressions
      val matchingExpressions =
        declarations.get.head.getAllExpressions.filter(e =>
          lineOrNegOne(e.position) == line
        )

      findNodeAtColumn(matchingExpressions, line, column)
    }
  }

  private def findDeclarationsAtLine(
      declarations: List[DeclarationStatement],
      line: Int
  ): Option[List[DeclarationStatement]] = {
    var lastDeclarations: Option[List[DeclarationStatement]] = None

    for ((nextDeclaration, i) <- declarations.view.zipWithIndex) {
      if (lastDeclarations.isEmpty) {
        // Populate with first item, no matter what
        lastDeclarations = Option(List(nextDeclaration))
      } else {
        val nextLine = lineOrNegOne(nextDeclaration.position)

        if (nextLine == line) {
          // Declaration is on this line
          // Check for additional declarations on this line
          val newDeclarations = mutable.MutableList(nextDeclaration)

          for (declarationIndex <- i to declarations.length - 1) {
            val checkDeclaration = declarations(declarationIndex)

            if (checkDeclaration.position.isDefined) {
              if (checkDeclaration.position.get.line == line) {
                newDeclarations += checkDeclaration
              } else {
                // Line doesn't match, done with this line
                return Option(newDeclarations.toList)
              }
            }
          }

          return Option(newDeclarations.toList)
        } else if (nextLine < line) {
          // Closer to desired line
          lastDeclarations = Option(List(nextDeclaration))
        }
      }
    }

    lastDeclarations
  }

  private def findNodeAtColumn(
      nodes: List[Node],
      line: Int,
      column: Int
  ): Option[Node] = {
    var lastNode: Option[Node] = None

    // Only consider nodes on this line (if we're opening a declaration, it could span multiple lines)
    for (nextNode <- nodes) if (lineOrNegOne(nextNode.position) == line) {
      val innerExpressions = extractNestedExpressions(nextNode)

      if (innerExpressions.isDefined) {
        for (innerExpression <- innerExpressions.get) {
          if (colOrNegOne(innerExpression.position) < column) {
            lastNode = Option(innerExpression)
          }
        }
      } else if (colOrNegOne(nextNode.position) < column) {
        lastNode = Option(nextNode)
      }
    }

    lastNode
  }

  private def lineOrNegOne(position: Option[Position]): Int =
    position match {
      case Some(pos) => pos.line
      case None      => -1
    }

  private def colOrNegOne(position: Option[Position]): Int =
    position match {
      case Some(pos) => pos.column
      case None      => -1
    }

  private def extractNestedExpressions(
      node: Node
  ): Option[List[Expression]] = {
    node match {
      case FunctionCallExpression(_, expressions) => Option(expressions)
      case default                                => None
    }
  }
}
