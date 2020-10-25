package millfork.language

import millfork.node.{
  DeclarationStatement,
  Expression,
  FunctionCallExpression,
  FunctionDeclarationStatement,
  Node,
  Program,
  Position,
  VariableDeclarationStatement,
  VariableExpression
}
import millfork.parser.ParsedProgram

import scala.collection.mutable
import millfork.node.ExpressionStatement
import millfork.node.ImportStatement

object NodeFinder {
  def findDeclarationForUsage(
      parsedModules: Stream[(String, Program)],
      node: Node
  ): Option[(String, DeclarationStatement)] = {
    node match {
      case importStatement: ImportStatement =>
        Some((importStatement.filename, importStatement))
      case expression: Expression => {
        val foundDeclaration = parsedModules.toStream
          .map {
            case (module, program) => {
              val declaration =
                matchingDeclarationForExpression(
                  expression,
                  program.declarations
                )
              if (declaration.isDefined) Some((module, declaration.get))
              else None
            }
          }
          .find(d => d.isDefined)

        if (foundDeclaration.isDefined) foundDeclaration.get else None
      }
      case default => None
    }
  }

  private def matchingDeclarationForExpression(
      expression: Expression,
      declarations: List[DeclarationStatement]
  ): Option[DeclarationStatement] =
    expression match {
      case FunctionCallExpression(name, expressions) =>
        declarations
          .filter(d => d.isInstanceOf[FunctionDeclarationStatement])
          .find(d => d.name == name)
      case VariableExpression(name) =>
        declarations
          .filter(d => d.isInstanceOf[VariableDeclarationStatement])
          .find(d => d.name == name)
      case default => None
    }

  def findNodeAtPosition(
      module: String,
      parsedModules: Map[String, Program],
      position: Position
  ): Option[Node] = {
    val line = position.line
    val column = position.column

    val activeProgram = parsedModules.get(module)

    if (activeProgram.isEmpty) {
      return None
    }

    val declarations =
      findDeclarationsAtLine(activeProgram.get.declarations, line)

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
      if (declarations.get.length > 1) {
        throw new Exception("Unexpected number of declarations")
      }

      findNodeAtColumn(declarations.get.head.getAllExpressions, line, column)
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
        lastDeclarations = Some(List(nextDeclaration))
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
                return Some(newDeclarations.toList)
              }
            }
          }

          return Some(newDeclarations.toList)
        } else if (nextLine < line) {
          // Closer to desired line
          lastDeclarations = Some(List(nextDeclaration))
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
    var lastPosition: Option[Position] = None

    // Only consider nodes on this line (if we're opening a declaration, it could span multiple lines)
    var flattenedNodes = nodes.flatMap(flattenNestedExpressions)

    for (nextNode <- flattenedNodes)
      if (lineOrNegOne(nextNode.position) == line) {
        if (nextNode.position.isEmpty) {
          throw new Error("Missing position for node " + nextNode.toString())
        }

        if (
          colOrNegOne(nextNode.position) < column && colOrNegOne(
            nextNode.position
            // Allow equality, because later nodes are of higher specificity
          ) >= colOrNegOne(lastPosition)
        ) {
          lastNode = Some(nextNode)
          lastPosition = nextNode.position
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

  private def flattenNestedExpressions(node: Node): List[Node] =
    node match {
      case statement: ExpressionStatement => {
        val innerExpressions = flattenNestedExpressions(
          statement.expression
        )

        List(statement.expression) ++ innerExpressions
      }
      case functionExpression: FunctionCallExpression =>
        List(functionExpression) ++
          functionExpression.expressions
            .flatMap(flattenNestedExpressions)
      case default => List(default)
    }

  private def sortNodes(nodes: List[Node]) =
    nodes.sortWith((a, b) => {
      if (a.position.isEmpty && b.position.isEmpty) {
        false
      } else if (a.position.isEmpty) {
        true
      } else if (b.position.isEmpty) {
        false
      } else {
        val aPos = a.position.get
        val bPos = b.position.get

        // aPos.line < bPos.line && aPos.column < bPos.column
        aPos.column < bPos.column
      }
    })
}
