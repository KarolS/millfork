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
import millfork.node.ParameterDeclaration
import millfork.env.ByConstant
import millfork.env.ByReference
import millfork.env.ByVariable
import millfork.node.ArrayDeclarationStatement
import millfork.node.AliasDefinitionStatement

object NodeFinder {
  def findDeclarationForUsage(
      parsedModules: Stream[(String, Program)],
      node: Node
  ): Option[(String, Node)] = {
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
  ): Option[Node] =
    expression match {
      case FunctionCallExpression(name, expressions) =>
        declarations
          .filter(d => d.isInstanceOf[FunctionDeclarationStatement])
          .find(d => d.name == name)
      case VariableExpression(name) =>
        declarations
          .flatMap(flattenNestedDeclarations)
          .find(d =>
            d match {
              case variableDeclaration: VariableDeclarationStatement =>
                variableDeclaration.name == name
              case ParameterDeclaration(typ, assemblyParamPassingConvention) =>
                assemblyParamPassingConvention match {
                  case ByConstant(pName)  => pName == name
                  case ByReference(pName) => pName == name
                  case ByVariable(pName)  => pName == name
                  case default            => false
                }
              case arrayDeclaration: ArrayDeclarationStatement =>
                arrayDeclaration.name == name
              case AliasDefinitionStatement(aName, target, important) =>
                aName == name
              case default => false
            }
          )
      case default => None
    }

  def matchingExpressionsForDeclaration(
      parsedModules: Stream[(String, Program)],
      declaration: DeclarationStatement
  ): List[(String, Node)] = {
    parsedModules.toStream.flatMap {
      case (module, program) => {
        val allDeclarations =
          program.declarations
            .flatMap(d => d.getAllExpressions)
            .flatMap(flattenNestedExpressions)

        declaration match {
          case f: FunctionDeclarationStatement =>
            allDeclarations
              .filter(d => d.isInstanceOf[FunctionCallExpression])
              .map(d => d.asInstanceOf[FunctionCallExpression])
              .find(d => d.functionName == f.name)
              .map(d => (module, d))
          case v: VariableDeclarationStatement =>
            allDeclarations
              .filter(d => d.isInstanceOf[VariableExpression])
              .map(d => d.asInstanceOf[VariableExpression])
              .find(d => d.name == v.name)
              .map(d => (module, d))
          case default => List()
        }
      }
    }.toList
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

  private def flattenNestedDeclarations(
      declaration: DeclarationStatement
  ): List[Node] =
    declaration match {
      case functionDeclaration: FunctionDeclarationStatement => {
        List(
          functionDeclaration
          // Pull statements rather than getAllExpressions, as the variable declarations don't seem to be properly handled otherwise
        ) ++ functionDeclaration.params ++ functionDeclaration.statements
          .getOrElse(List())
          .filter(e =>
            e.isInstanceOf[DeclarationStatement] || e
              .isInstanceOf[ParameterDeclaration]
          )
      }
      case default => List(default)
    }

  /**
    * Returns all of the expressions contained within an expression, including itself
    */
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
