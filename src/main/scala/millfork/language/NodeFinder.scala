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
import millfork.node.IndexedExpression
import millfork.node.Statement
import millfork.node.SumExpression
import millfork.env.ByLazilyEvaluableExpressionVariable
import millfork.node.EnumDefinitionStatement
import millfork.node.LabelStatement
import millfork.node.StructDefinitionStatement
import millfork.node.TypeDefinitionStatement
import millfork.node.UnionDefinitionStatement

object NodeFinder {

  /**
    * Finds the declaration matching the provided node
    *
    * @param orderedScopes A list, ordered by decreasing scope (function, local module, global module),
    * of tuples containing the module name and all declarations contained wherein
    * @param node The node to find the source declaration for
    * @return A tuple containing the module name and "declaration" (could be `ParameterDeclaration`, hence
    * the `Node` type); `None` otherwise
    */
  def findDeclarationForUsage(
      orderedScopes: List[(String, List[DeclarationStatement])],
      node: Node
  ): Option[(String, Node)] = {
    node match {
      case importStatement: ImportStatement =>
        Some((importStatement.filename, importStatement))
      case expression: Expression => {
        for ((moduleName, scopedDeclarations) <- orderedScopes) {
          val declaration =
            matchingDeclarationForExpression(
              expression,
              scopedDeclarations
            )

          if (declaration.isDefined) {
            return Some((moduleName, declaration.get))
          }
        }

        return None
      }
      case default => None
    }
  }

  /**
    * Searches for the declaration matching the type and name of the provided expression
    *
    * @param expression The expression to find the root declaration for
    * @param declarations The declarations to check
    * @return The matching declaration if found; `None` otherwise
    */
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
        matchVariableExpressionName(name, declarations)
      case IndexedExpression(name, index) =>
        matchVariableExpressionName(name, declarations)
      case default => None
    }

  /**
    * Searches for the declaration matching a variable name
    *
    * @param name The name of the variable
    * @param declarations The declarations to check
    * @return The matching declaration if found; `None` otherwise
    */
  private def matchVariableExpressionName(
      name: String,
      declarations: List[DeclarationStatement]
  ) =
    declarations
      .flatMap(d =>
        d match {
          // Extract nested declarations (and `ParameterDeclaration`s, which do not extend `DeclarationStatement`)
          // from functions
          case functionDeclaration: FunctionDeclarationStatement =>
            recursivelyFlatten(functionDeclaration)
              .filter(e =>
                e.isInstanceOf[DeclarationStatement] || e
                  .isInstanceOf[ParameterDeclaration]
              )
          case default => List(default)
        }
      )
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

  /**
    * Finds all expressions referencing a declaration
    *
    * @param parsedModules All program modules
    * @param declaration The declaration to find all references for
    * @return A list of tuples, containing the module name and the corresponding expression
    */
  def matchingExpressionsForDeclaration(
      parsedModules: Stream[(String, Program)],
      declaration: Node
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
              .filter(d => d.functionName == f.name)
              .map(d => (module, d))
          case v: VariableDeclarationStatement =>
            allDeclarations
              .filter(d => d.isInstanceOf[VariableExpression])
              .map(d => d.asInstanceOf[VariableExpression])
              .filter(d => d.name == v.name)
              .map(d => (module, d))
          case a: ArrayDeclarationStatement =>
            allDeclarations
              .filter(d => d.isInstanceOf[IndexedExpression])
              .map(d => d.asInstanceOf[IndexedExpression])
              .filter(d => d.name == a.name)
              .map(d => (module, d))
          case p: ParameterDeclaration => {
            val pName = p.assemblyParamPassingConvention match {
              case ByConstant(name)  => Some(name)
              case ByReference(name) => Some(name)
              case ByVariable(name)  => Some(name)
              case ByLazilyEvaluableExpressionVariable(name) =>
                Some(name)
              case _ => None
            }

            if (pName.isDefined) {
              allDeclarations
                .filter(d => extractNodeName(d) == pName)
                .map(d => (module, d))
            } else List()
          }
          case default => List()
        }
      }
    }.toList
  }

  /**
    * Finds the node and enclosing declaration scope for a given position
    *
    * @param program The program containing the position
    * @param position The position of the node to find
    * @return A tuple containing the found node, and a list of enclosing declaration scopes
    */
  def findNodeAtPosition(
      program: Program,
      position: Position
  ): (Option[Node], Option[List[DeclarationStatement]]) = {
    val line = position.line
    val column = position.column

    val declarations =
      findEnclosingDeclarationsAtLine(program.declarations, line)

    if (declarations.isEmpty) {
      return (None, None)
    }

    if (lineOrNegOne(declarations.get.head.position) != line) {
      // Declaration is a function or similar wrapper
      // Find inner expressions
      if (declarations.get.length > 1) {
        throw new Exception("Unexpected number of declarations")
      }

      return (
        findNodeAtColumn(
          recursivelyFlatten(declarations.get.head),
          line,
          column
        ),
        declarations
      )
    }

    // All declarations are current line, find matching node for column
    (
      findNodeAtColumn(
        declarations.get
          .flatMap(recursivelyFlatten),
        line,
        column
      ),
      declarations
    )
  }

  /**
    * Finds the narrowest top level declaration scope for the given line (typically enclosing function)
    *
    * @param declarations All program declarations in the file
    * @param line The line to search for
    */
  private def findEnclosingDeclarationsAtLine(
      declarations: List[DeclarationStatement],
      line: Int
  ): Option[List[DeclarationStatement]] = {
    var lastDeclarations: Option[List[DeclarationStatement]] =
      if (declarations.length > 0) Some(declarations.take(1)) else None

    for ((nextDeclaration, i) <- declarations.view.zipWithIndex) {
      val nextLine = lineOrNegOne(nextDeclaration.position)

      if (nextLine == line) {
        // Declaration is on this line
        // Check for additional declarations on this line
        val newDeclarations = mutable.MutableList(nextDeclaration)

        for (declarationIndex <- i to declarations.length - 1) {
          val checkDeclaration = declarations(declarationIndex)

          if (
            checkDeclaration.position.isDefined && checkDeclaration.position.get.line == line
          ) {
            newDeclarations += checkDeclaration
          } else {
            // Line doesn't match, done with this line
            return Some(newDeclarations.toList)
          }
        }

        return Some(newDeclarations.toList)
      } else if (nextLine < line) {
        // Closer to desired line
        lastDeclarations = Some(List(nextDeclaration))
      }
    }

    lastDeclarations
  }

  /**
    * Searches for closest column index less than the selected column on a given line
    */
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

  /**
    * Recursively flattens a node tree into a tree containing the node itself and all of its "owned" nodes.
    * In particular, function declarations don't call `getAllExpressions`, instead opting to manually pull out
    * `params` and `statements` to allow for proper access
    *
    * @param node The root of the tree to process
    * @return A flatten list of all nodes
    */
  private def recursivelyFlatten(node: Node): List[Node] =
    node match {
      case functionDeclaration: FunctionDeclarationStatement =>
        List(
          functionDeclaration
        ) ++ functionDeclaration.params ++ functionDeclaration.statements
          .getOrElse(List())
          .flatMap(recursivelyFlatten)
      case statement: Statement =>
        List(statement) ++ statement.getAllExpressions.flatMap(
          recursivelyFlatten
        )
      case expression: Expression => List(expression)
      case default                => List(default)
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
      case indexExpression: IndexedExpression =>
        List(indexExpression) ++
          flattenNestedExpressions(indexExpression.index)
      case sumExpression: SumExpression =>
        List(sumExpression) ++ sumExpression.expressions.flatMap(e =>
          flattenNestedExpressions(e._2)
        )
      case default => List(default)
    }

  /**
    * Returns the name of the node, if it exists
    */
  def extractNodeName(node: Node): Option[String] =
    node match {
      case a: AliasDefinitionStatement     => Some(a.name)
      case a: ArrayDeclarationStatement    => Some(a.name)
      case e: EnumDefinitionStatement      => Some(e.name)
      case f: FunctionCallExpression       => Some(f.functionName)
      case f: FunctionDeclarationStatement => Some(f.name)
      case l: LabelStatement               => Some(l.name)
      case s: StructDefinitionStatement    => Some(s.name)
      case t: TypeDefinitionStatement      => Some(t.name)
      case u: UnionDefinitionStatement     => Some(u.name)
      case v: VariableDeclarationStatement => Some(v.name)
      case v: VariableExpression           => Some(v.name)
      case _                               => None
    }
}
