package millfork.language

import millfork.node.Node
import millfork.node.DeclarationStatement
import millfork.node.FunctionDeclarationStatement
import millfork.node.ParameterDeclaration
import millfork.node.Expression
import millfork.node.VariableExpression
import millfork.node.VariableDeclarationStatement
import millfork.node.LiteralExpression
import millfork.env.ParamPassingConvention
import millfork.env.ByConstant
import millfork.env.ByVariable
import millfork.env.ByReference
import millfork.node.ImportStatement
import millfork.env.ByLazilyEvaluableExpressionVariable
import millfork.env.ByMosRegister
import millfork.node.MosRegister
import millfork.env.ByZRegister
import millfork.node.ZRegister
import millfork.env.ByM6809Register
import millfork.node.M6809Register
import millfork.node.ArrayDeclarationStatement
import millfork.output.MemoryAlignment
import millfork.output.NoAlignment
import millfork.output.DivisibleAlignment
import millfork.output.WithinPageAlignment

object NodeFormatter {
  // TODO: Remove Option
  def symbol(node: Node): Option[String] =
    node match {
      case statement: DeclarationStatement =>
        statement match {
          case functionStatement: FunctionDeclarationStatement => {
            val builder = new StringBuilder()

            if (functionStatement.constPure) {
              builder.append("const ")
            }

            if (functionStatement.interrupt) {
              builder.append("interrupt ")
            }

            if (functionStatement.kernalInterrupt) {
              builder.append("kernal_interrupt ")
            }

            if (functionStatement.assembly) {
              builder.append("asm ")
            }

            // Cannot have both "macro" and "inline"
            if (functionStatement.isMacro) {
              builder.append("macro ")
            } else if (
              functionStatement.inlinable.isDefined && functionStatement.inlinable.get
            ) {
              builder.append("inline ")
            }

            builder.append(
              s"""${functionStatement.resultType} ${functionStatement.name}(${functionStatement.params
                .map(symbol)
                .filter(n => n.isDefined)
                .map(n => n.get)
                .mkString(", ")})"""
            )

            Some(builder.toString())
          }
          case variableStatement: VariableDeclarationStatement => {
            val builder = new StringBuilder()

            if (variableStatement.constant) {
              builder.append("const ")
            }

            if (variableStatement.volatile) {
              builder.append("volatile ")
            }

            builder.append(
              s"""${variableStatement.typ} ${variableStatement.name}"""
            )

            if (variableStatement.initialValue.isDefined) {
              val formattedInitialValue = symbol(
                variableStatement.initialValue.get
              )

              if (formattedInitialValue.isDefined) {
                builder.append(s""" = ${formattedInitialValue.get}""")
              }
            }

            Some(builder.toString())
          }
          case importStatement: ImportStatement =>
            Some(s"""import ${importStatement.filename}""")
          case arrayStatement: ArrayDeclarationStatement => {
            val builder = new StringBuilder()

            if (arrayStatement.const) {
              builder.append("const ")
            }

            builder.append(
              s"""array(${arrayStatement.elementType}) ${arrayStatement.name}"""
            )

            if (arrayStatement.length.isDefined) {
              val formattedLength = symbol(arrayStatement.length.get)

              if (formattedLength.isDefined) {
                builder.append(s""" [${formattedLength.get}]""")
              }
            }

            if (arrayStatement.alignment.isDefined) {
              val formattedAlignment = symbol(
                arrayStatement.alignment.get
              )

              if (formattedAlignment.isDefined) {
                builder.append(s""" align(${formattedAlignment.get})""")
              }
            }

            if (arrayStatement.address.isDefined) {
              val formattedAddress = symbol(arrayStatement.address.get)

              if (formattedAddress.isDefined) {
                builder.append(s""" @ ${formattedAddress.get}""")
              }
            }

            if (arrayStatement.elements.isDefined) {
              val formattedInitialValue = arrayStatement.elements.get
                .getAllExpressions(false)
                .map(e => symbol(e))
                .filter(e => e.isDefined)
                .map(e => e.get)
                .mkString(", ")

              builder.append(s""" = [${formattedInitialValue}]""")
            }

            Some(builder.toString())
          }
          // TODO: Finish
          case default => None
        }
      case ParameterDeclaration(typ, assemblyParamPassingConvention) =>
        Some(s"""${typ} ${symbol(assemblyParamPassingConvention)}""")
      case expression: Expression =>
        expression match {
          case LiteralExpression(value, _) => Some(s"""${value}""")
          case VariableExpression(name)    => Some(s"""${name}""")
          case default                     => None
        }
      case default => None
    }

  def symbol(paramConvention: ParamPassingConvention): String =
    paramConvention match {
      case ByConstant(name)                          => name
      case ByVariable(name)                          => name
      case ByReference(name)                         => name
      case ByLazilyEvaluableExpressionVariable(name) => name
      case ByMosRegister(register) =>
        MosRegister.toString(register).getOrElse("")
      case ByZRegister(register) => ZRegister.toString(register).getOrElse("")
      case ByM6809Register(register) =>
        M6809Register.toString(register).getOrElse("")
    }

  def symbol(alignment: MemoryAlignment): Option[String] =
    alignment match {
      case NoAlignment => None
      // TOOD: Improve
      case DivisibleAlignment(divisor) => Some(s"""${divisor}""")
      case WithinPageAlignment         => Some("Within page")
    }

  /**
    * Render the textDocument/hover result into markdown.
    *
    * @param symbolSignature The signature of the symbol over the cursor, for example
    *                        "def map[B](fn: A => B): Option[B]"
    * @param docstring The Markdown documentation string for the symbol.
    */
  def hover(
      symbolSignature: String,
      docstring: String
  ): String = {
    val markdown = new StringBuilder()

    if (docstring.nonEmpty)
      markdown
        .append("\n")
        .append(docstring)

    if (symbolSignature.nonEmpty) {
      markdown
        .append("```mfk\n")
        .append(symbolSignature)
        .append("\n```")
    }
    markdown.toString()
  }
}
