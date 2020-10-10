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
      case ByConstant(name)  => name
      case ByVariable(name)  => name
      case ByReference(name) => name
      // TODO: Remove default
      case default => ""
    }

  /**
    * TODO: This function is nearly the same as https://github.com/scalameta/metals/blob/main/mtags/src/main/scala/scala/meta/internal/pc/HoverMarkup.scala
    *
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
    if (symbolSignature.nonEmpty) {
      markdown
        .append("```mfk\n")
        .append(symbolSignature)
        .append("\n```")
    }
    if (docstring.nonEmpty)
      markdown
        .append("\n")
        .append(docstring)
    markdown.toString()
  }
}
