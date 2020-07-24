package millfork.compiler

import millfork.node.{Expression, FunctionCallExpression, LiteralExpression}
import millfork.parser.TextCodec

/**
  * @author Karol Stasiak
  */
object StdLibOptUtils {


  private def fn(expr: Expression) = expr match {
    case f: FunctionCallExpression => f.functionName
    case _ => "library function call"
  }

  def isValidNulTerminated(codec: TextCodec, text: List[Expression], minLength: Int = 0, maxLength: Int = 255): Boolean = {
    if (codec.stringTerminator.length != 1) return false
    val Nul = codec.stringTerminator.head
    if (text.init.forall {
      case LiteralExpression(c, _) => c != Nul
      case _ => false
    }) {
      text.lastOption match {
        case Some(LiteralExpression(Nul, _)) if text.size >= minLength + 1 && text.size <= maxLength + 1 => return true
        case _ =>
      }
    }
    false
  }

  def isValidPascal(text: List[Expression]): Boolean = {
    text.headOption match {
      case Some(LiteralExpression(l, _)) if text.size == l + 1 && text.size <= 256 =>
        true
      case _ =>
        false
    }
  }

  def evalStrzLen(ctx: CompilationContext, expr: Expression, codec: TextCodec, text: List[Expression]): Option[Expression] = {
    if (codec.stringTerminator.length != 1) return None
    val Nul = codec.stringTerminator.head
    if (text.init.forall {
      case LiteralExpression(c, _) => c != Nul
      case _ => false
    }) {
      text.lastOption match {
        case Some(LiteralExpression(Nul, _)) if text.size <= 256 =>
          ctx.log.debug(s"Replacing ${fn(expr)} with constant argument", expr.position)
          return Some(LiteralExpression(text.size - 1, 1))
        case _ =>
      }
    }
    None
  }

  def evalPStrLen(ctx: CompilationContext, expr: Expression, text: List[Expression]): Option[Expression] = {
    text.headOption match {
      case Some(LiteralExpression(l, _)) if text.size == l + 1 && text.size <= 256 =>
        ctx.log.debug(s"Replacing ${fn(expr)} with constant argument", expr.position)
        return Some(LiteralExpression(l, 1))
      case _ =>
    }
    None
  }
}
