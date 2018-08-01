package millfork.parser

import fastparse.core.Parsed.{Failure, Success}
import millfork.{CompilationOptions, Platform, SeparatedList}
import millfork.error.{ConsoleLogger, Logger}
import millfork.node.Position

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */

case class PreprocessingResult(source: String, featureConstants: Map[String, Long], pragmas: Map[String, Int])

object Preprocessor {

  private val Regex = raw"\A\s*#\s*([a-z]+)\s*(.*?)\s*\z".r

  def preprocessForTest(options: CompilationOptions, code: String): PreprocessingResult = {
    apply(options, code.lines.toSeq)
  }

  case class IfContext(hadEnabled: Boolean, hadElse: Boolean, enabledBefore: Boolean)

  def apply(options: CompilationOptions, lines: Seq[String]): PreprocessingResult = {
    val platform = options.platform
    val log = options.log
//    if (log.traceEnabled) {
//      platform.features.foreach{
//        case (k, v) => log.trace(f"#define $k%15s $v%d")
//      }
//    }
    val result = mutable.ListBuffer[String]()
    val featureConstants = mutable.Map[String, Long]()
    val pragmas = mutable.Map[String, Int]()
    var enabled = true
    val ifStack = mutable.Stack[IfContext]()
    var lineNo = 0

    def evalParam(param: String, pos: Some[Position]): Long = {
      new PreprocessorParser(options).expression.parse(param) match {
        case Success(q, _) =>
          val value = q.apply(platform.features).getOrElse(0L)
//          log.trace(param + " ===> " + value)
          value
        case Failure(_, _, _) =>
          log.error("Failed to parse expression", pos)
          0L
      }
    }

    for (line <- lines) {
      lineNo += 1
      var resulting = ""
      line match {
        case Regex(keyword, param) =>
          val pos = Some(Position("", lineNo, 0, 0))
          keyword match {
            case "use" => if (enabled) {
              if (param == "") log.error("#use should have a parameter", pos)
              featureConstants += param -> platform.features.getOrElse(param, {
                log.warn(s"Undefined parameter $param, assuming 0", pos)
                0L
              })
            }
            case "fatal" => if (enabled) log.fatal(param, pos)
            case "error" => if (enabled) log.error(param, pos)
            case "warn" => if (enabled) log.warn(param, pos)
            case "info" => if (enabled) log.info(param, pos)
            case "if" =>
              if (enabled) {
                val value = evalParam(param, pos)
                enabled = value != 0
                ifStack.push(IfContext(enabled, false, true))
              } else {
                ifStack.push(IfContext(false, false, false))
              }
            case "endif" =>
              if (param != "") log.error("#endif shouldn't have a parameter", pos)
              if (ifStack.isEmpty)  log.error("Unmatched #endif", pos)
              else {
                enabled = ifStack.pop().enabledBefore
              }
            case "elseif" =>
              val skippingDepth = ifStack.top
              if (ifStack.isEmpty)  log.error("Unmatched #elseif", pos)
              else {
                val i = ifStack.top
                if (i.hadElse) log.error("#elseif after #else", pos)
                if (i.hadEnabled) {
                  enabled = false
                } else {
                  val value = evalParam(param, pos)
                  enabled = value != 0
                  ifStack.push(ifStack.pop().copy(hadEnabled = enabled))
                }
              }
            case "else" =>
              if (param != "") log.error("#else shouldn't have a parameter", pos)
              if (ifStack.isEmpty)  log.error("Unmatched #else", pos)
              else {
                val i = ifStack.top
                if (i.hadElse) log.error("Duplicate #else", pos)
                if (i.hadEnabled) {
                  enabled = false
                } else {
                  enabled = !enabled
                }
                ifStack.push(ifStack.pop().copy(hadEnabled = true, hadElse = true))
              }
            case "pragma" =>
              if (param == "") log.error("#pragma should", pos)
              pragmas += param -> lineNo
            case _ =>
              log.error("Invalid preprocessor directive: #" + keyword, pos)

          }
        case _ => if (enabled) resulting = line
      }
      result += resulting
    }
    if (ifStack.nonEmpty) {
      log.error("Unclosed #if")
    }
//    if (log.traceEnabled) {
//      result.zipWithIndex.foreach {
//        case (line, i) => log.trace(f"${i + 1}%-4d $line%s")
//      }
//    }
    PreprocessingResult(result.mkString("\n"), featureConstants.toMap, pragmas.toMap)
  }


}

class PreprocessorParser(options: CompilationOptions) {

  import fastparse.all._
  import MfParser.{HWS, identifier, nonStatementLevel, mfOperators}

  type M = Map[String, Long]
  type Q = M => Option[Long]
  val alwaysNone: M => Option[Long] = (_: M) => None
  val log = options.log

  val literalAtom: P[Q] = (MfParser.binaryAtom | MfParser.hexAtom | MfParser.octalAtom | MfParser.quaternaryAtom | MfParser.decimalAtom).map(l => _ => Some(l.value))

  val variableAtom: P[Q] = identifier.map(k => _.get(k))

  val atom: P[Q] = P(literalAtom | variableAtom)

  def mfParenExpr: P[Q] = P("(" ~/ HWS ~/ mfExpression(nonStatementLevel) ~ HWS ~/ ")")

  def functionCall: P[Q] = for {
    name <- identifier
    params <- HWS ~ "(" ~/ HWS ~/ mfExpression(nonStatementLevel).rep(min = 0, sep = HWS ~ "," ~/ HWS) ~ HWS ~/ ")" ~/ ""
  } yield (name, params.toList) match {
    case ("defined", List(p)) => {m:M => Some(if (p(m).isDefined) 1 else 0)}
    case ("not", List(p)) => {m:M => Some(if (p(m).getOrElse(0L) == 0) 1 else 0)}
    case ("lo", List(p)) => {m:M => Some(p(m).getOrElse(0L) & 0xff)}
    case ("hi", List(p)) => {m:M => Some(p(m).getOrElse(0L).>>(8).&(0xff))}
    case ("defined" | "lo" | "hi" | "not", ps) =>
      log.error(s"Invalid number of parameters to $name: ${ps.length}")
      alwaysNone
    case _ =>
      log.error("Invalid preprocessor function " + name)
      alwaysNone
  }


  def tightMfExpression: P[Q] = P(mfParenExpr | functionCall | atom) // TODO

  def tightMfExpressionButNotCall: P[Q] = P(mfParenExpr | atom) // TODO

  def expression: P[Q] = Start ~ HWS ~/ mfExpression(nonStatementLevel) ~ HWS ~ End

  def mfExpression(level: Int): P[Q] = {
    val allowedOperators = MfParser.mfOperatorsDropFlatten(level)

    def inner: P[SeparatedList[Q, String]] = {
      for {
        head <- tightMfExpression ~/ HWS
        maybeOperator <- StringIn(allowedOperators: _*).!.?
        maybeTail <- maybeOperator.fold[P[Option[List[(String, Q)]]]](Pass.map(_ => None))(o => (HWS ~/ inner ~/ HWS).map(x2 => Some((o -> x2.head) :: x2.tail)))
      } yield {
        maybeTail.fold[SeparatedList[Q, String]](SeparatedList.of(head))(t => SeparatedList(head, t))
      }
    }

    def arithOp(xs:SeparatedList[SeparatedList[Q, String], String], level: Int, f: (Long, Long) => Long): Q = {
      m:M => Some(xs.items.map{value => p(value, level + 1)(m).getOrElse(0L)}.reduce(f))
    }

    def boolOp(xs:SeparatedList[SeparatedList[Q, String], String], level: Int, f: (Boolean, Boolean) => Boolean): Q = {
      m:M => {
        val b = xs.items.map { value => p(value, level + 1)(m).getOrElse(0L) }.map(_ != 0).reduce(f)
        Some(if (b) 1L else 0L)
      }
    }

    def compOp(xs:SeparatedList[SeparatedList[Q, String], String], level: Int, f: (Long, Long) => Boolean): Q = {
      m:M => {
        val values = xs.items.map { value => p(value, level + 1)(m).getOrElse(0L) }
        Some(if (values.init.zip(values.tail).forall(f.tupled)) 1L else 0L)
      }
    }

    def p(list: SeparatedList[Q, String], level: Int): Q =
      if (level == mfOperators.length) list.head
      else {
        val xs = list.split(mfOperators(level).toSet(_))
        xs.separators.distinct match {
          case Nil =>
            if (xs.tail.nonEmpty)
              log.error("Too many different operators")
            p(xs.head, level + 1)
          case List("+") | List("-") | List("+", "-") | List("-", "+") =>
            val tuples = xs.toPairList("+")
            m:M => Some(tuples.map {
              case ("+", value) => p(value, level + 1)(m).getOrElse(0L)
              case ("-", value) => -p(value, level + 1)(m).getOrElse(0L)
              case _ => 0L
            }.sum)
          case List("+'") | List("-'") | List("+'", "-'") | List("-'", "+'") => ???
          case List(":") => ???
          case List("&&") => boolOp(xs, level, _ && _)
          case List("||") => boolOp(xs, level, _ || _)
          case List("&") => arithOp(xs, level, _ & _)
          case List("|") => arithOp(xs, level, _ | _)
          case List("^") => arithOp(xs, level, _ ^ _)
          case List("*") => arithOp(xs, level, _ * _)
          case List("<<") => arithOp(xs, level, _ << _)
          case List(">>") => arithOp(xs, level, _ >> _)
          case List("==") => compOp(xs, level, _ == _)
          case List("!=") => compOp(xs, level, _ != _)
          case List("<") => compOp(xs, level, _ < _)
          case List(">") => compOp(xs, level, _ > _)
          case List("<=") => compOp(xs, level, _ <= _)
          case List(">=") => compOp(xs, level, _ >= _)
          case List(x) =>
            log.error("Unsupported operator " + x)
            alwaysNone
          case _ =>
            log.error("Too many different operators")
            alwaysNone
        }
      }

    inner.map(x => p(x, 0))
  }
}