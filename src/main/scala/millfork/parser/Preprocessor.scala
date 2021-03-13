package millfork.parser

import fastparse.core.Parsed.{Failure, Success}
import millfork.{CompilationFlag, CompilationOptions, Platform, SeparatedList}
import millfork.error.{ConsoleLogger, Logger}
import millfork.node.Position

import java.nio.charset.StandardCharsets
import scala.collection.immutable.BitSet
import scala.collection.mutable

/**
  * @author Karol Stasiak
  */

case class PreprocessingResult(source: String, featureConstants: Map[String, Long], pragmas: Map[String, Int])

object Preprocessor {

  private val Regex = """\A\s*(?:#|\$\$)\s*([a-z]+)\s*(.*?)\s*\z""".r

  def preprocessForTest(options: CompilationOptions, code: String): PreprocessingResult = {
    apply(options, "", code.linesIterator.toSeq, Nil)
  }

  case class IfContext(hadEnabled: Boolean, hadElse: Boolean, enabledBefore: Boolean)

  private def isNotEmpty(s: String): Boolean = {
    val isEmpty = s.isEmpty || s.startsWith("//") || s.forall(_ == ';')
    ! isEmpty
  }

  def apply(options: CompilationOptions, shortFileName: String, lines: Seq[String], templateParams: List[String]): PreprocessingResult = {
    val platform = options.platform
    val log = options.log
//    if (log.traceEnabled) {
//      platform.features.foreach{
//        case (k, v) => log.trace(f"#define $k%15s $v%d")
//      }
//    }
    val result = mutable.ListBuffer[String]()
    val featureConstants = mutable.Map[String, Long]()
    val actualTemplateParams = mutable.Map[String, String]()
    val pragmas = mutable.Map[String, Int]()
    var enabled = true
    val ifStack = mutable.Stack[IfContext]()
    var lineNo = 0
    var currentFeatures = options.features

    def evalParam(param: String, pos: Some[Position]): Long = {
      new PreprocessorParser(options).expression.parse(param) match {
        case Success(q, _) =>
          val value = q.apply(currentFeatures).getOrElse(0L)
//          log.trace(param + " ===> " + value)
          value
        case Failure(_, _, _) =>
          log.error("Failed to parse expression", pos)
          0L
      }
    }

    def assertIdentifier(ident: String, pos: Option[Position]) : String = {
      ident.foreach{
        case ' ' => log.error("Unexpected space in a preprocessor identifier", pos)
        case '_' => // ok
        case c if c < 128 && Character.isDigit(c) => // ok
        case c if c < 128 && Character.isLetter(c) => // ok
        case _ => log.error("Invalid character in a preprocessor identifier", pos)
      }
      ident
    }

    for (line <- lines) {
      lineNo += 1
      val pos = Some(Position(shortFileName, lineNo, 0, 0))
      val lineWithParamsSubstituted: String = actualTemplateParams.foldLeft[String](line)((l, kv) => l.replace(kv._1, kv._2))
      var resulting = ""
      lineWithParamsSubstituted match {
        case Regex(keyword, param) =>
          keyword match {
            case "template" =>
              if (lineNo != 1) {
                log.error("#template should be the first line in the file", pos)
              }
              val paramNames = param.split(",").map(_.trim)
              if (paramNames.length == 1 && paramNames(0).isEmpty) {
                log.error("#template should be followed by a parameter list", pos)
              } else if (paramNames.exists(_.isEmpty)) {
                log.error("#template is followed by an invalid parameter list", pos)
              } else if (paramNames.length != templateParams.length) {
                log.error(s"#template has ${paramNames.length} parameters, but the module was instantiated with ${templateParams.length} parameters", pos)
              }
              if (paramNames.toSet.size != paramNames.length) {
                log.error(s"#template has duplicate parameter names", pos)
              }
              for {
                p <- paramNames
                q <- paramNames
                if p != q
              } {
                if (p.contains(q)) {
                  log.error(s"#template has duplicate parameters whose names are contained in names of other parameters", pos)
                }
              }
              for((paramName, actualParam) <- paramNames.zip(templateParams)) {
                if (paramName.nonEmpty) {
                  actualTemplateParams(paramName) = actualParam
                }
              }

            case "use" => if (enabled) {
              if (param == "") log.error("#use should have a parameter", pos)
              param.split("=", 2) match {
                case Array(p) =>
                  featureConstants += param -> options.features.getOrElse(param, {
                    if (options.flag(CompilationFlag.FallbackValueUseWarning)) log.warn(s"Undefined parameter $param, assuming 0", pos)
                    0L
                  })
                case Array(p0,p1) => featureConstants += assertIdentifier(p0.trim(), pos) -> evalParam(p1, pos)
              }
            }
            case "define" => if (enabled) {
              param.split("=", 2) match {
                case Array(p0,p1) => currentFeatures += assertIdentifier(p0.trim(), pos) -> evalParam(p1, pos)
                case _ => log.error("#define should have a parameter", pos)
              }
            }
            case "fatal" => if (enabled) log.fatal(param, pos)
            case "error" => if (enabled) log.error(param, pos)
            case "warn" => if (enabled) log.warn(param, pos)
            case "info" => if (enabled) log.info(param, pos)
            case "infoeval" =>
              if (enabled) {
                val value = evalParam(param, pos)
                log.info(s"$param = $value", pos)
              }
            case "if" =>
              if (enabled) {
                val value = evalParam(param, pos)
                enabled = value != 0
                ifStack.push(IfContext(hadEnabled = enabled, hadElse = false, enabledBefore = true))
              } else {
                ifStack.push(IfContext(hadEnabled = false, hadElse = false, enabledBefore = false))
              }
            case "endif" =>
              if (isNotEmpty(param)) log.error("#endif shouldn't have a parameter", pos)
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
                  enabled = i.enabledBefore && value != 0
                  ifStack.push(ifStack.pop().copy(hadEnabled = enabled))
                }
              }
            case "else" =>
              if (isNotEmpty(param)) {
                log.error("#else shouldn't have a parameter", pos)
                if (param.startsWith("if ")) {
                  log.error("Did you mean: #elseif")
                }
              }
              if (ifStack.isEmpty)  log.error("Unmatched #else", pos)
              else {
                val i = ifStack.top
                if (i.hadElse) log.error("Duplicate #else", pos)
                if (i.hadEnabled) {
                  enabled = false
                } else {
                  enabled = i.enabledBefore && !enabled
                }
                ifStack.push(ifStack.pop().copy(hadEnabled = true, hadElse = true))
              }
            case "pragma" =>
              if (enabled) {
                if (param == "") log.error("#pragma should have a parameter", pos)
                pragmas += param -> lineNo
              }
            case "ifdef" =>
              log.error("Invalid preprocessor directive: #" + keyword, pos)
              log.error("Did you mean: #if defined(" + param + ")")
            case "ifndef" =>
              log.error("Invalid preprocessor directive: #" + keyword, pos)
              log.error("Did you mean: #if not(defined(" + param + "))")
            case "elif" | "elsif" =>
              log.error("Invalid preprocessor directive: #" + keyword, pos)
              log.error("Did you mean: #elseif")
            case "undef" =>
              log.error("Invalid preprocessor directive: #" + keyword, pos)
              log.error("A once defined feature cannot be undefined")
            case _ =>
              log.error("Invalid preprocessor directive: #" + keyword, pos)

          }
        case _ => if (enabled) resulting = lineWithParamsSubstituted.replace("\t", "    ")
      }
      if (lineNo == 1 && templateParams.nonEmpty && actualTemplateParams.isEmpty) {
        log.error("A template module imported without actual parameters", pos)
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
  val log: Logger = options.log

  val invalidCharLiteralTypes: BitSet = BitSet(
    Character.LINE_SEPARATOR,
    Character.PARAGRAPH_SEPARATOR,
    Character.CONTROL,
    Character.PRIVATE_USE,
    Character.SURROGATE,
    Character.UNASSIGNED)

  val charAtom: P[Q] =
    ("'" ~/ CharPred(c => c >= ' ' && c != '\'' && !invalidCharLiteralTypes(Character.getType(c))).rep.! ~/ "'" ~/ HWS ~ identifier.?).map {
      case (content, encodingNameOpt) =>
        def theOnly(list: List[Int]): Q = {
          list match {
            case List(value) =>
              _ => Some(value.toLong)
            case _ =>
              log.error(s"Character `$content` cannot be encoded as one byte", None)
              _ => None
          }
        }
        val lenient = options.flag(CompilationFlag.LenientTextEncoding)
        val codepoints = content.codePoints().toArray.toList
        encodingNameOpt match {
          case Some("utf32") =>
            theOnly(TextCodecRepository.RawUtf32.encode(log, None, codepoints, options, lenient))
          case _ =>
            encodingNameOpt.getOrElse("default") match {
              case "default" =>
                theOnly(options.platform.defaultCodec.encode(log, None, codepoints, options, lenient))
              case "scr" =>
                theOnly(options.platform.screenCodec.encode(log, None, codepoints, options, lenient))
              case "z" | "pz" | "p" | "pdefault" | "defaultz" | "pdefaultz" | "pscr" | "scrz" | "pscrz" =>
                log.error("Invalid encoding for character literal")
                _ => None
              case encodingName =>
                val cwf = options.textCodecRepository.forName(encodingName, None, log)
                if (cwf.lengthPrefixed || cwf.nullTerminated) {
                  log.error("Invalid encoding for character literal")
                  _ => None
                } else {
                  theOnly(cwf.codec.encode(log, None, codepoints, options, cwf.lenient))
                }
            }
        }

    }

  val literalAtom: P[Q] = (MfParser.binaryAtom | MfParser.hexAtom | MfParser.octalAtom | MfParser.quaternaryAtom | MfParser.decimalAtom).map(l => (_:M) => Some(l.value)) | charAtom

  val variableAtom: P[Q] = identifier.map(k => _.get(k))

  val atom: P[Q] = P(literalAtom | variableAtom)

  def mfParenExpr: P[Q] = P("(" ~/ HWS ~/ mfExpression(nonStatementLevel) ~ HWS ~/ ")")

  def quotedFunctionCall: P[Q] = for {
    name <- identifier
    _ <- HWS ~ "("
    if name == "same"
    params <- "" ~/ HWS ~/ identifier.rep(min = 0, sep = HWS ~ "," ~/ HWS) ~ HWS ~/ ")" ~/ ""
  } yield (name, params.toList) match {
    case ("same", identifiers) => _ => Some(if (identifiers.toSet.size <= 1) 1L else 0L)
    case _ => alwaysNone
  }

  def functionCall: P[Q] = for {
    name <- identifier
    params <- HWS ~ "(" ~/ HWS ~/ mfExpression(nonStatementLevel).rep(min = 0, sep = HWS ~ "," ~/ HWS) ~ HWS ~/ ")" ~/ ""
  } yield (name, params.toList) match {
    case ("defined", List(p)) => {m:M => Some(if (p(m).isDefined) 1 else 0)}
    case ("not", List(p)) => {m:M => Some(if (p(m).getOrElse(0L) == 0) 1 else 0)}
    case ("lo", List(p)) => {m:M => Some(p(m).getOrElse(0L) & 0xff)}
    case ("hi", List(p)) => {m:M => Some(p(m).getOrElse(0L).>>(8).&(0xff))}
    case ("if", List(i, t, e)) => {m:M => if (i(m).getOrElse(0L) != 0) t(m) else e(m)}
    case ("min", ps@(_::_)) => {m:M => ps.map(_(m)).min}
    case ("max", ps@(_::_)) => {m:M => ps.map(_(m)).max}
    case ("defined" | "lo" | "hi" | "not" | "if" | "min" | "max", ps) =>
      log.error(s"Invalid number of parameters to $name: ${ps.length}")
      alwaysNone
    case _ =>
      log.error("Invalid preprocessor function " + name)
      alwaysNone
  }


  def tightMfExpression: P[Q] = P(mfParenExpr | quotedFunctionCall | functionCall | atom) // TODO

  def tightMfExpressionButNotCall: P[Q] = P(mfParenExpr | atom) // TODO

  def expression: P[Q] = Start ~ HWS ~/ mfExpression(nonStatementLevel) ~ HWS ~ End

  def mfExpression(level: Int): P[Q] = {
    val allowedOperators = MfParser.mfOperatorsDropFlatten(level)

    def inner: P[SeparatedList[Q, String]] = {
      for {
        head <- tightMfExpression ~/ HWS
        maybeOperator <- (StringIn(allowedOperators: _*).! ~ !CharIn(Seq('-','+','/'))).?
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