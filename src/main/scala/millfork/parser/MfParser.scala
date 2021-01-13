package millfork.parser

import java.lang.Long.parseLong
import java.nio.file.{Files, Paths}
import java.util

import fastparse.all._
import fastparse.core.Parsed.Failure
import millfork.assembly.Elidability
import millfork.env._
import millfork.error.{ConsoleLogger, Logger}
import millfork.node._
import millfork.output.{DivisibleAlignment, MemoryAlignment, NoAlignment}
import millfork.{CompilationFlag, CompilationOptions, Confusables, SeparatedList}

import scala.collection.immutable.BitSet

/**
  * @author Karol Stasiak
  */
abstract class MfParser[T](fileId: String, input: String, currentDirectory: String, options: CompilationOptions, featureConstants: Map[String, Long]) {

  import MfParser._

  var lastPosition = Position(fileId, 1, 1, 0)
  var lastLabel = ""
  protected val log: Logger = options.log

  def allowIntelHexAtomsInAssembly: Boolean

  val enableDebuggingOptions: Boolean = options.flag(CompilationFlag.EnableInternalTestSyntax)

  private def getCharacterNameSafe(c: Char): String = {
    try {
      "U+%04X %s".format(c.toInt, Character.getName(c))
    } catch {
      case _: Throwable => c match {
        case '\n' => "U+000A LINE FEED"
        case '\r' => "U+000D CARRIAGE RETURN"
        case _ => "U+%04X"
      }
    }
  }

  def toAst: Parsed[Program] = {
    val parse = program.parse(input + "\n\n\n")
    parse match {
      case _: Failure[_, _] =>
        if (lastPosition.cursor >= 0 && lastPosition.cursor < input.length) {
          val c = input(lastPosition.cursor)
          if (c >= 0x100 || c < 0x20 || c == '`') {
            log.error("Invalid character %s".format(getCharacterNameSafe(c)), Some(lastPosition))
            Confusables.map.get(c) match {
              case Some(ascii) =>
                log.info(s"Did you mean: $ascii")
              case _ =>
            }
          }
        }
      case _ =>
    }
    parse
  }

  private val lineStarts: Array[Int] = (-1 +: input.zipWithIndex.filter(_._1 == '\n').map(_._2).map(_+1)).toArray

  def position(label: String = ""): P[Position] = Index.map(i => indexToPosition(i, label))

  def indexToPosition(i: Int, label: String): Position = {
    var lineNumber = util.Arrays.binarySearch(lineStarts, i)
    if (lineNumber < 0) {
      lineNumber = - lineNumber - 2
    }
    val columnNumber = i - lineStarts(lineNumber) + 1
    lineNumber += 1
    val newPosition = Position(fileId, lineNumber, columnNumber, i)
    if (newPosition.cursor > lastPosition.cursor) {
      lastPosition = newPosition
      lastLabel = label
    }
    newPosition
  }

  val comment: P[Unit] = P("//" ~ CharsWhile(c => c != '\n' && c != '\r', min = 0) ~ ("\r\n" | "\r" | "\n"))

  val semicolon: P[Unit] = P(";" ~ CharsWhileIn("; \t", min = 0) ~ position("line break after a semicolon").map(_ => ()) ~ (comment | "\r\n" | "\r" | "\n").opaque("<line break>"))

  val semicolonComment: P[Unit] = P(";" ~ CharsWhile(c => c != '\n' && c != '\r' && c != '{' && c != '}', min = 0) ~ position("line break instead of braces").map(_ => ()) ~ ("\r\n" | "\r" | "\n").opaque("<line break>"))

  val AWS: P[Unit] = P((CharIn(" \t\n\r") | semicolon | comment).rep(min = 0)).opaque("<any whitespace>")

  val AWS_asm: P[Unit] = P((CharIn(" \t\n\r") | semicolonComment | comment).rep(min = 0)).opaque("<any whitespace>")

  val Before_EOL: P[Unit] = HWS ~  &("\r" | "\n" | ";" | "//").opaque("<line break>")

  val EOL: P[Unit] = P(HWS ~ ("\r\n" | "\r" | "\n" | semicolon | comment).opaque("<first line break>") ~ AWS).opaque("<line break>")

  val EOL_asm: P[Unit] = P(HWS ~ ("\r\n" | "\r" | "\n" | semicolonComment | comment).opaque("<first line break>") ~ AWS_asm).opaque("<line break>")

  val EOLOrComma: P[Unit] = P(HWS ~ ("\r\n" | "\r" | "\n" | "," | semicolon | comment).opaque("<first line break or comma>") ~ AWS).opaque("<line break or comma>")


  val elidable: P[Elidability.Value] = (("!" | "?").! ~/ HWS).?.map{
    case Some("?") => Elidability.Elidable
    case Some("!") => Elidability.Volatile
    case _ => Elidability.Fixed
  }

  val externFunctionBody: P[Option[List[Statement]]] = P("extern" ~/ PassWith(None))

  val bankDeclaration: P[Option[String]] = ("segment" ~/ AWS ~/ "(" ~/ AWS ~/ identifier ~/ AWS ~/ ")" ~/ AWS).?

  val breakStatement: P[Seq[ExecutableStatement]] = ("break" ~ !letterOrDigit ~/ HWS ~ identifier.?).map(l => Seq(BreakStatement(l.getOrElse(""))))

  val continueStatement: P[Seq[ExecutableStatement]] = ("continue" ~ !letterOrDigit ~/ HWS ~ identifier.?).map(l => Seq(ContinueStatement(l.getOrElse(""))))

  val forDirection: P[ForDirection.Value] =
    ("parallel" ~ HWS ~ "to").!.map(_ => ForDirection.ParallelTo) |
      ("parallel" ~ HWS ~ "until").!.map(_ => ForDirection.ParallelUntil) |
      "until".!.map(_ => ForDirection.Until) |
      "to".!.map(_ => ForDirection.To) |
      ("down" ~/ HWS ~/ "to").!.map(_ => ForDirection.DownTo)

  private def flags_(allowed: String*): P[Set[String]] = (StringIn(allowed: _*).! ~ SWS).rep(min = 0).map(_.toSet).opaque("<flags>")

  val variableFlags: P[Set[String]] = flags_("const", "static", "volatile", "stack", "register")

  val functionFlags: P[Set[String]] = flags_("extern", "asm", "inline", "interrupt", "macro", "noinline", "reentrant", "kernal_interrupt", "const")

  val codec: P[TextCodecWithFlags] = P(position("text codec identifier") ~ identifier.?.map(_.getOrElse(""))).map { case (position, encoding) =>
    val lenient = options.flag(CompilationFlag.LenientTextEncoding)
    encoding match {
      case "" | "default" => TextCodecWithFlags(options.platform.defaultCodec, nullTerminated = false, lengthPrefixed = false, lenient = lenient)
      case "z" | "defaultz" => TextCodecWithFlags(options.platform.defaultCodec, nullTerminated = true, lengthPrefixed = false, lenient = lenient)
      case "p" | "pdefault" => TextCodecWithFlags(options.platform.defaultCodec, nullTerminated = false, lengthPrefixed = true, lenient = lenient)
      case "pz" | "pdefaultz" => TextCodecWithFlags(options.platform.defaultCodec, nullTerminated = true, lengthPrefixed = true, lenient = lenient)
      case "scr" => TextCodecWithFlags(options.platform.screenCodec, nullTerminated = false, lengthPrefixed = false, lenient = lenient)
      case "scrz" => TextCodecWithFlags(options.platform.screenCodec, nullTerminated = true, lengthPrefixed = false, lenient = lenient)
      case "pscr" => TextCodecWithFlags(options.platform.screenCodec, nullTerminated = false, lengthPrefixed = true, lenient = lenient)
      case "pscrz" => TextCodecWithFlags(options.platform.screenCodec, nullTerminated = true, lengthPrefixed = true, lenient = lenient)
      case _ => options.textCodecRepository.forName(encoding, Some(position), log)
    }
  }

  //  def operator: P[String] = P(CharsWhileIn("!-+*/><=~|&^", min=1).!) // TODO: only valid operators

  val charAtom: P[LiteralExpression] = for {
    p <- position()
    c <- "'" ~/ CharPred(c => c >= ' ' && c != '\'' && !invalidCharLiteralTypes(Character.getType(c))).rep.! ~/ "'"
    TextCodecWithFlags(co, zt, pascal, lenient) <- HWS ~ codec
  } yield {
    if (zt) {
      log.error("Zero-terminated encoding is not a valid encoding for a character literal", Some(p))
    }
    if (pascal) {
      log.error("Length-prefixed encoding is not a valid encoding for a character literal", Some(p))
    }
    co.encode(options.log, Some(p), c.codePoints().toArray.toList, options, lenient = lenient) match {
      case List(value) =>
        LiteralExpression(value, 1)
      case _ =>
        log.error(s"Character `$c` cannot be encoded as one byte", Some(p))
        LiteralExpression(co.stringTerminator.head, 1)
    }
  }

  //noinspection NameBooleanParameters
  val variableAtom: P[Expression] = identifier.map{ i =>
    featureConstants.get(i) match {
      case Some(value) => LiteralExpression(value, size(value, false, false, false, false, false, false, false))
      case None => VariableExpression(i)
    }
  }

  val localLabelAtom : P[Expression] = ("." ~ identifier).!.map(VariableExpression)

  val textLiteral: P[List[Expression]] = P(position() ~ doubleQuotedString ~/ HWS ~ codec).map {
      case (p, s, TextCodecWithFlags(co, zt, lp, lenient)) =>
        var characters = co.encode(options.log, None, s.codePoints().toArray.toList, options, lenient = lenient).map(c => LiteralExpression(c, 1).pos(p))
        if (lp) {
          val sizeof = co.stringTerminator.length
          val codeUnitCount = characters.length / sizeof
          val maxAllowed = 1.<<(8*sizeof) - 1
          if (codeUnitCount > maxAllowed) {
            log.error(s"Length-prefixed string too long, the length is $codeUnitCount, maximum allowed is $maxAllowed", Some(p))
          }
          characters = (0 until sizeof).map(i => LiteralExpression(codeUnitCount.>>>(8*i).&(0xff), 1)).toList ++ characters
        }
        if (zt) characters ++= co.stringTerminator.map(nul => LiteralExpression(nul, 1))
        characters
    }

  val textLiteralAtom: P[TextLiteralExpression] = textLiteral.map(TextLiteralExpression)

  val literalAtom: P[LiteralExpression] = binaryAtom | hexAtom | octalAtom | quaternaryAtom | decimalAtom | charAtom

  val literalAtomWithIntel: P[LiteralExpression] = binaryAtom | hexAtom | octalAtom | quaternaryAtom | intelHexAtom | decimalAtom | charAtom

  val atom: P[Expression] = P(position() ~ (variableAtom | localLabelAtom | literalAtom | textLiteralAtom)).map{case (p,a) => a.pos(p)}

  val atomWithIntel: P[Expression] = P(position() ~ (variableAtom | localLabelAtom | literalAtomWithIntel | textLiteralAtom)).map{case (p,a) => a.pos(p)}

  val quotedAtom: P[String] = variableAtom.! | literalAtomWithIntel.map{
    case LiteralExpression(value, _) => value.toString
    case x => x.toString
  } | textLiteralAtom.!

  val importStatement: P[Seq[ImportStatement]] = ("import" ~ !letterOrDigit ~/ SWS ~/
    identifier.rep(min = 1, sep = "/") ~ HWS ~ ("<" ~/ HWS ~/ quotedAtom.rep(min = 1, sep = HWS ~ "," ~/ HWS) ~/ HWS ~/ ">" ~/ Pass).?).
    map{case (name, params) => Seq(ImportStatement(name.mkString("/"), params.getOrElse(Nil).toList))}

  val optimizationHintsDeclaration: P[Set[String]] =
    if (options.flag(CompilationFlag.EnableInternalTestSyntax)) {
      ("¥" ~/ HWS ~ "(" ~/ HWS ~/ identifier.rep(min = 0, sep = AWS ~ "," ~/ AWS) ~ HWS ~ ")" ~/ "").?.map {
        case None => Set()
        case Some(list) => list.toSet
      }
    } else P("").map(_ => Set.empty)
  
  val globalVariableDefinition: P[Seq[BankedDeclarationStatement]] = variableDefinition(true)
  val localVariableDefinition: P[Seq[DeclarationStatement]] = variableDefinition(false)

  def singleVariableDefinition: P[(Position, String, Option[Expression], Option[Expression], Set[String], Option[MemoryAlignment])] = for {
      p <- position()
      name <- identifier ~/ HWS ~/ Pass
      alignment1 <- alignmentDeclaration(fastAlignmentForFunctions).? ~/ HWS
      optimizationHints <- optimizationHintsDeclaration ~/ HWS
      alignment2 <- alignmentDeclaration(fastAlignmentForFunctions).? ~/ HWS
      addr <- ("@" ~/ HWS ~/ mfExpression(1, false)).?.opaque("<address>") ~ HWS
      initialValue <- ("=" ~/ AWS ~/ mfExpression(1, false)).? ~/ HWS // TODO
    } yield {
      if (alignment1.isDefined && alignment2.isDefined) log.error(s"Cannot define the alignment multiple times", Some(p))
      val alignment = alignment1.orElse(alignment2)
      (p, name, addr, initialValue, optimizationHints, alignment)
    }

  def variableDefinition(implicitlyGlobal: Boolean): P[Seq[BankedDeclarationStatement]] = for {
    p <- position()
    bank <- bankDeclaration
    flags <- variableFlags ~ HWS
    typ <- identifier ~/ SWS
    if typ != "array"
    vars <- singleVariableDefinition.rep(min = 1, sep = "," ~/ HWS)
    _ <- Before_EOL ~/ ""
  } yield {
    vars.map { case (p, name, addr, initialValue, optimizationHints, alignment) => VariableDeclarationStatement(name, typ,
      bank,
      global = implicitlyGlobal || flags("static"),
      stack = flags("stack"),
      constant = flags("const"),
      volatile = flags("volatile"),
      register = flags("register"),
      initialValue, addr, optimizationHints, alignment).pos(p)
    }
  }

  val paramDefinition: P[ParameterDeclaration] = for {
    p <- position()
    typ <- identifier ~/ SWS ~/ Pass
    name <- identifier ~/ Pass
  } yield {
    if (name == "register" || name == "const" || name == "ref" || name == "call") {
      log.error(s"Invalid parameter name: `$name`. Did you mean writing a macro?", Some(p))
    }
    ParameterDeclaration(typ, ByVariable(name)).pos(p)
  }

  def asmExpression: P[Expression] = (position() ~ NoCut(
    ("<" ~/ HWS ~ mfExpression(mathLevel, allowIntelHexAtomsInAssembly)).map(e => HalfWordExpression(e, hiByte = false)) |
      (">" ~/ HWS ~ mfExpression(mathLevel, allowIntelHexAtomsInAssembly)).map(e => HalfWordExpression(e, hiByte = true)) |
      mfExpression(mathLevel, allowIntelHexAtomsInAssembly)
  )).map { case (p, e) => e.pos(p) }

  def asmExpressionWithParens: P[(Expression, Boolean)] = (position() ~ NoCut(
    ("(" ~ HWS ~ asmExpression ~ HWS ~ ")").map(_ -> true) |
      asmExpression.map(_ -> false)
  )).map { case (p, e) => e._1.pos(p) -> e._2 }

  def asmExpressionWithParensOrApostrophe: P[(Expression, Boolean)] = (position() ~ NoCut(
    ("(" ~ HWS ~ asmExpression ~ HWS ~ ")").map(_ -> true) |
    (asmExpression ~ "'").map(_ -> true) |
      asmExpression.map(_ -> false)
  )).map { case (p, e) => e._1.pos(p) -> e._2 }

  def appcRegister: P[ParamPassingConvention]

  val appcComplex: P[ParamPassingConvention] =
    for {
      pos <- position("passing method")
      keyword <- (("const" | "ref" | "register" | "call").! ~ !letterOrDigit ~/ Pass).? ~/ Pass
      _ <- if (keyword.contains("call")) {log.error(s"Invalid assembly macro parameter passing convention: `call`", Some(pos)) ; Pass } else Pass
      if !keyword.contains("call")
      _ <- position("register name")
      register <- keyword match {
        case Some("register") => (AWS ~ "(" ~/ AWS ~ appcRegister ~/ AWS ~ ")" ~/ AWS).map(Some(_))
        case Some(_) => SWS.map(_ => None)
        case None => Pass.map(_ => None)
      }
      _ <- position("parameter name")
      ident <- identifier
    }  yield ((keyword, register, ident) match {
      case (None, _, name) => ByVariable(name)
      case (Some("const"), _, name) => ByConstant(name)
      case (Some("ref"), _, name) => ByReference(name)
      case (Some("register"), Some(reg), _) => reg
      case x => log.fatal(s"Unknown assembly parameter passing convention: `$x`")
    })

  val asmParamDefinition: P[ParameterDeclaration] = for {
    p <- position()
    typ <- identifier ~ SWS ~/ Pass
    appc <- appcRegister | appcComplex
  } yield ParameterDeclaration(typ, appc).pos(p)

  val macroParamDefinition: P[ParameterDeclaration] = for {
    p <- position()
    typ <- identifier ~ SWS ~/ Pass
    pos <- position("passing method")
    keyword <- ( ("const" | "ref" | "call" | "register").! ~ !letterOrDigit ~/ Pass).?
    _ <- if (keyword.contains("register")) {log.error(s"Invalid non-assembly macro parameter passing convention: `register`. Did you forget `asm`?", Some(pos)) ; Pass } else Pass
    if !keyword.contains("register")
    _ <- if (keyword.isDefined) SWS else Pass
    _ <- position("parameter name")
    name <- identifier
  } yield ParameterDeclaration(typ, (keyword match {
    case None => ByReference(name)
    case Some("ref") => ByReference(name)
    case Some("const") => ByConstant(name)
    case Some("call") => ByLazilyEvaluableExpressionVariable(name)
    case Some(x) => log.fatal(s"Invalid non-assembly macro parameter passing convention: `$x`")
  })).pos(p)

  def arrayListElement: P[ArrayContents] = arrayStringContents | arrayProcessedContents | arrayLoopContents | arrayFileContents | mfExpression(nonStatementLevel, false).map(e => LiteralContents(List(e)))

  def arrayProcessedContents: P[ArrayContents] = for {
    _ <- "@" ~/ HWS
    filter <- identifier
    _ <- AWS
    contents <- arrayContents
  } yield ProcessedContents(filter, contents)

  def arrayListContents: P[ArrayContents] = ("[" ~/ AWS ~/ arrayListElement.rep(sep = AWS ~ "," ~/ AWS) ~ AWS ~ "]" ~/ Pass).map(c => CombinedContents(c.toList))

  // TODO: should reserve the `file` identifier here?
  val arrayFileContents: P[ArrayContents] = for {
    p <- "file" ~ HWS ~/ "(" ~/ HWS ~/ position("file name")
    filePath <- doubleQuotedString ~/ HWS
    _ <- position("file start")
    optStart <- ("," ~/ HWS ~/ literalAtom ~/ HWS ~/ Pass).?
    _ <- position("slice length")
    optLength <- ("," ~/ HWS ~/ literalAtom ~/ HWS ~/ Pass).?
    _ <- position("closing parentheses")
    _ <- ")" ~/ Pass
  } yield {
    val data = Files.readAllBytes(Paths.get(currentDirectory, filePath))
    val slice: Array[Byte] = (optStart.map(_.value.toInt), optLength.map(_.value.toInt)) match {
      case (Some(start), Some(length)) =>
        if (data.length < start) {
          log.error(s"File $filePath is shorter (${data.length} B) that the start offset $start", Some(p))
          Array.fill(length)(0.toByte)
        } else if (data.length < start + length) {
          log.error(s"File $filePath is shorter (${data.length} B) that the start offset plus length ${start + length}", Some(p))
          Array.fill(length)(0.toByte)
        } else {
          data.slice(start, start + length)
        }
      case (Some(start), None) =>
        if (data.length < start) {
          log.error(s"File $filePath is shorter (${data.length} B) that the start offset $start", Some(p))
          Array[Byte](0)
        } else {
          data.drop(start)
        }
      case (None, None) => data
      case _ => throw new IllegalStateException("error parsing file()")
    }
    LiteralContents(slice.map(c => LiteralExpression(c & 0xff, 1)).toList)
  }

  def arrayStringContents: P[ArrayContents] = textLiteral.map(LiteralContents)

  def arrayLoopContents: P[ArrayContents] = for {
      identifier <- "for" ~ SWS ~/ identifier ~/ HWS ~ "," ~/ HWS ~ Pass
      start <- mfExpression(nonStatementLevel, false) ~ HWS ~ "," ~/ HWS ~/ Pass
      pos <- position("loop direction")
      direction <- forDirection ~/ HWS ~/ "," ~/ HWS ~/ Pass
      end <- mfExpression(nonStatementLevel, false, allowTopLevelIndexing = false)
      body <- AWS ~ arrayContents
    } yield {
    val fixedDirection = direction match {
      case ForDirection.ParallelUntil =>
        if (options.flag(CompilationFlag.FallbackValueUseWarning)) log.warn("`paralleluntil` is not allowed in array definitions, assuming `until`", Some(pos))
        ForDirection.Until
      case ForDirection.ParallelTo =>
        if (options.flag(CompilationFlag.FallbackValueUseWarning)) log.warn("`parallelto` is not allowed in array definitions, assuming `to`", Some(pos))
        ForDirection.To
      case x => x
    }
    ForLoopContents(identifier, start, end, fixedDirection, body)
  }

  def arrayContents: P[ArrayContents] = arrayProcessedContents | arrayListContents | arrayLoopContents | arrayFileContents | arrayStringContents

  def arrayContentsForAsm: P[RawBytesStatement] = (arrayListContents | arrayStringContents).map(c => RawBytesStatement(c, options.isBigEndian))

  val aliasDefinition: P[Seq[AliasDefinitionStatement]] = for {
    p <- position()
    name <- "alias" ~ !letterOrDigit ~/ SWS ~ identifier ~ HWS
    target <- "=" ~/ AWS ~/ identifier ~/ HWS
    important <- "!".!.? ~/ HWS
  } yield Seq(AliasDefinitionStatement(name, target, important.isDefined).pos(p))

  def fastAlignmentForArrays: MemoryAlignment
  def fastAlignmentForFunctions: MemoryAlignment

  def alignmentDeclaration(fast: MemoryAlignment): P[MemoryAlignment] = (position() ~ "align" ~/ AWS ~/ "(" ~/ AWS ~/ atom ~/ AWS ~/ ")").map {
    case (_, LiteralExpression(1, _)) => NoAlignment
    case (pos, LiteralExpression(n, _)) =>
      if (n >= 1 && n <= 0x8000 & (n & (n - 1)) == 0) DivisibleAlignment(n.toInt)
      else {
        log.error("Invalid alignment: " + n, Some(pos))
        NoAlignment
      }
    case (pos, VariableExpression("fast")) => fast
    case (pos, _) =>
      log.error("Invalid alignment", Some(pos))
      NoAlignment
  }

  val arrayDefinition: P[Seq[ArrayDeclarationStatement]] = for {
    p <- position()
    bank <- bankDeclaration
    const <- ("const".! ~ HWS).?
    _ <- "array" ~ !letterOrDigit
    elementType <- ("(" ~/ AWS ~/ identifier ~ AWS ~ ")").? ~/ HWS
    name <- identifier ~/ HWS
    length <- ("[" ~/ AWS ~/ mfExpression(nonStatementLevel, false) ~ AWS ~ "]").? ~ HWS
    alignment1 <- alignmentDeclaration(fastAlignmentForFunctions).? ~/ HWS
    optimizationHints <- optimizationHintsDeclaration ~/ HWS
    alignment2 <- alignmentDeclaration(fastAlignmentForFunctions).? ~/ HWS
    addr <- ("@" ~/ HWS ~/ mfExpression(1, false)).? ~/ HWS
    contents <- ("=" ~/ AWS ~/ arrayContents).? ~/ HWS
  } yield {
    if (alignment1.isDefined && alignment2.isDefined) log.error(s"Cannot define the alignment multiple times", Some(p))
    val alignment = alignment1.orElse(alignment2)
    Seq(ArrayDeclarationStatement(name, bank, length, elementType.getOrElse("byte"), addr, const.isDefined, contents, optimizationHints, alignment, options.isBigEndian).pos(p))
  }

  def tightMfExpression(allowIntelHex: Boolean, allowTopLevelIndexing: Boolean): P[Expression] = {
    val a = if (allowIntelHex) atomWithIntel else atom
    if (allowTopLevelIndexing)
      mfExpressionWrapper[Expression](mfParenExpr(allowIntelHex) | derefExpression | functionCall(allowIntelHex) | a)
    else
      mfParenExpr(allowIntelHex) | derefExpression | functionCall(allowIntelHex) | a
  }

  def tightMfExpressionButNotCall(allowIntelHex: Boolean, allowTopLevelIndexing: Boolean): P[Expression] = {
    val a = if (allowIntelHex) atomWithIntel else atom
    if (allowTopLevelIndexing)
      mfExpressionWrapper[Expression](mfParenExpr(allowIntelHex) | derefExpression | a)
    else
      mfParenExpr(allowIntelHex) | derefExpression | a
  }

  def mfExpression(level: Int, allowIntelHex: Boolean, allowTopLevelIndexing: Boolean = true): P[Expression] = {
    val allowedOperators = mfOperatorsDropFlatten(level)

    def inner: P[SeparatedList[(Boolean, Expression), String]] = {
      for {
        minus <- ("-".rep(min = 1).!.map(_.length().&(1).==(1)) ~/ HWS).?.map(_.getOrElse(false))
        head <- tightMfExpression(allowIntelHex, allowTopLevelIndexing) ~/ HWS
        maybeOperator <- (StringIn(allowedOperators: _*).! ~ !CharIn(Seq('/', '=', '-', '+', ':', '>', '<', '\''))).map(op => if (mfOperatorNormalizations.contains(op)) mfOperatorNormalizations(op) else op).?
        maybeTail <- maybeOperator.fold[P[Option[List[(String, (Boolean, Expression))]]]](Pass.map(_ => None))(o => (AWS ~/ inner ~/ HWS).map(x2 => Some((o -> x2.head) :: x2.tail)))
      } yield {
        maybeTail.fold[SeparatedList[(Boolean, Expression), String]](SeparatedList.of(minus -> head))(t => SeparatedList(minus -> head, t))
      }
    }

    def p(list: SeparatedList[(Boolean, Expression), String], level: Int): Expression =
      if (level == mfOperators.length) {
        if (list.head._1) {
          LiteralExpression(0, 1) #-# list.head._2
        } else {
          list.head._2
        }
      } else {
        val xs = list.split(mfOperators(level).toSet(_))
        xs.separators.distinct match {
          case Nil =>
            if (xs.tail.nonEmpty)
              log.error("Too many different operators; consider using parentheses for disambiguation", xs.head.head._2.position)
            p(xs.head, level + 1)
          case List("+") | List("-") | List("+", "-") | List("-", "+") =>
            SumExpression(xs.toPairList("+").map {
              case (op, value) =>
                if (value.count(_._1) > 0) {
                  if (value.size == 1) {
                    log.error("Too many different operators; consider using parentheses for disambiguation", xs.head.head._2.position)
                  }
                  (op == "+", p(value.map(p => (false, p._2)), level + 1))
                } else {
                  (op == "-", p(value, level + 1))
                }
            }, decimal = false).pos(list.head._2.position)
          case List("+'") | List("-'") | List("+'", "-'") | List("-'", "+'") =>
            SumExpression(xs.toPairList("+").map { case (op, value) =>
              if (value.exists(_._1)) log.error("Too many different operators; consider using parentheses for disambiguation", xs.head.head._2.position)
              (op == "-'", p(value, level + 1))
            }, decimal = true).pos(list.head._2.position)
          case List(":") =>
            if (xs.size != 2) {
              log.error("The `:` operator can have only two arguments", xs.head.head._2.position)
              LiteralExpression(0, 1)
            } else {
              SeparateBytesExpression(p(xs.head, level + 1), p(xs.tail.head._2, level + 1)).pos(list.head._2.position)
            }
          case List(eq) if level == 0 =>
            if (xs.size != 2) {
              log.error(s"The `$eq` operator can have only two arguments", xs.head.head._2.position)
              LiteralExpression(0, 1)
            } else {
              FunctionCallExpression(eq, xs.items.map(value => p(value, level + 1))).pos(list.head._2.position)
            }
          case List(op) =>
            FunctionCallExpression(op, xs.items.map(value => p(value, level + 1))).pos(list.head._2.position)
          case _ =>
            log.error("Too many different operators; consider using parentheses for disambiguation", xs.head.head._2.position)
            LiteralExpression(0, 1)
        }
      }

    inner.map(x => p(x, 0))
  }

  def index: P[Expression] = HWS ~ "[" ~/ AWS ~/ mfExpression(nonStatementLevel, false) ~ AWS ~/ "]" ~/ Pass

  def mfExpressionWrapper[E <: Expression](inner: P[E]): P[E] = for {
    expr <- inner
    firstIndices <- index.rep
    fieldPath <- (HWS ~ (("->".! ~/ AWS) | ".".!) ~/ AWS ~/ identifier ~/ index.rep).rep
  } yield (expr, firstIndices, fieldPath) match {
    case (_, Seq(), Seq()) => expr
    case (VariableExpression(vname), Seq(i), Seq()) => IndexedExpression(vname, i).pos(expr.position).asInstanceOf[E]
    case _ =>
      val fixedFieldPath = fieldPath.flatMap { e =>
        e match {
          case (".", "pointer", _) => Seq(e)
          case (".", f, _) if f.startsWith("pointer.") => Seq(e)
          case (".", "addr", _) => Seq(e)
          case (".", f, _) if f.startsWith("addr.") => Seq(e)
          case (".", f, i) => Seq((".", "pointer", Nil), ("->", f, i))
          case _ => Seq(e)
        }
      }
      IndirectFieldExpression(expr, firstIndices, fixedFieldPath.map {case (a,b,c) => (a == ".", b, c)}).pos(expr.position).asInstanceOf[E]
  }

//  def mfLhsExpression: P[LhsExpression] = for {
//    (p, left) <- position() ~ mfLhsExpressionSimple
//    rightOpt <- (HWS ~ ":" ~/ HWS ~ mfLhsExpressionSimple).?
//  } yield rightOpt.fold(left)(right => SeparateBytesExpression(left, right).pos(p))

  def mfLhsExpressionSimple: P[LhsExpression] =
    mfExpressionWrapper[LhsExpression](derefExpression | (position() ~ identifier).map{case (p,n) => VariableExpression(n).pos(p)} ~ HWS)

  def mfLhsExpression: P[LhsExpression] =
    mfExpression(nonStatementLevel, false).filter(_.isInstanceOf[LhsExpression]).map(_.asInstanceOf[LhsExpression])

  def mfParenExpr(allowIntelHex: Boolean): P[Expression] = P("(" ~/ AWS ~/ mfExpression(nonStatementLevel, allowIntelHex) ~ AWS ~/ ")")

  def functionCall(allowIntelHex: Boolean): P[FunctionCallExpression] = for {
    p <- position()
    name <- identifier
    params <- HWS ~ "(" ~/ AWS ~/ mfExpression(nonStatementLevel, allowIntelHex).rep(min = 0, sep = AWS ~ "," ~/ AWS) ~ AWS ~/ ")" ~/ ""
  } yield FunctionCallExpression(name, params.toList).pos(p)

  val derefExpression: P[DerefDebuggingExpression] = for {
    p <- position()
    if enableDebuggingOptions
    yens <- CharsWhileIn(Seq('¥')).! ~/ AWS
    inner <- mfParenExpr(false)
  } yield DerefDebuggingExpression(inner, yens.length).pos(p)

  val expressionStatement: P[Seq[ExecutableStatement]] = mfExpression(0, false).map {
    case FunctionCallExpression("=", List(t: LhsExpression, s)) =>
      Seq(Assignment(t, s).pos(t.position))
    case x@FunctionCallExpression("=", exprs) =>
      log.error("Invalid left-hand-side of an assignment", x.position)
      exprs.map(ExpressionStatement)
    case x =>
      Seq(ExpressionStatement(x).pos(x.position))
  }

  def keywordStatement: P[Seq[ExecutableStatement]] = P(
    returnOrDispatchStatement |
      gotoStatement |
      labelStatement |
      ifStatement |
      whileStatement |
      forEachStatement |
      forStatement |
      doWhileStatement |
      breakStatement |
      continueStatement |
      inlineAssembly)

  def executableStatement: P[Seq[ExecutableStatement]] = (position() ~ P(keywordStatement | expressionStatement)).map { case (p, s) => s.map(_.pos(p)) }

  def asmStatement: P[ExecutableStatement]

  def statement: P[Seq[Statement]] = (position() ~ P(keywordStatement | arrayDefinition | localVariableDefinition | expressionStatement)).map { case (p, s) => s.map(_.pos(p)) }

  def asmStatements: P[List[ExecutableStatement]] = ("{" ~/ AWS_asm ~/ asmStatement.rep(sep = NoCut(EOL_asm) ~ !"}" ~/ Pass) ~/ AWS_asm ~/ "}" ~/ Pass).map(_.toList)

  def statements: P[List[Statement]] = ("{" ~/ AWS ~ statement.rep(sep = NoCut(EOL) ~ !"}" ~/ Pass) ~/ AWS ~/ "}" ~/ Pass).map(_.flatten.toList)

  def mfFunctionSmallBody: P[List[Statement]] = for {
    _ <- "=" ~/ AWS
    expression <- mfExpression(nonStatementLevel, false)
  } yield List(ReturnStatement(Some(expression)).pos(expression.position))

  def mfFunctionBody: P[List[Statement]] = statements | mfFunctionSmallBody

  def executableStatements: P[Seq[ExecutableStatement]] = ("{" ~/ AWS ~/ executableStatement.rep(sep = NoCut(EOL) ~ !"}" ~/ Pass) ~/ AWS ~ "}").map(_.flatten)

  val dispatchLabel: P[ReturnDispatchLabel] =
    ("default" ~ !letterOrDigit ~/ AWS ~/ ("(" ~/ position("default branch range") ~ AWS ~/ mfExpression(nonStatementLevel, false).rep(min = 0, sep = AWS ~ "," ~/ AWS) ~ AWS ~/ ")" ~/ "").?).map{
      case None => DefaultReturnDispatchLabel(None, None)
      case Some((_, Seq())) => DefaultReturnDispatchLabel(None, None)
      case Some((_, Seq(e))) => DefaultReturnDispatchLabel(None, Some(e))
      case Some((_, Seq(s, e))) => DefaultReturnDispatchLabel(Some(s), Some(e))
      case Some((pos, _)) =>
        log.error("Invalid default branch declaration", Some(pos))
        DefaultReturnDispatchLabel(None, None)
    } | mfExpression(nonStatementLevel, false).rep(min = 0, sep = AWS ~ "," ~/ AWS).map(exprs => StandardReturnDispatchLabel(exprs.toList))

  val dispatchBranch: P[ReturnDispatchBranch] = for {
    pos <- position()
    l <- dispatchLabel ~/ HWS ~/ "@" ~/ HWS
    f <- tightMfExpressionButNotCall(false, allowTopLevelIndexing = false) ~/ HWS
    parameters <- ("(" ~/ position("dispatch actual parameters") ~ AWS ~/ mfExpression(nonStatementLevel, false).rep(min = 0, sep = AWS ~ "," ~/ AWS) ~ AWS ~/ ")" ~/ "").?
  } yield ReturnDispatchBranch(l, f, parameters.map(_._2.toList).getOrElse(Nil)).pos(pos)

  val dispatchStatementBody: P[Seq[ExecutableStatement]] = for {
    indexer <- "[" ~/ AWS ~/ mfExpression(nonStatementLevel, false) ~/ AWS ~/ "]" ~/ AWS
    _ <- position("dispatch statement body")
    parameters <- ("(" ~/ position("dispatch parameters") ~ AWS ~/ mfLhsExpression.rep(min = 0, sep = AWS ~ "," ~/ AWS) ~ AWS ~/ ")" ~/ "").?
    _ <- AWS ~/ position("dispatch statement body") ~/ "{" ~/ AWS
    branches <- dispatchBranch.rep(sep = EOL ~ !"}" ~/ Pass)
    _ <- AWS ~/ "}"
  } yield Seq(ReturnDispatchStatement(indexer, parameters.map(_._2.toList).getOrElse(Nil), branches.toList))

  val returnOrDispatchStatement: P[Seq[ExecutableStatement]] = "return" ~ !letterOrDigit ~/ HWS ~ (dispatchStatementBody | mfExpression(nonStatementLevel, false).?.map(ReturnStatement).map(Seq(_)))

  val gotoStatement: P[Seq[ExecutableStatement]] = "goto" ~ !letterOrDigit ~/ HWS ~ mfExpression(nonStatementLevel, false).map(GotoStatement).map(Seq(_))

  val labelStatement: P[Seq[ExecutableStatement]] = "label" ~ !letterOrDigit ~/ HWS ~ identifier.map(LabelStatement).map(Seq(_))

  def ifStatement: P[Seq[ExecutableStatement]] = for {
    condition <- "if" ~ !letterOrDigit ~/ HWS ~/ mfExpression(nonStatementLevel, false)
    thenBranch <- AWS ~/ executableStatements
    elseBranch <- (AWS ~ "else" ~/ AWS ~/ ((for{
      p <- position()
      s <- ifStatement
    } yield s.map(_.pos(p))) | executableStatements)).?
  } yield Seq(IfStatement(condition, thenBranch.toList, elseBranch.getOrElse(Nil).toList))

  def whileStatement: P[Seq[ExecutableStatement]] = for {
    condition <- "while" ~ !letterOrDigit ~/ HWS ~/ mfExpression(nonStatementLevel, false)
    body <- AWS ~ executableStatements
  } yield Seq(WhileStatement(condition, body.toList, Nil))

  def forStatement: P[Seq[ExecutableStatement]] = for {
    identifier <- "for" ~/ SWS ~/ identifier ~/ HWS ~ "," ~/ HWS ~ Pass
    start <- mfExpression(nonStatementLevel, false) ~ HWS ~ "," ~/ HWS ~/ Pass
    direction <- forDirection ~/ HWS ~/ "," ~/ HWS ~/ Pass
    end <- mfExpression(nonStatementLevel, false)
    body <- AWS ~ executableStatements
  } yield Seq(ForStatement(identifier, start, end, direction, body.toList))

  def forEachStatement: P[Seq[ExecutableStatement]] = for {
    id <- "for" ~ SWS ~ identifier ~ HWS ~ ("," ~ HWS ~ identifier ~ HWS).? ~ ":" ~/ HWS ~ Pass
    values <- ("[" ~/ AWS ~/ mfExpression(0, false).rep(min = 0, sep = AWS ~ "," ~/ AWS) ~ AWS ~/ "]" ~/ "").map(seq => Right(seq.toList)) | mfExpression(0, false).map(Left(_))
    body <- AWS ~ executableStatements
  } yield Seq(ForEachStatement(id._1, id._2, values, body.toList))

  def inlineAssembly: P[Seq[ExecutableStatement]] = "asm" ~ !letterOrDigit ~/ AWS ~ asmStatements

  //noinspection MutatorLikeMethodIsParameterless
  def doWhileStatement: P[Seq[ExecutableStatement]] = for {
    body <- "do" ~ !letterOrDigit ~/ AWS ~ executableStatements ~/ AWS
    condition <- "while" ~ !letterOrDigit ~/ HWS ~/ mfExpression(nonStatementLevel, false)
  } yield Seq(DoWhileStatement(body.toList, Nil, condition))

  val functionDefinition: P[Seq[BankedDeclarationStatement]] = for {
    p <- position()
    bank <- bankDeclaration
    flags <- functionFlags ~ HWS
    returnType <- identifier ~ SWS
    if !Environment.neverValidTypeIdentifiers(returnType)
    name <- identifier ~ HWS
    params <- "(" ~/ AWS ~/ (if (flags("asm")) asmParamDefinition else if (flags("macro")) macroParamDefinition else paramDefinition).rep(sep = AWS ~ "," ~/ AWS) ~ AWS ~ ")" ~/ AWS
    alignment1 <- alignmentDeclaration(fastAlignmentForFunctions).? ~/ AWS
    optimizationHints <- optimizationHintsDeclaration ~/ HWS
    alignment2 <- alignmentDeclaration(fastAlignmentForFunctions).? ~/ AWS
    addr <- ("@" ~/ HWS ~/ mfExpression(1, false)).?.opaque("<address>") ~/ AWS
    statements <- (externFunctionBody | (if (flags("asm")) asmStatements else mfFunctionBody).map(l => Some(l))) ~/ Pass
  } yield {
    if (alignment1.isDefined && alignment2.isDefined) log.error(s"Cannot define the alignment multiple times", Some(p))
    val alignment = alignment1.orElse(alignment2)
    if (flags("extern")) log.error("The extern keyword should go at the end of a function declaration", Some(p))
    if (flags("interrupt") && flags("macro")) log.error(s"Interrupt function `$name` cannot be macros", Some(p))
    if (flags("kernal_interrupt") && flags("macro")) log.error(s"Kernal interrupt function `$name` cannot be macros", Some(p))
    if (flags("interrupt") && flags("reentrant")) log.error(s"Interrupt function `$name` cannot be reentrant", Some(p))
    if (flags("interrupt") && flags("kernal_interrupt")) log.error(s"Interrupt function `$name` cannot be a Kernal interrupt", Some(p))
    if (flags("macro") && flags("reentrant")) log.error("Reentrant and macro exclude each other", Some(p))
    if (flags("inline") && flags("noinline")) log.error("Noinline and inline exclude each other", Some(p))
    if (flags("macro") && flags("noinline")) log.error("Noinline and macro exclude each other", Some(p))
    if (flags("inline") && flags("macro")) log.error("Macro and inline exclude each other", Some(p))
    if (flags("interrupt") && returnType != "void") log.error(s"Interrupt function `$name` has to return void", Some(p))
    if (flags("const") && returnType == "void") log.error(s"Const-pure function `$name` cannot return void", Some(p))
    if (flags("const") && flags("interrupt")) log.error(s"Const-pure function `$name` cannot be an interrupt", Some(p))
    if (flags("const") && flags("kernal_interrupt")) log.error(s"Const-pure function `$name` cannot be a Kernal interrupt", Some(p))
    if (flags("const") && flags("macro")) log.error(s"Const-pure function `$name` cannot be a macro", Some(p))
    if (flags("const") && flags("asm")) log.error(s"Const-pure function `$name` cannot contain assembly", Some(p))
    if (addr.isEmpty && statements.isEmpty) log.error(s"Extern function `$name` must have an address", Some(p))
    if (addr.isDefined && alignment.isDefined) log.error(s"Function `$name` has both address and alignment", Some(p))
    if (statements.isEmpty && alignment.isDefined) log.error(s"Extern function `$name` cannot have alignment", Some(p))
    if (statements.isEmpty && !flags("asm") && params.nonEmpty) log.error(s"Extern non-asm function `$name` cannot have parameters", Some(p))
    if (flags("asm")) validateAsmFunctionBody(p, flags, name, statements)
    if (flags("macro")) {
      statements.flatMap(_.find(_.isInstanceOf[VariableDeclarationStatement])) match {
        case Some(s) =>
          log.error(s"Macro functions cannot declare variables", s.position)
        case None =>
      }
      statements.flatMap(_.find(_.isInstanceOf[ArrayDeclarationStatement])) match {
        case Some(s) =>
          log.error(s"Macro functions cannot declare arrays", s.position)
        case None =>
      }
    }
    Seq(FunctionDeclarationStatement(name, returnType, params.toList,
      bank,
      addr,
      optimizationHints,
      alignment,
      statements,
      flags("macro"),
      if (flags("inline")) Some(true) else if (flags("noinline")) Some(false) else None,
      flags("asm"),
      flags("interrupt"),
      flags("kernal_interrupt"),
      flags("const") && !flags("asm"),
      flags("reentrant")).pos(p))
  }

  def validateAsmFunctionBody(p: Position, flags: Set[String], name: String, statements: Option[List[Statement]])

  val enumVariant: P[(String, Option[Expression])] = for {
    name <- identifier ~/ HWS
    value <- ("=" ~/ AWS ~/ mfExpression(1, false)).? ~ HWS
  } yield name -> value

  val enumVariants: P[List[(String, Option[Expression])]] =
    ("{" ~/ AWS ~ enumVariant.rep(sep = NoCut(EOLOrComma) ~ !"}" ~/ Pass) ~/ AWS ~/ "}" ~/ Pass).map(_.toList)

  val enumDefinition: P[Seq[EnumDefinitionStatement]] = for {
    p <- position()
    _ <- "enum" ~ !letterOrDigit ~/ SWS ~ position("enum name")
    name <- identifier ~/ HWS
    _ <- position("enum defintion block")
    variants <- enumVariants ~/ Pass
  } yield Seq(EnumDefinitionStatement(name, variants).pos(p))

  val compoundTypeField: P[FieldDesc] = ("array".! ~ HWS ~/ Pass).?.flatMap {
    case None =>
      for {
        typ <- identifier ~/ HWS
        name <- identifier ~ HWS
      } yield FieldDesc(typ, name, None)

    case Some(_) =>
      for {
        elementType <- ("(" ~/ AWS ~/ identifier ~ AWS ~ ")").? ~/ HWS
        if enableDebuggingOptions
        name <- identifier ~ HWS
        length <- "[" ~/ AWS ~/ mfExpression(nonStatementLevel, false) ~ AWS ~ "]" ~ HWS
      } yield FieldDesc(elementType.getOrElse("byte"), name, Some(length))
  }

  val compoundTypeFields: P[List[FieldDesc]] =
    ("{" ~/ AWS ~ compoundTypeField.rep(sep = NoCut(EOLOrComma) ~ !"}" ~/ Pass) ~/ AWS ~/ "}" ~/ Pass).map(_.toList)

  val structDefinition: P[Seq[StructDefinitionStatement]] = for {
    p <- position()
    _ <- "struct" ~ !letterOrDigit ~/ SWS ~ position("struct name")
    name <- identifier ~/ HWS
    align <- alignmentDeclaration(NoAlignment).? ~/ HWS
    _ <- position("struct definition block")
    fields <- compoundTypeFields ~/ Pass
  } yield Seq(StructDefinitionStatement(name, fields, align).pos(p))

  val unionDefinition: P[Seq[UnionDefinitionStatement]] = for {
    p <- position()
    _ <- "union" ~ !letterOrDigit ~/ SWS ~ position("union name")
    name <- identifier ~/ HWS
    align <- alignmentDeclaration(NoAlignment).? ~/ HWS
    _ <- position("union definition block")
    fields <- compoundTypeFields ~/ Pass
  } yield Seq(UnionDefinitionStatement(name, fields, align).pos(p))

  val segmentBlock: P[Seq[BankedDeclarationStatement]] = for {
    (_, bankName) <- "segment" ~ AWS ~ "(" ~ AWS ~ position("segment name") ~ identifier ~ AWS ~ ")" ~ AWS ~ "{" ~/ AWS
    body <- locatableDefinition.rep(sep = EOL)
    _ <- AWS ~ "}" ~/ Pass
  } yield {
    body.flatten.map { stmt =>
      if (stmt.bank.isEmpty) stmt.withChangedBank(bankName)
      else stmt
    }
  }


  def checkForNonlocatableDefinitions: P[Seq[BankedDeclarationStatement]] =
    ((StringIn("alias", "enum", "struct", "union", "import").! ~ SWS) ~/ position()).map{ x =>
      log.fatal(s"`${x._1}` statements are not allowed inside segment blocks", Some(x._2))
    }

  def locatableDefinition: P[Seq[BankedDeclarationStatement]] = checkForNonlocatableDefinitions | segmentBlock | arrayDefinition | functionDefinition | globalVariableDefinition

  val program: Parser[Program] = for {
    _ <- Start ~/ AWS ~/ position("top level statement")
    definitions <- (importStatement | aliasDefinition | enumDefinition | structDefinition | unionDefinition | locatableDefinition).rep(sep = EOL)
    _ <- AWS ~ End
  } yield Program(definitions.flatten.toList)

}

object MfParser {

  val SWS: P[Unit] = P(CharsWhileIn(" \t", min = 1)).opaque("<horizontal whitespace>")

  val HWS: P[Unit] = P(CharsWhileIn(" \t", min = 0)).opaque("<horizontal whitespace>")

  val letter: P[String] = P(CharIn("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_").!)

  val mosOpcodeLetter: P[String] = P(CharIn("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz012").!)

  val octalDigit: P[String] = P(CharIn("01234567").!)

  val letterOrDigit: P[Unit] = P(CharIn("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_.$1234567890"))

  val realLetterOrDigit: P[Unit] = P(CharIn("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_.1234567890"))

  val identifierTail: P[String] =
    CharsWhileIn("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_.1234567890", min = 1).rep(min = 1, sep = "$").!

  val identifier: P[String] = (letter ~ ("$".? ~ identifierTail).?).!.map(_.intern()).opaque("<identifier>")

  val doubleQuotedString: P[String] = P("\"" ~/ CharsWhile(c => c != '\"' && c != '\n' && c != '\r').?.! ~ "\"")

  def size(value: Long, wordLiteral: Boolean, int24Literal: Boolean, int32Literal: Boolean, int40Literal: Boolean, int48Literal: Boolean, int56Literal: Boolean, int64Literal: Boolean): Int = {
    val w  = value > 0xff              || value < -0x80             || wordLiteral
    val f  = value > 0xffff            || value < -0x8000           || int24Literal
    val l  = value > 0xffffff          || value < -0x800000         || int32Literal
    val q5 = value > 0xFFFFffffL       || value < -0x80000000L      || int40Literal
    val q6 = value > 0xffFFFFffffL     || value < -0x8000000000L    || int48Literal
    val q7 = value > 0xffffFFFFffffL   || value < -0x800000000000L  || int56Literal
    val q8 = value > 0xFFffffFFFFffffL || value < -0x8000000000000L || int64Literal
    if (q8) 8 else if (q7) 7 else if (q6) 6 else if (q5) 5 else if (l) 4 else if (f) 3 else if (w) 2 else 1
  }

  def sign(abs: Long, minus: Boolean): Long = if (minus) -abs else abs

  val invalidCharLiteralTypes: BitSet = BitSet(
    Character.LINE_SEPARATOR,
    Character.PARAGRAPH_SEPARATOR,
    Character.CONTROL,
    Character.PRIVATE_USE,
    Character.SURROGATE,
    Character.UNASSIGNED)

  val decimalAtom: P[LiteralExpression] =
    for {
      minus <- "-".!.?
      s <- CharsWhileIn("1234567890", min = 1).!.opaque("<decimal digits>") ~ !("x" | "b")
    } yield {
      val abs = parseLong(s, 10)
      val value = sign(abs, minus.isDefined)
      LiteralExpression(value, size(value, s.length > 3,  s.length > 5, s.length > 7, s.length > 10, s.length > 13, s.length > 15, s.length > 17))
    }

  val binaryAtom: P[LiteralExpression] =
    for {
      minus <- "-".!.?
      _ <- P("0b" | "%") ~/ Pass
      s <- CharsWhileIn("01", min = 1).!.opaque("<binary digits>")
    } yield {
      val abs = parseLong(s, 2)
      val value = sign(abs, minus.isDefined)
      LiteralExpression(value, size(value, s.length > 8, s.length > 16, s.length > 24, s.length > 32, s.length > 40, s.length > 48, s.length > 52))
    }

  val hexAtom: P[LiteralExpression] =
    for {
      minus <- "-".!.?
      _ <- P("0x" | "0X" | "$") ~/ Pass
      s <- CharsWhileIn("1234567890abcdefABCDEF", min = 1).!.opaque("<hex digits>")
    } yield {
      val abs = parseLong(s, 16)
      val value = sign(abs, minus.isDefined)
      LiteralExpression(value, size(value, s.length > 2, s.length > 4, s.length > 6, s.length > 8, s.length > 10, s.length > 12, s.length > 14))
    }

  val intelHexAtom: P[LiteralExpression] =
    for {
      minus <- "-".!.?
      head <- CharIn("0123456789").!
      tail <- CharsWhileIn("1234567890abcdefABCDEF", min = 1).!.opaque("<hex digits>")
      _ <- P("h" | "H")
    } yield {
      // check for marking zero:
      val s = if (head == "0" && tail.nonEmpty && tail.head >'9') tail else head + tail
      val abs = parseLong(s, 16)
      val value = sign(abs, minus.isDefined)
      LiteralExpression(value, size(value, s.length > 2, s.length > 4, s.length > 6, s.length > 8, s.length > 10, s.length > 12, s.length > 14))
    }

  val octalAtom: P[LiteralExpression] =
    for {
      minus <- "-".!.?
      _ <- P("0o" | "0O") ~/ Pass
      s <- CharsWhileIn("01234567", min = 1).!.opaque("<octal digits>")
    } yield {
      val abs = parseLong(s, 8)
      val value = sign(abs, minus.isDefined)
      LiteralExpression(value, size(value, s.length > 3, s.length > 6, s.length > 8, s.length > 11, s.length > 14, s.length > 16, s.length > 19))
    }

  val quaternaryAtom: P[LiteralExpression] =
    for {
      minus <- "-".!.?
      _ <- P("0q" | "0Q") ~/ Pass
      s <- CharsWhileIn("0123", min = 1).!.opaque("<quaternary digits>")
    } yield {
      val abs = parseLong(s, 4)
      val value = sign(abs, minus.isDefined)
      LiteralExpression(value, size(value, s.length > 4, s.length > 8, s.length > 12, s.length > 16, s.length > 20, s.length > 24, s.length > 28))
    }

  val mfOperators = List(
    List("+=", "-=", "+'=", "-'=", "^=", "&=", "|=", "*=", "*'=", "<<=", ">>=", "<<'=", ">>'=", "/=", "%%=", "=", "$*=", "$+=", "$-=", "$<<=", "$>>="),
    List("||", "^^"),
    List("&&"),
    List("==", "<=", ">=", "!=", "<", ">"),
    List(":"),
    List("+'", "-'", "<<'", ">>'", ">>>>", "+", "-", "&", "|", "^", "<<", ">>", "$+", "$-", "$<<", "$>>"),
    List("*'", "$*", "*", "/", "%%"))

  val mfOperatorNormalizations = Map(
    "$+" -> "+'",
    "$-" -> "-'",
    "$+=" -> "+'=",
    "$-=" -> "-'=",
    "$<<" -> "<<'",
    "$>>" -> ">>'",
    "$<<=" -> "<<'=",
    "$>>=" -> ">>'=",
    "$*" -> "*'",
    "$*=" -> "*'=",
  )

  val mfOperatorsDropFlatten: IndexedSeq[List[String]] = mfOperators.indices.map(i => mfOperators.drop(i).flatten)

  val nonStatementLevel = 1 // everything but not `=`
  val mathLevel = 4 // the `:` operator
  val minusLevel = 5 // the `-` operator

}
