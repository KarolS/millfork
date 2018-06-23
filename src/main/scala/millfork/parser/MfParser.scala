package millfork.parser

import java.nio.file.{Files, Paths}

import fastparse.all._
import millfork.assembly.mos.Opcode
import millfork.env._
import millfork.error.ErrorReporting
import millfork.node._
import millfork.{CompilationOptions, SeparatedList}

/**
  * @author Karol Stasiak
  */
abstract class MfParser[T](filename: String, input: String, currentDirectory: String, options: CompilationOptions) {

  var lastPosition = Position(filename, 1, 1, 0)
  var lastLabel = ""

  def toAst: Parsed[Program] = program.parse(input + "\n\n\n")

  private val lineStarts: Array[Int] = (0 +: input.zipWithIndex.filter(_._1 == '\n').map(_._2)).toArray

  def position(label: String = ""): P[Position] = Index.map(i => indexToPosition(i, label))

  def indexToPosition(i: Int, label: String): Position = {
    val prefix = lineStarts.takeWhile(_ <= i)
    val newPosition = Position(filename, prefix.length, i - prefix.last, i)
    if (newPosition.cursor > lastPosition.cursor) {
      lastPosition = newPosition
      lastLabel = label
    }
    newPosition
  }

  val comment: P[Unit] = P("//" ~/ CharsWhile(c => c != '\n' && c != '\r', min = 0) ~ ("\r\n" | "\r" | "\n"))

  val SWS: P[Unit] = P(CharsWhileIn(" \t", min = 1)).opaque("<horizontal whitespace>")

  val HWS: P[Unit] = P(CharsWhileIn(" \t", min = 0)).opaque("<horizontal whitespace>")

  val AWS: P[Unit] = P((CharIn(" \t\n\r;") | NoCut(comment)).rep(min = 0)).opaque("<any whitespace>")

  val EOL: P[Unit] = P(HWS ~ ("\r\n" | "\r" | "\n" | comment).opaque("<first line break>") ~ AWS).opaque("<line break>")

  val letter: P[String] = P(CharIn("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_").!)

  val letterOrDigit: P[Unit] = P(CharIn("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_.$1234567890"))

  val lettersOrDigits: P[String] = P(CharsWhileIn("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_.$1234567890", min = 0).!)

  val identifier: P[String] = P((letter ~ lettersOrDigits).map { case (a, b) => a + b }).opaque("<identifier>")

  //  def operator: P[String] = P(CharsWhileIn("!-+*/><=~|&^", min=1).!) // TODO: only valid operators

  private val invalidCharLiteralTypes = Set[Int](
    Character.LINE_SEPARATOR,
    Character.PARAGRAPH_SEPARATOR,
    Character.CONTROL,
    Character.PRIVATE_USE,
    Character.SURROGATE,
    Character.UNASSIGNED)

  def charAtom: P[LiteralExpression] = for {
    p <- position()
    c <- "'" ~/ CharPred(c => c >= ' ' && !invalidCharLiteralTypes(Character.getType(c))).! ~/ "'"
    co <- HWS ~ codec
  } yield {
    co.encode(Some(p), c.charAt(0)) match {
      case List(value) =>
        LiteralExpression(value, 1)
      case _ =>
        ErrorReporting.error(s"Character `$c` cannot be encoded as one byte", Some(p))
        LiteralExpression(0, 1)
    }
  }

  def size(value: Int, wordLiteral: Boolean, farwordLiteral: Boolean, longLiteral: Boolean): Int = {
    val w = value > 255 || value < -0x80 || wordLiteral
    val f = value > 0xffff || value < -0x8000 || farwordLiteral
    val l = value > 0xffffff || value < -0x800000 || longLiteral
    if (l) 4 else if (f) 3 else if (w) 2 else 1
  }

  def sign(abs: Int, minus: Boolean): Int = if (minus) -abs else abs

  val decimalAtom: P[LiteralExpression] =
    for {
      p <- position()
      minus <- "-".!.?
      s <- CharsWhileIn("1234567890", min = 1).!.opaque("<decimal digits>") ~ !("x" | "b")
    } yield {
      val abs = Integer.parseInt(s, 10)
      val value = sign(abs, minus.isDefined)
      LiteralExpression(value, size(value, s.length > 3,  s.length > 5, s.length > 7)).pos(p)
    }

  val binaryAtom: P[LiteralExpression] =
    for {
      p <- position()
      minus <- "-".!.?
      _ <- P("0b" | "%") ~/ Pass
      s <- CharsWhileIn("01", min = 1).!.opaque("<binary digits>")
    } yield {
      val abs = Integer.parseInt(s, 2)
      val value = sign(abs, minus.isDefined)
      LiteralExpression(value, size(value, s.length > 8, s.length > 16, s.length > 24)).pos(p)
    }

  val hexAtom: P[LiteralExpression] =
    for {
      p <- position()
      minus <- "-".!.?
      _ <- P("0x" | "0X" | "$") ~/ Pass
      s <- CharsWhileIn("1234567890abcdefABCDEF", min = 1).!.opaque("<hex digits>")
    } yield {
      val abs = Integer.parseInt(s, 16)
      val value = sign(abs, minus.isDefined)
      LiteralExpression(value, size(value, s.length > 2, s.length > 4, s.length > 6)).pos(p)
    }

  val octalAtom: P[LiteralExpression] =
    for {
      p <- position()
      minus <- "-".!.?
      _ <- P("0o" | "0O") ~/ Pass
      s <- CharsWhileIn("01234567", min = 1).!.opaque("<octal digits>")
    } yield {
      val abs = Integer.parseInt(s, 8)
      val value = sign(abs, minus.isDefined)
      LiteralExpression(value, size(value, s.length > 3, s.length > 6, s.length > 9)).pos(p)
    }

  val quaternaryAtom: P[LiteralExpression] =
    for {
      p <- position()
      minus <- "-".!.?
      _ <- P("0q" | "0Q") ~/ Pass
      s <- CharsWhileIn("0123", min = 1).!.opaque("<quaternary digits>")
    } yield {
      val abs = Integer.parseInt(s, 4)
      val value = sign(abs, minus.isDefined)
      LiteralExpression(value, size(value, s.length > 4, s.length > 8, s.length > 12)).pos(p)
    }

  val literalAtom: P[LiteralExpression] = charAtom | binaryAtom | hexAtom | octalAtom | quaternaryAtom | decimalAtom

  val atom: P[Expression] = P(literalAtom | (position() ~ identifier).map { case (p, i) => VariableExpression(i).pos(p) })

  val mfOperators = List(
    List("+=", "-=", "+'=", "-'=", "^=", "&=", "|=", "*=", "*'=", "<<=", ">>=", "<<'=", ">>'="),
    List("||", "^^"),
    List("&&"),
    List("==", "<=", ">=", "!=", "<", ">"),
    List(":"),
    List("+'", "-'", "<<'", ">>'", ">>>>", "+", "-", "&", "|", "^", "<<", ">>"),
    List("*'", "*"))

  val nonStatementLevel = 1 // everything but not `=`
  val mathLevel = 4 // the `:` operator

  def flags(allowed: String*): P[Set[String]] = StringIn(allowed: _*).!.rep(min = 0, sep = SWS).map(_.toSet).opaque("<flags>")

  def variableDefinition(implicitlyGlobal: Boolean): P[Seq[DeclarationStatement]] = for {
    p <- position()
    bank <- bankDeclaration
    flags <- flags("const", "static", "volatile", "stack", "register") ~ HWS
    typ <- identifier ~ SWS
    name <- identifier ~/ HWS ~/ Pass
    addr <- ("@" ~/ HWS ~/ mfExpression(1)).?.opaque("<address>") ~ HWS
    initialValue <- ("=" ~/ HWS ~/ mfExpression(1)).? ~ HWS
    _ <- &(EOL) ~/ ""
  } yield {
    Seq(VariableDeclarationStatement(name, typ,
      bank,
      global = implicitlyGlobal || flags("static"),
      stack = flags("stack"),
      constant = flags("const"),
      volatile = flags("volatile"),
      register = flags("register"),
      initialValue, addr).pos(p))
  }

  val externFunctionBody: P[Option[List[Statement]]] = P("extern" ~/ PassWith(None))

  val paramDefinition: P[ParameterDeclaration] = for {
    p <- position()
    typ <- identifier ~/ SWS ~/ Pass
    name <- identifier ~/ Pass
  } yield {
    ParameterDeclaration(typ, ByVariable(name)).pos(p)
  }

  def asmParamDefinition: P[ParameterDeclaration]

  def arrayListElement: P[ArrayContents] = arrayStringContents | arrayLoopContents | arrayFileContents | mfExpression(nonStatementLevel).map(e => LiteralContents(List(e)))

  def arrayProcessedContents: P[ArrayContents] = for {
    _ <- "@" ~/ HWS
    filter <- identifier
    _ <- AWS
    contents <- arrayContents
  } yield ProcessedContents(filter, contents)

  def arrayListContents: P[ArrayContents] = ("[" ~/ AWS ~/ arrayListElement.rep(sep = AWS ~ "," ~/ AWS) ~ AWS ~ "]" ~/ Pass).map(c => CombinedContents(c.toList))

  val doubleQuotedString: P[List[Char]] = P("\"" ~/ CharsWhile(c => c != '\"' && c != '\n' && c != '\r').! ~ "\"").map(_.toList)

  def codec: P[TextCodec] = P(position() ~ identifier).map {
    case (_, "ascii") => TextCodec.Ascii
    case (_, "petscii") => TextCodec.Petscii
    case (_, "pet") => TextCodec.Petscii
    case (_, "scr") => TextCodec.CbmScreencodes
    case (_, "atascii") => TextCodec.Atascii
    case (_, "atari") => TextCodec.Atascii
    case (_, "bbc") => TextCodec.Bbc
    case (_, "apple2") => TextCodec.Apple2
    case (_, "jis") => TextCodec.Jis
    case (_, "jisx") => TextCodec.Jis
    case (_, "iso_de") => TextCodec.IsoIec646De
    case (_, "iso_no") => TextCodec.IsoIec646No
    case (_, "iso_dk") => TextCodec.IsoIec646No
    case (_, "iso_se") => TextCodec.IsoIec646Se
    case (_, "iso_fi") => TextCodec.IsoIec646Se
    case (_, "iso_yu") => TextCodec.IsoIec646Yu
    case (p, x) =>
      ErrorReporting.error(s"Unknown string encoding: `$x`", Some(p))
      TextCodec.Ascii
  }

  // TODO: should reserve the `file` identifier here?
  def arrayFileContents: P[ArrayContents] = for {
    p <- "file" ~ HWS ~/ "(" ~/ HWS ~/ position()
    filePath <- doubleQuotedString ~/ HWS
    optSlice <- ("," ~/ HWS ~/ literalAtom ~/ HWS ~/ "," ~/ HWS ~/ literalAtom ~/ HWS ~/ Pass).?
    _ <- ")" ~/ Pass
  } yield {
    val data = Files.readAllBytes(Paths.get(currentDirectory, filePath.mkString))
    val slice = optSlice.fold(data) {
      case (start, length) => data.slice(start.value.toInt, start.value.toInt + length.value.toInt)
    }
    LiteralContents(slice.map(c => LiteralExpression(c & 0xff, 1)).toList)
  }

  def arrayStringContents: P[ArrayContents] = P(position() ~ doubleQuotedString ~/ HWS ~ codec).map {
    case (p, s, co) => LiteralContents(s.flatMap(c => co.encode(None, c)).map(c => LiteralExpression(c, 1).pos(p)))
  }

  def arrayLoopContents: P[ArrayContents] = for {
      identifier <- "for" ~ SWS ~/ identifier ~/ HWS ~ "," ~/ HWS ~ Pass
      start <- mfExpression(nonStatementLevel) ~ HWS ~ "," ~/ HWS ~/ Pass
      pos <- position()
      direction <- forDirection ~/ HWS ~/ "," ~/ HWS ~/ Pass
      end <- mfExpression(nonStatementLevel)
      body <- AWS ~ arrayContents
    } yield {
    val fixedDirection = direction match {
      case ForDirection.ParallelUntil =>
        ErrorReporting.warn("`paralleluntil` is not allowed in array definitions, assuming `until`", options, Some(pos))
        ForDirection.Until
      case ForDirection.ParallelTo =>
        ErrorReporting.warn("`parallelto` is not allowed in array definitions, assuming `to`", options, Some(pos))
        ForDirection.To
      case x => x
    }
    ForLoopContents(identifier, start, end, fixedDirection, body)
  }

  def arrayContents: P[ArrayContents] = arrayProcessedContents | arrayListContents | arrayLoopContents | arrayFileContents | arrayStringContents

  def arrayContentsForAsm: P[RawBytesStatement] = (arrayListContents | arrayStringContents).map(RawBytesStatement)

  def arrayDefinition: P[Seq[ArrayDeclarationStatement]] = for {
    p <- position()
    bank <- bankDeclaration
    name <- "array" ~ !letterOrDigit ~/ SWS ~ identifier ~ HWS
    length <- ("[" ~/ AWS ~/ mfExpression(nonStatementLevel) ~ AWS ~ "]").? ~ HWS
    addr <- ("@" ~/ HWS ~/ mfExpression(1)).? ~/ HWS
    contents <- ("=" ~/ HWS ~/ arrayContents).? ~/ HWS
  } yield Seq(ArrayDeclarationStatement(name, bank, length, addr, contents).pos(p))

  def tightMfExpression: P[Expression] = P(mfParenExpr | functionCall | mfIndexedExpression | atom) // TODO

  def tightMfExpressionButNotCall: P[Expression] = P(mfParenExpr | mfIndexedExpression | atom) // TODO

  def mfExpression(level: Int): P[Expression] = {
    val allowedOperators = mfOperators.drop(level).flatten

    def inner: P[SeparatedList[Expression, String]] = {
      for {
        head <- tightMfExpression ~/ HWS
        maybeOperator <- StringIn(allowedOperators: _*).!.?
        maybeTail <- maybeOperator.fold[P[Option[List[(String, Expression)]]]](Pass.map(_ => None))(o => (HWS ~/ inner ~/ HWS).map(x2 => Some((o -> x2.head) :: x2.tail)))
      } yield {
        maybeTail.fold[SeparatedList[Expression, String]](SeparatedList.of(head))(t => SeparatedList(head, t))
      }
    }

    def p(list: SeparatedList[Expression, String], level: Int): Expression =
      if (level == mfOperators.length) list.head
      else {
        val xs = list.split(mfOperators(level).toSet(_))
        xs.separators.distinct match {
          case Nil =>
            if (xs.tail.nonEmpty)
              ErrorReporting.error("Too many different operators")
            p(xs.head, level + 1)
          case List("+") | List("-") | List("+", "-") | List("-", "+") =>
            SumExpression(xs.toPairList("+").map { case (op, value) => (op == "-", p(value, level + 1)) }, decimal = false)
          case List("+'") | List("-'") | List("+'", "-'") | List("-'", "+'") =>
            SumExpression(xs.toPairList("+").map { case (op, value) => (op == "-", p(value, level + 1)) }, decimal = true)
          case List(":") =>
            if (xs.size != 2) {
              ErrorReporting.error("The `:` operator can have only two arguments", xs.head.head.position)
              LiteralExpression(0, 1)
            } else {
              SeparateBytesExpression(p(xs.head, level + 1), p(xs.tail.head._2, level + 1))
            }
          case List(op) =>
            FunctionCallExpression(op, xs.items.map(value => p(value, level + 1)))
          case _ =>
            ErrorReporting.error("Too many different operators")
            LiteralExpression(0, 1)
        }
      }

    inner.map(x => p(x, 0))
  }

  def mfLhsExpressionSimple: P[LhsExpression] = mfIndexedExpression | (position() ~ identifier).map { case (p, n) => VariableExpression(n).pos(p) }

  def mfLhsExpression: P[LhsExpression] = for {
    (p, left) <- position() ~ mfLhsExpressionSimple
    rightOpt <- (HWS ~ ":" ~/ HWS ~ mfLhsExpressionSimple).?
  } yield rightOpt.fold(left)(right => SeparateBytesExpression(left, right).pos(p))


  def mfParenExpr: P[Expression] = P("(" ~/ AWS ~/ mfExpression(nonStatementLevel) ~ AWS ~/ ")")

  def mfIndexedExpression: P[IndexedExpression] = for {
    p <- position()
    array <- identifier
    index <- HWS ~ "[" ~/ AWS ~/ mfExpression(nonStatementLevel) ~ AWS ~/ "]"
  } yield IndexedExpression(array, index).pos(p)

  def functionCall: P[FunctionCallExpression] = for {
    p <- position()
    name <- identifier
    params <- HWS ~ "(" ~/ AWS ~/ mfExpression(nonStatementLevel).rep(min = 0, sep = AWS ~ "," ~/ AWS) ~ AWS ~/ ")" ~/ ""
  } yield FunctionCallExpression(name, params.toList).pos(p)

  val expressionStatement: P[Seq[ExecutableStatement]] = mfExpression(0).map(x => Seq(ExpressionStatement(x)))

  val assignmentStatement: P[Seq[ExecutableStatement]] =
    (position() ~ mfLhsExpression ~ HWS ~ "=" ~/ HWS ~ mfExpression(1)).map {
      case (p, l, r) => Seq(Assignment(l, r).pos(p))
    }

  def keywordStatement: P[Seq[ExecutableStatement]] = P(
    returnOrDispatchStatement |
      ifStatement |
      whileStatement |
      forStatement |
      doWhileStatement |
      breakStatement |
      continueStatement |
      inlineAssembly |
      assignmentStatement)

  def executableStatement: P[Seq[ExecutableStatement]] = (position() ~ P(keywordStatement | expressionStatement)).map { case (p, s) => s.map(_.pos(p)) }

  def asmStatement: P[ExecutableStatement]

  def statement: P[Seq[Statement]] = (position() ~ P(keywordStatement | variableDefinition(false) | expressionStatement)).map { case (p, s) => s.map(_.pos(p)) }

  def asmStatements: P[List[ExecutableStatement]] = ("{" ~/ AWS ~/ asmStatement.rep(sep = NoCut(EOL) ~ !"}" ~/ Pass) ~/ AWS ~/ "}" ~/ Pass).map(_.toList)

  def statements: P[List[Statement]] = ("{" ~/ AWS ~ statement.rep(sep = NoCut(EOL) ~ !"}" ~/ Pass) ~/ AWS ~/ "}" ~/ Pass).map(_.flatten.toList)

  def executableStatements: P[Seq[ExecutableStatement]] = ("{" ~/ AWS ~/ executableStatement.rep(sep = NoCut(EOL) ~ !"}" ~/ Pass) ~/ AWS ~ "}").map(_.flatten)

  def dispatchLabel: P[ReturnDispatchLabel] =
    ("default" ~ !letterOrDigit ~/ AWS ~/ ("(" ~/ position("default branch range") ~ AWS ~/ mfExpression(nonStatementLevel).rep(min = 0, sep = AWS ~ "," ~/ AWS) ~ AWS ~/ ")" ~/ "").?).map{
      case None => DefaultReturnDispatchLabel(None, None)
      case Some((_, Seq())) => DefaultReturnDispatchLabel(None, None)
      case Some((_, Seq(e))) => DefaultReturnDispatchLabel(None, Some(e))
      case Some((_, Seq(s, e))) => DefaultReturnDispatchLabel(Some(s), Some(e))
      case Some((pos, _)) =>
        ErrorReporting.error("Invalid default branch declaration", Some(pos))
        DefaultReturnDispatchLabel(None, None)
    } | mfExpression(nonStatementLevel).rep(min = 0, sep = AWS ~ "," ~/ AWS).map(exprs => StandardReturnDispatchLabel(exprs.toList))

  def dispatchBranch: P[ReturnDispatchBranch] = for {
    pos <- position()
    l <- dispatchLabel ~/ HWS ~/ "@" ~/ HWS
    f <- tightMfExpressionButNotCall ~/ HWS
    parameters <- ("(" ~/ position("dispatch actual parameters") ~ AWS ~/ mfExpression(nonStatementLevel).rep(min = 0, sep = AWS ~ "," ~/ AWS) ~ AWS ~/ ")" ~/ "").?
  } yield ReturnDispatchBranch(l, f, parameters.map(_._2.toList).getOrElse(Nil)).pos(pos)

  def dispatchStatementBody: P[Seq[ExecutableStatement]] = for {
    indexer <- "[" ~/ AWS ~/ mfExpression(nonStatementLevel) ~/ AWS ~/ "]" ~/ AWS
    _ <- position("dispatch statement body")
    parameters <- ("(" ~/ position("dispatch parameters") ~ AWS ~/ mfLhsExpression.rep(min = 0, sep = AWS ~ "," ~/ AWS) ~ AWS ~/ ")" ~/ "").?
    _ <- AWS ~/ position("dispatch statement body") ~/ "{" ~/ AWS
    branches <- dispatchBranch.rep(sep = EOL ~ !"}" ~/ Pass)
    _ <- AWS ~/ "}"
  } yield Seq(ReturnDispatchStatement(indexer, parameters.map(_._2.toList).getOrElse(Nil), branches.toList))

  def returnOrDispatchStatement: P[Seq[ExecutableStatement]] = "return" ~ !letterOrDigit ~/ HWS ~ (dispatchStatementBody | mfExpression(nonStatementLevel).?.map(ReturnStatement).map(Seq(_)))

  def breakStatement: P[Seq[ExecutableStatement]] = ("break" ~ !letterOrDigit ~/ HWS ~ identifier.?).map(l => Seq(BreakStatement(l.getOrElse(""))))

  def continueStatement: P[Seq[ExecutableStatement]] = ("continue" ~ !letterOrDigit ~/ HWS ~ identifier.?).map(l => Seq(ContinueStatement(l.getOrElse(""))))

  def ifStatement: P[Seq[ExecutableStatement]] = for {
    condition <- "if" ~ !letterOrDigit ~/ HWS ~/ mfExpression(nonStatementLevel)
    thenBranch <- AWS ~/ executableStatements
    elseBranch <- (AWS ~ "else" ~/ AWS ~/ (ifStatement | executableStatements)).?
  } yield Seq(IfStatement(condition, thenBranch.toList, elseBranch.getOrElse(Nil).toList))

  def whileStatement: P[Seq[ExecutableStatement]] = for {
    condition <- "while" ~ !letterOrDigit ~/ HWS ~/ mfExpression(nonStatementLevel)
    body <- AWS ~ executableStatements
  } yield Seq(WhileStatement(condition, body.toList, Nil))

  def forDirection: P[ForDirection.Value] =
    ("parallel" ~ HWS ~ "to").!.map(_ => ForDirection.ParallelTo) |
      ("parallel" ~ HWS ~ "until").!.map(_ => ForDirection.ParallelUntil) |
      "until".!.map(_ => ForDirection.Until) |
      "to".!.map(_ => ForDirection.To) |
      ("down" ~/ HWS ~/ "to").!.map(_ => ForDirection.DownTo)

  def forStatement: P[Seq[ExecutableStatement]] = for {
    identifier <- "for" ~ SWS ~/ identifier ~/ HWS ~ "," ~/ HWS ~ Pass
    start <- mfExpression(nonStatementLevel) ~ HWS ~ "," ~/ HWS ~/ Pass
    direction <- forDirection ~/ HWS ~/ "," ~/ HWS ~/ Pass
    end <- mfExpression(nonStatementLevel)
    body <- AWS ~ executableStatements
  } yield Seq(ForStatement(identifier, start, end, direction, body.toList))

  def inlineAssembly: P[Seq[ExecutableStatement]] = for {
    condition <- "asm" ~ !letterOrDigit ~/ Pass
    body <- AWS ~ asmStatements
  } yield body

  //noinspection MutatorLikeMethodIsParameterless
  def doWhileStatement: P[Seq[ExecutableStatement]] = for {
    body <- "do" ~ !letterOrDigit ~/ AWS ~ executableStatements ~/ AWS
    condition <- "while" ~ !letterOrDigit ~/ HWS ~/ mfExpression(nonStatementLevel)
  } yield Seq(DoWhileStatement(body.toList, Nil, condition))




  def bankDeclaration: P[Option[String]] = ("segment" ~/ AWS ~/ "(" ~/ AWS ~/ identifier ~/ AWS ~/ ")" ~/ AWS).?

  def functionDefinition: P[Seq[DeclarationStatement]] = for {
    p <- position()
    bank <- bankDeclaration
    flags <- flags("asm", "inline", "interrupt", "macro", "noinline", "reentrant", "kernal_interrupt") ~ HWS
    returnType <- identifier ~ SWS
    name <- identifier ~ HWS
    params <- "(" ~/ AWS ~/ (if (flags("asm")) asmParamDefinition else paramDefinition).rep(sep = AWS ~ "," ~/ AWS) ~ AWS ~ ")" ~/ AWS
    addr <- ("@" ~/ HWS ~/ mfExpression(1)).?.opaque("<address>") ~/ AWS
    statements <- (externFunctionBody | (if (flags("asm")) asmStatements else statements).map(l => Some(l))) ~/ Pass
  } yield {
    if (flags("interrupt") && flags("macro")) ErrorReporting.error(s"Interrupt function `$name` cannot be macros", Some(p))
    if (flags("kernal_interrupt") && flags("macro")) ErrorReporting.error(s"Kernal interrupt function `$name` cannot be macros", Some(p))
    if (flags("interrupt") && flags("reentrant")) ErrorReporting.error("Interrupt function `$name` cannot be reentrant", Some(p))
    if (flags("interrupt") && flags("kernal_interrupt")) ErrorReporting.error("Interrupt function `$name` cannot be a Kernal interrupt", Some(p))
    if (flags("macro") && flags("reentrant")) ErrorReporting.error("Reentrant and macro exclude each other", Some(p))
    if (flags("inline") && flags("noinline")) ErrorReporting.error("Noinline and inline exclude each other", Some(p))
    if (flags("macro") && flags("noinline")) ErrorReporting.error("Noinline and macro exclude each other", Some(p))
    if (flags("inline") && flags("macro")) ErrorReporting.error("Macro and inline exclude each other", Some(p))
    if (flags("interrupt") && returnType != "void") ErrorReporting.error("Interrupt function `$name` has to return void", Some(p))
    if (addr.isEmpty && statements.isEmpty) ErrorReporting.error("Extern function `$name` must have an address", Some(p))
    if (statements.isEmpty && !flags("asm") && params.nonEmpty) ErrorReporting.error("Extern non-asm function `$name` cannot have parameters", Some(p))
    if (flags("asm")) validateAsmFunctionBody(p, flags, name, statements)
    Seq(FunctionDeclarationStatement(name, returnType, params.toList,
      bank,
      addr,
      statements,
      flags("macro"),
      if (flags("inline")) Some(true) else if (flags("noinline")) Some(false) else None,
      flags("asm"),
      flags("interrupt"),
      flags("kernal_interrupt"),
      flags("reentrant")).pos(p))
  }

  def validateAsmFunctionBody(p: Position, flags: Set[String], name: String, statements: Option[List[Statement]])

  def importStatement: Parser[Seq[ImportStatement]] = ("import" ~ !letterOrDigit ~/ SWS ~/ identifier).map(x => Seq(ImportStatement(x)))

  def program: Parser[Program] = for {
    _ <- Start ~/ AWS ~/ Pass
    definitions <- (importStatement | arrayDefinition | functionDefinition | variableDefinition(true)).rep(sep = EOL)
    _ <- AWS ~ End
  } yield Program(definitions.flatten.toList)


}
