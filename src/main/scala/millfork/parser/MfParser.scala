package millfork.parser

import java.nio.file.{Files, Paths}

import fastparse.all._
import millfork.assembly.{AddrMode, Opcode}
import millfork.env._
import millfork.error.ErrorReporting
import millfork.node._
import millfork.{CompilationOptions, SeparatedList}

/**
  * @author Karol Stasiak
  */
case class MfParser(filename: String, input: String, currentDirectory: String, options: CompilationOptions) {

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

  // TODO: 3-byte types
  def size(value: Int, wordLiteral: Boolean, longLiteral: Boolean): Int =
    if (value > 255 || value < -128 || wordLiteral)
      if (value > 0xffff || longLiteral) 4 else 2
    else 1

  def sign(abs: Int, minus: Boolean): Int = if (minus) -abs else abs

  val decimalAtom: P[LiteralExpression] =
    for {
      p <- position()
      minus <- "-".!.?
      s <- CharsWhileIn("1234567890", min = 1).!.opaque("<decimal digits>") ~ !("x" | "b")
    } yield {
      val abs = Integer.parseInt(s, 10)
      val value = sign(abs, minus.isDefined)
      LiteralExpression(value, size(value, s.length > 3, s.length > 5)).pos(p)
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
      LiteralExpression(value, size(value, s.length > 8, s.length > 16)).pos(p)
    }

  val hexAtom: P[LiteralExpression] =
    for {
      p <- position()
      minus <- "-".!.?
      _ <- P("0x" | "$") ~/ Pass
      s <- CharsWhileIn("1234567890abcdefABCDEF", min = 1).!.opaque("<hex digits>")
    } yield {
      val abs = Integer.parseInt(s, 16)
      val value = sign(abs, minus.isDefined)
      LiteralExpression(value, size(value, s.length > 2, s.length > 4)).pos(p)
    }

  val literalAtom: P[LiteralExpression] = binaryAtom | hexAtom | decimalAtom

  val atom: P[Expression] = P(literalAtom | (position() ~ identifier).map { case (p, i) => VariableExpression(i).pos(p) })

  val mlOperators = List(
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

  def variableDefinition(implicitlyGlobal: Boolean): P[DeclarationStatement] = for {
    p <- position()
    flags <- flags("const", "static", "volatile", "stack") ~ HWS
    typ <- identifier ~ SWS
    name <- identifier ~/ HWS ~/ Pass
    addr <- ("@" ~/ HWS ~/ mlExpression(1)).?.opaque("<address>") ~ HWS
    initialValue <- ("=" ~/ HWS ~/ mlExpression(1)).? ~ HWS
    _ <- &(EOL) ~/ ""
  } yield {
    VariableDeclarationStatement(name, typ,
      global = implicitlyGlobal || flags("static"),
      stack = flags("stack"),
      constant = flags("const"),
      volatile = flags("volatile"),
      initialValue, addr).pos(p)
  }

  val externFunctionBody: P[Option[List[Statement]]] = P("extern" ~/ PassWith(None))

  val paramDefinition: P[ParameterDeclaration] = for {
    p <- position()
    typ <- identifier ~/ SWS ~/ Pass
    name <- identifier ~/ Pass
  } yield {
    ParameterDeclaration(typ, ByVariable(name)).pos(p)
  }

  val appcSimple: P[ParamPassingConvention] = P("xy" | "yx" | "ax" | "ay" | "xa" | "ya" | "stack" | "a" | "x" | "y").!.map {
    case "xy" => ByRegister(Register.XY)
    case "yx" => ByRegister(Register.YX)
    case "ax" => ByRegister(Register.AX)
    case "ay" => ByRegister(Register.AY)
    case "xa" => ByRegister(Register.XA)
    case "ya" => ByRegister(Register.YA)
    case "a" => ByRegister(Register.A)
    case "x" => ByRegister(Register.X)
    case "y" => ByRegister(Register.Y)
    case x => ErrorReporting.fatal(s"Unknown assembly parameter passing convention: `$x`")
  }

  val appcComplex: P[ParamPassingConvention] = P((("const" | "ref").! ~/ AWS).? ~ AWS ~ identifier) map {
    case (None, name) => ByVariable(name)
    case (Some("const"), name) => ByConstant(name)
    case (Some("ref"), name) => ByReference(name)
    case x => ErrorReporting.fatal(s"Unknown assembly parameter passing convention: `$x`")
  }

  val asmParamDefinition: P[ParameterDeclaration] = for {
    p <- position()
    typ <- identifier ~ SWS
    appc <- appcSimple | appcComplex
  } yield ParameterDeclaration(typ, appc).pos(p)

  def arrayListElement: P[List[Expression]] = arrayStringContents | mlExpression(nonStatementLevel).map(List(_))

  def arrayListContents: P[List[Expression]] = ("[" ~/ AWS ~/ arrayListElement.rep(sep = AWS ~ "," ~/ AWS) ~ AWS ~ "]" ~/ Pass).map(_.flatten.toList)

  val doubleQuotedString: P[List[Char]] = P("\"" ~/ CharsWhile(c => c != '\"' && c != '\n' && c != '\r').! ~ "\"").map(_.toList)

  val codec: P[TextCodec] = P(position() ~ identifier).map {
    case (_, "ascii") => TextCodec.Ascii
    case (_, "petscii") => TextCodec.Petscii
    case (_, "pet") => TextCodec.Petscii
    case (_, "scr") => TextCodec.CbmScreencodes
    case (p, x) =>
      ErrorReporting.error(s"Unknown string encoding: `$x`", Some(p))
      TextCodec.Ascii
  }

  def arrayFileContents: P[List[Expression]] = for {
    p <- "file" ~ HWS ~/ "(" ~/ HWS ~/ position()
    filePath <- doubleQuotedString ~/ HWS
    optSlice <- ("," ~/ HWS ~/ literalAtom ~/ HWS ~/ "," ~/ HWS ~/ literalAtom ~/ HWS ~/ Pass).?
    _ <- ")" ~/ Pass
  } yield {
    val data = Files.readAllBytes(Paths.get(currentDirectory, filePath.mkString))
    val slice = optSlice.fold(data) {
      case (start, length) => data.drop(start.value.toInt).take(length.value.toInt)
    }
    slice.map(c => LiteralExpression(c & 0xff, 1)).toList
  }

  def arrayStringContents: P[List[Expression]] = P(position() ~ doubleQuotedString ~/ HWS ~ codec).map {
    case (p, s, co) => s.map(c => LiteralExpression(co.decode(None, c), 1).pos(p))
  }

  def arrayContents: P[List[Expression]] = arrayListContents | arrayFileContents | arrayStringContents

  def arrayDefinition: P[ArrayDeclarationStatement] = for {
    p <- position()
    name <- "array" ~ !letterOrDigit ~/ SWS ~ identifier ~ HWS
    length <- ("[" ~/ AWS ~/ mlExpression(nonStatementLevel) ~ AWS ~ "]").? ~ HWS
    addr <- ("@" ~/ HWS ~/ mlExpression(1)).? ~/ HWS
    contents <- ("=" ~/ HWS ~/ arrayContents).? ~/ HWS
  } yield ArrayDeclarationStatement(name, length, addr, contents).pos(p)

  def tightMlExpression: P[Expression] = P(mlParenExpr | functionCall | mlIndexedExpression | atom) // TODO

  def mlExpression(level: Int): P[Expression] = {
    val allowedOperators = mlOperators.drop(level).flatten

    def inner: P[SeparatedList[Expression, String]] = {
      for {
        head <- tightMlExpression ~/ HWS
        maybeOperator <- StringIn(allowedOperators: _*).!.?
        maybeTail <- maybeOperator.fold[P[Option[List[(String, Expression)]]]](Pass.map(_ => None))(o => (HWS ~/ inner ~/ HWS).map(x2 => Some((o -> x2.head) :: x2.tail)))
      } yield {
        maybeTail.fold[SeparatedList[Expression, String]](SeparatedList.of(head))(t => SeparatedList(head, t))
      }
    }

    def p(list: SeparatedList[Expression, String], level: Int): Expression =
      if (level == mlOperators.length) list.head
      else {
        val xs = list.split(mlOperators(level).toSet(_))
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

  def mlLhsExpressionSimple: P[LhsExpression] = mlIndexedExpression | (position() ~ identifier).map { case (p, n) => VariableExpression(n).pos(p) }

  def mlLhsExpression: P[LhsExpression] = {
    val separated = position() ~ mlLhsExpressionSimple ~ HWS ~ ":" ~/ HWS ~ mlLhsExpressionSimple
    separated.map { case (p, h, l) => SeparateBytesExpression(h, l).pos(p) } | mlLhsExpressionSimple
  }


  def mlParenExpr: P[Expression] = P("(" ~/ AWS ~/ mlExpression(nonStatementLevel) ~ AWS ~/ ")")

  def mlIndexedExpression: P[IndexedExpression] = for {
    p <- position()
    array <- identifier
    index <- HWS ~ "[" ~/ AWS ~/ mlExpression(nonStatementLevel) ~ AWS ~/ "]"
  } yield IndexedExpression(array, index).pos(p)

  def functionCall: P[FunctionCallExpression] = for {
    p <- position()
    name <- identifier
    params <- HWS ~ "(" ~/ AWS ~/ mlExpression(nonStatementLevel).rep(min = 0, sep = AWS ~ "," ~/ AWS) ~ AWS ~/ ")" ~/ ""
  } yield FunctionCallExpression(name, params.toList).pos(p)

  val expressionStatement: P[ExecutableStatement] = mlExpression(0).map(ExpressionStatement)

  val assignmentStatement: P[ExecutableStatement] =
    (position() ~ mlLhsExpression ~ HWS ~ "=" ~/ HWS ~ mlExpression(1)).map {
      case (p, l, r) => Assignment(l, r).pos(p)
    }

  def keywordStatement: P[ExecutableStatement] = P(returnStatement | ifStatement | whileStatement | forStatement | doWhileStatement | inlineAssembly | assignmentStatement)

  def executableStatement: P[ExecutableStatement] = (position() ~ P(keywordStatement | expressionStatement)).map { case (p, s) => s.pos(p) }

  // TODO: label and instruction in one line
  def asmLabel: P[ExecutableStatement] = (identifier ~ HWS ~ ":" ~/ HWS).map(l => AssemblyStatement(Opcode.LABEL, AddrMode.DoesNotExist, VariableExpression(l), elidable = true))

  //  def zeropageAddrModeHint: P[Option[Boolean]] = Pass

  def asmOpcode: P[Opcode.Value] = (position() ~ letter.rep(exactly = 3).!).map { case (p, o) => Opcode.lookup(o, Some(p)) }

  def asmExpression: P[Expression] = (position() ~ NoCut(
    ("<" ~/ HWS ~ mlExpression(mathLevel)).map(e => HalfWordExpression(e, hiByte = false)) |
      (">" ~/ HWS ~ mlExpression(mathLevel)).map(e => HalfWordExpression(e, hiByte = true)) |
      mlExpression(mathLevel)
  )).map { case (p, e) => e.pos(p) }

  val commaX = HWS ~ "," ~ HWS ~ ("X" | "x") ~ HWS
  val commaY = HWS ~ "," ~ HWS ~ ("Y" | "y") ~ HWS

  def asmParameter: P[(AddrMode.Value, Expression)] = {
    (SWS ~ (
      ("#" ~ asmExpression).map(AddrMode.Immediate -> _) |
        ("(" ~ HWS ~ asmExpression ~ HWS ~ ")" ~ commaY).map(AddrMode.IndexedY -> _) |
        ("(" ~ HWS ~ asmExpression ~ commaX ~ ")").map(AddrMode.IndexedX -> _) |
        ("(" ~ HWS ~ asmExpression ~ HWS ~ ")").map(AddrMode.Indirect -> _) |
        (asmExpression ~ commaX).map(AddrMode.AbsoluteX -> _) |
        (asmExpression ~ commaY).map(AddrMode.AbsoluteY -> _) |
        asmExpression.map(AddrMode.Absolute -> _)
      )).?.map(_.getOrElse(AddrMode.Implied -> LiteralExpression(0, 1)))
  }

  def elidable: P[Boolean] = ("?".! ~/ HWS).?.map(_.isDefined)

  def asmInstruction: P[ExecutableStatement] = {
    val lineParser: P[(Boolean, Opcode.Value, (AddrMode.Value, Expression))] = !"}" ~ elidable ~/ asmOpcode ~/ asmParameter
    lineParser.map { case (elid, op, param) =>
      AssemblyStatement(op, param._1, param._2, elid)
    }
  }

  def asmStatement: P[ExecutableStatement] = (position("assembly statement") ~ P(asmLabel | asmInstruction)).map { case (p, s) => s.pos(p) } // TODO: macros

  def statement: P[Statement] = (position() ~ P(keywordStatement | variableDefinition(false) | expressionStatement)).map { case (p, s) => s.pos(p) }

  def asmStatements: P[List[ExecutableStatement]] = ("{" ~/ AWS ~/ asmStatement.rep(sep = EOL ~ !"}" ~/ Pass) ~/ AWS ~/ "}" ~/ Pass).map(_.toList)

  def statements: P[List[Statement]] = ("{" ~/ AWS ~ statement.rep(sep = EOL ~ !"}" ~/ Pass) ~/ AWS ~/ "}" ~/ Pass).map(_.toList)

  def executableStatements: P[Seq[ExecutableStatement]] = "{" ~/ AWS ~/ executableStatement.rep(sep = EOL ~ !"}" ~/ Pass) ~/ AWS ~ "}"

  def returnStatement: P[ExecutableStatement] = ("return" ~ !letterOrDigit ~/ HWS ~ mlExpression(nonStatementLevel).?).map(ReturnStatement)

  def ifStatement: P[ExecutableStatement] = for {
    condition <- "if" ~ !letterOrDigit ~/ HWS ~/ mlExpression(nonStatementLevel)
    thenBranch <- AWS ~/ executableStatements
    elseBranch <- (AWS ~ "else" ~/ AWS ~/ executableStatements).?
  } yield IfStatement(condition, thenBranch.toList, elseBranch.getOrElse(Nil).toList)

  def whileStatement: P[ExecutableStatement] = for {
    condition <- "while" ~ !letterOrDigit ~/ HWS ~/ mlExpression(nonStatementLevel)
    body <- AWS ~ executableStatements
  } yield WhileStatement(condition, body.toList)

  def forDirection: P[ForDirection.Value] =
    ("parallel" ~ HWS ~ "to").!.map(_ => ForDirection.ParallelTo) |
      ("parallel" ~ HWS ~ "until").!.map(_ => ForDirection.ParallelUntil) |
      "until".!.map(_ => ForDirection.Until) |
      "to".!.map(_ => ForDirection.To) |
      ("down" ~/ HWS ~/ "to").!.map(_ => ForDirection.DownTo)

  def forStatement: P[ExecutableStatement] = for {
    identifier <- "for" ~ SWS ~/ identifier ~/ "," ~/ Pass
    start <- mlExpression(nonStatementLevel) ~ HWS ~ "," ~/ HWS ~/ Pass
    direction <- forDirection ~/ HWS ~/ "," ~/ HWS ~/ Pass
    end <- mlExpression(nonStatementLevel)
    body <- AWS ~ executableStatements
  } yield ForStatement(identifier, start, end, direction, body.toList)

  def inlineAssembly: P[ExecutableStatement] = for {
    condition <- "asm" ~ !letterOrDigit ~/ Pass
    body <- AWS ~ asmStatements
  } yield BlockStatement(body)

  //noinspection MutatorLikeMethodIsParameterless
  def doWhileStatement: P[ExecutableStatement] = for {
    body <- "do" ~ !letterOrDigit ~/ AWS ~ executableStatements ~/ AWS
    condition <- "while" ~ !letterOrDigit ~/ HWS ~/ mlExpression(nonStatementLevel)
  } yield DoWhileStatement(body.toList, condition)

  def functionDefinition: P[DeclarationStatement] = for {
    p <- position()
    flags <- flags("asm", "inline", "interrupt", "reentrant") ~ HWS
    returnType <- identifier ~ SWS
    name <- identifier ~ HWS
    params <- "(" ~/ AWS ~/ (if (flags("asm")) asmParamDefinition else paramDefinition).rep(sep = AWS ~ "," ~/ AWS) ~ AWS ~ ")" ~/ AWS
    addr <- ("@" ~/ HWS ~/ mlExpression(1)).?.opaque("<address>") ~/ AWS
    statements <- (externFunctionBody | (if (flags("asm")) asmStatements else statements).map(l => Some(l))) ~/ Pass
  } yield {
    if (flags("interrupt") && flags("inline")) ErrorReporting.error(s"Interrupt function `$name` cannot be inline", Some(p))
    if (flags("interrupt") && flags("reentrant")) ErrorReporting.error("Interrupt function `$name` cannot be reentrant", Some(p))
    if (flags("inline") && flags("reentrant")) ErrorReporting.error("Reentrant and inline exclude each other", Some(p))
    if (flags("interrupt") && returnType != "void") ErrorReporting.error("Interrupt function `$name` has to return void", Some(p))
    if (addr.isEmpty && statements.isEmpty) ErrorReporting.error("Extern function `$name` must have an address", Some(p))
    if (statements.isEmpty && !flags("asm") && params.nonEmpty) ErrorReporting.error("Extern non-asm function `$name` cannot have parameters", Some(p))
    if (flags("asm")) statements match {
      case Some(Nil) => ErrorReporting.warn("Assembly function `$name` is empty, did you mean RTS or RTI", options, Some(p))
      case Some(xs) =>
        if (flags("interrupt")) {
          if (xs.exists {
            case AssemblyStatement(Opcode.RTS, _, _, _) => true
            case _ => false
          }) ErrorReporting.warn("Assembly interrupt function `$name` contains RTS, did you mean RTI?", options, Some(p))
        } else {
          if (xs.exists {
            case AssemblyStatement(Opcode.RTI, _, _, _) => true
            case _ => false
          }) ErrorReporting.warn("Assembly non-interrupt function `$name` contains RTI, did you mean RTS?", options, Some(p))
        }
        if (!flags("inline")) {
          xs.last match {
            case AssemblyStatement(Opcode.RTS, _, _, _) => () // OK
            case AssemblyStatement(Opcode.RTI, _, _, _) => () // OK
            case AssemblyStatement(Opcode.JMP, _, _, _) => () // OK
            case _ =>
              val validReturn = if (flags("interrupt")) "RTI" else "RTS"
              ErrorReporting.warn(s"Non-inline assembly function `$name` should end in " + validReturn, options, Some(p))
          }
        }
      case None => ()
    }
    FunctionDeclarationStatement(name, returnType, params.toList,
      addr,
      statements,
      flags("inline"),
      flags("asm"),
      flags("interrupt"),
      flags("reentrant")).pos(p)
  }

  def importStatement: Parser[ImportStatement] = ("import" ~ !letterOrDigit ~/ SWS ~/ identifier).map(ImportStatement)

  def program: Parser[Program] = for {
    _ <- Start ~/ AWS ~/ Pass
    definitions <- (importStatement | arrayDefinition | functionDefinition | variableDefinition(true)).rep(sep = EOL)
    _ <- AWS ~ End
  } yield Program(definitions.toList)


}
