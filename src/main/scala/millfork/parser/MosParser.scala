package millfork.parser

import fastparse.all._
import millfork.assembly.mos.{AddrMode, AssemblyLine, Opcode}
import millfork.env._
import millfork.error.ConsoleLogger
import millfork.node._
import millfork.CompilationOptions

/**
  * @author Karol Stasiak
  */
case class MosParser(filename: String, input: String, currentDirectory: String, options: CompilationOptions, featureConstants: Map[String, Long]) extends MfParser[AssemblyLine](filename, input, currentDirectory, options, featureConstants) {

  import MfParser._

  // TODO: label and instruction in one line
  val asmLabel: P[ExecutableStatement] = (identifier ~ HWS ~ ":" ~/ HWS).map(l => MosAssemblyStatement(Opcode.LABEL, AddrMode.DoesNotExist, VariableExpression(l), elidable = true))

  //  def zeropageAddrModeHint: P[Option[Boolean]] = Pass

  val asmOpcode: P[Opcode.Value] = (position() ~ letter.rep(exactly = 3).! ~ ("_W" | "_w").?.!).map { case (p, suffix, o) => Opcode.lookup(o + suffix, Some(p), log) }

  private val commaX = HWS ~ "," ~ HWS ~ ("X" | "x") ~ HWS
  private val commaY = HWS ~ "," ~ HWS ~ ("Y" | "y") ~ HWS
  private val commaZ = HWS ~ "," ~ HWS ~ ("Z" | "z") ~ HWS
  private val commaS = HWS ~ "," ~ HWS ~ ("S" | "s") ~ HWS

  val farKeyword: P[Unit] = P(("f" | "F") ~ ("a" | "A") ~ ("r" | "R"))

  val asmParameter: P[(AddrMode.Value, Expression)] = {
    (SWS ~ (
      ("##" ~ asmExpression).map(AddrMode.WordImmediate -> _) |
      ("#" ~ asmExpression).map(AddrMode.Immediate -> _) |
        ("(" ~ HWS ~ asmExpression ~ HWS ~ ")" ~ commaY).map(AddrMode.IndexedY -> _) |
        (farKeyword ~ HWS ~ "(" ~ HWS ~ asmExpression ~ HWS ~ ")" ~ commaY).map(AddrMode.LongIndexedY -> _) |
        ("(" ~ HWS ~ asmExpression ~ commaS ~ ")" ~ commaY).map(AddrMode.IndexedSY -> _) |
        ("(" ~ HWS ~ asmExpression ~ HWS ~ ")" ~ commaZ).map(AddrMode.IndexedZ -> _) |
        ("(" ~ HWS ~ asmExpression ~ commaX ~ ")").map(AddrMode.IndexedX -> _) |
        ("(" ~ HWS ~ asmExpression ~ HWS ~ ")").map(AddrMode.Indirect -> _) |
        (farKeyword ~ HWS ~ "(" ~ HWS ~ asmExpression ~ HWS ~ ")").map(AddrMode.LongIndexedZ -> _) |
        (farKeyword ~ HWS ~ asmExpression ~ commaX).map(AddrMode.LongAbsoluteX -> _) |
        (farKeyword ~ HWS ~ asmExpression).map(AddrMode.LongAbsolute -> _) |
        (asmExpression ~ commaS).map(AddrMode.Stack -> _) |
        (asmExpression ~ commaX).map(AddrMode.AbsoluteX -> _) |
        (asmExpression ~ commaY).map(AddrMode.AbsoluteY -> _) |
        asmExpression.map(AddrMode.Absolute -> _)
      )).?.map(_.getOrElse(AddrMode.Implied -> LiteralExpression(0, 1)))
  }

  val asmInstruction: P[ExecutableStatement] = {
    val lineParser: P[(Boolean, Opcode.Value, (AddrMode.Value, Expression))] = !"}" ~ elidable ~/ asmOpcode ~/ asmParameter
    lineParser.map { case (elid, op, param) =>
      (op, param._1) match {
        case (Opcode.SAX, AddrMode.Implied) => MosAssemblyStatement(Opcode.HuSAX, param._1, param._2, elid)
        case (Opcode.SBX, AddrMode.Immediate) => MosAssemblyStatement(Opcode.SBX, param._1, param._2, elid)
        case (Opcode.SAY, AddrMode.AbsoluteX) => MosAssemblyStatement(Opcode.SHY, param._1, param._2, elid)
        case (Opcode.SBX, _) => MosAssemblyStatement(Opcode.SAX, param._1, param._2, elid)
        case (_, AddrMode.Indirect) if op != Opcode.JMP && op != Opcode.JSR => MosAssemblyStatement(op, AddrMode.IndexedZ, param._2, elid)
        case _ => MosAssemblyStatement(op, param._1, param._2, elid)
      }
    }
  }

  val asmMacro: P[ExecutableStatement] = ("+" ~/ HWS ~/ functionCall).map(ExpressionStatement)

  val asmStatement: P[ExecutableStatement] = (position("assembly statement") ~ P(asmLabel | asmMacro | arrayContentsForAsm | asmInstruction)).map { case (p, s) => s.pos(p) } // TODO: macros


  val appcSimple: P[ParamPassingConvention] = P(("xy" | "yx" | "ax" | "ay" | "xa" | "ya" | "stack" | "a" | "x" | "y") ~ !letterOrDigit).!.map {
    case "xy" => ByMosRegister(MosRegister.XY)
    case "yx" => ByMosRegister(MosRegister.YX)
    case "ax" => ByMosRegister(MosRegister.AX)
    case "ay" => ByMosRegister(MosRegister.AY)
    case "xa" => ByMosRegister(MosRegister.XA)
    case "ya" => ByMosRegister(MosRegister.YA)
    case "a" => ByMosRegister(MosRegister.A)
    case "x" => ByMosRegister(MosRegister.X)
    case "y" => ByMosRegister(MosRegister.Y)
    case x => log.fatal(s"Unknown assembly parameter passing convention: `$x`")
  }

  override val asmParamDefinition: P[ParameterDeclaration] = for {
    p <- position()
    typ <- identifier ~ SWS
    appc <- appcSimple | appcComplex
  } yield ParameterDeclaration(typ, appc).pos(p)

  def validateAsmFunctionBody(p: Position, flags: Set[String], name: String, statements: Option[List[Statement]]): Unit = {
    statements match {
      case Some(Nil) => log.warn("Assembly function `$name` is empty, did you mean RTS, RTI or JMP", Some(p))
      case Some(xs) =>
        if (flags("interrupt")) {
          if (xs.exists {
            case MosAssemblyStatement(Opcode.RTS, _, _, _) => true
            case _ => false
          }) log.warn("Assembly interrupt function `$name` contains RTS, did you mean RTI?", Some(p))
        } else {
          if (xs.exists {
            case MosAssemblyStatement(Opcode.RTI, _, _, _) => true
            case _ => false
          }) log.warn("Assembly non-interrupt function `$name` contains RTI, did you mean RTS?", Some(p))
        }
        if (!name.startsWith("__") && !flags("macro")) {
          xs.last match {
            case MosAssemblyStatement(Opcode.RTS, _, _, _) => () // OK
            case MosAssemblyStatement(Opcode.RTI, _, _, _) => () // OK
            case MosAssemblyStatement(Opcode.JMP, _, _, _) => () // OK
            case _ =>
              val validReturn = if (flags("interrupt")) "RTI" else "RTS"
              log.warn(s"Non-macro assembly function `$name` should end in " + validReturn, Some(p))
          }
        }
      case None => ()
    }
  }
}
