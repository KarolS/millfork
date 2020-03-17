package millfork.parser

import fastparse.all._
import millfork.assembly.mos.{AddrMode, AssemblyLine, Opcode, OpcodeClasses}
import millfork.env._
import millfork.node._
import millfork.CompilationOptions
import millfork.assembly.Elidability
import millfork.output.{MemoryAlignment, WithinPageAlignment}

/**
  * @author Karol Stasiak
  */
case class MosParser(filename: String, input: String, currentDirectory: String, options: CompilationOptions, featureConstants: Map[String, Long]) extends MfParser[AssemblyLine](filename, input, currentDirectory, options, featureConstants) {

  import MfParser._

  def allowIntelHexAtomsInAssembly: Boolean = false

  def fastAlignmentForArrays: MemoryAlignment = WithinPageAlignment
  def fastAlignmentForFunctions: MemoryAlignment = WithinPageAlignment

  // TODO: label and instruction in one line
  val asmLabel: P[ExecutableStatement] = (identifier ~ HWS ~ ":" ~/ HWS).map(l => MosAssemblyStatement(Opcode.LABEL, AddrMode.DoesNotExist, VariableExpression(l), Elidability.Elidable))

  //  def zeropageAddrModeHint: P[Option[Boolean]] = Pass

  val asmOpcode: P[Opcode.Value] = (position() ~ mosOpcodeLetter.rep(exactly = 3).! ~ octalDigit.?.! ~ ("_W" | "_w").?.!).map { case (p, bitNo, suffix, o) => Opcode.lookup(o + bitNo + suffix, Some(p), log) }

  private val commaX = HWS ~ "," ~ HWS ~ ("X" | "x") ~ HWS
  private val commaY = HWS ~ "," ~ HWS ~ ("Y" | "y") ~ HWS
  private val commaZ = HWS ~ "," ~ HWS ~ ("Z" | "z") ~ HWS
  private val commaS = HWS ~ "," ~ HWS ~ ("S" | "s") ~ HWS

  val farKeyword: P[Unit] = P(("f" | "F") ~ ("a" | "A") ~ ("r" | "R"))

  def isObviouslyZeropage(e: Expression): Boolean = e match {
    case LiteralExpression(value, size) => size == 1 && value >= 0 && value <= 0xff
    case FunctionCallExpression("lo" | "hi", _) => true
    case FunctionCallExpression("|" | "&" | "^", exprs) => exprs.forall(isObviouslyZeropage)
    case _ => false
  }

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
        (asmExpression ~ commaX).map { param =>
          if (isObviouslyZeropage(param)) {
            AddrMode.ZeroPageX -> param
          } else {
            AddrMode.AbsoluteX -> param
          }
        } |
        (asmExpression ~ commaY).map { param =>
          if (isObviouslyZeropage(param)) {
            AddrMode.ZeroPageY -> param
          } else {
            AddrMode.AbsoluteY -> param
          }
        } |
        // automatic zero page is handled elsewhere
        asmExpression.map(AddrMode.Absolute -> _)
      )).?.map(_.getOrElse(AddrMode.Implied -> LiteralExpression(0, 1)))
  }

  val asmInstruction: P[ExecutableStatement] = {
    import Opcode._
    for {
      elid <- !"}" ~ elidable
      position <- position("assembly statement")
      op <- asmOpcode ~/ Pass
      param <- op match {
        case op if OpcodeClasses.SingleBitBranch(op) =>
          (HWS ~ asmExpression ~ HWS ~ "," ~/ HWS ~ asmExpression).map{ x =>
            AddrMode.ZeroPageWithRelative -> FunctionCallExpression("byte_and_pointer$", List(x._1, x._2))
          }
        case op if OpcodeClasses.HudsonTransfer(op) =>
          (HWS ~ asmExpression ~ HWS ~ "," ~/ HWS ~ asmExpression ~ HWS ~ "," ~/ HWS ~ asmExpression).map{ x =>
            AddrMode.TripleAbsolute -> FunctionCallExpression("hudson_transfer$", List(x._1, x._2, x._3))
          }
        case Opcode.TST => (HWS ~ "#" ~/ HWS ~ asmExpression ~ HWS ~ "," ~/ HWS ~ asmExpression ~ commaX.!.?).map { x =>
          (if (x._3.isDefined) AddrMode.ImmediateWithAbsoluteX else AddrMode.ImmediateWithAbsolute )-> FunctionCallExpression("byte_and_pointer$", List(x._1, x._2))
        }
        case _ => asmParameter
      }
    } yield {
      ((op, param._1) match {
        case (Opcode.SAX, AddrMode.Implied) => MosAssemblyStatement(Opcode.HuSAX, param._1, param._2, elid)
        case (Opcode.SBX, AddrMode.Immediate) => MosAssemblyStatement(Opcode.SBX, param._1, param._2, elid)
        case (Opcode.SAY, AddrMode.AbsoluteX) => MosAssemblyStatement(Opcode.SHY, param._1, param._2, elid)
        case (Opcode.SAX, AddrMode.AbsoluteY) => MosAssemblyStatement(Opcode.SAX, AddrMode.ZeroPageY, param._2, elid)
        case (Opcode.ASR, AddrMode.Absolute) => MosAssemblyStatement(Opcode.ASR, AddrMode.ZeroPage, param._2, elid)
        case (Opcode.ASR, AddrMode.AbsoluteX) => MosAssemblyStatement(Opcode.ASR, AddrMode.ZeroPageX, param._2, elid)
        case (Opcode.SBX, _) => MosAssemblyStatement(Opcode.SAX, param._1, param._2, elid)
        case (_, AddrMode.ZeroPageX) if !OpcodeClasses.SupportsZeroPageX(op) => MosAssemblyStatement(op, AddrMode.AbsoluteX, param._2, elid)
        case (_, AddrMode.ZeroPageY) if !OpcodeClasses.SupportsZeroPageY(op) => MosAssemblyStatement(op, AddrMode.AbsoluteY, param._2, elid)
        case (_, AddrMode.Absolute) if OpcodeClasses.SingleBit(op) => MosAssemblyStatement(op, AddrMode.ZeroPage, param._2, elid)
        case (_, AddrMode.Indirect) if op != Opcode.JMP && op != Opcode.JSR => MosAssemblyStatement(op, AddrMode.IndexedZ, param._2, elid)
        case _ => MosAssemblyStatement(op, param._1, param._2, elid)
      }).pos(position)
    }
  }

  val asmMacro: P[ExecutableStatement] = ("+" ~/ HWS ~/ functionCall(false)).map(ExpressionStatement)

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
