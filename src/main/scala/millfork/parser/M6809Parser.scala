package millfork.parser

import java.util.Locale

import fastparse.all._
import millfork.{CompilationFlag, CompilationOptions}
import millfork.assembly.Elidability
import millfork.assembly.m6809.{AAccumulatorIndexed, Absolute, BAccumulatorIndexed, DAccumulatorIndexed, DirectPage, Immediate, Indexed, Inherent, InherentA, InherentB, LongRelative, MAddrMode, MLine, MOpcode, NonExistent, PostIncremented, PreDecremented, RegisterSet, Relative, TwoRegisters}
import millfork.env.{ByM6809Register, ParamPassingConvention}
import millfork.node.{ExecutableStatement, Expression, ExpressionStatement, LiteralExpression, M6809AssemblyStatement, M6809Register, ParameterDeclaration, Position, Statement, VariableExpression}
import millfork.output.{MemoryAlignment, NoAlignment, WithinPageAlignment}

/**
  * @author Karol Stasiak
  */
case class M6809Parser(filename: String,
                  input: String,
                  currentDirectory: String,
                  options: CompilationOptions,
                  featureConstants: Map[String, Long]) extends MfParser[MLine](filename, input, currentDirectory, options, featureConstants) {

  import MfParser._

  override def allowIntelHexAtomsInAssembly: Boolean = false

  val appcSimple: P[ParamPassingConvention] = P(("a" | "b" | "d" | "x" | "y" | "u" | "s" | "dp") ~ !letterOrDigit).!.map {
    case "a" => ByM6809Register(M6809Register.A)
    case "b" => ByM6809Register(M6809Register.B)
    case "d" => ByM6809Register(M6809Register.D)
    case "x" => ByM6809Register(M6809Register.X)
    case "y" => ByM6809Register(M6809Register.Y)
    case "u" => ByM6809Register(M6809Register.U)
    case x => log.fatal(s"Unknown assembly parameter passing convention: `$x`")
  }
  override val asmParamDefinition: P[ParameterDeclaration] = for {
    p <- position()
    typ <- identifier ~ SWS
    appc <- appcSimple | appcComplex
  } yield ParameterDeclaration(typ, appc).pos(p)

  def fastAlignmentForArrays: MemoryAlignment = WithinPageAlignment

  def fastAlignmentForFunctions: MemoryAlignment = NoAlignment

  val asmOpcode: P[(MOpcode.Value, Option[MAddrMode])] =
    (position() ~ (letter.rep ~ ("2" | "3").?).! ).map { case (p, o) => MOpcode.lookup(o, Some(p), log) }

  private def mapRegister(r: String): M6809Register.Value = r.toLowerCase(Locale.ROOT) match {
    case "x" => M6809Register.X
    case "y" => M6809Register.Y
    case "s" => M6809Register.S
    case "u" => M6809Register.U
    case "a" => M6809Register.A
    case "b" => M6809Register.B
    case "dp" => M6809Register.DP
    case "pc" => M6809Register.PC
    case "cc" => M6809Register.CC
  }

  val anyRegister: P[M6809Register.Value] = P(("x" | "X" | "y" | "Y" | "s" | "S" | "u" | "U" | "a" | "A" | "b" | "B" | "dp" | "DP" | "cc" | "CC").!).map(mapRegister)

  val indexRegister: P[M6809Register.Value] = P(("x" | "X" | "y" | "Y" | "s" | "S" | "u" | "U" | "pc" | "PC").!).map(mapRegister)

  val asmIndexedAddrMode: P[MAddrMode] = {
    (position() ~ "," ~/ HWS ~/ "-".rep.! ~/ HWS ~/ indexRegister ~/ HWS ~/ "+".rep.!).map {
      case (p, "-", r, "") => PreDecremented(r, 1, indirect = false)
      case (p, "--", r, "") => PreDecremented(r, 2, indirect = false)
      case (p, "", r, "+") => PostIncremented(r, 1, indirect = false)
      case (p, "", r, "++") => PostIncremented(r, 2, indirect = false)
      case (p, "", r, "") => Indexed(r, indirect = false)
      case (p, _, _, _) =>
        log.error("Invalid addressing mode", Some(p))
        Absolute(indirect = false)
    }
  }

  val asmParameterNoIndirectOrImmediate: P[(MAddrMode, Expression)] = {
    for {
      expr <- (asmExpression ~/ HWS).?
      am <- if (expr.isDefined) asmIndexedAddrMode.?.map(_.getOrElse(Absolute(false))) else asmIndexedAddrMode
    } yield {
      (expr, am) match {
        case (Some(VariableExpression("d" | "D")), Indexed(base, false)) => DAccumulatorIndexed(base, indirect = false) -> LiteralExpression(0, 1)
        case (Some(VariableExpression("a" | "A")), Indexed(base, false)) => AAccumulatorIndexed(base, indirect = false) -> LiteralExpression(0, 1)
        case (Some(VariableExpression("b" | "B")), Indexed(base, false)) => BAccumulatorIndexed(base, indirect = false) -> LiteralExpression(0, 1)
        case _ => am -> expr.getOrElse(LiteralExpression(0, 1))
      }
    }
  }

  // TODO: directpage addressing mode syntax
  val asmParameter: P[(MAddrMode, Expression)] = {
    (for {
      _ <- SWS
      pos <- position()
      (a, e) <-
        ("#" ~/ HWS ~/ asmExpression).map(Immediate -> _) |
        ("<" ~/ HWS ~/ asmExpression).map(DirectPage -> _) |
          ("[" ~/ AWS ~/ asmParameterNoIndirectOrImmediate ~/ AWS ~/ "]").map { case (a, e) => a.makeIndirect(pos) -> e } |
          asmParameterNoIndirectOrImmediate
    } yield {
      a -> e
    }).?.map(_.getOrElse(Inherent -> LiteralExpression(0, 1)))
  }


  val asmInstruction: P[ExecutableStatement] = {
    import MOpcode._
    for {
      _ <- !"}"
      elid <- elidable
      position <- position("assembly statement")
      (op, addrModeOverride) <- asmOpcode
      (addrMode, param) <- op match {
        case TFR | EXG =>
          (SWS ~/ anyRegister ~/ HWS ~/ "," ~/ HWS ~/ anyRegister).map { case (a, b) => TwoRegisters(a, b) -> LiteralExpression(0, 1) }
        case PULS | PULU | PSHS | PSHU =>
          SWS ~/ anyRegister.rep(sep = HWS ~ "," ~/ HWS).map(regs => RegisterSet.cleaned(regs.toSet) -> LiteralExpression(0, 1))
        case _ => asmParameter
      }
    } yield {
      val effAddrMode = (addrModeOverride, addrMode) match {
        case (Some(InherentA), Inherent) => InherentA
        case (Some(InherentB), Inherent) => InherentA
        case (Some(InherentA | InherentB), _) =>
          log.error("Inherent accumulator instructions cannot have parameters", Some(position))
          addrMode
        case (Some(LongRelative), Absolute(false)) => LongRelative
        case (Some(LongRelative), _) =>
          log.error("Branching instructions cannot have different addressing modes", Some(position))
          addrMode
        case (None, Absolute(false)) if MOpcode.Branching(op) => Relative
        case (None, _) => addrMode
      }
      M6809AssemblyStatement(op, effAddrMode, param, elid).pos(position)
    }
  }

  // TODO: label and instruction in one line
  val asmLabel: P[ExecutableStatement] = (identifier ~ HWS ~ ":" ~/ HWS).map(l => M6809AssemblyStatement(MOpcode.LABEL, NonExistent, VariableExpression(l), Elidability.Elidable))

  val asmMacro: P[ExecutableStatement] = ("+" ~/ HWS ~/ functionCall(false)).map(ExpressionStatement)

  val asmStatement: P[ExecutableStatement] = (position("assembly statement") ~ P(asmLabel | asmMacro | arrayContentsForAsm | asmInstruction)).map { case (p, s) => s.pos(p) }


  override def validateAsmFunctionBody(p: Position, flags: Set[String], name: String, statements: Option[List[Statement]]): Unit = {
    if (!options.flag(CompilationFlag.BuggyCodeWarning)) return
    statements match {
      case Some(Nil) => log.warn("Assembly function `$name` is empty, did you mean RTS, RTI, JMP, BRA or LBRA?", Some(p))
      case Some(xs) =>
        if (flags("interrupt")) {
          if (xs.exists {
            case M6809AssemblyStatement(MOpcode.RTS, _, _, _) => true
            case _ => false
          }) log.warn("Assembly interrupt function `$name` contains RTS, did you mean RTI?", Some(p))
        } else {
          if (xs.exists {
            case M6809AssemblyStatement(MOpcode.RTI, _, _, _) => true
            case _ => false
          }) log.warn("Assembly non-interrupt function `$name` contains RTI, did you mean RTS?", Some(p))
        }
        if (!name.startsWith("__") && !flags("macro")) {
          xs.last match {
            case M6809AssemblyStatement(MOpcode.RTS, _, _, _) => () // OK
            case M6809AssemblyStatement(MOpcode.RTI, _, _, _) => () // OK
            case M6809AssemblyStatement(MOpcode.JMP, _, _, _) => () // OK
            case M6809AssemblyStatement(MOpcode.BRA, _, _, _) => () // OK
            case _ =>
              val validReturn = if (flags("interrupt")) "RTI" else "RTS"
              log.warn(s"Non-macro assembly function `$name` should end in " + validReturn, Some(p))
          }
        }
      case None => ()
    }
  }
}
