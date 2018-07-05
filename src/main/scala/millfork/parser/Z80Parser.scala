package millfork.parser

import java.util.Locale

import fastparse.all._
import fastparse.core
import millfork.CompilationOptions
import millfork.assembly.z80.{ZOpcode, _}
import millfork.env.{ByZRegister, Constant, ParamPassingConvention}
import millfork.error.ErrorReporting
import millfork.node._

/**
  * @author Karol Stasiak
  */
case class Z80Parser(filename: String, input: String, currentDirectory: String, options: CompilationOptions) extends MfParser[ZLine](filename, input, currentDirectory, options) {

  import MfParser._

  private val zero = LiteralExpression(0, 1)

  val appcSimple: P[ParamPassingConvention] = P("a" | "b" | "c" | "d" | "e" | "hl" | "bc" | "de").!.map {
    case "a" => ByZRegister(ZRegister.A)
    case "b" => ByZRegister(ZRegister.B)
    case "c" => ByZRegister(ZRegister.C)
    case "d" => ByZRegister(ZRegister.D)
    case "e" => ByZRegister(ZRegister.E)
    case "hl" => ByZRegister(ZRegister.HL)
    case "bc" => ByZRegister(ZRegister.BC)
    case "de" => ByZRegister(ZRegister.DE)
    case x => ErrorReporting.fatal(s"Unknown assembly parameter passing convention: `$x`")
  }

  override val asmParamDefinition: P[ParameterDeclaration] = for {
    p <- position()
    typ <- identifier ~ SWS
    appc <- appcSimple | appcComplex
  } yield ParameterDeclaration(typ, appc).pos(p)

  // TODO: label and instruction in one line
  val asmLabel: P[ExecutableStatement] = (identifier ~ HWS ~ ":" ~/ HWS).map(l => Z80AssemblyStatement(ZOpcode.LABEL, NoRegisters, VariableExpression(l), elidable = true))

  val asmMacro: P[ExecutableStatement] = ("+" ~/ HWS ~/ functionCall).map(ExpressionStatement)

  private def normalOp8(op: ZOpcode.Value): P[(ZOpcode.Value, ZRegisters, Expression)] = asmExpressionWithParens.map {
    case (VariableExpression("A" | "a"), false) => (op, OneRegister(ZRegister.A), zero)
    case (VariableExpression("B" | "b"), false) => (op, OneRegister(ZRegister.A), zero)
    case (VariableExpression("C" | "c"), false) => (op, OneRegister(ZRegister.A), zero)
    case (VariableExpression("D" | "d"), false) => (op, OneRegister(ZRegister.A), zero)
    case (VariableExpression("E" | "e"), false) => (op, OneRegister(ZRegister.A), zero)
    case (VariableExpression("HL" | "hl"), true) => (op, OneRegister(ZRegister.MEM_HL), zero)
      // TODO: IX/IY
    case (e, false) => (op, OneRegister(ZRegister.IMM_8), e)
  }

  private def modifyOp8(op: ZOpcode.Value): P[(ZOpcode.Value, ZRegisters, Expression)] = asmExpressionWithParens.map {
    case (VariableExpression("A" | "a"), false) => (op, OneRegister(ZRegister.A), zero)
    case (VariableExpression("B" | "b"), false) => (op, OneRegister(ZRegister.A), zero)
    case (VariableExpression("C" | "c"), false) => (op, OneRegister(ZRegister.A), zero)
    case (VariableExpression("D" | "d"), false) => (op, OneRegister(ZRegister.A), zero)
    case (VariableExpression("E" | "e"), false) => (op, OneRegister(ZRegister.A), zero)
    case (VariableExpression("HL" | "hl"), true) => (op, OneRegister(ZRegister.MEM_HL), zero)
      // TODO: IX/IY
    case (e, _) => ErrorReporting.fatal("Invalid parameter for " + op, e.position)
  }

  private val jumpCondition: P[ZRegisters] = (HWS ~ identifier ~ HWS).map {
    case "Z" | "z" => IfFlagSet(ZFlag.Z)
    case "PE" | "pe" => IfFlagSet(ZFlag.P)
    case "C" | "c" => IfFlagSet(ZFlag.C)
    case "M" | "m" => IfFlagSet(ZFlag.S)
    case "NZ" | "nz" => IfFlagClear(ZFlag.Z)
    case "PO" | "po" => IfFlagClear(ZFlag.P)
    case "NC" | "nc" => IfFlagClear(ZFlag.C)
    case "P" | "p" => IfFlagClear(ZFlag.S)
    case _ => ErrorReporting.fatal("Invalid condition flag")
  }

  private val jumpConditionWithComma: P[ZRegisters] = (jumpCondition ~ "," ~/ HWS).?.map (_.getOrElse(NoRegisters))

  val asmInstruction: P[ExecutableStatement] = {
    import ZOpcode._
    for {
      el <- elidable
      opcode: String <- identifier ~/ HWS
      (actualOpcode, registers, param) <- opcode.toUpperCase(Locale.ROOT) match {
        case "RST" => asmExpression.map((RST, NoRegisters, _))
        case "RET" => P("").map(imm(RET)) // TODO: conditionals
        case "CALL" => (jumpCondition~asmExpression).map{case (reg, param) => (CALL, reg, param)}
        case "JP" => (jumpCondition~asmExpression).map{case (reg, param) => (JP, reg, param)}
        case "JR" => (jumpCondition~asmExpression).map{case (reg, param) => (JR, reg, param)}
        case "DJNZ" => asmExpression.map((DJNZ, NoRegisters, _))
        case "CP" => normalOp8(CP)
        case "AND" => normalOp8(AND)
        case "OR" => normalOp8(OR)
        case "XOR" => normalOp8(XOR)
        case "RL" => modifyOp8(RL)
        case "RR" => modifyOp8(RR)
        case "RLC" => modifyOp8(RLC)
        case "RRC" => modifyOp8(RRC)
        case "SLA" => modifyOp8(SLA)
        case "SLL" => modifyOp8(SLL)
        case "SRA" => modifyOp8(SRA)
        case "SRL" => modifyOp8(SRL)
        case _ => ErrorReporting.fatal("Unsupported opcode " + opcode)
      }
    } yield Z80AssemblyStatement(actualOpcode, registers, param, el)
  }

  private def imm(opcode: ZOpcode.Value): Any => (ZOpcode.Value, ZRegisters, Expression) = (_: Any) => {
    (opcode, NoRegisters, zero)
  }

  override val asmStatement: P[ExecutableStatement] = (position("assembly statement") ~ P(asmLabel | asmMacro | arrayContentsForAsm | asmInstruction)).map { case (p, s) => s.pos(p) } // TODO: macros

  override def validateAsmFunctionBody(p: Position, flags: Set[String], name: String, statements: Option[List[Statement]]): Unit = {
    statements match {
      case Some(Nil) => ErrorReporting.warn("Assembly function `$name` is empty, did you mean RET, RETI, RETN or JP", options, Some(p))
      case Some(xs) =>
        if (flags("interrupt")) {
          if (xs.exists {
            case Z80AssemblyStatement(ZOpcode.RET, _, _, _) => true
            case _ => false
          }) ErrorReporting.warn("Assembly interrupt function `$name` contains RET, did you mean RETI/RETN?", options, Some(p))
        } else {
          if (xs.exists {
            case Z80AssemblyStatement(ZOpcode.RETI, _, _, _) => true
            case Z80AssemblyStatement(ZOpcode.RETN, _, _, _) => true
            case _ => false
          }) ErrorReporting.warn("Assembly non-interrupt function `$name` contains RETI or RETN, did you mean RET?", options, Some(p))
        }
        if (!name.startsWith("__") && !flags("macro")) {
          xs.last match {
            case Z80AssemblyStatement(ZOpcode.RET, NoRegisters, _, _) => () // OK
            case Z80AssemblyStatement(ZOpcode.RETN, NoRegisters, _, _) => () // OK
            case Z80AssemblyStatement(ZOpcode.RETI, NoRegisters, _, _) => () // OK
            case Z80AssemblyStatement(ZOpcode.JP, NoRegisters, _, _) => () // OK
            case Z80AssemblyStatement(ZOpcode.JR, NoRegisters, _, _) => () // OK
            case _ =>
              val validReturn = if (flags("interrupt")) "RETI/RETN" else "RET"
              ErrorReporting.warn(s"Non-macro assembly function `$name` should end in " + validReturn, options, Some(p))
          }
        }
      case None => ()
    }
  }
}
