package millfork.parser

import java.util.Locale

import fastparse.all.{parserApi, _}
import fastparse.core
import millfork.assembly.{Elidability, z80}
import millfork.{CompilationFlag, CompilationOptions, node}
import millfork.assembly.z80.{ZOpcode, _}
import millfork.env.{ByZRegister, Constant, ParamPassingConvention}
import millfork.error.ConsoleLogger
import millfork.node._
import millfork.output.{MemoryAlignment, NoAlignment, WithinPageAlignment}

/**
  * @author Karol Stasiak
  */
case class Z80Parser(filename: String,
                     input: String,
                     currentDirectory: String,
                     options: CompilationOptions,
                     featureConstants: Map[String, Long],
                     useIntelSyntax: Boolean) extends MfParser[ZLine](filename, input, currentDirectory, options, featureConstants) {

  import MfParser._

  def allowIntelHexAtomsInAssembly: Boolean = useIntelSyntax

  def fastAlignmentForArrays: MemoryAlignment = WithinPageAlignment
  def fastAlignmentForFunctions: MemoryAlignment = NoAlignment

  private val zero = LiteralExpression(0, 1)

  val appcSimple: P[ParamPassingConvention] = (P("hl" | "bc" | "de" | "a" | "b" | "c" | "d" | "e" | "h" | "l").! ~ !letterOrDigit).map {
    case "a" => ByZRegister(ZRegister.A)
    case "b" => ByZRegister(ZRegister.B)
    case "c" => ByZRegister(ZRegister.C)
    case "d" => ByZRegister(ZRegister.D)
    case "e" => ByZRegister(ZRegister.E)
    case "h" => ByZRegister(ZRegister.H)
    case "l" => ByZRegister(ZRegister.L)
    case "hl" => ByZRegister(ZRegister.HL)
    case "bc" => ByZRegister(ZRegister.BC)
    case "de" => ByZRegister(ZRegister.DE)
    case x => log.fatal(s"Unknown assembly parameter passing convention: `$x`")
  }

  override val asmParamDefinition: P[ParameterDeclaration] = for {
    p <- position()
    typ <- identifier ~ SWS
    appc <- appcSimple | appcComplex
  } yield ParameterDeclaration(typ, appc).pos(p)

  // TODO: label and instruction in one line
  val asmLabel: P[ExecutableStatement] = (identifier ~ HWS ~ ":" ~/ HWS).map(l => Z80AssemblyStatement(ZOpcode.LABEL, NoRegisters, None, VariableExpression(l), elidability = Elidability.Elidable))

  val asmMacro: P[ExecutableStatement] = ("+" ~/ HWS ~/ functionCall(false)).map(ExpressionStatement)

  private val toRegister: Map[String, ZRegister.Value] = Map(
    "A" -> ZRegister.A, "a" -> ZRegister.A,
    "B" -> ZRegister.B, "b" -> ZRegister.B,
    "C" -> ZRegister.C, "c" -> ZRegister.C,
    "D" -> ZRegister.D, "d" -> ZRegister.D,
    "E" -> ZRegister.E, "e" -> ZRegister.E,
    "H" -> ZRegister.H, "h" -> ZRegister.H,
    "L" -> ZRegister.L, "l" -> ZRegister.L,
    "HL" -> ZRegister.HL, "hl" -> ZRegister.HL,
    "AF" -> ZRegister.AF, "af" -> ZRegister.AF,
    "BC" -> ZRegister.BC, "bc" -> ZRegister.BC,
    "DE" -> ZRegister.DE, "de" -> ZRegister.DE,
    "IX" -> ZRegister.IX, "ix" -> ZRegister.IX,
    "IY" -> ZRegister.IY, "iy" -> ZRegister.IY,
    "SP" -> ZRegister.SP, "sp" -> ZRegister.SP,
  )

  private val toIntelRegister8: Map[String, ZRegister.Value] = Map(
    "A" -> ZRegister.A, "a" -> ZRegister.A,
    "B" -> ZRegister.B, "b" -> ZRegister.B,
    "C" -> ZRegister.C, "c" -> ZRegister.C,
    "D" -> ZRegister.D, "d" -> ZRegister.D,
    "E" -> ZRegister.E, "e" -> ZRegister.E,
    "H" -> ZRegister.H, "h" -> ZRegister.H,
    "L" -> ZRegister.L, "l" -> ZRegister.L,
    "M" -> ZRegister.L, "m" -> ZRegister.MEM_HL,
  )

  private val toIntelRegister16: Map[String, ZRegister.Value] = Map(
    "H" -> ZRegister.HL, "h" -> ZRegister.HL,
    "PSW" -> ZRegister.AF, "psw" -> ZRegister.AF,
    "B" -> ZRegister.BC, "b" -> ZRegister.BC,
    "D" -> ZRegister.DE, "d" -> ZRegister.DE,
    "SP" -> ZRegister.SP, "sp" -> ZRegister.SP,
  )

  private def param(allowAbsolute: Boolean, allowRI: Boolean = false, allowFfc: Boolean = false): P[(ZRegister.Value, Option[Expression])] = asmExpressionWithParens.map {
    case (VariableExpression("R" | "r"), false) if allowRI => (ZRegister.R, None)
    case (VariableExpression("I" | "i"), false) if allowRI => (ZRegister.I, None)
    case (VariableExpression(r), false) if toRegister.contains(r)=> (toRegister(r), None)
    case (SumExpression(List(
    (false, LiteralExpression(0xff00, _)),
    (false, VariableExpression("C" | "c"))
    ), false), true) if allowFfc => (ZRegister.MEM_BC, None) // MEM_BC is a placeholder here for ($FF00 + C)
    case (VariableExpression("C" | "c"), true) if allowFfc => (ZRegister.MEM_BC, None) // MEM_BC is a placeholder here for ($FF00 + C)
    case (VariableExpression("HL" | "hl"), true) => (ZRegister.MEM_HL, None)
    case (VariableExpression("BC" | "bc"), true) => (ZRegister.MEM_BC, None)
    case (VariableExpression("DE" | "de"), true) => (ZRegister.MEM_DE, None)
    case (FunctionCallExpression("IX" | "ix", List(o)), _) => (ZRegister.MEM_IX_D, Some(o))
    case (FunctionCallExpression("IY" | "iy", List(o)), _) => (ZRegister.MEM_IY_D, Some(o))
    case (e, true) if allowAbsolute => (ZRegister.MEM_ABS_8, Some(e))
    case (e, _) => (ZRegister.IMM_8, Some(e))
  }

  private def intel8: P[ZRegister.Value] = asmExpression.map {
    case VariableExpression(r) if toIntelRegister8.contains(r) => toIntelRegister8(r)
    case x =>
      options.log.error("Invalid operand", x.position)
      ZRegister.A
  }

  private def intel16(allowPsw: Boolean): P[ZRegister.Value] = asmExpression.map {
    case x@VariableExpression(r) if toIntelRegister16.contains(r) =>
      val result = toIntelRegister16(r)
      if (result == ZRegister.AF && !allowPsw) options.log.error("Invalid operand", x.position)
      if (result == ZRegister.SP && allowPsw) options.log.error("Invalid operand", x.position)
      result
    case x =>
      log.error("Invalid operand", x.position)
      ZRegister.HL
  }

  def mapOne8Register(op: ZOpcode.Value)(param: (ZRegister.Value, Option[Expression])): (ZOpcode.Value, OneRegister, Option[Expression], Expression) = param match {
    case (reg@(ZRegister.MEM_IX_D | ZRegister.MEM_IY_D), Some(e)) => (op, OneRegister(reg), Some(e), zero)
    case (reg, addr) => (op, OneRegister(reg), None, addr.getOrElse(zero))
  }

  def one8Register(op: ZOpcode.Value): P[(ZOpcode.Value, OneRegister, Option[Expression], Expression)] = param(allowAbsolute = false).map{
    case (reg@(ZRegister.MEM_IX_D | ZRegister.MEM_IY_D), Some(e)) => (op, OneRegister(reg), Some(e), zero)
    case (reg, addr) => (op, OneRegister(reg), None, addr.getOrElse(zero))
  }

  def one8Or16Register(op8: ZOpcode.Value, op16: ZOpcode.Value): P[(ZOpcode.Value, OneRegister, Option[Expression], Expression)] = param(allowAbsolute = false).map{
    case (reg@(ZRegister.MEM_IX_D | ZRegister.MEM_IY_D), Some(e)) => (op8, OneRegister(reg), Some(e), zero)
    case (reg@(ZRegister.HL | ZRegister.DE | ZRegister.AF | ZRegister.SP | ZRegister.BC | ZRegister.IX | ZRegister.IY), addr) => (op16, OneRegister(reg), None, addr.getOrElse(zero))
    case (reg, addr) => (op8, OneRegister(reg), None, addr.getOrElse(zero))
  }

  def one16Register(op: ZOpcode.Value): P[(ZOpcode.Value, OneRegister, Option[Expression], Expression)] = param(allowAbsolute = false).map{
    case (reg@(ZRegister.MEM_IX_D | ZRegister.MEM_IY_D), Some(e)) => (op, OneRegister(reg), Some(e), zero)
    case (ZRegister.MEM_ABS_8, addr) => (op, OneRegister(ZRegister.MEM_ABS_16), None, addr.getOrElse(zero))
    case (ZRegister.IMM_8, addr) => (op, OneRegister(ZRegister.IMM_16), None, addr.getOrElse(zero))
    case (reg, addr) => (op, OneRegister(reg), None, addr.getOrElse(zero))
  }

  private val jumpCondition: P[ZRegisters] = (HWS ~ (
    "NZ" | "nz" | "nc" | "NC" | "NV" | "nv" |
      "PO" | "po" | "PE" | "pe" |
      "m" | "M" | "p" | "P" |
      "c" | "C" | "Z" | "z" | "V" | "v").! ~ HWS).map {
    case "Z" | "z" => IfFlagSet(ZFlag.Z)
    case "PE" | "pe" | "v" | "V" => IfFlagSet(ZFlag.P)
    case "C" | "c" => IfFlagSet(ZFlag.C)
    case "M" | "m" => IfFlagSet(ZFlag.S)
    case "NZ" | "nz" => IfFlagClear(ZFlag.Z)
    case "PO" | "po" | "NV" | "nv" => IfFlagClear(ZFlag.P)
    case "NC" | "nc" => IfFlagClear(ZFlag.C)
    case "P" | "p" => IfFlagClear(ZFlag.S)
    case _ => NoRegisters // shouldn't happen
  }

  private def is16Bit(r: ZRegister.Value): Boolean = r match {
    case ZRegister.HL => true
    case ZRegister.BC => true
    case ZRegister.DE => true
    case ZRegister.IX => true
    case ZRegister.IY => true
    case ZRegister.SP => true
    case ZRegister.AF => true
    case ZRegister.MEM_ABS_16 => true
    case ZRegister.IMM_16 => true
    case _ => false
  }

  private def merge(op8: ZOpcode.Value, op16: ZOpcode.Value, skipTargetA: Boolean)
                   (params: (ZRegister.Value, Option[Expression], ZRegister.Value, Option[Expression]))
  : (ZOpcode.Value, ZRegisters, Option[Expression], Expression) = params match {
    case (ZRegister.A, _, s, e) if skipTargetA => mapOne8Register(op8)(s -> e)

    case (tr@(ZRegister.MEM_IX_D | ZRegister.MEM_IY_D), Some(o), sr@(ZRegister.MEM_ABS_8 | ZRegister.IMM_8), Some(v)) =>
      (op8, TwoRegisters(tr, sr), Some(o), v)
    case (tr@(ZRegister.MEM_ABS_8 | ZRegister.IMM_8), Some(v), sr@(ZRegister.MEM_IX_D | ZRegister.MEM_IY_D), Some(o)) =>
      (op8, TwoRegisters(tr, sr), Some(o), v)

    case (tr@(ZRegister.MEM_IX_D | ZRegister.MEM_IY_D), Some(o), sr, _) =>
      (op8, TwoRegisters(tr, sr), Some(o), zero)
    case (tr, _, sr@(ZRegister.MEM_IX_D | ZRegister.MEM_IY_D), Some(o)) =>
      (op8, TwoRegisters(tr, sr), Some(o), zero)

    case (tr, _, ZRegister.MEM_ABS_8, Some(v)) if is16Bit(tr) =>
      (op16, TwoRegisters(tr, ZRegister.MEM_ABS_16), None, v)
    case (tr, _, ZRegister.IMM_8, Some(v)) if is16Bit(tr) =>
      (op16, TwoRegisters(tr, ZRegister.IMM_16), None, v)
    case (ZRegister.MEM_ABS_8, Some(v), sr, _) if is16Bit(sr) =>
      (op16, TwoRegisters(ZRegister.MEM_ABS_16, sr), None, v)
    case (ZRegister.IMM_8, Some(v), sr, _) if is16Bit(sr) =>
      (op16, TwoRegisters(ZRegister.IMM_16, sr), None, v)

    case (t, Some(v), s, None) =>
      (op8, TwoRegisters(t, s), None, v)
    case (t, None, s, Some(v)) =>
      (op8, TwoRegisters(t, s), None, v)
    case (t, None, s, None) =>
      (if (is16Bit(t)) op16 else op8, TwoRegisters(t, s), None, zero)
    case _ => ???
  }

  private val jumpConditionWithComma: P[ZRegisters] = (jumpCondition ~ "," ~/ HWS).?.map (_.getOrElse(NoRegisters))
  private val jumpConditionWithoutComma: P[ZRegisters] = (jumpCondition ~/ HWS).?.map (_.getOrElse(NoRegisters))

  val zilogAsmInstruction: P[ExecutableStatement] = {
    import ZOpcode._
    for {
      el <- elidable
      pos <- position()
      opcode: String <- identifier ~/ HWS
      tuple4: (ZOpcode.Value, ZRegisters, Option[Expression], Expression) <- opcode.toUpperCase(Locale.ROOT) match {
        case "RST" => asmExpression.map((RST, NoRegisters, None, _))
        case "IM" => asmExpression.map((IM, NoRegisters, None, _))
        case "EI" => imm(EI)
        case "DI" => imm(DI)
        case "HLT" => imm(HALT)
        case "HALT" => imm(HALT)
        case "STOP" => imm(STOP)

        case "RETN" => imm(RETN)
        case "RETI" => imm(RETI)
        case "RET" => P(jumpConditionWithoutComma).map((RET, _, None, zero))
        case "CALL" => (jumpConditionWithComma ~ asmExpression).map { case (reg, param) => (CALL, reg, None, param) }
        case "JP" => (jumpConditionWithComma ~ param(allowAbsolute = true)).map {
          case (NoRegisters, (ZRegister.MEM_ABS_8, Some(VariableExpression("ix" | "IX")))) =>
            (JP, OneRegister(ZRegister.IX), None, zero)
          case (NoRegisters, (ZRegister.MEM_ABS_8, Some(VariableExpression("iy" | "IY")))) =>
            (JP, OneRegister(ZRegister.IY), None, zero)
          case (NoRegisters, (ZRegister.MEM_HL, _)) =>
            (JP, OneRegister(ZRegister.HL), None, zero)
          case (cond, (ZRegister.MEM_ABS_8 | ZRegister.IMM_8, Some(param))) =>
            (JP, cond, None, param)
          case _ =>
            log.error("Invalid parameters for JP", Some(pos))
            (NOP, NoRegisters, None, zero)
        }
        case "JR" => (jumpConditionWithComma ~ asmExpression).map{case (reg, param) => (JR, reg, None, param)}
        case "DJNZ" => asmExpression.map((DJNZ, NoRegisters, None, _))

        case "CP" => one8Register(CP)
        case "AND" => one8Register(AND)
        case "OR" => one8Register(OR)
        case "XOR" => one8Register(XOR)
        case "SUB" => one8Register(SUB)
        case "DEC" => one8Or16Register(DEC, DEC_16)
        case "INC" => one8Or16Register(INC, INC_16)

        case "RLA" => imm(RLA)
        case "RRA" => imm(RRA)
        case "RLCA" => imm(RLCA)
        case "RRCA" => imm(RRCA)
        case "RL" => one8Register(RL)
        case "RR" => one8Register(RR)
        case "RLC" => one8Register(RLC)
        case "RRC" => one8Register(RRC)
        case "SLA" => one8Register(SLA)
        case "SLL" => one8Register(SLL)
        case "SRA" => one8Register(SRA)
        case "SRL" => one8Register(SRL)
        case "SWAP" => one8Register(SWAP)

        case "BIT" => (param(false) ~ HWS ~ position("comma").map(_ => ()) ~ "," ~/ HWS ~ param(false)).map {
          case (ZRegister.IMM_8, Some(LiteralExpression(n, _)), (r2, e2))
            if n >= 0 && n <= 7 && r2 != ZRegister.MEM_BC && r2 != ZRegister.MEM_DE =>
            (ZOpcodeClasses.BIT_seq(n.toInt), OneRegister(r2), e2, zero)
          case _ =>
            log.error("Invalid parameters for BIT", Some(pos))
            (NOP, NoRegisters, None, zero)
        }
        case "SET" => (param(false) ~ HWS ~ position("comma").map(_ => ()) ~ "," ~/ HWS ~ param(false)).map {
          case (ZRegister.IMM_8, Some(LiteralExpression(n, _)), (r2, e2))
            if n >= 0 && n <= 7 && r2 != ZRegister.MEM_BC && r2 != ZRegister.MEM_DE =>
            (ZOpcodeClasses.SET_seq(n.toInt), OneRegister(r2), e2, zero)
          case _ =>
            log.error("Invalid parameters for SET", Some(pos))
            (NOP, NoRegisters, None, zero)
        }
        case "RES" => (param(false) ~ HWS ~ position("comma").map(_ => ()) ~ "," ~/ HWS ~ param(false)).map {
          case (ZRegister.IMM_8, Some(LiteralExpression(n, _)), (r2, e2))
            if n >= 0 && n <= 7 && r2 != ZRegister.MEM_BC && r2 != ZRegister.MEM_DE =>
            (ZOpcodeClasses.RES_seq(n.toInt), OneRegister(r2), e2, zero)
          case _ =>
            log.error("Invalid parameters for RES", Some(pos))
            (NOP, NoRegisters, None, zero)
        }

        case "SCF" => imm(SCF)
        case "CCF" => imm(CCF)
        case "CPL" => imm(CPL)
        case "DAA" => imm(DAA)
        case "EXX" => imm(EXX)
        case "NOP" => imm(NOP)
        case "NEG" => imm(NEG)

        case "LDI" => imm(LDI) // TODO: Gameboy has a different LDI
        case "LDD" => imm(LDD) // TODO: Gameboy has a different LDD
        case "LDIR" => imm(LDIR)
        case "LDDR" => imm(LDDR)
        case "CPI" => imm(CPI)
        case "CPD" => imm(CPD)
        case "CPIR" => imm(CPIR)
        case "CPDR" => imm(CPDR)
        case "INI" => imm(INI)
        case "IND" => imm(IND)
        case "INIR" => imm(INIR)
        case "INDR" => imm(INDR)
        case "OUTI" => imm(OUTI)
        case "OUTD" => imm(OUTD)
        case "OUTIR" => imm(OUTIR)
        case "OUTDR" => imm(OUTDR)
        case "OTIR" => imm(OUTIR)
        case "OTDR" => imm(OUTDR)
        case "RLD" => imm(RLD)
        case "RRD" => imm(RRD)

        case "PUSH" => one16Register(PUSH)
        case "POP" => one16Register(POP)

        case "IN" => (asmExpressionWithParens ~ HWS ~ position("comma").map(_ => ()) ~ "," ~/ HWS ~ asmExpressionWithParens).map { p: (Expression, Boolean, (Expression, Boolean)) =>
          p match {
            case (VariableExpression(r), false, (VariableExpression("C" | "c"), true))
              if toRegister.contains(r) =>
              (IN_C, OneRegister(toRegister(r)), None, zero)
            case (VariableExpression(r), false, (port, true))
              if toRegister.contains(r) =>
              (IN_IMM, OneRegister(toRegister(r)), None, port)
            case _ =>
              log.error("Invalid parameters for IN", Some(pos))
              (NOP, NoRegisters, None, zero)
          }
        }
        case "OUT" => (asmExpressionWithParens ~ HWS ~ position("comma").map(_ => ()) ~ "," ~/ HWS ~ asmExpressionWithParens).map { p: (Expression, Boolean, (Expression, Boolean)) =>
          p match {
            case (VariableExpression("C" | "c"), true, (VariableExpression(r), false))
              if toRegister.contains(r) =>
              (OUT_C, OneRegister(toRegister(r)), None, zero)
            case (port, true, (VariableExpression(r), false))
              if toRegister.contains(r) =>
              (OUT_IMM, OneRegister(toRegister(r)), None, port)
            case _ =>
              log.error("Invalid parameters for OUT", Some(pos))
              (NOP, NoRegisters, None, zero)
          }
        }
        case "EX" => (asmExpressionWithParens ~ HWS ~ position("comma").map(_ => ()) ~ "," ~/ HWS ~ asmExpressionWithParensOrApostrophe).map { p: (Expression, Boolean, (Expression, Boolean)) =>
          p match {
            case (VariableExpression("AF" | "af"), false, (VariableExpression("AF" | "af"), _)) =>
              (EX_AF_AF, NoRegisters, None, zero)
            case (VariableExpression("DE" | "de"), false, (VariableExpression("HL" | "hl"), false)) =>
              (EX_DE_HL, NoRegisters, None, zero)
            case (VariableExpression("HL" | "hl"), false, (VariableExpression("DE" | "de"), false)) =>
              (EX_DE_HL, NoRegisters, None, zero)
            case (VariableExpression("SP" | "sp"), true, (VariableExpression(r), false))
              if toRegister.contains(r) =>
              (EX_SP, OneRegister(toRegister(r)), None, zero)
            case (VariableExpression(r), false, (VariableExpression("SP" | "sp"), true))
              if toRegister.contains(r) =>
              (EX_SP, OneRegister(toRegister(r)), None, zero)
            case _ =>
              log.error("Invalid parameters for EX", Some(pos))
              (NOP, NoRegisters, None, zero)
          }
        }

        case "LDH" => (param(allowAbsolute = true, allowFfc = true) ~ HWS ~ position("comma").map(_ => ()) ~ "," ~/ HWS ~ param(allowAbsolute = true, allowFfc = true)).map {
          case (ZRegister.MEM_ABS_8, Some(expr), (ZRegister.A, None)) => (LDH_DA, NoRegisters, None, expr)
          case (ZRegister.A, None, (ZRegister.MEM_ABS_8, Some(expr))) => (LDH_AD, NoRegisters, None, expr)
          case (ZRegister.A, None, (ZRegister.MEM_BC, None)) => (LDH_AC, NoRegisters, None, zero)
          case (ZRegister.MEM_BC, None, (ZRegister.A, None)) => (LDH_CA, NoRegisters, None, zero)
          case _ =>
            log.error("Invalid parameters for LDH", Some(pos))
            (NOP, NoRegisters, None, zero)
        }
        case "LD" => (param(allowAbsolute = true, allowRI = true) ~ HWS ~ position("comma").map(_ => ()) ~ "," ~/ HWS ~ param(allowAbsolute = true, allowRI = true)).map {
          case (ZRegister.HL, None, (ZRegister.IMM_8 | ZRegister.IMM_16, Some(SumExpression((false, VariableExpression("sp" | "SP")) :: offset, false))))
            if options.flags(CompilationFlag.EmitSharpOpcodes) =>
            (LD_HLSP, OneRegister(ZRegister.IMM_8), None, offset match {
              case List((false, expr)) => expr
              case (true, _) :: _ => SumExpression((false -> LiteralExpression(0, 1)) :: offset, decimal = false)
              case _ => SumExpression(offset, decimal = false)
            })
          case (ZRegister.A, None, (ZRegister.MEM_ABS_8, Some(VariableExpression("HLI" | "hli"))))
            if options.flags(CompilationFlag.EmitSharpOpcodes) =>
            (LD_AHLI, NoRegisters, None, zero)
          case (ZRegister.A, None, (ZRegister.MEM_ABS_8, Some(VariableExpression("HLD" | "hld"))))
            if options.flags(CompilationFlag.EmitSharpOpcodes) =>
            (LD_AHLD, NoRegisters, None, zero)
          case (ZRegister.MEM_ABS_8, Some(VariableExpression("HLI" | "hli")), (ZRegister.A, None))
            if options.flags(CompilationFlag.EmitSharpOpcodes) =>
            (LD_HLIA, NoRegisters, None, zero)
          case (ZRegister.MEM_ABS_8, Some(VariableExpression("HLD" | "hld")), (ZRegister.A, None))
            if options.flags(CompilationFlag.EmitSharpOpcodes) =>
            (LD_HLDA, NoRegisters, None, zero)

          case (r1, e1, (r2, e2)) => merge(LD, LD_16, skipTargetA = false)((r1, e1, r2, e2))
        }
        case "ADD" => (param(allowAbsolute = false) ~ HWS ~ position("comma").map(_ => ()) ~ "," ~/ HWS ~ param(allowAbsolute = false)).map {
          case (ZRegister.SP, None, (ZRegister.IMM_8, Some(expr))) if options.flags(CompilationFlag.EmitSharpOpcodes) =>
            (ADD_SP, OneRegister(ZRegister.IMM_8), None, expr)
          case (r1, e1, (r2, e2)) => merge(ADD, ADD_16, skipTargetA = true)((r1, e1, r2, e2))
        }
        case "ADC" => (param(allowAbsolute = false) ~ HWS ~ position("comma").map(_ => ()) ~ "," ~/ HWS ~ param(allowAbsolute = false)).map {
          case (r1, e1, (r2, e2)) => merge(ADC, ADC_16, skipTargetA = true)((r1, e1, r2, e2))
        }
        case "SBC" => (param(allowAbsolute = false) ~ HWS ~ position("comma").map(_ => ()) ~ "," ~/ HWS ~ param(allowAbsolute = false)).map {
          case (r1, e1, (r2, e2)) => merge(SBC, SBC_16, skipTargetA = true)((r1, e1, r2, e2))
        }

        case _ =>
          log.error("Unsupported opcode " + opcode, Some(pos))
          imm(NOP)
      }
    } yield {
      val (actualOpcode, registers, offset, param) = tuple4
      Z80AssemblyStatement(actualOpcode, registers, offset, param, el)
    }
  }

  val intelAsmInstruction: P[ExecutableStatement] = {

    import ZOpcode._
    import ZRegister._

    for {
      el <- elidable
      pos <- position()
      opcode: String <- identifier ~/ HWS
      tuple4: (ZOpcode.Value, ZRegisters, Option[Expression], Expression) <- opcode.toUpperCase(Locale.ROOT) match {
        case "NOP" => imm(NOP)
        case "RLC" => imm(RLCA)
        case "RRC" => imm(RRCA)
        case "RAL" => imm(RLA)
        case "RAR" => imm(RRA)
        case "DAA" => imm(DAA)
        case "CMA" => imm(CPL)
        case "STC" => imm(SCF)
        case "CMC" => imm(CCF)

        case "HLT" => imm(HALT)
        case "EI" => imm(EI)
        case "DI" => imm(DI)
        case "XCHG" => imm(EX_DE_HL)
        case "XTHL" => P("").map(_ => (EX_SP, OneRegister(HL), None, zero))
        case "SPHL" => P("").map(_ => (LD_16, TwoRegisters(SP, HL), None, zero))
        case "PCHL" => P("").map(_ => (JP, z80.OneRegister(HL), None, zero))

        case "MOV" => (intel8 ~ HWS ~ position("comma").map(_ => ()) ~ "," ~/ HWS ~ intel8).map {
          case (r1, r2) => (LD, TwoRegisters(r1, r2), None, zero)
        }
        case "LXI" => (intel16(false) ~ HWS ~ position("comma").map(_ => ()) ~ "," ~/ HWS ~ asmExpression).map {
          case (r, e) => (LD_16, TwoRegisters(r, IMM_16), None, e)
        }
        case "MVI" => (intel8 ~ HWS ~ position("comma").map(_ => ()) ~ "," ~/ HWS ~ asmExpression).map {
          case (r, e) => (LD, TwoRegisters(r, IMM_8), None, e)
        }
        case "DAD" => intel16(false).map { r => (ADD_16, TwoRegisters(HL, r), None, zero)}
        case "STAX" => intel16(false).map {
          case r@(ZRegister.BC | ZRegister.DE) => (LD, TwoRegisters(r, A), None, zero)
          case _ =>
            log.error("Invalid parameters for STAX", Some(pos))
            (NOP, NoRegisters, None, zero)
        }
        case "LDAX" => intel16(false).map {
          case r@(ZRegister.BC | ZRegister.DE) => (LD, TwoRegisters(A, r), None, zero)
          case _ =>
            log.error("Invalid parameter for STAX", Some(pos))
            (NOP, NoRegisters, None, zero)
        }
        case "INX" => intel16(false).map { r => (INC_16, OneRegister(r), None, zero)}
        case "DCX" => intel16(false).map { r => (DEC_16, OneRegister(r), None, zero)}
        case "POP" => intel16(true).map { r => (POP, OneRegister(r), None, zero)}
        case "PUSH" => intel16(true).map { r => (PUSH, OneRegister(r), None, zero)}
        case "INR" => intel8.map { r => (INC, OneRegister(r), None, zero)}
        case "DCR" => intel8.map { r => (DEC, OneRegister(r), None, zero)}
        case "ADD" => intel8.map { r => (ADD, OneRegister(r), None, zero)}
        case "SUB" => intel8.map { r => (SUB, OneRegister(r), None, zero)}
        case "ADC" => intel8.map { r => (ADC, OneRegister(r), None, zero)}
        case "SBB" => intel8.map { r => (SBC, OneRegister(r), None, zero)}
        case "ORA" => intel8.map { r => (OR, OneRegister(r), None, zero)}
        case "ANA" => intel8.map { r => (AND, OneRegister(r), None, zero)}
        case "XRA" => intel8.map { r => (XOR, OneRegister(r), None, zero)}
        case "CMP" => intel8.map { r => (CP, OneRegister(r), None, zero)}
        case "ADI" => asmExpression.map { e => (ADD, OneRegister(IMM_8), None, e)}
        case "ACI" => asmExpression.map { e => (ADC, OneRegister(IMM_8), None, e)}
        case "SUI" => asmExpression.map { e => (SUB, OneRegister(IMM_8), None, e)}
        case "SBI" => asmExpression.map { e => (SBC, OneRegister(IMM_8), None, e)}
        case "ANI" => asmExpression.map { e => (AND, OneRegister(IMM_8), None, e)}
        case "ORI" => asmExpression.map { e => (OR, OneRegister(IMM_8), None, e)}
        case "XRI" => asmExpression.map { e => (XOR, OneRegister(IMM_8), None, e)}
        case "CPI" => asmExpression.map { e => (CP, OneRegister(IMM_8), None, e)}

        case "SHLD" => asmExpression.map { e => (LD_16, TwoRegisters(MEM_ABS_16, HL), None, e)}
        case "LHLD" => asmExpression.map { e => (LD_16, TwoRegisters(HL, MEM_ABS_16), None, e)}
        case "STA" => asmExpression.map { e => (LD, TwoRegisters(MEM_ABS_8, A), None, e)}
        case "LDA" => asmExpression.map { e => (LD, TwoRegisters(A, MEM_ABS_8), None, e)}
        case "RST" => asmExpression.map {
          case LiteralExpression(value, _) if value >=0 && value <= 7=> (RST, NoRegisters, None, LiteralExpression(value * 8, 1))
          case _ =>
            log.error("Invalid parameter for RST", Some(pos))
            (NOP, NoRegisters, None, zero)
        }

        case "IN" => asmExpression.map { e => (IN_IMM, OneRegister(A), None, e) }
        case "OUT" => asmExpression.map { e => (OUT_IMM, OneRegister(A), None, e) }

        case "JMP" => asmExpression.map { e => (JP, NoRegisters, None, e)}
        case "JC" => asmExpression.map { e => (JP, IfFlagSet(ZFlag.C), None, e)}
        case "JZ" => asmExpression.map { e => (JP, IfFlagSet(ZFlag.Z), None, e)}
        case "JM" => asmExpression.map { e => (JP, IfFlagSet(ZFlag.S), None, e)}
        case "JPE" => asmExpression.map { e => (JP, IfFlagSet(ZFlag.P), None, e)}
        case "JNC" => asmExpression.map { e => (JP, IfFlagClear(ZFlag.C), None, e)}
        case "JNZ" => asmExpression.map { e => (JP, IfFlagClear(ZFlag.Z), None, e)}
        case "JP" => asmExpression.map { e => (JP, IfFlagClear(ZFlag.S), None, e)}
        case "JPO" => asmExpression.map { e => (JP, IfFlagClear(ZFlag.P), None, e)}
          
        case "RET" => imm(RET)
        case "RC" => P("").map { _ => (RET, IfFlagSet(ZFlag.C), None, zero)}
        case "RZ" => P("").map { _ => (RET, IfFlagSet(ZFlag.Z), None, zero)}
        case "RM" => P("").map { _ => (RET, IfFlagSet(ZFlag.S), None, zero)}
        case "RPE" => P("").map { _ => (RET, IfFlagSet(ZFlag.P), None, zero)}
        case "RNC" => P("").map { _ => (RET, IfFlagClear(ZFlag.C), None, zero)}
        case "RNZ" => P("").map { _ => (RET, IfFlagClear(ZFlag.Z), None, zero)}
        case "RP" => P("").map { _ => (RET, IfFlagClear(ZFlag.S), None, zero)}
        case "RPO" => P("").map { _ => (RET, IfFlagClear(ZFlag.P), None, zero)}
          
        case "CALL" => asmExpression.map { e => (CALL, NoRegisters, None, e)}
        case "CC" => asmExpression.map { e => (CALL, IfFlagSet(ZFlag.C), None, e)}
        case "CZ" => asmExpression.map { e => (CALL, IfFlagSet(ZFlag.Z), None, e)}
        case "CM" => asmExpression.map { e => (CALL, IfFlagSet(ZFlag.S), None, e)}
        case "CPE" => asmExpression.map { e => (CALL, IfFlagSet(ZFlag.P), None, e)}
        case "CNC" => asmExpression.map { e => (CALL, IfFlagClear(ZFlag.C), None, e)}
        case "CNZ" => asmExpression.map { e => (CALL, IfFlagClear(ZFlag.Z), None, e)}
        case "CP" => asmExpression.map { e => (CALL, IfFlagClear(ZFlag.S), None, e)}
        case "CPO" => asmExpression.map { e => (CALL, IfFlagClear(ZFlag.P), None, e)}
          
        case _ =>
          log.error("Unsupported opcode " + opcode, Some(pos))
          imm(NOP)
      }
    } yield {
      val (actualOpcode, registers, offset, param) = tuple4
      Z80AssemblyStatement(actualOpcode, registers, offset, param, el)
    }
  }

  val asmInstruction: P[ExecutableStatement] = if (useIntelSyntax) intelAsmInstruction else zilogAsmInstruction

  private def imm(opcode: ZOpcode.Value): P[(ZOpcode.Value, ZRegisters, Option[Expression], Expression)] =
    P("").map(_=>(opcode, NoRegisters, None, zero))

  private def regA(opcode: ZOpcode.Value): P[(ZOpcode.Value, ZRegisters, Option[Expression], Expression)] =
    P("").map(_=>(opcode, OneRegister(ZRegister.A), None, zero))

  override val asmStatement: P[ExecutableStatement] = (position("assembly statement") ~ P(asmLabel | asmMacro | arrayContentsForAsm | asmInstruction)).map { case (p, s) => s.pos(p) } // TODO: macros

  override def validateAsmFunctionBody(p: Position, flags: Set[String], name: String, statements: Option[List[Statement]]): Unit = {
    statements match {
      case Some(Nil) => log.warn("Assembly function `$name` is empty, did you mean RET, RETI, RETN or JP", Some(p))
      case Some(xs) =>
        if (flags("interrupt")) {
          if (xs.exists {
            case Z80AssemblyStatement(ZOpcode.RET, _, _, _, _) => true
            case _ => false
          }) log.warn("Assembly interrupt function `$name` contains RET, did you mean RETI/RETN?", Some(p))
        } else {
          if (xs.exists {
            case Z80AssemblyStatement(ZOpcode.RETI, _, _, _, _) => true
            case Z80AssemblyStatement(ZOpcode.RETN, _, _, _, _) => true
            case _ => false
          }) log.warn("Assembly non-interrupt function `$name` contains RETI or RETN, did you mean RET?", Some(p))
        }
        if (!name.startsWith("__") && !flags("macro")) {
          xs.last match {
            case Z80AssemblyStatement(ZOpcode.RET, NoRegisters, _, _, _) => () // OK
            case Z80AssemblyStatement(ZOpcode.RETN, NoRegisters, _, _, _) => () // OK
            case Z80AssemblyStatement(ZOpcode.RETI, NoRegisters, _, _, _) => () // OK
            case Z80AssemblyStatement(ZOpcode.JP, NoRegisters, _, _, _) => () // OK
            case Z80AssemblyStatement(ZOpcode.JR, NoRegisters, _, _, _) => () // OK
            case _ =>
              val validReturn = if (flags("interrupt")) "RETI/RETN" else "RET"
              log.warn(s"Non-macro assembly function `$name` should end in " + validReturn, Some(p))
          }
        }
      case None => ()
    }
  }
}
