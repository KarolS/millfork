package millfork.parser

import java.util.Locale

import fastparse.all.{parserApi, _}
import fastparse.core
import millfork.assembly.{Elidability, z80}
import millfork.{CompilationFlag, CompilationOptions, node}
import millfork.assembly.z80.{ZOpcode, _}
import millfork.env.{ByZRegister, Constant, ParamPassingConvention}
import millfork.error.ConsoleLogger
import millfork.node.{ZRegister, _}
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

  override val appcRegister: P[ParamPassingConvention] = (P("hl" | "bc" | "de" | "a" | "b" | "c" | "d" | "e" | "h" | "l").! ~ !letterOrDigit)
    .map(name => ByZRegister(ZRegister.fromString(name).getOrElse(log.fatal(s"Unknown assembly parameter passing convention: `$name`"))))

  override val asmParamDefinition: P[ParameterDeclaration] = for {
    p <- position()
    typ <- identifier ~ SWS
    appc <- appcRegister | appcComplex
  } yield ParameterDeclaration(typ, appc).pos(p)

  val asmLabel: P[ExecutableStatement] = ((".".? ~ identifier).! ~ HWS ~ ":" ~/ AWS_asm).map(l => Z80AssemblyStatement(ZOpcode.LABEL, NoRegisters, None, VariableExpression(l), elidability = Elidability.Elidable))

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

  private val toIndexHalf: Map[String, ZRegister.Value] = Map(
    "IXH" -> ZRegister.IXH, "ixh" -> ZRegister.IXH,
    "IXL" -> ZRegister.IXL, "ixl" -> ZRegister.IXL,
    "IYH" -> ZRegister.IYH, "iyh" -> ZRegister.IYH,
    "IYL" -> ZRegister.IYL, "iyl" -> ZRegister.IYL,
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
    "HL" -> ZRegister.HL, "hl" -> ZRegister.HL,
    "PSW" -> ZRegister.AF, "psw" -> ZRegister.AF,
    "B" -> ZRegister.BC, "b" -> ZRegister.BC,
    "BC" -> ZRegister.BC, "bc" -> ZRegister.BC,
    "D" -> ZRegister.DE, "d" -> ZRegister.DE,
    "DE" -> ZRegister.DE, "de" -> ZRegister.DE,
    "SP" -> ZRegister.SP, "sp" -> ZRegister.SP,
    "IX" -> ZRegister.IX, "ix" -> ZRegister.IX,
    "IY" -> ZRegister.IY, "iy" -> ZRegister.IY,
  )

  private def param(allowAbsolute: Boolean, allowRI: Boolean = false, allowFfc: Boolean = false): P[(ZRegister.Value, Option[Expression])] = asmExpressionWithParens.map {
    case (VariableExpression("R" | "r"), false) if allowRI => (ZRegister.R, None)
    case (VariableExpression("I" | "i"), false) if allowRI => (ZRegister.I, None)
    case (VariableExpression(r), false) if toRegister.contains(r)=> (toRegister(r), None)
    case (VariableExpression(r), false)
      if options.flag(CompilationFlag.EmitZ80Opcodes) &&
        options.flag(CompilationFlag.EmitIllegals) &&
        toIndexHalf.contains(r)=> (toIndexHalf(r), None)
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

  def one8RegisterIntel(op: ZOpcode.Value): P[(ZOpcode.Value, OneRegister, Option[Expression], Expression)] = intel8.map(reg => (op, OneRegister(reg), None, zero))

  def one8RegisterOr8085Illegal(op: ZOpcode.Value, illegalTarget: ZRegister.Value, illegalOp: ZOpcode.Value): P[(ZOpcode.Value, ZRegisters, Option[Expression], Expression)] = param(allowAbsolute = false).map{
    case (reg@(ZRegister.MEM_IX_D | ZRegister.MEM_IY_D), Some(e)) => (op, OneRegister(reg), Some(e), zero)
    case (reg, _) if reg == illegalTarget && options.flag(CompilationFlag.EmitIntel8085Opcodes) && options.flag(CompilationFlag.EmitIllegals) =>
      (illegalOp, NoRegisters, None, zero)
    case (reg, _) if reg == illegalTarget =>
      log.error("This instruction requires support for undocumented 8085 instructions: " + ZLine.register(op, reg))
      (op, OneRegister(ZRegister.A), None, zero)
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
    if (options.flag(CompilationFlag.EmitIllegals) && options.flag(CompilationFlag.EmitIntel8085Opcodes))
    "NZ" | "nz" | "nc" | "NC" | "NV" | "nv" |
      "PO" | "po" | "PE" | "pe" |
      "m" | "M" | "p" | "P" |
      "c" | "C" | "Z" | "z" | "V" | "v" |
      "k" | "K" | "nk" | "NK" |
      "x5" | "X5" | "nx5" | "NX5"
    else
      "NZ" | "nz" | "nc" | "NC" | "NV" | "nv" |
        "PO" | "po" | "PE" | "pe" |
        "m" | "M" | "p" | "P" |
        "c" | "C" | "Z" | "z" | "V" | "v"
    ).! ~ HWS).map {
    case "Z" | "z" => IfFlagSet(ZFlag.Z)
    case "PE" | "pe" | "v" | "V" => IfFlagSet(ZFlag.P)
    case "C" | "c" => IfFlagSet(ZFlag.C)
    case "M" | "m" => IfFlagSet(ZFlag.S)
    case "NZ" | "nz" => IfFlagClear(ZFlag.Z)
    case "PO" | "po" | "NV" | "nv" => IfFlagClear(ZFlag.P)
    case "NC" | "nc" => IfFlagClear(ZFlag.C)
    case "P" | "p" => IfFlagClear(ZFlag.S)
    case "K" | "k" | "X5" | "x5" => IfFlagSet(ZFlag.K)
    case "NK" | "nk" |"NX5" | "nx5" => IfFlagClear(ZFlag.K)
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

  private val indexHalves: Set[ZRegister.Value] = {
    import ZRegister._
    Set(IXH, IXL, IYH, IYL)
  }
  private val MEM_R: Set[ZRegister.Value] = {
    import ZRegister._
    Set(MEM_HL,MEM_IX_D,MEM_IY_D)
  }

  def invalidIllegalRegisters(r1: ZRegister.Value, r2: ZRegister.Value): Boolean = {
    import ZRegister._
    if (MEM_R(r1) && MEM_R(r2)) return true
    if (!indexHalves(r1) && !indexHalves(r2)) return false
    if (!options.flag(CompilationFlag.EmitIllegals) && !options.flag(CompilationFlag.EmitZ80Opcodes) && (indexHalves(r1) || indexHalves(r2))) return true
    if ((r1 == H || r1 == L || r1 == MEM_IX_D || r1 == MEM_IY_D) && indexHalves(r2)) return true
    if ((r2 == H || r2 == L || r2 == MEM_IX_D || r2 == MEM_IY_D) && indexHalves(r1)) return true
    if ((r1 == IYH || r1 == IYL) && (r2 == IXH || r2 == IXL)) return true
    if ((r1 == IXH || r1 == IXL) && (r2 == IYH || r2 == IYL)) return true
    false // TODO
  }

  val zilogAsmInstruction: P[ExecutableStatement] = {
    import ZOpcode._
    for {
      el <- elidable
      pos <- position("assembly statement")
      opcode: String <- identifier ~/ HWS
      tuple4: (ZOpcode.Value, ZRegisters, Option[Expression], Expression) <- opcode.toUpperCase(Locale.ROOT) match {
        case "RST" => asmExpression.map((RST, NoRegisters, None, _))
        case "IM" => asmExpression.map((IM, NoRegisters, None, _))
        case "EI" => imm(EI)
        case "DI" => imm(DI)
        case "HLT" => imm(HALT)
        case "HALT" => imm(HALT)
        case "STOP" => imm(STOP)
        case "RIM" => imm(RIM)
        case "SIM" => imm(SIM)

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
        case "RL" => one8RegisterOr8085Illegal(RL, ZRegister.DE, RLDE)
        case "RR" => one8Register(RR)
        case "RLC" => one8Register(RLC)
        case "RRC" => one8Register(RRC)
        case "SLA" => one8Register(SLA)
        case "SLL" | "SLS" => one8Register(SLL)
        case "SRA" => one8RegisterOr8085Illegal(SRA, ZRegister.HL, RRHL)
        case "SRL" => one8Register(SRL)
        case "SWAP" => one8Register(SWAP)

        case "BIT" => (param(allowAbsolute = false) ~ HWS ~ position("comma").map(_ => ()) ~ "," ~/ HWS ~ param(allowAbsolute = false)).map {
          case (ZRegister.IMM_8, Some(LiteralExpression(n, _)), (r2, e2))
            if n >= 0 && n <= 7 && r2 != ZRegister.MEM_BC && r2 != ZRegister.MEM_DE =>
            (ZOpcodeClasses.BIT_seq(n.toInt), OneRegister(r2), e2, zero)
          case _ =>
            log.error("Invalid parameters for BIT", Some(pos))
            (NOP, NoRegisters, None, zero)
        }
        case "SET" => (param(allowAbsolute = false) ~ HWS ~ position("comma").map(_ => ()) ~ "," ~/ HWS ~ param(allowAbsolute = false)).map {
          case (ZRegister.IMM_8, Some(LiteralExpression(n, _)), (r2, e2))
            if n >= 0 && n <= 7 && r2 != ZRegister.MEM_BC && r2 != ZRegister.MEM_DE =>
            (ZOpcodeClasses.SET_seq(n.toInt), OneRegister(r2), e2, zero)
          case _ =>
            log.error("Invalid parameters for SET", Some(pos))
            (NOP, NoRegisters, None, zero)
        }
        case "RES" => (param(allowAbsolute = false) ~ HWS ~ position("comma").map(_ => ()) ~ "," ~/ HWS ~ param(allowAbsolute = false)).map {
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
          case (ZRegister.DE, None, (ZRegister.IMM_8 | ZRegister.IMM_16, Some(SumExpression((false, VariableExpression("sp" | "SP")) :: offset, false))))
            if options.flags(CompilationFlag.EmitIntel8085Opcodes) && options.flags(CompilationFlag.EmitIllegals) =>
            (LD_DESP, OneRegister(ZRegister.IMM_8), None, offset match {
              case List((false, expr)) => expr
              case (true, _) :: _ => SumExpression((false -> LiteralExpression(0, 1)) :: offset, decimal = false)
              case _ => SumExpression(offset, decimal = false)
            })
          case (ZRegister.DE, None, (ZRegister.IMM_8 | ZRegister.IMM_16, Some(SumExpression((false, VariableExpression("hl" | "HL")) :: offset, false))))
            if options.flags(CompilationFlag.EmitIntel8085Opcodes) && options.flags(CompilationFlag.EmitIllegals) =>
            (LD_DEHL, OneRegister(ZRegister.IMM_8), None, offset match {
              case List((false, expr)) => expr
              case (true, _) :: _ => SumExpression((false -> LiteralExpression(0, 1)) :: offset, decimal = false)
              case _ => SumExpression(offset, decimal = false)
            })
          case (ZRegister.HL, None, (ZRegister.IMM_8 | ZRegister.IMM_16, Some(SumExpression((false, VariableExpression("sp" | "SP")) :: offset, false))))
            if options.flags(CompilationFlag.EmitSharpOpcodes) =>
            (LD_HLSP, OneRegister(ZRegister.IMM_8), None, offset match {
              case List((false, expr)) => expr
              case (true, _) :: _ => SumExpression((false -> LiteralExpression(0, 1)) :: offset, decimal = false)
              case _ => SumExpression(offset, decimal = false)
            })
          case (ZRegister.MEM_DE, None, (ZRegister.HL, None))
            if options.flags(CompilationFlag.EmitIntel8085Opcodes) && options.flags(CompilationFlag.EmitIllegals) =>
            (SHLX, NoRegisters, None, zero)
          case (ZRegister.HL, None, (ZRegister.MEM_DE, None))
            if options.flags(CompilationFlag.EmitIntel8085Opcodes) && options.flags(CompilationFlag.EmitIllegals) =>
            (SHLX, NoRegisters, None, zero)
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

          case (r1, e1, (r2, e2)) =>
            if (invalidIllegalRegisters(r1,r2)) {
              log.error("Invalid parameters for LD", Some(pos))
            }
            merge(LD, LD_16, skipTargetA = false)((r1, e1, r2, e2))
        }
        case "ADD" => (param(allowAbsolute = false) ~ HWS ~ position("comma").map(_ => ()) ~ "," ~/ HWS ~ param(allowAbsolute = false)).map {

          case (ZRegister.HL, None, (ZRegister.A, None)) if options.flags(CompilationFlag.EmitZ80NextOpcodes) =>
            (ADD_16, TwoRegisters(ZRegister.HL, ZRegister.A), None, zero)
          case (ZRegister.DE, None, (ZRegister.A, None)) if options.flags(CompilationFlag.EmitZ80NextOpcodes) =>
            (ADD_16, TwoRegisters(ZRegister.DE, ZRegister.A), None, zero)
          case (ZRegister.BC, None, (ZRegister.A, None)) if options.flags(CompilationFlag.EmitZ80NextOpcodes) =>
            (ADD_16, TwoRegisters(ZRegister.BC, ZRegister.A), None, zero)
          case (ZRegister.HL, None, (ZRegister.IMM_8, Some(expr))) =>
            (ADD_16, TwoRegisters(ZRegister.HL, ZRegister.IMM_16), None, expr)
          case (ZRegister.DE, None, (ZRegister.IMM_8, Some(expr))) =>
            (ADD_16, TwoRegisters(ZRegister.DE, ZRegister.IMM_16), None, expr)
          case (ZRegister.BC, None, (ZRegister.IMM_8, Some(expr))) =>
            (ADD_16, TwoRegisters(ZRegister.BC, ZRegister.IMM_16), None, expr)

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

        case "DSUB" => imm(DSUB)
        case "RSTV" => imm(RSTV)

        case "LDIX" => imm(LDIX)
        case "LDWS" => imm(LDWS)
        case "LDIRX" => imm(LDIRX)
        case "LDDX" => imm(LDDX)
        case "LDDRX" => imm(LDDRX)
        case "LDPIRX" => imm(LDPIRX)
        case "OUTINB" => imm(OUTINB)
        case "SWAPNIB" => imm(SWAPNIB)
        case "PIXELDN" => imm(PIXELDN)
        case "PIXELAD" => imm(PIXELAD)
        case "SETAE" => imm(SETAE)
        case "MUL" => (("D"|"d") ~ HWS ~ "," ~/ HWS ~ ("E" | "e")).?.map { _ => (MUL, NoRegisters, None, zero)}
        case "MIRROR" => ("A"|"a").?.map { _ => (MUL, NoRegisters, None, zero)}
        case "NEXTREG" =>(param(allowAbsolute = false) ~ HWS ~ position("comma").map(_ => ()) ~ "," ~/ HWS ~ param(allowAbsolute = false)).map {
          case (ZRegister.IMM_8, Some(n), (ZRegister.A, None)) => (NEXTREG, TwoRegisters(ZRegister.IMM_8, ZRegister.A), None, n)
          case (ZRegister.IMM_8, Some(n), (ZRegister.IMM_8, Some(v))) => (NEXTREG, TwoRegisters(ZRegister.IMM_8, ZRegister.IMM_8), None, SeparateBytesExpression(v, n))
          case _ =>
            log.error("Invalid parameters for NEXTREG", Some(pos))
            (NOP, NoRegisters, None, zero)
        }
        case "TEST" => one8Register(TEST)

        case _ =>
          log.error("Unsupported opcode " + opcode, Some(pos))
          imm(NOP)
      }
    } yield {
      val (actualOpcode, registers, offset, param) = tuple4
      Z80AssemblyStatement(actualOpcode, registers, offset, param, el).pos(pos)
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
        case "RIM" => imm(RIM)
        case "SIM" => imm(SIM)

        case "EXAF" => imm(EX_AF_AF)
        case "EXX" => imm(EXX)
        case "NEG" => imm(NEG)
        case "IM0" => P("").map(_ => (IM, NoRegisters, None, LiteralExpression(0, 1)))
        case "IM1" => P("").map(_ => (IM, NoRegisters, None, LiteralExpression(1, 1)))
        case "IM2" => P("").map(_ => (IM, NoRegisters, None, LiteralExpression(2, 1)))
        case "RETI" => imm(RETI)
        case "RETN" => imm(RETN)
        case "RLD" => imm(RLD)
        case "RRD" => imm(RRD)
        case "INI" => imm(INI)
        case "INIR" => imm(INIR)
        case "IND" => imm(IND)
        case "INDR" => imm(INDR)
        case "OUTI" => imm(OUTI)
        case "OUTIR" => imm(OUTIR)
        case "OUTD" => imm(OUTD)
        case "OUTDR" => imm(OUTDR)
        case "LDI" => imm(LDI)
        case "LDIR" => imm(LDIR)
        case "LDD" => imm(LDD)
        case "LDDR" => imm(LDDR)
        case "CCI" => imm(CPI)
        case "CCIR" => imm(CPIR)
        case "CCD" => imm(CPD)
        case "CCDR" => imm(CPDR)

        case "RALR" => one8RegisterIntel(RL)
        case "RARR" => one8RegisterIntel(RR)
        case "RLCR" => one8RegisterIntel(RLC)
        case "RRCR" => one8RegisterIntel(RRC)
        case "SLAR" => one8RegisterIntel(SLA)
        case "SLLR" => one8RegisterIntel(SLL)
        case "SRAR" => one8RegisterIntel(SRA)
        case "SRLR" => one8RegisterIntel(SRL)
          
        case "RALX" => asmExpression.map(d => (RL, OneRegister(MEM_IX_D), Some(d), zero))
        case "RARX" => asmExpression.map(d => (RR, OneRegister(MEM_IX_D), Some(d), zero))
        case "RLCX" => asmExpression.map(d => (RLC, OneRegister(MEM_IX_D), Some(d), zero))
        case "RRCX" => asmExpression.map(d => (RRC, OneRegister(MEM_IX_D), Some(d), zero))
        case "SLAX" => asmExpression.map(d => (SLA, OneRegister(MEM_IX_D), Some(d), zero))
        case "SLLX" => asmExpression.map(d => (SLL, OneRegister(MEM_IX_D), Some(d), zero))
        case "SRAX" => asmExpression.map(d => (SRA, OneRegister(MEM_IX_D), Some(d), zero))
        case "SRLX" => asmExpression.map(d => (SRL, OneRegister(MEM_IX_D), Some(d), zero))
          
        case "RALY" => asmExpression.map(d => (RL, OneRegister(MEM_IY_D), Some(d), zero))
        case "RARY" => asmExpression.map(d => (RR, OneRegister(MEM_IY_D), Some(d), zero))
        case "RLCY" => asmExpression.map(d => (RLC, OneRegister(MEM_IY_D), Some(d), zero))
        case "RRCY" => asmExpression.map(d => (RRC, OneRegister(MEM_IY_D), Some(d), zero))
        case "SLAY" => asmExpression.map(d => (SLA, OneRegister(MEM_IY_D), Some(d), zero))
        case "SLLY" => asmExpression.map(d => (SLL, OneRegister(MEM_IY_D), Some(d), zero))
        case "SRAY" => asmExpression.map(d => (SRA, OneRegister(MEM_IY_D), Some(d), zero))
        case "SRLY" => asmExpression.map(d => (SRL, OneRegister(MEM_IY_D), Some(d), zero))

        case "HLT" => imm(HALT)
        case "EI" => imm(EI)
        case "DI" => imm(DI)
        case "XCHG" => imm(EX_DE_HL)
        case "XTHL" => P("").map(_ => (EX_SP, OneRegister(HL), None, zero))
        case "XTIX" => P("").map(_ => (EX_SP, OneRegister(IX), None, zero))
        case "XTIY" => P("").map(_ => (EX_SP, OneRegister(IY), None, zero))
        case "SPHL" => P("").map(_ => (LD_16, TwoRegisters(SP, HL), None, zero))
        case "SPIX" => P("").map(_ => (LD_16, TwoRegisters(SP, IX), None, zero))
        case "SPIY" => P("").map(_ => (LD_16, TwoRegisters(SP, IY), None, zero))
        case "PCHL" => P("").map(_ => (JP, z80.OneRegister(HL), None, zero))
        case "PCIX" => P("").map(_ => (JP, z80.OneRegister(IX), None, zero))
        case "PCIY" => P("").map(_ => (JP, z80.OneRegister(IY), None, zero))

        case "MOV" => (intel8 ~ HWS ~ position("comma").map(_ => ()) ~ "," ~/ HWS ~ intel8).map {
          case (r1, r2) => (LD, TwoRegisters(r1, r2), None, zero)
        }
        case "LXI" => (intel16(false) ~ HWS ~ position("comma").map(_ => ()) ~ "," ~/ HWS ~ asmExpression).map {
          case (r, e) => (LD_16, TwoRegisters(r, IMM_16), None, e)
        }
        case "LXIX" => asmExpression.map {
          case (e) => (LD_16, TwoRegisters(IX, IMM_16), None, e)
        }
        case "LXIY" => asmExpression.map {
          case (e) => (LD_16, TwoRegisters(IY, IMM_16), None, e)
        }
        case "MVI" => (intel8 ~ HWS ~ position("comma").map(_ => ()) ~ "," ~/ HWS ~ asmExpression).map {
          case (r, e) => (LD, TwoRegisters(r, IMM_8), None, e)
        }
        case "MVIX" => (asmExpression ~ HWS ~ position("comma").map(_ => ()) ~ "," ~/ HWS ~ asmExpression).map {
          case (e, d) => (LD, TwoRegisters(MEM_IX_D, IMM_8), Some(d), e)
        }
        case "MVIY" => (asmExpression ~ HWS ~ position("comma").map(_ => ()) ~ "," ~/ HWS ~ asmExpression).map {
          case (e, d) => (LD, TwoRegisters(MEM_IY_D, IMM_8), Some(d), e)
        }
        case "LDX" => (intel8 ~ HWS ~ position("comma").map(_ => ()) ~ "," ~/ HWS ~ asmExpression).map {
          case (r, d) => (LD, TwoRegisters(r, MEM_IX_D), Some(d), zero)
        }
        case "LDY" => (intel8 ~ HWS ~ position("comma").map(_ => ()) ~ "," ~/ HWS ~ asmExpression).map {
          case (r, d) => (LD, TwoRegisters(r, MEM_IY_D), Some(d), zero)
        }
        case "STX" => (intel8 ~ HWS ~ position("comma").map(_ => ()) ~ "," ~/ HWS ~ asmExpression).map {
          case (r, d) => (LD, TwoRegisters(MEM_IX_D, r), Some(d), zero)
        }
        case "STY" => (intel8 ~ HWS ~ position("comma").map(_ => ()) ~ "," ~/ HWS ~ asmExpression).map {
          case (r, d) => (LD, TwoRegisters(MEM_IY_D, r), Some(d), zero)
        }
        case "DAD" => intel16(false).map { r => (ADD_16, TwoRegisters(HL, r), None, zero)}
        case "DADC" => intel16(false).map { r => (ADC_16, TwoRegisters(HL, r), None, zero)}
        case "DSBC" => intel16(false).map { r => (SBC_16, TwoRegisters(HL, r), None, zero)}
        case "DADX" => intel16(false).map { r => (ADD_16, TwoRegisters(IX, r), None, zero)}
        case "DADY" => intel16(false).map { r => (ADD_16, TwoRegisters(IY, r), None, zero)}
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
        case "INXIX" => P("").map(_ => (INC_16, OneRegister(IX), None, zero))
        case "INXIY" => P("").map(_ => (INC_16, OneRegister(IY), None, zero))
        case "DCX" => intel16(false).map { r => (DEC_16, OneRegister(r), None, zero)}
        case "DCXIX" => P("").map(_ => (DEC_16, OneRegister(IX), None, zero))
        case "DCXIY" => P("").map(_ => (DEC_16, OneRegister(IY), None, zero))
        case "POP" => intel16(true).map { r => (POP, OneRegister(r), None, zero)}
        case "POPIX" => P("").map(_ => (POP, OneRegister(IX), None, zero))
        case "POPIY" => P("").map(_ => (POP, OneRegister(IY), None, zero))
        case "PUSH" => intel16(true).map { r => (PUSH, OneRegister(r), None, zero)}
        case "PUSHIX" => P("").map(_ => (PUSH, OneRegister(IX), None, zero))
        case "PUSHIY" => P("").map(_ => (PUSH, OneRegister(IY), None, zero))
        case "INR" => intel8.map { r => (INC, OneRegister(r), None, zero)}
        case "INRX" => asmExpression.map { d => (INC, OneRegister(MEM_IX_D), Some(d), zero)}
        case "INRY" => asmExpression.map { d => (INC, OneRegister(MEM_IY_D), Some(d), zero)}
        case "DCR" => intel8.map { r => (DEC, OneRegister(r), None, zero)}
        case "DCRX" => asmExpression.map { d => (DEC, OneRegister(MEM_IX_D), Some(d), zero)}
        case "DCRY" => asmExpression.map { d => (DEC, OneRegister(MEM_IY_D), Some(d), zero)}
        case "ADD" => intel8.map { r => (ADD, OneRegister(r), None, zero)}
        case "ADDX" => asmExpression.map { d => (ADD, OneRegister(MEM_IX_D), Some(d), zero)}
        case "ADDY" => asmExpression.map { d => (ADD, OneRegister(MEM_IY_D), Some(d), zero)}
        case "SUB" => intel8.map { r => (SUB, OneRegister(r), None, zero)}
        case "SUBX" => asmExpression.map { d => (SUB, OneRegister(MEM_IX_D), Some(d), zero)}
        case "SUBY" => asmExpression.map { d => (SUB, OneRegister(MEM_IY_D), Some(d), zero)}
        case "ADC" => intel8.map { r => (ADC, OneRegister(r), None, zero)}
        case "ADCX" => asmExpression.map { d => (ADC, OneRegister(MEM_IX_D), Some(d), zero)}
        case "ADCY" => asmExpression.map { d => (ADC, OneRegister(MEM_IY_D), Some(d), zero)}
        case "SBB" => intel8.map { r => (SBC, OneRegister(r), None, zero)}
        case "SBCX" => asmExpression.map { d => (SBC, OneRegister(MEM_IX_D), Some(d), zero)}
        case "SBCY" => asmExpression.map { d => (SBC, OneRegister(MEM_IY_D), Some(d), zero)}
        case "ORA" => intel8.map { r => (OR, OneRegister(r), None, zero)}
        case "ORX" => asmExpression.map { d => (OR, OneRegister(MEM_IX_D), Some(d), zero)}
        case "ORY" => asmExpression.map { d => (OR, OneRegister(MEM_IY_D), Some(d), zero)}
        case "ANA" => intel8.map { r => (AND, OneRegister(r), None, zero)}
        case "ANDX" => asmExpression.map { d => (AND, OneRegister(MEM_IX_D), Some(d), zero)}
        case "ANDY" => asmExpression.map { d => (AND, OneRegister(MEM_IY_D), Some(d), zero)}
        case "XRA" => intel8.map { r => (XOR, OneRegister(r), None, zero)}
        case "XORX" => asmExpression.map { d => (XOR, OneRegister(MEM_IX_D), Some(d), zero)}
        case "XORY" => asmExpression.map { d => (XOR, OneRegister(MEM_IY_D), Some(d), zero)}
        case "CMP" => intel8.map { r => (CP, OneRegister(r), None, zero)}
        case "CMPX" => asmExpression.map { d => (CP, OneRegister(MEM_IX_D), Some(d), zero)}
        case "CMPY" => asmExpression.map { d => (CP, OneRegister(MEM_IY_D), Some(d), zero)}
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
        case "LBCD" => asmExpression.map { e => (LD_16, TwoRegisters(BC, MEM_ABS_16), None, e)}
        case "SBCD" => asmExpression.map { e => (LD_16, TwoRegisters(MEM_ABS_16, BC), None, e)}
        case "LDED" => asmExpression.map { e => (LD_16, TwoRegisters(DE, MEM_ABS_16), None, e)}
        case "SDED" => asmExpression.map { e => (LD_16, TwoRegisters(MEM_ABS_16, DE), None, e)}
        case "LSPD" => asmExpression.map { e => (LD_16, TwoRegisters(SP, MEM_ABS_16), None, e)}
        case "SSPD" => asmExpression.map { e => (LD_16, TwoRegisters(MEM_ABS_16, SP), None, e)}
        case "LIXD" => asmExpression.map { e => (LD_16, TwoRegisters(IX, MEM_ABS_16), None, e)}
        case "SIXD" => asmExpression.map { e => (LD_16, TwoRegisters(MEM_ABS_16, IX), None, e)}
        case "LIYD" => asmExpression.map { e => (LD_16, TwoRegisters(IY, MEM_ABS_16), None, e)}
        case "SIYD" => asmExpression.map { e => (LD_16, TwoRegisters(MEM_ABS_16, IY), None, e)}
        case "STA" => asmExpression.map { e => (LD, TwoRegisters(MEM_ABS_8, A), None, e)}
        case "LDA" => asmExpression.map { e => (LD, TwoRegisters(A, MEM_ABS_8), None, e)}
        case "LDAI" => P("").map(_ => (LD, TwoRegisters(A, I), None, zero))
        case "LDAR" => P("").map(_ => (LD, TwoRegisters(A, R), None, zero))
        case "STAI" => P("").map(_ => (LD, TwoRegisters(I, A), None, zero))
        case "STAR" => P("").map(_ => (LD, TwoRegisters(R, A), None, zero))
        case "RST" => asmExpression.map {
          case LiteralExpression(value, _) if value >=0 && value <= 7=> (RST, NoRegisters, None, LiteralExpression(value * 8, 1))
          case _ =>
            log.error("Invalid parameter for RST", Some(pos))
            (NOP, NoRegisters, None, zero)
        }

        case "INP" => intel8.map { r => (IN_C, OneRegister(r), None, zero) }
        case "OUTP" => intel8.map { r => (OUT_C, OneRegister(r), None, zero) }
        case "IN" => asmExpression.map { e => (IN_IMM, OneRegister(A), None, e) }
        case "OUT" => asmExpression.map { e => (OUT_IMM, OneRegister(A), None, e) }

        case "DJNZ" => asmExpression.map { e => (DJNZ, NoRegisters, None, e)}
        case "JMP" => asmExpression.map { e => (JP, NoRegisters, None, e)}
        case "JMPR" | "JR" => asmExpression.map { e => (JR, NoRegisters, None, e)}
        case "JC" => asmExpression.map { e => (JP, IfFlagSet(ZFlag.C), None, e)}
        case "JZ" => asmExpression.map { e => (JP, IfFlagSet(ZFlag.Z), None, e)}
        case "JM" => asmExpression.map { e => (JP, IfFlagSet(ZFlag.S), None, e)}
        case "JPE" => asmExpression.map { e => (JP, IfFlagSet(ZFlag.P), None, e)}
        case "JNC" => asmExpression.map { e => (JP, IfFlagClear(ZFlag.C), None, e)}
        case "JNZ" => asmExpression.map { e => (JP, IfFlagClear(ZFlag.Z), None, e)}
        case "JP" => asmExpression.map { e => (JP, IfFlagClear(ZFlag.S), None, e)}
        case "JPO" => asmExpression.map { e => (JP, IfFlagClear(ZFlag.P), None, e)}
        case "JRC" => asmExpression.map { e => (JR, IfFlagSet(ZFlag.C), None, e)}
        case "JRZ" => asmExpression.map { e => (JR, IfFlagSet(ZFlag.Z), None, e)}
        case "JRM" => asmExpression.map { e => (JR, IfFlagSet(ZFlag.S), None, e)}
        case "JRPE" => asmExpression.map { e => (JR, IfFlagSet(ZFlag.P), None, e)}
        case "JRNC" => asmExpression.map { e => (JR, IfFlagClear(ZFlag.C), None, e)}
        case "JRNZ" => asmExpression.map { e => (JR, IfFlagClear(ZFlag.Z), None, e)}
        case "JRP" => asmExpression.map { e => (JR, IfFlagClear(ZFlag.S), None, e)}
        case "JRPO" => asmExpression.map { e => (JR, IfFlagClear(ZFlag.P), None, e)}
          
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

        case "DSUB" => imm(DSUB)
        case "JNK" | "JNX5" => asmExpression.map { e => (JP, IfFlagClear(ZFlag.K), None, e)}
        case "JK" | "JX5" => asmExpression.map { e => (JP, IfFlagSet(ZFlag.K), None, e)}
        case "RRHL" | "ARHL" => imm(RRHL)
        case "RLDE" | "RDEL" => imm(RLDE)
        case "RSTV" | "OVRST8" => imm(RSTV)
        case "LDSI" => asmExpression.map { e => (LD_DESP, NoRegisters, None, e)}
        case "LDHI" => asmExpression.map { e => (LD_DEHL, NoRegisters, None, e)}
        case "SHLDE" | "SHLX" => imm(SHLX)
        case "LHLDE" | "LHLX" =>imm(LHLX)


        case "RES" => parseSingleBitOpWithIntelSyntax(ZOpcodeClasses.RES_seq, "RES", pos)
        case "SETB" => parseSingleBitOpWithIntelSyntax(ZOpcodeClasses.SET_seq, "SETB", pos)
        case "SET" => parseSingleBitOpWithIntelSyntax(ZOpcodeClasses.SET_seq, "SET", pos)
        case "BIT" => parseSingleBitOpWithIntelSyntax(ZOpcodeClasses.BIT_seq, "BIT", pos)
        case "RESX" => parseSingleBitIndexedOpWithIntelSyntax(ZOpcodeClasses.RES_seq, MEM_IX_D, "RESX", pos)
        case "SETX" => parseSingleBitIndexedOpWithIntelSyntax(ZOpcodeClasses.SET_seq, MEM_IX_D, "SETX", pos)
        case "BITX" => parseSingleBitIndexedOpWithIntelSyntax(ZOpcodeClasses.BIT_seq, MEM_IX_D, "BITX", pos)
        case "RESY" => parseSingleBitIndexedOpWithIntelSyntax(ZOpcodeClasses.RES_seq, MEM_IY_D, "RESY", pos)
        case "SETY" => parseSingleBitIndexedOpWithIntelSyntax(ZOpcodeClasses.SET_seq, MEM_IY_D, "SETY", pos)
        case "BITY" => parseSingleBitIndexedOpWithIntelSyntax(ZOpcodeClasses.BIT_seq, MEM_IY_D, "BITY", pos)

        case _ =>
          log.error("Unsupported opcode " + opcode, Some(pos))
          imm(NOP)
      }
    } yield {
      val (actualOpcode, registers, offset, param) = tuple4
      Z80AssemblyStatement(actualOpcode, registers, offset, param, el)
    }
  }

  private def parseSingleBitOpWithIntelSyntax(OP_seq: IndexedSeq[ZOpcode.Value], opcodeString: String, pos: Position): P[(ZOpcode.Value, ZRegisters, Option[Expression], Expression)] = {
    import ZOpcode.NOP
    (param(allowAbsolute = false) ~ HWS ~ position("comma").map(_ => ()) ~ "," ~/ HWS ~ intel8).map {
      case (ZRegister.IMM_8, Some(LiteralExpression(n, _)), r2)
        if n >= 0 && n <= 7 && r2 != ZRegister.MEM_BC && r2 != ZRegister.MEM_DE =>
        (OP_seq(n.toInt), OneRegister(r2), None, zero)
      case _ =>
        log.error("Invalid parameters for " + opcodeString, Some(pos))
        (NOP, NoRegisters, None, zero)
    }
  }

  private def parseSingleBitIndexedOpWithIntelSyntax(OP_seq: IndexedSeq[ZOpcode.Value], memIndex: ZRegister.Value, opcodeString: String, pos: Position): P[(ZOpcode.Value, ZRegisters, Option[Expression], Expression)] = {
    import ZOpcode.NOP
    (param(allowAbsolute = false) ~ HWS ~ position("comma").map(_ => ()) ~ "," ~/ HWS ~ asmExpression).map {
      case (ZRegister.IMM_8, Some(LiteralExpression(n, _)), e2)
        if n >= 0 && n <= 7 =>
        (OP_seq(n.toInt), OneRegister(memIndex), Some(e2), zero)
      case _ =>
        log.error("Invalid parameters for " + opcodeString, Some(pos))
        (NOP, NoRegisters, None, zero)
    }
  }

  val asmInstruction: P[ExecutableStatement] = if (useIntelSyntax) intelAsmInstruction else zilogAsmInstruction

  private def imm(opcode: ZOpcode.Value): P[(ZOpcode.Value, ZRegisters, Option[Expression], Expression)] =
    P("").map(_=>(opcode, NoRegisters, None, zero))

  private def regA(opcode: ZOpcode.Value): P[(ZOpcode.Value, ZRegisters, Option[Expression], Expression)] =
    P("").map(_=>(opcode, OneRegister(ZRegister.A), None, zero))

  override val asmStatement: P[ExecutableStatement] = (position("assembly statement") ~ P(asmLabel | asmMacro | arrayContentsForAsm | asmInstruction)).map { case (p, s) => s.pos(p) } // TODO: macros

  override def validateAsmFunctionBody(p: Position, flags: Set[String], name: String, statements: Option[List[Statement]]): Unit = {
    if (!options.flag(CompilationFlag.BuggyCodeWarning)) return
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
