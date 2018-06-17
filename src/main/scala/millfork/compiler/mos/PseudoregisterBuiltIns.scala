package millfork.compiler.mos

import millfork.CompilationFlag
import millfork.assembly.mos.AddrMode._
import millfork.assembly.mos.Opcode._
import millfork.assembly.mos._
import millfork.compiler.{BranchSpec, CompilationContext, NoBranching}
import millfork.env._
import millfork.error.ErrorReporting
import millfork.node._

/**
  * @author Karol Stasiak
  */
object PseudoregisterBuiltIns {

  def compileWordAdditionToAX(ctx: CompilationContext, params: List[(Boolean, Expression)], decimal: Boolean): List[AssemblyLine] = {
    val b = ctx.env.get[Type]("byte")
    val w = ctx.env.get[Type]("word")
    if (!decimal) {
      val (variablePart, constPart) = ctx.env.evalVariableAndConstantSubParts(SumExpression(params, decimal = false))
      variablePart match {
        case None =>
          return MosExpressionCompiler.compileConstant(ctx, constPart, RegisterVariable(MosRegister.AX, w))
        case Some(v) =>
          val typ = MosExpressionCompiler.getExpressionType(ctx, v)
          if (typ.size == 1 && !typ.isSigned) {
            val bytePart = MosExpressionCompiler.compile(ctx, v, Some(b -> RegisterVariable(MosRegister.A, b)), BranchSpec.None)
            val label = MosCompiler.nextLabel("ah")
            return bytePart ++ List(
              AssemblyLine.implied(CLC),
              AssemblyLine.immediate(ADC, constPart.loByte),
              AssemblyLine.immediate(LDX, constPart.hiByte),
              AssemblyLine.relative(BCC, label),
              AssemblyLine.implied(INX),
              AssemblyLine.label(label)
            )
          }
      }
    }
    if (!ctx.options.flag(CompilationFlag.ZeropagePseudoregister)) {
      ErrorReporting.error("Word addition or subtraction requires the zeropage pseudoregister", params.headOption.flatMap(_._2.position))
      return Nil
    }
    if (params.isEmpty) {
      return List(AssemblyLine.immediate(LDA, 0), AssemblyLine.immediate(LDX, 0))
    }
    val reg = ctx.env.get[VariableInMemory]("__reg")
    val head = params.head match {
      case (false, e) => MosExpressionCompiler.compile(ctx, e, Some(MosExpressionCompiler.getExpressionType(ctx, e) -> reg), BranchSpec.None)
      case (true, e) => ???
    }
    params.tail.foldLeft[List[AssemblyLine]](head){case (code, (sub, param)) => code ++ addToReg(ctx, param, sub, decimal)} ++ List(
      AssemblyLine.zeropage(LDA, reg),
      AssemblyLine.zeropage(LDX, reg, 1),
    )
  }

  def addToReg(ctx: CompilationContext, r: Expression, subtract: Boolean, decimal: Boolean): List[AssemblyLine] = {
    if (!ctx.options.flag(CompilationFlag.ZeropagePseudoregister)) {
      ErrorReporting.error("Word addition or subtraction requires the zeropage pseudoregister", r.position)
      return Nil
    }
    val b = ctx.env.get[Type]("byte")
    val w = ctx.env.get[Type]("word")
    val reg = ctx.env.get[VariableInMemory]("__reg")
    // TODO: smarter on 65816
    val compileRight = MosExpressionCompiler.compile(ctx, r, Some(MosExpressionCompiler.getExpressionType(ctx, r) -> reg), BranchSpec.None)
    val op = if (subtract) SBC else ADC
    val prepareCarry = AssemblyLine.implied(if (subtract) SEC else CLC)
    compileRight match {

      case List(
      AssemblyLine(LDA, Immediate, NumericConstant(0, _), _),
      AssemblyLine(STA, ZeroPage, _, _),
      AssemblyLine(LDX | LDA, Immediate, NumericConstant(0, _), _),
      AssemblyLine(STA | STX, ZeroPage, _, _)) => Nil

      case List(
      l@AssemblyLine(LDA, _, _, _),
      AssemblyLine(STA, ZeroPage, _, _),
      h@AssemblyLine(LDX | LDA, addrMode, _, _),
      AssemblyLine(STA | STX, ZeroPage, _, _)) if addrMode != ZeroPageY => BuiltIns.wrapInSedCldIfNeeded(decimal,
        List(prepareCarry,
          AssemblyLine.zeropage(LDA, reg),
          l.copy(opcode = op),
          AssemblyLine.zeropage(STA, reg),
          AssemblyLine.zeropage(LDA, reg, 1),
          h.copy(opcode = op),
          AssemblyLine.zeropage(STA, reg, 1),
          AssemblyLine.zeropage(LDA, reg)))

      case _ => BuiltIns.wrapInSedCldIfNeeded(decimal,
        List(
          AssemblyLine.zeropage(LDA, reg, 1),
          AssemblyLine.implied(PHA),
          AssemblyLine.zeropage(LDA, reg),
          AssemblyLine.implied(PHA)) ++ compileRight ++ List(
          prepareCarry,
          AssemblyLine.implied(PLA),
          AssemblyLine.zeropage(op, reg),
          AssemblyLine.zeropage(STA, reg),
          AssemblyLine.implied(PLA),
          AssemblyLine.zeropage(op, reg, 1),
          AssemblyLine.zeropage(STA, reg, 1)))
    }
  }


  def compileWordBitOpsToAX(ctx: CompilationContext, params: List[Expression], op: Opcode.Value): List[AssemblyLine] = {
    if (!ctx.options.flag(CompilationFlag.ZeropagePseudoregister)) {
      ErrorReporting.error("Word bit operation requires the zeropage pseudoregister", params.headOption.flatMap(_.position))
      return Nil
    }
    if (params.isEmpty) {
      return List(AssemblyLine.immediate(LDA, 0), AssemblyLine.immediate(LDX, 0))
    }
    val b = ctx.env.get[Type]("byte")
    val w = ctx.env.get[Type]("word")
    val reg = ctx.env.get[VariableInMemory]("__reg")
    val head = MosExpressionCompiler.compile(ctx, params.head, Some(MosExpressionCompiler.getExpressionType(ctx, params.head) -> reg), BranchSpec.None)
    params.tail.foldLeft[List[AssemblyLine]](head){case (code, param) => code ++ bitOpReg(ctx, param, op)} ++ List(
      AssemblyLine.zeropage(LDA, reg),
      AssemblyLine.zeropage(LDX, reg, 1),
    )
  }

  def bitOpReg(ctx: CompilationContext, r: Expression, op: Opcode.Value): List[AssemblyLine] = {
    if (!ctx.options.flag(CompilationFlag.ZeropagePseudoregister)) {
      ErrorReporting.error("Word bit operation requires the zeropage pseudoregister", r.position)
      return Nil
    }
    val b = ctx.env.get[Type]("byte")
    val w = ctx.env.get[Type]("word")
    val reg = ctx.env.get[VariableInMemory]("__reg")
    // TODO: smarter on 65816
    val compileRight = MosExpressionCompiler.compile(ctx, r, Some(MosExpressionCompiler.getExpressionType(ctx, r) -> reg), BranchSpec.None)
    compileRight match {
      case List(
      AssemblyLine(LDA, Immediate, NumericConstant(0, _), _),
      AssemblyLine(STA, ZeroPage, _, _),
      AssemblyLine(LDX | LDA, Immediate, NumericConstant(0, _), _),
      AssemblyLine(STA | STX, ZeroPage, _, _))
        if op != AND => Nil

      case List(
      AssemblyLine(LDA, Immediate, NumericConstant(0xff, _), _),
      AssemblyLine(STA, ZeroPage, _, _),
      AssemblyLine(LDX | LDA, Immediate, NumericConstant(0xff, _), _),
      AssemblyLine(STA | STX, ZeroPage, _, _))
        if op == AND => Nil

      case List(
      l@AssemblyLine(LDA, _, _, _),
      AssemblyLine(STA, ZeroPage, _, _),
      h@AssemblyLine(LDX | LDA, addrMode, _, _),
      AssemblyLine(STA | STX, ZeroPage, _, _)) if addrMode != ZeroPageY =>
        List(
          AssemblyLine.zeropage(LDA, reg),
          l.copy(opcode = op),
          AssemblyLine.zeropage(STA, reg),
          AssemblyLine.zeropage(LDA, reg, 1),
          h.copy(opcode = op),
          AssemblyLine.zeropage(STA, reg, 1),
          AssemblyLine.zeropage(LDA, reg))

      case _ =>
        List(
          AssemblyLine.zeropage(LDA, reg, 1),
          AssemblyLine.implied(PHA),
          AssemblyLine.zeropage(LDA, reg),
          AssemblyLine.implied(PHA)) ++ compileRight ++ List(
          AssemblyLine.implied(PLA),
          AssemblyLine.zeropage(op, reg),
          AssemblyLine.zeropage(STA, reg),
          AssemblyLine.implied(PLA),
          AssemblyLine.zeropage(op, reg, 1),
          AssemblyLine.zeropage(STA, reg, 1))
    }
  }

  def compileWordShiftOps(left: Boolean, ctx: CompilationContext, l: Expression, r: Expression): List[AssemblyLine] = {
    if (!ctx.options.flag(CompilationFlag.ZeropagePseudoregister)) {
      ErrorReporting.error("Word shifting requires the zeropage pseudoregister", l.position)
      return Nil
    }
    val b = ctx.env.get[Type]("byte")
    val w = ctx.env.get[Type]("word")
    val reg = ctx.env.get[VariableInMemory]("__reg")
    val firstParamCompiled = MosExpressionCompiler.compile(ctx, l, Some(MosExpressionCompiler.getExpressionType(ctx, l) -> reg), NoBranching)
    ctx.env.eval(r) match {
      case Some(NumericConstant(0, _)) =>
        Nil
      case Some(NumericConstant(v, _)) if v > 0 =>
        if (ctx.options.flag(CompilationFlag.EmitNative65816Opcodes)) {
          firstParamCompiled ++
            List(AssemblyLine.accu16) ++
            List.fill(v.toInt)(if (left) AssemblyLine.zeropage(ASL_W, reg) else AssemblyLine.zeropage(LSR_W, reg)) ++
            List(AssemblyLine.accu8, AssemblyLine.zeropage(LDA, reg), AssemblyLine.zeropage(LDX, reg, 1))
        } else {
          val cycle =
            if (left) List(AssemblyLine.zeropage(ASL, reg), AssemblyLine.zeropage(ROL, reg, 1))
            else List(AssemblyLine.zeropage(LSR, reg, 1), AssemblyLine.zeropage(ROR, reg))
          firstParamCompiled ++ List.fill(v.toInt)(cycle).flatten ++ List(AssemblyLine.zeropage(LDA, reg), AssemblyLine.zeropage(LDX, reg, 1))
        }
      case _ =>
        val compileCounter = MosExpressionCompiler.compile(ctx, r, Some(b -> RegisterVariable(MosRegister.X, b)), NoBranching)
        val compileCounterAndPrepareFirstParam = compileCounter match {
          case List(AssemblyLine(LDX, _, _, _)) => firstParamCompiled ++ compileCounter
          case List(AssemblyLine(LDY, _, _, _), AssemblyLine(LDX, _, _, _)) => firstParamCompiled ++ compileCounter
          case _ =>
            MosExpressionCompiler.compile(ctx, r, Some(b -> RegisterVariable(MosRegister.A, b)), NoBranching) ++
              List(AssemblyLine.implied(PHA)) ++
              firstParamCompiled ++ (
              if (ctx.options.flag(CompilationFlag.EmitCmosOpcodes)) List(AssemblyLine.implied(PLX))
              else List(AssemblyLine.implied(PLA), AssemblyLine.implied(TAX))
              )
        }
        val labelRepeat = MosCompiler.nextLabel("sr")
        val labelSkip = MosCompiler.nextLabel("ss")
        if (ctx.options.flag(CompilationFlag.EmitNative65816Opcodes)) {
          compileCounterAndPrepareFirstParam ++ List(
            AssemblyLine.relative(BEQ, labelSkip),
            AssemblyLine.accu16,
            AssemblyLine.label(labelRepeat),
            AssemblyLine.zeropage(if (left) ASL_W else LSR_W, reg),
            AssemblyLine.implied(DEX),
            AssemblyLine.relative(BNE, labelRepeat),
            AssemblyLine.accu8,
            AssemblyLine.label(labelSkip),
            AssemblyLine.zeropage(LDA, reg),
            AssemblyLine.zeropage(LDX, reg, 1))
        } else {
          compileCounterAndPrepareFirstParam ++ List(
            AssemblyLine.relative(BEQ, labelSkip),
            AssemblyLine.label(labelRepeat),
            if (left) AssemblyLine.zeropage(ASL, reg) else AssemblyLine.zeropage(LSR, reg, 1),
            if (left) AssemblyLine.zeropage(ROL, reg, 1) else AssemblyLine.zeropage(ROR, reg),
            AssemblyLine.implied(DEX),
            AssemblyLine.relative(BNE, labelRepeat),
            AssemblyLine.label(labelSkip),
            AssemblyLine.zeropage(LDA, reg),
            AssemblyLine.zeropage(LDX, reg, 1))
        }
    }
  }

  def compileByteMultiplication(ctx: CompilationContext, param1OrRegister: Option[Expression], param2: Expression, storeInRegLo: Boolean): List[AssemblyLine] = {
    if (!ctx.options.flag(CompilationFlag.ZeropagePseudoregister)) {
      ErrorReporting.error("Variable byte multiplication requires the zeropage pseudoregister", param1OrRegister.flatMap(_.position))
      return Nil
    }
    val b = ctx.env.get[Type]("byte")
    val w = ctx.env.get[Type]("word")
    val reg = ctx.env.get[VariableInMemory]("__reg")
    val load: List[AssemblyLine] = param1OrRegister match {
      case Some(param1) =>
        val code1 = MosExpressionCompiler.compile(ctx, param1, Some(b -> RegisterVariable(MosRegister.A, b)), BranchSpec.None)
        val code2 = MosExpressionCompiler.compile(ctx, param2, Some(b -> RegisterVariable(MosRegister.A, b)), BranchSpec.None)
        if (!usesRegLo(code2)) {
          code1 ++ List(AssemblyLine.zeropage(STA, reg)) ++ code2 ++ List(AssemblyLine.zeropage(STA, reg, 1))
        } else if (!usesRegLo(code1)) {
          code2 ++ List(AssemblyLine.zeropage(STA, reg)) ++ code1 ++ List(AssemblyLine.zeropage(STA, reg, 1))
        } else {
          code1 ++ List(AssemblyLine.implied(PHA)) ++ code2 ++ List(
            AssemblyLine.zeropage(STA, reg),
            AssemblyLine.implied(PLA),
            AssemblyLine.zeropage(STA, reg, 1)
          )
        }
      case None =>
        val code2 = MosExpressionCompiler.compile(ctx, param2, Some(b -> RegisterVariable(MosRegister.A, b)), BranchSpec.None)
        if (!usesRegLo(code2)) {
          List(AssemblyLine.zeropage(STA, reg)) ++ code2 ++ List(AssemblyLine.zeropage(STA, reg, 1))
        } else if (!usesRegHi(code2)) {
          List(AssemblyLine.zeropage(STA, reg, 1)) ++ code2 ++ List(AssemblyLine.zeropage(STA, reg))
        } else {
          List(AssemblyLine.implied(PHA)) ++ code2 ++ List(
            AssemblyLine.zeropage(STA, reg),
            AssemblyLine.implied(PLA),
            AssemblyLine.zeropage(STA, reg, 1)
          )
        }
    }
    val calculate = AssemblyLine.absoluteOrLongAbsolute(JSR, ctx.env.get[FunctionInMemory]("__mul_u8u8u8"), ctx.options) ::
      (if (storeInRegLo) List(AssemblyLine.zeropage(STA, reg)) else Nil)
    load ++ calculate
  }

  private def simplicity(env: Environment, expr: Expression): Char = {
    val constPart = env.eval(expr) match {
      case Some(NumericConstant(_, _)) => 'Z'
      case Some(_) => 'Y'
      case None => expr match {
        case VariableExpression(_) => 'V'
        case IndexedExpression(_, LiteralExpression(_, _)) => 'K'
        case IndexedExpression(_, VariableExpression(_)) => 'J'
        case IndexedExpression(_, _) => 'I'
        case _ => 'A'
      }
    }
    constPart
  }

  def usesRegLo(code: List[AssemblyLine]): Boolean = code.forall{
    case AssemblyLine(JSR | BSR | TCD | TDC, _, _, _) => true
    case AssemblyLine(_, _, MemoryAddressConstant(th), _) if th.name == "__reg" => true
    case _ => false
  }

  def usesRegHi(code: List[AssemblyLine]): Boolean = code.forall{
    case AssemblyLine(JSR | BSR | TCD | TDC, _, _, _) => true
    case AssemblyLine(_, _, CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(1, _)), _) if th.name == "__reg" => true
    case _ => false
  }
}
