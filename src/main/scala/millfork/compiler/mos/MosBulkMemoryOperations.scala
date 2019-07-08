package millfork.compiler.mos

import millfork.CompilationFlag
import millfork.assembly.mos.{AddrMode, AssemblyLine, AssemblyLine0, Opcode}
import millfork.compiler.{AbstractExpressionCompiler, BranchSpec, CompilationContext}
import millfork.env.{ConstantPointy, Label, MemoryAddressConstant, MemoryVariable, NumericConstant, RelativeVariable, StackVariablePointy, Type, Variable, VariableAllocationMethod, VariableInMemory, VariablePointy}
import millfork.node._
import millfork.assembly.mos.Opcode._

/**
  * @author Karol Stasiak
  */
object MosBulkMemoryOperations {

  def compileMemset(ctx: CompilationContext, target: IndexedExpression, source: Expression, f: ForStatement): List[AssemblyLine] = {
    if (ctx.options.zpRegisterSize < 2 ||
      target.name != f.variable ||
      target.index.containsVariable(f.variable) ||
      !target.index.isPure ||
      f.direction == ForDirection.DownTo) return MosStatementCompiler.compileForStatement(ctx, f)._1
    ctx.env.getPointy(target.name)
    val sizeExpr = f.direction match {
      case ForDirection.DownTo =>
        SumExpression(List(false -> f.start, true -> f.end, false -> LiteralExpression(1, 1)), decimal = false)
      case ForDirection.To | ForDirection.ParallelTo =>
        SumExpression(List(false -> f.end, true -> f.start, false -> LiteralExpression(1, 1)), decimal = false)
      case ForDirection.Until | ForDirection.ParallelUntil =>
        SumExpression(List(false -> f.end, true -> f.start), decimal = false)
    }
    val reg = ctx.env.get[VariableInMemory]("__reg.loword")
    val w = ctx.env.get[Type]("word")
    val size = ctx.env.eval(sizeExpr) match {
      case Some(c) => c.quickSimplify
      case _ => return MosStatementCompiler.compileForStatement(ctx, f)._1
    }
    val useTwoRegs = ctx.options.flag(CompilationFlag.OptimizeForSpeed) && ctx.options.zpRegisterSize >= 4
    val loadReg =
      if (useTwoRegs) {
        import millfork.assembly.mos.AddrMode._
        val first = MosExpressionCompiler.compile(ctx, SumExpression(List(false -> f.start, false -> target.index), decimal = false), Some(w -> reg), BranchSpec.None)
        first ++ (first match {
          case List(AssemblyLine0(LDA, Immediate, l), AssemblyLine0(LDA, ZeroPage, r0), AssemblyLine0(LDA, Immediate, h), AssemblyLine0(LDA, ZeroPage, r1))
            if (r1-r0).quickSimplify.isProvably(1) =>
            val c = (h.asl(8) + l+ 0x80).quickSimplify
            List(
              AssemblyLine.immediate(LDA, c.loByte),
              AssemblyLine.zeropage(STA, reg, 2),
              AssemblyLine.immediate(LDA, c.subbyte(1)),
              AssemblyLine.zeropage(STA, reg, 3))
          case _ =>
            List(
              AssemblyLine.zeropage(LDA, reg),
              AssemblyLine.implied(CLC),
              AssemblyLine.immediate(ADC, 0x80),
              AssemblyLine.zeropage(STA, reg, 2),
              AssemblyLine.zeropage(LDA, reg, 1),
              AssemblyLine.immediate(ADC, 0),
              AssemblyLine.zeropage(STA, reg, 3))
        })
      }else MosExpressionCompiler.compile(ctx, SumExpression(List(false -> f.start, false -> target.index), decimal = false), Some(w -> reg), BranchSpec.None)

    val loadSource = MosExpressionCompiler.compileToA(ctx, source)
    val loadAll = if (MosExpressionCompiler.changesZpreg(loadSource, 0) || MosExpressionCompiler.changesZpreg(loadSource, 1)) {
      loadSource ++ MosExpressionCompiler.preserveRegisterIfNeeded(ctx, MosRegister.A, loadReg)
    } else {
      loadReg ++ loadSource
    }
    val wholePageCount = size.hiByte.quickSimplify

    def fillOnePage: List[AssemblyLine] = {
      val label = ctx.nextLabel("ms")
      if (useTwoRegs) {
        if (ctx.options.flag(CompilationFlag.OptimizeForSonicSpeed)) {
          List(
            AssemblyLine.immediate(LDY, 0x80),
            AssemblyLine.label(label),
            AssemblyLine.implied(DEY),
            AssemblyLine.indexedY(STA, reg),
            AssemblyLine.indexedY(STA, reg, 2),
            AssemblyLine.implied(DEY),
            AssemblyLine.indexedY(STA, reg),
            AssemblyLine.indexedY(STA, reg, 2),
            AssemblyLine.relative(BNE, label))
        } else if (ctx.options.flag(CompilationFlag.OptimizeForSpeed)) {
          List(
            AssemblyLine.immediate(LDY, 0x80),
            AssemblyLine.label(label),
            AssemblyLine.implied(DEY),
            AssemblyLine.indexedY(STA, reg),
            AssemblyLine.indexedY(STA, reg, 2),
            AssemblyLine.relative(BNE, label))
        } else ???
      } else {
        if (ctx.options.flag(CompilationFlag.OptimizeForSonicSpeed)) {
          List(
            AssemblyLine.immediate(LDY, 0),
            AssemblyLine.label(label),
            AssemblyLine.indexedY(STA, reg),
            AssemblyLine.implied(INY),
            AssemblyLine.indexedY(STA, reg),
            AssemblyLine.implied(INY),
            AssemblyLine.indexedY(STA, reg),
            AssemblyLine.implied(INY),
            AssemblyLine.indexedY(STA, reg),
            AssemblyLine.implied(INY),
            AssemblyLine.relative(BNE, label))
        } else if (ctx.options.flag(CompilationFlag.OptimizeForSpeed)) {
          List(
            AssemblyLine.immediate(LDY, 0),
            AssemblyLine.label(label),
            AssemblyLine.indexedY(STA, reg),
            AssemblyLine.implied(INY),
            AssemblyLine.indexedY(STA, reg),
            AssemblyLine.implied(INY),
            AssemblyLine.relative(BNE, label))
        } else {
          List(
            AssemblyLine.immediate(LDY, 0),
            AssemblyLine.label(label),
            AssemblyLine.indexedY(STA, reg),
            AssemblyLine.implied(INY),
            AssemblyLine.relative(BNE, label))
        }
      }
    }

    val setWholePages = wholePageCount match {
      case NumericConstant(0, _) => Nil
      case NumericConstant(1, _) =>
        fillOnePage
      case _ =>
        val labelX = ctx.nextLabel("ms")
        val labelXSkip = ctx.nextLabel("ms")
        List(
          AssemblyLine.immediate(LDX, wholePageCount),
          AssemblyLine.relative(BEQ, labelXSkip),
          AssemblyLine.label(labelX)) ++ fillOnePage ++
          (if (useTwoRegs) List(AssemblyLine.zeropage(INC, reg, 3)) else Nil) ++ List(
          AssemblyLine.zeropage(INC, reg, 1),
          AssemblyLine.implied(DEX),
          AssemblyLine.relative(BNE, labelX),
          AssemblyLine.label(labelXSkip))
    }
    val restSize = size.loByte.quickSimplify
    val setRest = restSize match {
      case NumericConstant(0, _) => Nil
      case NumericConstant(1, _) =>
        List(AssemblyLine.indexedY(STA, reg))
      case NumericConstant(2, _) => List(
        AssemblyLine.indexedY(STA, reg),
        AssemblyLine.implied(INY),
        AssemblyLine.indexedY(STA, reg))
      case _ =>
        val label = ctx.nextLabel("ms")
        val labelSkip = ctx.nextLabel("ms")
        if (f.direction == ForDirection.ParallelUntil) {
          List(
            AssemblyLine.immediate(LDY, restSize),
            AssemblyLine.relative(BEQ, labelSkip),
            AssemblyLine.label(label),
            AssemblyLine.implied(DEY),
            AssemblyLine.indexedY(STA, reg),
            AssemblyLine.relative(BNE, label),
            AssemblyLine.label(labelSkip))
        } else {
          List(
            AssemblyLine.immediate(LDY, 0),
            AssemblyLine.label(label),
            AssemblyLine.immediate(CPY, restSize),
            AssemblyLine.relative(BCS, labelSkip),
            AssemblyLine.indexedY(STA, reg),
            AssemblyLine.implied(INY),
            AssemblyLine.relative(BNE, label),
            AssemblyLine.label(labelSkip))
        }
    }
    loadAll ++ setWholePages ++ setRest
  }

  def compileMemmodify(ctx: CompilationContext, targetExpression: IndexedExpression, operator: String, source: Expression, f: ForStatement): Option[List[AssemblyLine]] = {
    val env = ctx.env
    if (AbstractExpressionCompiler.getExpressionType(ctx, source).size != 1) return None
    if (AbstractExpressionCompiler.getExpressionType(ctx, targetExpression).size != 1) return None
    val indexVariable = ctx.env.get[Variable](f.variable)
    if (indexVariable.isVolatile) return None
    if (f.direction != ForDirection.ParallelUntil) return None
    if (!env.eval(f.start).exists(_.isProvablyZero)) return None
    val offset = env.evalVariableAndConstantSubParts(targetExpression.index) match {
      case (Some(VariableExpression(v)), o) if v == f.variable => o
      case _ => return None
    }
    val reg = env.get[VariableInMemory]("__reg.loword")
    val (prepareZpreg, addrMode, parameter) = env.getPointy(targetExpression.name) match {
      case _: VariablePointy | _:StackVariablePointy =>
        val offsetExpr = GeneratedConstantExpression(offset, env.get[Type]("word"))
        val pz = MosExpressionCompiler.compileToZReg(ctx, SumExpression(List(false -> VariableExpression(targetExpression.name), false -> offsetExpr), decimal = false))
        (pz, AddrMode.IndexedY, reg.toAddress)
      case c: ConstantPointy =>
        if (indexVariable.typ.size == 1) (Nil, AddrMode.AbsoluteX, c.value)
        else {
          val pz = MosExpressionCompiler.compileToZReg(ctx, GeneratedConstantExpression(c.value + offset, env.get[Type]("pointer")))
          (pz, AddrMode.IndexedY, reg.toAddress)
        }
    }
    def wrapLdaSta(code: List[AssemblyLine]) : List[AssemblyLine] = AssemblyLine(LDA, addrMode, parameter) :: (code :+AssemblyLine(STA, addrMode, parameter))
    val body = env.eval(source) match {
      case Some(NumericConstant(0, _)) => return Some(MosExpressionCompiler.compile(ctx, f.end, None, BranchSpec.None))
      case Some(NumericConstant(n, _)) if operator == "<<=" && n > 0 =>
        addrMode match {
          case AddrMode.AbsoluteX if n <= 2 => List.fill(n.toInt)(AssemblyLine.absoluteX(ASL, parameter))
          case _ => wrapLdaSta(List.fill(n.toInt)(AssemblyLine.implied(ASL)))
        }
      case Some(NumericConstant(n, _)) if operator == ">>=" && n > 0 =>
        addrMode match {
          case AddrMode.AbsoluteX if n <= 2 => List.fill(n.toInt)(AssemblyLine.absoluteX(LSR, parameter))
          case _ => wrapLdaSta(List.fill(n.toInt)(AssemblyLine.implied(LSR)))
        }
      case Some(NumericConstant(n@(1 | 2), _)) if operator == "+=" && addrMode == AddrMode.AbsoluteX =>
        List.fill(n.toInt)(AssemblyLine.absoluteX(INC, parameter))
      case Some(NumericConstant(n@(1 | 2), _)) if operator == "-=" && addrMode == AddrMode.AbsoluteX =>
        List.fill(n.toInt)(AssemblyLine.absoluteX(DEC, parameter))
      case _ if operator == ">>=" || operator == "<<=" => return None
      case Some(c) if operator == "+=" => wrapLdaSta(List(AssemblyLine.implied(CLC), AssemblyLine.immediate(ADC, c)))
      case Some(c) if operator == "-=" => wrapLdaSta(List(AssemblyLine.implied(SEC), AssemblyLine.immediate(SBC, c)))
      case Some(c) if operator == "&=" => wrapLdaSta(List(AssemblyLine.immediate(AND, c)))
      case Some(c) if operator == "|=" => wrapLdaSta(List(AssemblyLine.immediate(ORA, c)))
      case Some(c) if operator == "^=" => wrapLdaSta(List(AssemblyLine.immediate(EOR, c)))
      case _ => return None // TODO
    }
    val TAL = addrMode match {
      case AddrMode.AbsoluteX => TAX
      case AddrMode.IndexedY => TAY
    }
    val TLA = addrMode match {
      case AddrMode.AbsoluteX => TXA
      case AddrMode.IndexedY => TYA
    }
    val DEL = addrMode match {
      case AddrMode.AbsoluteX => DEX
      case AddrMode.IndexedY => DEY
    }
    indexVariable.typ.size match {
      case 1 =>
        val end = MosExpressionCompiler.compileToA(ctx, f.end)
        val prepareAll = if (addrMode == AddrMode.IndexedY && (MosExpressionCompiler.changesZpreg(end, 0) || MosExpressionCompiler.changesZpreg(end, 1))) {
          end ++ List(AssemblyLine.implied(PHA)) ++ MosExpressionCompiler.fixTsx(prepareZpreg) ++ List(AssemblyLine.implied(PLA), AssemblyLine.implied(TAL))
        } else {
          prepareZpreg ++ end ++ List(AssemblyLine.implied(TAL))
        }
        val label = ctx.nextLabel("fo")
        Some(prepareAll ++ List(AssemblyLine.label(label), AssemblyLine.implied(DEL)) ++ body ++ List(AssemblyLine.implied(TLA), AssemblyLine.relative(BNE, label)))
      case 2 =>
        // TODO
        None
      case _ => None
    }
  }

  def compileFold(ctx: CompilationContext, targetExpression: VariableExpression, operator: String, source: Expression, f: ForStatement): Option[List[AssemblyLine]] = {
    import AddrMode._
    import ForDirection._
    val target = ctx.env.maybeGet[Variable](targetExpression.name).getOrElse(return None)
    if (target.isVolatile) return None
    val indexVariable = ctx.env.get[Variable](f.variable)
    if (indexVariable.isVolatile || indexVariable.typ.size != 1) return None
    indexVariable match {
      case v: MemoryVariable =>
        if (v.alloc == VariableAllocationMethod.Static) return None
      case _ => return None
    }
    val sourceType = AbstractExpressionCompiler.getExpressionType(ctx, source)
    if (sourceType.size != 1) return None
    if (!sourceType.isArithmetic) return None
    if (operator == "+=") {
      if (!target.typ.isArithmetic) return None
      if (target.typ.size > 2) return None
      if (target.typ.size == 2 && sourceType.isSigned) return None
    } else {
      if (!target.typ.isArithmetic) return None
      if (target.typ.size != 1) return None
    }
    val loadSource = MosExpressionCompiler.compileToA(ctx, source) match {
      case List(AssemblyLine0(LDY, Absolute | ZeroPage, MemoryAddressConstant(index)), l@AssemblyLine0(LDA, AbsoluteY | IndexedY, _)) if index.name == indexVariable.name => l
      case List(l@AssemblyLine0(LDA, Absolute | ZeroPage | Immediate, _)) => l
      case _ => return None
    }

    def isSimple(l: List[AssemblyLine]): Boolean = l match {
      case List(l@AssemblyLine0(LDA, Absolute | ZeroPage | Immediate, _)) => true
      case _ => false
    }

    def isConstant(l: List[AssemblyLine], c: Long => Boolean): Boolean = l match {
      case List(l@AssemblyLine0(LDA, Immediate, NumericConstant(n, _))) => c(n)
      case _ => false
    }

    def targetifyY(l: List[AssemblyLine]): List[AssemblyLine] = if (isSimple(l)) l.map(_.copy(opcode = LDY)) else l :+ AssemblyLine.implied(TAY)

    def toCpy(l: List[AssemblyLine], branch: Opcode.Value, label: String): List[AssemblyLine] = if (isSimple(l)) l.map(_.copy(opcode = CPY)) :+ AssemblyLine.relative(branch, label) else throw new IllegalArgumentException
    def branch(branch: Opcode.Value, label: String): List[AssemblyLine] = if (branch == JMP) List(AssemblyLine.absolute(JMP, Label(label))) else List(AssemblyLine.relative(branch, label))

    val loadStart = MosExpressionCompiler.compileToA(ctx, f.start)
    val loadEnd = MosExpressionCompiler.compileToA(ctx, f.end)

    val dey = AssemblyLine.implied(DEY)
    val iny = AssemblyLine.implied(INY)
    lazy val skipLabel = ctx.nextLabel("fo") // TODO
    val loopLabel = ctx.nextLabel("fo") // TODO
    val loopStart = if (operator.contains("'")) List(AssemblyLine.implied(SED), AssemblyLine.label(loopLabel)) else List(AssemblyLine.label(loopLabel))
    val cld = if (operator.contains("'")) List(AssemblyLine.implied(CLD)) else Nil
    lazy val loopSkip = List(AssemblyLine.label(skipLabel))

    val frame: (List[AssemblyLine], List[AssemblyLine]) = f.direction match {
      case ParallelTo | DownTo if isConstant(loadStart, _ == 0) && isConstant(loadEnd, i => i > 0 && i <= 0x7f) =>
        (targetifyY(loadEnd) ++ loopStart) -> (dey :: branch(BPL, loopLabel) ++ cld)
      case ParallelTo | To if isConstant(loadStart, i => i > 0 && i < 255) && isConstant(loadEnd, _ == 255) =>
        (targetifyY(loadStart) ++ loopStart) -> (iny :: branch(BNE, loopLabel) ++ cld)
      case ParallelUntil if isConstant(loadStart, _ == 0) && isConstant(loadEnd, i => i > 0 && i <= 0x7f) =>
        (targetifyY(loadEnd).map(l => l.copy(parameter = l.parameter - 1)) ++ loopStart) -> (dey :: branch(BPL, loopLabel) ++ cld)
      case ParallelUntil if isConstant(loadStart, _ == 0) =>
        (targetifyY(loadEnd) ++ loopStart :+ dey) -> (toCpy(loadStart, BNE, loopLabel) ++ cld)
      case Until | ParallelUntil =>
        if (isSimple(loadEnd)) {
          if (cld.isEmpty) {
            (targetifyY(loadStart) ++ branch(JMP, skipLabel) ++ loopStart) -> (iny :: loopSkip ++ toCpy(loadEnd, BNE, loopLabel))
          } else {
            (targetifyY(loadStart) ++ toCpy(loadEnd, BEQ, skipLabel) ++ loopStart) -> (iny :: toCpy(loadEnd, BNE, loopLabel) ++ cld ++ loopSkip)
          }
        } else return None
      case To | ParallelTo | DownTo =>
        return None
    }
    val opcode = operator match {
      case "+=" | "+'=" => ADC
      case "-=" | "-'=" => SBC
      case "|=" => ORA
      case "&=" => AND
      case "^=" => EOR
      case _ => throw new IllegalArgumentException(operator)
    }
    val carry = operator match {
      case "+=" | "+'=" => List(AssemblyLine.implied(CLC))
      case "-=" | "-'=" => List(AssemblyLine.implied(SEC))
      case _ => Nil
    }
    val incHi = if (target.typ.size == 2) {
      val l = ctx.nextLabel("ah")
      List(AssemblyLine.relative(BCC, l), AssemblyLine.implied(INX), AssemblyLine.label(l))
    } else Nil

    val body = (carry ++ List(loadSource.copy(opcode = opcode)) ++ incHi).map(_.position(targetExpression.position))

    val loadTarget = if (target.typ.size == 2) MosExpressionCompiler.compileToAX(ctx, targetExpression) else MosExpressionCompiler.compileToA(ctx, targetExpression)
    val storeTarget =
      if (target.typ.size == 2) MosExpressionCompiler.expressionStorageFromAX(ctx, Some(target.typ -> target), targetExpression.position)
      else MosExpressionCompiler.expressionStorageFromA(ctx, Some(target.typ -> target), targetExpression.position)

    Some(loadTarget ++ frame._1 ++ body ++ frame._2 ++ storeTarget)
  }
}
