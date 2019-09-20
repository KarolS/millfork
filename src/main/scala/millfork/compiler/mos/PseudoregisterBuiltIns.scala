package millfork.compiler.mos

import millfork.CompilationFlag
import millfork.assembly.mos.AddrMode._
import millfork.assembly.mos.Opcode._
import millfork.assembly.mos._
import millfork.compiler.{AbstractExpressionCompiler, BranchSpec, CompilationContext, NoBranching}
import millfork.env._
import millfork.error.ConsoleLogger
import millfork.node._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * @author Karol Stasiak
  */
object PseudoregisterBuiltIns {

  def compileWordAdditionViaAX(ctx: CompilationContext, target: Option[(Type, Variable)], position: Option[Position], params: List[(Boolean, Expression)], decimal: Boolean): List[AssemblyLine] = {
    val b = ctx.env.get[Type]("byte")
    val w = ctx.env.get[Type]("word")
    val store = MosExpressionCompiler.expressionStorageFromAX(ctx, target, position)
    if (!decimal && params.length == 2) {
      store match {
        case List(AssemblyLine0(STA, Absolute | ZeroPage, _), AssemblyLine0(STX, Absolute | ZeroPage, _)) =>
          val reads = params.map(p => p._1 -> MosExpressionCompiler.compileToAX(ctx, p._2))
          fastBinaryWordAddition(reads, store.map(x => List(x.copy(opcode = STA)))).foreach(return _)
        case _ =>
      }
    }
    compileWordAdditionToAX(ctx, params, decimal = decimal) ++ store
  }

  private def fastBinaryWordAddition(reads: List[(Boolean, List[AssemblyLine])], stores: List[List[AssemblyLine]]): Option[List[AssemblyLine]] = {
    val countYs = reads.count(_._2.exists(_.concernsY))
    if (countYs > 1 || countYs == 1 && stores.exists(_.exists(_.concernsY))) return None
    val result = ListBuffer[AssemblyLine]()
    var hard = Option.empty[List[AssemblyLine]]
    val niceReads = mutable.ListBuffer[(List[AssemblyLine], List[AssemblyLine])]()
    var constant: Constant = NumericConstant(0, 2)
    var counter = 0
    for ((subtract, read) <- reads) {
      read match {
        case List(AssemblyLine0(LDA, Immediate, l), AssemblyLine0(LDX, Immediate, h)) =>
          if (subtract) constant -= h.asl(8).+(l).quickSimplify
          else constant += h.asl(8).+(l).quickSimplify
        case List(l@AssemblyLine0(LDA, Absolute | ZeroPage | Immediate, _), h@AssemblyLine0(LDX, Absolute | ZeroPage | Immediate, _)) =>
          if (subtract) niceReads.+=(List(AssemblyLine.implied(SEC), l.copy(opcode = SBC)) -> List(h.copy(opcode = SBC)))
          else niceReads += (List(AssemblyLine.implied(CLC), l.copy(opcode = ADC)) -> List(h.copy(opcode = ADC)))
          counter += 1
        case _ =>
          if (hard.isDefined || subtract) return None
          hard = Some(read)
          counter += 1
      }
    }
    if (!constant.isProvablyZero) {
      counter += 1
    }
    if (counter > 2) {
      return None
    }
    hard match {
      case Some(ax) =>
        niceReads.prepend(ax -> List(AssemblyLine.implied(TXA)))
        if (!constant.isProvablyZero) {
          if (constant.isQuiteNegative) {
            niceReads += List(AssemblyLine.implied(CLC), AssemblyLine.immediate(ADC, constant.loByte)) -> List(AssemblyLine.immediate(ADC, constant.hiByte))
          } else {
            val negC = Constant.Zero.-(constant).quickSimplify.quickSimplify
            niceReads += List(AssemblyLine.implied(SEC), AssemblyLine.immediate(SBC, negC.loByte)) -> List(AssemblyLine.immediate(SBC, negC.hiByte))
          }
        }
      case None =>
        if (constant.isProvablyZero) {
          if (niceReads.nonEmpty && niceReads.head._1.head.opcode == SEC) return None
          niceReads(0) = (List(niceReads.head._1.last.copy(opcode = LDA)) -> List(niceReads.head._2.last.copy(opcode = LDA)))
        } else {
          niceReads.prepend(List(AssemblyLine.immediate(LDA, constant.loByte)) -> List(AssemblyLine.immediate(LDA, constant.hiByte)))
        }
    }
    if (doesMemoryAccessOverlap(niceReads.flatMap(_._1).toList, stores(1))) return None
    if (doesMemoryAccessOverlap(niceReads.flatMap(_._2).toList, stores(0))) return None
    for (b <- 0 to 1) {
      for (read <- niceReads) {
        if (b == 0) result ++= read._1
        else result ++= read._2
      }
      result ++= stores(b)
    }
    Some(result.toList)
  }

  def compileWordAdditionToAX(ctx: CompilationContext, params: List[(Boolean, Expression)], decimal: Boolean): List[AssemblyLine] = {
    val b = ctx.env.get[Type]("byte")
    val w = ctx.env.get[Type]("word")
    if (!decimal) {
      val (variablePart, constPart) = ctx.env.evalVariableAndConstantSubParts(SumExpression(params, decimal = false))
      variablePart match {
        case None =>
          return MosExpressionCompiler.compileConstant(ctx, constPart, w, RegisterVariable(MosRegister.AX, w))
        case Some(v) =>
          val typ = MosExpressionCompiler.getExpressionType(ctx, v)
          if (typ.size == 1 && !typ.isSigned) {
            val bytePart = MosExpressionCompiler.compile(ctx, v, Some(b -> RegisterVariable(MosRegister.A, b)), BranchSpec.None)
            val label = ctx.nextLabel("ah")
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
    if (ctx.options.zpRegisterSize < 2) {
      ctx.log.error("Word addition or subtraction requires the zeropage pseudoregister", params.headOption.flatMap(_._2.position))
      return Nil
    }
    if (params.isEmpty) {
      return List(AssemblyLine.immediate(LDA, 0), AssemblyLine.immediate(LDX, 0))
    }
    val reg = ctx.env.get[VariableInMemory]("__reg.loword")
    val addToRegs = params.map{ case (sub, param) => addToReg(ctx, param, sub, decimal) }
    val newHead = params.indices.find{ i =>
      !params(i)._1 && addToRegs(i).exists(l => l.opcode == PHA || l.opcode == PHA_W)
    }.getOrElse(0)
    val head = params(newHead) match {
      case (false, e) => MosExpressionCompiler.compile(ctx, e, Some(MosExpressionCompiler.getExpressionType(ctx, e) -> reg), BranchSpec.None)
      case (true, e) => ???
    }
    params.indices.filter(_ != newHead).foldLeft[List[AssemblyLine]](head){case (code, index) => code ++ addToRegs(index)} ++ List(
      AssemblyLine.zeropage(LDA, reg),
      AssemblyLine.zeropage(LDX, reg, 1),
    )
  }

  def compileWordAdditionToAW(ctx: CompilationContext, params: List[(Boolean, Expression)], decimal: Boolean): List[AssemblyLine] = {
    // assume native16 is enabled and accu16 mode is disabled
    val b = ctx.env.get[Type]("byte")
    val w = ctx.env.get[Type]("word")
    if (!decimal) {
      val (variablePart, constPart) = ctx.env.evalVariableAndConstantSubParts(SumExpression(params, decimal = false))
      variablePart match {
        case None =>
          return MosExpressionCompiler.compileConstant(ctx, constPart, w, RegisterVariable(MosRegister.AW, w))
        case Some(v) =>
      }
    }
    if (ctx.options.zpRegisterSize < 2) {
      ctx.log.error("Word addition or subtraction requires the zeropage pseudoregister", params.headOption.flatMap(_._2.position))
      return Nil
    }
    if (params.isEmpty) {
      return List(AssemblyLine.accu16, AssemblyLine.immediate(LDA_W, 0), AssemblyLine.accu8)
    }
    val reg = ctx.env.get[VariableInMemory]("__reg.loword")
    val addToRegs = params.map{ case (sub, param) => addToReg(ctx, param, sub, decimal) }
    val newHead = params.indices.find{ i =>
      !params(i)._1 && addToRegs(i).exists(l => l.opcode == PHA || l.opcode == PHA_W)
    }.getOrElse(0)
    val head = params(newHead) match {
      case (false, e) => MosExpressionCompiler.compile(ctx, e, Some(MosExpressionCompiler.getExpressionType(ctx, e) -> reg), BranchSpec.None)
      case (true, e) => ???
    }
    params.indices.filter(_ != newHead).foldLeft[List[AssemblyLine]](head){case (code, index) => code ++ addToRegs(index)} ++ List(
      AssemblyLine.accu16,
      AssemblyLine.zeropage(LDA_W, reg),
      AssemblyLine.accu8
    )
  }

  def addToReg(ctx: CompilationContext, r: Expression, subtract: Boolean, decimal: Boolean): List[AssemblyLine] = {
    if (ctx.options.zpRegisterSize < 2) {
      ctx.log.error("Word addition or subtraction requires the zeropage pseudoregister", r.position)
      return Nil
    }
    if (decimal && !ctx.options.flag(CompilationFlag.DecimalMode) && ctx.options.zpRegisterSize < 4) {
      ctx.log.error("Unsupported decimal operation. Consider increasing the size of the zeropage register.", r.position)
      return Nil
    }
    val native16 = ctx.options.flag(CompilationFlag.EmitNative65816Opcodes)
    val b = ctx.env.get[Type]("byte")
    val w = ctx.env.get[Type]("word")
    val reg = ctx.env.get[VariableInMemory]("__reg.loword")
    // TODO: smarter on 65816
    val op = if (subtract) SBC else ADC
    val op16 = if (subtract) SBC_W else ADC_W
    val prepareCarry = AssemblyLine.implied(if (subtract) SEC else CLC)
    val rightType = MosExpressionCompiler.getExpressionType(ctx, r)
    if (decimal && !ctx.options.flag(CompilationFlag.DecimalMode)) {
      val compileRight = MosExpressionCompiler.compile(ctx, r, Some(rightType -> ctx.env.get[VariableInMemory]("__reg.b2b3")), BranchSpec.None)
      compileRight match {
            case List(
            AssemblyLine0(LDA, Immediate, NumericConstant(0, _)),
            AssemblyLine0(STA, ZeroPage, _),
            AssemblyLine0(LDX | LDA, Immediate, NumericConstant(0, _)),
            AssemblyLine0(STA | STX, ZeroPage, _)) => Nil
            case _ =>
              compileRight ++
              List(
                AssemblyLine.zeropage(LDX, reg, 3),
                AssemblyLine.zeropage(LDA, reg, 2),
                AssemblyLine.zeropage(STA, reg, 3),
                AssemblyLine.implied(TXA),
                AssemblyLine.implied(PHA),
                AssemblyLine.zeropage(LDA, reg),
                AssemblyLine.zeropage(STA, reg, 2)) ++ (
                if (subtract) List(AssemblyLine.absolute(JSR, ctx.env.get[ThingInMemory]("__sub_decimal")))
                else List(AssemblyLine.implied(CLC), AssemblyLine.absolute(JSR, ctx.env.get[ThingInMemory]("__adc_decimal")))
              ) ++ List(
                AssemblyLine.zeropage(STA, reg),
                AssemblyLine.implied(PLA),
                AssemblyLine.zeropage(STA, reg, 3),
                AssemblyLine.zeropage(LDA, reg, 1),
                AssemblyLine.zeropage(STA, reg, 2),
                AssemblyLine.absolute(JSR,
                  if (subtract) ctx.env.get[ThingInMemory]("__sbc_decimal")
                  else ctx.env.get[ThingInMemory]("__adc_decimal")),
                AssemblyLine.zeropage(STA, reg, 1))
          }
    } else {
      val compileRight = MosExpressionCompiler.compile(ctx, r, Some(rightType -> reg), BranchSpec.None)
      compileRight match {

        case List(
        AssemblyLine0(LDA, Immediate, NumericConstant(0, _)),
        AssemblyLine0(STA, ZeroPage, _),
        AssemblyLine0(LDX | LDA, Immediate, NumericConstant(0, _)),
        AssemblyLine0(STA | STX, ZeroPage, _)) => Nil
        case List(
        AssemblyLine0(REP, Immediate, NumericConstant(0x20, _)),
        AssemblyLine0(LDA_W, WordImmediate, NumericConstant(0, _)),
        AssemblyLine0(STA_W, ZeroPage, _),
        AssemblyLine0(SEP, Immediate, NumericConstant(0x20, _))) => Nil

        case List(
        l@AssemblyLine0(LDA, _, _),
        AssemblyLine0(STA, ZeroPage, _),
        h@AssemblyLine0(LDX | LDA, addrMode, _),
        AssemblyLine0(STA | STX, ZeroPage, _)) if addrMode != ZeroPageY => BuiltIns.wrapInSedCldIfNeeded(decimal,
          List(prepareCarry,
            AssemblyLine.zeropage(LDA, reg),
            l.copy(opcode = op),
            AssemblyLine.zeropage(STA, reg),
            AssemblyLine.zeropage(LDA, reg, 1),
            h.copy(opcode = op),
            AssemblyLine.zeropage(STA, reg, 1)))
        case List(
        AssemblyLine0(REP, Immediate, NumericConstant(0x20, _)),
        l@AssemblyLine0(LDA_W, addrMode, _),
        AssemblyLine0(STA_W, ZeroPage, _),
        AssemblyLine0(SEP, Immediate, NumericConstant(0x20, _))) if addrMode != ZeroPageY =>
          BuiltIns.wrapInSedCldIfNeeded(decimal, List(
            AssemblyLine.accu16,
            prepareCarry,
            AssemblyLine.zeropage(LDA_W, reg),
            l.copy(opcode = op16),
            AssemblyLine.zeropage(STA_W, reg),
            AssemblyLine.accu8))

        case _ =>
          if (native16) {
            List(AssemblyLine.accu16, AssemblyLine.zeropage(LDA_W, reg), AssemblyLine.implied(PHA_W), AssemblyLine.accu8) ++
              MosExpressionCompiler.fixTsx(MosExpressionCompiler.fixTsx(compileRight)) ++
              List(AssemblyLine.accu16, prepareCarry, AssemblyLine.implied(PLA_W), AssemblyLine.zeropage(op16, reg), AssemblyLine.zeropage(STA_W, reg), AssemblyLine.accu8)
          } else List(
            AssemblyLine.zeropage(LDA, reg, 1),
            AssemblyLine.implied(PHA),
            AssemblyLine.zeropage(LDA, reg),
            AssemblyLine.implied(PHA)) ++ MosExpressionCompiler.fixTsx(MosExpressionCompiler.fixTsx(compileRight)) ++
            BuiltIns.wrapInSedCldIfNeeded(decimal, List(
              prepareCarry,
              AssemblyLine.implied(PLA),
              AssemblyLine.zeropage(op, reg),
              AssemblyLine.zeropage(STA, reg),
              AssemblyLine.implied(PLA),
              AssemblyLine.zeropage(op, reg, 1),
              AssemblyLine.zeropage(STA, reg, 1)))
      }
    }
  }


  private def compileFastWordBitOpsToAX(reads: List[List[AssemblyLine]], op: Opcode.Value): Option[List[AssemblyLine]] = {
    val resultLo = mutable.ListBuffer[AssemblyLine]()
    val resultHi = mutable.ListBuffer[AssemblyLine]()
    var complex = Option.empty[List[AssemblyLine]]
    for(read <- reads) {
      read match {
        case List(l@AssemblyLine0(LDA, Immediate | Absolute | ZeroPage, _), h@AssemblyLine0(LDX, Immediate | Absolute | ZeroPage, _)) =>
          if (resultHi.isEmpty) {
            resultHi += h.copy(opcode = LDA)
            resultLo += l
          } else {
            resultHi += h.copy(opcode = op)
            resultLo += l.copy(opcode = op)
          }
        case _ =>
          if (complex.isDefined) return None
          else complex = Some(read)
      }
    }
    complex match {
      case Some(x) =>
        if (resultHi.isEmpty) {
          if (resultLo.isEmpty) return Some(x)
          else return None
        } else {
          if (resultLo.isEmpty) return None
          if (resultHi.exists(_.concernsX)) return None
          if (resultLo.exists(_.concernsX)) return None
          if (resultHi.head.opcode != LDA) return None
          if (resultLo.head.opcode != LDA) return None
          resultHi(0) = resultHi.head.copy(opcode = op)
          resultLo(0) = resultLo.head.copy(opcode = op)
        }
        val index = x.lastIndexWhere(_.opcode.==(TAX))
        if (index < 0) return None
        val (handleHigh, taxAndHandleLo) = x.splitAt(index)
        if (taxAndHandleLo.tail.exists(l => l.concernsX || (l.opcode match {
          case TAX | CLC | SEC | CLD | SED => false
          case LDA | ADC | SBC | AND | ORA | EOR => false
          case ASL | LSR | ROL | ROR => false
          case ANC | ALR | ARR => false
          case NOP => false
          case INC | DEC => l.addrMode != Implied
          case _ => true
        }))) return None
        Some(handleHigh ++ resultHi ++ taxAndHandleLo ++ resultLo)
      case None =>
        resultHi += AssemblyLine.implied(TAX)
        resultHi ++= resultLo
        Some(resultHi.toList)
    }
  }

  def compileWordBitOpsToAX(ctx: CompilationContext, params: List[Expression], op: Opcode.Value): List[AssemblyLine] = {
    if (ctx.options.zpRegisterSize < 2) {
      ctx.log.error("Word bit operation requires the zeropage pseudoregister", params.headOption.flatMap(_.position))
      return Nil
    }
    if (params.isEmpty) {
      return List(AssemblyLine.immediate(LDA, 0), AssemblyLine.immediate(LDX, 0))
    }
    compileFastWordBitOpsToAX(params.map(p => MosExpressionCompiler.compileToAX(ctx, p)), op).foreach(return _)

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
    if (ctx.options.zpRegisterSize < 2) {
      ctx.log.error("Word bit operation requires the zeropage pseudoregister", r.position)
      return Nil
    }
    val b = ctx.env.get[Type]("byte")
    val w = ctx.env.get[Type]("word")
    val reg = ctx.env.get[VariableInMemory]("__reg")
    // TODO: smarter on 65816
    val compileRight = MosExpressionCompiler.compile(ctx, r, Some(MosExpressionCompiler.getExpressionType(ctx, r) -> reg), BranchSpec.None)
    compileRight match {
      case List(
      AssemblyLine0(LDA, Immediate, NumericConstant(0, _)),
      AssemblyLine0(STA, ZeroPage, _),
      AssemblyLine0(LDX | LDA, Immediate, NumericConstant(0, _)),
      AssemblyLine0(STA | STX, ZeroPage, _))
        if op != AND => Nil

      case List(
      AssemblyLine0(LDA, Immediate, NumericConstant(0xff, _)),
      AssemblyLine0(STA, ZeroPage, _),
      AssemblyLine0(LDX | LDA, Immediate, NumericConstant(0xff, _)),
      AssemblyLine0(STA | STX, ZeroPage, _))
        if op == AND => Nil

      case List(
      l@AssemblyLine0(LDA, _, _),
      AssemblyLine0(STA, ZeroPage, _),
      h@AssemblyLine0(LDX | LDA, addrMode, _),
      AssemblyLine0(STA | STX, ZeroPage, _)) if addrMode != ZeroPageY =>
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

  private def unrollShift(ctx: CompilationContext, count: Long, bodySize8bit: Int, bodySize16bit: Int): Boolean = {
    // TODO: figure out how this interacts with the optimizer
    val bodySize = if (ctx.options.flag(CompilationFlag.EmitNative65816Opcodes)) bodySize16bit else bodySize8bit
    if (ctx.options.flag(CompilationFlag.OptimizeForSpeed)) true
    else if (ctx.options.flag(CompilationFlag.OptimizeForSize)) count * bodySize < bodySize + 5 // will unroll up to <<2 (<<3 on 65816)
    else count * bodySize < bodySize + 13 // will unroll up to <<4 (<<7 on 65816)
  }

  def compileWordShiftOps(left: Boolean, ctx: CompilationContext, l: Expression, r: Expression): List[AssemblyLine] = {
    if (ctx.options.zpRegisterSize < 2) {
      ctx.log.error("Word shifting requires the zeropage pseudoregister", l.position)
      return Nil
    }
    val b = ctx.env.get[Type]("byte")
    val w = ctx.env.get[Type]("word")
    val reg = ctx.env.get[VariableInMemory]("__reg.loword")
    val firstParamCompiled = MosExpressionCompiler.compile(ctx, l, Some(MosExpressionCompiler.getExpressionType(ctx, l) -> reg), NoBranching)
    ctx.env.eval(r) match {
      case Some(NumericConstant(0, _)) =>
        List(AssemblyLine.zeropage(LDA, reg), AssemblyLine.zeropage(LDX, reg, 1))
      case Some(NumericConstant(1, _)) if (firstParamCompiled match {
        case List(
        AssemblyLine0(LDA, ZeroPage | Absolute | Immediate, _),
        AssemblyLine0(STA, ZeroPage, _),
        AssemblyLine0(LDA, ZeroPage | Absolute | Immediate, _),
        AssemblyLine0(STA, ZeroPage, _)) => true
        case _ => false
      }) =>
        if (left) {
          List(
            firstParamCompiled(0),
            AssemblyLine.implied(ASL),
            AssemblyLine.zeropage(STA, reg),
            firstParamCompiled(2),
            AssemblyLine.implied(ROL),
            AssemblyLine.implied(TAX),
            AssemblyLine.zeropage(LDA, reg))
        } else {
          List(
            firstParamCompiled(2),
            AssemblyLine.implied(LSR),
            AssemblyLine.implied(TAX),
            firstParamCompiled(0),
            AssemblyLine.implied(ROR))
        }
      case Some(NumericConstant(v, _)) if v > 0 && unrollShift(ctx, v, 2, 4) =>
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
          case List(AssemblyLine0(LDX, _, _)) => firstParamCompiled ++ compileCounter
          case List(AssemblyLine0(LDY, _, _), AssemblyLine0(LDX, _, _)) => firstParamCompiled ++ compileCounter
          case _ =>
            MosExpressionCompiler.compile(ctx, r, Some(b -> RegisterVariable(MosRegister.A, b)), NoBranching) ++
              List(AssemblyLine.implied(PHA)) ++
              firstParamCompiled ++ (
              if (ctx.options.flag(CompilationFlag.EmitCmosOpcodes)) List(AssemblyLine.implied(PLX))
              else List(AssemblyLine.implied(PLA), AssemblyLine.implied(TAX))
              )
        }
        val labelRepeat = ctx.nextLabel("sr")
        val labelSkip = ctx.nextLabel("ss")
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

  def compileByteMultiplication(ctx: CompilationContext, param1OrRegister: Option[Expression], param2: Expression, storeInRegLo: Boolean): List[AssemblyLine] =
    compileByteMultiplicationOrDivision(ctx, param1OrRegister, param2, storeInRegLo, "__mul_u8u8u8", commutative = true)
  def compileUnsignedByteDivision(ctx: CompilationContext, param1OrRegister: Option[Expression], param2: Expression, storeInRegLo: Boolean): List[AssemblyLine] =
    compileByteMultiplicationOrDivision(ctx, param1OrRegister, param2, storeInRegLo, "__div_u8u8u8u8", commutative = false)
  def compileUnsignedByteModulo(ctx: CompilationContext, param1OrRegister: Option[Expression], param2: Expression, storeInRegLo: Boolean): List[AssemblyLine] =
    compileByteMultiplicationOrDivision(ctx, param1OrRegister, param2, storeInRegLo, "__mod_u8u8u8u8", commutative = false)

  def compileByteMultiplicationOrDivision(ctx: CompilationContext, param1OrRegister: Option[Expression], param2: Expression, storeInRegLo: Boolean, functionName: String, commutative: Boolean): List[AssemblyLine] = {
    if (ctx.options.zpRegisterSize < 2) {
      ctx.log.error("Variable byte multiplication requires the zeropage pseudoregister", param1OrRegister.flatMap(_.position))
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
        } else if (!commutative) {
          code2 ++ List(AssemblyLine.implied(PHA)) ++ MosExpressionCompiler.fixTsx(code1) ++ List(
            AssemblyLine.zeropage(STA, reg),
            AssemblyLine.implied(PLA),
            AssemblyLine.zeropage(STA, reg, 1)
          )
        } else if (!usesRegLo(code1)) {
          code2 ++ List(AssemblyLine.zeropage(STA, reg)) ++ code1 ++ List(AssemblyLine.zeropage(STA, reg, 1))
        } else {
          code1 ++ List(AssemblyLine.implied(PHA)) ++ MosExpressionCompiler.fixTsx(code2) ++ List(
            AssemblyLine.zeropage(STA, reg),
            AssemblyLine.implied(PLA),
            AssemblyLine.zeropage(STA, reg, 1)
          )
        }
      case None =>
        val code2 = MosExpressionCompiler.compile(ctx, param2, Some(b -> RegisterVariable(MosRegister.A, b)), BranchSpec.None)
        if (!usesRegLo(code2)) {
          List(AssemblyLine.zeropage(STA, reg)) ++ code2 ++ List(AssemblyLine.zeropage(STA, reg, 1))
        } else if (!commutative) {
          List(AssemblyLine.implied(PHA)) ++ MosExpressionCompiler.fixTsx(code2) ++ List(
            AssemblyLine.zeropage(STA, reg, 1),
            AssemblyLine.implied(PLA),
            AssemblyLine.zeropage(STA, reg)
          )
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
    val calculate = AssemblyLine.absoluteOrLongAbsolute(JSR, ctx.env.get[FunctionInMemory](functionName), ctx.options) ::
      (if (storeInRegLo) List(AssemblyLine.zeropage(STA, reg)) else Nil)
    load ++ calculate
  }

  private def isPowerOfTwoUpTo15(n: Long): Boolean = if (n <= 0 || n >= 0x8000) false else 0 == ((n-1) & n)

  def compileWordWordMultiplication(ctx: CompilationContext, param1OrRegister: Option[Expression], param2: Expression): List[AssemblyLine] = {
    if (ctx.options.zpRegisterSize < 4) {
      ctx.log.error("Variable word-word multiplication requires the zeropage pseudoregister of size at least 4", param1OrRegister.flatMap(_.position))
      return Nil
    }
    val w = ctx.env.get[Type]("word")
    val reg = ctx.env.get[VariableInMemory]("__reg")
    val load: List[AssemblyLine] = param1OrRegister match {
      case Some(param1) =>
        val code1 = MosExpressionCompiler.compile(ctx, param1, Some(w -> RegisterVariable(MosRegister.AX, w)), BranchSpec.None)
        val code2 = MosExpressionCompiler.compile(ctx, param2, Some(w -> RegisterVariable(MosRegister.AX, w)), BranchSpec.None)
        if (!usesRegLo(code2) && !usesRegHi(code2)) {
          code1 ++ List(AssemblyLine.zeropage(STA, reg), AssemblyLine.zeropage(STX, reg, 1)) ++ code2 ++ List(AssemblyLine.zeropage(STA, reg, 2), AssemblyLine.zeropage(STX, reg, 3))
        } else if (!usesReg2(code1) && !usesReg3(code1)) {
          code2 ++ List(AssemblyLine.zeropage(STA, reg, 2), AssemblyLine.zeropage(STX, reg, 3)) ++ code1 ++ List(AssemblyLine.zeropage(STA, reg), AssemblyLine.zeropage(STX, reg, 1))
        } else {
          code2 ++ List(
            AssemblyLine.implied(PHA),
            AssemblyLine.implied(TXA),
            AssemblyLine.implied(PHA)
          ) ++ code1 ++ List(
            AssemblyLine.zeropage(STA, reg),
            AssemblyLine.zeropage(STX, reg, 1),
            AssemblyLine.implied(PLA),
            AssemblyLine.zeropage(STA, reg, 3),
            AssemblyLine.implied(PLA),
            AssemblyLine.zeropage(STA, reg, 2)
          )
        }
      case None =>
        val code2 = MosExpressionCompiler.compile(ctx, param2, Some(w -> RegisterVariable(MosRegister.AX, w)), BranchSpec.None)
        if (!usesRegLo(code2) && !usesRegHi(code2)) {
          List(AssemblyLine.zeropage(STA, reg), AssemblyLine.zeropage(STX, reg, 1)) ++ code2 ++ List(AssemblyLine.zeropage(STA, reg, 2), AssemblyLine.zeropage(STX, reg, 3))
        } else {
          List(AssemblyLine.implied(PHA), AssemblyLine.implied(TXA), AssemblyLine.implied(PHA)) ++ code2 ++ List(
            AssemblyLine.zeropage(STA, reg, 2),
            AssemblyLine.zeropage(STX, reg, 3),
            AssemblyLine.implied(PLA),
            AssemblyLine.zeropage(STA, reg, 1),
            AssemblyLine.implied(PLA),
            AssemblyLine.zeropage(STA, reg)
          )
        }
    }
    load :+ AssemblyLine.absoluteOrLongAbsolute(JSR, ctx.env.get[FunctionInMemory]("__mul_u16u16u16"), ctx.options)
  }

  def compileWordMultiplication(ctx: CompilationContext, param1OrRegister: Option[Expression], param2: Expression, storeInRegLo: Boolean): List[AssemblyLine] = {
    if (ctx.options.zpRegisterSize < 3) {
      ctx.log.error("Variable word-byte multiplication requires the zeropage pseudoregister of size at least 3", param1OrRegister.flatMap(_.position))
      return Nil
    }
    (param1OrRegister.fold(2)(e => AbstractExpressionCompiler.getExpressionType(ctx, e).size),
      AbstractExpressionCompiler.getExpressionType(ctx, param2).size) match {
      case (2, 2) => return compileWordWordMultiplication(ctx, param1OrRegister, param2)
      case (1, 2) => return compileWordMultiplication(ctx, Some(param2), param1OrRegister.get, storeInRegLo)
      case (2 | 1, 1) => // ok
      case _ => ctx.log.fatal("Invalid code path", param2.position)
    }
    if (!storeInRegLo && param1OrRegister.isDefined) {
      (ctx.env.eval(param1OrRegister.get), ctx.env.eval(param2)) match {
        case (Some(l), Some(r)) =>
          val product = CompoundConstant(MathOperator.Times, l, r).quickSimplify
          return List(AssemblyLine.immediate(LDA, product.loByte), AssemblyLine.immediate(LDX, product.hiByte))
        case (Some(NumericConstant(c, _)), _) if isPowerOfTwoUpTo15(c)=>
          return compileWordShiftOps(left = true, ctx, param2, LiteralExpression(java.lang.Long.bitCount(c - 1), 1))
        case (_, Some(NumericConstant(c, _))) if isPowerOfTwoUpTo15(c)=>
          return compileWordShiftOps(left = true, ctx, param1OrRegister.get, LiteralExpression(java.lang.Long.bitCount(c - 1), 1))
        case _ =>
      }
    }
    val b = ctx.env.get[Type]("byte")
    val w = ctx.env.get[Type]("word")
    val reg = ctx.env.get[VariableInMemory]("__reg")
    val load: List[AssemblyLine] = param1OrRegister match {
      case Some(param1) =>
        val code1 = MosExpressionCompiler.compile(ctx, param1, Some(w -> RegisterVariable(MosRegister.AX, w)), BranchSpec.None)
        val code2 = MosExpressionCompiler.compile(ctx, param2, Some(b -> RegisterVariable(MosRegister.A, b)), BranchSpec.None)
        if (!usesRegLo(code2) && !usesRegHi(code2)) {
          code1 ++ List(AssemblyLine.zeropage(STA, reg), AssemblyLine.zeropage(STX, reg, 1)) ++ code2 ++ List(AssemblyLine.zeropage(STA, reg, 2))
        } else if (!usesReg2(code1)) {
          code2 ++ List(AssemblyLine.zeropage(STA, reg, 2)) ++ code1 ++ List(AssemblyLine.zeropage(STA, reg), AssemblyLine.zeropage(STX, reg, 1))
        } else {
          code2 ++ List(AssemblyLine.implied(PHA)) ++ code1 ++ List(
            AssemblyLine.zeropage(STA, reg),
            AssemblyLine.zeropage(STX, reg, 1),
            AssemblyLine.implied(PLA),
            AssemblyLine.zeropage(STA, reg, 2)
          )
        }
      case None =>
        val code2 = MosExpressionCompiler.compile(ctx, param2, Some(b -> RegisterVariable(MosRegister.A, b)), BranchSpec.None)
        if (!usesRegLo(code2) && !usesRegHi(code2)) {
          List(AssemblyLine.zeropage(STA, reg), AssemblyLine.zeropage(STX, reg, 1)) ++ code2 ++ List(AssemblyLine.zeropage(STA, reg, 2))
        } else {
          List(AssemblyLine.implied(PHA), AssemblyLine.implied(TXA), AssemblyLine.implied(PHA)) ++ code2 ++ List(
            AssemblyLine.zeropage(STA, reg, 2),
            AssemblyLine.implied(PLA),
            AssemblyLine.zeropage(STA, reg, 1),
            AssemblyLine.implied(PLA),
            AssemblyLine.zeropage(STA, reg)
          )
        }
    }
    val calculate = AssemblyLine.absoluteOrLongAbsolute(JSR, ctx.env.get[FunctionInMemory]("__mul_u16u8u16"), ctx.options) ::
      (if (storeInRegLo) List(AssemblyLine.zeropage(STA, reg), AssemblyLine.zeropage(STX, reg, 1)) else Nil)
    load ++ calculate
  }

  def compileWordWordDivision(ctx: CompilationContext, param1OrRegister: Option[Expression], param2: Expression, modulo: Boolean): List[AssemblyLine] = {
    if (ctx.options.zpRegisterSize < 4) {
      ctx.log.error("Variable word-word division requires the zeropage pseudoregister of size at least 4", param1OrRegister.flatMap(_.position))
      return Nil
    }
    val w = ctx.env.get[Type]("word")
    val reg = ctx.env.get[VariableInMemory]("__reg")
    val load: List[AssemblyLine] = param1OrRegister match {
      case Some(param1) =>
        val code1 = MosExpressionCompiler.compile(ctx, param1, Some(w -> RegisterVariable(MosRegister.AX, w)), BranchSpec.None)
        val code2 = MosExpressionCompiler.compile(ctx, param2, Some(w -> RegisterVariable(MosRegister.AX, w)), BranchSpec.None)
        if (!usesRegLo(code2) && !usesRegHi(code2)) {
          code1 ++ List(AssemblyLine.zeropage(STA, reg), AssemblyLine.zeropage(STX, reg, 1)) ++ code2 ++ List(AssemblyLine.zeropage(STA, reg, 2), AssemblyLine.zeropage(STX, reg, 3))
        } else if (!usesReg2(code1) && !usesReg3(code1)) {
          code2 ++ List(AssemblyLine.zeropage(STA, reg, 2), AssemblyLine.zeropage(STX, reg, 3)) ++ code1 ++ List(AssemblyLine.zeropage(STA, reg), AssemblyLine.zeropage(STX, reg, 1))
        } else {
          code2 ++ List(
            AssemblyLine.implied(PHA),
            AssemblyLine.implied(TXA),
            AssemblyLine.implied(PHA)
          ) ++ code1 ++ List(
            AssemblyLine.zeropage(STA, reg),
            AssemblyLine.zeropage(STX, reg, 1),
            AssemblyLine.implied(PLA),
            AssemblyLine.zeropage(STA, reg, 3),
            AssemblyLine.implied(PLA),
            AssemblyLine.zeropage(STA, reg, 2)
          )
        }
      case None =>
        val code2 = MosExpressionCompiler.compile(ctx, param2, Some(w -> RegisterVariable(MosRegister.AX, w)), BranchSpec.None)
        if (!usesRegLo(code2) && !usesRegHi(code2)) {
          List(AssemblyLine.zeropage(STA, reg), AssemblyLine.zeropage(STX, reg, 1)) ++ code2 ++ List(AssemblyLine.zeropage(STA, reg, 2), AssemblyLine.zeropage(STX, reg, 3))
        } else {
          List(AssemblyLine.implied(PHA), AssemblyLine.implied(TXA), AssemblyLine.implied(PHA)) ++ code2 ++ List(
            AssemblyLine.zeropage(STA, reg, 2),
            AssemblyLine.zeropage(STX, reg, 3),
            AssemblyLine.implied(PLA),
            AssemblyLine.zeropage(STA, reg, 1),
            AssemblyLine.implied(PLA),
            AssemblyLine.zeropage(STA, reg)
          )
        }
    }
    val functionName = if(modulo) "__mod_u16u16u16u16" else "__div_u16u16u16u16"
    load :+ AssemblyLine.absoluteOrLongAbsolute(JSR, ctx.env.get[FunctionInMemory](functionName), ctx.options)
  }

  def compileUnsignedWordByByteDivision(ctx: CompilationContext, param1: Expression, param2: Expression, modulo: Boolean): List[AssemblyLine] = {
    (AbstractExpressionCompiler.getExpressionType(ctx, param1).size,
      AbstractExpressionCompiler.getExpressionType(ctx, param2).size) match {
      case (2 | 1, 2) => return compileWordWordDivision(ctx, Some(param1), param2, modulo)
      case (2 | 1, 1) => // ok
      case _ => ctx.log.fatal("Invalid code path", param2.position)
    }
    val b = ctx.env.get[Type]("byte")
    val w = ctx.env.get[Type]("word")
    val reg = ctx.env.get[VariableInMemory]("__reg")
    (ctx.env.eval(param1), ctx.env.eval(param2)) match {
      case (Some(l), Some(r)) =>
        if (r.isProvablyZero) {
          ctx.log.error("Unsigned division by zero", param2.position)
        }
        val operator = if (modulo) MathOperator.Modulo else MathOperator.Divide
        val product = CompoundConstant(operator, l, r).quickSimplify
        return List(AssemblyLine.immediate(LDA, product.loByte), AssemblyLine.immediate(LDX, product.hiByte))
      case (_, Some(NumericConstant(p, _))) =>
        if (p == 0) {
          ctx.log.error("Unsigned division by zero", param2.position)
        }
        if (p == 1) {
          if (modulo) return MosExpressionCompiler.compile(ctx, param1, None, BranchSpec.None) ++ List(AssemblyLine.immediate(LDA, 0), AssemblyLine.immediate(LDX, 0))
          else return MosExpressionCompiler.compile(ctx, param1, Some(w -> RegisterVariable(MosRegister.AX, w)), BranchSpec.None)
        } else if (p < 256 && isPowerOfTwoUpTo15(p)) {
          if (modulo) return MosExpressionCompiler.compile(ctx, param1, Some(w -> RegisterVariable(MosRegister.AX, w)), BranchSpec.None) ++ List(AssemblyLine.immediate(AND, p - 1), AssemblyLine.immediate(LDX, 0))
          else return compileWordShiftOps(left = false, ctx, param1, LiteralExpression(Integer.numberOfTrailingZeros(p.toInt), 1))
        }
      case _ =>
    }
    val code1 = MosExpressionCompiler.compile(ctx, param1, Some(w -> RegisterVariable(MosRegister.AX, w)), BranchSpec.None)
    val code2 = MosExpressionCompiler.compile(ctx, param2, Some(b -> RegisterVariable(MosRegister.A, b)), BranchSpec.None)
    val load = if (!usesRegLo(code2) && !usesRegHi(code2)) {
      code1 ++ List(AssemblyLine.zeropage(STA, reg), AssemblyLine.zeropage(STX, reg, 1)) ++ code2 ++ List(AssemblyLine.zeropage(STA, reg, 2))
    } else if (!usesReg2(code1)) {
      code2 ++ List(AssemblyLine.zeropage(STA, reg, 2)) ++ code1 ++ List(AssemblyLine.zeropage(STA, reg), AssemblyLine.zeropage(STX, reg, 1))
    } else {
      code2 ++ List(AssemblyLine.implied(PHA)) ++ code1 ++ List(
        AssemblyLine.zeropage(STA, reg),
        AssemblyLine.zeropage(STX, reg, 1),
        AssemblyLine.implied(PLA),
        AssemblyLine.zeropage(STA, reg, 2)
      )
    }
    val functionName = if(modulo) "__mod_u16u8u16u8" else "__div_u16u8u16u8"
    load ++ List(AssemblyLine.absoluteOrLongAbsolute(JSR, ctx.env.get[FunctionInMemory](functionName), ctx.options))
  }

  private def simplicity(env: Environment, expr: Expression): Char = {
    val constPart = env.eval(expr) match {
      case Some(NumericConstant(_, _)) => 'Z'
      case Some(_) => 'Y'
      case None => expr match {
        case VariableExpression(_) => 'V'
        case IndexedExpression(_, LiteralExpression(_, _)) => 'K'
        case IndexedExpression(_, GeneratedConstantExpression(_, _)) => 'K'
        case IndexedExpression(_, VariableExpression(_)) => 'J'
        case IndexedExpression(_, _) => 'I'
        case _ => 'A'
      }
    }
    constPart
  }

  def usesRegLo(code: List[AssemblyLine]): Boolean = code.exists{
    case AssemblyLine0(JSR | BSR | TCD | TDC, _, _) => true
    case AssemblyLine0(_, _, MemoryAddressConstant(th)) if th.name == "__reg" => true
    case _ => false
  }

  def usesRegHi(code: List[AssemblyLine]): Boolean = code.exists{
    case AssemblyLine0(JSR | BSR | TCD | TDC, _, _) => true
    case AssemblyLine0(_, _, CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(1, _))) if th.name == "__reg" => true
    case _ => false
  }

  def usesReg2(code: List[AssemblyLine]): Boolean = code.exists{
    case AssemblyLine0(JSR | BSR | TCD | TDC, _, _) => true
    case AssemblyLine0(_, _, CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(2, _))) if th.name == "__reg" => true
    case _ => false
  }

  def usesReg3(code: List[AssemblyLine]): Boolean = code.exists{
    case AssemblyLine0(JSR | BSR | TCD | TDC, _, _) => true
    case AssemblyLine0(_, _, CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(3, _))) if th.name == "__reg" => true
    case _ => false
  }

  def doesMemoryAccessOverlap(l1: List[AssemblyLine], l2: List[AssemblyLine]): Boolean = {
    for{
      a1 <- l1
      if a1.addrMode != Immediate && a1.addrMode != Implied
      a2 <- l2
      if a2.addrMode != Immediate && a2.addrMode != Implied
      if (a1.opcode == STA) != (a2.opcode == STA)
    } {
      if (a1.parameter == a2.parameter) return true
    }
    false
  }
}
