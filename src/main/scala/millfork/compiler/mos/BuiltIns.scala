package millfork.compiler.mos

import millfork.{CompilationFlag, assembly}
import millfork.assembly.Elidability
import millfork.assembly.mos.AddrMode._
import millfork.assembly.mos.Opcode._
import millfork.assembly.mos._
import millfork.compiler._
import millfork.env._
import millfork.node._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * @author Karol Stasiak
  */
//noinspection RedundantDefaultArgument
object BuiltIns {

  object IndexChoice extends Enumeration {
    val RequireX, PreferX, PreferY = Value
  }

  def wrapInSedCldIfNeeded(decimal: Boolean, code: List[AssemblyLine]): List[AssemblyLine] = {
    if (decimal) {
      AssemblyLine.implied(SED) :: (code :+ AssemblyLine.implied(CLD))
    } else {
      code
    }
  }

  def staTo(op: Opcode.Value, l: List[AssemblyLine]): List[AssemblyLine] = l.map(x => if (x.opcode == STA) x.copy(opcode = op) else x)

  def cmpTo(op: Opcode.Value, l: List[AssemblyLine]): List[AssemblyLine] = l.map(x => if (x.opcode == CMP) x.copy(opcode = op) else x)

  def ldTo(op: Opcode.Value, l: List[AssemblyLine]): List[AssemblyLine] = l.map(x => if (x.opcode == LDA || x.opcode == LDX || x.opcode == LDY) x.copy(opcode = op) else x)

  def simpleOperation(opcode: Opcode.Value, ctx: CompilationContext, source: Expression, indexChoice: IndexChoice.Value, preserveA: Boolean, commutative: Boolean, decimal: Boolean = false): List[AssemblyLine] = {
    val env = ctx.env
    val parts: (List[AssemblyLine], List[AssemblyLine]) = env.eval(source).fold {
      val b = env.get[Type]("byte")
      source match {
        case VariableExpression(name) =>
          val v = env.get[Variable](name)
          if (v.typ.size > 1) {
            ctx.log.error(s"Variable `$name` is too big for a built-in operation", source.position)
            return Nil
          }
          Nil -> AssemblyLine.variable(ctx, opcode, v)
        case IndexedExpression(arrayName, index) =>
          val pointy = env.getPointy(arrayName)
          AbstractExpressionCompiler.checkIndexType(ctx, pointy, index)
          val (variablePart, constantPart) = env.evalVariableAndConstantSubParts(index)
          val indexerSize = variablePart.map(v => getIndexerSize(ctx, v)).getOrElse(1)
          val totalIndexSize = getIndexerSize(ctx, index)
          (pointy, totalIndexSize, indexerSize, indexChoice, variablePart) match {
            case (p: ConstantPointy, _, _, _, None) =>
              Nil -> List(AssemblyLine.absolute(opcode, p.value + constantPart))
            case (p: ConstantPointy, _, 1, IndexChoice.RequireX | IndexChoice.PreferX, Some(v)) =>
              MosExpressionCompiler.compile(ctx, v, Some(b -> RegisterVariable(MosRegister.X, pointy.indexType)), NoBranching) -> List(AssemblyLine.absoluteX(opcode, p.value + constantPart))
            case (p: ConstantPointy, _, 1, IndexChoice.PreferY, Some(v)) =>
              MosExpressionCompiler.compile(ctx, v, Some(b -> RegisterVariable(MosRegister.Y, pointy.indexType)), NoBranching) -> List(AssemblyLine.absoluteY(opcode, p.value + constantPart))
            case (p: VariablePointy, 0 | 1, _, IndexChoice.PreferX | IndexChoice.PreferY, _) =>
              MosExpressionCompiler.compile(ctx, index, Some(b -> RegisterVariable(MosRegister.Y, pointy.indexType)), NoBranching) -> List(AssemblyLine.indexedY(opcode, p.addr))
            case (p: ConstantPointy, _, 2, IndexChoice.PreferX | IndexChoice.PreferY, Some(v)) =>
              MosExpressionCompiler.prepareWordIndexing(ctx, p, index) -> List(AssemblyLine.indexedY(opcode, env.get[VariableInMemory]("__reg")))
            case (p: VariablePointy, 2, _, IndexChoice.PreferX | IndexChoice.PreferY, _) =>
              MosExpressionCompiler.prepareWordIndexing(ctx, p, index) -> List(AssemblyLine.indexedY(opcode, env.get[VariableInMemory]("__reg")))
            case _ =>
              ctx.log.error("Invalid index for simple operation argument", index.position)
              Nil -> Nil
          }
        case FunctionCallExpression(name, List(param)) if env.maybeGet[Type](name).isDefined =>
          return simpleOperation(opcode, ctx, param, indexChoice, preserveA, commutative, decimal)
        case _: FunctionCallExpression | _: SumExpression if commutative =>
          // TODO: is it ok?
          if (ctx.options.zpRegisterSize >= 1) {
            val reg = ctx.env.get[ThingInMemory]("__reg")
            return List(AssemblyLine.implied(PHA)) ++
              MosExpressionCompiler.compileToA(ctx, source) ++
              List(AssemblyLine.zeropage(STA, reg), AssemblyLine.implied(PLA)) ++
              wrapInSedCldIfNeeded(decimal, List(AssemblyLine.zeropage(opcode, reg)))
          } else if (ctx.options.flag(CompilationFlag.EmitEmulation65816Opcodes)) {
            return List(AssemblyLine.implied(PHA)) ++ MosExpressionCompiler.compileToA(ctx.addStack(1), source) ++ wrapInSedCldIfNeeded(decimal, List(
              AssemblyLine.stackRelative(opcode, 1),
              AssemblyLine.implied(PHX)))
          } else {
            return List(AssemblyLine.implied(PHA)) ++ MosExpressionCompiler.compileToA(ctx.addStack(1), source) ++ wrapInSedCldIfNeeded(decimal, List(
              AssemblyLine.implied(TSX),
              AssemblyLine.absoluteX(opcode, 0x101),
              AssemblyLine.implied(INX),
              AssemblyLine.implied(TXS))) // this TXS is fine, it won't appear in 65816 code
          }
        case _ =>
          if (ctx.options.zpRegisterSize < 1) {
            ctx.log.error("Right-hand-side expression requires a zero-page register", source.position)
            return Nil
          }
          val reg = ctx.env.get[ThingInMemory]("__reg")
          return List(AssemblyLine.implied(PHA)) ++
            MosExpressionCompiler.compileToA(ctx, source) ++
            List(AssemblyLine.zeropage(STA, reg), AssemblyLine.implied(PLA)) ++
            wrapInSedCldIfNeeded(decimal, List(AssemblyLine.zeropage(opcode, reg)))
      }
    } {
      const =>
        if (const.requiredSize > 1) {
          ctx.log.error("Constant too big for a built-in operation", source.position)
        }
        Nil -> List(AssemblyLine.immediate(opcode, const))
    }
    val preparations = parts._1
    val finalRead = wrapInSedCldIfNeeded(decimal, parts._2)
    if (preserveA && AssemblyLine.treatment(preparations, State.A) != Treatment.Unchanged) {
      AssemblyLine.implied(PHA) :: (MosExpressionCompiler.fixTsx(preparations) ++ (AssemblyLine.implied(PLA) :: finalRead))
    } else {
      preparations ++ finalRead
    }
  }

  def insertBeforeLast(item: AssemblyLine, list: List[AssemblyLine]): List[AssemblyLine] = list match {
    case Nil => Nil
    case last :: cld :: Nil if cld.opcode == CLD => item :: last :: cld :: Nil
    case last :: cld :: dex :: txs :: Nil if cld.opcode == CLD && dex.opcode == DEX && txs.opcode == TXS => item :: last :: cld :: dex :: txs :: Nil
    case last :: cld :: inx :: txs :: Nil if cld.opcode == CLD && inx.opcode == INX && txs.opcode == TXS => item :: last :: cld :: inx :: txs :: Nil
    case last :: dex :: txs :: Nil if dex.opcode == DEX && txs.opcode == TXS => item :: last :: dex :: txs :: Nil
    case last :: inx :: txs :: Nil if inx.opcode == INX && txs.opcode == TXS => item :: last :: inx :: txs :: Nil
    case last :: Nil => item :: last :: Nil
    case first :: rest => first :: insertBeforeLast(item, rest)
  }

  def compileAddition(ctx: CompilationContext, params: List[(Boolean, Expression)], decimal: Boolean): List[AssemblyLine] = {
    if (decimal && !ctx.options.flag(CompilationFlag.DecimalMode) && ctx.options.zpRegisterSize < 4) {
      ctx.log.error("Unsupported decimal operation. Consider increasing the size of the zeropage register.", params.head._2.position)
      return compileAddition(ctx, params, decimal = false)
    }
    //    if (params.isEmpty) {
    //      return Nil
    //    }
    val env = ctx.env
    val b = env.get[Type]("byte")
    val sortedParams = params.sortBy { case (subtract, expr) =>
      simplicity(env, expr) + (if (subtract) "X" else "P")
    }
    // TODO: merge constants
    val normalizedParams = sortedParams

    val h = normalizedParams.head
    val firstParamCompiled = MosExpressionCompiler.compile(ctx, h._2, Some(b -> RegisterVariable(MosRegister.A, b)), NoBranching)
    val firstParamSignCompiled = if (h._1) {
      // TODO: check if decimal subtraction works correctly here
      List(AssemblyLine.immediate(EOR, 0xff), AssemblyLine.implied(SEC), AssemblyLine.immediate(ADC, 0))
    } else {
      Nil
    }

    val remainingParamsCompiled = normalizedParams.tail.flatMap { p =>
      if (decimal && !ctx.options.flag(CompilationFlag.DecimalMode)) {
        val reg = ctx.env.get[VariableInMemory]("__reg")
        if (p._1) {
          List(AssemblyLine.zeropage(STA, reg, 2)) ++
            MosExpressionCompiler.preserveZpregIfNeededDestroyingAAndX(ctx, 2,
              MosExpressionCompiler.compileToA(ctx, p._2)) ++
            List(AssemblyLine.zeropage(STA, reg, 3), AssemblyLine.absolute(JSR, ctx.env.get[FunctionInMemory]("__sub_decimal")))
        } else {
          List(AssemblyLine.zeropage(STA, reg, 2), AssemblyLine.implied(CLC)) ++
            MosExpressionCompiler.preserveZpregIfNeededDestroyingAAndX(ctx, 2, MosExpressionCompiler.compileToA(ctx, p._2)) ++
            List(AssemblyLine.zeropage(STA, reg, 3), AssemblyLine.absolute(JSR, ctx.env.get[FunctionInMemory]("__adc_decimal")))
        }
      } else {
        if (p._1) {
          insertBeforeLast(AssemblyLine.implied(SEC), simpleOperation(SBC, ctx, p._2, IndexChoice.PreferY, preserveA = true, commutative = false, decimal = decimal))
        } else {
          insertBeforeLast(AssemblyLine.implied(CLC), simpleOperation(ADC, ctx, p._2, IndexChoice.PreferY, preserveA = true, commutative = true, decimal = decimal))
        }
      }
    }
    firstParamCompiled ++ firstParamSignCompiled ++ remainingParamsCompiled
  }

  private def simplicity(env: Environment, expr: Expression): Char = {
    val constPart = env.eval(expr) match {
      case Some(NumericConstant(_, _)) => 'Z'
      case Some(_) => 'Y'
      case None => expr match {
        case VariableExpression(_) => 'V'
        case IndexedExpression(_, LiteralExpression(_, _)) => 'K'
        case IndexedExpression(_, GeneratedConstantExpression(_, _)) => 'K'
        case IndexedExpression(_, expr@VariableExpression(v)) =>
          env.eval(expr) match {
            case Some(_) => 'K'
            case None => env.get[Variable](v).typ.size match {
              case 1 => 'J'
              case _ => 'I'
            }
          }
        case IndexedExpression(_, VariableExpression(v)) if env.get[Variable](v).typ.size == 1 => 'J'
        case IndexedExpression(_, _) => 'I'
        case _ => 'A'
      }
    }
    constPart
  }

  def compileBitOps(opcode: Opcode.Value, ctx: CompilationContext, params: List[Expression]): List[AssemblyLine] = {
    val b = ctx.env.get[Type]("byte")

    val sortedParams = params.sortBy { expr => simplicity(ctx.env, expr) }

    val h = sortedParams.head
    val firstParamCompiled = MosExpressionCompiler.compile(ctx, h, Some(b -> RegisterVariable(MosRegister.A, b)), NoBranching)

    val remainingParamsCompiled = sortedParams.tail.flatMap { p =>
      simpleOperation(opcode, ctx, p, IndexChoice.PreferY, preserveA = true, commutative = true)
    }

    firstParamCompiled ++ remainingParamsCompiled
  }

  def maybeCompileShiftFromByteToWord(ctx: CompilationContext, l: Expression, r: Expression, left: Boolean): Option[List[AssemblyLine]] = {
    val env = ctx.env
    env.eval(r) match {
      case Some(NumericConstant(n, _)) =>
        l match {
          case FunctionCallExpression(wordTypeName, List(param)) =>
            if (AbstractExpressionCompiler.getExpressionType(ctx, param).size == 1 && env.maybeGet[Type](wordTypeName).exists(_.size == 2)) {
              Some(MosExpressionCompiler.compileToA(ctx, param) ++ BuiltIns.compileShiftFromByteToWord(ctx, n.toInt, left))
            } else {
              None
            }
          case _ => None
        }
      case _ => None
    }
  }

  def compileShiftFromByteToWord(ctx: CompilationContext, count: Int, left: Boolean): List[AssemblyLine] = {
    if (count == 8) {
      List(AssemblyLine.implied(TAX), AssemblyLine.immediate(LDA, 0))
    } else {
      List(AssemblyLine.implied(PHA)) ++
        List.fill(8 - count)(AssemblyLine.implied(if (left) LSR else ASL)) ++
        List(AssemblyLine.implied(TAX), AssemblyLine.implied(PLA)) ++
        List.fill(count)(AssemblyLine.implied(if (left) ASL else LSR))
    }
  }

  def compileShiftOps(opcode: Opcode.Value, ctx: CompilationContext, l: Expression, r: Expression): List[AssemblyLine] = {
    val b = ctx.env.get[Type]("byte")
    val firstParamCompiled = MosExpressionCompiler.compile(ctx, l, Some(b -> RegisterVariable(MosRegister.A, b)), NoBranching)
    ctx.env.eval(r) match {
      case Some(NumericConstant(0, _)) =>
        MosExpressionCompiler.compile(ctx, l, None, NoBranching)
      case Some(NumericConstant(v, _)) if v > 0 =>
        firstParamCompiled ++ List.fill(v.toInt)(AssemblyLine.implied(opcode))
      case _ =>
        val compileCounter = MosExpressionCompiler.preserveRegisterIfNeeded(ctx, MosRegister.A,
          MosExpressionCompiler.compile(ctx, r, Some(b -> RegisterVariable(MosRegister.X, b)), NoBranching))
        val labelSkip = ctx.nextLabel("ss")
        val labelRepeat = ctx.nextLabel("sr")
        val loop = List(
          AssemblyLine.relative(BEQ, labelSkip),
          AssemblyLine.label(labelRepeat),
          AssemblyLine.implied(opcode),
          AssemblyLine.implied(DEX),
          AssemblyLine.relative(BNE, labelRepeat),
          AssemblyLine.label(labelSkip))
        firstParamCompiled ++ compileCounter ++ loop
    }
  }

  def compileNonetOps(ctx: CompilationContext, lhs: Expression, rhs: Expression): List[AssemblyLine] = {
    val env = ctx.env
    lhs match {
      case FunctionCallExpression("nonet", List(lparam)) =>
        (env.eval(lhs), env.eval(rhs)) match {
          case (None, Some(NumericConstant(0, _))) =>
            return MosExpressionCompiler.compile(ctx, lparam, None, NoBranching)
          case (None, Some(NumericConstant(n, _))) if n > 0 =>
            return MosExpressionCompiler.compile(ctx, lparam, None, NoBranching) ++
              (AssemblyLine.implied(ROR) :: List.fill(n.toInt - 1)(AssemblyLine.implied(LSR)))
          case _ =>
        }
      case _ =>
    }
    val b = env.get[Type]("byte")
    val (ldaHi, ldaLo) = env.eval(lhs) match {
      case Some(c) =>
        List(AssemblyLine.immediate(LDA, c.hiByte)) -> List(AssemblyLine.immediate(LDA, c.loByte))
      case _ => lhs match {
        case v: VariableExpression =>
          val variable = env.get[Variable](v.name)
          AssemblyLine.variable(ctx, LDA, variable, 1) -> AssemblyLine.variable(ctx, LDA, variable, 0)
        case SeparateBytesExpression(h: VariableExpression, l: VariableExpression) =>
          AssemblyLine.variable(ctx, LDA, env.get[Variable](h.name), 0) -> AssemblyLine.variable(ctx, LDA, env.get[Variable](l.name), 0)
        case _ =>
          ???
      }
    }
    env.eval(rhs) match {
      case Some(NumericConstant(0, _)) =>
        MosExpressionCompiler.compile(ctx, lhs, None, NoBranching)
      case Some(NumericConstant(shift, _)) if shift > 0 =>
        if (ctx.options.flag(CompilationFlag.RorWarning))
          ctx.log.warn("ROR instruction generated", lhs.position)
        ldaHi ++ List(AssemblyLine.implied(ROR)) ++ ldaLo ++ List(AssemblyLine.implied(ROR)) ++ List.fill(shift.toInt - 1)(AssemblyLine.implied(LSR))
      case _ =>
        ctx.log.error("Non-constant shift amount", rhs.position) // TODO
        Nil
    }
  }

  def compileInPlaceByteShiftOps(opcode: Opcode.Value, ctx: CompilationContext, lhs: LhsExpression, rhs: Expression): List[AssemblyLine] = {
    val env = ctx.env
    val b = env.get[Type]("byte")
    val firstParamCompiled = MosExpressionCompiler.compile(ctx, lhs, Some(b -> RegisterVariable(MosRegister.A, b)), NoBranching)
    env.eval(rhs) match {
      case Some(NumericConstant(0, _)) =>
        MosExpressionCompiler.compile(ctx, lhs, None, NoBranching)
      case Some(NumericConstant(v, _)) if v > 0 =>
        val result = simpleOperation(opcode, ctx, lhs, IndexChoice.RequireX, preserveA = true, commutative = false)
        result ++ List.fill(v.toInt - 1)(result.last)
      case _ =>
        compileShiftOps(opcode, ctx, lhs, rhs) ++ MosExpressionCompiler.compileByteStorage(ctx, MosRegister.A, lhs)
    }
  }

  def compileInPlaceWordOrLongShiftOps(ctx: CompilationContext, lhs: LhsExpression, rhs: Expression, aslRatherThanLsr: Boolean): List[AssemblyLine] = {
    if (lhs.isInstanceOf[DerefExpression]) {
      ctx.log.error("Too complex left-hand-side expression")
      return MosExpressionCompiler.compileToAX(ctx, lhs) ++ MosExpressionCompiler.compileToAX(ctx, rhs)
    }
    val env = ctx.env
    val b = env.get[Type]("byte")
    val targetBytes = getStorageForEachByte(ctx, lhs)
    val lo = targetBytes.head
    val hi = targetBytes.last
    // TODO: this probably breaks in case of complex split word expressions
    env.eval(rhs) match {
      case Some(NumericConstant(0, _)) =>
        MosExpressionCompiler.compile(ctx, lhs, None, NoBranching)
      case Some(NumericConstant(shift, _)) if shift > 0 =>
        if (ctx.options.flags(CompilationFlag.EmitNative65816Opcodes)) {
          targetBytes match {
            case List(List(AssemblyLine0(STA, a1, l)), List(AssemblyLine0(STA, a2, h))) =>
              if (a1 == a2 && l.+(1).quickSimplify == h) {
                return List(AssemblyLine.accu16) ++ List.fill(shift.toInt)(AssemblyLine(if (aslRatherThanLsr) ASL_W else LSR_W, a1, l)) ++ List(AssemblyLine.accu8)
              }
            case _ =>
          }
        }
        List.fill(shift.toInt)(if (aslRatherThanLsr) {
          staTo(ASL, lo) ++ targetBytes.tail.flatMap { b => staTo(ROL, b) }
        } else {
          if (ctx.options.flag(CompilationFlag.RorWarning))
            ctx.log.warn("ROR instruction generated", lhs.position)
          staTo(LSR, hi) ++ targetBytes.reverse.tail.flatMap { b => staTo(ROR, b) }
        }).flatten
      case _ =>
        val usesX = targetBytes.exists(_.exists(_.concernsX))
        val usesY = targetBytes.exists(_.exists(_.concernsY))
        val (register, decrease) = (usesX, usesY) match {
          case (true, false) => MosRegister.Y -> DEY
          case (false, true) => MosRegister.X -> DEX
          case (false, false) => MosRegister.X -> DEX
          case (true, true) => ???
        }

        val compileCounter = MosExpressionCompiler.preserveRegisterIfNeeded(ctx, MosRegister.A,
          MosExpressionCompiler.compile(ctx, rhs, Some(b -> RegisterVariable(register, b)), NoBranching))
        val labelSkip = ctx.nextLabel("ss")
        val labelRepeat = ctx.nextLabel("sr")

        if (ctx.options.flags(CompilationFlag.EmitNative65816Opcodes)) {
          targetBytes match {
            case List(List(AssemblyLine0(STA, a1, l)), List(AssemblyLine0(STA, a2, h))) =>
              if (a1 == a2 && l.+(1).quickSimplify == h) {
                return compileCounter ++ List(
                  AssemblyLine.relative(BEQ, labelSkip),
                  AssemblyLine.accu16,
                  AssemblyLine.label(labelRepeat),
                  AssemblyLine(if (aslRatherThanLsr) ASL_W else LSR_W, a1, l),
                  AssemblyLine.implied(decrease),
                  AssemblyLine.relative(BNE, labelRepeat),
                  AssemblyLine.accu8,
                  AssemblyLine.label(labelSkip))
              }
            case _ =>
          }
        }

        compileCounter ++ List(
          AssemblyLine.relative(BEQ, labelSkip),
          AssemblyLine.label(labelRepeat)) ++ (if (aslRatherThanLsr) {
          staTo(ASL, lo) ++ targetBytes.tail.flatMap { b => staTo(ROL, b) }
        } else {
          if (ctx.options.flag(CompilationFlag.RorWarning))
            ctx.log.warn("ROR instruction generated", lhs.position)
          staTo(LSR, hi) ++ targetBytes.reverse.tail.flatMap { b => staTo(ROR, b) }
        }) ++ List(
          AssemblyLine.implied(decrease),
          AssemblyLine.relative(BNE, labelRepeat),
          AssemblyLine.label(labelSkip))
    }
  }

  def compileByteComparison(ctx: CompilationContext, compType: ComparisonType.Value, lhs: Expression, rhs: Expression, branches: BranchSpec): List[AssemblyLine] = {
    val env = ctx.env
    val b = env.get[Type]("byte")
    if (simplicity(env, lhs) >= 'J' && simplicity(env, rhs) < 'J') {
      return compileByteComparison(ctx, ComparisonType.flip(compType), rhs, lhs, branches)
    }
    val firstParamCompiled = MosExpressionCompiler.compile(ctx, lhs, Some(b -> RegisterVariable(MosRegister.A, b)), NoBranching)
    val maybeConstant = env.eval(rhs)
    maybeConstant match {
      case Some(NumericConstant(0, _)) =>
        compType match {
          case ComparisonType.LessUnsigned =>
            ctx.log.warn("Unsigned < 0 is always false", lhs.position)
          case ComparisonType.LessOrEqualUnsigned =>
            if (ctx.options.flag(CompilationFlag.ExtraComparisonWarnings))
              ctx.log.warn("Unsigned <= 0 means the same as unsigned == 0", lhs.position)
          case ComparisonType.GreaterUnsigned =>
            if (ctx.options.flag(CompilationFlag.ExtraComparisonWarnings))
              ctx.log.warn("Unsigned > 0 means the same as unsigned != 0", lhs.position)
          case ComparisonType.GreaterOrEqualUnsigned =>
            ctx.log.warn("Unsigned >= 0 is always true", lhs.position)
          case _ =>
        }
      case Some(NumericConstant(1, _)) =>
        if (ctx.options.flag(CompilationFlag.ExtraComparisonWarnings)) {
          compType match {
            case ComparisonType.LessUnsigned =>
              ctx.log.warn("Unsigned < 1 means the same as unsigned == 0", lhs.position)
            case ComparisonType.GreaterOrEqualUnsigned =>
              ctx.log.warn("Unsigned >= 1 means the same as unsigned != 0", lhs.position)
            case _ =>
          }
        }
      case _ =>
    }
    val cmpOp = if (ComparisonType.isSigned(compType)) SBC else CMP
    var comparingAgainstZero = false
    val secondParamCompiled0 = maybeConstant match {
      case Some(x) =>
        compType match {
          case ComparisonType.Equal | ComparisonType.NotEqual | ComparisonType.LessSigned | ComparisonType.GreaterOrEqualSigned | ComparisonType.LessOrEqualSigned | ComparisonType.GreaterSigned =>
            if (x.quickSimplify.isLowestByteAlwaysEqual(0) && OpcodeClasses.ChangesAAlways(firstParamCompiled.last.opcode)) {
              comparingAgainstZero = true
              Nil
            } else List(AssemblyLine.immediate(cmpOp, x))
          case _ =>
            List(AssemblyLine.immediate(cmpOp, x))
        }
      case _ => compType match {
        case ComparisonType.Equal | ComparisonType.NotEqual | ComparisonType.LessSigned | ComparisonType.GreaterOrEqualSigned =>
          val secondParamCompiledUnoptimized = simpleOperation(cmpOp, ctx, rhs, IndexChoice.PreferY, preserveA = true, commutative = false)
          secondParamCompiledUnoptimized match {
            case List(AssemblyLine(cmpOp, Immediate, NumericConstant(0, _), Elidability.Elidable, _)) =>
              if (OpcodeClasses.ChangesAAlways(firstParamCompiled.last.opcode)) {
                Nil
              } else {
                secondParamCompiledUnoptimized
              }
            case _ => secondParamCompiledUnoptimized
          }
        case _ =>
          simpleOperation(cmpOp, ctx, rhs, IndexChoice.PreferY, preserveA = true, commutative = false)
      }
    }
    var secondParamCompiled = if(cmpOp == SBC && !comparingAgainstZero) AssemblyLine.implied(SEC) :: secondParamCompiled0 else secondParamCompiled0
    val (effectiveComparisonType, label) = branches match {
      case NoBranching => return Nil
      case BranchIfTrue(l) => compType -> l
      case BranchIfFalse(l) => ComparisonType.negate(compType) -> l
    }
    (env.eval(lhs), env.eval(rhs)) match {
      case (Some(NumericConstant(lc, _)),  Some(NumericConstant(rc, _))) =>
        return if (effectiveComparisonType match {
          // TODO: those masks are probably wrong
          case ComparisonType.Equal =>
            (lc & 0xff) == (rc & 0xff)
          case ComparisonType.NotEqual =>
            (lc & 0xff) != (rc & 0xff)
          case ComparisonType.LessOrEqualUnsigned =>
            (lc & 0xff) <= (rc & 0xff)
          case ComparisonType.GreaterOrEqualUnsigned =>
            (lc & 0xff) >= (rc & 0xff)
          case ComparisonType.GreaterUnsigned =>
            (lc & 0xff) > (rc & 0xff)
          case ComparisonType.LessUnsigned =>
            (lc & 0xff) < (rc & 0xff)
          case ComparisonType.LessOrEqualSigned =>
            lc.toByte <= rc.toByte
          case ComparisonType.GreaterOrEqualSigned =>
            lc.toByte >= rc.toByte
          case ComparisonType.GreaterSigned =>
            lc.toByte > rc.toByte
          case ComparisonType.LessSigned =>
            lc.toByte < rc.toByte
        }) List(AssemblyLine.absolute(JMP, Label(label))) else Nil
      case _ =>
    }
    val branchingCompiled = effectiveComparisonType match {
      case ComparisonType.Equal =>
        List(AssemblyLine.relative(BEQ, Label(label)))
      case ComparisonType.NotEqual =>
        List(AssemblyLine.relative(BNE, Label(label)))

      case ComparisonType.LessUnsigned =>
        List(AssemblyLine.relative(BCC, Label(label)))
      case ComparisonType.GreaterOrEqualUnsigned =>
        List(AssemblyLine.relative(BCS, Label(label)))
      case ComparisonType.LessOrEqualUnsigned =>
        List(AssemblyLine.relative(BCC, Label(label)), AssemblyLine.relative(BEQ, Label(label)))
      case ComparisonType.GreaterUnsigned =>
        val x = ctx.nextLabel("co")
        List(
          AssemblyLine.relative(BEQ, x),
          AssemblyLine.relative(BCS, Label(label)),
          AssemblyLine.label(x))

      case ComparisonType.LessSigned =>
        if (comparingAgainstZero) List(AssemblyLine.relative(BMI, label)) else {
          maybeConstant match {
            case Some(NumericConstant(n@(1 | 2 | 4 | 8 | 16 | 32 | 64), _)) =>
              secondParamCompiled = Nil
              List(
                AssemblyLine.immediate(AND, 255 ^ (n - 1)),
                AssemblyLine.relative(BEQ, label),
                AssemblyLine.relative(BMI, label))
            case _ =>
              val fixup = ctx.nextLabel("co")
              List(
                AssemblyLine.relative(BVC, fixup),
                AssemblyLine.immediate(EOR, 0x80),
                AssemblyLine.label(fixup),
                AssemblyLine.relative(BMI, label))
          }
        }
      case ComparisonType.GreaterOrEqualSigned =>
        if (comparingAgainstZero) List(AssemblyLine.relative(BPL, label)) else {
          maybeConstant match {
            case Some(NumericConstant(n@(1 | 2 | 4 | 8 | 16 | 32 | 64), _)) =>
              secondParamCompiled = Nil
              val x = ctx.nextLabel("co")
              List(
                AssemblyLine.immediate(AND, 255 ^ (n - 1)),
                AssemblyLine.relative(BEQ, x),
                AssemblyLine.relative(BPL, label),
                AssemblyLine.label(x))
            case _ =>
              val fixup = ctx.nextLabel("co")
              List(
                AssemblyLine.relative(BVC, fixup),
                AssemblyLine.immediate(EOR, 0x80),
                AssemblyLine.label(fixup), AssemblyLine.relative(BPL, label))
          }
        }
      case ComparisonType.LessOrEqualSigned =>
        if (comparingAgainstZero) {
          List(AssemblyLine.relative(BEQ, label),
            AssemblyLine.relative(BMI, label))
        } else {
          maybeConstant match {
            case Some(NumericConstant(n@(0 | 1 | 3 | 7 | 15 | 31 | 63), _)) =>
              secondParamCompiled = Nil
              List(
                AssemblyLine.immediate(AND, 255 ^ n),
                AssemblyLine.relative(BEQ, label),
                AssemblyLine.relative(BMI, label))
            case _ =>
              val fixup = ctx.nextLabel("co")
              List(AssemblyLine.relative(BVC, fixup),
                AssemblyLine.immediate(EOR, 0x80),
                AssemblyLine.label(fixup),
                AssemblyLine.relative(BMI, label),
                AssemblyLine.relative(BEQ, label))
          }
        }
      case ComparisonType.GreaterSigned =>
        if (comparingAgainstZero) {
          val x = ctx.nextLabel("co")
          List(AssemblyLine.relative(BEQ, x),
            AssemblyLine.relative(BPL, label),
            AssemblyLine.label(x))
        } else {
          maybeConstant match {
            case Some(NumericConstant(n@(0 | 1 | 3 | 7 | 15 | 31 | 63), _)) =>
              secondParamCompiled = Nil
              val x = ctx.nextLabel("co")
              List(
                AssemblyLine.immediate(AND, 255 ^ n),
                AssemblyLine.relative(BEQ, x),
                AssemblyLine.relative(BPL, label),
                AssemblyLine.label(x))
            case _ =>
              val fixup = ctx.nextLabel("co")
              val x = ctx.nextLabel("co")
              List(
                AssemblyLine.relative(BVC, fixup),
                AssemblyLine.immediate(EOR, 0x80),
                AssemblyLine.label(fixup),
                AssemblyLine.relative(BEQ, x),
                AssemblyLine.relative(BPL, label),
                AssemblyLine.label(x))
          }
        }
    }
    firstParamCompiled ++ secondParamCompiled ++ branchingCompiled

  }

  def compileWordComparison(ctx: CompilationContext, compType: ComparisonType.Value, lhs: Expression, rhs: Expression, branches: BranchSpec): List[AssemblyLine] = {
    val env = ctx.env
    // TODO: comparing longer variables
    val b = env.get[Type]("byte")
    val w = env.get[Type]("word")

    val (effectiveComparisonType, x) = branches match {
      case NoBranching => return Nil
      case BranchIfTrue(label) => compType -> label
      case BranchIfFalse(label) => ComparisonType.negate(compType) -> label
    }
    val (preparations, lh, ll, rh, rl) = (lhs, env.eval(lhs), rhs, env.eval(rhs)) match {
      case (_, Some(NumericConstant(lc, _)), _, Some(NumericConstant(rc, _))) =>
        return if (effectiveComparisonType match {
          // TODO: those masks are probably wrong
          case ComparisonType.Equal =>
            (lc & 0xffff) == (rc & 0xffff) // ??
          case ComparisonType.NotEqual =>
            (lc & 0xffff) != (rc & 0xffff) // ??

          case ComparisonType.LessOrEqualUnsigned =>
            (lc & 0xffff) <= (rc & 0xffff)
          case ComparisonType.GreaterOrEqualUnsigned =>
            (lc & 0xffff) >= (rc & 0xffff)
          case ComparisonType.GreaterUnsigned =>
            (lc & 0xffff) > (rc & 0xffff)
          case ComparisonType.LessUnsigned =>
            (lc & 0xffff) < (rc & 0xffff)

          case ComparisonType.LessOrEqualSigned =>
            lc.toShort <= rc.toShort
          case ComparisonType.GreaterOrEqualSigned =>
            lc.toShort >= rc.toShort
          case ComparisonType.GreaterSigned =>
            lc.toShort > rc.toShort
          case ComparisonType.LessSigned =>
            lc.toShort < rc.toShort
        }) List(AssemblyLine.absolute(JMP, Label(x))) else Nil
      case (_, Some(lc), _, Some(rc)) =>
        // TODO: comparing late-bound constants
        ???
      case (_, Some(lc), rv: VariableExpression, None) =>
        return compileWordComparison(ctx, ComparisonType.flip(compType), rhs, lhs, branches)
      case (v: VariableExpression, None, _, Some(rc)) =>
        val lva = env.get[VariableInMemory](v.name)
        (Nil,
          AssemblyLine.variable(ctx, CMP, lva, 1),
          AssemblyLine.variable(ctx, CMP, lva, 0),
          List(AssemblyLine.immediate(CMP, rc.hiByte.quickSimplify)),
          List(AssemblyLine.immediate(CMP, rc.loByte.quickSimplify)))
      case (lv: VariableExpression, None, rv: VariableExpression, None) =>
        val lva = env.get[VariableInMemory](lv.name)
        val rva = env.get[VariableInMemory](rv.name)
        (Nil,
          AssemblyLine.variable(ctx, CMP, lva, 1),
          AssemblyLine.variable(ctx, CMP, lva, 0),
          AssemblyLine.variable(ctx, CMP, rva, 1),
          AssemblyLine.variable(ctx, CMP, rva, 0))
      case (expr, None, _, Some(constant)) if effectiveComparisonType == ComparisonType.Equal =>
        val innerLabel = ctx.nextLabel("cp")
        return MosExpressionCompiler.compileToAX(ctx, expr) ++ List(
          AssemblyLine.immediate(CMP, constant.loByte),
          AssemblyLine.relative(BNE, innerLabel),
          AssemblyLine.immediate(CPX, constant.hiByte),
          AssemblyLine.relative(BEQ, Label(x)),
          AssemblyLine.label(innerLabel))
      case (_, Some(constant), expr, None) if effectiveComparisonType == ComparisonType.Equal =>
        val innerLabel = ctx.nextLabel("cp")
        return MosExpressionCompiler.compileToAX(ctx, expr) ++ List(
          AssemblyLine.immediate(CMP, constant.loByte),
          AssemblyLine.relative(BNE, innerLabel),
          AssemblyLine.immediate(CPX, constant.hiByte),
          AssemblyLine.relative(BEQ, Label(x)),
          AssemblyLine.label(innerLabel))
      case (expr, None, _, Some(constant)) if effectiveComparisonType == ComparisonType.NotEqual =>
        return MosExpressionCompiler.compileToAX(ctx, expr) ++ List(
          AssemblyLine.immediate(CMP, constant.loByte),
          AssemblyLine.relative(BNE, Label(x)),
          AssemblyLine.immediate(CPX, constant.hiByte),
          AssemblyLine.relative(BNE, Label(x)))
      case (_, Some(constant), expr, None) if effectiveComparisonType == ComparisonType.NotEqual =>
        return MosExpressionCompiler.compileToAX(ctx, expr) ++ List(
          AssemblyLine.immediate(CMP, constant.loByte),
          AssemblyLine.relative(BNE, Label(x)),
          AssemblyLine.immediate(CPX, constant.hiByte),
          AssemblyLine.relative(BNE, Label(x)))
      case _ =>
        // TODO comparing expressions
        ctx.log.error("Too complex expressions in comparison", lhs.position.orElse(rhs.position))
        (Nil, Nil, Nil, Nil, Nil)
    }
    val lType = MosExpressionCompiler.getExpressionType(ctx, lhs)
    val rType = MosExpressionCompiler.getExpressionType(ctx, rhs)
    def isConstant(h: List[AssemblyLine], l: List[AssemblyLine], value: Int): Boolean = {
      (h,l) match {
        case (
          List(AssemblyLine0(CMP, Immediate, NumericConstant(vh, _))),
          List(AssemblyLine0(CMP, Immediate, NumericConstant(vl, _)))
          ) if vh.&(0xff).<<(8) + vl.&(0xff) == value => true
        case _ => false
      }
    }

    val compactEqualityComparison =
      if (isConstant(rh, rl, 0)) {
        Some(cmpTo(LDA, ll) ++ cmpTo(ORA, lh))
      } else if (isConstant(lh, ll, 0)) {
        Some(cmpTo(LDA, rl) ++ cmpTo(ORA, rh))
      } else if (ctx.options.flag(CompilationFlag.OptimizeForSpeed)) {
        None
      } else if (isConstant(rh, rl, 0xffff)) {
          Some(cmpTo(LDA, ll) ++ cmpTo(AND, lh) ++ List(AssemblyLine.immediate(CMP, 0xff)))
      } else if (isConstant(lh, ll, 0xffff)) {
        Some(cmpTo(LDA, rl) ++ cmpTo(AND, rh) ++ List(AssemblyLine.immediate(CMP, 0xff)))
      } else if (lType.size == 1 && !lType.isSigned) {
        Some(cmpTo(LDA, ll) ++ cmpTo(EOR, rl) ++ cmpTo(ORA, rh))
      } else if (rType.size == 1 && !rType.isSigned) {
        Some(cmpTo(LDA, rl) ++ cmpTo(EOR, ll) ++ cmpTo(ORA, lh))
      } else {
        None
      }
    effectiveComparisonType match {
      case ComparisonType.Equal =>
        compactEqualityComparison match {
          case Some(code) => code :+ AssemblyLine.relative(BEQ, Label(x))
          case None =>
            val innerLabel = ctx.nextLabel("cp")
            cmpTo(LDA, ll) ++
              cmpTo(CMP, rl) ++
              List(AssemblyLine.relative(BNE, innerLabel)) ++
              cmpTo(LDA, lh) ++
              cmpTo(CMP, rh) ++
              List(
                AssemblyLine.relative(BEQ, Label(x)),
                AssemblyLine.label(innerLabel))
        }

      case ComparisonType.NotEqual =>
        compactEqualityComparison match {
          case Some(code) => code :+ AssemblyLine.relative(BNE, Label(x))
          case None =>
            cmpTo(LDA, ll) ++
              cmpTo(CMP, rl) ++
              List(AssemblyLine.relative(BNE, Label(x))) ++
              cmpTo(LDA, lh) ++
              cmpTo(CMP, rh) ++
              List(AssemblyLine.relative(BNE, Label(x)))
        }

      case ComparisonType.LessUnsigned =>
        val innerLabel = ctx.nextLabel("cp")
        cmpTo(LDA, lh) ++
          cmpTo(CMP, rh) ++
          List(
            AssemblyLine.relative(BCC, Label(x)),
            AssemblyLine.relative(BNE, innerLabel)) ++
          cmpTo(LDA, ll) ++
          cmpTo(CMP, rl) ++
          List(
            AssemblyLine.relative(BCC, Label(x)),
            AssemblyLine.label(innerLabel))

      case ComparisonType.LessOrEqualUnsigned =>
        val innerLabel = ctx.nextLabel("cp")
        cmpTo(LDA, rh) ++
          cmpTo(CMP, lh) ++
          List(AssemblyLine.relative(BCC, innerLabel),
            AssemblyLine.relative(BNE, x)) ++
          cmpTo(LDA, rl) ++
          cmpTo(CMP, ll) ++
          List(AssemblyLine.relative(BCS, x),
            AssemblyLine.label(innerLabel))

      case ComparisonType.GreaterUnsigned =>
        val innerLabel = ctx.nextLabel("cp")
        cmpTo(LDA, rh) ++
          cmpTo(CMP, lh) ++
          List(AssemblyLine.relative(BCC, Label(x)),
            AssemblyLine.relative(BNE, innerLabel)) ++
          cmpTo(LDA, rl) ++
          cmpTo(CMP, ll) ++
          List(AssemblyLine.relative(BCC, Label(x)),
            AssemblyLine.label(innerLabel))

      case ComparisonType.GreaterOrEqualUnsigned =>
        val innerLabel = ctx.nextLabel("cp")
        cmpTo(LDA, lh) ++
          cmpTo(CMP, rh) ++
          List(AssemblyLine.relative(BCC, innerLabel),
            AssemblyLine.relative(BNE, x)) ++
          cmpTo(LDA, ll) ++
          cmpTo(CMP, rl) ++
          List(AssemblyLine.relative(BCS, x),
            AssemblyLine.label(innerLabel))

      case ComparisonType.LessSigned =>
        val fixup = ctx.nextLabel("co")
        cmpTo(LDA, ll) ++
          List(AssemblyLine.implied(SEC)) ++
          cmpTo(SBC, rl) ++
          cmpTo(LDA, lh) ++
          cmpTo(SBC, rh) ++
          List(
            AssemblyLine.relative(BVC, fixup),
            AssemblyLine.immediate(EOR, 0x80),
            AssemblyLine.label(fixup))
        List(AssemblyLine.relative(BCC, x))

      case ComparisonType.GreaterOrEqualSigned =>
        val fixup = ctx.nextLabel("co")
        cmpTo(LDA, ll) ++
          List(AssemblyLine.implied(SEC)) ++
          cmpTo(SBC, rl) ++
          cmpTo(LDA, lh) ++
          cmpTo(SBC, rh) ++
          List(
            AssemblyLine.relative(BVC, fixup),
            AssemblyLine.immediate(EOR, 0x80),
            AssemblyLine.label(fixup))
        List(AssemblyLine.relative(BCS, x))
      case _ => ???
      // TODO: signed word comparisons: <=, >
    }
  }

  def compileLongComparison(ctx: CompilationContext, compType: ComparisonType.Value, lhs: Expression, rhs: Expression, size:Int, branches: BranchSpec, alreadyFlipped: Boolean = false): List[AssemblyLine] = {
    val rType = MosExpressionCompiler.getExpressionType(ctx, rhs)
    if (rType.size < size && rType.isSigned) {
      if (alreadyFlipped) ???
      else return compileLongComparison(ctx, ComparisonType.flip(compType), rhs, lhs, size, branches, alreadyFlipped = true)
    }

    val (effectiveComparisonType, label) = branches match {
      case NoBranching => return Nil
      case BranchIfTrue(x) => compType -> x
      case BranchIfFalse(x) => ComparisonType.negate(compType) -> x
    }

    // TODO: check for carry flag clobbering
    val l = getLoadForEachByte(ctx, lhs, size)
    val r = getLoadForEachByte(ctx, rhs, size)

    val mask = (1L << (size * 8)) - 1
    (ctx.env.eval(lhs), ctx.env.eval(rhs)) match {
      case (Some(NumericConstant(lc, _)), Some(NumericConstant(rc, _))) =>
        return if (effectiveComparisonType match {
          // TODO: those masks are probably wrong
          case ComparisonType.Equal =>
            (lc & mask) == (rc & mask) // ??
          case ComparisonType.NotEqual =>
            (lc & mask) != (rc & mask) // ??

          case ComparisonType.LessOrEqualUnsigned =>
            (lc & mask) <= (rc & mask)
          case ComparisonType.GreaterOrEqualUnsigned =>
            (lc & mask) >= (rc & mask)
          case ComparisonType.GreaterUnsigned =>
            (lc & mask) > (rc & mask)
          case ComparisonType.LessUnsigned =>
            (lc & mask) < (rc & mask)

          case ComparisonType.LessOrEqualSigned =>
            signExtend(lc, mask) <= signExtend(lc, mask)
          case ComparisonType.GreaterOrEqualSigned =>
            signExtend(lc, mask) >= signExtend(lc, mask)
          case ComparisonType.GreaterSigned =>
            signExtend(lc, mask) > signExtend(lc, mask)
          case ComparisonType.LessSigned =>
            signExtend(lc, mask) < signExtend(lc, mask)
        }) List(AssemblyLine.absolute(JMP, Label(label))) else Nil
      case  _ =>
        effectiveComparisonType match {
          case ComparisonType.Equal =>
            val innerLabel = ctx.nextLabel("cp")
            val bytewise = l.zip(r).map{
              case (cmpL, cmpR) => cmpTo(LDA, cmpL) ++ cmpTo(CMP, cmpR)
            }
            bytewise.init.flatMap(b => b :+ AssemblyLine.relative(BNE, innerLabel)) ++ bytewise.last ++List(
                AssemblyLine.relative(BEQ, Label(label)),
                AssemblyLine.label(innerLabel))
          case ComparisonType.NotEqual =>
            l.zip(r).flatMap {
              case (cmpL, cmpR) => cmpTo(LDA, cmpL) ++ cmpTo(CMP, cmpR) :+ AssemblyLine.relative(BNE, label)
            }
          case ComparisonType.LessUnsigned =>
            val calculateCarry = AssemblyLine.implied(SEC) :: l.zip(r).flatMap{
              case (cmpL, cmpR) => cmpTo(LDA, cmpL) ++ cmpTo(SBC, cmpR)
            }
            calculateCarry ++ List(AssemblyLine.relative(BCC, Label(label)))
          case ComparisonType.GreaterOrEqualUnsigned =>
            val calculateCarry = AssemblyLine.implied(SEC) :: l.zip(r).flatMap{
              case (cmpL, cmpR) => cmpTo(LDA, cmpL) ++ cmpTo(SBC, cmpR)
            }
            calculateCarry ++ List(AssemblyLine.relative(BCS, Label(label)))
          case ComparisonType.GreaterUnsigned | ComparisonType.LessOrEqualUnsigned =>
            compileLongComparison(ctx, ComparisonType.flip(compType), rhs, lhs, size, branches, alreadyFlipped = true)
          case _ =>
            ctx.log.error("Long signed comparisons are not yet supported", lhs.position)
            Nil
        }
    }

  }

  private def signExtend(value: Long, mask: Long): Long = {
    val masked = value & mask
    if (masked > mask/2) masked | ~mask
    else masked
  }

  def compileInPlaceByteMultiplication(ctx: CompilationContext, v: LhsExpression, addend: Expression): List[AssemblyLine] = {
    val b = ctx.env.get[Type]("byte")
    ctx.env.eval(addend) match {
      case Some(NumericConstant(0, _)) =>
        MosExpressionCompiler.compile(ctx, v, None, NoBranching) ++ (AssemblyLine.immediate(LDA, 0) :: MosExpressionCompiler.compileByteStorage(ctx, MosRegister.A, v))
      case Some(NumericConstant(1, _)) =>
        MosExpressionCompiler.compile(ctx, v, None, NoBranching)
      case Some(NumericConstant(x, _)) =>
        compileByteMultiplication(ctx, v, x.toInt) ++ MosExpressionCompiler.compileByteStorage(ctx, MosRegister.A, v)
      case _ =>
        PseudoregisterBuiltIns.compileByteMultiplication(ctx, Some(v), addend, storeInRegLo = false) ++ MosExpressionCompiler.compileByteStorage(ctx, MosRegister.A, v)
    }
  }

  private def isPowerOfTwoUpTo15(n: Long): Boolean = if (n <= 0 || n >= 0x8000) false else 0 == ((n-1) & n)

  def compileInPlaceWordMultiplication(ctx: CompilationContext, v: LhsExpression, addend: Expression): List[AssemblyLine] = {
    if (v.isInstanceOf[DerefExpression]) {
      ctx.log.error("Too complex left-hand-side expression")
      return MosExpressionCompiler.compileToAX(ctx, v) ++ MosExpressionCompiler.compileToAX(ctx, addend)
    }
    val b = ctx.env.get[Type]("byte")
    val w = ctx.env.get[Type]("word")
    ctx.env.eval(addend) match {
      case Some(NumericConstant(0, _)) =>
        MosExpressionCompiler.compile(ctx, v, None, NoBranching) ++ MosExpressionCompiler.compileAssignment(ctx, LiteralExpression(0, 2), v)
      case Some(NumericConstant(1, _)) =>
        MosExpressionCompiler.compile(ctx, v, None, NoBranching)
      case Some(NumericConstant(n, _)) if isPowerOfTwoUpTo15(n) =>
        BuiltIns.compileInPlaceWordOrLongShiftOps(ctx, v, LiteralExpression(java.lang.Long.bitCount(n - 1), 1), aslRatherThanLsr = true)
      case _ =>
        // TODO: optimize?
        PseudoregisterBuiltIns.compileWordMultiplication(ctx, Some(v), addend, storeInRegLo = true) ++
          MosExpressionCompiler.compileAssignment(ctx, VariableExpression("__reg.loword"), v)
    }
  }

  def compileByteMultiplication(ctx: CompilationContext, v: Expression, c: Int): List[AssemblyLine] = {
    val result = ListBuffer[AssemblyLine]()
    // TODO: optimise
    val addingCode = simpleOperation(ADC, ctx, v, IndexChoice.PreferY, preserveA = false, commutative = false, decimal = false)
    val adc = addingCode.last
    val indexing = addingCode.init
    result ++= indexing
    result += AssemblyLine.immediate(LDA, 0)
    val mult = c & 0xff
    var mask = 128
    var empty = true
    while (mask > 0) {
      if (!empty) {
        result += AssemblyLine.implied(ASL)
      }
      if ((mult & mask) != 0) {
        result ++= List(AssemblyLine.implied(CLC), adc)
        empty = false
      }

      mask >>>= 1
    }
    result.toList
  }

  //noinspection ZeroIndexToHead
  def compileByteMultiplication(ctx: CompilationContext, params: List[Expression]): List[AssemblyLine] = {
    val (constants, variables) = params.map(p => p -> ctx.env.eval(p)).partition(_._2.exists(_.isInstanceOf[NumericConstant]))
    val constant = constants.map(_._2.get.asInstanceOf[NumericConstant].value).foldLeft(1L)(_ * _).toInt
    variables.length match {
      case 0 => List(AssemblyLine.immediate(LDA, constant & 0xff))
      case 1 =>
        val sim = simplicity(ctx.env, variables.head._1)
        if (sim >= 'I') {
          compileByteMultiplication(ctx, variables.head._1, constant)
        } else {
          MosExpressionCompiler.compileToA(ctx, variables.head._1) ++
            List(AssemblyLine.zeropage(STA, ctx.env.get[ThingInMemory]("__reg.b0"))) ++
            compileByteMultiplication(ctx, VariableExpression("__reg.b0"), constant)
        }
      case 2 =>
        if (constant == 1)
          PseudoregisterBuiltIns.compileByteMultiplication(ctx, Some(variables(0)._1), variables(1)._1, storeInRegLo = false)
        else
          PseudoregisterBuiltIns.compileByteMultiplication(ctx, Some(variables(0)._1), variables(1)._1, storeInRegLo = true) ++
          compileByteMultiplication(ctx, VariableExpression("__reg.b0"), constant)
      case _ => ??? // TODO
    }
  }

  def compileUnsignedByteDivision(ctx: CompilationContext, p: Expression, q: Expression, modulo: Boolean): List[AssemblyLine] = {
    if (ctx.options.zpRegisterSize < 1) {
      ctx.log.error("Byte division requires the zeropage pseudoregister", p.position)
      return Nil
    }
    ctx.env.eval(q) match {
      case Some(NumericConstant(qq, _)) =>
        if (qq < 0) {
          ctx.log.error("Unsigned division by negative constant", q.position)
          Nil
        } else if (qq == 0) {
          ctx.log.error("Unsigned division by zero", q.position)
          Nil
        } else if (qq > 255) {
          if (modulo) MosExpressionCompiler.compileToA(ctx, p)
          else List(AssemblyLine.immediate(LDA, 0))
        } else {
          compileUnsignedByteDivision(ctx, p, qq.toInt, modulo)
        }
      case Some(_) =>
        ctx.log.error("Unsigned division by unknown constant", q.position)
        Nil
      case None =>
        ctx.log.error("Unsigned division by a variable expression", q.position)
        Nil
    }
  }
  def compileUnsignedByteDivision(ctx: CompilationContext, p: Expression, q: Int, modulo: Boolean): List[AssemblyLine] = {
    val reg = ctx.env.get[VariableInMemory]("__reg")
    val initP = MosExpressionCompiler.compileToA(ctx, p)
    val result = ListBuffer[AssemblyLine]()
    if (ctx.options.flag(CompilationFlag.EmitCmosOpcodes)) {
      result ++= initP
      result += AssemblyLine.zeropage(STZ, reg)
    } else if (MosExpressionCompiler.changesZpreg(initP, 0)) {
      result ++= initP
      result += AssemblyLine.implied(PHA)
      result += AssemblyLine.immediate(LDA, 0)
      result += AssemblyLine.zeropage(STA, reg)
      result += AssemblyLine.implied(PLA)
    } else {
      result += AssemblyLine.immediate(LDA, 0)
      result += AssemblyLine.zeropage(STA, reg)
      result ++= initP
    }

    for (i <- 7.to(0, -1)) {
      if ((q << i) <= 255) {
        val lbl = ctx.nextLabel("dv")
        result += AssemblyLine.immediate(CMP, q << i)
        result += AssemblyLine.relative(BCC, lbl)
        result += AssemblyLine.immediate(SBC, q << i)
        result += AssemblyLine.label(lbl)
        result += AssemblyLine.zeropage(ROL, reg)
      }
    }
    if (!modulo) {
      result += AssemblyLine.zeropage(LDA, reg)
    }
    result.toList
  }

  def compileInPlaceByteAddition(ctx: CompilationContext, v: LhsExpression, addend: Expression, subtract: Boolean, decimal: Boolean): List[AssemblyLine] = {
    if (decimal && !ctx.options.flag(CompilationFlag.DecimalMode) && ctx.options.zpRegisterSize < 4) {
      ctx.log.error("Unsupported decimal operation. Consider increasing the size of the zeropage register.", v.position)
      return compileInPlaceByteAddition(ctx, v, addend, subtract, decimal = false)
    }
    val env = ctx.env
    val b = env.get[Type]("byte")
    val lhsIsDirectlyIncrementable = v match {
      case _: VariableExpression => true
      case IndexedExpression(pointy, indexExpr) =>
        val indexerSize = getIndexerSize(ctx, indexExpr)
        indexerSize <= 1 && (env.getPointy(pointy) match {
          case _: ConstantPointy => true
          case _: VariablePointy => false
        })
      case _ => false
    }
    env.eval(addend) match {
      case Some(NumericConstant(0, _)) => MosExpressionCompiler.compile(ctx, v, None, NoBranching)
      case Some(NumericConstant(1, _)) if lhsIsDirectlyIncrementable && !decimal => if (subtract) {
        simpleOperation(DEC, ctx, v, IndexChoice.RequireX, preserveA = false, commutative = true)
      } else {
        simpleOperation(INC, ctx, v, IndexChoice.RequireX, preserveA = false, commutative = true)
      }
      // TODO: compile +=2 to two INCs
      case Some(NumericConstant(-1, _)) if lhsIsDirectlyIncrementable && !decimal => if (subtract) {
        simpleOperation(INC, ctx, v, IndexChoice.RequireX, preserveA = false, commutative = true)
      } else {
        simpleOperation(DEC, ctx, v, IndexChoice.RequireX, preserveA = false, commutative = true)
      }
      case _ =>
        if (decimal && !ctx.options.flag(CompilationFlag.DecimalMode)) {
          val reg = ctx.env.get[MemoryVariable]("__reg")
          val loadRhs = MosExpressionCompiler.compile(ctx, addend, Some(b -> ctx.env.genRelativeVariable(reg.toAddress + 3, b, zeropage = true)), NoBranching)
          val loadLhs = MosExpressionCompiler.compileToA(ctx, v)
          val storeLhs = MosExpressionCompiler.compileByteStorage(ctx, MosRegister.A, v)
          val subroutine = if (subtract) "__sub_decimal" else "__adc_decimal"
          loadRhs ++ MosExpressionCompiler.preserveZpregIfNeededDestroyingAAndX(ctx, 3, loadLhs) ++ List(
            AssemblyLine.zeropage(STA, reg, 2)) ++ (if (subtract) List(
            AssemblyLine.absolute(JSR, ctx.env.get[ThingInMemory]("__sub_decimal"))
          ) else List(
            AssemblyLine.implied(CLC),
            AssemblyLine.absolute(JSR, ctx.env.get[ThingInMemory]("__adc_decimal"))
          )) ++ storeLhs
        } else if (!subtract && simplicity(env, v) > simplicity(env, addend)) {
          val loadRhs = MosExpressionCompiler.compile(ctx, addend, Some(b -> RegisterVariable(MosRegister.A, b)), NoBranching)
          val modifyAcc = insertBeforeLast(AssemblyLine.implied(CLC), simpleOperation(ADC, ctx, v, IndexChoice.PreferY, preserveA = true, commutative = true, decimal = decimal))
          val storeLhs = MosExpressionCompiler.compileByteStorage(ctx, MosRegister.A, v)
          loadRhs ++ modifyAcc ++ storeLhs
        } else {
          val loadLhs = MosExpressionCompiler.compile(ctx, v, Some(b -> RegisterVariable(MosRegister.A, b)), NoBranching)
          val modifyLhs = if (subtract) {
            insertBeforeLast(AssemblyLine.implied(SEC), simpleOperation(SBC, ctx, addend, IndexChoice.PreferY, preserveA = true, commutative = false, decimal = decimal))
          } else {
            insertBeforeLast(AssemblyLine.implied(CLC), simpleOperation(ADC, ctx, addend, IndexChoice.PreferY, preserveA = true, commutative = true, decimal = decimal))
          }
          val storeLhs = MosExpressionCompiler.compileByteStorage(ctx, MosRegister.A, v)
          loadLhs ++ modifyLhs ++ storeLhs
        }
    }
  }

  private def getIndexerSize(ctx: CompilationContext, indexExpr: Expression): Int = {
    ctx.env.evalVariableAndConstantSubParts(indexExpr)._1.map(v => MosExpressionCompiler.getExpressionType(ctx, v).size).sum
  }

  def compileInPlaceWordOrLongAddition(ctx: CompilationContext, lhs: LhsExpression, addend: Expression, subtract: Boolean, decimal: Boolean): List[AssemblyLine] = {
    if (decimal && !ctx.options.flag(CompilationFlag.DecimalMode) && ctx.options.zpRegisterSize < 4) {
      ctx.log.error("Unsupported decimal operation. Consider increasing the size of the zeropage register.", lhs.position)
      return compileInPlaceWordOrLongAddition(ctx, lhs, addend, subtract, decimal = false)
    }
    if (lhs.isInstanceOf[DerefExpression]) {
      ctx.log.error("Too complex left-hand-side expression")
      return MosExpressionCompiler.compileToAX(ctx, lhs) ++ MosExpressionCompiler.compileToAX(ctx, addend)
    }
    val env = ctx.env
    val b = env.get[Type]("byte")
    val w = env.get[Type]("word")
    val targetBytes: List[List[AssemblyLine]] = getStorageForEachByte(ctx, lhs)
    val lhsIsStack = targetBytes.head.head.opcode == TSX
    val targetSize = targetBytes.size
    val addendType = MosExpressionCompiler.getExpressionType(ctx, addend)
    var addendSize = addendType.size

    def isRhsComplex(xs: List[AssemblyLine]): Boolean = xs match {
      case AssemblyLine0(LDA, _, _) :: Nil => false
      case AssemblyLine0(LDA, _, _) :: AssemblyLine0(LDX, _, _) :: Nil => false
      case _ => true
    }

    def isRhsStack(xs: List[AssemblyLine]): Boolean = xs.exists(_.opcode == TSX)

    val canUseIncDec = !decimal && targetBytes.forall(_.forall(l => l.opcode != STA || (l.addrMode match {
      case AddrMode.Absolute => true
      case AddrMode.AbsoluteX => true
      case AddrMode.ZeroPage => true
      case AddrMode.ZeroPageX => true
      case _ => false
    })))

    def doDec(lines: List[List[AssemblyLine]]):List[AssemblyLine] = lines match {
      case Nil => Nil
      case x :: Nil => staTo(DEC, x)
      case x :: xs =>
        val label = ctx.nextLabel("de")
        staTo(LDA, x) ++
          List(AssemblyLine.relative(BNE, label)) ++
          doDec(xs) ++
          List(AssemblyLine.label(label)) ++
          staTo(DEC, x)
    }

    val (calculateRhs, addendByteRead0): (List[AssemblyLine], List[List[AssemblyLine]]) = env.eval(addend) match {
      case Some(NumericConstant(0, _)) =>
        return MosExpressionCompiler.compile(ctx, lhs, None, NoBranching)
      case Some(NumericConstant(1, _)) if canUseIncDec && !subtract =>
        if (ctx.options.flags(CompilationFlag.Emit65CE02Opcodes)) {
          targetBytes match {
            case List(List(AssemblyLine0(STA, ZeroPage, l)), List(AssemblyLine0(STA, ZeroPage, h))) =>
              if (l.+(1).quickSimplify == h) {
                return List(AssemblyLine.zeropage(INC_W, l))
              }
            case _ =>
          }
        }
        if (ctx.options.flags(CompilationFlag.EmitNative65816Opcodes)) {
          targetBytes match {
            case List(List(AssemblyLine0(STA, a1@(ZeroPage | Absolute | ZeroPageX | AbsoluteX), l)), List(AssemblyLine0(STA, a2, h))) =>
              if (a1 == a2 && l.+(1).quickSimplify == h) {
                return List(AssemblyLine.accu16, AssemblyLine(INC_W, a1, l), AssemblyLine.accu8)
              }
          }
        }
        val label = ctx.nextLabel("in")
        return staTo(INC, targetBytes.head) ++ targetBytes.tail.flatMap(l => AssemblyLine.relative(BNE, label)::staTo(INC, l)) :+ AssemblyLine.label(label)
      case Some(NumericConstant(-1, _)) if canUseIncDec && subtract =>
        if (ctx.options.flags(CompilationFlag.Emit65CE02Opcodes)) {
          targetBytes match {
            case List(List(AssemblyLine0(STA, ZeroPage, l)), List(AssemblyLine0(STA, ZeroPage, h))) =>
              if (l.+(1).quickSimplify == h) {
                return List(AssemblyLine.zeropage(INC_W, l))
              }
          }
        }
        if (ctx.options.flags(CompilationFlag.EmitNative65816Opcodes)) {
          targetBytes match {
            case List(List(AssemblyLine0(STA, a1@(ZeroPage | Absolute | ZeroPageX | AbsoluteX), l)), List(AssemblyLine0(STA, a2, h))) =>
              if (a1 == a2 && l.+(1).quickSimplify == h) {
                return List(AssemblyLine.accu16, AssemblyLine(INC_W, a1, l), AssemblyLine.accu8)
              }
          }
        }
        val label = ctx.nextLabel("in")
        return staTo(INC, targetBytes.head) ++ targetBytes.tail.flatMap(l => AssemblyLine.relative(BNE, label)::staTo(INC, l)) :+ AssemblyLine.label(label)
      case Some(NumericConstant(1, _)) if canUseIncDec && subtract =>
        if (ctx.options.flags(CompilationFlag.Emit65CE02Opcodes)) {
          targetBytes match {
            case List(List(AssemblyLine0(STA, ZeroPage, l)), List(AssemblyLine0(STA, ZeroPage, h))) =>
              if (l.+(1).quickSimplify == h) {
                return List(AssemblyLine.zeropage(DEC_W, l))
              }
          }
        }
        if (ctx.options.flags(CompilationFlag.EmitNative65816Opcodes)) {
          targetBytes match {
            case List(List(AssemblyLine0(STA, a1@(ZeroPage | Absolute | ZeroPageX | AbsoluteX), l)), List(AssemblyLine0(STA, a2, h))) =>
              if (a1 == a2 && l.+(1).quickSimplify == h) {
                return List(AssemblyLine.accu16, AssemblyLine(DEC_W, a1, l), AssemblyLine.accu8)
              }
          }
        }
        val label = ctx.nextLabel("de")
        return doDec(targetBytes)
      case Some(NumericConstant(-1, _)) if canUseIncDec && !subtract =>
        if (ctx.options.flags(CompilationFlag.Emit65CE02Opcodes)) {
          targetBytes match {
            case List(List(AssemblyLine0(STA, ZeroPage, l)), List(AssemblyLine0(STA, ZeroPage, h))) =>
              if (l.+(1).quickSimplify == h) {
                return List(AssemblyLine.zeropage(DEC_W, l))
              }
          }
        }
        if (ctx.options.flags(CompilationFlag.EmitNative65816Opcodes)) {
          targetBytes match {
            case List(List(AssemblyLine0(STA, a1@(ZeroPage | Absolute | ZeroPageX | AbsoluteX), l)), List(AssemblyLine0(STA, a2, h))) =>
              if (a1 == a2 && l.+(1).quickSimplify == h) {
                return List(AssemblyLine.accu16, AssemblyLine(DEC_W, a1, l), AssemblyLine.accu8)
              }
          }
        }
        val label = ctx.nextLabel("de")
        return doDec(targetBytes)
      case Some(constant) =>
        addendSize = targetSize
        Nil -> List.tabulate(targetSize)(i => List(AssemblyLine.immediate(LDA, constant.subbyte(i))))
      case None =>
        addendSize match {
          case 1 =>
            val base = MosExpressionCompiler.compile(ctx, addend, Some(b -> RegisterVariable(MosRegister.A, b)), NoBranching)
            if (subtract) {
              if (isRhsComplex(base)) {
                if (isRhsStack(base)) {
                  ctx.log.warn("Subtracting a stack-based value", addend.position)
                  ???
                }
                (base ++ List(AssemblyLine.implied(PHA))) -> List(List(AssemblyLine.implied(TSX), AssemblyLine.absoluteX(LDA, 0x101)))
              } else {
                Nil -> base.map(l => l.copy(opcode = LDA) :: Nil)
              }
            } else {
              base -> List(Nil)
            }
          case 2 =>
            val base = MosExpressionCompiler.compile(ctx, addend, Some(MosExpressionCompiler.getExpressionType(ctx, addend) -> RegisterVariable(MosRegister.AX, w)), NoBranching)
            if (isRhsStack(base)) {
              val fixedBase = MosExpressionCompiler.compile(ctx, addend, Some(MosExpressionCompiler.getExpressionType(ctx, addend) -> RegisterVariable(MosRegister.AY, w)), NoBranching)
              if (subtract) {
                ctx.log.warn("Subtracting a stack-based value", addend.position)
                if (isRhsComplex(base)) {
                  ???
                } else {
                  Nil -> fixedBase.map(l => l.copy(opcode = LDA) :: Nil)
                  ???
                }
              } else {
                fixedBase -> List(Nil, List(AssemblyLine.implied(TYA)))
              }
            } else {
              if (subtract) {
                if (isRhsComplex(base)) {
                  (base ++ List(
                    AssemblyLine.implied(PHA),
                    AssemblyLine.implied(TXA),
                    AssemblyLine.implied(PHA))
                    ) -> List(
                    List(AssemblyLine.implied(TSX), AssemblyLine.absoluteX(LDA, 0x102)),
                    List(AssemblyLine.implied(TSX), AssemblyLine.absoluteX(LDA, 0x101)))
                } else {
                  Nil -> base.map(l => l.copy(opcode = LDA) :: Nil)
                }
              } else {
                if (lhsIsStack) {
                  val fixedBase = MosExpressionCompiler.compile(ctx, addend, Some(MosExpressionCompiler.getExpressionType(ctx, addend) -> RegisterVariable(MosRegister.AY, w)), NoBranching)
                  fixedBase -> List(Nil, List(AssemblyLine.implied(TYA)))
                } else {
                  base -> List(Nil, List(AssemblyLine.implied(TXA)))
                }
              }
            }
          case _ => addend match {
            case vv: VariableExpression =>
              val source = env.get[Variable](vv.name)
              Nil -> List.tabulate(addendSize)(i => AssemblyLine.variable(ctx, LDA, source, i))
            case f: FunctionCallExpression =>
              val jsr = MosExpressionCompiler.compile(ctx, addend, None, BranchSpec.None)
              val result = ctx.env.get[VariableInMemory](f.functionName + ".return")
              jsr -> List.tabulate(addendSize)(i => AssemblyLine.variable(ctx, LDA, result, i))
          }
        }
    }
    val addendByteRead = addendByteRead0 ++ List.fill((targetSize - addendByteRead0.size) max 0)(List(AssemblyLine.immediate(LDA, 0)))

    if (ctx.options.flags(CompilationFlag.EmitNative65816Opcodes)) {
      (removeTsx(targetBytes), calculateRhs, removeTsx(addendByteRead)) match {
        case (
          List(List(AssemblyLine0(STA, ta1, tl)), List(AssemblyLine0(STA, ta2, th))),
          Nil,
          List(List(AssemblyLine0(LDA, Immediate, al)), List(AssemblyLine0(LDA, Immediate, ah)))) =>
          if (ta1 == ta2 && tl.+(1).quickSimplify == th) {
            return wrapInSedCldIfNeeded(decimal, List(
              AssemblyLine.implied(if(subtract) SEC else CLC),
              AssemblyLine.accu16,
              AssemblyLine(LDA_W, ta1, tl),
              AssemblyLine(if(subtract) SBC_W else ADC_W, WordImmediate, ah.asl(8).+(al).quickSimplify),
              AssemblyLine(STA_W, ta1, tl),
              AssemblyLine.accu8))
          }
        case (
          List(List(AssemblyLine0(STA, ta1, tl)), List(AssemblyLine0(STA, ta2, th))),
          Nil,
          List(List(AssemblyLine0(LDA, aa1, al)), List(AssemblyLine0(LDA, aa2, ah)))) =>
          if (ta1 == ta2 && aa1 == aa2 && tl.+(1).quickSimplify == th && al.+(1).quickSimplify == ah) {
            return wrapInSedCldIfNeeded(decimal, List(
              AssemblyLine.accu16,
              AssemblyLine.implied(if(subtract) SEC else CLC),
              AssemblyLine(LDA_W, ta1, tl),
              AssemblyLine(if(subtract) SBC_W else ADC_W, aa1, al),
              AssemblyLine(STA_W, ta1, tl),
              AssemblyLine.accu8))
          }
        case (
          List(List(AssemblyLine0(STA, ta1, tl)), List(AssemblyLine0(STA, ta2, th))),
          List(AssemblyLine0(TSX, _, _), AssemblyLine0(LDA, AbsoluteX, NumericConstant(al, _)), AssemblyLine0(LDY, AbsoluteX, NumericConstant(ah, _))),
          List(Nil, List(AssemblyLine0(TYA, _, _)))) =>
          if (ta1 == ta2 && tl.+(1).quickSimplify == th && al + 1 == ah) {
            return wrapInSedCldIfNeeded(decimal, List(
              AssemblyLine.accu16,
              AssemblyLine.implied(if(subtract) SEC else CLC),
              AssemblyLine(LDA_W, ta1, tl),
              AssemblyLine(if(subtract) SBC_W else ADC_W, Stack, NumericConstant(al & 0xff, 1)),
              AssemblyLine(STA_W, ta1, tl),
              AssemblyLine.accu8))
          }
        case _ =>
      }
    }
    val buffer = mutable.ListBuffer[AssemblyLine]()
    buffer ++= calculateRhs
    buffer += AssemblyLine.implied(if (subtract) SEC else CLC)
    val extendMultipleBytes = targetSize > addendSize + 1
    val extendAtLeastOneByte = targetSize > addendSize
    for (i <- 0 until targetSize) {
      if (decimal && !ctx.options.flag(CompilationFlag.DecimalMode)) {
        val reg = ctx.env.get[VariableInMemory]("__reg")
        buffer ++= staTo(LDA, targetBytes(i))
        if (targetBytes(i).isEmpty) {
          buffer += AssemblyLine.immediate(LDA, 0)
        }
        buffer += AssemblyLine.zeropage(STA, reg, 2)
        // TODO: AX?
        buffer ++= MosExpressionCompiler.preserveZpregIfNeededDestroyingAAndX(ctx, 2, addendByteRead(i))
        buffer += AssemblyLine.zeropage(STA, reg, 3)
        if (subtract) {
          if (i == 0) {
            buffer += AssemblyLine.absolute(JSR, env.get[ThingInMemory]("__sub_decimal"))
          } else {
            buffer += AssemblyLine.absolute(JSR, env.get[ThingInMemory]("__sbc_decimal"))
          }
        } else {
          if (i == 0) {
            buffer += AssemblyLine.implied(CLC)
          }
          buffer += AssemblyLine.absolute(JSR, env.get[ThingInMemory]("__adc_decimal"))
        }
        buffer ++= targetBytes(i)
      } else if (subtract) {
        if (addendSize < targetSize && addendType.isSigned) {
          // TODO: sign extension
          ???
        }
        buffer ++= staTo(LDA, targetBytes(i))
        buffer ++= wrapInSedCldIfNeeded(decimal, ldTo(SBC, addendByteRead(i)))
        buffer ++= targetBytes(i)
      } else {
        if (i >= addendSize) {
          if (addendType.isSigned) {
            val label = ctx.nextLabel("sx")
            buffer += AssemblyLine.implied(TXA)
            if (i == addendSize) {
              buffer += AssemblyLine.immediate(ORA, 0x7f)
              buffer += AssemblyLine.relative(BMI, label)
              buffer += AssemblyLine.immediate(LDA, 0)
              buffer += AssemblyLine.label(label)
              if (extendMultipleBytes) buffer += AssemblyLine.implied(TAX)
            }
          } else {
            buffer += AssemblyLine.immediate(LDA, 0)
          }
        } else {
          buffer ++= addendByteRead(i)
          if (addendType.isSigned && i == addendSize - 1 && extendAtLeastOneByte) {
            buffer += AssemblyLine.implied(TAX)
          }
        }
        buffer ++= wrapInSedCldIfNeeded(decimal, staTo(ADC, targetBytes(i)))
        buffer ++= targetBytes(i)
      }
    }
    for (i <- 0 until calculateRhs.count(a => a.opcode == PHA) - calculateRhs.count(a => a.opcode == PLA)) {
      buffer += AssemblyLine.implied(PLA)
    }
    buffer.toList
  }

  def compileInPlaceByteBitOp(ctx: CompilationContext, v: LhsExpression, param: Expression, operation: Opcode.Value): List[AssemblyLine] = {
    val env = ctx.env
    val b = env.get[Type]("byte")
    (operation, env.eval(param)) match {
      case (EOR, Some(NumericConstant(0, _)))
           | (ORA, Some(NumericConstant(0, _)))
           | (AND, Some(NumericConstant(0xff, _)))
           | (AND, Some(NumericConstant(-1, _))) =>
        MosExpressionCompiler.compile(ctx, v, None, NoBranching)
      case _ =>
        if (simplicity(env, v) > simplicity(env, param)) {
          val loadRhs = MosExpressionCompiler.compile(ctx, param, Some(b -> RegisterVariable(MosRegister.A, b)), NoBranching)
          val modifyAcc = simpleOperation(operation, ctx, v, IndexChoice.PreferY, preserveA = true, commutative = true)
          val storeLhs = MosExpressionCompiler.compileByteStorage(ctx, MosRegister.A, v)
          loadRhs ++ modifyAcc ++ storeLhs
        } else {
          val loadLhs = MosExpressionCompiler.compile(ctx, v, Some(b -> RegisterVariable(MosRegister.A, b)), NoBranching)
          val modifyLhs = simpleOperation(operation, ctx, param, IndexChoice.PreferY, preserveA = true, commutative = true)
          val storeLhs = MosExpressionCompiler.compileByteStorage(ctx, MosRegister.A, v)
          loadLhs ++ modifyLhs ++ storeLhs
        }
    }
  }


  def compileInPlaceWordOrLongBitOp(ctx: CompilationContext, lhs: LhsExpression, param: Expression, operation: Opcode.Value): List[AssemblyLine] = {
    if (lhs.isInstanceOf[DerefExpression]) {
      ctx.log.error("Too complex left-hand-side expression")
      return MosExpressionCompiler.compileToAX(ctx, lhs) ++ MosExpressionCompiler.compileToAX(ctx, param)
    }
    val env = ctx.env
    val b = env.get[Type]("byte")
    val w = env.get[Type]("word")
    val targetBytes: List[List[AssemblyLine]] = getStorageForEachByte(ctx, lhs)
    val lo = targetBytes.head
    val targetSize = targetBytes.size
    val paramType = MosExpressionCompiler.getExpressionType(ctx, param)
    var paramSize = paramType.size
    val extendMultipleBytes = targetSize > paramSize + 1
    val extendAtLeastOneByte = targetSize > paramSize
    val (calculateRhs, addendByteRead) = env.eval(param) match {
      case Some(constant) =>
        paramSize = targetSize
        Nil -> List.tabulate(targetSize)(i => List(AssemblyLine.immediate(LDA, constant.subbyte(i))))
      case None =>
        paramSize match {
          case 1 =>
            val base = MosExpressionCompiler.compile(ctx, param, Some(b -> RegisterVariable(MosRegister.A, b)), NoBranching)
            base -> List(Nil)
          case 2 =>
            val base = MosExpressionCompiler.compile(ctx, param, Some(MosExpressionCompiler.getExpressionType(ctx, param) -> RegisterVariable(MosRegister.AX, w)), NoBranching)
            base -> List(Nil, List(AssemblyLine.implied(TXA)))
          case _ => param match {
            case vv: VariableExpression =>
              val source = env.get[Variable](vv.name)
              Nil -> List.tabulate(paramSize)(i => AssemblyLine.variable(ctx, LDA, source, i))
            case f: FunctionCallExpression =>
              val jsr = MosExpressionCompiler.compile(ctx, param, None, BranchSpec.None)
              val result = ctx.env.get[VariableInMemory](f.functionName + ".return")
              jsr -> List.tabulate(paramSize)(i => AssemblyLine.variable(ctx, LDA, result, i))
          }
        }
    }
    if (ctx.options.flags(CompilationFlag.EmitNative65816Opcodes)) {
      (removeTsx(targetBytes), removeTsx(addendByteRead)) match {
        case (List(List(AssemblyLine0(STA, ta1, tl)), List(AssemblyLine0(STA, ta2, th))), List(List(AssemblyLine0(LDA, Immediate, al)), List(AssemblyLine0(LDA, Immediate, ah)))) =>
          if (ta1 == ta2 && tl.+(1).quickSimplify == th) {
            return List(
              AssemblyLine.accu16,
              AssemblyLine(LDA_W, ta1, tl),
              AssemblyLine(Opcode.widen(operation).get, WordImmediate, ah.asl(8).+(al).quickSimplify),
              AssemblyLine(STA_W, ta1, tl),
              AssemblyLine.accu8)
          }
        case (List(List(AssemblyLine0(STA, ta1, tl)), List(AssemblyLine0(STA, ta2, th))), List(List(AssemblyLine0(LDA, aa1, al)), List(AssemblyLine0(LDA, aa2, ah)))) =>
          if (ta1 == ta2 && aa1 == aa2 && tl.+(1).quickSimplify == th && al.+(1).quickSimplify == ah) {
            return List(
              AssemblyLine.accu16,
              AssemblyLine(LDA_W, ta1, tl),
              AssemblyLine(Opcode.widen(operation).get, aa1, al),
              AssemblyLine(STA_W, ta1, tl),
              AssemblyLine.accu8)
          }
        case _ =>
      }
    }
    val AllOnes = (1L << (8 * targetSize)) - 1
    (operation, env.eval(param)) match {
      case (EOR, Some(NumericConstant(0, _)))
           | (ORA, Some(NumericConstant(0, _)))
           | (AND, Some(NumericConstant(AllOnes, _))) =>
        MosExpressionCompiler.compile(ctx, lhs, None, NoBranching)
      case _ =>
        val buffer = mutable.ListBuffer[AssemblyLine]()
        buffer ++= calculateRhs
        for (i <- 0 until targetSize) {
          if (i < paramSize) {
            buffer ++= addendByteRead(i)
            if (paramType.isSigned && i == paramSize - 1 && extendAtLeastOneByte) {
              buffer += AssemblyLine.implied(TAX)
            }
          } else {
            if (paramType.isSigned) {
              val label = ctx.nextLabel("sx")
              buffer += AssemblyLine.implied(TXA)
              if (i == paramSize) {
                buffer += AssemblyLine.immediate(ORA, 0x7f)
                buffer += AssemblyLine.relative(BMI, label)
                buffer += AssemblyLine.immediate(LDA, 0)
                buffer += AssemblyLine.label(label)
                if (extendMultipleBytes) buffer += AssemblyLine.implied(TAX)
              }
            } else {
              buffer += AssemblyLine.immediate(LDA, 0)
            }
          }
          buffer ++= staTo(operation, targetBytes(i))
          buffer ++= targetBytes(i)
        }
        for (i <- 0 until calculateRhs.count(a => a.opcode == PHA) - calculateRhs.count(a => a.opcode == PLA)) {
          buffer += AssemblyLine.implied(PLA)
        }
        buffer.toList
    }
  }


  def getStorageForEachByte(ctx: CompilationContext, lhs: LhsExpression): List[List[AssemblyLine]] = {
    val env = ctx.env
    lhs match {
      case v: VariableExpression =>
        val variable = env.get[Variable](v.name)
        List.tabulate(variable.typ.size) { i => AssemblyLine.variable(ctx, STA, variable, i) }
      case IndexedExpression(variable, index) =>
        List(MosExpressionCompiler.compileByteStorage(ctx, MosRegister.A, lhs))
      case SeparateBytesExpression(h: LhsExpression, l: LhsExpression) =>
        if (simplicity(ctx.env, h) < 'J' || simplicity(ctx.env, l) < 'J') {
          // a[b]:c[d] is the most complex expression that doesn't cause the following warning
          ctx.log.warn("Too complex expression given to the `:` operator, generated code might be wrong", lhs.position)
        }
        List(
          getStorageForEachByte(ctx, l).head,
          MosExpressionCompiler.preserveRegisterIfNeeded(ctx, MosRegister.A, getStorageForEachByte(ctx, h).head))
      case _ =>
        ???
    }
  }
  private def getLoadForEachByte(ctx: CompilationContext, expr: Expression, size: Int): List[List[AssemblyLine]] = {
    val env = ctx.env
    env.eval(expr) match {
      case Some(c) =>
        List.tabulate(size) { i => List(AssemblyLine.immediate(CMP, c.subbyte(i))) }
      case None =>
        expr match {
          case v: VariableExpression =>
            val variable = env.get[Variable](v.name)
            List.tabulate(size) { i =>
              if (i < variable.typ.size) {
                AssemblyLine.variable(ctx, CMP, variable, i)
              } else if (variable.typ.isSigned) {
                val label = ctx.nextLabel("sx")
                AssemblyLine.variable(ctx, LDA, variable, variable.typ.size - 1) ++ List(
                  AssemblyLine.immediate(ORA, 0x7F),
                  AssemblyLine.relative(BMI, label),
                  AssemblyLine.immediate(CMP, 0),
                  AssemblyLine.label(label))
              } else List(AssemblyLine.immediate(CMP, 0))
            }
          case expr@IndexedExpression(variable, index) =>
            List.tabulate(size) { i =>
              if (i == 0) MosExpressionCompiler.compileByteStorage(ctx, MosRegister.A, expr)
              else List(AssemblyLine.immediate(CMP, 0))
            }
          case SeparateBytesExpression(h: LhsExpression, l: LhsExpression) =>
            if (simplicity(ctx.env, h) < 'J' || simplicity(ctx.env, l) < 'J') {
              // a[b]:c[d] is the most complex expression that doesn't cause the following warning
              ctx.log.warn("Too complex expression given to the `:` operator, generated code might be wrong", expr.position)
            }
            List.tabulate(size) { i =>
              if (i == 0) getStorageForEachByte(ctx, l).head
              else if (i == 1) MosExpressionCompiler.preserveRegisterIfNeeded(ctx, MosRegister.A, getStorageForEachByte(ctx, h).head)
              else List(AssemblyLine.immediate(CMP, 0))
            }
          case _ =>
            ???
        }
    }
  }

  private def removeTsx(codes: List[List[AssemblyLine]]): List[List[AssemblyLine]] = codes.map {
    case List(AssemblyLine0(TSX, _, _), AssemblyLine0(op, AbsoluteX, NumericConstant(nn, _))) if nn >= 0x100 && nn <= 0x1ff =>
      List(AssemblyLine(op, Stack, NumericConstant(nn & 0xff, 1)))
    case x => x
  }
}
