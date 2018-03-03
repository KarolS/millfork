package millfork.compiler

import millfork.{CompilationFlag, CompilationOptions}
import millfork.assembly._
import millfork.env._
import millfork.node._
import millfork.assembly.Opcode._
import millfork.assembly.AddrMode._
import millfork.error.ErrorReporting

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

  def ldTo(op: Opcode.Value, l: List[AssemblyLine]): List[AssemblyLine] = l.map(x => if (x.opcode == LDA || x.opcode == LDX || x.opcode == LDY) x.copy(opcode = op) else x)

  def simpleOperation(opcode: Opcode.Value, ctx: CompilationContext, source: Expression, indexChoice: IndexChoice.Value, preserveA: Boolean, commutative: Boolean, decimal: Boolean = false): List[AssemblyLine] = {
    val env = ctx.env
    val parts: (List[AssemblyLine], List[AssemblyLine]) = env.eval(source).fold {
      val b = env.get[Type]("byte")
      source match {
        case VariableExpression(name) =>
          val v = env.get[Variable](name)
          if (v.typ.size > 1) {
            ErrorReporting.error(s"Variable `$name` is too big for a built-in operation", source.position)
            return Nil
          }
          Nil -> AssemblyLine.variable(ctx, opcode, v)
        case IndexedExpression(arrayName, index) =>
          indexChoice match {
            case IndexChoice.RequireX | IndexChoice.PreferX =>
              val array = env.getArrayOrPointer(arrayName)
              val calculateIndex = ExpressionCompiler.compile(ctx, index, Some(b -> RegisterVariable(Register.X, b)), NoBranching)
              val baseAddress = array match {
                case c: ConstantThing => c.value
                case a: MfArray => a.toAddress
              }
              calculateIndex -> List(AssemblyLine.absoluteX(opcode, baseAddress))
            case IndexChoice.PreferY =>
              val array = env.getArrayOrPointer(arrayName)
              val calculateIndex = ExpressionCompiler.compile(ctx, index, Some(b -> RegisterVariable(Register.Y, b)), NoBranching)
              array match {
                case c: ConstantThing =>
                  calculateIndex -> List(AssemblyLine.absoluteY(opcode, c.value))
                case a: MfArray =>
                  calculateIndex -> List(AssemblyLine.absoluteY(opcode, a.toAddress))
                case v: VariableInMemory if v.typ.name == "pointer" =>
                  calculateIndex -> List(AssemblyLine.indexedY(opcode, v.toAddress))
              }
          }
        case FunctionCallExpression(name, List(param)) if env.maybeGet[Type](name).isDefined =>
          return simpleOperation(opcode, ctx, param, indexChoice, preserveA, commutative, decimal)
        case _: FunctionCallExpression | _:SumExpression if commutative =>
          // TODO: is it ok?
          if (ctx.options.flag(CompilationFlag.EmitEmulation65816Opcodes)) {
            return List(AssemblyLine.implied(PHA)) ++ ExpressionCompiler.compile(ctx.addStack(1), source, Some(b -> RegisterVariable(Register.A, b)), NoBranching) ++ wrapInSedCldIfNeeded(decimal, List(
              AssemblyLine.stackRelative(opcode, 1),
              AssemblyLine.implied(PHX)))
          } else {
            return List(AssemblyLine.implied(PHA)) ++ ExpressionCompiler.compile(ctx.addStack(1), source, Some(b -> RegisterVariable(Register.A, b)), NoBranching) ++ wrapInSedCldIfNeeded(decimal, List(
              AssemblyLine.implied(TSX),
              AssemblyLine.absoluteX(opcode, 0x101),
              AssemblyLine.implied(INX),
              AssemblyLine.implied(TXS))) // this TXS is fine, it won't appear in 65816 code
          }
        case _ =>
          ErrorReporting.error("Right-hand-side expression is too complex", source.position)
          return Nil
      }
    } {
      const =>
        if (const.requiredSize > 1) {
          ErrorReporting.error("Constant too big for a built-in operation", source.position)
        }
        Nil -> List(AssemblyLine.immediate(opcode, const))
    }
    val preparations = parts._1
    val finalRead = wrapInSedCldIfNeeded(decimal, parts._2)
    if (preserveA && AssemblyLine.treatment(preparations, State.A) != Treatment.Unchanged) {
      AssemblyLine.implied(PHA) :: (preparations ++ (AssemblyLine.implied(PLA) :: finalRead))
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
    if (decimal && !ctx.options.flag(CompilationFlag.DecimalMode)) {
      ErrorReporting.warn("Unsupported decimal operation", ctx.options, params.head._2.position)
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
    val firstParamCompiled = ExpressionCompiler.compile(ctx, h._2, Some(b -> RegisterVariable(Register.A, b)), NoBranching)
    val firstParamSignCompiled = if (h._1) {
      // TODO: check if decimal subtraction works correctly here
      List(AssemblyLine.immediate(EOR, 0xff), AssemblyLine.implied(SEC), AssemblyLine.immediate(ADC, 0))
    } else {
      Nil
    }

    val remainingParamsCompiled = normalizedParams.tail.flatMap { p =>
      if (p._1) {
        insertBeforeLast(AssemblyLine.implied(SEC), simpleOperation(SBC, ctx, p._2, IndexChoice.PreferY, preserveA = true, commutative = false, decimal = decimal))
      } else {
        insertBeforeLast(AssemblyLine.implied(CLC), simpleOperation(ADC, ctx, p._2, IndexChoice.PreferY, preserveA = true, commutative = true, decimal = decimal))
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
        case IndexedExpression(_, VariableExpression(_)) => 'J'
        case IndexedExpression(_, _) => 'I'
        case _ => 'A'
      }
    }
    constPart
  }

  def compileBitOps(opcode: Opcode.Value, ctx: CompilationContext, params: List[Expression]): List[AssemblyLine] = {
    val b = ctx.env.get[Type]("byte")

    val sortedParams = params.sortBy { expr =>
      ctx.env.eval(expr) match {
        case Some(NumericConstant(_, _)) => "Z"
        case Some(_) => "Y"
        case None => expr match {
          case VariableExpression(_) => "V"
          case IndexedExpression(_, LiteralExpression(_, _)) => "K"
          case IndexedExpression(_, VariableExpression(_)) => "J"
          case IndexedExpression(_, _) => "I"
          case _ => "A"
        }
      }
    }

    val h = sortedParams.head
    val firstParamCompiled = ExpressionCompiler.compile(ctx, h, Some(b -> RegisterVariable(Register.A, b)), NoBranching)

    val remainingParamsCompiled = sortedParams.tail.flatMap { p =>
      simpleOperation(opcode, ctx, p, IndexChoice.PreferY, preserveA = true, commutative = true)
    }

    firstParamCompiled ++ remainingParamsCompiled
  }

  def compileShiftOps(opcode: Opcode.Value, ctx: CompilationContext, l: Expression, r: Expression): List[AssemblyLine] = {
    val b = ctx.env.get[Type]("byte")
    val firstParamCompiled = ExpressionCompiler.compile(ctx, l, Some(b -> RegisterVariable(Register.A, b)), NoBranching)
    ctx.env.eval(r) match {
      case Some(NumericConstant(0, _)) =>
        Nil
      case Some(NumericConstant(v, _)) if v > 0 =>
        firstParamCompiled ++ List.fill(v.toInt)(AssemblyLine.implied(opcode))
      case _ =>
        ErrorReporting.error("Cannot shift by a non-constant amount")
        Nil
    }
  }

  def compileNonetOps(ctx: CompilationContext, lhs: LhsExpression, rhs: Expression): List[AssemblyLine] = {
    val env = ctx.env
    val b = env.get[Type]("byte")
    val (ldaHi, ldaLo) = lhs match {
      case v: VariableExpression =>
        val variable = env.get[Variable](v.name)
        AssemblyLine.variable(ctx, LDA, variable, 1) -> AssemblyLine.variable(ctx, LDA, variable, 0)
      case SeparateBytesExpression(h: VariableExpression, l: VariableExpression) =>
        AssemblyLine.variable(ctx, LDA, env.get[Variable](h.name), 0) -> AssemblyLine.variable(ctx, LDA, env.get[Variable](l.name), 0)
      case _ =>
        ???
    }
    env.eval(rhs) match {
      case Some(NumericConstant(0, _)) =>
        Nil
      case Some(NumericConstant(shift, _)) if shift > 0 =>
        if (ctx.options.flag(CompilationFlag.RorWarning))
          ErrorReporting.warn("ROR instruction generated", ctx.options, lhs.position)
        ldaHi ++ List(AssemblyLine.implied(ROR)) ++ ldaLo ++ List(AssemblyLine.implied(ROR)) ++ List.fill(shift.toInt - 1)(AssemblyLine.implied(LSR))
      case _ =>
        ErrorReporting.error("Non-constant shift amount", rhs.position) // TODO
        Nil
    }
  }

  def compileNonetLeftShift(ctx: CompilationContext, lhs: Expression, rhs: Expression): List[AssemblyLine] = {
    val label = MfCompiler.nextLabel("sh")
    compileShiftOps(ASL, ctx, lhs, rhs) ++ List(
      AssemblyLine.immediate(LDX, 0),
      AssemblyLine.relative(BCC, label),
      AssemblyLine.implied(INX),
      AssemblyLine.label(label)
    )
  }

  def compileInPlaceByteShiftOps(opcode: Opcode.Value, ctx: CompilationContext, lhs: LhsExpression, rhs: Expression): List[AssemblyLine] = {
    val env = ctx.env
    val b = env.get[Type]("byte")
    val firstParamCompiled = ExpressionCompiler.compile(ctx, lhs, Some(b -> RegisterVariable(Register.A, b)), NoBranching)
    env.eval(rhs) match {
      case Some(NumericConstant(0, _)) =>
        Nil
      case Some(NumericConstant(v, _)) if v > 0 =>
        val result = simpleOperation(opcode, ctx, lhs, IndexChoice.RequireX, preserveA = true, commutative = false)
        result ++ List.fill(v.toInt - 1)(result.last)
      case _ =>
        ErrorReporting.error("Non-constant shift amount", rhs.position) // TODO
        Nil
    }
  }

  def compileInPlaceWordOrLongShiftOps(ctx: CompilationContext, lhs: LhsExpression, rhs: Expression, aslRatherThanLsr: Boolean): List[AssemblyLine] = {
    val env = ctx.env
    val b = env.get[Type]("byte")
    val targetBytes = getStorageForEachByte(ctx, lhs)
    val lo = targetBytes.head
    val hi = targetBytes.last
    env.eval(rhs) match {
      case Some(NumericConstant(0, _)) =>
        Nil
      case Some(NumericConstant(shift, _)) if shift > 0 =>
        if (ctx.options.flags(CompilationFlag.EmitNative65816Opcodes)) {
          targetBytes match {
            case List(List(AssemblyLine(STA, a1, l, _)), List(AssemblyLine(STA, a2, h, _))) =>
              if (a1 == a2 && l.+(1).quickSimplify == h) {
                return List(AssemblyLine.accu16) ++ List.fill(shift.toInt)(AssemblyLine(if (aslRatherThanLsr) ASL_W else LSR_W, a1, l)) ++ List(AssemblyLine.accu8)
              }
          }
        }
        List.fill(shift.toInt)(if (aslRatherThanLsr) {
          staTo(ASL, lo) ++ targetBytes.tail.flatMap { b => staTo(ROL, b) }
        } else {
          if (ctx.options.flag(CompilationFlag.RorWarning))
            ErrorReporting.warn("ROR instruction generated", ctx.options, lhs.position)
          staTo(LSR, hi) ++ targetBytes.reverse.tail.flatMap { b => staTo(ROR, b) }
        }).flatten
      case _ =>
        ErrorReporting.error("Non-constant shift amount", rhs.position) // TODO
        Nil
    }
  }

  def compileByteComparison(ctx: CompilationContext, compType: ComparisonType.Value, lhs: Expression, rhs: Expression, branches: BranchSpec): List[AssemblyLine] = {
    val env = ctx.env
    val b = env.get[Type]("byte")
    if (simplicity(env, lhs) >= 'J' && simplicity(env, rhs) < 'J') {
      return compileByteComparison(ctx, ComparisonType.flip(compType), rhs, lhs, branches)
    }
    val firstParamCompiled = ExpressionCompiler.compile(ctx, lhs, Some(b -> RegisterVariable(Register.A, b)), NoBranching)
    val maybeConstant = env.eval(rhs)
    maybeConstant match {
      case Some(NumericConstant(0, _)) =>
        compType match {
          case ComparisonType.LessUnsigned =>
            ErrorReporting.warn("Unsigned < 0 is always false", ctx.options, lhs.position)
          case ComparisonType.LessOrEqualUnsigned =>
            if (ctx.options.flag(CompilationFlag.ExtraComparisonWarnings))
              ErrorReporting.warn("Unsigned <= 0 means the same as unsigned == 0", ctx.options, lhs.position)
          case ComparisonType.GreaterUnsigned =>
            if (ctx.options.flag(CompilationFlag.ExtraComparisonWarnings))
              ErrorReporting.warn("Unsigned > 0 means the same as unsigned != 0", ctx.options, lhs.position)
          case ComparisonType.GreaterOrEqualUnsigned =>
            ErrorReporting.warn("Unsigned >= 0 is always true", ctx.options, lhs.position)
          case _ =>
        }
      case Some(NumericConstant(1, _)) =>
        if (ctx.options.flag(CompilationFlag.ExtraComparisonWarnings)) {
          compType match {
            case ComparisonType.LessUnsigned =>
              ErrorReporting.warn("Unsigned < 1 means the same as unsigned == 0", ctx.options, lhs.position)
            case ComparisonType.GreaterOrEqualUnsigned =>
              ErrorReporting.warn("Unsigned >= 1 means the same as unsigned != 0", ctx.options, lhs.position)
            case _ =>
          }
        }
      case _ =>
    }
    val secondParamCompiled = maybeConstant match {
      case Some(x) =>
        compType match {
          case ComparisonType.Equal | ComparisonType.NotEqual | ComparisonType.LessSigned | ComparisonType.GreaterOrEqualSigned =>
            if (x.quickSimplify.isLowestByteAlwaysEqual(0) && OpcodeClasses.ChangesAAlways(firstParamCompiled.last.opcode)) Nil
            else List(AssemblyLine.immediate(CMP, x))
          case _ =>
            List(AssemblyLine.immediate(CMP, x))
        }
      case _ => compType match {
        case ComparisonType.Equal | ComparisonType.NotEqual | ComparisonType.LessSigned | ComparisonType.GreaterOrEqualSigned =>
          val secondParamCompiledUnoptimized = simpleOperation(CMP, ctx, rhs, IndexChoice.PreferY, preserveA = true, commutative = false)
          secondParamCompiledUnoptimized match {
            case List(AssemblyLine(CMP, Immediate, NumericConstant(0, _), true)) =>
              if (OpcodeClasses.ChangesAAlways(firstParamCompiled.last.opcode)) {
                Nil
              } else {
                secondParamCompiledUnoptimized
              }
            case _ => secondParamCompiledUnoptimized
          }
        case _ =>
          simpleOperation(CMP, ctx, rhs, IndexChoice.PreferY, preserveA = true, commutative = false)
      }
    }
    val (effectiveComparisonType, label) = branches match {
      case NoBranching => return Nil
      case BranchIfTrue(l) => compType -> l
      case BranchIfFalse(l) => ComparisonType.negate(compType) -> l
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
        val x = MfCompiler.nextLabel("co")
        List(
          AssemblyLine.relative(BEQ, x),
          AssemblyLine.relative(BCS, Label(label)),
          AssemblyLine.label(x))

      case ComparisonType.LessSigned =>
        List(AssemblyLine.relative(BMI, Label(label)))
      case ComparisonType.GreaterOrEqualSigned =>
        List(AssemblyLine.relative(BPL, Label(label)))
      case ComparisonType.LessOrEqualSigned =>
        List(AssemblyLine.relative(BMI, Label(label)), AssemblyLine.relative(BEQ, Label(label)))
      case ComparisonType.GreaterSigned =>
        val x = MfCompiler.nextLabel("co")
        List(
          AssemblyLine.relative(BEQ, x),
          AssemblyLine.relative(BPL, Label(label)),
          AssemblyLine.label(x))
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
    val (lh, ll, rh, rl) = (lhs, env.eval(lhs), rhs, env.eval(rhs)) match {
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
      case (_, Some(lc), rv: VariableInMemory, None) =>
        return compileWordComparison(ctx, ComparisonType.flip(compType), rhs, lhs, branches)
      case (v: VariableExpression, None, _, Some(rc)) =>
        val lva = env.get[VariableInMemory](v.name)
        (AssemblyLine.variable(ctx, STA, lva, 1),
          AssemblyLine.variable(ctx, STA, lva, 0),
          List(AssemblyLine.immediate(STA, rc.hiByte)),
          List(AssemblyLine.immediate(STA, rc.loByte)))
      case (lv: VariableExpression, None, rv: VariableExpression, None) =>
        val lva = env.get[VariableInMemory](lv.name)
        val rva = env.get[VariableInMemory](rv.name)
        (AssemblyLine.variable(ctx, STA, lva, 1),
          AssemblyLine.variable(ctx, STA, lva, 0),
          AssemblyLine.variable(ctx, STA, rva, 1),
          AssemblyLine.variable(ctx, STA, rva, 0))
    }
    effectiveComparisonType match {
      case ComparisonType.Equal =>
        val innerLabel = MfCompiler.nextLabel("cp")
        staTo(LDA, ll) ++
          staTo(CMP, rl) ++
          List(AssemblyLine.relative(BNE, innerLabel)) ++
          staTo(LDA, lh) ++
          staTo(CMP, rh) ++
          List(
            AssemblyLine.relative(BEQ, Label(x)),
            AssemblyLine.label(innerLabel))

      case ComparisonType.NotEqual =>
        staTo(LDA, ll) ++
          staTo(CMP, rl) ++
          List(AssemblyLine.relative(BNE, Label(x))) ++
          staTo(LDA, lh) ++
          staTo(CMP, rh) ++
          List(AssemblyLine.relative(BNE, Label(x)))

      case ComparisonType.LessUnsigned =>
        val innerLabel = MfCompiler.nextLabel("cp")
        staTo(LDA, lh) ++
          staTo(CMP, rh) ++
          List(
            AssemblyLine.relative(BCC, Label(x)),
            AssemblyLine.relative(BNE, innerLabel)) ++
          staTo(LDA, ll) ++
          staTo(CMP, rl) ++
          List(
            AssemblyLine.relative(BCC, Label(x)),
            AssemblyLine.label(innerLabel))

      case ComparisonType.LessOrEqualUnsigned =>
        val innerLabel = MfCompiler.nextLabel("cp")
        staTo(LDA, rh) ++
          staTo(CMP, lh) ++
          List(AssemblyLine.relative(BCC, innerLabel),
            AssemblyLine.relative(BNE, x)) ++
          staTo(LDA, rl) ++
          staTo(CMP, ll) ++
          List(AssemblyLine.relative(BCS, x),
            AssemblyLine.label(innerLabel))

      case ComparisonType.GreaterUnsigned =>
        val innerLabel = MfCompiler.nextLabel("cp")
        staTo(LDA, rh) ++
          staTo(CMP, lh) ++
          List(AssemblyLine.relative(BCC, Label(x)),
            AssemblyLine.relative(BNE, innerLabel)) ++
          staTo(LDA, rl) ++
          staTo(CMP, ll) ++
          List(AssemblyLine.relative(BCC, Label(x)),
            AssemblyLine.label(innerLabel))

      case ComparisonType.GreaterOrEqualUnsigned =>
        val innerLabel = MfCompiler.nextLabel("cp")
        staTo(LDA, lh) ++
          staTo(CMP, rh) ++
          List(AssemblyLine.relative(BCC, innerLabel),
            AssemblyLine.relative(BNE, x)) ++
          staTo(LDA, ll) ++
          staTo(CMP, rl) ++
          List(AssemblyLine.relative(BCS, x),
            AssemblyLine.label(innerLabel))

      case _ => ???
      // TODO: signed word comparisons
    }
  }

  def compileInPlaceByteMultiplication(ctx: CompilationContext, v: LhsExpression, addend: Expression): List[AssemblyLine] = {
    val b = ctx.env.get[Type]("byte")
    ctx.env.eval(addend) match {
      case Some(NumericConstant(0, _)) =>
        AssemblyLine.immediate(LDA, 0) :: ExpressionCompiler.compileByteStorage(ctx, Register.A, v)
      case Some(NumericConstant(1, _)) =>
        Nil
      case Some(NumericConstant(x, _)) =>
        compileByteMultiplication(ctx, v, x.toInt) ++ ExpressionCompiler.compileByteStorage(ctx, Register.A, v)
      case _ =>
        ErrorReporting.error("Multiplying by not a constant not supported", v.position)
        Nil
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

  def compileByteMultiplication(ctx: CompilationContext, params: List[Expression]): List[AssemblyLine] = {
    val (constants, variables) = params.map(p => p -> ctx.env.eval(p)).partition(_._2.exists(_.isInstanceOf[NumericConstant]))
    val constant = constants.map(_._2.get.asInstanceOf[NumericConstant].value).foldLeft(1L)(_ * _).toInt
    variables.length match {
      case 0 => List(AssemblyLine.immediate(LDA, constant & 0xff))
      case 1 =>compileByteMultiplication(ctx, variables.head._1, constant)
      case 2 =>
        ErrorReporting.error("Multiplying by not a constant not supported", params.head.position)
        Nil
    }
  }

  def compileInPlaceByteAddition(ctx: CompilationContext, v: LhsExpression, addend: Expression, subtract: Boolean, decimal: Boolean): List[AssemblyLine] = {
    if (decimal && !ctx.options.flag(CompilationFlag.DecimalMode)) {
      ErrorReporting.warn("Unsupported decimal operation", ctx.options, v.position)
    }
    val env = ctx.env
    val b = env.get[Type]("byte")
    val lhsIsDirectlyIncrementable = v match {
      case _:VariableExpression => true
      case IndexedExpression(pointy, _) => env.getPointy(pointy) match {
        case _:ConstantPointy => true
        case _:VariablePointy => false
      }
      case _ => false
    }
    env.eval(addend) match {
      case Some(NumericConstant(0, _)) => Nil
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
        if (!subtract && simplicity(env, v) > simplicity(env, addend)) {
          val loadRhs = ExpressionCompiler.compile(ctx, addend, Some(b -> RegisterVariable(Register.A, b)), NoBranching)
          val modifyAcc = insertBeforeLast(AssemblyLine.implied(CLC), simpleOperation(ADC, ctx, v, IndexChoice.PreferY, preserveA = true, commutative = true, decimal = decimal))
          val storeLhs = ExpressionCompiler.compileByteStorage(ctx, Register.A, v)
          loadRhs ++ modifyAcc ++ storeLhs
        } else {
          val loadLhs = ExpressionCompiler.compile(ctx, v, Some(b -> RegisterVariable(Register.A, b)), NoBranching)
          val modifyLhs = if (subtract) {
            insertBeforeLast(AssemblyLine.implied(SEC), simpleOperation(SBC, ctx, addend, IndexChoice.PreferY, preserveA = true, commutative = false, decimal = decimal))
          } else {
            insertBeforeLast(AssemblyLine.implied(CLC), simpleOperation(ADC, ctx, addend, IndexChoice.PreferY, preserveA = true, commutative = true, decimal = decimal))
          }
          val storeLhs = ExpressionCompiler.compileByteStorage(ctx, Register.A, v)
          loadLhs ++ modifyLhs ++ storeLhs
        }
    }
  }

  def compileInPlaceWordOrLongAddition(ctx: CompilationContext, lhs: LhsExpression, addend: Expression, subtract: Boolean, decimal: Boolean): List[AssemblyLine] = {
    if (decimal && !ctx.options.flag(CompilationFlag.DecimalMode)) {
      ErrorReporting.warn("Unsupported decimal operation", ctx.options, lhs.position)
    }
    val env = ctx.env
    val b = env.get[Type]("byte")
    val w = env.get[Type]("word")
    val targetBytes: List[List[AssemblyLine]] = getStorageForEachByte(ctx, lhs)
    val lhsIsStack = targetBytes.head.head.opcode == TSX
    val targetSize = targetBytes.size
    val addendType = ExpressionCompiler.getExpressionType(ctx, addend)
    var addendSize = addendType.size

    def isRhsComplex(xs: List[AssemblyLine]): Boolean = xs match {
      case AssemblyLine(LDA, _, _, _) :: Nil => false
      case AssemblyLine(LDA, _, _, _) :: AssemblyLine(LDX, _, _, _) :: Nil => false
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
        val label = MfCompiler.nextLabel("de")
        staTo(LDA, x) ++
          List(AssemblyLine.relative(BNE, label)) ++
          doDec(xs) ++
          List(AssemblyLine.label(label)) ++
          staTo(DEC, x)
    }

    val (calculateRhs, addendByteRead0): (List[AssemblyLine], List[List[AssemblyLine]]) = env.eval(addend) match {
      case Some(NumericConstant(0, _)) =>
        return Nil
      case Some(NumericConstant(1, _)) if canUseIncDec && !subtract =>
        if (ctx.options.flags(CompilationFlag.Emit65CE02Opcodes)) {
          targetBytes match {
            case List(List(AssemblyLine(STA, ZeroPage, l, _)), List(AssemblyLine(STA, ZeroPage, h, _))) =>
              if (l.+(1).quickSimplify == h) {
                return List(AssemblyLine.zeropage(INC_W, l))
              }
          }
        }
        if (ctx.options.flags(CompilationFlag.EmitNative65816Opcodes)) {
          targetBytes match {
            case List(List(AssemblyLine(STA, a1@(ZeroPage | Absolute | ZeroPageX | AbsoluteX), l, _)), List(AssemblyLine(STA, a2, h, _))) =>
              if (a1 == a2 && l.+(1).quickSimplify == h) {
                return List(AssemblyLine.accu16, AssemblyLine(INC_W, a1, l), AssemblyLine.accu8)
              }
          }
        }
        val label = MfCompiler.nextLabel("in")
        return staTo(INC, targetBytes.head) ++ targetBytes.tail.flatMap(l => AssemblyLine.relative(BNE, label)::staTo(INC, l)) :+ AssemblyLine.label(label)
      case Some(NumericConstant(-1, _)) if canUseIncDec && subtract =>
        if (ctx.options.flags(CompilationFlag.Emit65CE02Opcodes)) {
          targetBytes match {
            case List(List(AssemblyLine(STA, ZeroPage, l, _)), List(AssemblyLine(STA, ZeroPage, h, _))) =>
              if (l.+(1).quickSimplify == h) {
                return List(AssemblyLine.zeropage(INC_W, l))
              }
          }
        }
        if (ctx.options.flags(CompilationFlag.EmitNative65816Opcodes)) {
          targetBytes match {
            case List(List(AssemblyLine(STA, a1@(ZeroPage | Absolute | ZeroPageX | AbsoluteX), l, _)), List(AssemblyLine(STA, a2, h, _))) =>
              if (a1 == a2 && l.+(1).quickSimplify == h) {
                return List(AssemblyLine.accu16, AssemblyLine(INC_W, a1, l), AssemblyLine.accu8)
              }
          }
        }
        val label = MfCompiler.nextLabel("in")
        return staTo(INC, targetBytes.head) ++ targetBytes.tail.flatMap(l => AssemblyLine.relative(BNE, label)::staTo(INC, l)) :+ AssemblyLine.label(label)
      case Some(NumericConstant(1, _)) if canUseIncDec && subtract =>
        if (ctx.options.flags(CompilationFlag.Emit65CE02Opcodes)) {
          targetBytes match {
            case List(List(AssemblyLine(STA, ZeroPage, l, _)), List(AssemblyLine(STA, ZeroPage, h, _))) =>
              if (l.+(1).quickSimplify == h) {
                return List(AssemblyLine.zeropage(DEC_W, l))
              }
          }
        }
        if (ctx.options.flags(CompilationFlag.EmitNative65816Opcodes)) {
          targetBytes match {
            case List(List(AssemblyLine(STA, a1@(ZeroPage | Absolute | ZeroPageX | AbsoluteX), l, _)), List(AssemblyLine(STA, a2, h, _))) =>
              if (a1 == a2 && l.+(1).quickSimplify == h) {
                return List(AssemblyLine.accu16, AssemblyLine(DEC_W, a1, l), AssemblyLine.accu8)
              }
          }
        }
        val label = MfCompiler.nextLabel("de")
        return doDec(targetBytes)
      case Some(NumericConstant(-1, _)) if canUseIncDec && !subtract =>
        if (ctx.options.flags(CompilationFlag.Emit65CE02Opcodes)) {
          targetBytes match {
            case List(List(AssemblyLine(STA, ZeroPage, l, _)), List(AssemblyLine(STA, ZeroPage, h, _))) =>
              if (l.+(1).quickSimplify == h) {
                return List(AssemblyLine.zeropage(DEC_W, l))
              }
          }
        }
        if (ctx.options.flags(CompilationFlag.EmitNative65816Opcodes)) {
          targetBytes match {
            case List(List(AssemblyLine(STA, a1@(ZeroPage | Absolute | ZeroPageX | AbsoluteX), l, _)), List(AssemblyLine(STA, a2, h, _))) =>
              if (a1 == a2 && l.+(1).quickSimplify == h) {
                return List(AssemblyLine.accu16, AssemblyLine(DEC_W, a1, l), AssemblyLine.accu8)
              }
          }
        }
        val label = MfCompiler.nextLabel("de")
        return doDec(targetBytes)
      case Some(constant) =>
        addendSize = targetSize
        Nil -> List.tabulate(targetSize)(i => List(AssemblyLine.immediate(LDA, constant.subbyte(i))))
      case None =>
        addendSize match {
          case 1 =>
            val base = ExpressionCompiler.compile(ctx, addend, Some(b -> RegisterVariable(Register.A, b)), NoBranching)
            if (subtract) {
              if (isRhsComplex(base)) {
                if (isRhsStack(base)) {
                  ErrorReporting.warn("Subtracting a stack-based value", ctx.options)
                  ???
                }
                (base ++ List(AssemblyLine.implied(PHA))) -> List(List(AssemblyLine.implied(TSX), AssemblyLine.absoluteX(LDA, 0x101)))
              } else {
                Nil -> base.map(_ :: Nil)
              }
            } else {
              base -> List(Nil)
            }
          case 2 =>
            val base = ExpressionCompiler.compile(ctx, addend, Some(w -> RegisterVariable(Register.AX, w)), NoBranching)
            if (isRhsStack(base)) {
              val fixedBase = ExpressionCompiler.compile(ctx, addend, Some(w -> RegisterVariable(Register.AY, w)), NoBranching)
              if (subtract) {
                ErrorReporting.warn("Subtracting a stack-based value", ctx.options)
                if (isRhsComplex(base)) {
                  ???
                } else {
                  Nil -> fixedBase
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
                  Nil -> base.map(_ :: Nil)
                }
              } else {
                if (lhsIsStack) {
                  val fixedBase = ExpressionCompiler.compile(ctx, addend, Some(w -> RegisterVariable(Register.AY, w)), NoBranching)
                  fixedBase -> List(Nil, List(AssemblyLine.implied(TYA)))
                } else {
                  base -> List(Nil, List(AssemblyLine.implied(TXA)))
                }
              }
            }
          case _ => Nil -> (addend match {
            case vv: VariableExpression =>
              val source = env.get[Variable](vv.name)
              List.tabulate(addendSize)(i => AssemblyLine.variable(ctx, LDA, source, i))
          })
        }
    }
    val addendByteRead = addendByteRead0 ++ List.fill((targetSize - addendByteRead0.size) max 0)(List(AssemblyLine.immediate(LDA, 0)))

    if (ctx.options.flags(CompilationFlag.EmitNative65816Opcodes)) {
      (removeTsx(targetBytes), removeTsx(addendByteRead)) match {
        case (List(List(AssemblyLine(STA, ta1, tl, _)), List(AssemblyLine(STA, ta2, th, _))), List(List(AssemblyLine(LDA, Immediate, al, _)), List(AssemblyLine(LDA, Immediate, ah, _)))) =>
          if (ta1 == ta2 && tl.+(1).quickSimplify == th) {
            return wrapInSedCldIfNeeded(decimal, List(
              AssemblyLine.implied(if(subtract) SEC else CLC),
              AssemblyLine.accu16,
              AssemblyLine(LDA_W, ta1, tl),
              AssemblyLine(if(subtract) SBC_W else ADC_W, WordImmediate, ah.asl(8).+(al).quickSimplify),
              AssemblyLine(STA_W, ta1, tl),
              AssemblyLine.accu8))
          }
        case (List(List(AssemblyLine(STA, ta1, tl, _)), List(AssemblyLine(STA, ta2, th, _))), List(List(AssemblyLine(LDA, aa1, al, _)), List(AssemblyLine(LDA, aa2, ah, _)))) =>
          if (ta1 == ta2 && aa1 == aa2 && tl.+(1).quickSimplify == th && al.+(1).quickSimplify == ah) {
            return wrapInSedCldIfNeeded(decimal, List(
              AssemblyLine.accu16,
              AssemblyLine.implied(if(subtract) SEC else CLC),
              AssemblyLine(LDA_W, ta1, tl),
              AssemblyLine(if(subtract) SBC_W else ADC_W, aa1, al),
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
      if (subtract) {
        if (addendSize < targetSize && addendType.isSigned) {
          // TODO: sign extension
          ???
        }
        buffer ++= staTo(LDA, targetBytes(i))
        buffer ++= ldTo(SBC, addendByteRead(i))
        buffer ++= targetBytes(i)
      } else {
        if (i >= addendSize) {
          if (addendType.isSigned) {
            val label = MfCompiler.nextLabel("sx")
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
        Nil
      case _ =>
        if (simplicity(env, v) > simplicity(env, param)) {
          val loadRhs = ExpressionCompiler.compile(ctx, param, Some(b -> RegisterVariable(Register.A, b)), NoBranching)
          val modifyAcc = simpleOperation(operation, ctx, v, IndexChoice.PreferY, preserveA = true, commutative = true)
          val storeLhs = ExpressionCompiler.compileByteStorage(ctx, Register.A, v)
          loadRhs ++ modifyAcc ++ storeLhs
        } else {
          val loadLhs = ExpressionCompiler.compile(ctx, v, Some(b -> RegisterVariable(Register.A, b)), NoBranching)
          val modifyLhs = simpleOperation(operation, ctx, param, IndexChoice.PreferY, preserveA = true, commutative = true)
          val storeLhs = ExpressionCompiler.compileByteStorage(ctx, Register.A, v)
          loadLhs ++ modifyLhs ++ storeLhs
        }
    }
  }


  def compileInPlaceWordOrLongBitOp(ctx: CompilationContext, lhs: LhsExpression, param: Expression, operation: Opcode.Value): List[AssemblyLine] = {
    val env = ctx.env
    val b = env.get[Type]("byte")
    val w = env.get[Type]("word")
    val targetBytes: List[List[AssemblyLine]] = getStorageForEachByte(ctx, lhs)
    val lo = targetBytes.head
    val targetSize = targetBytes.size
    val paramType = ExpressionCompiler.getExpressionType(ctx, param)
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
            val base = ExpressionCompiler.compile(ctx, param, Some(b -> RegisterVariable(Register.A, b)), NoBranching)
            base -> List(Nil)
          case 2 =>
            val base = ExpressionCompiler.compile(ctx, param, Some(w -> RegisterVariable(Register.AX, w)), NoBranching)
            base -> List(Nil, List(AssemblyLine.implied(TXA)))
          case _ => Nil -> (param match {
            case vv: VariableExpression =>
              val source = env.get[Variable](vv.name)
              List.tabulate(paramSize)(i => AssemblyLine.variable(ctx, LDA, source, i))
          })
        }
    }
    if (ctx.options.flags(CompilationFlag.EmitNative65816Opcodes)) {
      (removeTsx(targetBytes), removeTsx(addendByteRead)) match {
        case (List(List(AssemblyLine(STA, ta1, tl, _)), List(AssemblyLine(STA, ta2, th, _))), List(List(AssemblyLine(LDA, Immediate, al, _)), List(AssemblyLine(LDA, Immediate, ah, _)))) =>
          if (ta1 == ta2 && tl.+(1).quickSimplify == th) {
            return List(
              AssemblyLine.accu16,
              AssemblyLine(LDA_W, ta1, tl),
              AssemblyLine(Opcode.widen(operation).get, WordImmediate, ah.asl(8).+(al).quickSimplify),
              AssemblyLine(STA_W, ta1, tl),
              AssemblyLine.accu8)
          }
        case (List(List(AssemblyLine(STA, ta1, tl, _)), List(AssemblyLine(STA, ta2, th, _))), List(List(AssemblyLine(LDA, aa1, al, _)), List(AssemblyLine(LDA, aa2, ah, _)))) =>
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
        Nil
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
              val label = MfCompiler.nextLabel("sx")
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


  private def getStorageForEachByte(ctx: CompilationContext, lhs: LhsExpression): List[List[AssemblyLine]] = {
    val env = ctx.env
    lhs match {
      case v: VariableExpression =>
        val variable = env.get[Variable](v.name)
        List.tabulate(variable.typ.size) { i => AssemblyLine.variable(ctx, STA, variable, i) }
      case IndexedExpression(variable, index) =>
        List(ExpressionCompiler.compileByteStorage(ctx, Register.A, lhs))
      case SeparateBytesExpression(h: LhsExpression, l: LhsExpression) =>
        if (simplicity(ctx.env, h) < 'J' || simplicity(ctx.env, l) < 'J') {
          // a[b]:c[d] is the most complex expression that doesn't cause the following warning
          ErrorReporting.warn("Too complex expression given to the `:` operator, generated code might be wrong", ctx.options, lhs.position)
        }
        List(
          getStorageForEachByte(ctx, l).head,
          ExpressionCompiler.preserveRegisterIfNeeded(ctx, Register.A, getStorageForEachByte(ctx, h).head))
      case _ =>
        ???
    }
  }

  private def removeTsx(codes: List[List[AssemblyLine]]): List[List[AssemblyLine]] = codes.map {
    case List(AssemblyLine(TSX, _, _, _), AssemblyLine(op, AbsoluteX, NumericConstant(nn, _), _)) if nn >= 0x100 && nn <= 0x1ff =>
      List(AssemblyLine(op, Stack, NumericConstant(nn & 0xff, 1)))
    case x => x
  }
}
