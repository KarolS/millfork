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
import scala.reflect.macros.blackbox


object ComparisonType extends Enumeration {
  val Equal, NotEqual,
  LessUnsigned, LessSigned,
  GreaterUnsigned, GreaterSigned,
  LessOrEqualUnsigned, LessOrEqualSigned,
  GreaterOrEqualUnsigned, GreaterOrEqualSigned = Value

  def flip(x: ComparisonType.Value): ComparisonType.Value = x match {
    case LessUnsigned => GreaterUnsigned
    case GreaterUnsigned => LessUnsigned
    case LessOrEqualUnsigned => GreaterOrEqualUnsigned
    case GreaterOrEqualUnsigned => LessOrEqualUnsigned
    case LessSigned => GreaterSigned
    case GreaterSigned => LessSigned
    case LessOrEqualSigned => GreaterOrEqualSigned
    case GreaterOrEqualSigned => LessOrEqualSigned
    case _ => x
  }

  def negate(x: ComparisonType.Value): ComparisonType.Value = x match {
    case LessUnsigned => GreaterOrEqualUnsigned
    case GreaterUnsigned => LessOrEqualUnsigned
    case LessOrEqualUnsigned => GreaterUnsigned
    case GreaterOrEqualUnsigned => LessUnsigned
    case LessSigned => GreaterOrEqualSigned
    case GreaterSigned => LessOrEqualSigned
    case LessOrEqualSigned => GreaterSigned
    case GreaterOrEqualSigned => LessSigned
    case Equal => NotEqual
    case NotEqual => Equal
  }
}

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
              val calculateIndex = MlCompiler.compile(ctx, index, Some(b -> RegisterVariable(Register.X, b)), NoBranching)
              val baseAddress = array match {
                case c: ConstantThing => c.value
                case a: MlArray => a.toAddress
              }
              calculateIndex -> List(AssemblyLine.absoluteX(opcode, baseAddress))
            case IndexChoice.PreferY =>
              val array = env.getArrayOrPointer(arrayName)
              val calculateIndex = MlCompiler.compile(ctx, index, Some(b -> RegisterVariable(Register.Y, b)), NoBranching)
              val baseAddress = array match {
                case c: ConstantThing => c.value
                case a: MlArray => a.toAddress
              }
              calculateIndex -> List(AssemblyLine.absoluteY(opcode, baseAddress))
          }
        case _: FunctionCallExpression | _:SumExpression if commutative =>
          // TODO: is it ok?
          return List(AssemblyLine.implied(PHA)) ++ MlCompiler.compile(ctx.addStack(1), source, Some(b -> RegisterVariable(Register.A, b)), NoBranching) ++ wrapInSedCldIfNeeded(decimal, List(
            AssemblyLine.implied(TSX),
            AssemblyLine.absoluteX(opcode, 0x101),
            AssemblyLine.implied(INX),
            AssemblyLine.implied(TXS)))
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
      val constPart = env.eval(expr) match {
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
      val subtractPart = if (subtract) "X" else "P"
      constPart + subtractPart
    }
    // TODO: merge constants
    val normalizedParams = sortedParams

    val h = normalizedParams.head
    val firstParamCompiled = MlCompiler.compile(ctx, h._2, Some(b -> RegisterVariable(Register.A, b)), NoBranching)
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
    val firstParamCompiled = MlCompiler.compile(ctx, h, Some(b -> RegisterVariable(Register.A, b)), NoBranching)

    val remainingParamsCompiled = sortedParams.tail.flatMap { p =>
      simpleOperation(opcode, ctx, p, IndexChoice.PreferY, preserveA = true, commutative = true)
    }

    firstParamCompiled ++ remainingParamsCompiled
  }

  def compileShiftOps(opcode: Opcode.Value, ctx: CompilationContext, l: Expression, r: Expression): List[AssemblyLine] = {
    val b = ctx.env.get[Type]("byte")
    val firstParamCompiled = MlCompiler.compile(ctx, l, Some(b -> RegisterVariable(Register.A, b)), NoBranching)
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

  def compileInPlaceByteShiftOps(opcode: Opcode.Value, ctx: CompilationContext, lhs: LhsExpression, rhs: Expression): List[AssemblyLine] = {
    val env = ctx.env
    val b = env.get[Type]("byte")
    val firstParamCompiled = MlCompiler.compile(ctx, lhs, Some(b -> RegisterVariable(Register.A, b)), NoBranching)
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
    val targetBytes = lhs match {
      case v: VariableExpression =>
        val variable = env.get[Variable](v.name)
        List.tabulate(variable.typ.size) { i => AssemblyLine.variable(ctx, STA, variable, i) }
      case SeparateBytesExpression(h: VariableExpression, l: VariableExpression) =>
        List(
          AssemblyLine.variable(ctx, STA, env.get[Variable](l.name)),
          AssemblyLine.variable(ctx, STA, env.get[Variable](h.name)))
    }
    val lo = targetBytes.head
    val hi = targetBytes.last
    env.eval(rhs) match {
      case Some(NumericConstant(0, _)) =>
        Nil
      case Some(NumericConstant(shift, _)) if shift > 0 =>
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
    val firstParamCompiled = MlCompiler.compile(ctx, lhs, Some(b -> RegisterVariable(Register.A, b)), NoBranching)
    env.eval(rhs) match {
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
    val secondParamCompiledUnoptimized = simpleOperation(CMP, ctx, rhs, IndexChoice.PreferY, preserveA = true, commutative = false)
    val secondParamCompiled = compType match {
      case ComparisonType.Equal | ComparisonType.NotEqual | ComparisonType.LessSigned | ComparisonType.GreaterOrEqualSigned =>
        secondParamCompiledUnoptimized match {
          case List(AssemblyLine(CMP, Immediate, NumericConstant(0, _), true)) =>
            if (OpcodeClasses.ChangesAAlways(firstParamCompiled.last.opcode)) {
              Nil
            } else {
              secondParamCompiledUnoptimized
            }
          case _ => secondParamCompiledUnoptimized
        }
      case _ => secondParamCompiledUnoptimized
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
        val x = MlCompiler.nextLabel("co")
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
        val x = MlCompiler.nextLabel("co")
        List(
          AssemblyLine.relative(BEQ, x),
          AssemblyLine.relative(BPL, Label(label)),
          AssemblyLine.label(x))
    }
    firstParamCompiled ++ secondParamCompiled ++ branchingCompiled

  }

  def compileWordComparison(ctx: CompilationContext, compType: ComparisonType.Value, lhs: Expression, rhs: Expression, branches: BranchSpec): List[AssemblyLine] = {
    val env = ctx.env
    // TODO: comparing stack variables
    val b = env.get[Type]("byte")
    val w = env.get[Type]("word")

    val (effectiveComparisonType, x) = branches match {
      case NoBranching => return Nil
      case BranchIfTrue(label) => compType -> label
      case BranchIfFalse(label) => ComparisonType.negate(compType) -> label
    }
    val (lh, ll, rh, rl, ram) = (lhs, env.eval(lhs), rhs, env.eval(rhs)) match {
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
        // TODO: stack variables
        (env.get[VariableInMemory](v.name + ".hi").toAddress,
          env.get[VariableInMemory](v.name + ".lo").toAddress,
          rc.hiByte,
          rc.loByte,
          Immediate)
      case (lv: VariableExpression, None, rv: VariableExpression, None) =>
        // TODO: stack variables
        (env.get[VariableInMemory](lv.name + ".hi").toAddress,
          env.get[VariableInMemory](lv.name + ".lo").toAddress,
          env.get[VariableInMemory](rv.name + ".hi").toAddress,
          env.get[VariableInMemory](rv.name + ".lo").toAddress, Absolute)
    }
    effectiveComparisonType match {
      case ComparisonType.Equal =>
        val innerLabel = MlCompiler.nextLabel("cp")
        List(AssemblyLine.absolute(LDA, ll),
          AssemblyLine(CMP, ram, rl),
          AssemblyLine.relative(BNE, innerLabel),
          AssemblyLine.absolute(LDA, lh),
          AssemblyLine(CMP, ram, rh),
          AssemblyLine.relative(BEQ, Label(x)),
          AssemblyLine.label(innerLabel))

      case ComparisonType.NotEqual =>
        List(AssemblyLine.absolute(LDA, ll),
          AssemblyLine(CMP, ram, rl),
          AssemblyLine.relative(BNE, Label(x)),
          AssemblyLine.absolute(LDA, lh),
          AssemblyLine(CMP, ram, rh),
          AssemblyLine.relative(BNE, Label(x)))

      case ComparisonType.LessUnsigned =>
        val innerLabel = MlCompiler.nextLabel("cp")
        List(AssemblyLine.absolute(LDA, lh),
          AssemblyLine(CMP, ram, rh),
          AssemblyLine.relative(BCC, Label(x)),
          AssemblyLine.relative(BNE, innerLabel),
          AssemblyLine.absolute(LDA, ll),
          AssemblyLine(CMP, ram, rl),
          AssemblyLine.relative(BCC, Label(x)),
          AssemblyLine.label(innerLabel))

      case ComparisonType.LessOrEqualUnsigned =>
        val innerLabel = MlCompiler.nextLabel("cp")
        List(AssemblyLine(LDA, ram, rh),
          AssemblyLine.absolute(CMP, lh),
          AssemblyLine.relative(BCC, innerLabel),
          AssemblyLine.relative(BNE, x),
          AssemblyLine(LDA, ram, rl),
          AssemblyLine.absolute(CMP, ll),
          AssemblyLine.relative(BCS, x),
          AssemblyLine.label(innerLabel))

      case ComparisonType.GreaterUnsigned =>
        val innerLabel = MlCompiler.nextLabel("cp")
        List(AssemblyLine(LDA, ram, rh),
          AssemblyLine.absolute(CMP, lh),
          AssemblyLine.relative(BCC, Label(x)),
          AssemblyLine.relative(BNE, innerLabel),
          AssemblyLine(LDA, ram, rl),
          AssemblyLine.absolute(CMP, ll),
          AssemblyLine.relative(BCC, Label(x)),
          AssemblyLine.label(innerLabel))

      case ComparisonType.GreaterOrEqualUnsigned =>
        val innerLabel = MlCompiler.nextLabel("cp")
        List(AssemblyLine.absolute(LDA, lh),
          AssemblyLine(CMP, ram, rh),
          AssemblyLine.relative(BCC, innerLabel),
          AssemblyLine.relative(BNE, x),
          AssemblyLine.absolute(LDA, ll),
          AssemblyLine(CMP, ram, rl),
          AssemblyLine.relative(BCS, x),
          AssemblyLine.label(innerLabel))

      case _ => ???
      // TODO: signed word comparisons
    }
  }

  def compileInPlaceByteMultiplication(ctx: CompilationContext, v: LhsExpression, addend: Expression): List[AssemblyLine] = {
    val b = ctx.env.get[Type]("byte")
    ctx.env.eval(addend) match {
      case Some(NumericConstant(0, _)) =>
        AssemblyLine.immediate(LDA, 0) :: MlCompiler.compileByteStorage(ctx, Register.A, v)
      case Some(NumericConstant(1, _)) =>
        Nil
      case Some(NumericConstant(x, _)) =>
        compileByteMultiplication(ctx, v, x.toInt) ++ MlCompiler.compileByteStorage(ctx, Register.A, v)
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
    env.eval(addend) match {
      case Some(NumericConstant(0, _)) => Nil
      case Some(NumericConstant(1, _)) if !decimal => if (subtract) {
        simpleOperation(DEC, ctx, v, IndexChoice.RequireX, preserveA = false, commutative = true)
      } else {
        simpleOperation(INC, ctx, v, IndexChoice.RequireX, preserveA = false, commutative = true)
      }
      // TODO: compile +=2 to two INCs
      case Some(NumericConstant(-1, _)) if !decimal => if (subtract) {
        simpleOperation(INC, ctx, v, IndexChoice.RequireX, preserveA = false, commutative = true)
      } else {
        simpleOperation(DEC, ctx, v, IndexChoice.RequireX, preserveA = false, commutative = true)
      }
      case _ =>
        val loadLhs = MlCompiler.compile(ctx, v, Some(b -> RegisterVariable(Register.A, b)), NoBranching)
        val modifyLhs = if (subtract) {
          insertBeforeLast(AssemblyLine.implied(SEC), simpleOperation(SBC, ctx, addend, IndexChoice.PreferY, preserveA = true, commutative = false, decimal = decimal))
        } else {
          insertBeforeLast(AssemblyLine.implied(CLC), simpleOperation(ADC, ctx, addend, IndexChoice.PreferY, preserveA = true, commutative = true, decimal = decimal))
        }
        val storeLhs = MlCompiler.compileByteStorage(ctx, Register.A, v)
        loadLhs ++ modifyLhs ++ storeLhs
    }
  }

  def compileInPlaceWordOrLongAddition(ctx: CompilationContext, lhs: LhsExpression, addend: Expression, subtract: Boolean, decimal: Boolean): List[AssemblyLine] = {
    if (decimal && !ctx.options.flag(CompilationFlag.DecimalMode)) {
      ErrorReporting.warn("Unsupported decimal operation", ctx.options, lhs.position)
    }
    val env = ctx.env
    val b = env.get[Type]("byte")
    val w = env.get[Type]("word")
    val targetBytes: List[List[AssemblyLine]] = lhs match {
      case v: VariableExpression =>
        val variable = env.get[Variable](v.name)
        List.tabulate(variable.typ.size) { i => AssemblyLine.variable(ctx, STA, variable, i) }
      case SeparateBytesExpression(h: VariableExpression, l: VariableExpression) =>
        val lv = env.get[Variable](l.name)
        val hv = env.get[Variable](h.name)
        List(
          AssemblyLine.variable(ctx, STA, lv),
          AssemblyLine.variable(ctx, STA, hv))
    }
    val lhsIsStack = targetBytes.head.head.opcode == TSX
    val targetSize = targetBytes.size
    val addendType = MlCompiler.getExpressionType(ctx, addend)
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
        val label = MlCompiler.nextLabel("de")
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
        val label = MlCompiler.nextLabel("in")
        return staTo(INC, targetBytes.head) ++ targetBytes.tail.flatMap(l => AssemblyLine.relative(BNE, label)::staTo(INC, l)) :+ AssemblyLine.label(label)
      case Some(NumericConstant(-1, _)) if canUseIncDec && subtract =>
        val label = MlCompiler.nextLabel("in")
        return staTo(INC, targetBytes.head) ++ targetBytes.tail.flatMap(l => AssemblyLine.relative(BNE, label)::staTo(INC, l)) :+ AssemblyLine.label(label)
      case Some(NumericConstant(1, _)) if canUseIncDec && subtract =>
        val label = MlCompiler.nextLabel("de")
        return doDec(targetBytes)
      case Some(NumericConstant(-1, _)) if canUseIncDec && !subtract =>
        val label = MlCompiler.nextLabel("de")
        return doDec(targetBytes)
      case Some(constant) =>
        addendSize = targetSize
        Nil -> List.tabulate(targetSize)(i => List(AssemblyLine.immediate(LDA, constant.subbyte(i))))
      case None =>
        addendSize match {
          case 1 =>
            val base = MlCompiler.compile(ctx, addend, Some(b -> RegisterVariable(Register.A, b)), NoBranching)
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
            val base = MlCompiler.compile(ctx, addend, Some(w -> RegisterVariable(Register.AX, w)), NoBranching)
            if (isRhsStack(base)) {
              val fixedBase = MlCompiler.compile(ctx, addend, Some(w -> RegisterVariable(Register.AY, w)), NoBranching)
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
                  val fixedBase = MlCompiler.compile(ctx, addend, Some(w -> RegisterVariable(Register.AY, w)), NoBranching)
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
            val label = MlCompiler.nextLabel("sx")
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
        val loadLhs = MlCompiler.compile(ctx, v, Some(b -> RegisterVariable(Register.A, b)), NoBranching)
        val modifyLhs = simpleOperation(operation, ctx, param, IndexChoice.PreferY, preserveA = true, commutative = true)
        val storeLhs = MlCompiler.compileByteStorage(ctx, Register.A, v)
        loadLhs ++ modifyLhs ++ storeLhs
    }
  }


  def compileInPlaceWordOrLongBitOp(ctx: CompilationContext, lhs: LhsExpression, param: Expression, operation: Opcode.Value): List[AssemblyLine] = {
    val env = ctx.env
    val b = env.get[Type]("byte")
    val w = env.get[Type]("word")
    val targetBytes: List[List[AssemblyLine]] = lhs match {
      case v: VariableExpression =>
        val variable = env.get[Variable](v.name)
        List.tabulate(variable.typ.size) { i => AssemblyLine.variable(ctx, STA, variable, i) }
      case SeparateBytesExpression(h: VariableExpression, l: VariableExpression) =>
        val lv = env.get[Variable](l.name)
        val hv = env.get[Variable](h.name)
        List(
          AssemblyLine.variable(ctx, STA, lv),
          AssemblyLine.variable(ctx, STA, hv))
      case _ =>
        ???
    }
    val lo = targetBytes.head
    val targetSize = targetBytes.size
    val paramType = MlCompiler.getExpressionType(ctx, param)
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
            val base = MlCompiler.compile(ctx, param, Some(b -> RegisterVariable(Register.A, b)), NoBranching)
            base -> List(Nil)
          case 2 =>
            val base = MlCompiler.compile(ctx, param, Some(w -> RegisterVariable(Register.AX, w)), NoBranching)
            base -> List(Nil, List(AssemblyLine.implied(TXA)))
          case _ => Nil -> (param match {
            case vv: VariableExpression =>
              val source = env.get[Variable](vv.name)
              List.tabulate(paramSize)(i => AssemblyLine.variable(ctx, LDA, source, i))
          })
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
              val label = MlCompiler.nextLabel("sx")
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


}
