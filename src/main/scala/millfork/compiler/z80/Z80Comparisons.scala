package millfork.compiler.z80

import millfork.CompilationFlag
import millfork.assembly.z80._
import millfork.compiler.{ComparisonType, _}
import millfork.env.NumericConstant
import millfork.node.{Expression, ZRegister}

/**
  * @author Karol Stasiak
  */
object Z80Comparisons {

  import ComparisonType._

  def compile8BitComparison(ctx: CompilationContext, compType: ComparisonType.Value, l: Expression, r: Expression, branches: BranchSpec): List[ZLine] = {
    handleConstantComparison(ctx, compType, l, r, branches).foreach(return _)
    compType match {
      case GreaterUnsigned | LessOrEqualUnsigned | GreaterSigned | LessOrEqualSigned =>
        return compile8BitComparison(ctx, ComparisonType.flip(compType), r, l, branches)
      case _ => ()
    }
    val calculateFlags =
      Z80ExpressionCompiler.compileToA(ctx, r) ++
        List(ZLine.ld8(ZRegister.E, ZRegister.A)) ++
        Z80ExpressionCompiler.stashDEIfChanged(ctx, Z80ExpressionCompiler.compileToA(ctx, l)) ++
        List(ZLine.register(ZOpcode.CP, ZRegister.E))
    if (branches == NoBranching) return calculateFlags
    val jump = (compType, branches) match {
      case (Equal, BranchIfTrue(label)) => ZLine.jump(label, IfFlagSet(ZFlag.Z))
      case (Equal, BranchIfFalse(label)) => ZLine.jump(label, IfFlagClear(ZFlag.Z))
      case (NotEqual, BranchIfTrue(label)) => ZLine.jump(label, IfFlagClear(ZFlag.Z))
      case (NotEqual, BranchIfFalse(label)) => ZLine.jump(label, IfFlagSet(ZFlag.Z))
      case (LessUnsigned, BranchIfTrue(label)) => ZLine.jump(label, IfFlagSet(ZFlag.C))
      case (LessUnsigned, BranchIfFalse(label)) => ZLine.jump(label, IfFlagClear(ZFlag.C))
      case (GreaterOrEqualUnsigned, BranchIfTrue(label)) => ZLine.jump(label, IfFlagClear(ZFlag.C))
      case (GreaterOrEqualUnsigned, BranchIfFalse(label)) => ZLine.jump(label, IfFlagSet(ZFlag.C))
      case _ => ???
    }
    calculateFlags :+ jump
  }

  private def handleConstantComparison(ctx: CompilationContext, compType: ComparisonType.Value, l: Expression, r: Expression, branches: BranchSpec): Option[List[ZLine]] = {
    (ctx.env.eval(l), ctx.env.eval(r)) match {
      case (Some(NumericConstant(lc, _)), Some(NumericConstant(rc, _))) =>
        val constantCondition = compType match {
          case Equal => lc == rc
          case NotEqual => lc != rc
          case GreaterSigned | GreaterUnsigned => lc > rc
          case LessOrEqualSigned | LessOrEqualUnsigned => lc <= rc
          case GreaterOrEqualSigned | GreaterOrEqualUnsigned => lc >= rc
          case LessSigned | LessUnsigned => lc < rc
        }
        return Some(branches match {
          case BranchIfFalse(b) => if (!constantCondition) List(ZLine.jump(b)) else Nil
          case BranchIfTrue(b) => if (constantCondition) List(ZLine.jump(b)) else Nil
          case _ => Nil
        })
      case _ =>
    }
    None
  }

  def compile16BitComparison(ctx: CompilationContext, compType: ComparisonType.Value, l: Expression, r: Expression, branches: BranchSpec): List[ZLine] = {
    handleConstantComparison(ctx, compType, l, r, branches).foreach(return _)
    compType match {
      case GreaterUnsigned | LessOrEqualUnsigned | GreaterSigned | LessOrEqualSigned =>
        return compile16BitComparison(ctx, ComparisonType.flip(compType), r, l, branches)
      case _ => ()
    }
    val calculateLeft = Z80ExpressionCompiler.compileToHL(ctx, l)
    val calculateRight = Z80ExpressionCompiler.compileToHL(ctx, r)
    val (calculated, useBC) = if (calculateLeft.exists(Z80ExpressionCompiler.changesBC)) {
      if (calculateLeft.exists(Z80ExpressionCompiler.changesDE)) {
        calculateRight ++ List(ZLine.register(ZOpcode.PUSH, ZRegister.HL)) ++ Z80ExpressionCompiler.fixTsx(ctx, calculateLeft) ++ List(ZLine.register(ZOpcode.POP, ZRegister.BC)) -> false
      } else {
        calculateRight ++ List(ZLine.ld8(ZRegister.D, ZRegister.H), ZLine.ld8(ZRegister.E, ZRegister.L)) ++ calculateLeft -> false
      }
    } else {
      calculateRight ++ List(ZLine.ld8(ZRegister.B, ZRegister.H), ZLine.ld8(ZRegister.C, ZRegister.L)) ++ calculateLeft -> true
    }
    if (ctx.options.flag(CompilationFlag.EmitZ80Opcodes)) {
      val calculateFlags = calculated ++ List(
        ZLine.register(ZOpcode.OR, ZRegister.A),
        ZLine.registers(ZOpcode.SBC_16, ZRegister.HL, if (useBC) ZRegister.BC else ZRegister.DE))
      if (branches == NoBranching) return calculateFlags
      val jump = (compType, branches) match {
        case (Equal, BranchIfTrue(label)) => ZLine.jump(label, IfFlagSet(ZFlag.Z))
        case (Equal, BranchIfFalse(label)) => ZLine.jump(label, IfFlagClear(ZFlag.Z))
        case (NotEqual, BranchIfTrue(label)) => ZLine.jump(label, IfFlagClear(ZFlag.Z))
        case (NotEqual, BranchIfFalse(label)) => ZLine.jump(label, IfFlagSet(ZFlag.Z))
        case (LessUnsigned, BranchIfTrue(label)) => ZLine.jump(label, IfFlagSet(ZFlag.C))
        case (LessUnsigned, BranchIfFalse(label)) => ZLine.jump(label, IfFlagClear(ZFlag.C))
        case (GreaterOrEqualUnsigned, BranchIfTrue(label)) => ZLine.jump(label, IfFlagClear(ZFlag.C))
        case (GreaterOrEqualUnsigned, BranchIfFalse(label)) => ZLine.jump(label, IfFlagSet(ZFlag.C))
        case _ => ???
      }
      calculateFlags :+ jump
    } else if (compType == Equal || compType == NotEqual) {
      import ZRegister._
      import ZOpcode._
      val calculateFlags = calculated ++ List(
        ZLine.ld8(A, L),
        ZLine.register(XOR, if (useBC) C else E),
        ZLine.ld8(L, A),
        ZLine.ld8(A, H),
        ZLine.register(XOR, if (useBC) B else D),
        ZLine.register(OR, L))
      if (branches == NoBranching) return calculateFlags
      val jump = (compType, branches) match {
        case (Equal, BranchIfTrue(label)) => ZLine.jump(label, IfFlagSet(ZFlag.Z))
        case (Equal, BranchIfFalse(label)) => ZLine.jump(label, IfFlagClear(ZFlag.Z))
        case (NotEqual, BranchIfTrue(label)) => ZLine.jump(label, IfFlagClear(ZFlag.Z))
        case (NotEqual, BranchIfFalse(label)) => ZLine.jump(label, IfFlagSet(ZFlag.Z))
        case _ => throw new IllegalStateException()
      }
      calculateFlags :+ jump
    } else {
      import ZRegister._
      import ZOpcode._
      val calculateFlags = calculated ++ List(
              ZLine.ld8(A, L),
              ZLine.register(SUB, if (useBC) C else E),
              ZLine.ld8(A, H),
              ZLine.register(SBC, if (useBC) B else D))
      if (branches == NoBranching) return calculateFlags
      val jump = (compType, branches) match {
        case (LessUnsigned, BranchIfTrue(label)) => ZLine.jump(label, IfFlagSet(ZFlag.C))
        case (LessUnsigned, BranchIfFalse(label)) => ZLine.jump(label, IfFlagClear(ZFlag.C))
        case (GreaterOrEqualUnsigned, BranchIfTrue(label)) => ZLine.jump(label, IfFlagClear(ZFlag.C))
        case (GreaterOrEqualUnsigned, BranchIfFalse(label)) => ZLine.jump(label, IfFlagSet(ZFlag.C))
        case _ => ???
      }
      calculateFlags :+ jump
    }
  }

  def compileLongRelativeComparison(ctx: CompilationContext, compType: ComparisonType.Value, l: Expression, r: Expression, branches: BranchSpec): List[ZLine] = {
    handleConstantComparison(ctx, compType, l, r, branches).foreach(return _)
    compType match {
      case Equal | NotEqual => throw new IllegalArgumentException
      case GreaterUnsigned | LessOrEqualUnsigned | GreaterSigned | LessOrEqualSigned =>
        return compileLongRelativeComparison(ctx, ComparisonType.flip(compType), r, l, branches)
      case _ => ()
    }
    val lt = Z80ExpressionCompiler.getExpressionType(ctx, l)
    val rt = Z80ExpressionCompiler.getExpressionType(ctx, r)
    val size = lt.size max rt.size
    val calculateLeft = Z80ExpressionCompiler.compileByteReads(ctx, l, size, ZExpressionTarget.HL)
    val calculateRight = Z80ExpressionCompiler.compileByteReads(ctx, r, size, ZExpressionTarget.BC)
    val preserveHl = isBytesFromHL(calculateLeft)
    val preserveBc = isBytesFromBC(calculateRight)
    val calculateFlags = calculateLeft.zip(calculateRight).zipWithIndex.flatMap { case ((lb, rb), i) =>
      import ZOpcode._
      import ZRegister._
      val sub = if (i == 0) SUB else SBC
      var compareBytes = (lb, rb) match {
        case (List(ZLine(LD, TwoRegisters(A, _), _, _)),
        List(ZLine(LD, TwoRegisters(A, IMM_8), param, _))) =>
          lb :+ ZLine.imm8(sub, param)
        case (List(ZLine(LD, TwoRegisters(A, _), _, _)),
        List(ZLine(LD, TwoRegisters(A, reg), _, _))) if reg != MEM_ABS_8 =>
          lb :+ ZLine.register(sub, reg)
        case (List(ZLine(LD, TwoRegisters(A, _), _, _)), _) =>
          Z80ExpressionCompiler.stashAFIfChangedF(ctx, rb :+ ZLine.ld8(E, A)) ++ lb :+ ZLine.register(sub, E)
        case _ =>
          if (preserveBc || preserveHl) ??? // TODO: preserve HL/BC for the next round of comparisons
        var compileArgs = rb ++ List(ZLine.ld8(E, A)) ++ Z80ExpressionCompiler.stashDEIfChanged(ctx, lb) ++ List(ZLine.ld8(D, A))
          if (i > 0) compileArgs = Z80ExpressionCompiler.stashAFIfChangedF(ctx, compileArgs)
          compileArgs ++ List(ZLine.ld8(A, D), ZLine.register(sub, E))
      }
      if (i > 0 && preserveBc) compareBytes = Z80ExpressionCompiler.stashBCIfChanged(ctx, compareBytes)
      if (i > 0 && preserveHl) compareBytes = Z80ExpressionCompiler.stashHLIfChanged(ctx, compareBytes)
      compareBytes
    }
    if (branches == NoBranching) return calculateFlags
    val jump = (compType, branches) match {
      case (Equal, BranchIfTrue(label)) => ZLine.jump(label, IfFlagSet(ZFlag.Z))
      case (Equal, BranchIfFalse(label)) => ZLine.jump(label, IfFlagClear(ZFlag.Z))
      case (NotEqual, BranchIfTrue(label)) => ZLine.jump(label, IfFlagClear(ZFlag.Z))
      case (NotEqual, BranchIfFalse(label)) => ZLine.jump(label, IfFlagSet(ZFlag.Z))
      case (LessUnsigned, BranchIfTrue(label)) => ZLine.jump(label, IfFlagSet(ZFlag.C))
      case (LessUnsigned, BranchIfFalse(label)) => ZLine.jump(label, IfFlagClear(ZFlag.C))
      case (GreaterOrEqualUnsigned, BranchIfTrue(label)) => ZLine.jump(label, IfFlagClear(ZFlag.C))
      case (GreaterOrEqualUnsigned, BranchIfFalse(label)) => ZLine.jump(label, IfFlagSet(ZFlag.C))
      case _ => ???
    }
    calculateFlags :+ jump
  }

  def compileLongEqualityComparison(ctx: CompilationContext, compType: ComparisonType.Value, l: Expression, r: Expression, branches: BranchSpec): List[ZLine] = {
    handleConstantComparison(ctx, compType, l, r, branches).foreach(return _)
    val lt = Z80ExpressionCompiler.getExpressionType(ctx, l)
    val rt = Z80ExpressionCompiler.getExpressionType(ctx, r)
    val size = lt.size max rt.size
    val calculateLeft = Z80ExpressionCompiler.compileByteReads(ctx, l, size, ZExpressionTarget.HL)
    val calculateRight = Z80ExpressionCompiler.compileByteReads(ctx, r, size, ZExpressionTarget.BC)
    val preserveHl = isBytesFromHL(calculateLeft)
    val preserveBc = isBytesFromBC(calculateRight)
    val innerLabel = ctx.nextLabel("cp")
    val (jump, epilogue) = (compType, branches) match {
      case (Equal, BranchIfTrue(label)) =>
        ZLine.jump(innerLabel, IfFlagClear(ZFlag.Z)) -> ZLine.jump(label, IfFlagSet(ZFlag.Z))
      case (NotEqual, BranchIfFalse(label)) =>
        ZLine.jump(innerLabel, IfFlagClear(ZFlag.Z)) -> ZLine.jump(label, IfFlagSet(ZFlag.Z))
      case (Equal, BranchIfFalse(label)) =>
        ZLine.jump(innerLabel, IfFlagSet(ZFlag.Z)) -> ZLine.jump(label, IfFlagClear(ZFlag.Z))
      case (NotEqual, BranchIfTrue(label)) =>
        ZLine.jump(innerLabel, IfFlagSet(ZFlag.Z)) -> ZLine.jump(label, IfFlagClear(ZFlag.Z))
      case (_, NoBranching) => ZLine.implied(ZOpcode.NOP) -> ZLine.implied(ZOpcode.NOP)
      case _ => throw new IllegalArgumentException
    }
    val calculateFlags = calculateLeft.zip(calculateRight).zipWithIndex.flatMap { case ((lb, rb), i) =>
      var compareBytes = {
        import ZOpcode._
        import ZRegister._
        (lb, rb) match {
          case (_, List(ZLine(LD, TwoRegisters(A, IMM_8), param, _))) =>
            lb :+ ZLine.imm8(CP, param)
          case (List(ZLine(LD, TwoRegisters(A, IMM_8), param, _)), _) =>
            rb :+ ZLine.imm8(CP, param)
          case (List(ZLine(LD, TwoRegisters(A, _), _, _)),
          List(ZLine(LD, TwoRegisters(A, reg), _, _))) if reg != MEM_ABS_8 =>
            lb :+ ZLine.register(CP, reg)
          case (List(ZLine(LD, TwoRegisters(A, reg), _, _)),
          List(ZLine(LD, TwoRegisters(A, _), _, _))) if reg != MEM_ABS_8 =>
            rb :+ ZLine.register(CP, reg)
          case (List(ZLine(LD, TwoRegisters(A, _), _, _)), _) =>
            (rb :+ ZLine.ld8(E, A)) ++ lb :+ ZLine.register(CP, E)
          case _ =>
            var actualLb = lb
            if (i == 0 && preserveBc) actualLb = Z80ExpressionCompiler.stashBCIfChanged(ctx, actualLb)
            actualLb ++ List(ZLine.ld8(E, A)) ++ Z80ExpressionCompiler.stashDEIfChanged(ctx, rb) :+ ZLine.register(CP, E)
        }
      }
      if (i > 0 && preserveBc) compareBytes = Z80ExpressionCompiler.stashBCIfChanged(ctx, compareBytes)
      if (i > 0 && preserveHl) compareBytes = Z80ExpressionCompiler.stashHLIfChanged(ctx, compareBytes)
      if (i != size - 1 && branches != NoBranching) compareBytes :+ jump else compareBytes
    }
    if (branches == NoBranching) calculateFlags
    else calculateFlags ++ List(epilogue, ZLine.label(innerLabel))
  }


  private def isBytesFromHL(calculateLeft: List[List[ZLine]]) = {
    calculateLeft(1) match {
      case List(ZLine(ZOpcode.LD, TwoRegisters(ZRegister.A, ZRegister.H), _, _)) => true
      case _ => false
    }
  }

  private def isBytesFromBC(calculateLeft: List[List[ZLine]]) = {
    calculateLeft(1) match {
      case List(ZLine(ZOpcode.LD, TwoRegisters(ZRegister.A, ZRegister.B), _, _)) => true
      case _ => false
    }
  }
}
