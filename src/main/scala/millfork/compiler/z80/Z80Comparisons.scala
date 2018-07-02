package millfork.compiler.z80

import millfork.assembly.z80._
import millfork.compiler._
import millfork.env.NumericConstant
import millfork.node.{Expression, ZRegister}

/**
  * @author Karol Stasiak
  */
object Z80Comparisons {

  import ComparisonType._

  def compile8BitComparison(ctx: CompilationContext, compType: ComparisonType.Value, l: Expression, r: Expression, branches: BranchSpec): List[ZLine] = {
    (ctx.env.eval(l), ctx.env.eval(r)) match {
      case (Some(NumericConstant(lc, _)), Some(NumericConstant(rc, _))) =>
        val constantCondition = compType match {
          case Equal => lc == rc
          case NotEqual => lc != rc
          case GreaterSigned | GreaterUnsigned => lc > rc
          case LessOrEqualSigned | LessOrEqualUnsigned => lc <= rc
          case GreaterOrEqualSigned | GreaterOrEqualUnsigned=> lc >= rc
          case LessSigned | LessUnsigned => lc < rc
        }
        return branches match {
          case BranchIfFalse(b) => if (!constantCondition) List(ZLine.jump(b)) else Nil
          case BranchIfTrue(b) => if (constantCondition) List(ZLine.jump(b)) else Nil
          case _ => Nil
        }
      case _ =>
    }
    compType match {
      case GreaterUnsigned | LessOrEqualUnsigned | GreaterSigned | LessOrEqualSigned =>
        return compile8BitComparison(ctx, ComparisonType.flip(compType), r, l, branches)
      case _ => ()
    }
    val calculateFlags =
      Z80ExpressionCompiler.compileToA(ctx, r) ++
        List(ZLine.ld8(ZRegister.E, ZRegister.A)) ++
        Z80ExpressionCompiler.stashDEIfChanged(Z80ExpressionCompiler.compileToA(ctx, l)) ++
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
}
