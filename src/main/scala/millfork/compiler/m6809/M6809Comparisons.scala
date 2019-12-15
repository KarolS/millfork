package millfork.compiler.m6809

import millfork.assembly.m6809.{MLine, MLine0}
import millfork.compiler.{BranchIfFalse, BranchIfTrue, BranchSpec, ComparisonType, CompilationContext}
import millfork.node.{Expression, M6809Register}
import millfork.assembly.m6809.MOpcode._

/**
  * @author Karol Stasiak
  */
object M6809Comparisons  {

  def isTrivial(lc: List[MLine]): Boolean = lc match {
    case List(MLine0(LDB | LDD, _, _)) => true
    case _ => false
  }

  def compile8BitComparison(ctx: CompilationContext, comparisonType: ComparisonType.Value, l: Expression, r: Expression, branches: BranchSpec): scala.List[MLine] = {
    val jump = branches match {
      case BranchIfFalse(label) => return compile8BitComparison(ctx, ComparisonType.negate(comparisonType), l, r, branches.flip)
      case BranchIfTrue(label) => List(MLine.longBranch(
        comparisonType match {
          case ComparisonType.Equal => BEQ
          case ComparisonType.NotEqual => BNE
          case ComparisonType.GreaterOrEqualUnsigned => BCC
          case ComparisonType.LessUnsigned => BCS
          case ComparisonType.GreaterUnsigned => BHI
          case ComparisonType.LessOrEqualUnsigned => BLS
          case ComparisonType.GreaterSigned => BGT
          case ComparisonType.GreaterOrEqualSigned => BGE
          case ComparisonType.LessSigned => BLT
          case ComparisonType.LessOrEqualSigned => BLE
        },
        label
      ))
      case _ => Nil
    }
    val lc = M6809ExpressionCompiler.compileToB(ctx, l)
    val rc = M6809ExpressionCompiler.compileToB(ctx, r)
    if (isTrivial(lc) && !isTrivial(rc)) return compile8BitComparison(ctx, ComparisonType.flip(comparisonType), r, l, branches)
    if (isTrivial(rc)) {
      lc ++ rc.map(_.copy(opcode = CMPB)) ++ jump
    } else {
      rc ++ List(MLine.pp(PSHS, M6809Register.B)) ++ lc ++ List(MLine.accessAndPullS(CMPB)) ++ jump
    }
  }

  def compile16BitComparison(ctx: CompilationContext, comparisonType: ComparisonType.Value, l: Expression, r: Expression, branches: BranchSpec): scala.List[MLine] = {
    val jump = branches match {
      case BranchIfFalse(label) => return compile16BitComparison(ctx, ComparisonType.negate(comparisonType), l, r, branches.flip)
      case BranchIfTrue(label) => List(MLine.longBranch(
        comparisonType match {
          case ComparisonType.Equal => BEQ
          case ComparisonType.NotEqual => BNE
          case ComparisonType.GreaterOrEqualUnsigned => BCC
          case ComparisonType.LessUnsigned => BCS
          case ComparisonType.GreaterUnsigned => BHI
          case ComparisonType.LessOrEqualUnsigned => BLS
          case ComparisonType.GreaterSigned => BGT
          case ComparisonType.GreaterOrEqualSigned => BGE
          case ComparisonType.LessSigned => BLT
          case ComparisonType.LessOrEqualSigned => BLE
        },
        label
      ))
      case _ => Nil
    }
    val lc = M6809ExpressionCompiler.compileToD(ctx, l)
    val rc = M6809ExpressionCompiler.compileToD(ctx, r)
    if (isTrivial(lc) && !isTrivial(rc)) return compile16BitComparison(ctx, ComparisonType.flip(comparisonType), r, l, branches)
    if (isTrivial(rc)) {
      lc ++ rc.map(_.copy(opcode = CMPD)) ++ jump
    } else {
      rc ++ List(MLine.pp(PSHS, M6809Register.D)) ++ lc ++ List(MLine.accessAndPullSTwice(CMPD)) ++ jump
    }
  }

}
