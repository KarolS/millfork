package millfork.compiler.m6809

import millfork.assembly.m6809.MLine
import millfork.compiler.CompilationContext
import millfork.node.Expression
import millfork.assembly.m6809.MOpcode._
import millfork.env.NumericConstant
import millfork.node.M6809Register._

/**
  * @author Karol Stasiak
  */
object M6809DecimalBuiltins {

  def compileByteDecimalShiftLeft(ctx: CompilationContext, lhs: Option[Expression], rhs: Expression): List[MLine] = {
    val load = lhs match {
      case None => List(MLine.indexedX(LDA, 0))
      case Some(l) => M6809ExpressionCompiler.compileToA(ctx, l)
    }
    val loop = ctx.env.eval(rhs) match {
      case Some(NumericConstant(0, _)) => Nil
      case Some(NumericConstant(1, _)) => List(
        MLine.pp(PSHS, A),
        MLine.accessAndPullS(ADDA),
        MLine.inherent(DAA)
      )
      case _ =>
        val labelSkip = ctx.nextLabel("ss")
        val labelRepeat = ctx.nextLabel("sr")
        M6809ExpressionCompiler.stashAIfNeeded(ctx, M6809ExpressionCompiler.compileToB(ctx, rhs)) ++ List(
          MLine.label(labelRepeat),
          MLine.immediate(CMPB, 0),
          MLine.shortBranch(BEQ, labelSkip),
          MLine.pp(PSHS, A),
          MLine.accessAndPullS(ADDA),
          MLine.inherent(DAA),
          MLine.inherentB(DEC),
          MLine.shortBranch(BRA, labelRepeat),
          MLine.label(labelSkip)
        )
    }
    val store = lhs match {
      case None => List(MLine.indexedX(STA, 0))
      case Some(l) => Nil
    }
    load ++ loop ++ store
  }
}
