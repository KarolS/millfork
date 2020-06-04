package millfork.compiler.m6809

import millfork.assembly.m6809.{MLine, RegisterSet}
import millfork.compiler.CompilationContext
import millfork.env.{CompoundConstant, Constant, MathOperator, NumericConstant, ThingInMemory}
import millfork.node.{Expression, IndexedExpression, LiteralExpression, M6809Register, VariableExpression}
import millfork.assembly.m6809.MOpcode._
import M6809Register._

import scala.collection.mutable.ListBuffer

/**
  * @author Karol Stasiak
  */
object M6809MulDiv {

  def compileByteMultiplication(ctx: CompilationContext, params: List[Expression], updateDerefX: Boolean): List[MLine] = {
    var constant = Constant.One
    val variablePart = params.flatMap { p =>
      ctx.env.eval(p) match {
        case Some(c) =>
          constant = CompoundConstant(MathOperator.Times, constant, c).quickSimplify
          None
        case None => Some(p)
      }
    }
    if (!updateDerefX && variablePart.isEmpty) {
      return List(MLine.immediate(LDB, constant))
    }
    val result = ListBuffer[MLine]()
    if (updateDerefX) {
      result += MLine.indexedX(LDB, 0)
    } else {
      result ++= M6809ExpressionCompiler.compileToB(ctx, variablePart.head)
    }
    for (factor <- if (updateDerefX) variablePart else variablePart.tail) {
      factor match {
        case _: VariableExpression =>
          result ++= M6809ExpressionCompiler.stashBIfNeeded(ctx, M6809ExpressionCompiler.compileToA(ctx, factor))
        case _ =>
          // TODO: optimize?
          result += MLine.tfr(B, A)
          result ++= M6809ExpressionCompiler.stashAIfNeeded(ctx, M6809ExpressionCompiler.compileToB(ctx, factor))
      }
      result += MLine.inherent(MUL)
    }
    constant.loByte.quickSimplify match {
      case NumericConstant(0, _) => result += MLine.immediate(LDB, 0)
      case NumericConstant(1, _) => ()
      case NumericConstant(2, _) => result += MLine.inherentB(ASL)
      case NumericConstant(4, _) => result ++= List(MLine.inherentB(ASL), MLine.inherentB(ASL))
      case NumericConstant(8, _) => result ++= List(MLine.inherentB(ASL), MLine.inherentB(ASL), MLine.inherentB(ASL))
      case NumericConstant(16, _) => result ++= List(MLine.inherentB(ASL), MLine.inherentB(ASL), MLine.inherentB(ASL), MLine.inherentB(ASL))
      case NumericConstant(32, _) => result ++= List(MLine.inherentB(ASL), MLine.inherentB(ASL), MLine.inherentB(ASL), MLine.inherentB(ASL), MLine.inherentB(ASL))
      case NumericConstant(-1 | 255, _) => result += MLine.inherentB(NEG)
      case NumericConstant(-2 | 254, _) => result ++= List(MLine.inherentB(ASL), MLine.inherentB(NEG))
      case _ => result ++= List(MLine.immediate(LDA, constant), MLine.inherent(MUL))
    }
    if (updateDerefX) {
      result += MLine.indexedX(STB, 0)
    }
    result.toList
  }

  def compileWordMultiplication(ctx: CompilationContext, params: List[Expression], updateDerefX: Boolean): List[MLine] = {
    var constant = Constant.One
    val variablePart = params.flatMap { p =>
      ctx.env.eval(p) match {
        case Some(c) =>
          constant = CompoundConstant(MathOperator.Times, constant, c).quickSimplify
          None
        case None => Some(p)
      }
    }
    if (!updateDerefX && variablePart.isEmpty) {
      return List(MLine.immediate(LDD, constant))
    }
    val result = ListBuffer[MLine]()
    if (updateDerefX) {
      result += MLine.indexedX(LDD, 0)
      result += MLine(PSHS, RegisterSet(Set(X)), Constant.Zero)
    } else {
      result ++= M6809ExpressionCompiler.compileToD(ctx, variablePart.head)
    }
    for (factor <- if (updateDerefX) variablePart else variablePart.tail) {
      factor match {
        case _: VariableExpression =>
          result ++= M6809ExpressionCompiler.stashDIfNeeded(ctx, M6809ExpressionCompiler.compileToX(ctx, factor))
        case _ =>
          // TODO: optimize?
          result += MLine.tfr(D, X)
          result ++= M6809ExpressionCompiler.stashXIfNeeded(ctx, M6809ExpressionCompiler.compileToD(ctx, factor))
      }
      result += MLine.absolute(JSR, ctx.env.get[ThingInMemory]("__mul_u16u16u16"))
    }
    constant.loByte.quickSimplify match {
      case NumericConstant(0, _) => result += MLine.immediate(LDD, 0)
      case NumericConstant(1, _) => ()
      case NumericConstant(2, _) => result ++= List(MLine.inherentB(ASL), MLine.inherentA(ROL))
      case _ => result ++= List(MLine.immediate(LDX, constant), MLine.absolute(JSR, ctx.env.get[ThingInMemory]("__mul_u16u16u16")))
    }
    if (updateDerefX) {
      result += MLine(PULS, RegisterSet(Set(X)), Constant.Zero)
      result += MLine.indexedX(STD, 0)
    }
    result.toList
  }
}
