package millfork.compiler.m6809

import millfork.assembly.m6809.{MLine, RegisterSet}
import millfork.compiler.CompilationContext
import millfork.env.{CompoundConstant, Constant, MathOperator, NumericConstant, ThingInMemory}
import millfork.node.{Expression, IndexedExpression, LiteralExpression, M6809Register, VariableExpression}
import millfork.assembly.m6809.MOpcode._
import M6809Register._
import millfork.CompilationFlag

import scala.collection.mutable.ListBuffer

/**
  * @author Karol Stasiak
  */
object M6809MulDiv {

  def compileByteMultiplication(ctx: CompilationContext, params: List[Expression], updateDerefX: Boolean, forceMul: Boolean = false): List[MLine] = {
    var constant = Constant.One
    val variablePart = if(forceMul) params else params.flatMap { p =>
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
      result += MLine.pp(PSHS, X)
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
      result += MLine.pp(PULS, X)
      result += MLine.indexedX(STD, 0)
    }
    result.toList
  }

  def compileByteDivision(ctx: CompilationContext, left: Option[Expression], right: Expression, mod: Boolean): List[MLine] = {
    ctx.env.eval(right) match {
      case Some(NumericConstant(0, _)) =>
        ctx.log.error("Division by zero", right.position)
        return left match {
          case Some(l) => M6809ExpressionCompiler.compileToB(ctx, l)
          case None => List(MLine.indexedX(CLR, 0))
        }
      case Some(NumericConstant(1, _)) =>
        return left match {
          case Some(l) =>
            if (mod) M6809ExpressionCompiler.compileToB(ctx, l) :+ MLine.immediate(LDB, 0)
            else M6809ExpressionCompiler.compileToB(ctx, l)
          case None =>
            if (mod) List(MLine.indexedX(CLR, 0))
            else Nil
        }
      case Some(NumericConstant(n@(2 | 4 | 8 | 16 | 32 | 64 | 128), _)) if mod =>
        return left match {
          case Some(l) => M6809ExpressionCompiler.compileToB(ctx, l) :+ MLine.immediate(ANDB, n.toInt - 1)
          case None => List(MLine.indexedX(LDB, 0), MLine.immediate(ANDB, n.toInt - 1), MLine.indexedX(STB, 0))
        }
      case Some(NumericConstant(q@(2 | 4 | 8 | 16 | 32), _)) if !mod =>
        // division by 2^n takes 2n cycles and n bytes, ignoring loads/stores
        val result = ListBuffer[MLine]()
        left match {
          case Some(l) => result ++= M6809ExpressionCompiler.compileToB(ctx, l)
          case None => result += MLine.indexedX(LDB, 0)
        }
        for (i <- 0 until Integer.numberOfTrailingZeros(q.toInt)) {
          result += MLine.inherentB(LSR)
        }
        if (left.isEmpty) {
          result += MLine.indexedX(STB, 0)
        }
        return result.toList
      case Some(NumericConstant(q, _)) if !mod && q >= 128 => {
        val result = ListBuffer[MLine]()
        left match {
          case Some(l) => result ++= M6809ExpressionCompiler.compileToB(ctx, l)
          case None => result += MLine.indexedX(LDB, 0)
        }
        result += MLine.immediate(SUBB, q.toInt)
        result += MLine.inherent(SEX)
        left match {
          case Some(l) =>
            // TODO: in this order?
            result += MLine.tfr(A, B)
            result += MLine.immediate(ANDB, 1)
            result += MLine.immediate(EORB, 1)
          case None =>
            result += MLine.immediate(ANDA, 1)
            result += MLine.immediate(EORA, 1)
            result += MLine.indexedX(STA, 0)
        }
        return result.toList

      }
      case Some(NumericConstant(q, _)) if !mod && q < 128 => {
        // division with a magic constant takes 2+11+2k+6 = 19+2k cycles and 2+1+k+2 = 5+k bytes, ignoring loads/stores for the not-in-place version
        val nc = 256 - 256 % q.toInt - 1
        var power = 256
        var shift = 8
        while (power <= nc * (q - 1 - (power - 1) % q)) {
          power *= 2
          shift += 1
        }
        val magic = ((power + q - 1 - (power - 1) % q) / q).toInt
        val result = ListBuffer[MLine]()
        left match {
          case Some(l) => result ++= M6809ExpressionCompiler.compileToB(ctx, l)
          case None => result += MLine.indexedX(LDB, 0)
        }
        magic match {
          case 1 =>
          case 2 =>
            result += MLine.inherentB(ASL)
            result += MLine.inherentA(ROL)
          case 4 if !ctx.options.flags(CompilationFlag.OptimizeForSize) =>
            result += MLine.inherentB(ASL)
            result += MLine.inherentA(ROL)
            result += MLine.inherentB(ASL)
            result += MLine.inherentA(ROL)
          case _ =>
            result += MLine.immediate(LDA, magic)
            result += MLine.inherent(MUL)
        }
        for (i <- 8 until shift) {
          result += MLine.inherentA(LSR)
        }
        left match {
          case Some(l) => result += MLine.tfr(A, B)
          case None => result += MLine.indexedX(STA, 0)
        }
        return result.toList
      }
      // TODO: other interesting constants
      case _ => ()
    }
    val result = ListBuffer[MLine]()
    left match {
      case None =>
        result += MLine.indexedX(LDA, 0)
        result ++= M6809ExpressionCompiler.stashAIfNeeded(ctx, M6809ExpressionCompiler.stashXIfNeeded(ctx, M6809ExpressionCompiler.compileToB(ctx, right)))
      case Some(l) =>
        result ++= M6809ExpressionCompiler.compileToA(ctx, l)
        result ++= M6809ExpressionCompiler.stashAIfNeeded(ctx, M6809ExpressionCompiler.compileToB(ctx, right))
    }
    result += MLine.absolute(JSR, ctx.env.get[ThingInMemory]("__divmod_u8u8u8u8"))

    left match {
      case Some(_) =>
        if (!mod) result += MLine.tfr(A, B)
      case None =>
        if (mod)
          result += MLine.indexedX(STB, 0)
        else
          result += MLine.indexedX(STA, 0)
    }
    result.toList
  }

  def compileWordDivision(ctx: CompilationContext, left: Option[Expression], right: Expression, mod: Boolean): List[MLine] = {
    ctx.env.eval(right) match {
      case Some(NumericConstant(1, _)) =>
        return left match {
          case Some(l) =>
            if (mod) M6809ExpressionCompiler.compileToD(ctx, l) :+ MLine.immediate(LDD, 0)
            else M6809ExpressionCompiler.compileToD(ctx, l)
          case None =>
            if (mod) List(MLine.inherentA(CLR), MLine.inherentB(CLR), MLine.indexedX(STD, 0))
            else Nil
        }
      case Some(NumericConstant(n@(2 | 4 | 8 | 16 | 32 | 64 | 128), _)) if mod =>
        return left match {
          case Some(l) => M6809ExpressionCompiler.compileToD(ctx, l) ++ List(MLine.inherentA(CLR), MLine.immediate(ANDB, n.toInt - 1))
          case None => List(MLine.indexedX(LDB, 1), MLine.inherentA(CLR), MLine.immediate(ANDB, n.toInt - 1), MLine.indexedX(STD, 0))
        }
      case Some(NumericConstant(256, _)) if !mod =>
        return left match {
          case Some(l) => M6809ExpressionCompiler.compileToD(ctx, l) ++ List(MLine.tfr(A, B), MLine.inherentA(CLR))
          case None => List(MLine.indexedX(LDB, 0), MLine.inherentA(CLR), MLine.indexedX(STD, 0))
        }
      // TODO: other interesting constants
      case _ => ()
    }
    val result = ListBuffer[MLine]()
    left match {
      case None =>
        result += MLine.pp(PSHS, X)
        result += MLine.indexedX(LDX, 0)
        result ++= M6809ExpressionCompiler.stashXIfNeeded(ctx, M6809ExpressionCompiler.compileToD(ctx, right))
      case Some(l) =>
        result ++= M6809ExpressionCompiler.compileToX(ctx, l)
        result ++= M6809ExpressionCompiler.stashXIfNeeded(ctx, M6809ExpressionCompiler.compileToD(ctx, right))
    }
    result += MLine.absolute(JSR, ctx.env.get[ThingInMemory]("__divmod_u16u16u16u16"))
    if (!mod) {
      result += MLine.tfr(X, D)
    }
    if (left.isEmpty) {
      result += MLine.pp(PULS, X)
      result += MLine.indexedX(STD, 0)
    }
    result.toList
  }
}
