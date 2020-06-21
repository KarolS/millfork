package millfork.compiler.m6809

import millfork.assembly.m6809.{Immediate, MLine, MLine0}
import millfork.compiler.{AbstractExpressionCompiler, CompilationContext}
import millfork.node.{Expression, LhsExpression}
import millfork.assembly.m6809.MOpcode._
import millfork.env.{Constant, NumericConstant}
import millfork.node.M6809Register._

import scala.collection.mutable.ListBuffer

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
          MLine.immediate(CMPB, 0),
          MLine.shortBranch(BEQ, labelSkip),
          MLine.label(labelRepeat),
          MLine.pp(PSHS, A),
          MLine.accessAndPullS(ADDA),
          MLine.inherent(DAA),
          MLine.inherentB(DEC),
          MLine.shortBranch(BNE, labelRepeat),
          MLine.label(labelSkip)
        )
    }
    val store = lhs match {
      case None => List(MLine.indexedX(STA, 0))
      case Some(l) => Nil
    }
    load ++ loop ++ store
  }

  def compileLongDecimalShiftLeft(ctx: CompilationContext, lhs: LhsExpression, rhs: Expression): List[MLine] = {
    val result = new ListBuffer[MLine]()
    val byteCount = AbstractExpressionCompiler.getExpressionType(ctx, lhs).size
    val targetAddr = M6809ExpressionCompiler.compileAddressToX(ctx, lhs) match {
      case List(MLine0(LDX, Immediate, addr)) =>
        Some(addr)
      case xs =>
        result ++= xs
        None
    }
    val once = List.tabulate(byteCount){ i =>
      val innerResult = new ListBuffer[MLine]()
      targetAddr match {
        case Some(addr) =>
          innerResult += MLine.absolute(LDA, addr + (byteCount - 1 - i))
          innerResult += MLine.absolute(if (i == 0) ADDA else ADCA, addr + (byteCount - 1 - i))
        case None =>
          innerResult += MLine.indexedX(LDA, byteCount - 1 - i)
          innerResult += MLine.indexedX(if (i == 0) ADDA else ADCA, byteCount - 1 - i)
      }
      // TODO: use these if volatile:
      // innerResult += MLine.pp(PSHS, A)
      // innerResult += MLine.accessAndPullS(if (i == 0) ADDA else ADCA)
      innerResult += MLine.inherent(DAA)
      targetAddr match {
        case Some(addr) =>
          innerResult += MLine.absolute(STA, addr + (byteCount - 1 - i))
        case None =>
          innerResult += MLine.indexedX(STA, byteCount - 1 - i)
      }
      innerResult.toSeq
    }.flatten
    ctx.env.eval(rhs) match {
      case Some(NumericConstant(0, _)) =>
        // TODO: volatile?
      case Some(NumericConstant(1, _)) =>
        result ++= once
      case _ =>
        val labelSkip = ctx.nextLabel("ss")
        val labelRepeat = ctx.nextLabel("sr")
        result ++= M6809ExpressionCompiler.compileToB(ctx, rhs)
        result += MLine.immediate(CMPB, 0)
        result += MLine.shortBranch(BEQ, labelSkip)
        result += MLine.label(labelRepeat)
        result ++= once
        result += MLine.inherentB(DEC)
        result += MLine.shortBranch(BNE, labelRepeat)
        result += MLine.label(labelSkip)
    }
    result.toList
  }

  def compileInPlaceByteMultiplication(ctx: CompilationContext, target: LhsExpression, multiplicand: Expression): List[MLine] = {
    val multiplier = ctx.env.eval(multiplicand) match {
      case Some(NumericConstant(v, _)) =>
        if (v.&(0xf0) > 0x90 || v.&(0xf) > 9)
          ctx.log.error("Invalid decimal constant", multiplicand.position)
        (v.&(0xf0).>>(4) * 10 + v.&(0xf)).toInt min 99
      case _ =>
        ctx.log.error("Cannot multiply by a non-constant amount", multiplicand.position)
        return Nil
    }
    val result = new ListBuffer[MLine]()
    val targetAddr: Option[Constant] = M6809ExpressionCompiler.compileAddressToX(ctx, target) match {
      case List(MLine(LDX, Immediate, addr, _, _)) =>
        result += MLine.absolute(LDB, addr)
        Some(addr)
      case xs =>
        result ++= xs
        result += MLine.indexedX(LDB, 0)
        None
    }
    multiplier match {
      case 0 =>
        result += MLine.inherentA(CLR)
      case 1 =>
        result += MLine.tfr(B, A)
      case x =>
        result += MLine.tfr(B, A)
        result += MLine.pp(PSHS, D)
        val add1 = List()
        // TODO: rethink this:
        val ways = waysStolenFromTheZ80Implementation
        for(way <- ways(x)) way match {
          case 1 =>
            result += MLine.indexedS(ADDA, 1)
            result += MLine.inherent(DAA)
            result += MLine.indexedS(STA, 0)
          case q if q < 10 =>
            for(i<-0 until (q-1)) {
              result += MLine.indexedS(ADDA, 0)
              result += MLine.inherent(DAA)
            }
            result += MLine.indexedS(STA, 0)
          case q if q >= 10 =>
            result += MLine.inherentA(ASL)
            result += MLine.inherentA(ASL)
            result += MLine.inherentA(ASL)
            result += MLine.inherentA(ASL)
            for(i<-0 until (q-10)) {
              result += MLine.indexedS(ADDA, 0)
              result += MLine.inherent(DAA)
            }
            result += MLine.indexedS(STA, 0)
        }
        result += MLine.indexedS(LEAS, 2)
    }
    targetAddr match {
      case Some(addr) =>
        result += MLine.absolute(STA, addr)
      case None =>
        result += MLine.indexedX(STA, 0)
    }
    result.toList
  }

  private lazy val waysStolenFromTheZ80Implementation: Map[Int, List[Int]] = Map(
    2 -> List(2), 3 -> List(3), 4 -> List(2,2), 5 -> List(2,2,1), 6 -> List(3,2), 7 -> List(3,2,1), 8 -> List(2,2,2), 9 -> List(3,3), 10 -> List(10),
    11 -> List(11), 12 -> List(12), 13 -> List(13), 14 -> List(3,2,1,2), 15 -> List(3,5), 16 -> List(2,2,2,2), 17 -> List(2,2,2,2,1), 18 -> List(3,3,2), 19 -> List(3,3,2,1), 20 -> List(2,10),
    21 -> List(2,10,1), 22 -> List(11,2), 23 -> List(11,2,1), 24 -> List(12,2), 25 -> List(12,2,1), 26 -> List(2,13), 27 -> List(3,3,3), 28 -> List(3,2,1,2,2), 29 -> List(3,2,1,2,2,1), 30 -> List(3,10),
    31 -> List(3,10,1), 32 -> List(2,2,2,2,2), 33 -> List(11,3), 34 -> List(11,3,1), 35 -> List(11,3,1,1), 36 -> List(3,12), 37 -> List(3,12,1), 38 -> List(3,3,2,1,2), 39 -> List(3,13), 40 -> List(2,2,10),
    41 -> List(2,2,10,1), 42 -> List(2,10,1,2), 43 -> List(2,10,1,2,1), 44 -> List(11,2,2), 45 -> List(11,2,2,1), 46 -> List(11,2,1,2), 47 -> List(11,2,1,2,1), 48 -> List(12,2,2), 49 -> List(12,2,2,1), 50 -> List(2,2,1,10),
    51 -> List(2,2,1,10,1), 52 -> List(2,2,13), 53 -> List(2,2,13,1), 54 -> List(3,3,3,2), 55 -> List(11,5), 56 -> List(3,2,1,2,2,2), 57 -> List(3,3,2,1,3), 58 -> List(3,2,1,2,2,1,2), 59 -> List(3,2,1,2,2,1,2,1), 60 -> List(3,2,10),
    61 -> List(3,2,10,1), 62 -> List(3,10,1,2), 63 -> List(2,10,1,3), 64 -> List(2,2,2,2,2,2), 65 -> List(2,2,2,2,2,2,1), 66 -> List(11,3,2), 67 -> List(11,3,2,1), 68 -> List(11,3,1,2), 69 -> List(11,2,1,3), 70 -> List(3,2,1,10),
    71 -> List(3,2,1,10,1), 72 -> List(3,12,2), 73 -> List(3,12,2,1), 74 -> List(3,12,1,2), 75 -> List(12,2,1,3), 76 -> List(3,3,2,1,2,2), 77 -> List(3,2,1,11), 78 -> List(3,2,13), 79 -> List(3,2,13,1), 80 -> List(2,2,2,10),
    81 -> List(2,2,2,10,1), 82 -> List(2,2,10,1,2), 83 -> List(2,2,10,1,2,1), 84 -> List(2,10,1,2,2), 85 -> List(2,10,1,2,2,1), 86 -> List(2,10,1,2,1,2), 87 -> List(2,10,1,2,1,2,1), 88 -> List(11,2,2,2), 89 -> List(11,2,2,2,1), 90 -> List(3,3,10),
    91 -> List(3,3,10,1), 92 -> List(11,2,1,2,2), 93 -> List(3,10,1,3), 94 -> List(3,10,1,3,1), 95 -> List(3,3,2,1,5), 96 -> List(12,2,2,2), 97 -> List(12,2,2,2,1), 98 -> List(12,2,2,1,2), 99 -> List(11,3,3),
  )


  private def shiftOrRotateAccumulatorRight(ctx: CompilationContext, rotate: Boolean, preserveCarry: Boolean): List[MLine] = {
    val skipHiDigit = ctx.nextLabel("ds")
    val skipLoDigit = ctx.nextLabel("ds")
    val result = new ListBuffer[MLine]()
    result += (if (rotate) MLine.inherentA(ROR) else MLine.inherentA(LSR))
    if (preserveCarry) result += MLine.pp(PSHS, CC)
    result += MLine.shortBranch(BPL, skipHiDigit)
    result += MLine.immediate(SUBA, 0x30)
    result += MLine.label(skipHiDigit)
    result += MLine.immediate(BITA, 8)
    result += MLine.shortBranch(BEQ, skipLoDigit)
    result += MLine.immediate(SUBA, 0x3)
    result += MLine.label(skipLoDigit)
    if (preserveCarry) result += MLine.pp(PULS, CC)
    result.toList
  }

  def compileByteDecimalShiftRight(ctx: CompilationContext, lhs: Option[Expression], rhs: Expression): List[MLine] = {
    val result = new ListBuffer[MLine]()
    lhs match {
      case None => result += MLine.indexedX(LDA, 0)
      case Some(l) => result ++= M6809ExpressionCompiler.compileToA(ctx, l)
    }
    val once = shiftOrRotateAccumulatorRight(ctx, rotate = false, preserveCarry = false)
    ctx.env.eval(rhs) match {
      case Some(NumericConstant(0, _)) =>
      case Some(NumericConstant(1, _)) =>
        result ++= once
      case _ =>
        val labelSkip = ctx.nextLabel("ss")
        val labelRepeat = ctx.nextLabel("sr")
        result ++= M6809ExpressionCompiler.stashAIfNeeded(ctx, M6809ExpressionCompiler.compileToB(ctx, rhs))
        result += MLine.immediate(CMPB, 0)
        result += MLine.shortBranch(BEQ, labelSkip)
        result += MLine.label(labelRepeat)
        result ++= once
        result += MLine.inherentB(DEC)
        result += MLine.shortBranch(BNE, labelRepeat)
        result += MLine.label(labelSkip)

    }
    lhs match {
      case None => result += MLine.indexedX(STA, 0)
      case _ =>
    }
    result.toList
  }

  def compileLongDecimalShiftRight(ctx: CompilationContext, lhs: LhsExpression, rhs: Expression): List[MLine] = {
    val result = new ListBuffer[MLine]()
    val byteCount = AbstractExpressionCompiler.getExpressionType(ctx, lhs).size
    val targetAddr = M6809ExpressionCompiler.compileAddressToX(ctx, lhs) match {
      case List(MLine0(LDX, Immediate, addr)) =>
        Some(addr)
      case xs =>
        result ++= xs
        None
    }
    val once = List.tabulate(byteCount){ i =>
      val innerResult = new ListBuffer[MLine]()
      targetAddr match {
        case Some(addr) =>
          innerResult += MLine.absolute(LDA, addr + i)
        case None =>
          innerResult += MLine.indexedX(LDA, i)
      }
      innerResult ++= shiftOrRotateAccumulatorRight(ctx, rotate = i != 0, preserveCarry = i != byteCount - 1)
      targetAddr match {
        case Some(addr) =>
          innerResult += MLine.absolute(STA, addr + i)
        case None =>
          innerResult += MLine.indexedX(STA, i)
      }
      innerResult.toSeq
    }.flatten
    ctx.env.eval(rhs) match {
      case Some(NumericConstant(0, _)) =>
        // TODO: volatile?
      case Some(NumericConstant(1, _)) =>
        result ++= once
      case _ =>
        val labelSkip = ctx.nextLabel("ss")
        val labelRepeat = ctx.nextLabel("sr")
        result ++= M6809ExpressionCompiler.compileToB(ctx, rhs)
        result += MLine.immediate(CMPB, 0)
        result += MLine.shortBranch(BEQ, labelSkip)
        result += MLine.label(labelRepeat)
        result ++= once
        result += MLine.inherentB(DEC)
        result += MLine.shortBranch(BNE, labelRepeat)
        result += MLine.label(labelSkip)
    }
    result.toList
  }

}
