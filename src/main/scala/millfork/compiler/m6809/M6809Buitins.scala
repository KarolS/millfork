package millfork.compiler.m6809

import millfork.assembly.m6809.{Absolute, Immediate, MLine, MLine0, MOpcode}
import millfork.assembly.m6809.MOpcode._
import millfork.compiler.CompilationContext
import millfork.env.{CompoundConstant, Constant, MathOperator, NumericConstant}
import millfork.node.{Expression, LhsExpression, M6809Register, SumExpression}

import scala.collection.mutable.ListBuffer

/**
  * @author Karol Stasiak
  */
object M6809Buitins {

  def perform8BitInPlace(ctx: CompilationContext, l: LhsExpression, r: Expression, opcode: MOpcode.Value): List[MLine] = {
    val lc = M6809ExpressionCompiler.compileToB(ctx, l)
    val rc = M6809ExpressionCompiler.compileToB(ctx, r)
    (lc, rc) match {
      case (_, List(MLine0(LDB, Immediate, c))) if c.isProvably(1) && opcode == ADDB && lc.last.opcode == LDB =>
        lc.init ++ List(lc.last.copy(opcode = INC))
      case (_, List(MLine0(LDB, Immediate, c))) if c.isProvably(1) && opcode == SUBB && lc.last.opcode == LDB =>
        lc.init ++ List(lc.last.copy(opcode = DEC))
      case (List(ldl@MLine0(LDB, Absolute(false), _)), _) if opcode != SUBB =>
        rc ++ List(ldl.copy(opcode = opcode), ldl.copy(opcode = STB))
      case (_, List(ldr@MLine0(LDB, Absolute(false), _))) if lc.last.opcode == LDB =>
        lc ++ List(ldr.copy(opcode = opcode), lc.last.copy(opcode = STB))
      case _ if lc.last.opcode == LDB =>
        lc ++ List(MLine.pp(PSHS, M6809Register.B)) ++ rc ++ List(MLine.accessAndPullS(opcode))
      case _ =>
        println(lc)
        ???
    }
  }


  def split(ctx: CompilationContext, expr: SumExpression): (Constant, List[(Boolean, Expression)]) = {
    var constant = Constant.Zero
    val variable = ListBuffer[(Boolean, Expression)]()
    for((neg, e) <- expr.expressions) {
      ctx.env.eval(e) match {
        case Some(c) if !neg || !expr.decimal =>
          constant = CompoundConstant(
            (neg, expr.decimal) match {
              case (false, false) => MathOperator.Plus
              case (true, false) => MathOperator.Minus
              case (false, true) => MathOperator.DecimalPlus
              case (false, true) => MathOperator.DecimalMinus
            },
            constant, c
          ).quickSimplify
        case _ => variable += (neg -> e)
      }
    }
    constant -> variable.toList
  }

  def split(ctx: CompilationContext, params: List[Expression], op: MathOperator.Value, empty: Int): (Constant, List[Expression]) = {
    var constant: Constant = NumericConstant(empty, Constant.minimumSize(empty))
    val variable = ListBuffer[Expression]()
    for(e <- params) {
      ctx.env.eval(e) match {
        case Some(c) =>
          constant = CompoundConstant(op, constant, c).quickSimplify
        case _ => variable += e
      }
    }
    constant -> variable.toList
  }


  def complexityB(code: List[MLine]): Int = code match {
    case List(MLine0(LDB, _, _)) => 0
    case _ => -1
  }
  def complexityD(code: List[MLine]): Int = code match {
    case List(MLine0(LDD, _, _)) => 0
    case _ => -1
  }

  def compileByteSum(ctx: CompilationContext, expr: SumExpression, fromScratch: Boolean): List[MLine] = {
    val ld = if (expr.decimal) LDA else LDB
    val add = if (expr.decimal) ADDA else ADDB
    val sub = if (expr.decimal) SUBA else SUBB
    val negLine = if (expr.decimal) MLine.inherentA(NEG) else MLine.inherentB(NEG)
    val (constant, variable) = split(ctx, expr)
    val addendReads = variable
      .map(addend => addend._1 -> M6809ExpressionCompiler.compileToB(ctx, addend._2))
      .map(code => complexityB(code._2) -> code).sortBy(_._1).map(_._2)
    val result = ListBuffer[MLine]()
    for ((neg, load) <- addendReads) {
      if (result.isEmpty && fromScratch) {
        result ++= load
        if (neg) result += MLine.inherent(NEG)
      } else {
        load match {
          case List(l@MLine0(LDB, _, _)) =>
            if (neg) {
              result += l.copy(opcode = sub)
              if (expr.decimal) ???
            } else {
              result += l.copy(opcode = add)
              if (expr.decimal) result += MLine.inherent(DAA)
            }
          case _ =>
            if (expr.decimal) {
              result += MLine.pp(PSHS, M6809Register.A)
              result ++= load
              result += MLine.pp(PULS, M6809Register.A)
              if (neg) {
                ???
                if (expr.decimal) ???
              } else {
                result += MLine.accessAndPullS(ADDA)
                result += MLine.inherent(DAA)
              }
            } else {
              result += MLine.pp(PSHS, M6809Register.B)
              result ++= load
              if (neg) result += negLine
              result += MLine.accessAndPullS(ADDB)
            }
        }
      }
    }
    if (!constant.isProvablyZero) {
      if (result.isEmpty) {
        result += MLine.immediate(ld, constant)
      } else {
        result += MLine.immediate(add, constant)
        if (expr.decimal) result += MLine.inherent(DAA)
      }
    }
    if (expr.decimal) result += MLine.tfr(M6809Register.A, M6809Register.B)
    result.toList
  }

  def compileByteBitwise(ctx: CompilationContext, params: List[Expression], fromScratch: Boolean, opcode: MOpcode.Value, op: MathOperator.Value, empty: Int): List[MLine] = {
    val (constant, variable) = split(ctx, params, op, empty)

    val addendReads = variable
      .map(addend => M6809ExpressionCompiler.compileToB(ctx, addend))
      .map(code => complexityB(code) -> code).sortBy(_._1).map(_._2)
    val result = ListBuffer[MLine]()
    for (load <- addendReads) {
      if (result.isEmpty && fromScratch) {
        result ++= load
      } else {
        load match {
          case List(l@MLine0(LDB, _, _)) =>
            result += l.copy(opcode = opcode)
          case _ =>
            result += MLine.pp(PSHS, M6809Register.B)
            result ++= load
            result += MLine.accessAndPullS(opcode)
        }
      }
    }
    if (!constant.isProvably(empty)) {
      if (result.isEmpty) {
        result += MLine.immediate(LDB, constant)
      } else {
        result += MLine.immediate(opcode, constant)
      }
    }
    result.toList
  }

  def compileWordSum(ctx: CompilationContext, expr: SumExpression, fromScratch: Boolean): List[MLine] = {
    ???
  }
  def compileWordBitwise(ctx: CompilationContext,
                         params: List[Expression],
                         fromScratch: Boolean,
                         opcodeB: MOpcode.Value,
                         opcodeA: MOpcode.Value,
                         op: MathOperator.Value,
                         empty: Int): List[MLine] = {
    ???
  }
}
