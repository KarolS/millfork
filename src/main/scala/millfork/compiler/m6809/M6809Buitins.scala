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
        // TODO: preserve X?
        lc ++ List(MLine.pp(PSHS, M6809Register.B)) ++ rc ++ List(MLine.accessAndPullS(opcode), lc.last.copy(opcode = STB))
      case _ =>
        println(lc)
        ???
    }
  }

  def perform16BitInPlace(ctx: CompilationContext, l: LhsExpression, r: Expression, opcode: MOpcode.Value, commutative: Boolean): List[MLine] = {
    val lc = M6809ExpressionCompiler.compileToD(ctx, l)
    val rc = M6809ExpressionCompiler.compileToD(ctx, r)
    (lc, rc) match {
      case (List(ldl@MLine0(LDD, Absolute(false), _)), _) if opcode != SUBD =>
        rc ++ List(ldl.copy(opcode = opcode), ldl.copy(opcode = STD))
      case (_, List(ldr@MLine0(LDD, Absolute(false), _))) if lc.last.opcode == LDD =>
        lc ++ List(ldr.copy(opcode = opcode), lc.last.copy(opcode = STD))
      case _ if lc.last.opcode == LDD && commutative =>
        // TODO: preserve X?
        lc ++ List(MLine.pp(PSHS, M6809Register.D)) ++ rc ++ List(MLine.accessAndPullSTwice(opcode), lc.last.copy(opcode = STD))
      case _ if lc.last.opcode == LDD =>
        // TODO: preserve X?
        rc ++ List(MLine.pp(PSHS, M6809Register.D)) ++ lc ++ List(MLine.accessAndPullSTwice(opcode), lc.last.copy(opcode = STD))
      case _ =>
        println(lc)
        ???
    }
  }

  def perform16BitInPlace(ctx: CompilationContext, l: LhsExpression, r: Expression, opcodeA: MOpcode.Value, opcodeB: MOpcode.Value, commutative: Boolean): List[MLine] = {
    val lc = M6809ExpressionCompiler.compileToD(ctx, l)
    val rc = M6809ExpressionCompiler.compileToD(ctx, r)
    (lc, rc) match {
      case (List(ldl@MLine0(LDD, Absolute(false), _)), _) =>
        rc ++ List(ldl.copy(opcode = opcodeA), ldl.copy(opcode = opcodeB, parameter = ldl.parameter + 1), ldl.copy(opcode = STD))
      case (_, List(ldr@MLine0(LDD, Absolute(false), _))) if lc.last.opcode == LDD =>
        lc ++ List(ldr.copy(opcode = opcodeA), ldr.copy(opcode = opcodeB, parameter = ldr.parameter + 1), lc.last.copy(opcode = STD))
      case (_, List(ldr@MLine0(LDD, Immediate, _))) if lc.last.opcode == LDD =>
        lc ++ List(ldr.copy(opcode = opcodeA, parameter = ldr.parameter.hiByte), ldr.copy(opcode = opcodeB, parameter = ldr.parameter.loByte), lc.last.copy(opcode = STD))
      case _ if lc.last.opcode == LDD && commutative =>
        // TODO: preserve X?
        lc ++ List(MLine.pp(PSHS, M6809Register.D)) ++ rc ++ List(MLine.accessAndPullS(opcodeA), MLine.accessAndPullS(opcodeB), lc.last.copy(opcode = STD))
      case _ if lc.last.opcode == LDD =>
        // TODO: preserve X?
        rc ++ List(MLine.pp(PSHS, M6809Register.D)) ++ lc ++ List(MLine.accessAndPullS(opcodeA), MLine.accessAndPullS(opcodeB), lc.last.copy(opcode = STD))
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
        if (neg) result += MLine.inherentB(NEG)
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
    if (expr.decimal) ???
    val (constant, variable) = split(ctx, expr)
    val addendReads = variable
      .map(addend => addend._1 -> M6809ExpressionCompiler.compileToD(ctx, addend._2))
      .map(code => (if (code._1) 1 else 0, complexityD(code._2)) -> code).sortBy(_._1).map(_._2)
    val result = ListBuffer[MLine]()
    for ((neg, load) <- addendReads) {
      if (result.isEmpty && fromScratch) {
        result ++= load
        if (neg) {
          result += MLine.immediate(EORA, 0xff)
          result += MLine.immediate(EORB, 0xff)
          result += MLine.immediate(ADDD, 1)
        }
      } else {
        load match {
          case List(l@MLine0(LDD, _, _)) =>
            if (neg) {
              result += l.copy(opcode = SUBD)
            } else {
              result += l.copy(opcode = ADDD)
            }
          case _ =>
            if (neg) {
              result += MLine.pp(PSHS, M6809Register.D)
              result ++= load
              // TODO: optimize
              result += MLine.immediate(EORA, 0xff)
              result += MLine.immediate(EORB, 0xff)
              result += MLine.immediate(ADDD, 1)
              result += MLine.accessAndPullSTwice(ADDD)
            } else {
              result += MLine.pp(PSHS, M6809Register.D)
              result ++= load
              result += MLine.accessAndPullSTwice(ADDD)
            }
        }
      }
    }
    if (!constant.isProvablyZero) {
      if (result.isEmpty) {
        result += MLine.immediate(LDD, constant)
      } else {
        result += MLine.immediate(ADDD, constant)
      }
    }
    result.toList
  }
  def compileWordBitwise(ctx: CompilationContext,
                         params: List[Expression],
                         fromScratch: Boolean,
                         opcodeA: MOpcode.Value,
                         opcodeB: MOpcode.Value,
                         op: MathOperator.Value,
                         empty: Int): List[MLine] = {
    val (constant, variable) = split(ctx, params, op, empty)
    val addendReads = variable
      .map(addend => M6809ExpressionCompiler.compileToD(ctx, addend))
      .map(code => complexityD(code) -> code).sortBy(_._1).map(_._2)
    val result = ListBuffer[MLine]()
    for (load <- addendReads) {
      if (result.isEmpty && fromScratch) {
        result ++= load
      } else {
        load match {
          case List(l@MLine0(LDD, Absolute(false), addr)) =>
            result += l.copy(opcode = opcodeA)
            result += l.copy(opcode = opcodeB, parameter = addr + 1)
          case List(l@MLine0(LDD, Immediate, c)) =>
            result += l.copy(opcode = opcodeA, parameter = c.hiByte)
            result += l.copy(opcode = opcodeB, parameter = c.loByte)
          case _ =>
            result += MLine.pp(PSHS, M6809Register.D)
            result ++= load
            result += MLine.accessAndPullS(opcodeA)
            result += MLine.accessAndPullS(opcodeB)
        }
      }
    }
    if (!constant.isProvably(empty)) {
      if (result.isEmpty) {
        result += MLine.immediate(LDD, constant)
      } else {
        result += MLine.immediate(opcodeA, constant.hiByte)
        result += MLine.immediate(opcodeB, constant.loByte)
      }
    }
    result.toList
  }

  def compileByteShiftForB(ctx: CompilationContext, rhs:Expression, left: Boolean): List[MLine] = {
    val op = if (left) MOpcode.ASL else MOpcode.LSR
    ctx.env.eval(rhs) match {
      case Some(NumericConstant(0, _)) => Nil
      case Some(NumericConstant(n, _)) => List.fill(n.toInt)(MLine.inherentB(op))
      case _ =>
        val loop = ctx.nextLabel("sr")
        val skip = ctx.nextLabel("ss")
        M6809ExpressionCompiler.stashBIfNeeded(ctx, M6809ExpressionCompiler.compileToX(ctx, rhs)) ++ List(
          MLine.label(loop),
          MLine.indexedX(LEAX, -1),
          MLine.immediate(CMPX, -1),
          MLine.shortBranch(BEQ, skip),
          MLine.inherentB(op),
          MLine.shortBranch(BRA, loop),
          MLine.label(skip)
        )
    }
  }

  def compileWordShiftForD(ctx: CompilationContext, rhs:Expression, left: Boolean): List[MLine] = {
    val op = if (left) List(MLine.inherentB(ASL), MLine.inherentA(ROL)) else List(MLine.inherentA(LSR), MLine.inherentB(ROR))
    ctx.env.eval(rhs) match {
      case Some(NumericConstant(0, _)) => Nil
      case Some(NumericConstant(8, _)) =>
        if (left) List(MLine.tfr(M6809Register.B, M6809Register.A), MLine.immediate(LDB, 0))
        else List(MLine.tfr(M6809Register.A, M6809Register.B), MLine.immediate(LDA, 0))
      case Some(NumericConstant(n, _)) => List.fill(n.toInt)(op).flatten
      case _ =>
        val loop = ctx.nextLabel("sr")
        val skip = ctx.nextLabel("ss")
        M6809ExpressionCompiler.stashDIfNeeded(ctx, M6809ExpressionCompiler.compileToX(ctx, rhs)) ++ List(
          MLine.label(loop),
          MLine.indexedX(LEAX, -1),
          MLine.immediate(CMPX, -1),
          MLine.shortBranch(BEQ, skip))++op++List(
          MLine.shortBranch(BRA, loop),
          MLine.label(skip)
        )
    }
  }

}
