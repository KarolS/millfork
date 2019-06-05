package millfork.compiler.z80

import millfork.CompilationFlag
import millfork.assembly.z80._
import millfork.compiler.{AbstractExpressionCompiler, CompilationContext}
import millfork.env._
import millfork.node.{ConstantArrayElementExpression, Expression, LhsExpression, ZRegister}

import scala.collection.mutable.ListBuffer

/**
  * @author Karol Stasiak
  */
object Z80Multiply {

  /**
    * Compiles A = A * D
    */
  private def multiplication(ctx: CompilationContext): List[ZLine] = {
    List(ZLine(ZOpcode.CALL, NoRegisters,
      ctx.env.get[ThingInMemory]("__mul_u8u8u8").toAddress))
  }

  /**
    * Compiles HL = A * DE
    */
  private def multiplication16And8(ctx: CompilationContext): List[ZLine] = {
    List(ZLine(ZOpcode.CALL, NoRegisters,
      ctx.env.get[ThingInMemory]("__mul_u16u8u16").toAddress))
  }

  /**
    * Calculate A = l * r
    */
  def compile8BitMultiply(ctx: CompilationContext, params: List[Expression]): List[ZLine] = {
    var numericConst = 1L
    var otherConst: Constant = NumericConstant(1, 1)
    val filteredParams = params.filter { expr =>
      ctx.env.eval(expr) match {
        case None =>
          true
        case Some(NumericConstant(n, _)) =>
          numericConst *= n
          false
        case Some(c) =>
          otherConst = CompoundConstant(MathOperator.Times, otherConst, c).loByte.quickSimplify
          false
      }
    }
    val productOfConstants = CompoundConstant(MathOperator.Times, otherConst, NumericConstant(numericConst & 0xff, 1)).quickSimplify
    (filteredParams, otherConst) match {
      case (Nil, NumericConstant(n, _)) => List(ZLine.ldImm8(ZRegister.A, (numericConst * n).toInt))
      case (Nil, _) => List(ZLine.ldImm8(ZRegister.A, productOfConstants))
      case (List(a), NumericConstant(n, _)) => Z80ExpressionCompiler.compileToA(ctx, a) ++ compile8BitMultiply((numericConst * n).toInt)
      case (List(a), _) =>
        compile8BitMultiply(ctx, a, ConstantArrayElementExpression(productOfConstants))
      case (List(a, b), NumericConstant(n, _)) =>
        compile8BitMultiply(ctx, a, b) ++ compile8BitMultiply((numericConst * n).toInt)
      case _ => ???
    }
  }

  /**
    * Calculate A = l * r
    */
  def compile8BitMultiply(ctx: CompilationContext, l: Expression, r: Expression): List[ZLine] = {
    (ctx.env.eval(l), ctx.env.eval(r)) match {
      case (Some(a), Some(b)) => List(ZLine.ldImm8(ZRegister.A, CompoundConstant(MathOperator.Times, a, b).loByte.quickSimplify))
      case (Some(NumericConstant(count, _)), None) => Z80ExpressionCompiler.compileToA(ctx, r) ++ compile8BitMultiply(count.toInt)
      case (None, Some(NumericConstant(count, _))) => Z80ExpressionCompiler.compileToA(ctx, l) ++ compile8BitMultiply(count.toInt)
      case _ =>
        val lb = Z80ExpressionCompiler.compileToA(ctx, l)
        val rb = Z80ExpressionCompiler.compileToA(ctx, r)
        val load = if (lb.exists(Z80ExpressionCompiler.changesDE)) {
          lb ++ List(ZLine.ld8(ZRegister.D, ZRegister.A)) ++ Z80ExpressionCompiler.stashDEIfChanged(ctx, rb)
        } else {
          rb ++ List(ZLine.ld8(ZRegister.D, ZRegister.A)) ++ lb
        }
        load ++ multiplication(ctx)
    }
  }

  /**
    * Calculate A = l * r
    */
  def compile8BitInPlaceMultiply(ctx: CompilationContext, l: LhsExpression, r: Expression): List[ZLine] = {
    ctx.env.eval(r) match {
      case Some(NumericConstant(count, _)) =>
        val (load, store) = Z80ExpressionCompiler.calculateLoadAndStoreForByte(ctx, l)
        load ++ compile8BitMultiply(count.toInt) ++ store
      case Some(c) =>
        val (load, store) = Z80ExpressionCompiler.calculateLoadAndStoreForByte(ctx, l)
        load ++ List(ZLine.ldImm8(ZRegister.D, c)) ++ multiplication(ctx) ++ store
      case _ =>
        val (load, store) = Z80ExpressionCompiler.calculateLoadAndStoreForByte(ctx, l)
        val rb = Z80ExpressionCompiler.compileToA(ctx, r)
        val loadRegisters = if (load.exists(Z80ExpressionCompiler.changesDE)) {
          load ++ List(ZLine.ld8(ZRegister.D, ZRegister.A)) ++ Z80ExpressionCompiler.stashDEIfChanged(ctx, rb)
        } else {
          rb ++ List(ZLine.ld8(ZRegister.D, ZRegister.A)) ++ load
        }
        loadRegisters ++ multiplication(ctx) ++ store
    }
  }

  /**
    * Calculate A = p / q or A = p %% q
    */
  def compileUnsignedByteDivision(ctx: CompilationContext, p: LhsExpression, q: Expression, modulo: Boolean): List[ZLine] = {
    ctx.env.eval(q) match {
      case Some(NumericConstant(qq, _)) =>
        if (qq < 0) {
          ctx.log.error("Unsigned division by negative constant", q.position)
          Nil
        } else if (qq == 0) {
          ctx.log.error("Unsigned division by zero", q.position)
          Nil
        } else if (qq > 255) {
          if (modulo) Z80ExpressionCompiler.compileToA(ctx, p)
          else List(ZLine.ldImm8(ZRegister.A, 0))
        } else {
          compileUnsignedByteDivisionImpl(ctx, p, qq.toInt, modulo)
        }
      case Some(_) =>
        ctx.log.error("Unsigned division by unknown constant", q.position)
        Nil
      case None =>
        ctx.log.error("Unsigned division by a variable expression", q.position)
        Nil
    }
  }
  /**
      * Calculate A = p / q or A = p %% q
      */
    def compileUnsignedByteDivisionImpl(ctx: CompilationContext, p: LhsExpression, q: Int, modulo: Boolean): List[ZLine] = {
      import ZRegister._
      import ZOpcode._
      val result = ListBuffer[ZLine]()
      result ++= Z80ExpressionCompiler.compileToA(ctx, p)
      result += ZLine.ldImm8(E, 0)

      for (i <- 7.to(0, -1)) {
        if ((q << i) <= 255) {
          val lbl = ctx.nextLabel("dv")
          result += ZLine.imm8(CP, q << i)
          result += ZLine.jumpR(ctx, lbl, IfFlagSet(ZFlag.C))
          result += ZLine.imm8(SUB, q << i)
          result += ZLine.label(lbl)
          result += ZLine.implied(CCF) // TODO: optimize?
          if (ctx.options.flag(CompilationFlag.EmitExtended80Opcodes)) {
            result += ZLine.register(RL, E)
          } else {
            result += ZLine.ld8(D, A)
            result += ZLine.ld8(A, E)
            result += ZLine.implied(RLA)
            result += ZLine.ld8(E, A)
            result += ZLine.ld8(A, D)
          }
        }
      }
      if (!modulo) {
        result += ZLine.ld8(A, E)
      }
      result.toList
    }

  /**
    * Calculate HL = l * r
    */
  def compile16And8BitMultiplyToHL(ctx: CompilationContext, l: Expression, r: Expression): List[ZLine] = {
    (AbstractExpressionCompiler.getExpressionType(ctx, l).size,
      AbstractExpressionCompiler.getExpressionType(ctx, r).size) match {
      case (1, 2) => return compile16And8BitMultiplyToHL(ctx, r, l)
      case (2 | 1, 1) => // ok
      case _ => ctx.log.fatal("Invalid code path", l.position)
    }
    ctx.env.eval(r) match {
      case Some(c) =>
        Z80ExpressionCompiler.compileToDE(ctx, l) ++ List(ZLine.ldImm8(ZRegister.A, c)) ++ multiplication16And8(ctx)
      case _ =>
        val lw = Z80ExpressionCompiler.compileToDE(ctx, l)
        val rb = Z80ExpressionCompiler.compileToA(ctx, r)
        val loadRegisters = lw ++ Z80ExpressionCompiler.stashDEIfChanged(ctx, rb)
        loadRegisters ++ multiplication16And8(ctx)
    }
  }

  /**
    * Calculate l = l * r
    */
  def compile16And8BitInPlaceMultiply(ctx: CompilationContext, l: LhsExpression, r: Expression): List[ZLine] = {
    compile16And8BitMultiplyToHL(ctx, l, r) ++ Z80ExpressionCompiler.storeHL(ctx, l, signedSource = false)
  }

  /**
    * Calculate A = count * x
    */
  def compile8BitMultiply(count: Int): List[ZLine] = {
    import millfork.assembly.z80.ZOpcode._
    import ZRegister._
    count match {
      case 0 => List(ZLine.ldImm8(A, 0))
      case 1 => Nil
      case n if n > 0 && n.-(1).&(n).==(0) => List.fill(Integer.numberOfTrailingZeros(n))(ZLine.register(ADD, A))
      case _ =>
        ZLine.ld8(E,A) :: Integer.toString(count & 0xff, 2).tail.flatMap{
          case '0' => List(ZLine.register(ADD, A))
          case '1' => List(ZLine.register(ADD, A), ZLine.register(ADD, E))
        }.toList
    }
  }
}
