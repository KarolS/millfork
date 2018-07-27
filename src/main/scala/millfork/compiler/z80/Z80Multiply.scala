package millfork.compiler.z80

import millfork.CompilationFlag
import millfork.assembly.z80._
import millfork.compiler.{BranchSpec, CompilationContext}
import millfork.env.{CompoundConstant, Constant, MathOperator, NumericConstant}
import millfork.node.{ConstantArrayElementExpression, Expression, LhsExpression, ZRegister}

/**
  * @author Karol Stasiak
  */
object Z80Multiply {

  /**
    * Compiles A = A * D
    */
  private def multiplication(ctx: CompilationContext): List[ZLine] = {
    import millfork.assembly.z80.ZOpcode._
    import ZRegister._
    import ZLine._
    if(ctx.options.flag(CompilationFlag.EmitExtended80Opcodes)) {
      val lblAdd = Z80Compiler.nextLabel("mu")
      val lblLoop = Z80Compiler.nextLabel("mu")
      val lblStart = Z80Compiler.nextLabel("mu")
      List(
        ld8(E, A),
        ldImm8(A, 0),
        jumpR(ctx, lblStart),
        label(lblAdd),
        register(ADD, E),
        label(lblLoop),
        register(SLA, E),
        label(lblStart),
        register(SRL, D),
        jumpR(ctx, lblAdd, IfFlagSet(ZFlag.C)),
        jumpR(ctx, lblLoop, IfFlagClear(ZFlag.Z)))
    } else {
      // TODO: optimize
      val lblAdd = Z80Compiler.nextLabel("mu")
      val lblLoop = Z80Compiler.nextLabel("mu")
      val lblStart = Z80Compiler.nextLabel("mu")
      List(
        ld8(E, A),
        ldImm8(C, 0),
        jumpR(ctx, lblStart),
        label(lblAdd),
        ld8(A, C),
        register(ADD, E),
        ld8(C, A),
        label(lblLoop),
        ld8(A, E),
        register(ADD, A),
        ld8(E, A),
        label(lblStart),
        register(OR, A),
        ld8(A, D),
        register(RR, A),
        ld8(D, A),
        jumpR(ctx, lblAdd, IfFlagSet(ZFlag.C)),
        jumpR(ctx, lblLoop, IfFlagClear(ZFlag.Z)),
        ld8(A, C))
    }
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
          lb ++ List(ZLine.ld8(ZRegister.D, ZRegister.A)) ++ Z80ExpressionCompiler.stashDEIfChanged(rb)
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
          load ++ List(ZLine.ld8(ZRegister.D, ZRegister.A)) ++ Z80ExpressionCompiler.stashDEIfChanged(rb)
        } else {
          rb ++ List(ZLine.ld8(ZRegister.D, ZRegister.A)) ++ load
        }
        loadRegisters ++ multiplication(ctx) ++ store
    }
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
