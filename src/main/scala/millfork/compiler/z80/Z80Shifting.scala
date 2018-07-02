package millfork.compiler.z80

import millfork.CompilationFlag
import millfork.assembly.z80.{NoRegisters, ZLine, ZOpcode}
import millfork.compiler.CompilationContext
import millfork.env.NumericConstant
import millfork.node.{Expression, LhsExpression, ZRegister}

import scala.collection.GenTraversableOnce

/**
  * @author Karol Stasiak
  */
object Z80Shifting {

  private def fixAfterShiftIfNeeded(extendedOps: Boolean, left: Boolean, i: Long): List[ZLine] =
    if (extendedOps) {
      Nil
    } else if (left) {
      List(ZLine.imm8(ZOpcode.AND, 0xff & (0xff << i)))
    } else {
      List(ZLine.imm8(ZOpcode.AND, 0xff & (0xff >> i)))
    }

  def compile8BitShift(ctx: CompilationContext, lhs: Expression, rhs: Expression, left: Boolean): List[ZLine] = {
    val env = ctx.env
    val extendedOps = ctx.options.flag(CompilationFlag.EmitExtended80Opcodes)
    val op =
      if (extendedOps) {
        if (left) ZOpcode.SLA else ZOpcode.SRL
      } else {
        if (left) ZOpcode.RL else ZOpcode.RR
      }
    val l = Z80ExpressionCompiler.compileToA(ctx, lhs)
    env.eval(rhs) match {
      case Some(NumericConstant(i, _)) =>
        if (i <= 0) {
          l
        } else if (i >= 8) {
          l :+ ZLine.ldImm8(ZRegister.A, 0)
        } else {
          l ++ List.tabulate(i.toInt)(_ => ZLine.register(op, ZRegister.A)) ++ fixAfterShiftIfNeeded(extendedOps, left, i)
        }
      case _ =>
        val calcCount = Z80ExpressionCompiler.compileToA(ctx, rhs) :+ ZLine.ld8(ZRegister.B, ZRegister.A)
        val l = Z80ExpressionCompiler.stashBCIfChanged(Z80ExpressionCompiler.compileToA(ctx, lhs))
        val loopBody = ZLine.register(op, ZRegister.A) :: fixAfterShiftIfNeeded(extendedOps, left, 1)
        val label = Z80Compiler.nextLabel("sh")
        calcCount ++ l ++ List(ZLine.label(label)) ++ loopBody :+ ZLine.djnz(label)
    }
  }

  def compile8BitShiftInPlace(ctx: CompilationContext, lhs: LhsExpression, rhs: Expression, left: Boolean): List[ZLine] = {
    val env = ctx.env
    val extendedOps = ctx.options.flag(CompilationFlag.EmitExtended80Opcodes)
    val op =
      if (extendedOps) {
        if (left) ZOpcode.SLA else ZOpcode.SRL
      } else {
        if (left) ZOpcode.RL else ZOpcode.RR
      }
    env.eval(rhs) match {
      case Some(NumericConstant(i, _)) =>
        if (i <= 0) {
          Z80ExpressionCompiler.compileToA(ctx, lhs)
        } else if (i >= 8) {
          ZLine.ldImm8(ZRegister.A, 0) :: Z80ExpressionCompiler.storeA(ctx, lhs, signedSource = false)
        } else {
          Z80ExpressionCompiler.calculateAddressToAppropriatePointer(ctx, lhs) match {
            case Some((register, l)) =>
              // SLA A = 8 cycles
              // SLA (HL) = 15 cycles
              // LD A,(HL) or LD (HL),A = 7 cycles
              // SLA (IX+d) = 23 cycles
              // LD A,(IX+d) or LD (IX+d),A = 19 cycles
              // when using A is profitable?
              // for HL:
              // 15x > 7 + 8x + 7
              // 7x > 14
              // x > 2
              // for IX+d
              // 23x > 19 + 8x + 19
              // 15x > 38
              // x > 2.5
              // so:
              // - for x <= 2, don't use A
              // - for x >= 3, use A
              if (extendedOps && i <= 2) {
                l ++ List.tabulate(i.toInt)(_ => ZLine.register(op, register))
              } else {
                l ++ List(ZLine.ld8(ZRegister.A, register)) ++
                  List.tabulate(i.toInt)(_ => ZLine.register(op, ZRegister.A)) ++
                  fixAfterShiftIfNeeded(extendedOps, left, i) ++
                  List(ZLine.ld8(register, ZRegister.A))
              }
          }
        }
      case _ =>
        val calcCount = Z80ExpressionCompiler.compileToA(ctx, rhs) :+ ZLine.ld8(ZRegister.B, ZRegister.A)
        val l = Z80ExpressionCompiler.stashBCIfChanged(Z80ExpressionCompiler.compileToA(ctx, lhs))
        val loopBody = ZLine.register(op, ZRegister.A) :: fixAfterShiftIfNeeded(extendedOps, left, 1)
        val label = Z80Compiler.nextLabel("sh")
        calcCount ++ l ++ List(ZLine.label(label)) ++ loopBody ++ List(ZLine.djnz(label)) ++ Z80ExpressionCompiler.storeA(ctx, lhs, signedSource = false)
    }
  }

  def compile16BitShift(ctx: CompilationContext, lhs: Expression, rhs: Expression, left: Boolean): List[ZLine] = {
    val env = ctx.env
    val extendedOps = ctx.options.flag(CompilationFlag.EmitExtended80Opcodes)
    val l = Z80ExpressionCompiler.compileToHL(ctx, lhs)
    env.eval(rhs) match {
      case Some(NumericConstant(i, _)) =>
        if (i <= 0) {
          l
        } else if (i >= 16) {
          l :+ ZLine.ldImm16(ZRegister.HL, 0)
//        } else if (i > 8) { // TODO: optimize shifts larger than 8
//          ???
        } else {
          if (left) {
            l ++ List.tabulate(i.toInt)(_ => ZLine.registers(ZOpcode.ADD_16, ZRegister.HL, ZRegister.HL))
          } else {
            if (extendedOps) {
              l ++ (0L until i).flatMap(_ => List(
                ZLine.register(ZOpcode.SRL, ZRegister.H),
                ZLine.register(ZOpcode.RR, ZRegister.L)
              ))
            } else {
              l ++ (1L until i).flatMap(_ => List(
                ZLine.ld8(ZRegister.A, ZRegister.H),
                ZLine.register(ZOpcode.RR, ZRegister.A),
                ZLine.ld8(ZRegister.H, ZRegister.A),
                ZLine.ld8(ZRegister.A, ZRegister.L),
                ZLine.register(ZOpcode.RR, ZRegister.A),
                ZLine.ld8(ZRegister.L, ZRegister.A)
              )) ++ List(
                ZLine.ld8(ZRegister.A, ZRegister.H),
                ZLine.register(ZOpcode.RR, ZRegister.A),
                ZLine.imm8(ZOpcode.AND, (0xff >> i) & 0xff),
                ZLine.ld8(ZRegister.H, ZRegister.A),
                ZLine.ld8(ZRegister.A, ZRegister.L),
                ZLine.register(ZOpcode.RR, ZRegister.A),
                //                ZLine.imm8(ZOpcode.AND, (0xff << (i - 8)) & 0xff), // TODO: properly mask the low byte!!!
                ZLine.ld8(ZRegister.L, ZRegister.A)
              )
            }
          }
        }
      case _ =>
        val calcCount = Z80ExpressionCompiler.compileToA(ctx, rhs) :+ ZLine.ld8(ZRegister.B, ZRegister.A)
        val loopBody =
          if (extendedOps) {
            if (left) {
              List(
                ZLine.register(ZOpcode.SLA, ZRegister.L),
                ZLine.register(ZOpcode.RL, ZRegister.H))
            } else {
              List(
                ZLine.register(ZOpcode.SRL, ZRegister.H),
                ZLine.register(ZOpcode.RR, ZRegister.L))
            }
          } else {
            if (left) {
              List(
                ZLine.ld8(ZRegister.A, ZRegister.L),
                ZLine.register(ZOpcode.RL, ZRegister.A),
                ZLine.imm8(ZOpcode.AND, 0xfe),
                ZLine.ld8(ZRegister.L, ZRegister.A),
                ZLine.ld8(ZRegister.A, ZRegister.H),
                ZLine.register(ZOpcode.RL, ZRegister.A),
                ZLine.ld8(ZRegister.H, ZRegister.A))
            } else {
              List(
                ZLine.ld8(ZRegister.A, ZRegister.H),
                ZLine.register(ZOpcode.RR, ZRegister.A),
                ZLine.imm8(ZOpcode.AND, 0x7f),
                ZLine.ld8(ZRegister.H, ZRegister.A),
                ZLine.ld8(ZRegister.A, ZRegister.L),
                ZLine.register(ZOpcode.RR, ZRegister.A),
                ZLine.ld8(ZRegister.L, ZRegister.A))
            }
          }
        val label = Z80Compiler.nextLabel("sh")
        calcCount ++ l ++ List(ZLine.label(label)) ++ loopBody :+ ZLine.djnz(label)
    }
  }

}
