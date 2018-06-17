package millfork.compiler.z80

import millfork.assembly.z80.{ZLine, ZOpcode}
import millfork.compiler.CompilationContext
import millfork.node._
import ZOpcode._
import millfork.CompilationFlag
import millfork.env._
import millfork.error.ErrorReporting

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
object ZBuiltIns {

  def compile8BitOperation(ctx: CompilationContext, op: ZOpcode.Value, params: List[Expression]): List[ZLine] = {
    var const = op match {
      case AND => NumericConstant(0xff, 1)
      case OR | XOR => Constant.Zero
    }
    var hasConst = false
    var result = mutable.ListBuffer[ZLine]()
    params.foreach(expr => ctx.env.eval(expr) match {
      case None =>
        if (result.isEmpty) {
          result ++= Z80ExpressionCompiler.compileToA(ctx, expr)
        } else {
          result += ZLine.ld8(ZRegister.E, ZRegister.A)
          result ++= Z80ExpressionCompiler.stashDEIfChanged(Z80ExpressionCompiler.compileToA(ctx, expr))
          result += ZLine.register(op, ZRegister.E)
        }
      case Some(c) =>
        hasConst = true
        const = op match {
          case AND => CompoundConstant(MathOperator.And, const, c).quickSimplify
          case OR => CompoundConstant(MathOperator.Or, const, c).quickSimplify
          case XOR => CompoundConstant(MathOperator.Exor, const, c).quickSimplify
        }
    })
    if (hasConst) {
      result += ZLine.imm8(op, const)
    }
    result.toList
  }

  def compile16BitOperation(ctx: CompilationContext, op: ZOpcode.Value, params: List[Expression]): List[ZLine] = {
    var const = op match {
      case AND => NumericConstant(0xffff, 1)
      case OR | XOR => Constant.Zero
    }
    var hasConst = false
    var result = mutable.ListBuffer[ZLine]()
    params.foreach(expr => ctx.env.eval(expr) match {
      case None =>
        if (result.isEmpty) {
          result ++= Z80ExpressionCompiler.compileToHL(ctx, expr)
        } else {
          result += ZLine.ld8(ZRegister.E, ZRegister.L)
          result += ZLine.ld8(ZRegister.D, ZRegister.H)
          result ++= Z80ExpressionCompiler.stashDEIfChanged(Z80ExpressionCompiler.compileToHL(ctx, expr))
          result ++= List(
            ZLine.ld8(ZRegister.A, ZRegister.L),
            ZLine.register(op, ZRegister.E),
            ZLine.ld8(ZRegister.L, ZRegister.A),
            ZLine.ld8(ZRegister.A, ZRegister.H),
            ZLine.register(op, ZRegister.D),
            ZLine.ld8(ZRegister.H, ZRegister.A)
          )
        }
      case Some(c) =>
        hasConst = true
        const = op match {
          case AND => CompoundConstant(MathOperator.And, const, c).quickSimplify
          case OR => CompoundConstant(MathOperator.Or, const, c).quickSimplify
          case XOR => CompoundConstant(MathOperator.Exor, const, c).quickSimplify
        }
    })
    if (hasConst) {
      result ++= List(
        ZLine.ld8(ZRegister.A, ZRegister.L),
        ZLine.imm8(op, const.loByte),
        ZLine.ld8(ZRegister.L, ZRegister.A),
        ZLine.ld8(ZRegister.A, ZRegister.H),
        ZLine.imm8(op, const.hiByte),
        ZLine.ld8(ZRegister.H, ZRegister.A)
      )
    }
    result.toList
  }

  def compile8BitSum(ctx: CompilationContext, params: List[(Boolean, Expression)], decimal: Boolean): List[ZLine] = {
    var const = Constant.Zero
    var hasConst = false
    var result = mutable.ListBuffer[ZLine]()
    params.foreach {
      case (false, expr) =>
        ctx.env.eval(expr) match {
          case None =>
            if (result.isEmpty) {
              result ++= Z80ExpressionCompiler.compileToA(ctx, expr)
            } else {
              result += ZLine.ld8(ZRegister.E, ZRegister.A)
              result ++= Z80ExpressionCompiler.stashDEIfChanged(Z80ExpressionCompiler.compileToA(ctx, expr))
              result += ZLine.register(ADD, ZRegister.E)
              if (decimal) {
                result += ZLine.implied(DAA)
              }
            }
          case Some(c) =>
            hasConst = true
            if (decimal) {
              const = CompoundConstant(MathOperator.DecimalPlus, const, c).quickSimplify
            } else {
              const = (const + c).quickSimplify
            }
        }
      case (true, expr) =>
        ctx.env.eval(expr) match {
          case None =>
            if (result.isEmpty) {
              result ++= Z80ExpressionCompiler.compileToA(ctx, expr)
              if (ctx.options.flag(CompilationFlag.EmitExtended80Opcodes)) {
                result += ZLine.implied(NEG)
              } else {
                result += ZLine.implied(CPL)
                result += ZLine.register(INC, ZRegister.A)
              }
            } else {
              // TODO: optimize
              result += ZLine.ld8(ZRegister.D, ZRegister.A)
              result ++= Z80ExpressionCompiler.stashDEIfChanged(Z80ExpressionCompiler.compileToA(ctx, expr))
              result += ZLine.ld8(ZRegister.E, ZRegister.A)
              result += ZLine.ld8(ZRegister.A, ZRegister.D)
              result += ZLine.register(SUB, ZRegister.E)
              if (decimal) {
                result += ZLine.implied(DAA)
              }
            }
          case Some(c) =>
            hasConst = true
            if (decimal) {
              const = CompoundConstant(MathOperator.DecimalMinus, const, c).quickSimplify
            } else {
              const = (const - c).quickSimplify
            }
        }
    }
    if (hasConst) {
      result += ZLine.imm8(AND, const.loByte)
    }
    result.toList
  }

  def compile16BitSum(ctx: CompilationContext, params: List[(Boolean, Expression)], decimal: Boolean): List[ZLine] = {
    var const = Constant.Zero
    var hasConst = false
    var result = mutable.ListBuffer[ZLine]()
    if (decimal) {
      params.foreach {
        case (false, expr) =>
          ctx.env.eval(expr) match {
            case None =>
              if (result.isEmpty) {
                result ++= Z80ExpressionCompiler.compileToHL(ctx, expr)
              } else {
                result += ZLine.ld8(ZRegister.E, ZRegister.L)
                result += ZLine.ld8(ZRegister.D, ZRegister.H)
                result ++= Z80ExpressionCompiler.stashDEIfChanged(Z80ExpressionCompiler.compileToHL(ctx, expr))
                result += ZLine.ld8(ZRegister.A, ZRegister.L)
                result += ZLine.registers(ADD, ZRegister.A, ZRegister.E)
                result += ZLine.implied(DAA)
                result += ZLine.ld8(ZRegister.L, ZRegister.A)
                result += ZLine.ld8(ZRegister.A, ZRegister.H)
                result += ZLine.registers(ADC, ZRegister.A, ZRegister.D)
                result += ZLine.implied(DAA)
                result += ZLine.ld8(ZRegister.H, ZRegister.A)
              }
            case Some(c) =>
              hasConst = true
              const = CompoundConstant(MathOperator.DecimalPlus, const, c).quickSimplify
          }
        case (true, expr) =>
          ctx.env.eval(expr) match {
            case None =>
              ErrorReporting.error("Decimal subtraction not supported on Intel 8080-like CPUs.", expr.position)
            case Some(c) =>
              hasConst = true
              const = CompoundConstant(MathOperator.DecimalMinus, const, c).quickSimplify
          }
      }
    } else {
      params.foreach {
        case (false, expr) =>
          ctx.env.eval(expr) match {
            case None =>
              if (result.isEmpty) {
                result ++= Z80ExpressionCompiler.compileToHL(ctx, expr)
              } else {
                result += ZLine.ld8(ZRegister.E, ZRegister.L)
                result += ZLine.ld8(ZRegister.D, ZRegister.H)
                result ++= Z80ExpressionCompiler.stashDEIfChanged(Z80ExpressionCompiler.compileToHL(ctx, expr))
                result += ZLine.registers(ADD_16, ZRegister.HL, ZRegister.DE)
              }
            case Some(c) =>
              hasConst = true
              const = (const + c).quickSimplify
          }
        case (true, expr) =>
          ctx.env.eval(expr) match {
            case None =>
              if (result.isEmpty) {
                ???
              } else {
                if (ctx.options.flag(CompilationFlag.EmitExtended80Opcodes)) {
                  // TODO: optimize
                  result += ZLine.ld8(ZRegister.D, ZRegister.H)
                  result += ZLine.ld8(ZRegister.E, ZRegister.L)
                  result ++= Z80ExpressionCompiler.stashDEIfChanged(Z80ExpressionCompiler.compileToHL(ctx, expr))
                  result += ZLine.ld8(ZRegister.B, ZRegister.H)
                  result += ZLine.ld8(ZRegister.C, ZRegister.L)
                  result += ZLine.ld8(ZRegister.H, ZRegister.D)
                  result += ZLine.ld8(ZRegister.L, ZRegister.E)
                  /// TODO: carry?
                  result += ZLine.registers(SBC_16, ZRegister.HL, ZRegister.BC)
                } else {
                  ???
                }
              }
            case Some(c) =>
              hasConst = true
              const = (const - c).quickSimplify
          }
      }
    }
    if (hasConst) {
      result ++= List(
        ZLine.ldImm8(ZRegister.D, const.hiByte),
        ZLine.ldImm8(ZRegister.E, const.loByte),
        ZLine.registers(ADD_16, ZRegister.HL, ZRegister.DE)
      )
    }
    result.toList
  }

  def perform8BitInPlace(ctx: CompilationContext, lhs: LhsExpression, rhs: Expression, opcode: ZOpcode.Value, decimal: Boolean = false): List[ZLine] = {
    val calculateAddress = lhs match {
      case VariableExpression(name) =>
        ctx.env.get[Variable](name) match {
          case v: VariableInMemory => List(ZLine.ldImm16(ZRegister.HL, v.toAddress))
          case _ => ???
        }
      case i: IndexedExpression => Z80ExpressionCompiler.calculateAddressToHL(ctx, i)
    }
    val constantRight = ctx.env.eval(rhs)
    val calculateChange = Z80ExpressionCompiler.compileToA(ctx, rhs)
    val setup = (calculateChange.exists(Z80ExpressionCompiler.changesHL), calculateAddress.exists(Z80ExpressionCompiler.changesA)) match {
      case (false, false) => calculateChange ++ calculateAddress
      case (true, false) => calculateChange ++ calculateAddress
      case (false, true) => calculateAddress ++ calculateChange
      case (true, true) => calculateAddress ++ List(ZLine.register(PUSH, ZRegister.HL)) ++ calculateChange ++ List(ZLine.register(POP, ZRegister.HL))
    }
    opcode match {
      case ADD if decimal =>
        setup ++ List(
          ZLine.register(ADD, ZRegister.MEM_HL),
          ZLine.implied(DAA),
          ZLine.ld8(ZRegister.MEM_HL, ZRegister.A))
      case ADD if !decimal =>
        constantRight match {
          case Some(NumericConstant(1, _)) =>
            calculateAddress :+ ZLine.register(INC, ZRegister.MEM_HL)
          case Some(NumericConstant(0xff | -1, _)) =>
            calculateAddress :+ ZLine.register(DEC, ZRegister.MEM_HL)
          case _ =>
            setup ++ List(
              ZLine.register(ADD, ZRegister.MEM_HL),
              ZLine.ld8(ZRegister.MEM_HL, ZRegister.A))
        }
      case SUB if decimal =>
        setup ++ List(
          ZLine.ld8(ZRegister.E, ZRegister.A),
          ZLine.ld8(ZRegister.A, ZRegister.MEM_HL),
          ZLine.register(SUB, ZRegister.E),
          ZLine.implied(DAA),
          ZLine.ld8(ZRegister.MEM_HL, ZRegister.A))
      case SUB if !decimal=>
        constantRight match {
          case Some(NumericConstant(1, _)) =>
            calculateAddress :+ ZLine.register(DEC, ZRegister.MEM_HL)
          case Some(NumericConstant(0xff | -1, _)) =>
            calculateAddress :+ ZLine.register(INC, ZRegister.MEM_HL)
          case _ =>
            if (ctx.options.flag(CompilationFlag.EmitExtended80Opcodes)) {
              setup ++ List(
                ZLine.implied(NEG),
                ZLine.register(ADD, ZRegister.MEM_HL),
                ZLine.ld8(ZRegister.MEM_HL, ZRegister.A))
            } else {
              setup ++ List(
                ZLine.implied(CPL),
                ZLine.register(INC, ZRegister.A),
                ZLine.register(SUB, ZRegister.MEM_HL),
                ZLine.ld8(ZRegister.MEM_HL, ZRegister.A))
            }
        }
      case XOR =>
        constantRight match {
          case Some(NumericConstant(0, _)) =>
            calculateAddress
          case Some(NumericConstant(0xff | -1, _)) =>
            calculateAddress ++ List(
              ZLine.ld8(ZRegister.A, ZRegister.MEM_HL),
              ZLine.implied(CPL),
              ZLine.ld8(ZRegister.MEM_HL, ZRegister.A))
          case _ =>
            setup ++ List(
              ZLine.register(XOR, ZRegister.MEM_HL),
              ZLine.ld8(ZRegister.MEM_HL, ZRegister.A))
        }
      case XOR =>
        constantRight match {
          case Some(NumericConstant(0, _)) =>
            calculateAddress
          case Some(NumericConstant(0xff | -1, _)) =>
            calculateAddress :+ ZLine.ldImm8(ZRegister.MEM_HL, 0xff)
          case _ =>
            setup ++ List(
              ZLine.register(XOR, ZRegister.MEM_HL),
              ZLine.ld8(ZRegister.MEM_HL, ZRegister.A))
        }
      case AND =>
        constantRight match {
          case Some(NumericConstant(0, _)) =>
            calculateAddress :+ ZLine.ldImm8(ZRegister.MEM_HL, 0)
          case Some(NumericConstant(0xff | -1, _)) =>
            calculateAddress
          case _ =>
            setup ++ List(
              ZLine.register(AND, ZRegister.MEM_HL),
              ZLine.ld8(ZRegister.MEM_HL, ZRegister.A))
        }
    }
  }

}
