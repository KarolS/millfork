package millfork.compiler.z80

import millfork.assembly.z80._
import millfork.compiler.CompilationContext
import millfork.node._
import ZOpcode._
import millfork.CompilationFlag
import millfork.env._
import millfork.error.ConsoleLogger

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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
          result ++= Z80ExpressionCompiler.stashDEIfChanged(ctx, Z80ExpressionCompiler.compileToA(ctx, expr))
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
      case AND => NumericConstant(0xffff, 2)
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
          result ++= Z80ExpressionCompiler.stashDEIfChanged(ctx, Z80ExpressionCompiler.compileToHL(ctx, expr))
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
              result ++= Z80ExpressionCompiler.stashDEIfChanged(ctx, Z80ExpressionCompiler.compileToA(ctx, expr))
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
              if (decimal) ???
              result ++= Z80ExpressionCompiler.compileToA(ctx, expr)
              if (ctx.options.flag(CompilationFlag.EmitZ80Opcodes)) {
                result += ZLine.implied(NEG)
              } else {
                result += ZLine.implied(CPL)
                result += ZLine.register(INC, ZRegister.A)
              }
            } else {
              if (!decimal || ctx.options.flag(CompilationFlag.EmitExtended80Opcodes)) {
                // TODO: optimize
                result += ZLine.ld8(ZRegister.D, ZRegister.A)
                result ++= Z80ExpressionCompiler.stashDEIfChanged(ctx, Z80ExpressionCompiler.compileToA(ctx, expr))
                result += ZLine.ld8(ZRegister.E, ZRegister.A)
                result += ZLine.ld8(ZRegister.A, ZRegister.D)
                result += ZLine.register(SUB, ZRegister.E)
                if (decimal) {
                  result += ZLine.implied(DAA)
                }
              } else {
                // -' on Intel 8080
                result += ZLine.ld8(ZRegister.D, ZRegister.A)
                result ++= Z80ExpressionCompiler.stashDEIfChanged(ctx, Z80ExpressionCompiler.compileToA(ctx, expr))
                result += ZLine.ld8(ZRegister.E, ZRegister.A)
                result += ZLine.ldImm8(ZRegister.A, 0x9a)
                result += ZLine.register(SUB, ZRegister.E)
                result += ZLine.register(ADD, ZRegister.D)
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
      const match {
        case CompoundConstant(MathOperator.DecimalMinus | MathOperator.Minus, NumericConstant(0, _), c) =>
          if (!decimal || ctx.options.flag(CompilationFlag.EmitExtended80Opcodes)) {
            result += ZLine.imm8(SUB, c.loByte)
            if (decimal) {
              result += ZLine.implied(DAA)
            }
          } else {
            import ZRegister._
            result += ZLine.ld8(E, A)
            result += ZLine.ldImm8(A, 0x9a)
            result += ZLine.imm8(SUB, c.loByte)
            result += ZLine.register(ADD, E)
            result += ZLine.implied(DAA)
          }
        case NumericConstant(n, _) if n < 0 && !decimal =>
          result += ZLine.imm8(SUB, (-n.toInt) & 0xff)
        case NumericConstant(0, _) =>
          // nothing
        case _ =>
          result += ZLine.imm8(ADD, const.loByte)
          if (decimal) {
            result += ZLine.implied(DAA)
          }
      }
    }
    result.toList
  }

  def compile16BitSum(ctx: CompilationContext, params: List[(Boolean, Expression)], decimal: Boolean): List[ZLine] = {
    var const: Constant = Constant.WordZero
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
                result ++= Z80ExpressionCompiler.stashDEIfChanged(ctx, Z80ExpressionCompiler.compileToHL(ctx, expr))
                result += ZLine.ld8(ZRegister.A, ZRegister.L)
                result += ZLine.register(ADD, ZRegister.E)
                result += ZLine.implied(DAA)
                result += ZLine.ld8(ZRegister.L, ZRegister.A)
                result += ZLine.ld8(ZRegister.A, ZRegister.H)
                result += ZLine.register(ADC, ZRegister.D)
                result += ZLine.implied(DAA)
                result += ZLine.ld8(ZRegister.H, ZRegister.A)
              }
            case Some(c) =>
              hasConst = true
              const = CompoundConstant(MathOperator.DecimalPlus, const, c).quickSimplify
          }
        case (true, expr) =>
          ctx.env.eval(expr) match {
            case Some(c) if hasConst && const.isProvablyGreaterOrEqualThan(c)=>
              const = CompoundConstant(MathOperator.DecimalMinus, const, c).quickSimplify
            case _ =>
              if (result.isEmpty) {
                result += ZLine.ldImm16(ZRegister.HL, const)
                const = Constant.WordZero
                hasConst = false
              }
              if (ctx.options.flag(CompilationFlag.EmitExtended80Opcodes)) {
                result += ZLine.ld8(ZRegister.E, ZRegister.L)
                result += ZLine.ld8(ZRegister.D, ZRegister.H)
                result ++= Z80ExpressionCompiler.stashDEIfChanged(ctx, Z80ExpressionCompiler.compileToHL(ctx, expr))
                result += ZLine.ld8(ZRegister.A, ZRegister.E)
                result += ZLine.register(SUB, ZRegister.L)
                result += ZLine.implied(DAA)
                result += ZLine.ld8(ZRegister.L, ZRegister.A)
                result += ZLine.ld8(ZRegister.A, ZRegister.D)
                result += ZLine.register(SBC, ZRegister.H)
                result += ZLine.implied(DAA)
                result += ZLine.ld8(ZRegister.H, ZRegister.A)
              } else {
                result += ZLine.ld8(ZRegister.E, ZRegister.L)
                result += ZLine.ld8(ZRegister.D, ZRegister.H)
                result ++= Z80ExpressionCompiler.stashDEIfChanged(ctx, Z80ExpressionCompiler.compileToHL(ctx, expr))
                result += ZLine.ldImm8(ZRegister.A, 0x9A)
                result += ZLine.register(SUB, ZRegister.L)
                result += ZLine.register(ADD, ZRegister.E)
                result += ZLine.implied(DAA)
                result += ZLine.ld8(ZRegister.L, ZRegister.A)
                result += ZLine.ldImm8(ZRegister.A, 0x99)
                result += ZLine.imm8(ADC, 0)
                result += ZLine.register(SUB, ZRegister.H)
                result += ZLine.register(ADD, ZRegister.D)
                result += ZLine.implied(DAA)
                result += ZLine.ld8(ZRegister.H, ZRegister.A)
              }
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
                result ++= Z80ExpressionCompiler.stashDEIfChanged(ctx, Z80ExpressionCompiler.compileToHL(ctx, expr))
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
                if (ctx.options.flag(CompilationFlag.EmitZ80Opcodes)) {
                  result ++= Z80ExpressionCompiler.compileToBC(ctx, expr)
                  result += ZLine.ldImm16(ZRegister.HL, 0)
                  result += ZLine.register(OR, ZRegister.A)
                  result += ZLine.registers(SBC_16, ZRegister.HL, ZRegister.BC)
                } else {
                  result ++= Z80ExpressionCompiler.compileToHL(ctx, expr)
                  result += ZLine.register(DEC_16, ZRegister.HL)
                  result += ZLine.ld8(ZRegister.A, ZRegister.H)
                  result += ZLine.implied(CPL)
                  result += ZLine.ld8(ZRegister.H, ZRegister.A)
                  result += ZLine.ld8(ZRegister.A, ZRegister.L)
                  result += ZLine.implied(CPL)
                  result += ZLine.ld8(ZRegister.L, ZRegister.A)
                }
              } else {
                if (ctx.options.flag(CompilationFlag.EmitZ80Opcodes)) {
                  // TODO: optimize
                  result += ZLine.implied(EX_DE_HL)
                  result ++= Z80ExpressionCompiler.stashDEIfChanged(ctx, Z80ExpressionCompiler.compileToHL(ctx, expr))
                  result += ZLine.ld8(ZRegister.B, ZRegister.H)
                  result += ZLine.ld8(ZRegister.C, ZRegister.L)
                  result += ZLine.implied(EX_DE_HL)
                  result += ZLine.register(OR, ZRegister.A)
                  result += ZLine.registers(SBC_16, ZRegister.HL, ZRegister.BC)
                } else if (ctx.options.flag(CompilationFlag.EmitIntel8085Opcodes) && ctx.options.flag(CompilationFlag.EmitIllegals)) {
                  // TODO: optimize
                  result += ZLine.implied(EX_DE_HL)
                  result ++= Z80ExpressionCompiler.stashDEIfChanged(ctx, Z80ExpressionCompiler.compileToHL(ctx, expr))
                  result += ZLine.ld8(ZRegister.B, ZRegister.H)
                  result += ZLine.ld8(ZRegister.C, ZRegister.L)
                  result += ZLine.implied(EX_DE_HL)
                  result += ZLine.implied(DSUB)
                } else {
                  // TODO: optimize
                  result += ZLine.ld8(ZRegister.D, ZRegister.H)
                  result += ZLine.ld8(ZRegister.E, ZRegister.L)
                  result ++= Z80ExpressionCompiler.stashDEIfChanged(ctx, Z80ExpressionCompiler.compileToHL(ctx, expr))
                  result += ZLine.ld8(ZRegister.A, ZRegister.E)
                  result += ZLine.register(SUB, ZRegister.L)
                  result += ZLine.ld8(ZRegister.L, ZRegister.A)
                  result += ZLine.ld8(ZRegister.A, ZRegister.D)
                  result += ZLine.register(SBC, ZRegister.H)
                  result += ZLine.ld8(ZRegister.H, ZRegister.A)
                }
              }
            case Some(c) =>
              hasConst = true
              const = (const - c).quickSimplify
          }
      }
    }
    if (hasConst) {
      if (const.isProvablyZero) {
        // do nothing
      } else if (const.isProvably(1)) {
        result += ZLine.register(INC_16, ZRegister.HL)
      } else if (ctx.options.flag(CompilationFlag.EmitIntel8085Opcodes) && ctx.options.flag(CompilationFlag.EmitIllegals) && const.isProvablyInRange(1, 127)) {
        result ++= List(
          ZLine.imm8(LD_DEHL, const),
          ZLine.implied(EX_DE_HL)
        )
      } else {
        result ++= List(
          ZLine.ldImm16(ZRegister.DE, const),
          ZLine.registers(ADD_16, ZRegister.HL, ZRegister.DE)
        )
      }
    }
    result.toList
  }

  def perform8BitInPlace(ctx: CompilationContext, lhs: LhsExpression, rhs: Expression, opcode: ZOpcode.Value, decimal: Boolean = false): List[ZLine] = {
    val (lv, calculateAddress):(LocalVariableAddressOperand, List[ZLine]) = Z80ExpressionCompiler.calculateAddressToAppropriatePointer(ctx, lhs, forWriting = true).getOrElse{
      ctx.log.error("Invalid left-hand-side expression", lhs.position)
      LocalVariableAddressViaHL -> Nil
    }
    val constantRight = ctx.env.eval(rhs)
    val calculateChange = Z80ExpressionCompiler.compileToA(ctx, rhs)
    val setup = (calculateChange.exists(Z80ExpressionCompiler.changesHL), calculateAddress.exists(Z80ExpressionCompiler.changesA)) match {
      case (false, false) => calculateChange ++ calculateAddress
      case (true, false) => calculateChange ++ calculateAddress
      case (false, true) => calculateAddress ++ calculateChange
      case (true, true) => calculateAddress ++ Z80ExpressionCompiler.stashHLIfChanged(ctx, calculateChange)
    }
    opcode match {
      case ADD if decimal =>
        setup ++ List(
          ZLine.register(ADD, lv),
          ZLine.implied(DAA),
          ZLine.ld8(lv, ZRegister.A))
      case ADD if !decimal =>
        constantRight match {
            // n × INC (HL) = 11n cycles, n bytes
            // n × INC (IX,d) = 23n cycles, 2n bytes
            // LD A,n ; ADD (HL) ; LD (HL),A = 21 cycles, 4 bytes
            // LD A,n ; ADD (IX,d) ; LD (IX,d),A = 45 cycles, 6 bytes
          case Some(NumericConstant(1, _)) =>
            calculateAddress :+ ZLine.register(INC, lv)
          case Some(NumericConstant(2, _)) =>
            calculateAddress :+ ZLine.register(INC, lv) :+ ZLine.register(INC, lv)
          case Some(NumericConstant(3, _)) if ctx.options.flag(CompilationFlag.OptimizeForSpeed) =>
            calculateAddress ++ List(ZLine.register(INC, lv), ZLine.register(INC, lv), ZLine.register(INC, lv))
          case Some(NumericConstant(0xff | -1, _)) =>
            calculateAddress :+ ZLine.register(DEC, lv)
          case Some(NumericConstant(0xfe | -2, _)) =>
            calculateAddress :+ ZLine.register(DEC, lv) :+ ZLine.register(DEC, lv)
          case Some(NumericConstant(0xfd | -3, _)) if ctx.options.flag(CompilationFlag.OptimizeForSpeed) =>
            calculateAddress ++ List(ZLine.register(DEC, lv), ZLine.register(DEC, lv), ZLine.register(DEC, lv))
          case _ =>
            setup ++ List(
              ZLine.register(ADD, lv),
              ZLine.ld8(lv, ZRegister.A))
        }
      case SUB if decimal =>
        if (ctx.options.flag(CompilationFlag.EmitExtended80Opcodes)) {
          setup ++ List(
            ZLine.ld8(ZRegister.E, ZRegister.A),
            ZLine.ld8(ZRegister.A, lv),
            ZLine.register(SUB, ZRegister.E),
            ZLine.implied(DAA),
            ZLine.ld8(lv, ZRegister.A))
        } else {
          setup ++ List(
            ZLine.ld8(ZRegister.E, ZRegister.A),
            ZLine.ld8(ZRegister.D, lv),
            ZLine.ldImm8(ZRegister.A, 0x9a),
            ZLine.register(SUB, ZRegister.E),
            ZLine.register(ADD, ZRegister.D),
            ZLine.implied(DAA),
            ZLine.ld8(lv, ZRegister.A))
        }
      case SUB if !decimal=>
        constantRight match {
          case Some(NumericConstant(1, _)) =>
            calculateAddress :+ ZLine.register(DEC, lv)
          case Some(NumericConstant(2, _)) =>
            calculateAddress :+ ZLine.register(DEC, lv) :+ ZLine.register(INC, lv)
          case Some(NumericConstant(3, _)) if ctx.options.flag(CompilationFlag.OptimizeForSpeed) =>
            calculateAddress ++ List(ZLine.register(DEC, lv), ZLine.register(INC, lv), ZLine.register(INC, lv))
          case Some(NumericConstant(0xff | -1, _)) =>
            calculateAddress :+ ZLine.register(INC, lv)
          case Some(NumericConstant(0xfe | -2, _)) =>
            calculateAddress :+ ZLine.register(INC, lv) :+ ZLine.register(INC, lv)
          case Some(NumericConstant(0xfd | -3, _)) if ctx.options.flag(CompilationFlag.OptimizeForSpeed) =>
            calculateAddress ++ List(ZLine.register(INC, lv), ZLine.register(INC, lv), ZLine.register(INC, lv))
          case _ =>
            if (ctx.options.flag(CompilationFlag.EmitZ80Opcodes)) {
              setup ++ List(
                ZLine.implied(NEG),
                ZLine.register(ADD, lv),
                ZLine.ld8(lv, ZRegister.A))
            } else {
              setup ++ List(
                ZLine.implied(CPL),
                ZLine.register(INC, ZRegister.A),
                ZLine.register(ADD, lv),
                ZLine.ld8(lv, ZRegister.A))
            }
        }
      case XOR =>
        constantRight match {
          case Some(NumericConstant(0, _)) =>
            calculateAddress
          case Some(NumericConstant(0xff | -1, _)) =>
            calculateAddress ++ List(
              ZLine.ld8(ZRegister.A, lv),
              ZLine.implied(CPL),
              ZLine.ld8(lv, ZRegister.A))
          case _ =>
            setup ++ List(
              ZLine.register(XOR, lv),
              ZLine.ld8(lv, ZRegister.A))
        }
      case OR =>
        constantRight match {
          case Some(NumericConstant(0, _)) =>
            calculateAddress
          case Some(NumericConstant(0xff | -1, _)) =>
            calculateAddress :+ ZLine.ldImm8(lv, 0xff)
          case Some(NumericConstant(n, _)) if n.&(n - 1) == 0 && ctx.options.flag(CompilationFlag.EmitExtended80Opcodes) =>
            calculateAddress :+ ZLine.register(ZOpcodeClasses.SET_seq(Integer.numberOfTrailingZeros(n.toInt)), lv)
          case _ =>
            setup ++ List(
              ZLine.register(OR, lv),
              ZLine.ld8(lv, ZRegister.A))
        }
      case AND =>
        constantRight match {
          case Some(NumericConstant(0, _)) =>
            calculateAddress :+ ZLine.ldImm8(lv, 0)
          case Some(NumericConstant(0xff | -1, _)) =>
            calculateAddress
          case Some(NumericConstant(n, _)) if n.^(0xff).&(n.^(0xff) - 1) == 0 && ctx.options.flag(CompilationFlag.EmitExtended80Opcodes) =>
            calculateAddress :+ ZLine.register(ZOpcodeClasses.RES_seq(Integer.numberOfTrailingZeros(n.^(0xff).toInt)), lv)
          case _ =>
            setup ++ List(
              ZLine.register(AND, lv),
              ZLine.ld8(lv, ZRegister.A))
        }
    }
  }


  def performLongInPlace(ctx: CompilationContext, lhs: LhsExpression, rhs: Expression, opcodeFirst: ZOpcode.Value, opcodeLater: ZOpcode.Value, size: Int, decimal: Boolean = false): List[ZLine] = {
    lhs match {
      case dx@DerefExpression(inner, offset, targetType) =>
        val env = ctx.env
        if (targetType.size == 2) {
          if (opcodeFirst == ADD && !decimal && ctx.options.flag(CompilationFlag.EmitZ80Opcodes)) {
            val l = Z80ExpressionCompiler.compileToBC(ctx, inner #+# offset)
            val r = Z80ExpressionCompiler.compileToDE(ctx, rhs)
            val s = List(
              ZLine.ld8(ZRegister.A, ZRegister.MEM_BC),
              ZLine.ld8(ZRegister.L, ZRegister.A),
              ZLine.register(INC_16, ZRegister.BC),
              ZLine.ld8(ZRegister.A, ZRegister.MEM_BC),
              ZLine.ld8(ZRegister.H, ZRegister.A),
              ZLine.registers(ADD_16, ZRegister.HL, ZRegister.DE),
              ZLine.ld8(ZRegister.A, ZRegister.H),
              ZLine.ld8(ZRegister.MEM_BC, ZRegister.A),
              ZLine.register(DEC_16, ZRegister.BC),
              ZLine.ld8(ZRegister.A, ZRegister.L),
              ZLine.ld8(ZRegister.MEM_BC, ZRegister.A),
            )
            if (!r.exists(Z80ExpressionCompiler.changesBC)) {
              return l ++ r ++ s
            } else if (!l.exists(Z80ExpressionCompiler.changesDE)) {
              return r ++ l ++ s
            } else {
              return l ++ Z80ExpressionCompiler.stashBCIfChanged(ctx, r) ++ s
            }
          } else {
            val l = Z80ExpressionCompiler.compileDerefPointer(ctx, dx)
            val r = Z80ExpressionCompiler.compileToDE(ctx, rhs)
            val s = if (opcodeFirst == SUB && decimal) {
              if (ctx.options.flag(CompilationFlag.EmitExtended80Opcodes)) {
                List(
                  ZLine.ld8(ZRegister.A, ZRegister.MEM_HL),
                  ZLine.register(SUB, ZRegister.E),
                  ZLine.implied(DAA),
                  ZLine.ld8(ZRegister.MEM_HL, ZRegister.A),
                  ZLine.register(INC_16, ZRegister.HL),
                  ZLine.ld8(ZRegister.A, ZRegister.MEM_HL),
                  ZLine.register(SBC, ZRegister.D),
                  ZLine.implied(DAA),
                  ZLine.ld8(ZRegister.MEM_HL, ZRegister.A),
                )
              } else {
                ctx.log.error("Decimal subtraction from such a complex LHS is not yet supported", dx.position)
                Nil
              }
            } else if (opcodeFirst == ADD && decimal) {
              List(
                ZLine.ld8(ZRegister.A, ZRegister.MEM_HL),
                ZLine.register(ADD, ZRegister.E),
                ZLine.implied(DAA),
                ZLine.ld8(ZRegister.MEM_HL, ZRegister.A),
                ZLine.register(INC_16, ZRegister.HL),
                ZLine.ld8(ZRegister.A, ZRegister.MEM_HL),
                ZLine.register(ADC, ZRegister.D),
                ZLine.implied(DAA),
                ZLine.ld8(ZRegister.MEM_HL, ZRegister.A),
              )
            } else {
              List(
                ZLine.ld8(ZRegister.A, ZRegister.MEM_HL),
                ZLine.register(opcodeFirst, ZRegister.E),
                ZLine.ld8(ZRegister.MEM_HL, ZRegister.A),
                ZLine.register(INC_16, ZRegister.HL),
                ZLine.ld8(ZRegister.A, ZRegister.MEM_HL),
                ZLine.register(opcodeLater, ZRegister.D),
                ZLine.ld8(ZRegister.MEM_HL, ZRegister.A),
              )
            }
            if (!r.exists(Z80ExpressionCompiler.changesHL)) {
              return l ++ r ++ s
            } else if (!l.exists(Z80ExpressionCompiler.changesDE)) {
              return r ++ l ++ s
            } else {
              return l ++ Z80ExpressionCompiler.stashHLIfChanged(ctx, r) ++ s
            }
          }
        } else {
          val target = Z80ExpressionCompiler.compileToHL(ctx, inner #+# offset)
          val sourceBytes = Z80ExpressionCompiler.compileByteReads(ctx, rhs, size, ZExpressionTarget.BC).map(l => Z80ExpressionCompiler.stashHLIfChanged(ctx, l))
          val result = ListBuffer[ZLine]()
          result ++= target
          if (opcodeFirst == SUB && decimal) {
            ???
          } else if (opcodeFirst == SUB && !decimal) {
            for (i <- 0 until size) {
              if (i != 0) result += ZLine.register(PUSH, ZRegister.AF)
              result ++= sourceBytes(i)
              result += ZLine.ld8(ZRegister.E, ZRegister.A)
              if (i != 0) result += ZLine.register(POP, ZRegister.AF)
              result += ZLine.ld8(ZRegister.A, ZRegister.MEM_HL)
              result += ZLine.register(if (i == 0) opcodeFirst else opcodeLater, ZRegister.E)
              result += ZLine.ld8(ZRegister.MEM_HL, ZRegister.A)
              if (i != size - 1) result += ZLine.register(INC_16, ZRegister.HL)
            }
          } else {
            for (i <- 0 until size) {
              result ++= sourceBytes(i)
              result += ZLine.register(if (i==0) opcodeFirst else opcodeLater, ZRegister.MEM_HL)
              if (decimal) result += ZLine.implied(DAA)
              result += ZLine.ld8(ZRegister.MEM_HL, ZRegister.A)
              if (i != size -1) result += ZLine.register(INC_16, ZRegister.HL)
            }
          }
          return result.toList
//          ctx.log.error("Too complex left-hand-side expression", lhs.position)
//          return Z80ExpressionCompiler.compile(ctx, lhs, ZExpressionTarget.NOTHING) ++ Z80ExpressionCompiler.compile(ctx, rhs, ZExpressionTarget.NOTHING)
        }
      case _ =>
    }
    if (size == 2 && !decimal) {
      // n × INC HL
      // 6n cycles, n bytes

      // LD A,L ; ADD A,n ; LD L,A ; LD H,A ; ADC A,m ; LD H,A
      // 30 cycles, 8 bytes

      // LD A,L ; ADD A,n ; LD L,A ; JR NC,... ; INC H
      // 27 bytes, 7 bytes

      // LD DE,nn ; ADD HL,DE
      // 21 cycles, 4 bytes
      val maxForInc = 3
      if (opcodeFirst == ZOpcode.ADD) {
        ctx.env.eval(rhs) match {
          case Some(NumericConstant(0, _)) =>
            return Z80ExpressionCompiler.compileToHL(ctx, lhs)
          case Some(NumericConstant(n, _)) if n > 0 && n <= maxForInc =>
            return Z80ExpressionCompiler.compileToHL(ctx, lhs) ++
              List.fill(n.toInt)(ZLine.register(INC_16, ZRegister.HL)) ++
              Z80ExpressionCompiler.storeHL(ctx, lhs, signedSource = false)
          case Some(NumericConstant(n, _)) if n < 0 && n >= -maxForInc =>
            return Z80ExpressionCompiler.compileToHL(ctx, lhs) ++
              List.fill(-n.toInt)(ZLine.register(DEC_16, ZRegister.HL)) ++
              Z80ExpressionCompiler.storeHL(ctx, lhs, signedSource = false)
          case _ =>
            val loadRight = Z80ExpressionCompiler.compileToDE(ctx, rhs)
            val loadLeft = Z80ExpressionCompiler.stashDEIfChanged(ctx, Z80ExpressionCompiler.compileToHL(ctx, lhs))
            val calculateAndStore = ZLine.registers(ADD_16, ZRegister.HL, ZRegister.DE) :: Z80ExpressionCompiler.storeHL(ctx, lhs, signedSource = false)
            return loadRight ++ loadLeft ++ calculateAndStore
        }
      }
      if (opcodeFirst == ZOpcode.SUB) {
        ctx.env.eval(rhs) match {
          case Some(NumericConstant(0, _)) =>
            return Z80ExpressionCompiler.compileToHL(ctx, lhs)
          case Some(NumericConstant(n, _)) if n > 0 && n <= maxForInc =>
            return Z80ExpressionCompiler.compileToHL(ctx, lhs) ++
              List.fill(n.toInt)(ZLine.register(DEC_16, ZRegister.HL)) ++
              Z80ExpressionCompiler.storeHL(ctx, lhs, signedSource = false)
          case Some(NumericConstant(n, _)) if n < 0 && n >= -maxForInc =>
            return Z80ExpressionCompiler.compileToHL(ctx, lhs) ++
              List.fill(-n.toInt)(ZLine.register(INC_16, ZRegister.HL)) ++
              Z80ExpressionCompiler.storeHL(ctx, lhs, signedSource = false)
          case _ =>
            if (ctx.options.flag(CompilationFlag.EmitZ80Opcodes)) {
              val loadRight = Z80ExpressionCompiler.compileToDE(ctx, rhs)
              val loadLeft = Z80ExpressionCompiler.stashDEIfChanged(ctx, Z80ExpressionCompiler.compileToHL(ctx, lhs))
              // OR A clears carry before SBC
              val calculateAndStore = List(
                ZLine.register(OR, ZRegister.A),
                ZLine.registers(SBC_16, ZRegister.HL, ZRegister.DE)) ++
                Z80ExpressionCompiler.storeHL(ctx, lhs, signedSource = false)
              return loadRight ++ loadLeft ++ calculateAndStore
            }
        }
      }
    }
    val store = Z80ExpressionCompiler.compileByteStores(ctx, lhs, size, includeStep = false)
    val loadRight = Z80ExpressionCompiler.compileByteReads(ctx, rhs, size, ZExpressionTarget.BC)
    val rightIsBig = loadRight.exists(_.exists(l => l.changesRegister(ZRegister.HL) || l.changesRegister(ZRegister.DE)))
    // TODO: don't evaluate LHS twice
    val (leftStasher, calcStasher, loadLeft) = if (rightIsBig) {
      (
        ((e: List[ZLine]) => Z80ExpressionCompiler.stashHLIfChanged(ctx, e)),
        ((e: List[ZLine]) => Z80ExpressionCompiler.stashDEIfChanged(ctx, e)),
        Z80ExpressionCompiler.compileByteReads(ctx, lhs, size, ZExpressionTarget.BC)
      )
    } else {
      (
        ((e: List[ZLine]) => Z80ExpressionCompiler.stashBCIfChanged(ctx, e)),
        ((e: List[ZLine]) => e),
        Z80ExpressionCompiler.compileByteReads(ctx, lhs, size, ZExpressionTarget.HL)
      )
    }
    List.tabulate(size) { i =>
      import ZRegister._
      // TODO: stash things correctly?
      if (opcodeFirst == ZOpcode.SUB && decimal && !ctx.options.flag(CompilationFlag.EmitExtended80Opcodes)) {
        // TODO: check if carry is ok
        if (i == 0) {
          loadRight(i) ++ calcStasher(List(ZLine.ld8(ZRegister.E, ZRegister.A)) ++
            (leftStasher(loadLeft(i)) ++ List(
              ZLine.ld8(D, A),
              ZLine.ldImm8(A, 0x9A),
              ZLine.register(SUB, E),
              ZLine.register(ADD, D),
              ZLine.implied(DAA)) ++ store(i)))
        } else {
          loadRight(i) ++ calcStasher(List(ZLine.ld8(ZRegister.E, ZRegister.A)) ++
            (leftStasher(loadLeft(i)) ++ List(
              ZLine.ld8(D, A),
              ZLine.ldImm8(A, 0x99),
              ZLine.imm8(ADC, 0),
              ZLine.register(SUB, E),
              ZLine.register(ADD, D),
              ZLine.implied(DAA)) ++ store(i)))
        }
      } else {
        val firstPhase = loadRight(i) ++ calcStasher(List(ZLine.ld8(ZRegister.E, ZRegister.A)) ++
          (leftStasher(loadLeft(i)) :+ ZLine.register(if (i == 0) opcodeFirst else opcodeLater, ZRegister.E)))
        val secondPhase = if (decimal) firstPhase :+ ZLine.implied(ZOpcode.DAA) else firstPhase
        secondPhase ++ store(i)
      }
    }.flatten
  }
}
