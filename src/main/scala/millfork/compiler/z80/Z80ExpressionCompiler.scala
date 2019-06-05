package millfork.compiler.z80

import millfork.CompilationFlag
import millfork.assembly.Elidability
import millfork.assembly.z80._
import millfork.compiler._
import millfork.env._
import millfork.node.{ZRegister, _}
import millfork.assembly.z80.ZOpcode._
import millfork.error.ConsoleLogger
import millfork.output.{NoAlignment, WithinPageAlignment}

/**
  * @author Karol Stasiak
  */
object ZExpressionTarget extends Enumeration {
  val A, HL, BC, DE, EHL, DEHL, NOTHING = Value
}

object Z80ExpressionCompiler extends AbstractExpressionCompiler[ZLine] {

  def compileToA(ctx: CompilationContext, expression: Expression): List[ZLine] = compile(ctx, expression, ZExpressionTarget.A)

  def compile8BitTo(ctx: CompilationContext, expression: Expression, register: ZRegister.Value): List[ZLine] = {
    if (ZRegister.A == register) compileToA(ctx, expression) else {
      val toA = compileToA(ctx, expression)
      if (toA.isEmpty) Nil else {
        toA.last match {
          case ZLine0(ZOpcode.LD, TwoRegisters(ZRegister.A, source), _) if source == register =>
            toA.init
          case ZLine0(ZOpcode.LD, TwoRegisters(ZRegister.A, source@(ZRegister.B | ZRegister.C | ZRegister.D | ZRegister.E | ZRegister.MEM_HL)), _) =>
            toA.init :+ ZLine.ld8(register, source)
          case ZLine0(ZOpcode.LD, TwoRegistersOffset(ZRegister.A, ZRegister.MEM_IX_D, offset), _) =>
            toA.init :+ ZLine.ldViaIx(register, offset)
          case ZLine0(ZOpcode.LD, TwoRegistersOffset(ZRegister.A, ZRegister.MEM_IY_D, offset), _) =>
            toA.init :+ ZLine.ldViaIy(register, offset)
          case _ =>
            toA :+ ZLine.ld8(register, ZRegister.A)
        }
      }
    }
  }

  def compileToHL(ctx: CompilationContext, expression: Expression): List[ZLine] = compile(ctx, expression, ZExpressionTarget.HL)

  def compileDerefPointer(ctx: CompilationContext, expression: DerefExpression): List[ZLine] = {
    compileToHL(ctx, expression.inner) ++ (expression.offset match {
      case 0 => Nil
      case i if i < 5 => List.fill(i)(ZLine.register(INC_16, ZRegister.HL)) // TODO: a better threshold
      case _ => List(ZLine.ldImm8(ZRegister.C, expression.offset), ZLine.ldImm8(ZRegister.B, 0), ZLine.registers(ADD_16, ZRegister.HL, ZRegister.BC))
    })
  }

  def compileToEHL(ctx: CompilationContext, expression: Expression): List[ZLine] = compile(ctx, expression, ZExpressionTarget.EHL)

  def compileToDEHL(ctx: CompilationContext, expression: Expression): List[ZLine] = compile(ctx, expression, ZExpressionTarget.DEHL)

  def compileToBC(ctx: CompilationContext, expression: Expression): List[ZLine] = compile(ctx, expression, ZExpressionTarget.BC)

  def compileToDE(ctx: CompilationContext, expression: Expression): List[ZLine] = compile(ctx, expression, ZExpressionTarget.DE)

  def changesBC(line: ZLine): Boolean = {
    import ZRegister._
    if (ZOpcodeClasses.ChangesBCAlways(line.opcode)) return true
    if (ZOpcodeClasses.ChangesFirstRegister(line.opcode)) return line.registers match {
      case TwoRegisters(B | C | BC, _) => true
      case _ => false
    }
    if (ZOpcodeClasses.ChangesOnlyRegister(line.opcode)) return line.registers match {
      case OneRegister(B | C | BC) => true
      case _ => false
    }
    false
  }

  def changesAF(line: ZLine): Boolean = {
    import ZRegister._
    if (ZOpcodeClasses.ChangesAFAlways(line.opcode)) return true
    if (ZOpcodeClasses.ChangesFirstRegister(line.opcode)) return line.registers match {
      case TwoRegisters(A, _) => true
      case _ => false
    }
    if (ZOpcodeClasses.ChangesOnlyRegister(line.opcode)) return line.registers match {
      case OneRegister(A) => true
      case _ => false
    }
    false
  }

  def changesF(line: ZLine): Boolean = {
    // TODO
    ZOpcodeClasses.ChangesAFAlways(line.opcode)
  }

  def changesDE(line: ZLine): Boolean = {
    import ZRegister._
    if (ZOpcodeClasses.ChangesDEAlways(line.opcode)) return true
    if (ZOpcodeClasses.ChangesFirstRegister(line.opcode)) return line.registers match {
      case TwoRegisters(D | E | DE, _) => true
      case _ => false
    }
    if (ZOpcodeClasses.ChangesOnlyRegister(line.opcode)) return line.registers match {
      case OneRegister(D | E | DE) => true
      case _ => false
    }
    false
  }

  def changesHL(line: ZLine): Boolean = {
    import ZRegister._
    if (ZOpcodeClasses.ChangesHLAlways(line.opcode)) return true
    if (ZOpcodeClasses.ChangesFirstRegister(line.opcode)) return line.registers match {
      case TwoRegisters(H | L | HL, _) => true
      case _ => false
    }
    if (ZOpcodeClasses.ChangesOnlyRegister(line.opcode)) return line.registers match {
      case OneRegister(H | L | HL) => true
      case _ => false
    }
    false
  }

  def changesA(line: ZLine): Boolean = {
    import ZRegister._
    if (ZOpcodeClasses.ChangesAAlways(line.opcode)) return true
    if (ZOpcodeClasses.ChangesFirstRegister(line.opcode)) return line.registers match {
      case TwoRegisters(A | AF, _) => true
      case _ => false
    }
    if (ZOpcodeClasses.ChangesOnlyRegister(line.opcode)) return line.registers match {
      case OneRegister(A | AF) => true
      case _ => false
    }
    false
  }

  def fixTsx(ctx: CompilationContext, code: List[ZLine]): List[ZLine] = code match {
    case (ldhlsp@ZLine0(LD_HLSP, _, param)) :: xs => ldhlsp.copy(parameter = param + 2) :: fixTsx(ctx, xs)
    case (ldhl@ZLine0(LD_16, TwoRegisters(ZRegister.HL, ZRegister.IMM_16), param)) ::
      (addhlsp@ZLine0(ADD_16, TwoRegisters(ZRegister.HL, ZRegister.SP), _)) ::
      (ldsphl@ZLine0(LD_16, TwoRegisters(ZRegister.SP, ZRegister.HL), _)) ::
      xs => ??? // TODO: ldhl :: addhlsp :: ldsphl :: fixTsx(ctx, xs)
    case (ldhl@ZLine0(LD_16, TwoRegisters(ZRegister.HL, ZRegister.IMM_16), param)) ::
      (addhlsp@ZLine0(ADD_16, TwoRegisters(ZRegister.HL, ZRegister.SP), _)) ::
      xs => ldhl.copy(parameter = param + 2) :: addhlsp :: fixTsx(ctx, xs)
    case (x@ZLine0(EX_SP, _, _)) :: xs =>
      // EX_SP should be only generated by the optimizer
      ctx.log.warn("Stray EX (SP) encountered, generated code might be invalid")
      x :: fixTsx(ctx, xs)
    case x :: xs => x :: fixTsx(ctx, xs)
    case Nil => Nil
  }

  def stashAFIfChanged(ctx: CompilationContext, lines: List[ZLine]): List[ZLine] = if (lines.exists(changesAF))
    ZLine.register(PUSH, ZRegister.AF) :: (fixTsx(ctx, lines) :+ ZLine.register(POP, ZRegister.AF)) else lines

  def stashAFIfChangedF(ctx: CompilationContext, lines: List[ZLine]): List[ZLine] = if (lines.exists(changesF))
    ZLine.register(PUSH, ZRegister.AF) :: (fixTsx(ctx, lines) :+ ZLine.register(POP, ZRegister.AF)) else lines

  def stashBCIfChanged(ctx: CompilationContext, lines: List[ZLine]): List[ZLine] = if (lines.exists(changesBC))
    ZLine.register(PUSH, ZRegister.BC) :: (fixTsx(ctx, lines) :+ ZLine.register(POP, ZRegister.BC)) else lines

  def stashDEIfChanged(ctx: CompilationContext, lines: List[ZLine]): List[ZLine] = if (lines.exists(changesDE))
    ZLine.register(PUSH, ZRegister.DE) :: (fixTsx(ctx, lines) :+ ZLine.register(POP, ZRegister.DE)) else lines

  def stashHLIfChanged(ctx: CompilationContext, lines: List[ZLine]): List[ZLine] = if (lines.exists(changesHL))
    ZLine.register(PUSH, ZRegister.HL) :: (fixTsx(ctx, lines) :+ ZLine.register(POP, ZRegister.HL)) else lines

  def targetifyA(ctx: CompilationContext, target: ZExpressionTarget.Value, lines: List[ZLine], isSigned: Boolean): List[ZLine] = {
    def toWord(h: ZRegister.Value, l: ZRegister.Value) = {
      lines ++ (if (isSigned) {
        val label = ctx.nextLabel("sx")
        if (ctx.options.flag(CompilationFlag.EmitIntel8080Opcodes)) {
          List(
            ZLine.ld8(l, ZRegister.A),
            ZLine.ldImm8(h, 0xff),
            ZLine.imm8(OR, 0x7f),
            ZLine.jump(label, IfFlagSet(ZFlag.S)),
            ZLine.ldImm8(h, 0),
            ZLine.label(label))
        } else if (ctx.options.flag(CompilationFlag.EmitExtended80Opcodes)) {
          // TODO: is this optimal?
          List(
            ZLine.ld8(l, ZRegister.A),
            ZLine.ldImm8(h, 0xff),
            ZLine.register(BIT7, ZRegister.A),
            ZLine.jump(label, IfFlagClear(ZFlag.Z)),
            ZLine.ldImm8(h, 0),
            ZLine.label(label))
        } else {
          throw new IllegalStateException()
        }
      } else {
        List(
          ZLine.ld8(l, ZRegister.A),
          ZLine.ldImm8(h, 0))
      })
    }

    target match {
      case ZExpressionTarget.NOTHING | ZExpressionTarget.A => lines
      case ZExpressionTarget.HL => toWord(ZRegister.H, ZRegister.L)
      case ZExpressionTarget.BC => toWord(ZRegister.B, ZRegister.C)
      case ZExpressionTarget.DE => toWord(ZRegister.D, ZRegister.E)
      case ZExpressionTarget.EHL => toWord(ZRegister.H, ZRegister.L) ++ List(ZLine.ldImm8(ZRegister.E, 0))
      case ZExpressionTarget.DEHL => toWord(ZRegister.H, ZRegister.L) ++ List(ZLine.ldImm16(ZRegister.DE, 0))
    }
  }

  def targetifyHL(ctx: CompilationContext, target: ZExpressionTarget.Value, lines: List[ZLine]): List[ZLine] = target match {
    case ZExpressionTarget.NOTHING | ZExpressionTarget.HL => lines
    case ZExpressionTarget.A => lines :+ ZLine.ld8(ZRegister.A, ZRegister.L)
    case ZExpressionTarget.BC => lines ++ List(ZLine.ld8(ZRegister.C, ZRegister.L), ZLine.ld8(ZRegister.B, ZRegister.H))
    case ZExpressionTarget.DE => lines ++ List(ZLine.ld8(ZRegister.E, ZRegister.L), ZLine.ld8(ZRegister.D, ZRegister.H))
    case ZExpressionTarget.EHL => lines ++ List(ZLine.ldImm8(ZRegister.E, 0))
    case ZExpressionTarget.DEHL => lines ++ List(ZLine.ldImm16(ZRegister.DE, 0))
  }

  def targetifyEHL(ctx: CompilationContext, target: ZExpressionTarget.Value, lines: List[ZLine]): List[ZLine] = target match {
    case ZExpressionTarget.NOTHING | ZExpressionTarget.EHL => lines
    case ZExpressionTarget.DEHL => lines ++ List(ZLine.ldImm8(ZRegister.D, 0))
  }

  def targetifyDEHL(ctx: CompilationContext, target: ZExpressionTarget.Value, lines: List[ZLine]): List[ZLine] = target match {
    case ZExpressionTarget.NOTHING | ZExpressionTarget.DEHL => lines
  }

  def compile(ctx: CompilationContext, expression: Expression, target: ZExpressionTarget.Value, branches: BranchSpec = BranchSpec.None): List[ZLine] = {
    val env = ctx.env
    val b = env.get[Type]("byte")
    val w = env.get[Type]("word")
    env.eval(expression) match {
      case Some(const) =>
        target match {
          case ZExpressionTarget.A =>
            List(ZLine.ldImm8(ZRegister.A, const))
          case ZExpressionTarget.HL =>
            List(ZLine.ldImm16(ZRegister.HL, const))
          case ZExpressionTarget.BC =>
            List(ZLine.ldImm16(ZRegister.BC, const))
          case ZExpressionTarget.DE =>
            List(ZLine.ldImm16(ZRegister.DE, const))
          case ZExpressionTarget.EHL =>
            List(ZLine.ldImm16(ZRegister.HL, const.subword(0)), ZLine.ldImm8(ZRegister.E, const.subbyte(2)))
          case ZExpressionTarget.DEHL =>
            List(ZLine.ldImm16(ZRegister.HL, const.subword(0)), ZLine.ldImm16(ZRegister.DE, const.subword(2)))
          case ZExpressionTarget.NOTHING =>
            Nil // TODO
        }
      case None =>
        expression match {
          case LiteralExpression(value, _) => ???
          case GeneratedConstantExpression(_, _) => ???
          case VariableExpression(name) =>
            env.get[Variable](name) match {
              case v: VariableInMemory =>
                import ZRegister._
                v.typ.size match {
                  case 0 => ???
                  case 1 => loadByte(v.toAddress, target, v.isVolatile)
                  case 2 => target match {
                    case ZExpressionTarget.NOTHING => Nil
                    case ZExpressionTarget.HL =>
                      if (ctx.options.flag(CompilationFlag.EmitIntel8080Opcodes)) {
                        List(ZLine.ldAbs16(HL, v))
                      } else {
                        // TODO: is it optimal?
                        List(ZLine.ldAbs8(A, v), ZLine.ld8(L, A), ZLine.ldAbs8(A, v, 1), ZLine.ld8(H, A))
                      }
                    case ZExpressionTarget.EHL =>
                      // TODO: signed words
                      if (ctx.options.flag(CompilationFlag.EmitIntel8080Opcodes)) {
                        List(ZLine.ldAbs16(HL, v), ZLine.ldImm8(E, 0))
                      } else {
                        // TODO: is it optimal?
                        List(ZLine.ldAbs8(A, v), ZLine.ld8(L, A), ZLine.ldAbs8(A, v, 1), ZLine.ld8(H, A), ZLine.ldImm8(E, 0))
                      }
                    case ZExpressionTarget.DEHL =>
                      // TODO: signed words
                      if (ctx.options.flag(CompilationFlag.EmitIntel8080Opcodes)) {
                        List(ZLine.ldAbs16(HL, v), ZLine.ldImm16(DE, 0))
                      } else {
                        // TODO: is it optimal?
                        List(ZLine.ldAbs8(A, v), ZLine.ld8(L, A), ZLine.ldAbs8(A, v, 1), ZLine.ld8(H, A), ZLine.ldImm16(DE, 0))
                      }
                    case ZExpressionTarget.BC =>
                      if (ctx.options.flag(CompilationFlag.EmitZ80Opcodes)) {
                        List(ZLine.ldAbs16(BC, v))
                      } else if (ctx.options.flag(CompilationFlag.EmitIntel8080Opcodes)) {
                        List(ZLine.ldAbs16(HL, v), ZLine.ld8(B, H), ZLine.ld8(C, L))
                      } else {
                        // TODO: is it optimal?
                        List(ZLine.ldAbs8(A, v), ZLine.ld8(C, A), ZLine.ldAbs8(A, v, 1), ZLine.ld8(B, A))
                      }
                    case ZExpressionTarget.DE =>
                      if (ctx.options.flag(CompilationFlag.EmitZ80Opcodes)) {
                        List(ZLine.ldAbs16(DE, v))
                      } else if (ctx.options.flag(CompilationFlag.EmitIntel8080Opcodes)) {
                        List(ZLine.ldAbs16(HL, v), ZLine.ld8(D, H), ZLine.ld8(E, L))
                      } else {
                        // TODO: is it optimal?
                        List(ZLine.ldAbs8(A, v), ZLine.ld8(E, A), ZLine.ldAbs8(A, v, 1), ZLine.ld8(D, A))
                      }
                  }
                  case 3 => target match {
                    case ZExpressionTarget.NOTHING => Nil
                    case ZExpressionTarget.EHL =>
                      if (ctx.options.flag(CompilationFlag.EmitIntel8080Opcodes)) {
                        List(ZLine.ldAbs16(HL, v), ZLine.ldAbs8(A, v, 2), ZLine.ld8(E, A))
                      } else {
                        // TODO: is it optimal?
                        List(
                          ZLine.ldAbs8(A, v), ZLine.ld8(L, A),
                          ZLine.ldAbs8(A, v, 1), ZLine.ld8(H, A),
                          ZLine.ldAbs8(A, v, 2), ZLine.ld8(E, A))
                      }
                    case ZExpressionTarget.DEHL =>
                      // TODO: signed farwords
                      if (ctx.options.flag(CompilationFlag.EmitIntel8080Opcodes)) {
                        List(ZLine.ldAbs16(HL, v), ZLine.ldAbs8(A, v, 2), ZLine.ld8(E, A), ZLine.ldImm8(D, 0))
                      } else {
                        // TODO: is it optimal?
                        List(
                          ZLine.ldAbs8(A, v), ZLine.ld8(L, A),
                          ZLine.ldAbs8(A, v, 1), ZLine.ld8(H, A),
                          ZLine.ldAbs8(A, v, 2), ZLine.ld8(E, A), ZLine.ldImm8(D, 0))
                      }
                  }
                  case 4 => target match {
                    case ZExpressionTarget.NOTHING => Nil
                    case ZExpressionTarget.DEHL =>
                      // TODO: signed farwords
                      if (ctx.options.flag(CompilationFlag.EmitZ80Opcodes)) {
                        List(ZLine.ldAbs16(HL, v), ZLine.ldAbs16(DE, v.toAddress + 2))
                      } else if (ctx.options.flag(CompilationFlag.EmitIntel8080Opcodes)) {
                        // The optimizer might spit out an EX DE,HL
                        List(ZLine.ldAbs16(HL, v.toAddress + 2), ZLine.ld8(D,H), ZLine.ld8(E,L),ZLine.ldAbs16(HL, v))
                      } else {
                        // TODO: is it optimal?
                        List(
                          ZLine.ldAbs8(A, v), ZLine.ld8(L, A),
                          ZLine.ldAbs8(A, v, 1), ZLine.ld8(H, A),
                          ZLine.ldAbs8(A, v, 2), ZLine.ld8(E, A),
                          ZLine.ldAbs8(A, v, 3), ZLine.ld8(D, A))
                      }
                  }
                }
              case v: StackVariable =>
                import ZRegister._
                if (ctx.options.flag(CompilationFlag.UseIxForStack) || ctx.options.flag(CompilationFlag.UseIyForStack)) {
                  val x = ctx.options.flag(CompilationFlag.UseIxForStack)
                  v.typ.size match {
                    case 0 => ???
                    case 1 =>
                      if (x) loadByteViaIX(v.baseOffset, target)
                      else loadByteViaIY(v.baseOffset, target)
                    case 2 => target match {
                      // TODO: signed words
                      case ZExpressionTarget.NOTHING => Nil
                      case ZExpressionTarget.HL =>
                        List(ZLine.ldViaIxy(x, ZRegister.L, v.baseOffset), ZLine.ldViaIxy(x, ZRegister.H, v.baseOffset + 1))
                      case ZExpressionTarget.BC =>
                        List(ZLine.ldViaIxy(x, ZRegister.C, v.baseOffset), ZLine.ldViaIxy(x, ZRegister.B, v.baseOffset + 1))
                      case ZExpressionTarget.DE =>
                        List(ZLine.ldViaIxy(x, ZRegister.E, v.baseOffset), ZLine.ldViaIxy(x, ZRegister.D, v.baseOffset + 1))
                      case ZExpressionTarget.EHL =>
                        List(ZLine.ldViaIxy(x, L, v.baseOffset), ZLine.ldViaIxy(x, H, v.baseOffset + 1), ZLine.ldImm8(E, 0))
                      case ZExpressionTarget.DEHL =>
                        List(ZLine.ldViaIxy(x, L, v.baseOffset), ZLine.ldViaIxy(x, H, v.baseOffset + 1), ZLine.ldImm16(DE, 0))
                    }
                    case 3 => target match {
                      // TODO: signed farwords
                      case ZExpressionTarget.NOTHING => Nil
                      case ZExpressionTarget.EHL =>
                        List(ZLine.ldViaIxy(x, L, v.baseOffset), ZLine.ldViaIxy(x, H, v.baseOffset + 1), ZLine.ldViaIxy(x, E, v.baseOffset + 2))
                      case ZExpressionTarget.DEHL =>
                        List(ZLine.ldViaIxy(x, L, v.baseOffset), ZLine.ldViaIxy(x, H, v.baseOffset + 1), ZLine.ldViaIxy(x, E, v.baseOffset + 2), ZLine.ldImm8(D, 0))
                    }
                    case 4 => target match {
                      case ZExpressionTarget.NOTHING => Nil
                      case ZExpressionTarget.EHL =>
                        List(ZLine.ldViaIxy(x, L, v.baseOffset), ZLine.ldViaIxy(x, H, v.baseOffset + 1), ZLine.ldViaIxy(x, E, v.baseOffset + 2))
                      case ZExpressionTarget.DEHL =>
                        List(ZLine.ldViaIxy(x, L, v.baseOffset), ZLine.ldViaIxy(x, H, v.baseOffset + 1), ZLine.ldViaIxy(x, E, v.baseOffset + 2), ZLine.ldViaIxy(x, D, v.baseOffset + 3))
                    }
                    case _ => ???
                  }
                } else {
                  val loadHL = calculateStackAddressToHL(ctx, v.baseOffset)
                  lazy val loadHL2 = calculateStackAddressToHL(ctx, v.baseOffset + 2)
                  lazy val loadHL3 = calculateStackAddressToHL(ctx, v.baseOffset + 3)
                  import ZRegister._
                  v.typ.size match {
                    case 0 => ???
                    case 1 => loadHL ++ loadByteViaHL(target)
                    case 2 => target match {
                      // TODO: signed words
                      case ZExpressionTarget.NOTHING => Nil
                      case ZExpressionTarget.HL =>
                        loadHL ++ List(ZLine.ld8(A,MEM_HL), ZLine.register(INC_16, HL), ZLine.ld8(H, MEM_HL), ZLine.ld8(L, A))
                      case ZExpressionTarget.BC =>
                        loadHL ++ List(ZLine.ld8(C,MEM_HL), ZLine.register(INC_16, HL), ZLine.ld8(B, MEM_HL))
                      case ZExpressionTarget.DE =>
                        loadHL ++ List(ZLine.ld8(E,MEM_HL), ZLine.register(INC_16, HL), ZLine.ld8(D, MEM_HL))
                      case ZExpressionTarget.EHL =>
                        loadHL ++ List(ZLine.ld8(A,MEM_HL), ZLine.register(INC_16, HL), ZLine.ld8(H, MEM_HL), ZLine.ld8(L, A), ZLine.ldImm8(E, 0))
                      case ZExpressionTarget.DEHL =>
                        loadHL ++ List(ZLine.ld8(A,MEM_HL), ZLine.register(INC_16, HL), ZLine.ld8(H, MEM_HL), ZLine.ld8(L, A), ZLine.ldImm16(DE, 0))
                    }
                    case 3 => target match {
                      // TODO: signed farwords
                      case ZExpressionTarget.NOTHING => Nil
                      case ZExpressionTarget.EHL =>
                        loadHL2 ++ List(
                          ZLine.ld8(E,MEM_HL),
                          ZLine.register(DEC_16, HL),
                          ZLine.ld8(A, MEM_HL),
                          ZLine.register(DEC_16, HL),
                          ZLine.ld8(L, MEM_HL),
                          ZLine.ld8(H, A))
                      case ZExpressionTarget.DEHL =>
                        loadHL2 ++ List(
                          ZLine.ld8(E,MEM_HL),
                          ZLine.register(DEC_16, HL),
                          ZLine.ld8(A, MEM_HL),
                          ZLine.register(DEC_16, HL),
                          ZLine.ld8(L, MEM_HL),
                          ZLine.ld8(H, A),
                          ZLine.ldImm8(D, 0))
                    }
                    case 4 => target match {
                      case ZExpressionTarget.NOTHING => Nil
                      case ZExpressionTarget.DEHL =>
                        loadHL3 ++ List(
                          ZLine.ld8(D,MEM_HL),
                          ZLine.register(DEC_16, HL),
                          ZLine.ld8(E,MEM_HL),
                          ZLine.register(DEC_16, HL),
                          ZLine.ld8(A, MEM_HL),
                          ZLine.register(DEC_16, HL),
                          ZLine.ld8(L, MEM_HL),
                          ZLine.ld8(H, A))
                    }
                    case _ => ???
                  }
                }
            }
          case i: IndexedExpression =>
            calculateAddressToHL(ctx, i, forWriting = false) match {
              case List(ZLine0(LD_16, TwoRegisters(ZRegister.HL, ZRegister.IMM_16), addr)) => loadByte(addr, target, volatile = false)
              case code => code ++ loadByteViaHL(target)
            }
          case SumExpression(params, decimal) =>
            getArithmeticParamMaxSize(ctx, params.map(_._2)) match {
              case 1 => targetifyA(ctx, target, ZBuiltIns.compile8BitSum(ctx, params, decimal), isSigned = false)
              case 2 => targetifyHL(ctx, target, ZBuiltIns.compile16BitSum(ctx, params, decimal))
            }
          case SeparateBytesExpression(h, l) =>
            val hi = compileToA(ctx, h)
            val lo = compileToA(ctx, l)

            def xxx(hr: ZRegister.Value, lr: ZRegister.Value, allowRedirect: Boolean = false): List[ZLine] = {
              if (!lo.exists(x => x.changesRegister(hr) || x.readsRegister(hr))) {
                hi ++ List(ZLine.ld8(hr, ZRegister.A)) ++
                  lo ++ List(ZLine.ld8(lr, ZRegister.A))
              } else if (!hi.exists(x => x.changesRegister(lr) || x.readsRegister(lr))) {
                lo ++ List(ZLine.ld8(lr, ZRegister.A)) ++
                  hi ++ List(ZLine.ld8(hr, ZRegister.A))
              } else if (allowRedirect) {
                hr match {
                  case ZRegister.H =>
                    val viaDe = xxx(ZRegister.D, ZRegister.E)
                    if (viaDe.nonEmpty) {
                      viaDe ++ List(ZLine.ld8(ZRegister.L, ZRegister.E), ZLine.ld8(ZRegister.H, ZRegister.D))
                    } else {
                      val viaBc = xxx(ZRegister.B, ZRegister.C)
                      if (viaBc.nonEmpty) {
                        viaBc ++ List(ZLine.ld8(ZRegister.L, ZRegister.C), ZLine.ld8(ZRegister.H, ZRegister.B))
                      } else {
                        ???
                      }
                    }
                  case ZRegister.B =>
                    val viaHl = xxx(ZRegister.H, ZRegister.L)
                    if (viaHl.nonEmpty) {
                      viaHl ++ List(ZLine.ld8(ZRegister.C, ZRegister.L), ZLine.ld8(ZRegister.B, ZRegister.H))
                    } else {
                      val viaDe = xxx(ZRegister.D, ZRegister.E)
                      if (viaDe.nonEmpty) {
                        viaDe ++ List(ZLine.ld8(ZRegister.C, ZRegister.E), ZLine.ld8(ZRegister.B, ZRegister.D))
                      } else {
                        ???
                      }
                    }
                  case ZRegister.D =>
                    val viaHl = xxx(ZRegister.H, ZRegister.L)
                    if (viaHl.nonEmpty) {
                      viaHl ++ List(ZLine.ld8(ZRegister.E, ZRegister.L), ZLine.ld8(ZRegister.D, ZRegister.H))
                    } else {
                      val viaBc = xxx(ZRegister.B, ZRegister.C)
                      if (viaBc.nonEmpty) {
                        viaBc ++ List(ZLine.ld8(ZRegister.E, ZRegister.C), ZLine.ld8(ZRegister.D, ZRegister.B))
                      } else {
                        ???
                      }
                    }
                }
              } else Nil
            }

            target match {
              case ZExpressionTarget.NOTHING => hi ++ lo
              case ZExpressionTarget.HL => xxx(ZRegister.H, ZRegister.L, allowRedirect = true)
              case ZExpressionTarget.DE => xxx(ZRegister.D, ZRegister.E, allowRedirect = true)
              case ZExpressionTarget.BC => xxx(ZRegister.B, ZRegister.C, allowRedirect = true)
            }
          case e@DerefExpression(inner, offset, targetType) =>
            compileDerefPointer(ctx, e) ++ (targetType.size match {
              case 1 => target match {
                case ZExpressionTarget.A => List(ZLine.ld8(ZRegister.A, ZRegister.MEM_HL))
                case ZExpressionTarget.DEHL =>
                  if (targetType.isSigned) {
                    List(ZLine.ld8(ZRegister.A, ZRegister.MEM_HL), ZLine.ld8(ZRegister.L, ZRegister.A)) ++
                      signExtendHighestByte(ctx, ZRegister.A) ++
                      List(ZLine.ld8(ZRegister.H, ZRegister.A), ZLine.ld8(ZRegister.E, ZRegister.A), ZLine.ld8(ZRegister.D, ZRegister.A))
                  } else {
                    List(ZLine.ld8(ZRegister.L, ZRegister.MEM_HL), ZLine.ldImm8(ZRegister.H, 0), ZLine.ldImm8(ZRegister.E, 0), ZLine.ldImm8(ZRegister.D, 0))
                  }
                case ZExpressionTarget.HL =>
                  if (targetType.isSigned) {
                    List(ZLine.ld8(ZRegister.A, ZRegister.MEM_HL), ZLine.ld8(ZRegister.L, ZRegister.A)) ++ signExtendHighestByte(ctx, ZRegister.H)
                  } else {
                    List(ZLine.ld8(ZRegister.L, ZRegister.MEM_HL), ZLine.ldImm8(ZRegister.H, 0))
                  }
                case ZExpressionTarget.BC =>
                  if (targetType.isSigned) {
                    List(ZLine.ld8(ZRegister.A, ZRegister.MEM_HL), ZLine.ld8(ZRegister.C, ZRegister.A)) ++ signExtendHighestByte(ctx, ZRegister.B)
                  } else {
                    List(ZLine.ld8(ZRegister.C, ZRegister.MEM_HL), ZLine.ldImm8(ZRegister.B, 0))
                  }
                case ZExpressionTarget.DE =>
                  if (targetType.isSigned) {
                    List(ZLine.ld8(ZRegister.A, ZRegister.MEM_HL), ZLine.ld8(ZRegister.E, ZRegister.A)) ++ signExtendHighestByte(ctx, ZRegister.D)
                  } else {
                    List(ZLine.ld8(ZRegister.E, ZRegister.MEM_HL), ZLine.ldImm8(ZRegister.D, 0))
                  }
                case ZExpressionTarget.NOTHING => Nil
              }
              case 2 => target match {
                case ZExpressionTarget.DEHL =>
                  List(
                    ZLine.ld8(ZRegister.A, ZRegister.MEM_HL),
                    ZLine.register(INC_16, ZRegister.HL),
                    ZLine.ld8(ZRegister.H, ZRegister.MEM_HL),
                    ZLine.ld8(ZRegister.L, ZRegister.A),
                    ZLine.ldImm8(ZRegister.E, 0),
                    ZLine.ldImm8(ZRegister.D, 0)) // TODO
                case ZExpressionTarget.HL =>
                  List(
                      ZLine.ld8(ZRegister.A, ZRegister.MEM_HL),
                      ZLine.register(INC_16, ZRegister.HL),
                      ZLine.ld8(ZRegister.H, ZRegister.MEM_HL),
                      ZLine.ld8(ZRegister.L, ZRegister.A))
                case ZExpressionTarget.BC =>
                    List(
                      ZLine.ld8(ZRegister.C, ZRegister.MEM_HL),
                      ZLine.register(INC_16, ZRegister.HL),
                      ZLine.ld8(ZRegister.B, ZRegister.MEM_HL))
                case ZExpressionTarget.DE =>
                    List(
                      ZLine.ld8(ZRegister.E, ZRegister.MEM_HL),
                      ZLine.register(INC_16, ZRegister.HL),
                      ZLine.ld8(ZRegister.D, ZRegister.MEM_HL))
                case ZExpressionTarget.NOTHING => Nil
              }
              case _ =>
                ctx.log.error("Cannot read a large object indirectly")
                Nil
            })
          case f@FunctionCallExpression(name, params) =>
            name match {
              case "not" =>
                assertBool(ctx, "not", params, 1)
                compile(ctx, params.head, target, branches.flip)
              case "hi" =>
                if (params.length != 1) {
                  ctx.log.error("Too many parameters for hi/lo", f.position)
                  Nil
                } else {
                  compileToHL(ctx, params.head) ++ (target match {
                    case ZExpressionTarget.NOTHING => Nil
                    case ZExpressionTarget.A=> List(ZLine.ld8(ZRegister.A, ZRegister.H))
                    case ZExpressionTarget.HL=> List(ZLine.ld8(ZRegister.L, ZRegister.H), ZLine.ldImm8(ZRegister.H, 0))
                    case ZExpressionTarget.BC=> List(ZLine.ld8(ZRegister.C, ZRegister.H), ZLine.ldImm8(ZRegister.B, 0))
                    case ZExpressionTarget.DE=> List(ZLine.ld8(ZRegister.E, ZRegister.H), ZLine.ldImm8(ZRegister.D, 0))
                    case ZExpressionTarget.EHL => List(ZLine.ld8(ZRegister.L, ZRegister.H), ZLine.ldImm8(ZRegister.H, 0), ZLine.ldImm8(ZRegister.E, 0))
                    case ZExpressionTarget.DEHL => List(ZLine.ld8(ZRegister.L, ZRegister.H), ZLine.ldImm8(ZRegister.H, 0), ZLine.ldImm16(ZRegister.DE, 0))
                  })
                }
              case "lo" =>
                if (params.length != 1) {
                  ctx.log.error("Too many parameters for hi/lo", f.position)
                  Nil
                } else {
                  compileToHL(ctx, params.head) ++ (target match {
                    case ZExpressionTarget.NOTHING => Nil
                    case ZExpressionTarget.A => List(ZLine.ld8(ZRegister.A, ZRegister.L))
                    case ZExpressionTarget.HL => List(ZLine.ldImm8(ZRegister.H, 0))
                    case ZExpressionTarget.BC => List(ZLine.ld8(ZRegister.C, ZRegister.L), ZLine.ldImm8(ZRegister.B, 0))
                    case ZExpressionTarget.DE => List(ZLine.ld8(ZRegister.E, ZRegister.L), ZLine.ldImm8(ZRegister.D, 0))
                    case ZExpressionTarget.EHL => List(ZLine.ldImm8(ZRegister.H, 0), ZLine.ldImm8(ZRegister.E, 0))
                    case ZExpressionTarget.DEHL => List(ZLine.ldImm8(ZRegister.H, 0), ZLine.ldImm16(ZRegister.DE, 0))
                  })
                }
              case "sizeof" =>
                ctx.log.fatal("Unreachable branch: 8080 sizeof")
                Nil
              case "nonet" =>
                if (params.length != 1) {
                  ctx.log.error("Invalid number of parameters", f.position)
                  Nil
                } else {
                  compileToA(ctx, params.head) ++ (target match {
                    case ZExpressionTarget.NOTHING => Nil
                    case ZExpressionTarget.A => Nil
                    case ZExpressionTarget.EHL => compile(ctx, expression, ZExpressionTarget.HL, branches) :+ ZLine.ldImm8(ZRegister.E, 0)
                    case ZExpressionTarget.DEHL => compile(ctx, expression, ZExpressionTarget.HL, branches) :+ ZLine.ldImm16(ZRegister.DE, 0)
                    case ZExpressionTarget.HL =>
                      if (ctx.options.flag(CompilationFlag.EmitExtended80Opcodes)) {
                        List(
                          ZLine.ld8(ZRegister.L, ZRegister.A),
                          ZLine.ldImm8(ZRegister.H, 0),
                          ZLine.register(RL, ZRegister.H))
                      } else {
                        List(
                          ZLine.ld8(ZRegister.L, ZRegister.A),
                          ZLine.ldImm8(ZRegister.A, 0),
                          ZLine.implied(RLA),
                          ZLine.ld8(ZRegister.H, ZRegister.A))
                      }
                    case ZExpressionTarget.BC =>
                      if (ctx.options.flag(CompilationFlag.EmitExtended80Opcodes)) {
                        List(
                          ZLine.ld8(ZRegister.C, ZRegister.A),
                          ZLine.ldImm8(ZRegister.B, 0),
                          ZLine.register(RL, ZRegister.B))
                      } else {
                        List(
                          ZLine.ld8(ZRegister.C, ZRegister.A),
                          ZLine.ldImm8(ZRegister.A, 0),
                          ZLine.implied(RLA),
                          ZLine.ld8(ZRegister.B, ZRegister.A))
                      }
                    case ZExpressionTarget.DE =>
                      if (ctx.options.flag(CompilationFlag.EmitExtended80Opcodes)) {
                        List(
                          ZLine.ld8(ZRegister.E, ZRegister.A),
                          ZLine.ldImm8(ZRegister.B, 0),
                          ZLine.register(RL, ZRegister.D))
                      } else {
                        List(
                          ZLine.ld8(ZRegister.E, ZRegister.A),
                          ZLine.ldImm8(ZRegister.A, 0),
                          ZLine.implied(RLA),
                          ZLine.ld8(ZRegister.D, ZRegister.A))
                      }
                  })
                }
              case "&&" =>
                assertBool(ctx, "&&", params)
                branches match {
                  case BranchIfFalse(_) =>
                    params.flatMap(compile(ctx, _, target, branches))
                  case _ =>
                    val skip = ctx.nextLabel("an")
                    params.init.flatMap(compile(ctx, _, target, BranchIfFalse(skip))) ++
                      compile(ctx, params.last, target, branches) ++
                      List(ZLine.label(skip))
                }
              case "||" =>
                assertBool(ctx, "||", params)
                branches match {
                  case BranchIfTrue(_) =>
                    params.flatMap(compile(ctx, _, target, branches))
                  case _ =>
                    val skip = ctx.nextLabel("or")
                    params.init.flatMap(compile(ctx, _, target, BranchIfTrue(skip))) ++
                      compile(ctx, params.last, target, branches) ++
                      List(ZLine.label(skip))
                }
              case "^^" => ???

              case "&" =>
                getArithmeticParamMaxSize(ctx, params) match {
                  case 1 => targetifyA(ctx, target, ZBuiltIns.compile8BitOperation(ctx, AND, params), isSigned = false)
                  case 2 => targetifyHL(ctx, target, ZBuiltIns.compile16BitOperation(ctx, AND, params))
                }
              case "*" =>
                assertSizesForMultiplication(ctx, params, inPlace = false)
                getArithmeticParamMaxSize(ctx, params) match {
                  case 1 =>
                    targetifyA(ctx, target, Z80Multiply.compile8BitMultiply(ctx, params), isSigned = false)
                  case 2 =>
                    //noinspection ZeroIndexToHead
                    targetifyHL(ctx, target, Z80Multiply.compile16And8BitMultiplyToHL(ctx, params(0), params(1)))
                }
              case "|" =>
                getArithmeticParamMaxSize(ctx, params) match {
                  case 1 => targetifyA(ctx, target, ZBuiltIns.compile8BitOperation(ctx, OR, params), isSigned = false)
                  case 2 => targetifyHL(ctx, target, ZBuiltIns.compile16BitOperation(ctx, OR, params))
                }
              case "^" =>
                getArithmeticParamMaxSize(ctx, params) match {
                  case 1 => targetifyA(ctx, target, ZBuiltIns.compile8BitOperation(ctx, XOR, params), isSigned = false)
                  case 2 => targetifyHL(ctx, target, ZBuiltIns.compile16BitOperation(ctx, XOR, params))
                }
              case ">>>>" =>
                val (l, r, size) = assertArithmeticBinary(ctx, params)
                size match {
                  case 2 =>
                    targetifyA (ctx, target, compileToHL (ctx, l) ++ Z80Shifting.compileNonetShiftRight (ctx, r), isSigned = false)
                  case 1 =>
                    targetifyA(ctx, target, Z80Shifting.compile8BitShift(ctx, l, r, left = false), isSigned = false)
                  case _ => ???
                }
              case "<<" =>
                val (l, r, size) = assertArithmeticBinary(ctx, params)
                size match {
                  case 1 => targetifyA(ctx, target, Z80Shifting.compile8BitShift(ctx, l, r, left = true), isSigned = false)
                  case 2 => Z80Shifting.compile16BitShift(ctx, l, r, left = true)
                  case _ => ???
                }
              case ">>" =>
                val (l, r, size) = assertArithmeticBinary(ctx, params)
                size match {
                  case 1 => targetifyA(ctx, target, Z80Shifting.compile8BitShift(ctx, l, r, left = false), isSigned = false)
                  case 2 => Z80Shifting.compile16BitShift(ctx, l, r, left = false)
                  case _ => ???
                }
              case "<<'" =>
                assertAllArithmeticBytes("Long shift ops not supported", ctx, params)
                val (l, r, 1) = assertArithmeticBinary(ctx, params)
                targetifyA(ctx, target, compileToA(ctx, l) ++ Z80DecimalBuiltIns.compileByteShiftLeft(ctx, r), isSigned = false)
              case ">>'" =>
                assertAllArithmeticBytes("Long shift ops not supported", ctx, params)
                val (l, r, 1) = assertArithmeticBinary(ctx, params)
                targetifyA(ctx, target, Z80DecimalBuiltIns.compileByteShiftRight(ctx, Some(l), r), isSigned = false)
              case "<" =>
                val (size, signed) = assertArithmeticComparison(ctx, params)
                compileTransitiveRelation(ctx, "<", params, target, branches) { (l, r) =>
                  size match {
                    case 1 => Z80Comparisons.compile8BitComparison(ctx, if (signed) ComparisonType.LessSigned else ComparisonType.LessUnsigned, l, r, branches)
                    case 2 => Z80Comparisons.compile16BitComparison(ctx, if (signed) ComparisonType.LessSigned else ComparisonType.LessUnsigned, l, r, branches)
                    case _ => Z80Comparisons.compileLongRelativeComparison(ctx, if (signed) ComparisonType.LessSigned else ComparisonType.LessUnsigned, l, r, branches)
                  }
                }
              case ">=" =>
                val (size, signed) = assertArithmeticComparison(ctx, params)
                compileTransitiveRelation(ctx, ">=", params, target, branches) { (l, r) =>
                  size match {
                    case 1 => Z80Comparisons.compile8BitComparison(ctx, if (signed) ComparisonType.GreaterOrEqualSigned else ComparisonType.GreaterOrEqualUnsigned, l, r, branches)
                    case 2 => Z80Comparisons.compile16BitComparison(ctx, if (signed) ComparisonType.GreaterOrEqualSigned else ComparisonType.GreaterOrEqualUnsigned, l, r, branches)
                    case _ => Z80Comparisons.compileLongRelativeComparison(ctx, if (signed) ComparisonType.GreaterOrEqualSigned else ComparisonType.GreaterOrEqualUnsigned, l, r, branches)
                  }
                }
              case ">" =>
                val (size, signed) = assertArithmeticComparison(ctx, params)
                compileTransitiveRelation(ctx, ">", params, target, branches) { (l, r) =>
                  size match {
                    case 1 => Z80Comparisons.compile8BitComparison(ctx, if (signed) ComparisonType.GreaterSigned else ComparisonType.GreaterUnsigned, l, r, branches)
                    case 2 => Z80Comparisons.compile16BitComparison(ctx, if (signed) ComparisonType.GreaterSigned else ComparisonType.GreaterUnsigned, l, r, branches)
                    case _ => Z80Comparisons.compileLongRelativeComparison(ctx, if (signed) ComparisonType.GreaterSigned else ComparisonType.GreaterUnsigned, l, r, branches)
                  }
                }
              case "<=" =>
                val (size, signed) = assertArithmeticComparison(ctx, params)
                compileTransitiveRelation(ctx, "<=", params, target, branches) { (l, r) =>
                  size match {
                    case 1 => Z80Comparisons.compile8BitComparison(ctx, if (signed) ComparisonType.LessOrEqualSigned else ComparisonType.LessOrEqualUnsigned, l, r, branches)
                    case 2 => Z80Comparisons.compile16BitComparison(ctx, if (signed) ComparisonType.LessOrEqualSigned else ComparisonType.LessOrEqualUnsigned, l, r, branches)
                    case _ => Z80Comparisons.compileLongRelativeComparison(ctx, if (signed) ComparisonType.LessOrEqualSigned else ComparisonType.LessOrEqualUnsigned, l, r, branches)
                  }
                }
              case "==" =>
                val size = params.map(p => getExpressionType(ctx, p).size).max
                compileTransitiveRelation(ctx, "==", params, target, branches) { (l, r) =>
                  size match {
                    case 1 => Z80Comparisons.compile8BitComparison(ctx, ComparisonType.Equal, l, r, branches)
                    case 2 => Z80Comparisons.compile16BitComparison(ctx, ComparisonType.Equal, l, r, branches)
                    case _ => Z80Comparisons.compileLongEqualityComparison(ctx, ComparisonType.Equal, l, r, branches)
                  }
                }
              case "!=" =>
                val (l, r, size) = assertBinary(ctx, params)
                compileTransitiveRelation(ctx, "!=", params, target, branches) { (l, r) =>
                  size match {
                    case 1 => Z80Comparisons.compile8BitComparison(ctx, ComparisonType.NotEqual, l, r, branches)
                    case 2 => Z80Comparisons.compile16BitComparison(ctx, ComparisonType.NotEqual, l, r, branches)
                    case _ => Z80Comparisons.compileLongEqualityComparison(ctx, ComparisonType.NotEqual, l, r, branches)
                  }
                }
              case "+=" =>
                val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
                size match {
                  case 1 => ZBuiltIns.perform8BitInPlace(ctx, l, r, ADD)
                  case _ => ZBuiltIns.performLongInPlace(ctx, l, r, ADD, ADC, size)
                }
              case "-=" =>
                val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
                size match {
                  case 1 => ZBuiltIns.perform8BitInPlace(ctx, l, r, SUB)
                  case _ => ZBuiltIns.performLongInPlace(ctx, l, r, SUB, SBC, size)
                }
              case "+'=" =>
                val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
                size match {
                  case 1 => ZBuiltIns.perform8BitInPlace(ctx, l, r, ADD, decimal = true)
                  case _ => ZBuiltIns.performLongInPlace(ctx, l, r, ADD, ADC, size, decimal = true)
                }
              case "-'=" =>
                val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
                size match {
                  case 1 => ZBuiltIns.perform8BitInPlace(ctx, l, r, SUB, decimal = true)
                  case _ => ZBuiltIns.performLongInPlace(ctx, l, r, SUB, SBC, size, decimal = true)
                }
              case "<<=" =>
                val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
                size match {
                  case 1 => Z80Shifting.compile8BitShiftInPlace(ctx, l, r, left = true)
                  case 2 => Z80Shifting.compile16BitShift(ctx, l, r, left = true) ++ storeHL(ctx, l, signedSource = false)
                  case _ => Z80Shifting.compileLongShiftInPlace(ctx, l, r, size, left = true)
                }
              case ">>=" =>
                val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
                size match {
                  case 1 => Z80Shifting.compile8BitShiftInPlace(ctx, l, r, left = false)
                  case 2 => Z80Shifting.compile16BitShift(ctx, l, r, left = false) ++ storeHL(ctx, l, signedSource = false)
                  case _ => Z80Shifting.compileLongShiftInPlace(ctx, l, r, size, left = false)
                }
              case "<<'=" =>
                val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
                size match {
                  case 1 =>
                    calculateAddressToAppropriatePointer(ctx, l, forWriting = true) match {
                      case Some((lvo, code)) =>
                        code ++
                          (ZLine.ld8(ZRegister.A, lvo) ::
                            (Z80DecimalBuiltIns.compileByteShiftLeft(ctx, r) :+ ZLine.ld8(lvo, ZRegister.A)))
                      case None =>
                        ctx.log.error("Invalid left-hand side", l.position)
                        Nil
                    }
                  case 2 =>
                    compileToHL(ctx, l) ++ Z80DecimalBuiltIns.compileWordShiftLeft(ctx, r) ++ storeHL(ctx, l, signedSource = false)
                  case _ => Z80DecimalBuiltIns.compileInPlaceShiftLeft(ctx, l, r, size)
                }
              case ">>'=" =>
                val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
                size match {
                  case 1 =>
                    calculateAddressToAppropriatePointer(ctx, l, forWriting = true) match {
                      case Some((lvo, code)) =>
                        code ++
                          (ZLine.ld8(ZRegister.A, lvo) ::
                            (Z80DecimalBuiltIns.compileByteShiftRight(ctx, None, r) :+ ZLine.ld8(lvo, ZRegister.A)))
                      case None =>
                        ctx.log.error("Invalid left-hand side", l.position)
                        Nil
                    }
                  case 2 =>
                    Z80DecimalBuiltIns.compileWordShiftRight(ctx, l, r) ++ storeHL(ctx, l, signedSource = false)
                  case _ => Z80DecimalBuiltIns.compileInPlaceShiftRight(ctx, l, r, size)
                }
              case "*=" =>
                assertSizesForMultiplication(ctx, params, inPlace = true)
                val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
                size match {
                  case 1 =>
                    Z80Multiply.compile8BitInPlaceMultiply(ctx, l, r)
                  case 2 =>
                    Z80Multiply.compile16And8BitInPlaceMultiply(ctx, l, r)
                }
              case "/=" | "%%=" =>
                assertSizesForDivision(ctx, params, inPlace = true)
                val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
                size match {
                  case 1 =>
                    calculateAddressToAppropriatePointer(ctx, l, forWriting = true) match {
                      case Some((LocalVariableAddressViaHL, List(ZLine0(LD_16, TwoRegisters(ZRegister.HL, ZRegister.IMM_16), addr)))) =>
                        Z80Multiply.compileUnsignedByteDivision(ctx, Right(l), r, f.functionName == "%%=") :+ ZLine.ldAbs8(addr, ZRegister.A)
                      case Some((lvo, code)) =>
                        code ++ (stashHLIfChanged(ctx, Z80Multiply.compileUnsignedByteDivision(ctx, Left(lvo), r, f.functionName == "%%=")) :+ ZLine.ld8(lvo, ZRegister.A))
                      case None =>
                        ctx.log.error("Invalid left-hand side", l.position)
                        Nil
                    }
                }
              case "/" | "%%" =>
                assertSizesForDivision(ctx, params, inPlace = false)
                val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
                size match {
                  case 1 =>
                    targetifyA(ctx, target, Z80Multiply.compileUnsignedByteDivision(ctx, Right(l), r, f.functionName == "%%"), false)
                }
              case "*'=" =>
                assertAllArithmeticBytes("Long multiplication not supported", ctx, params)
                val (l, r, 1) = assertArithmeticAssignmentLike(ctx, params)
                calculateAddressToAppropriatePointer(ctx, l, forWriting = true) match {
                  case Some((lvo, code)) =>
                    code ++
                      (ZLine.ld8(ZRegister.A, lvo) ::
                        (Z80DecimalBuiltIns.compileInPlaceByteMultiplication(ctx, r) :+ ZLine.ld8(lvo, ZRegister.A)))
                  case None =>
                    ctx.log.error("Invalid left-hand side", l.position)
                    Nil
                }
              case "&=" =>
                val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
                size match {
                  case 1 => ZBuiltIns.perform8BitInPlace(ctx, l, r, AND)
                  case _ => ZBuiltIns.performLongInPlace(ctx, l, r, AND, AND, size)
                }
              case "^=" =>
                val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
                size match {
                  case 1 => ZBuiltIns.perform8BitInPlace(ctx, l, r, XOR)
                  case _ => ZBuiltIns.performLongInPlace(ctx, l, r, XOR, XOR, size)
                }
              case "|=" =>
                val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
                size match {
                  case 1 => ZBuiltIns.perform8BitInPlace(ctx, l, r, OR)
                  case _ => ZBuiltIns.performLongInPlace(ctx, l, r, OR, OR, size)
                }
              case _ =>
                env.maybeGet[Type](f.functionName) match {
                  case Some(typ) =>
                    val sourceType = validateTypeCastAndGetSourceExpressionType(ctx, typ, params)
                    return sourceType.size match {
                      case 1 => targetifyA(ctx, target, compileToA(ctx, params.head), isSigned = sourceType.isSigned)
                      case 2 => targetifyHL(ctx, target, compileToHL(ctx, params.head))
                      case _ => ???
                    }
                  case None =>
                  // fallthrough to the lookup below
                }
                lookupFunction(ctx, f) match {
                  case function: MacroFunction =>
                    val (paramPreparation, statements) = Z80MacroExpander.inlineFunction(ctx, function, params, expression.position)
                    paramPreparation ++ statements.map {
                      case _ => ???
                    }
                  case function: EmptyFunction =>
                    ??? // TODO: type conversion?
                  case function: FunctionInMemory =>
                    function match {
                      case nf: NormalFunction =>
                        if (nf.interrupt) {
                          ctx.log.error(s"Calling an interrupt function `${f.functionName}`", expression.position)
                        }
                      case _ => ()
                    }
                    val result = function.params match {
                      case AssemblyParamSignature(List(AssemblyParam(typ1, ZRegisterVariable(ZRegister.A, typ2), AssemblyParameterPassingBehaviour.Copy)))
                        if typ1.size == 1 && typ2.size == 1 =>
                        compileToA(ctx, params.head) :+ ZLine(CALL, NoRegisters, function.toAddress)
                      case AssemblyParamSignature(List(AssemblyParam(typ1, ZRegisterVariable(
                      register@(ZRegister.B | ZRegister.C | ZRegister.D | ZRegister.E | ZRegister.H | ZRegister.L),
                      typ2), AssemblyParameterPassingBehaviour.Copy)))
                        if typ1.size == 1 && typ2.size == 1 =>
                        compile8BitTo(ctx, params.head, register) :+ ZLine(CALL, NoRegisters, function.toAddress)
                      case AssemblyParamSignature(List(AssemblyParam(typ1, ZRegisterVariable(ZRegister.HL, typ2), AssemblyParameterPassingBehaviour.Copy)))
                        if typ1.size == 2 && typ2.size == 2 =>
                        compileToHL(ctx, params.head) :+ ZLine(CALL, NoRegisters, function.toAddress)
                      case AssemblyParamSignature(List(AssemblyParam(typ1, ZRegisterVariable(ZRegister.DE, typ2), AssemblyParameterPassingBehaviour.Copy)))
                        if typ1.size == 2 && typ2.size == 2 =>
                        compileToDE(ctx, params.head) :+ ZLine(CALL, NoRegisters, function.toAddress)
                      case AssemblyParamSignature(List(AssemblyParam(typ1, ZRegisterVariable(ZRegister.BC, typ2), AssemblyParameterPassingBehaviour.Copy)))
                        if typ1.size == 2 && typ2.size == 2 =>
                        compileToBC(ctx, params.head) :+ ZLine(CALL, NoRegisters, function.toAddress)
                      case AssemblyParamSignature(Nil) =>
                        List(ZLine(CALL, NoRegisters, function.toAddress))
                      case AssemblyParamSignature(paramConvs) =>
                        // TODO: stop being lazy and implement this
                        ???
                      case NormalParamSignature(List(param)) if param.typ.size == 1 =>
                        compileToA(ctx, params.head) :+ ZLine(CALL, NoRegisters, function.toAddress)
                      case NormalParamSignature(List(param)) if param.typ.size == 2 =>
                        compileToHL(ctx, params.head) :+ ZLine(CALL, NoRegisters, function.toAddress)
                      case NormalParamSignature(List(param)) if param.typ.size == 3 =>
                        compileToEHL(ctx, params.head) :+ ZLine(CALL, NoRegisters, function.toAddress)
                      case NormalParamSignature(List(param)) if param.typ.size == 4 =>
                        compileToDEHL(ctx, params.head) :+ ZLine(CALL, NoRegisters, function.toAddress)
                      case NormalParamSignature(paramVars) =>
                        params.zip(paramVars).flatMap {
                          case (paramExpr, paramVar) =>
                            val callCtx = callingContext(ctx, function.name, paramVar)
                            paramVar.typ.size match {
                              case 1 =>
                                compileToA(ctx, paramExpr) ++ storeA(callCtx, VariableExpression(paramVar.name + "`aa"), paramVar.typ.isSigned)
                              case 2 =>
                                compileToHL(ctx, paramExpr) ++ storeHL(callCtx, VariableExpression(paramVar.name + "`aa"), paramVar.typ.isSigned)
                              case _ =>
                                storeLarge(callCtx, VariableExpression(paramVar.name + "`aa"), paramExpr)
                            }
                        } ++ List(ZLine(CALL, NoRegisters, function.toAddress))
                    }
                    function.returnType.size match {
                      case 1 =>
                        targetifyA(ctx, target, result, isSigned = function.returnType.isSigned)
                      case 2 =>
                        targetifyHL(ctx, target, result)
                      case 3 =>
                        targetifyEHL(ctx, target, result)
                      case 4 =>
                        targetifyDEHL(ctx, target, result)
                      case _ =>
                        result
                    }
                }
            }

        }
    }
  }

  def calculateLoadAndStoreForByte(ctx: CompilationContext, expr: LhsExpression): (List[ZLine], List[ZLine]) = {
    Z80ExpressionCompiler.calculateAddressToAppropriatePointer(ctx, expr, forWriting = true) match { // TODO: forWriting?
      case Some((LocalVariableAddressViaHL, calculate)) =>
        (calculate :+ ZLine.ld8(ZRegister.A, ZRegister.MEM_HL)) -> List(ZLine.ld8(ZRegister.MEM_HL, ZRegister.A))
      case Some((LocalVariableAddressViaIX(offset), calculate)) =>
        (calculate :+ ZLine.ldViaIx(ZRegister.A, offset)) -> List(ZLine.ldViaIx(offset, ZRegister.A))
      case Some((LocalVariableAddressViaIY(offset), calculate)) =>
        (calculate :+ ZLine.ldViaIy(ZRegister.A, offset)) -> List(ZLine.ldViaIy(offset, ZRegister.A))
      case None => expr match {
        case SeparateBytesExpression(h: LhsExpression, l: LhsExpression) =>
          val lo = calculateLoadAndStoreForByte(ctx, l)
          val (_, hiStore) = calculateLoadAndStoreForByte(ctx, h)
          lo._1 -> (lo._2 ++ List(ZLine.ldImm8(ZRegister.A, 0)) ++ hiStore)
        case _ => ???
      }
    }
  }

  def calculateAddressToAppropriatePointer(ctx: CompilationContext, expr: LhsExpression, forWriting: Boolean): Option[(LocalVariableAddressOperand, List[ZLine])] = {
    val env = ctx.env
    expr match {
      case VariableExpression(name) =>
        env.get[Variable](name) match {
          case v:VariableInMemory => Some(LocalVariableAddressViaHL -> List(ZLine.ldImm16(ZRegister.HL, v.toAddress)))
          case v:StackVariable =>
            if (ctx.options.flag(CompilationFlag.UseIxForStack)){
              Some(LocalVariableAddressViaIX(v.baseOffset) -> Nil)
            } else if (ctx.options.flag(CompilationFlag.UseIyForStack)){
              Some(LocalVariableAddressViaIY(v.baseOffset) -> Nil)
            } else {
              Some(LocalVariableAddressViaHL -> calculateStackAddressToHL(ctx, v))
            }
        }
      case i:IndexedExpression => Some(LocalVariableAddressViaHL -> calculateAddressToHL(ctx, i, forWriting))
      case i:DerefExpression => Some(LocalVariableAddressViaHL -> compileDerefPointer(ctx, i))
      case _:SeparateBytesExpression => None
      case _ => ???
    }
  }

  @inline
  def calculateStackAddressToHL(ctx: CompilationContext, v: StackVariable): List[ZLine] = calculateStackAddressToHL(ctx, v.baseOffset)

  def calculateStackAddressToHL(ctx: CompilationContext, baseOffset: Int): List[ZLine] = {
    if (ctx.options.flag(CompilationFlag.UseIxForStack) || ctx.options.flag(CompilationFlag.UseIyForStack)) {
      ???
    } else if (ctx.options.flag(CompilationFlag.EmitSharpOpcodes)) {
      List(ZLine.imm8(ZOpcode.LD_HLSP, baseOffset + ctx.extraStackOffset))
    } else {
      List(ZLine.ldImm16(ZRegister.HL, baseOffset + ctx.extraStackOffset), ZLine.registers(ZOpcode.ADD_16, ZRegister.HL, ZRegister.SP))
    }
  }

  def calculateAddressToHL(ctx: CompilationContext, i: IndexedExpression, forWriting: Boolean): List[ZLine] = {
    val env = ctx.env
    val pointy = env.getPointy(i.name)
    AbstractExpressionCompiler.checkIndexType(ctx, pointy, i.index)
    pointy match {
      case ConstantPointy(baseAddr, _, size, _, _, alignment, readOnly) =>
        if (forWriting && readOnly) {
          ctx.log.error("Writing to a constant array", i.position)
        }
        env.evalVariableAndConstantSubParts(i.index) match {
          case (None, offset) => List(ZLine.ldImm16(ZRegister.HL, (baseAddr + offset).quickSimplify))
          case (Some(index), offset) =>
            val constantPart = (baseAddr + offset).quickSimplify
            if (getExpressionType(ctx, i.index).size == 1 && size.exists(_ < 256) && alignment == WithinPageAlignment) {
              compileToA(ctx, i.index) ++ List(
                ZLine.imm8(ADD, constantPart.loByte),
                ZLine.ld8(ZRegister.L, ZRegister.A),
                ZLine.ldImm8(ZRegister.H, constantPart.hiByte))
            } else {
              List(ZLine.ldImm16(ZRegister.BC, constantPart)) ++
                stashBCIfChanged(ctx, compileToHL(ctx, index)) ++
                List(ZLine.registers(ADD_16, ZRegister.HL, ZRegister.BC))
            }
        }
      case VariablePointy(varAddr, _, _, _) =>
        env.eval(i.index) match {
          case Some(NumericConstant(0, _)) =>
            if (ctx.options.flag(CompilationFlag.EmitIntel8080Opcodes)) {
              List(ZLine.ldAbs16(ZRegister.HL, varAddr))
            } else {
              // TODO: is this reasonable?
              List(
                ZLine.ldAbs8(ZRegister.A, varAddr),
                ZLine.ld8(ZRegister.L, ZRegister.A),
                ZLine.ldAbs8(ZRegister.A, varAddr + 1),
                ZLine.ld8(ZRegister.H, ZRegister.A))
            }
          case _ =>
            if (ctx.options.flag(CompilationFlag.EmitIntel8080Opcodes)) {
              compileToBC(ctx, i.index) ++
                List(
                  ZLine.ldAbs16(ZRegister.HL, varAddr),
                  ZLine.registers(ADD_16, ZRegister.HL, ZRegister.BC))
            } else {
              // TODO: is this reasonable?
              compileToBC(ctx, i.index) ++
                List(
                  ZLine.ldAbs8(ZRegister.A, varAddr),
                  ZLine.ld8(ZRegister.L, ZRegister.A),
                  ZLine.ldAbs8(ZRegister.A, varAddr + 1),
                  ZLine.ld8(ZRegister.H, ZRegister.A),
                  ZLine.registers(ADD_16, ZRegister.HL, ZRegister.BC))
            }
        }
      case _: StackVariablePointy =>
        compileToHL(ctx, VariableExpression(i.name).pos(i.position)) ++
          stashHLIfChanged(ctx, compileToBC(ctx, i.index)) ++
          List(ZLine.registers(ADD_16, ZRegister.HL, ZRegister.BC))
    }
  }

  def loadByte(sourceAddr: Constant, target: ZExpressionTarget.Value, volatile: Boolean): List[ZLine] = {
    val elidability = if (volatile) Elidability.Volatile else Elidability.Elidable
    target match {
      case ZExpressionTarget.NOTHING => Nil
      case ZExpressionTarget.A => List(ZLine.ldAbs8(ZRegister.A, sourceAddr, elidability))
      case ZExpressionTarget.HL => List(ZLine.ldAbs8(ZRegister.A, sourceAddr, elidability), ZLine.ld8(ZRegister.L, ZRegister.A), ZLine.ldImm8(ZRegister.H, 0))
      case ZExpressionTarget.BC => List(ZLine.ldAbs8(ZRegister.A, sourceAddr, elidability), ZLine.ld8(ZRegister.C, ZRegister.A), ZLine.ldImm8(ZRegister.B, 0))
      case ZExpressionTarget.DE => List(ZLine.ldAbs8(ZRegister.A, sourceAddr, elidability), ZLine.ld8(ZRegister.E, ZRegister.A), ZLine.ldImm8(ZRegister.D, 0))
      case ZExpressionTarget.EHL => List(ZLine.ldAbs8(ZRegister.A, sourceAddr, elidability), ZLine.ld8(ZRegister.L, ZRegister.A), ZLine.ldImm8(ZRegister.H, 0), ZLine.ldImm8(ZRegister.E, 0))
      case ZExpressionTarget.DEHL => List(ZLine.ldAbs8(ZRegister.A, sourceAddr, elidability), ZLine.ld8(ZRegister.L, ZRegister.A), ZLine.ldImm8(ZRegister.H, 0), ZLine.ldImm16(ZRegister.DE, 0))
    }
  }

  def loadByteViaIX(offset: Int, target: ZExpressionTarget.Value): List[ZLine] = {
    target match {
      case ZExpressionTarget.NOTHING => Nil
      case ZExpressionTarget.A => List(ZLine.ldViaIx(ZRegister.A, offset))
      case ZExpressionTarget.HL => List(ZLine.ldViaIx(ZRegister.L, offset), ZLine.ldImm8(ZRegister.H, 0))
      case ZExpressionTarget.BC => List(ZLine.ldViaIx(ZRegister.C, offset), ZLine.ldImm8(ZRegister.B, 0))
      case ZExpressionTarget.DE => List(ZLine.ldViaIx(ZRegister.E, offset), ZLine.ldImm8(ZRegister.D, 0))
      case ZExpressionTarget.EHL => List(ZLine.ldViaIx(ZRegister.L, offset), ZLine.ldImm8(ZRegister.H, 0), ZLine.ldImm8(ZRegister.E, 0))
      case ZExpressionTarget.DEHL => List(ZLine.ldViaIx(ZRegister.L, offset), ZLine.ldImm8(ZRegister.H, 0), ZLine.ldImm16(ZRegister.DE, 0))
    }
  }

  def loadByteViaIY(offset: Int, target: ZExpressionTarget.Value): List[ZLine] = {
    target match {
      case ZExpressionTarget.NOTHING => Nil
      case ZExpressionTarget.A => List(ZLine.ldViaIy(ZRegister.A, offset))
      case ZExpressionTarget.HL => List(ZLine.ldViaIy(ZRegister.L, offset), ZLine.ldImm8(ZRegister.H, 0))
      case ZExpressionTarget.BC => List(ZLine.ldViaIy(ZRegister.C, offset), ZLine.ldImm8(ZRegister.B, 0))
      case ZExpressionTarget.DE => List(ZLine.ldViaIy(ZRegister.E, offset), ZLine.ldImm8(ZRegister.D, 0))
      case ZExpressionTarget.EHL => List(ZLine.ldViaIy(ZRegister.L, offset), ZLine.ldImm8(ZRegister.H, 0), ZLine.ldImm8(ZRegister.E, 0))
      case ZExpressionTarget.DEHL => List(ZLine.ldViaIy(ZRegister.L, offset), ZLine.ldImm8(ZRegister.H, 0), ZLine.ldImm16(ZRegister.DE, 0))
    }
  }

  def loadByteViaHL(target: ZExpressionTarget.Value): List[ZLine] = {
    target match {
      case ZExpressionTarget.NOTHING => Nil
      case ZExpressionTarget.A => List(ZLine.ld8(ZRegister.A, ZRegister.MEM_HL))
      case ZExpressionTarget.HL => List(ZLine.ld8(ZRegister.L, ZRegister.MEM_HL), ZLine.ldImm8(ZRegister.H, 0))
      case ZExpressionTarget.BC => List(ZLine.ld8(ZRegister.C, ZRegister.MEM_HL), ZLine.ldImm8(ZRegister.B, 0))
      case ZExpressionTarget.DE => List(ZLine.ld8(ZRegister.E, ZRegister.MEM_HL), ZLine.ldImm8(ZRegister.D, 0))
      case ZExpressionTarget.EHL => List(ZLine.ld8(ZRegister.L, ZRegister.MEM_HL), ZLine.ldImm8(ZRegister.H, 0), ZLine.ldImm8(ZRegister.E, 0))
      case ZExpressionTarget.DEHL => List(ZLine.ld8(ZRegister.L, ZRegister.MEM_HL), ZLine.ldImm8(ZRegister.H, 0), ZLine.ldImm16(ZRegister.DE, 0))
    }
  }

  def signExtend(ctx: CompilationContext, targetAddr: Constant, hiRegister: ZRegister.Value, bytes: Int, signedSource: Boolean): List[ZLine] = {
    if (bytes == 0) return Nil
    val prepareA = if (signedSource) {
      signExtendHighestByte(ctx, hiRegister)
    } else {
      List(ZLine.ldImm8(ZRegister.A, 0))
    }
    val fillUpperBytes = List.tabulate(bytes)(i => ZLine.ldAbs8((targetAddr + i).quickSimplify, ZRegister.A))
    prepareA ++ fillUpperBytes
  }

  private def signExtendHighestByte(ctx: CompilationContext, hiRegister: ZRegister.Value): List[ZLine] = {
    val prefix = if (hiRegister == ZRegister.A) Nil else List(ZLine.ld8(ZRegister.A, hiRegister))
    val label = ctx.nextLabel("sx")
    if (ctx.options.flag(CompilationFlag.EmitIntel8080Opcodes)) {
      prefix ++ List(
        ZLine.imm8(OR, 0x7f),
        ZLine.jump(label, IfFlagSet(ZFlag.S)),
        ZLine.ldImm8(ZRegister.A, 0),
        ZLine.label(label))
    } else if (ctx.options.flag(CompilationFlag.EmitExtended80Opcodes)) {
      // TODO: is this optimal?
      prefix ++ List(
        ZLine.imm8(OR, 0x7f),
        ZLine.register(BIT7, ZRegister.A),
        ZLine.jump(label, IfFlagClear(ZFlag.Z)),
        ZLine.ldImm8(ZRegister.A, 0),
        ZLine.label(label))
    } else {
      throw new IllegalStateException()
    }
  }

  def signExtendViaIX(ctx: CompilationContext, targetOffset: Int, hiRegister: ZRegister.Value, bytes: Int, signedSource: Boolean): List[ZLine] = {
    if (bytes == 0) return Nil
    val prepareA = if (signedSource) {
      signExtendHighestByte(ctx, hiRegister)
    } else {
      List(ZLine.ldImm8(ZRegister.A, 0))
    }
    val fillUpperBytes = List.tabulate(bytes)(i => ZLine.ldViaIx(targetOffset + i, ZRegister.A))
    prepareA ++ fillUpperBytes
  }

  def signExtendViaIY(ctx: CompilationContext, targetOffset: Int, hiRegister: ZRegister.Value, bytes: Int, signedSource: Boolean): List[ZLine] = {
    if (bytes == 0) return Nil
    val prepareA = if (signedSource) {
      signExtendHighestByte(ctx, hiRegister)
    } else {
      List(ZLine.ldImm8(ZRegister.A, 0))
    }
    val fillUpperBytes = List.tabulate(bytes)(i => ZLine.ldViaIy(targetOffset + i, ZRegister.A))
    prepareA ++ fillUpperBytes
  }

  def signExtendViaHL(ctx: CompilationContext, hiRegister: ZRegister.Value, bytes: Int, signedSource: Boolean): List[ZLine] = {
    if (bytes == 0) return Nil
    val prepareA = if (signedSource) {
      signExtendHighestByte(ctx, hiRegister)
    } else {
      List(ZLine.ldImm8(ZRegister.A, 0))
    }
    val fillUpperBytes = List.fill(bytes)(List(ZLine.register(INC_16, ZRegister.HL), ZLine.ld8(ZRegister.MEM_HL, ZRegister.A))).flatten
    prepareA ++ fillUpperBytes
  }

  def storeA(ctx: CompilationContext, targetAddr: Constant, targetSize: Int, signedSource: Boolean): List[ZLine] = {
    targetSize match {
      case 0 => Nil
      case 1 => List(ZLine.ldAbs8(targetAddr, ZRegister.A))
      case n => ZLine.ldAbs8(targetAddr, ZRegister.A) :: signExtend(ctx, targetAddr + 1, ZRegister.A, n - 1, signedSource)
    }
  }

  def storeAViaHL(ctx: CompilationContext, targetSize: Int, signedSource: Boolean): List[ZLine] = {
    targetSize match {
      case 0 => Nil
      case 1 => List(ZLine.ld8(ZRegister.MEM_HL, ZRegister.A))
      case n => ZLine.ld8(ZRegister.MEM_HL, ZRegister.A) :: signExtendViaHL(ctx, ZRegister.A, n - 1, signedSource)
    }
  }

  def storeAViaIX(ctx: CompilationContext, targetOffset: Int, targetSize: Int, signedSource: Boolean): List[ZLine] = {
    targetSize match {
      case 0 => Nil
      case 1 => List(ZLine.ldViaIx(targetOffset, ZRegister.A))
      case n => ZLine.ldViaIx(targetOffset, ZRegister.A) :: signExtendViaIX(ctx, targetOffset + 1, ZRegister.A, n - 1, signedSource)
    }
  }

  def storeAViaIY(ctx: CompilationContext, targetOffset: Int, targetSize: Int, signedSource: Boolean): List[ZLine] = {
    targetSize match {
      case 0 => Nil
      case 1 => List(ZLine.ldViaIy(targetOffset, ZRegister.A))
      case n => ZLine.ldViaIy(targetOffset, ZRegister.A) :: signExtendViaIY(ctx, targetOffset + 1, ZRegister.A, n - 1, signedSource)
    }
  }

  def storeHL(ctx: CompilationContext, targetAddr: Constant, targetSize: Int, signedSource: Boolean): List[ZLine] = {
    // TODO: LD (nnnn),HL compatibility?
    targetSize match {
      case 0 => Nil
      case 1 => List(ZLine.ld8(ZRegister.A, ZRegister.L), ZLine.ldAbs8(targetAddr, ZRegister.A))
      case 2 =>
        if (ctx.options.flag(CompilationFlag.EmitIntel8080Opcodes)){
          List(ZLine.ldAbs16(targetAddr, ZRegister.HL))
        } else {
          List(
            ZLine.ld8(ZRegister.A, ZRegister.L),
            ZLine.ldAbs8(targetAddr, ZRegister.A),
            ZLine.ld8(ZRegister.A, ZRegister.H),
            ZLine.ldAbs8(targetAddr + 1, ZRegister.A))
        }
      case n =>
        if (ctx.options.flag(CompilationFlag.EmitIntel8080Opcodes)){
          ZLine.ldAbs16(targetAddr, ZRegister.HL) :: signExtend(ctx, targetAddr + 2, ZRegister.H, n - 2, signedSource)
        } else {
          List(
            ZLine.ld8(ZRegister.A, ZRegister.L),
            ZLine.ldAbs8(targetAddr, ZRegister.A),
            ZLine.ld8(ZRegister.A, ZRegister.H),
            ZLine.ldAbs8(targetAddr + 1, ZRegister.A)) ++ signExtend(ctx, targetAddr + 2, ZRegister.H, n - 2, signedSource)
        }
    }
  }

  def storeHLViaIX(ctx: CompilationContext, offset: Int, targetSize: Int, signedSource: Boolean): List[ZLine] = {
    // TODO: LD (nnnn),HL compatibility?
    targetSize match {
      case 0 => Nil
      case 1 => List(ZLine.ldViaIx(offset, ZRegister.L))
      case 2 => List(ZLine.ldViaIx(offset, ZRegister.L), ZLine.ldViaIx(offset + 1, ZRegister.H))
      case n => List(ZLine.ldViaIx(offset, ZRegister.L), ZLine.ldViaIx(offset + 1, ZRegister.H)) ++ signExtendViaIX(ctx, offset + 2, ZRegister.H, n - 2, signedSource)
    }
  }

  def storeHLViaIY(ctx: CompilationContext, offset: Int, targetSize: Int, signedSource: Boolean): List[ZLine] = {
    // TODO: LD (nnnn),HL compatibility?
    targetSize match {
      case 0 => Nil
      case 1 => List(ZLine.ldViaIy(offset, ZRegister.L))
      case 2 => List(ZLine.ldViaIy(offset, ZRegister.L), ZLine.ldViaIy(offset + 1, ZRegister.H))
      case n => List(ZLine.ldViaIy(offset, ZRegister.L), ZLine.ldViaIy(offset + 1, ZRegister.H)) ++ signExtendViaIY(ctx, offset + 2, ZRegister.H, n - 2, signedSource)
    }
  }

  def storeA(ctx: CompilationContext, target: LhsExpression, signedSource: Boolean): List[ZLine] = {
    val env = ctx.env
    target match {
      case VariableExpression(vname) =>
        env.get[Variable](vname) match {
          case v: VariableInMemory => storeA(ctx, v.toAddress, v.typ.size, signedSource)
          case v: StackVariable =>
            if (ctx.options.flag(CompilationFlag.UseIxForStack)){
              storeAViaIX(ctx, v.baseOffset, v.typ.size, signedSource)
            } else if (ctx.options.flag(CompilationFlag.UseIyForStack)){
              storeAViaIY(ctx, v.baseOffset, v.typ.size, signedSource)
            } else {
              calculateStackAddressToHL(ctx, v) ++ storeAViaHL(ctx, v.typ.size, signedSource)
            }
        }
      case i:IndexedExpression =>
        calculateAddressToHL(ctx, i, forWriting = true) match {
          case List(ZLine0(LD_16, TwoRegisters(ZRegister.HL, ZRegister.IMM_16), addr)) => storeA(ctx, addr, 1, signedSource)
          case code => if (code.exists(changesA)) {
            List(ZLine.ld8(ZRegister.E, ZRegister.A)) ++ stashDEIfChanged(ctx, code) :+ ZLine.ld8(ZRegister.MEM_HL, ZRegister.E)
          } else code :+ ZLine.ld8(ZRegister.MEM_HL, ZRegister.A)
        }
      case e@DerefExpression(_, _, targetType) =>
        val lo = stashAFIfChanged(ctx, compileDerefPointer(ctx, e)) :+ ZLine.ld8(ZRegister.MEM_HL, ZRegister.A)
        if (targetType.size == 1) lo
        else if (targetType.size == 2) {
          lo ++ List(ZLine.register(INC_16, ZRegister.HL)) ++ signExtendHighestByte(ctx, ZRegister.MEM_HL)
          lo
        } else {
          lo ++ signExtendHighestByte(ctx, ZRegister.A) ++ List.tabulate(targetType.size - 1)(_ => List(
            ZLine.register(INC_16, ZRegister.HL),
            ZLine.ld8(ZRegister.MEM_HL, ZRegister.A)
          )).flatten
        }
      //TODO
      case SeparateBytesExpression(hi, lo) => ???
    }
  }

  def storeHL(ctx: CompilationContext, target: LhsExpression, signedSource: Boolean): List[ZLine] = {
    val env = ctx.env
    target match {
      case VariableExpression(vname) =>
        env.get[Variable](vname) match {
          case v: VariableInMemory => storeHL(ctx, v.toAddress, v.typ.size, signedSource)
          case v: StackVariable =>
            import ZRegister._
            if (ctx.options.flag(CompilationFlag.UseIxForStack)){
              storeHLViaIX(ctx, v.baseOffset, v.typ.size, signedSource)
            } else if (ctx.options.flag(CompilationFlag.UseIyForStack)){
              storeHLViaIY(ctx, v.baseOffset, v.typ.size, signedSource)
            } else if (ctx.options.flag(CompilationFlag.EmitIntel8080Opcodes)) {
              List(
                ZLine.register(PUSH, DE),
                ZLine.implied(EX_DE_HL)) ++
                fixTsx(ctx, calculateStackAddressToHL(ctx, v)) ++
                List(
                  ZLine.ld8(MEM_HL, E),
                  ZLine.register(INC_16, HL),
                  ZLine.ld8(MEM_HL, D),
                  ZLine.register(POP, DE))
            } else {
              List(
                ZLine.register(PUSH, DE),
                ZLine.ld8(D, H),
                ZLine.ld8(E, L)) ++
                fixTsx(ctx, calculateStackAddressToHL(ctx, v)) ++
                List(
                  ZLine.ld8(MEM_HL, E),
                  ZLine.register(INC_16, HL),
                  ZLine.ld8(MEM_HL, D),
                  ZLine.register(POP, DE))
            }
        }
      case IndexedExpression(pointyName, indexExpr) =>
        env.getPointy(pointyName) match {
          case p: ConstantPointy =>
            env.evalVariableAndConstantSubParts(indexExpr) match {
              case (None, offset) => ZLine.ld8(ZRegister.A, ZRegister.L) :: storeA(ctx, (p.value + offset).quickSimplify, 1, signedSource)
            }
        }
      case SeparateBytesExpression(hi: LhsExpression, lo: LhsExpression) =>
        Z80ExpressionCompiler.stashHLIfChanged(ctx, ZLine.ld8(ZRegister.A, ZRegister.L) :: storeA(ctx, lo, signedSource)) ++
          (ZLine.ld8(ZRegister.A, ZRegister.H) :: storeA(ctx, hi, signedSource))
      case e:DerefExpression =>
        List(ZLine.register(PUSH, ZRegister.HL)) ++ compileDerefPointer(ctx, e) ++ List(
          ZLine.register(POP, ZRegister.BC),
          ZLine.ld8(ZRegister.MEM_HL, ZRegister.C),
          ZLine.register(INC_16, ZRegister.HL),
          ZLine.ld8(ZRegister.MEM_HL, ZRegister.B))
      case _: SeparateBytesExpression =>
        ctx.log.error("Invalid `:`", target.position)
        Nil
    }
  }

  def storeLarge(ctx: CompilationContext, target: LhsExpression, source: Expression): List[ZLine] = {
    val env = ctx.env
    target match {
      case VariableExpression(vname) =>
        env.get[Variable](vname) match {
          case v: Variable =>
            import ZRegister._
            val size = v.typ.size
            val reads = compileByteReads(ctx, source, size, ZExpressionTarget.HL)
            val stores = compileByteStores(ctx, target, size, includeStep = true)
            if (stores.exists(_.exists(_.changesRegister(HL)))) {
              if (reads.tail.exists(_.exists(_.changesRegister(HL)))) {
                // most likely stack-to-stack copy
                // use DE as the secondary pointer
                val fixedReads = reads.head.init ++ List(ZLine.ld8(E, L), ZLine.ld8(D, H), ZLine.ld8(A, MEM_DE)) :: reads.tail.map(_.map {
                  case l@ZLine0(LD, TwoRegisters(A, MEM_HL), _) => l.copy(registers = TwoRegisters(A, MEM_DE))
                  case l@ZLine0(INC_16, OneRegister(HL), _) => l.copy(registers = OneRegister(DE))
                  case l@ZLine0(DEC_16, OneRegister(HL), _) => l.copy(registers = OneRegister(DE))
                  case l => l
                })
                fixedReads.zip(stores).flatMap(t => t._1 ++ t._2)
              } else {
                val fixedReads = reads.head ++ List(ZLine.ld8(B, H)) :: reads.tail.map(_.map {
                  case l@ZLine0(LD, TwoRegisters(reg, H), _) => l.copy(registers = TwoRegisters(reg, B))
                  case l@ZLine0(LD, TwoRegisters(reg, L), _) => l.copy(registers = TwoRegisters(reg, C))
                  case l => l
                })
                val fixedStores = stores.map(_.map {
                  case l@ZLine0(LD, TwoRegisters(H, reg), _) => l.copy(registers = TwoRegisters(B, reg))
                  case l@ZLine0(LD, TwoRegisters(L, reg), _) => l.copy(registers = TwoRegisters(C, reg))
                  case l => l
                })
                fixedReads.zip(fixedStores).flatMap(t => t._1 ++ t._2)
              }
            } else {
              reads.zip(stores).flatMap(t => t._1 ++ t._2)
            }
        }
      case _ => ???
    }
  }

  private def compileTransitiveRelation(ctx: CompilationContext,
                                        operator: String,
                                        params: List[Expression],
                                        target: ZExpressionTarget.Value,
                                        branches: BranchSpec)(binary: (Expression, Expression) => List[ZLine]): List[ZLine] = {
    params match {
      case List(l, r) => binary(l, r)
      case List(_) | Nil =>
        ctx.log.fatal("")
      case _ =>
        params.tail.init.foreach { e =>
          if (ctx.env.eval(e).isEmpty) e match {
            case VariableExpression(_) =>
            case LiteralExpression(_, _) =>
            case GeneratedConstantExpression(_, _) =>
            case IndexedExpression(_, VariableExpression(_)) =>
            case IndexedExpression(_, LiteralExpression(_, _)) =>
            case IndexedExpression(_, GeneratedConstantExpression(_, _)) =>
            case IndexedExpression(_, SumExpression(sumParams, false)) if isUpToOneVar(sumParams) =>
            case _ =>
              ctx.log.warn("A complex expression may be evaluated multiple times", e.position)
          }
        }
        val conjunction = params.init.zip(params.tail).map {
          case (l, r) => FunctionCallExpression(operator, List(l, r))
        }.reduceLeft((a, b) => FunctionCallExpression("&&", List(a, b)))
        compile(ctx, conjunction, target, branches)
    }
  }

  def compileByteReads(ctx: CompilationContext, rhs: Expression, size: Int, temporaryTarget: ZExpressionTarget.Value): List[List[ZLine]] = {
    if (size == 1) throw new IllegalArgumentException
    val env = ctx.env
    env.eval(rhs) match {
      case Some(constant) =>
        List.tabulate(size)(i => List(ZLine.ldImm8(ZRegister.A, constant.subbyte(i))))
      case None =>
        rhs match {
          case VariableExpression(vname) =>
            env.get[Variable](vname) match {
              case v: VariableInMemory =>
                List.tabulate(size) { i =>
                  if (i < v.typ.size) {
                    List(ZLine.ldAbs8(ZRegister.A, v.toAddress + i))
                  } else if (v.typ.isSigned) {
                    ZLine.ldAbs8(ZRegister.A, v.toAddress + v.typ.size - 1) :: signExtendHighestByte(ctx, ZRegister.A)
                  } else {
                    List(ZLine.ldImm8(ZRegister.A, 0))
                  }
                }
              case v: StackVariable =>
                import ZRegister._
                if (ctx.options.flag(CompilationFlag.UseIxForStack)) {
                  List.tabulate(size) { i =>
                    if (i < v.typ.size) {
                      List(ZLine.ldViaIx(A, v.baseOffset + i))
                    } else if (v.typ.isSigned) {
                      ZLine.ldViaIx(A, v.baseOffset + v.typ.size - 1) :: signExtendHighestByte(ctx, A)
                    } else {
                      List(ZLine.ldImm8(A, 0))
                    }
                  }
                } else if (ctx.options.flag(CompilationFlag.UseIyForStack)) {
                  List.tabulate(size) { i =>
                    if (i < v.typ.size) {
                      List(ZLine.ldViaIy(A, v.baseOffset + i))
                    } else if (v.typ.isSigned) {
                      ZLine.ldViaIy(A, v.baseOffset + v.typ.size - 1) :: signExtendHighestByte(ctx, A)
                    } else {
                      List(ZLine.ldImm8(A, 0))
                    }
                  }
                } else {
                  val prepareHL = calculateStackAddressToHL(ctx, v)
                  List.tabulate(size) { i =>
                    if (i == 0) {
                      prepareHL :+ ZLine.ld8(A, MEM_HL)
                    } else if (i < v.typ.size) {
                      List(ZLine.register(INC_16, HL), ZLine.ld8(A, MEM_HL))
                    } else if (v.typ.isSigned) {
                      ZLine.ld8(A, MEM_HL) :: signExtendHighestByte(ctx, ZRegister.A)
                    } else {
                      List(ZLine.ldImm8(A, 0))
                    }
                  }
                }
            }
          case SeparateBytesExpression(hi, lo) =>
            List.tabulate(size) { i =>
              if (i == 0) {
                compileToA(ctx, lo)
              } else if (i == 1) {
                compileToA(ctx, hi)
              } else {
                List(ZLine.ldImm8(ZRegister.A, 0))
              }
            }
          case _ =>
            val (h, l) = temporaryTarget match {
              case ZExpressionTarget.HL => ZRegister.H -> ZRegister.L
              case ZExpressionTarget.BC => ZRegister.B -> ZRegister.C
              case ZExpressionTarget.DE => ZRegister.D -> ZRegister.E
              case _ => throw new IllegalArgumentException("temporaryTarget")
            }
            val typ = getExpressionType(ctx, rhs)
            typ.size match {
              case 1 =>
                List.tabulate(size) { i =>
                  if (i == 0) {
                    if (typ.isSigned) {
                      (compileToA(ctx, rhs) :+ ZLine.ld8(l, ZRegister.A)) ++
                        signExtendHighestByte(ctx, ZRegister.A) ++ List(ZLine.ld8(h, ZRegister.A), ZLine.ld8(ZRegister.A, l))
                    } else {
                      compileToA(ctx, rhs)
                    }
                  } else if (typ.isSigned) {
                    List(ZLine.ld8(ZRegister.A, h))
                  } else {
                    // TODO: signed words?
                    List(ZLine.ldImm8(ZRegister.A, 0))
                  }
                }
              case 2 =>
                List.tabulate(size) { i =>
                  if (i == 0) {
                    compile(ctx, rhs, temporaryTarget, BranchSpec.None) :+ ZLine.ld8(ZRegister.A, l)
                  } else if (i == 1) {
                    List(ZLine.ld8(ZRegister.A, h))
                  } else {
                    // TODO: signed words?
                    List(ZLine.ldImm8(ZRegister.A, 0))
                  }
                }
              case 3 =>
                List.tabulate(size) {
                  case 0 => compileToEHL(ctx, rhs) :+ ZLine.ld8(ZRegister.A, ZRegister.L)
                  case 1 => List(ZLine.ld8(ZRegister.A, ZRegister.H))
                  case 2 => List(ZLine.ld8(ZRegister.A, ZRegister.E))
                  case _ => List(ZLine.ldImm8(ZRegister.A, 0)) // TODO: signed farwords?
                }
              case 4 =>
                List.tabulate(size) {
                  case 0 => compileToDEHL(ctx, rhs) :+ ZLine.ld8(ZRegister.A, ZRegister.L)
                  case 1 => List(ZLine.ld8(ZRegister.A, ZRegister.H))
                  case 2 => List(ZLine.ld8(ZRegister.A, ZRegister.E))
                  case 3 => List(ZLine.ld8(ZRegister.A, ZRegister.D))
                  case _ => List(ZLine.ldImm8(ZRegister.A, 0)) // TODO: signed longs?
                }
              case _ =>
                rhs match {
                  case FunctionCallExpression(fname, _) =>
                    env.maybeGet[NormalFunction](fname) match {
                      case Some(function) =>
                        val result = env.get[VariableInMemory](function.name + ".return")
                        List.tabulate(size) {
                          case 0 => compile(ctx, rhs, ZExpressionTarget.NOTHING, BranchSpec.None) :+ ZLine.ldAbs8(ZRegister.A, result)
                          case i if i < typ.size => List(ZLine.ldAbs8(ZRegister.A, result.toAddress + i))
                          case _ => List(ZLine.ldImm8(ZRegister.A, 0)) // TODO: signed large types?
                        }
                    }
                }
            }
        }
    }
  }


  def compileByteStores(ctx: CompilationContext, lhs: LhsExpression, size: Int, includeStep: Boolean): List[List[ZLine]] = {
    if (size == 1) throw new IllegalArgumentException
    val env = ctx.env
    lhs match {
      case VariableExpression(vname) =>
        env.get[Variable](vname) match {
          case v: VariableInMemory =>
            if (v.typ.size < size) {
              ctx.log.error(s"Variable `$vname` is too small", lhs.position)
            }
            List.tabulate(size) { i =>
              if (i < size) {
                List(ZLine.ldAbs8(v.toAddress + i, ZRegister.A))
              } else {
                Nil
              }
            }
          case v: StackVariable =>
            if (v.typ.size < size) {
              ctx.log.error(s"Variable `$vname` is too small", lhs.position)
            }
            import ZRegister._
            if (ctx.options.flag(CompilationFlag.UseIxForStack)) {
              List.tabulate(size) { i =>
                if (i < size) {
                  List(ZLine.ldViaIx(v.baseOffset + i, ZRegister.A))
                } else {
                  Nil
                }
              }
            } else
            if (ctx.options.flag(CompilationFlag.UseIyForStack)) {
              List.tabulate(size) { i =>
                if (i < size) {
                  List(ZLine.ldViaIy(v.baseOffset + i, ZRegister.A))
                } else {
                  Nil
                }
              }
            } else {
              val prepareHL = calculateStackAddressToHL(ctx, v)
              List.tabulate(size) { i =>
                if (i == 0) {
                  prepareHL :+ ZLine.ld8(MEM_HL, A)
                } else if (i < v.typ.size) {
                  if (includeStep) List(ZLine.register(INC_16, HL), ZLine.ld8(MEM_HL, A))
                  else List(ZLine.ld8(MEM_HL, A))
                } else Nil
              }
            }
        }
      case SeparateBytesExpression(hi: LhsExpression, lo: LhsExpression) =>
        if (size > 2) {
          ctx.log.error(s"Left hand side is too small", lhs.position)
        }
        List.tabulate(size) { i =>
          if (i == 0) {
            storeA(ctx, lo, signedSource = false)
          } else if (i == 1) {
            storeA(ctx, hi, signedSource = false)
          } else {
            Nil
          }
        }
    }
  }

}
