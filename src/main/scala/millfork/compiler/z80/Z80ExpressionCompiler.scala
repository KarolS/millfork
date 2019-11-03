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

  def compileToFatBooleanInA(ctx: CompilationContext, expression: Expression): List[ZLine] = {
    val sourceType = AbstractExpressionCompiler.getExpressionType(ctx, expression)
    sourceType match {
      case FatBooleanType => compileToA(ctx, expression)
      case t: ConstantBooleanType =>
        List(ZLine.ldImm8(ZRegister.A, if (t.value) 1 else 0))
      case BuiltInBooleanType | _: FlagBooleanType =>
        // TODO optimize if using CARRY
        // TODO: helper functions to convert flags to booleans, to make code smaller
        val label = ctx.env.nextLabel("bo")
        val condition = Z80ExpressionCompiler.compile(ctx, expression, ZExpressionTarget.NOTHING, BranchIfFalse(label))
        val conditionWithoutJump = condition.init
        val hasOnlyOneJump = !conditionWithoutJump.exists(_.refersTo(label))
        if (hasOnlyOneJump && (condition.last.opcode == JP || condition.last.opcode == JR)) {
          import ZRegister._
          condition.last.registers match {
            case IfFlagClear(ZFlag.C) =>
              // our bool is in the carry flag
              return conditionWithoutJump ++ List(ZLine.ldImm8(A, 0), ZLine.implied(RLA))
            case IfFlagSet(ZFlag.C) =>
              // our bool is in the carry flag, negated
              return conditionWithoutJump ++ List(ZLine.ldImm8(A, 0), ZLine.implied(CCF), ZLine.implied(RLA))
            case IfFlagClear(ZFlag.S) if areSZFlagsBasedOnA(conditionWithoutJump) =>
              // our bool is in the sign flag and the 7th bit of A
              return conditionWithoutJump ++ List(ZLine.implied(RLCA), ZLine.imm8(AND, 1))
            case IfFlagSet(ZFlag.S) if areSZFlagsBasedOnA(conditionWithoutJump) =>
              // our bool is in the sign flag and the 7th bit of A, negated
              return conditionWithoutJump ++ List(ZLine.implied(RLCA), ZLine.imm8(XOR, 1), ZLine.imm8(AND, 1))
            case _ =>
              // TODO: helper functions to convert flags to booleans, to make code smaller
          }
        }
        if (ctx.options.flag(CompilationFlag.OptimizeForSpeed)) {
          val skip = ctx.env.nextLabel("bo")
          condition ++ List(
            ZLine.ldImm8(ZRegister.A, 1),
            ZLine.jumpR(ctx, skip),
            ZLine.label(label),
            ZLine.ldImm8(ZRegister.A, 0),
            ZLine.label(skip)
          )
        } else {
          conditionWithoutJump ++ List(
            ZLine.ldImm8(ZRegister.A, 0),
            condition.last,
            ZLine.register(INC, ZRegister.A),
            ZLine.label(label)
          )
        }
      case _ =>
        println(sourceType)
        ???
    }
  }

  def areSZFlagsBasedOnA(code: List[ZLine]): Boolean = {
    for (line <- code.reverse) {
      line.opcode match {
        case ADD | SUB | SBC | ADC | XOR | OR | AND => return true
        case CP => return line.registers == OneRegister(ZRegister.IMM_8) && line.parameter.isProvablyZero
        case LD | LD_16 | NOP | POP => ()
        case RR | RL | SLA | SLL | RRC | RLC | SRA | SRL => return line.registers == OneRegister(ZRegister.A)
        case _ => return false
      }
    }
    false
  }

  def compile8BitTo(ctx: CompilationContext, expression: Expression, register: ZRegister.Value): List[ZLine] = {
    if (ZRegister.A == register) compileToA(ctx, expression) else {
      val toA = compileToA(ctx, expression)
      if (toA.isEmpty) Nil else {
        toA.last match {
          case ZLine0(ZOpcode.LD, TwoRegisters(ZRegister.A, source), _) if source == register =>
            toA.init
          case ZLine0(ZOpcode.LD, TwoRegisters(ZRegister.A, source@(ZRegister.B | ZRegister.C | ZRegister.D | ZRegister.E | ZRegister.MEM_HL)), _) =>
            toA.init :+ ZLine.ld8(register, source)
          case ZLine0(ZOpcode.LD, TwoRegisters(ZRegister.A, ZRegister.IMM_8), param) if toA.size == 1 =>
            List(ZLine.ldImm8(register, param))
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
    import ZRegister._
    val innerPart = compileToHL(ctx, expression.inner)
    innerPart match {
      case List(ZLine0(LD_16, TwoRegisters(HL, IMM_16), c)) =>
        List(ZLine(LD_16, TwoRegisters(HL, IMM_16), c + expression.offset))
      case _ =>
        innerPart ++ (expression.offset match {
          case 0 => Nil
          case i if i < 5 => List.fill(i)(ZLine.register(INC_16, ZRegister.HL)) // TODO: a better threshold
          case _ => List(ZLine.ldImm8(ZRegister.C, expression.offset), ZLine.ldImm8(ZRegister.B, 0), ZLine.registers(ADD_16, ZRegister.HL, ZRegister.BC))
        })
    }
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
    case (lddesp@ZLine0(LD_DESP, _, param)) :: xs => lddesp.copy(parameter = param + 2) :: fixTsx(ctx, xs)
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
    val exprType = AbstractExpressionCompiler.getExpressionType(ctx, expression)
    if (branches != NoBranching) {
      (exprType, branches) match {
        case (FatBooleanType, _) =>
          return compile(ctx, FunctionCallExpression("!=", List(expression, LiteralExpression(0, 1))), target, branches)
        case (ConstantBooleanType(_, false), BranchIfTrue(_)) | (ConstantBooleanType(_, true), BranchIfFalse(_))=>
          return compile(ctx, expression, target, NoBranching)
        case (ConstantBooleanType(_, true), BranchIfTrue(x)) =>
          return compile(ctx, expression, target, NoBranching) :+ ZLine.jump(x)
        case (ConstantBooleanType(_, false), BranchIfFalse(x)) =>
          return compile(ctx, expression, target, NoBranching) :+ ZLine.jump(x)
        case _ => ()
      }
    }
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
            env.get[VariableLikeThing](name) match {
              case s: StackOffsetThing =>
                s.subbyte match {
                  case None => targetifyHL(ctx, target, calculateStackAddressToHL(ctx, s.offset))
                  case Some(0) => targetifyA(ctx, target, calculateStackAddressToHL(ctx, s.offset) :+ ZLine.ld8(ZRegister.A, ZRegister.L), isSigned = false)
                  case Some(1) => targetifyA(ctx, target, calculateStackAddressToHL(ctx, s.offset) :+ ZLine.ld8(ZRegister.A, ZRegister.H), isSigned = false)
                  case _ => throw new IllegalArgumentException
                }
              case v: VariableInMemory =>
                import ZRegister._
                v.typ.size match {
                  case 0 => ???
                  case 1 => loadByte(ctx, v.toAddress, target, v.isVolatile, v.typ.isSigned)
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
                      // TODO: signed int24s
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
                      // TODO: signed int24s
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
                      // TODO: signed int24s
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
                        if (ctx.options.flag(CompilationFlag.EmitIntel8085Opcodes) && ctx.options.flag(CompilationFlag.EmitIllegals)) {
                          List(ZLine.imm8(LD_DESP, ctx.extraStackOffset + v.baseOffset), ZLine.implied(LHLX))
                        } else {
                          loadHL ++ List(ZLine.ld8(A, MEM_HL), ZLine.register(INC_16, HL), ZLine.ld8(H, MEM_HL), ZLine.ld8(L, A))
                        }
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
                      // TODO: signed int24s
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
              case List(ZLine0(LD_16, TwoRegisters(ZRegister.HL, ZRegister.IMM_16), addr)) => loadByte(ctx, addr, target, volatile = false, signExtend = false)
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
                case ZExpressionTarget.EHL =>
                  List(
                    ZLine.ld8(ZRegister.A, ZRegister.MEM_HL),
                    ZLine.register(INC_16, ZRegister.HL),
                    ZLine.ld8(ZRegister.H, ZRegister.MEM_HL),
                    ZLine.ld8(ZRegister.L, ZRegister.A),
                    ZLine.ldImm8(ZRegister.E, 0)) // TODO
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
              case 3 => target match {
                case ZExpressionTarget.NOTHING => Nil
                case ZExpressionTarget.EHL | ZExpressionTarget.DEHL =>
                  if (ctx.options.flag(CompilationFlag.EmitIntel8080Opcodes)) {
                    List(
                      ZLine.ld8(ZRegister.E, ZRegister.MEM_HL),
                      ZLine.register(INC_16, ZRegister.HL),
                      ZLine.ld8(ZRegister.D, ZRegister.MEM_HL),
                      ZLine.register(INC_16, ZRegister.HL),
                      ZLine.ld8(ZRegister.L, ZRegister.MEM_HL),
                      ZLine.ldImm8(ZRegister.H, 0),
                      ZLine.implied(EX_DE_HL)) // TODO
                  } else {
                    List(
                      ZLine.ld8(ZRegister.C, ZRegister.MEM_HL),
                      ZLine.register(INC_16, ZRegister.HL),
                      ZLine.ld8(ZRegister.B, ZRegister.MEM_HL),
                      ZLine.register(INC_16, ZRegister.HL),
                      ZLine.ld8(ZRegister.E, ZRegister.MEM_HL),
                      ZLine.ld8(ZRegister.L, ZRegister.C),
                      ZLine.ld8(ZRegister.H, ZRegister.B),
                      ZLine.ldImm8(ZRegister.D, 0)) // TODO
                  }
              }
              case 4 => target match {
                case ZExpressionTarget.NOTHING => Nil
                case ZExpressionTarget.DEHL =>
                  if (ctx.options.flag(CompilationFlag.EmitIntel8080Opcodes)) {
                    List(
                      ZLine.ld8(ZRegister.E, ZRegister.MEM_HL),
                      ZLine.register(INC_16, ZRegister.HL),
                      ZLine.ld8(ZRegister.D, ZRegister.MEM_HL),
                      ZLine.register(INC_16, ZRegister.HL),
                      ZLine.ld8(ZRegister.A, ZRegister.MEM_HL),
                      ZLine.register(INC_16, ZRegister.HL),
                      ZLine.ld8(ZRegister.H, ZRegister.MEM_HL),
                      ZLine.ld8(ZRegister.L, ZRegister.A),
                      ZLine.implied(EX_DE_HL)) // TODO
                  } else {
                    List(
                      ZLine.ld8(ZRegister.C, ZRegister.MEM_HL),
                      ZLine.register(INC_16, ZRegister.HL),
                      ZLine.ld8(ZRegister.B, ZRegister.MEM_HL),
                      ZLine.register(INC_16, ZRegister.HL),
                      ZLine.ld8(ZRegister.E, ZRegister.MEM_HL),
                      ZLine.register(INC_16, ZRegister.HL),
                      ZLine.ld8(ZRegister.D, ZRegister.MEM_HL),
                      ZLine.ld8(ZRegister.L, ZRegister.C),
                      ZLine.ld8(ZRegister.H, ZRegister.B)) // TODO
                  }
              }
              case _ =>
                ctx.log.error("Cannot read a large object indirectly")
                Nil
            })
          case f@FunctionCallExpression(name, params) =>
            name match {
              case "call" =>
                val callLine = ZLine(CALL, NoRegisters, env.get[ThingInMemory]("call").toAddress)
                params match {
                  case List(fp) =>
                    getExpressionType(ctx, fp) match {
                      case FunctionPointerType(_, _, _, _, Some(v)) if (v.name == "void") =>
                        compileToDE(ctx, fp) :+ callLine
                      case _ =>
                        ctx.log.error("Not a function pointer", fp.position)
                        compile(ctx, fp, ZExpressionTarget.NOTHING, BranchSpec.None)
                    }
                  case List(fp, param) =>
                    getExpressionType(ctx, fp) match {
                      case FunctionPointerType(_, _, _, Some(pt), Some(v)) =>
                        if (pt.size > 2 || pt.size < 1) {
                          ctx.log.error("Invalid parameter type", param.position)
                          compileToHL(ctx, fp) ++ compile(ctx, param, ZExpressionTarget.NOTHING)
                        } else if (getExpressionType(ctx, param).isAssignableTo(pt)) {
                          pt.size match {
                            case 1 =>
                              compileToDE(ctx, fp) ++ stashDEIfChanged(ctx, compileToA(ctx, param)) :+ callLine
                            case 2 =>
                              compileToDE(ctx, fp) ++ stashDEIfChanged(ctx, compileToHL(ctx, param)) :+ callLine
                          }
                        } else {
                          ctx.log.error("Invalid parameter type", param.position)
                          compileToHL(ctx, fp) ++ compile(ctx, param, ZExpressionTarget.NOTHING)
                        }
                      case _ =>
                        ctx.log.error("Not a function pointer", fp.position)
                        compile(ctx, fp, ZExpressionTarget.NOTHING) ++ compile(ctx, param, ZExpressionTarget.NOTHING)
                    }
                  case _ =>
                    ctx.log.error("Invalid call syntax", f.position)
                    Nil
                }
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
                    targetifyHL(ctx, target, Z80Multiply.compile16BitMultiplyToHL(ctx, params(0), params(1)))
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
                    Z80Multiply.compile16BitInPlaceMultiply(ctx, l, r)
                }
              case "/=" | "%%=" =>
                assertSizesForDivision(ctx, params, inPlace = true)
                val (l, r, size) = assertArithmeticAssignmentLike(ctx, params)
                size match {
                  case 1 =>
                    calculateAddressToAppropriatePointer(ctx, l, forWriting = true) match {
                      case Some((LocalVariableAddressViaHL, List(ZLine0(LD_16, TwoRegisters(ZRegister.HL, ZRegister.IMM_16), addr)))) =>
                        Z80Multiply.compileUnsignedByteDivision(ctx, Right(l), r, f.functionName == "%%=") :+ ZLine.ldAbs8(addr, ZRegister.A)
                      case Some((LocalVariableAddressViaHL, code)) =>
                        code ++ (stashHLIfChanged(ctx, Z80Multiply.compileUnsignedByteDivision(ctx, Left(LocalVariableAddressViaHL), r, f.functionName == "%%=")) :+ ZLine.ld8(ZRegister.MEM_HL, ZRegister.A))
                      case Some((lvo, code)) =>
                        code ++ (Z80Multiply.compileUnsignedByteDivision(ctx, Left(lvo), r, f.functionName == "%%=") :+ ZLine.ld8(lvo, ZRegister.A))
                      case None =>
                        ctx.log.error("Invalid left-hand side", l.position)
                        Nil
                    }
                  case 2 =>
                    val modulo = f.functionName == "%%="
                    val rhsWord = getExpressionType(ctx, r).size == 2
                    if (modulo && !rhsWord) {
                      calculateAddressToAppropriatePointer(ctx, l, forWriting = true) match {
                        case Some((LocalVariableAddressViaHL, List(ZLine0(LD_16, TwoRegisters(ZRegister.HL, ZRegister.IMM_16), addr)))) =>
                          Z80Multiply.compileUnsignedWordDivision(ctx, Right(l), r, modulo = true, rhsWord = false) ++ List(
                            ZLine.ldAbs8(addr, ZRegister.A),
                            ZLine.register(XOR, ZRegister.A),
                            ZLine.ldAbs8(addr+1, ZRegister.A)
                          )
                        case Some((lvo@LocalVariableAddressViaHL, code)) =>
                          code ++ stashHLIfChanged(ctx, Z80Multiply.compileUnsignedWordDivision(ctx, Left(lvo), r, modulo = true, rhsWord = false)) ++ List(
                            ZLine.ld8(ZRegister.MEM_HL, ZRegister.A),
                            ZLine.register(INC_16, ZRegister.HL),
                            ZLine.ldImm8(ZRegister.MEM_HL, 0)
                          )
                        case Some((lvo@LocalVariableAddressViaIX(offset), code)) =>
                          code ++ Z80Multiply.compileUnsignedWordDivision(ctx, Left(lvo), r, modulo = true, rhsWord = false) ++ List(
                            ZLine.ldViaIx(offset, ZRegister.A),
                            ZLine.ld0ViaIx(offset + 1)
                          )
                        case Some((lvo@LocalVariableAddressViaIY(offset), code)) =>
                          code ++ Z80Multiply.compileUnsignedWordDivision(ctx, Left(lvo), r, modulo = true, rhsWord = false) ++ List(
                            ZLine.ldViaIy(offset, ZRegister.A),
                            ZLine.ld0ViaIy(offset + 1)
                          )
                        case None =>
                          ctx.log.error("Invalid left-hand side", l.position)
                          Nil
                      }
                    } else {
                      calculateAddressToAppropriatePointer(ctx, l, forWriting = true) match {
                        case Some((lvo@LocalVariableAddressViaHL, code)) =>
                          code ++
                            stashHLIfChanged(ctx,
                              Z80Multiply.compileUnsignedWordDivision(ctx, Left(LocalVariableAddressViaHL), r, modulo, rhsWord) ++ (
                                if (ctx.options.flags(CompilationFlag.EmitIntel8080Opcodes)) List(ZLine.implied(EX_DE_HL))
                                else List(ZLine.ld8(ZRegister.E, ZRegister.L), ZLine.ld8(ZRegister.D, ZRegister.H))
                                )
                            ) ++
                            List(
                            ZLine.ld8(ZRegister.MEM_HL, ZRegister.E),
                            ZLine.register(INC_16, ZRegister.HL),
                            ZLine.ld8(ZRegister.MEM_HL, ZRegister.D)
                          )
                        case Some((lvo@LocalVariableAddressViaIX(offset), code)) =>
                          code ++
                            Z80Multiply.compileUnsignedWordDivision(ctx, Left(lvo), r, modulo, rhsWord) ++
                            storeHLViaIX(ctx, offset, 2, signedSource = false)
                        case Some((lvo@LocalVariableAddressViaIY(offset), code)) =>
                          code ++
                            Z80Multiply.compileUnsignedWordDivision(ctx, Left(lvo), r, modulo, rhsWord) ++
                            storeHLViaIY(ctx, offset, 2, signedSource = false)
                        case _ =>
                          ctx.log.error("Invalid left-hand side", l.position)
                          Nil
                      }
                    }
                }
              case "/" | "%%" =>
                assertSizesForDivision(ctx, params, inPlace = false)
                val (l, r, size) = assertArithmeticBinary(ctx, params)
                val modulo = f.functionName == "%%"
                val rhsWord = getExpressionType(ctx, r).size == 2
                size match {
                  case 1 =>
                    targetifyA(ctx, target, Z80Multiply.compileUnsignedByteDivision(ctx, Right(l), r, modulo), isSigned = false)
                  case 2 =>
                    if (modulo && !rhsWord) {
                      targetifyA(ctx, target, Z80Multiply.compileUnsignedWordDivision(ctx, Right(l), r, modulo = true, rhsWord = false), isSigned = false)
                    } else {
                      targetifyHL(ctx, target, Z80Multiply.compileUnsignedWordDivision(ctx, Right(l), r, modulo, rhsWord))
                    }
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
                    if (sourceType.isBoollike) {
                      return targetifyA(ctx, target, compileToFatBooleanInA(ctx, params.head), isSigned = false)
                    }
                    return typ.size match {
                        // TODO: alternating signedness?
                      case 1 => targetifyA(ctx, target, compileToA(ctx, params.head), isSigned = typ.isSigned)
                      case 2 => targetifyHL(ctx, target, compileToHL(ctx, params.head))
                      case _ => ???
                    }
                  case None =>
                  // fallthrough to the lookup below
                }
                lookupFunction(ctx, f) match {
                  case function: MacroFunction =>
                    val (paramPreparation, statements) = Z80MacroExpander.inlineFunction(ctx, function, params, expression.position)
                    paramPreparation ++ statements.flatMap { s =>
                      Z80StatementCompiler.compile(ctx, s) match {
                        case (code, Nil) => code
                        case (code, _) => ctx.log.error("Invalid statement in macro expansion", expression.position); code
                      }
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
                      case AssemblyParamSignature(paramConvs) =>val pairs = params.zip(paramConvs)
                        val viaMemory = pairs.flatMap {
                          case (paramExpr, AssemblyParam(typ, paramVar: VariableInMemory, AssemblyParameterPassingBehaviour.Copy)) =>
                            ctx.log.error("Variable parameters to assembly functions are not supported", expression.position)
                            Nil
                          case _ => Nil
                        }
                        val viaRegisters = pairs.flatMap {
                          case (paramExpr, AssemblyParam(typ, paramVar@ZRegisterVariable(register, _), AssemblyParameterPassingBehaviour.Copy)) =>
                            if (typ.size != ZRegister.registerSize(register)) {
                              ctx.log.error(s"Type ${typ.name} and register $register are of different sizes", expression.position)
                            }
                            val compi = ZRegister.registerSize(register) match {
                              case 1 => compile8BitTo(ctx, paramExpr, register)
                              case 2 => register match {
                                case ZRegister.HL => compileToHL(ctx, paramExpr)
                                case ZRegister.BC => compileToBC(ctx, paramExpr)
                                case ZRegister.DE => compileToDE(ctx, paramExpr)
                                case _ =>
                                  ctx.log.error(s"Unsupported register $register", expression.position)
                                  Nil
                              }
                            }
                            Some(register -> compi)
                          case _ => Nil
                        } match {
                          case Seq() => Nil
                          case Seq((_, param)) => param
                          case Seq((ZRegister.HL, phl), (_, pxx)) => phl ++ stashHLIfChanged(ctx, pxx)
                          case Seq((_, pxx), (ZRegister.HL, phl)) => phl ++ stashHLIfChanged(ctx, pxx)
                          case Seq((ZRegister.DE, pde), (_, pxx)) => pde ++ stashDEIfChanged(ctx, pxx)
                          case Seq((_, pxx), (ZRegister.DE, pde)) => pde ++ stashDEIfChanged(ctx, pxx)
                          case Seq((ZRegister.BC, pbc), (_, pxx)) => pbc ++ stashBCIfChanged(ctx, pxx)
                          case Seq((_, pxx), (ZRegister.BC, pbc)) => pbc ++ stashBCIfChanged(ctx, pxx)
                          case other =>
                            ctx.log.warn("Unsupported register parameter combination: " + other.map(_._1.toString).mkString("(", ",", ")"), expression.position)
                            other.flatMap(_._2) // TODO : make sure all registers are passed in correctly
                        }
                        viaMemory ++ viaRegisters :+ ZLine(CALL, NoRegisters, function.toAddress)
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
      // TODO: is this correct?
      List(ZLine.ldImm16(ZRegister.HL, baseOffset + ctx.extraStackOffset), ZLine.registers(ZOpcode.ADD_16, ZRegister.HL, ZRegister.SP))
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
    val elementSize = pointy.elementType.size
    val logElemSize = elementSize match {
      case 1 => 0
      case 2 => 1
      case _ =>
        ctx.log.error("Cannot access a large object this way", i.position)
        0
    }
    pointy match {
      case ConstantPointy(baseAddr, _, sizeInBytes, _, _, _, alignment, readOnly) =>
        if (forWriting && readOnly) {
          ctx.log.error("Writing to a constant array", i.position)
        }
        env.evalVariableAndConstantSubParts(i.index) match {
          case (None, offset) => List(ZLine.ldImm16(ZRegister.HL, (baseAddr + offset * elementSize).quickSimplify))
          case (Some(index), offset) =>
            val constantPart = (baseAddr + offset * elementSize).quickSimplify
            if (getExpressionType(ctx, i.index).size == 1 && sizeInBytes.exists(_ < 256) && alignment == WithinPageAlignment) {
              compileToA(ctx, i.index) ++ List.fill(logElemSize)(ZLine.register(ADD, ZRegister.A)) ++ List(
                ZLine.imm8(ADD, constantPart.loByte),
                ZLine.ld8(ZRegister.L, ZRegister.A),
                ZLine.ldImm8(ZRegister.H, constantPart.hiByte))
            } else {
              List(ZLine.ldImm16(ZRegister.BC, constantPart)) ++
                stashBCIfChanged(ctx, compileToHL(ctx, index)) ++
                List.fill(logElemSize)(ZLine.registers(ADD_16, ZRegister.HL, ZRegister.HL)) ++
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
                List(ZLine.ldAbs16(ZRegister.HL, varAddr)) ++
                  List.fill(elementSize)(ZLine.registers(ADD_16, ZRegister.HL, ZRegister.BC))
            } else {
              // TODO: is this reasonable?
              compileToBC(ctx, i.index) ++
                List(
                  ZLine.ldAbs8(ZRegister.A, varAddr),
                  ZLine.ld8(ZRegister.L, ZRegister.A),
                  ZLine.ldAbs8(ZRegister.A, varAddr + 1),
                  ZLine.ld8(ZRegister.H, ZRegister.A)) ++
                  List.fill(elementSize)(ZLine.registers(ADD_16, ZRegister.HL, ZRegister.BC))
            }
        }
      case _: StackVariablePointy =>
        compileToHL(ctx, VariableExpression(i.name).pos(i.position)) ++
          stashHLIfChanged(ctx, compileToBC(ctx, i.index)) ++
          List.fill(elementSize)(ZLine.registers(ADD_16, ZRegister.HL, ZRegister.BC))
    }
  }

  def loadByte(ctx: CompilationContext, sourceAddr: Constant, target: ZExpressionTarget.Value, volatile: Boolean, signExtend: Boolean): List[ZLine] = {
    import ZRegister._
    import ZLine.{ld8, ldImm8, ldAbs8, ldImm16}
    val elidability = if (volatile) Elidability.Volatile else Elidability.Elidable
    target match {
      case ZExpressionTarget.NOTHING => Nil
      case ZExpressionTarget.A => List(ZLine.ldAbs8(ZRegister.A, sourceAddr, elidability))
      case ZExpressionTarget.HL =>
        if (signExtend) {
          List(ldAbs8(A, sourceAddr, elidability), ld8(L, A)) ++
            signExtendHighestByte(ctx, A, signExtend) ++
            List(ld8(H, A))
        } else {
          List(ldAbs8(A, sourceAddr, elidability), ld8(L, A), ldImm8(H, 0))
        }
      case ZExpressionTarget.BC =>
        if (signExtend) {
          List(ldAbs8(A, sourceAddr, elidability), ld8(L, A)) ++
            signExtendHighestByte(ctx, A, signExtend) ++
            List(ld8(H, A))
        } else {
          List(ldAbs8(A, sourceAddr, elidability), ld8(C, A), ldImm8(B, 0))
        }
      case ZExpressionTarget.DE =>
        if (signExtend) {
          List(ldAbs8(A, sourceAddr, elidability), ld8(E, A)) ++
            signExtendHighestByte(ctx, A, signExtend) ++
            List(ld8(D, A))
        } else {
          List(ldAbs8(A, sourceAddr, elidability), ld8(E, A), ldImm8(D, 0))
        }
      case ZExpressionTarget.EHL =>
        if (signExtend) {
          List(ldAbs8(A, sourceAddr, elidability), ld8(L, A)) ++
            signExtendHighestByte(ctx, A, signExtend) ++
            List(ld8(H, A), ld8(E, A))
        } else {
          List(ldAbs8(ZRegister.A, sourceAddr, elidability), ld8(L, A), ldImm8(H, 0), ldImm8(E, 0))
        }
      case ZExpressionTarget.DEHL =>
        if (signExtend) {
          List(ldAbs8(A, sourceAddr, elidability), ld8(L, A)) ++
            signExtendHighestByte(ctx, A, signExtend) ++
            List(ld8(H, A), ld8(E, A), ld8(D, A))
        } else {
          List(ldAbs8(A, sourceAddr, elidability), ld8(L, A), ldImm8(H, 0), ldImm16(DE, 0))
        }
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
    if (bytes <= 0) return Nil
    val prepareA = if (signedSource) {
      signExtendHighestByte(ctx, hiRegister)
    } else {
      List(ZLine.ldImm8(ZRegister.A, 0))
    }
    val fillUpperBytes = List.tabulate(bytes)(i => ZLine.ldAbs8((targetAddr + i).quickSimplify, ZRegister.A))
    prepareA ++ fillUpperBytes
  }

  private def signExtendHighestByte(ctx: CompilationContext, hiRegister: ZRegister.Value, signedSource: Boolean = true): List[ZLine] = {
    if (!signedSource) {
      return List(ZLine.ldImm8(ZRegister.A, 0))
    }
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
    if (bytes <= 0) return Nil
    val prepareA = if (signedSource) {
      signExtendHighestByte(ctx, hiRegister)
    } else {
      List(ZLine.ldImm8(ZRegister.A, 0))
    }
    val fillUpperBytes = List.tabulate(bytes)(i => ZLine.ldViaIx(targetOffset + i, ZRegister.A))
    prepareA ++ fillUpperBytes
  }

  def signExtendViaIY(ctx: CompilationContext, targetOffset: Int, hiRegister: ZRegister.Value, bytes: Int, signedSource: Boolean): List[ZLine] = {
    if (bytes <= 0) return Nil
    val prepareA = if (signedSource) {
      signExtendHighestByte(ctx, hiRegister)
    } else {
      List(ZLine.ldImm8(ZRegister.A, 0))
    }
    val fillUpperBytes = List.tabulate(bytes)(i => ZLine.ldViaIy(targetOffset + i, ZRegister.A))
    prepareA ++ fillUpperBytes
  }

  def signExtendViaHL(ctx: CompilationContext, hiRegister: ZRegister.Value, bytes: Int, signedSource: Boolean): List[ZLine] = {
    if (bytes <= 0) return Nil
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

  def storeEHL(ctx: CompilationContext, targetAddr: Constant, targetSize: Int, signedSource: Boolean): List[ZLine] = {
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
          List(
            ZLine.ldAbs16(targetAddr, ZRegister.HL),
            ZLine.ld8(ZRegister.A, ZRegister.E),
            ZLine.ldAbs8(targetAddr + 2, ZRegister.A)) ++ signExtend(ctx, targetAddr + 3, ZRegister.E, n - 3, signedSource)
        } else {
          List(
            ZLine.ld8(ZRegister.A, ZRegister.L),
            ZLine.ldAbs8(targetAddr, ZRegister.A),
            ZLine.ld8(ZRegister.A, ZRegister.H),
            ZLine.ldAbs8(targetAddr + 1, ZRegister.A),
            ZLine.ld8(ZRegister.A, ZRegister.E),
            ZLine.ldAbs8(targetAddr + 2, ZRegister.A)) ++ signExtend(ctx, targetAddr + 3, ZRegister.E, n - 3, signedSource)
        }
    }
  }

  def storeDEHL(ctx: CompilationContext, targetAddr: Constant, targetSize: Int, signedSource: Boolean): List[ZLine] = {
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
      case 3 =>
        if (ctx.options.flag(CompilationFlag.EmitIntel8080Opcodes)) {
          List(
            ZLine.ldAbs16(targetAddr, ZRegister.HL),
            ZLine.ld8(ZRegister.A, ZRegister.E),
            ZLine.ldAbs8(targetAddr + 2, ZRegister.A))
        } else {
          List(
            ZLine.ld8(ZRegister.A, ZRegister.L),
            ZLine.ldAbs8(targetAddr, ZRegister.A),
            ZLine.ld8(ZRegister.A, ZRegister.H),
            ZLine.ldAbs8(targetAddr + 1, ZRegister.A),
            ZLine.ld8(ZRegister.A, ZRegister.E),
            ZLine.ldAbs8(targetAddr + 2, ZRegister.A))
        }
      case n =>
        if (ctx.options.flag(CompilationFlag.EmitIntel8080Opcodes)){
          List(
            ZLine.ldAbs16(targetAddr, ZRegister.HL),
            ZLine.ld8(ZRegister.A, ZRegister.E),
            ZLine.ldAbs8(targetAddr + 2, ZRegister.A),
            ZLine.ld8(ZRegister.A, ZRegister.D),
            ZLine.ldAbs8(targetAddr + 3, ZRegister.A)) ++ signExtend(ctx, targetAddr + 4, ZRegister.D, n - 4, signedSource)
        } else {
          List(
            ZLine.ld8(ZRegister.A, ZRegister.L),
            ZLine.ldAbs8(targetAddr, ZRegister.A),
            ZLine.ld8(ZRegister.A, ZRegister.H),
            ZLine.ldAbs8(targetAddr + 1, ZRegister.A),
            ZLine.ld8(ZRegister.A, ZRegister.E),
            ZLine.ldAbs8(targetAddr + 2, ZRegister.A),
            ZLine.ld8(ZRegister.A, ZRegister.D),
            ZLine.ldAbs8(targetAddr + 3, ZRegister.A)) ++ signExtend(ctx, targetAddr + 4, ZRegister.D, n - 4, signedSource)
        }
    }
  }

  def storeHLViaIX(ctx: CompilationContext, offset: Int, targetSize: Int, signedSource: Boolean): List[ZLine] = {
    targetSize match {
      case 0 => Nil
      case 1 => List(ZLine.ldViaIx(offset, ZRegister.L))
      case n => List(ZLine.ldViaIx(offset, ZRegister.L), ZLine.ldViaIx(offset + 1, ZRegister.H)) ++ signExtendViaIX(ctx, offset + 2, ZRegister.H, n - 2, signedSource)
    }
  }

  def storeEHLViaIX(ctx: CompilationContext, offset: Int, targetSize: Int, signedSource: Boolean): List[ZLine] = {
    import ZRegister._
    targetSize match {
      case 0 => Nil
      case 1 => List(ZLine.ldViaIx(offset, L))
      case 2 => List(ZLine.ldViaIx(offset, L), ZLine.ldViaIx(offset + 1, H))
      case n => List(ZLine.ldViaIx(offset, L), ZLine.ldViaIx(offset + 1, H), ZLine.ldViaIx(offset + 2, E)) ++ signExtendViaIX(ctx, offset + 3, E, n - 3, signedSource)
    }
  }

  def storeDEHLViaIX(ctx: CompilationContext, offset: Int, targetSize: Int, signedSource: Boolean): List[ZLine] = {
    import ZRegister._
    targetSize match {
      case 0 => Nil
      case 1 => List(ZLine.ldViaIx(offset, L))
      case 2 => List(ZLine.ldViaIx(offset, L), ZLine.ldViaIx(offset + 1, H))
      case 3 => List(ZLine.ldViaIx(offset, L), ZLine.ldViaIx(offset + 1, H), ZLine.ldViaIx(offset + 2, E))
      case n => List(ZLine.ldViaIx(offset, L), ZLine.ldViaIx(offset + 1, H), ZLine.ldViaIx(offset + 2, E), ZLine.ldViaIx(offset + 3, D)) ++ signExtendViaIX(ctx, offset + 4, D, n - 4, signedSource)
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

  def storeEHLViaIY(ctx: CompilationContext, offset: Int, targetSize: Int, signedSource: Boolean): List[ZLine] = {
    import ZRegister._
    targetSize match {
      case 0 => Nil
      case 1 => List(ZLine.ldViaIy(offset, L))
      case 2 => List(ZLine.ldViaIy(offset, L), ZLine.ldViaIy(offset + 1, H))
      case n => List(ZLine.ldViaIy(offset, L), ZLine.ldViaIy(offset + 1, H), ZLine.ldViaIy(offset + 2, E)) ++ signExtendViaIY(ctx, offset + 3, E, n - 3, signedSource)
    }
  }

  def storeDEHLViaIY(ctx: CompilationContext, offset: Int, targetSize: Int, signedSource: Boolean): List[ZLine] = {
    import ZRegister._
    targetSize match {
      case 0 => Nil
      case 1 => List(ZLine.ldViaIy(offset, L))
      case 2 => List(ZLine.ldViaIy(offset, L), ZLine.ldViaIy(offset + 1, H))
      case 3 => List(ZLine.ldViaIy(offset, L), ZLine.ldViaIy(offset + 1, H), ZLine.ldViaIy(offset + 2, E))
      case n => List(ZLine.ldViaIy(offset, L), ZLine.ldViaIy(offset + 1, H), ZLine.ldViaIy(offset + 2, E), ZLine.ldViaIy(offset + 3, D)) ++ signExtendViaIY(ctx, offset + 4, D, n - 4, signedSource)
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

  def storeConstantWord(ctx: CompilationContext, target: LhsExpression, source: Constant, signedSource: Boolean): List[ZLine] = {
    target match {
      case e: DerefExpression =>
        compileDerefPointer(ctx, e) ++ List(
          ZLine.ldImm8(ZRegister.MEM_HL, source.loByte),
          ZLine.register(INC_16, ZRegister.HL),
          ZLine.ldImm8(ZRegister.MEM_HL, source.hiByte))
      case _ => ZLine.ldImm16(ZRegister.HL, source) :: storeHL(ctx, target, signedSource)
    }
  }

  def storeHL(ctx: CompilationContext, target: LhsExpression, signedSource: Boolean): List[ZLine] = {
    import ZRegister._
    val env = ctx.env
    target match {
      case VariableExpression(vname) =>
        env.get[Variable](vname) match {
          case v: VariableInMemory => storeHL(ctx, v.toAddress, v.typ.size, signedSource)
          case v: StackVariable =>
            if (ctx.options.flag(CompilationFlag.UseIxForStack)){
              storeHLViaIX(ctx, v.baseOffset, v.typ.size, signedSource)
            } else if (ctx.options.flag(CompilationFlag.UseIyForStack)){
              storeHLViaIY(ctx, v.baseOffset, v.typ.size, signedSource)
            } else if (ctx.options.flag(CompilationFlag.EmitIntel8085Opcodes) && ctx.options.flag(CompilationFlag.EmitIllegals)) {
              if (v.typ.size > 2) {
                List(ZLine.register(PUSH, DE),
                  ZLine.imm8(LD_DESP, v.baseOffset + 2),
                  ZLine.implied(SHLX),
                  ZLine.register(INC_16, DE)
                ) ++ signExtendHighestByte(ctx, H) ++ List.fill(v.typ.size - 2)(List(
                  ZLine.register(INC_16, DE),
                  ZLine.ld8(MEM_DE, A)
                )).flatten ++ List(
                  ZLine.register(POP, DE))
              } else {
                List(ZLine.register(PUSH, DE),
                  ZLine.imm8(LD_DESP, v.baseOffset + 2),
                  ZLine.implied(SHLX),
                  ZLine.register(POP, DE))
              }
            } else {
              List(ZLine.register(PUSH, DE)) ++
                (if (ctx.options.flag(CompilationFlag.EmitIntel8080Opcodes))
                  List(ZLine.implied(EX_DE_HL))
                else List(ZLine.ld8(D, H), ZLine.ld8(E, L))) ++
                fixTsx(ctx, calculateStackAddressToHL(ctx, v)) ++
                List(
                  ZLine.ld8(MEM_HL, E),
                  ZLine.register(INC_16, HL),
                  ZLine.ld8(MEM_HL, D)) ++
                (if (v.typ.size > 2)
                  signExtendHighestByte(ctx, D) ++
                    List.fill(v.typ.size - 2)(List(
                      ZLine.register(INC_16, HL),
                      ZLine.ld8(MEM_HL, A)
                    )).flatten
                else Nil) ++
                List(ZLine.register(POP, DE))
            }
        }
      case IndexedExpression(pointyName, indexExpr) =>
        env.getPointy(pointyName) match {
          case p: ConstantPointy =>
            env.evalVariableAndConstantSubParts(indexExpr) match {
              case (None, offset) => ZLine.ld8(ZRegister.A, ZRegister.L) :: storeA(ctx, (p.value + offset).quickSimplify, 1, signedSource)
            }
          case _ => ctx.log.fatal("Whee!") // the statement preprocessor should have removed all of those
        }
      case SeparateBytesExpression(hi: LhsExpression, lo: LhsExpression) =>
        Z80ExpressionCompiler.stashHLIfChanged(ctx, ZLine.ld8(ZRegister.A, ZRegister.L) :: storeA(ctx, lo, signedSource)) ++
          (ZLine.ld8(ZRegister.A, ZRegister.H) :: storeA(ctx, hi, signedSource))
      case e:DerefExpression =>
        if (ctx.options.flag(CompilationFlag.EmitIntel8085Opcodes) && ctx.options.flag(CompilationFlag.EmitIllegals)) {
          List(ZLine.register(PUSH, ZRegister.HL)) ++ fixTsx(ctx, compileDerefPointer(ctx, e)) ++ List(
            ZLine.register(POP, ZRegister.DE),
            ZLine.implied(EX_DE_HL),
            ZLine.implied(SHLX)) ++
            (if (e.targetType.size > 2)
              signExtendHighestByte(ctx, H) ++
                List(ZLine.register(INC_16, DE)) ++
                List.fill(e.targetType.size - 2)(List(
                  ZLine.register(INC_16, DE),
                  ZLine.ld8(MEM_DE, A)
                )).flatten
            else Nil)
        } else {
          List(ZLine.register(PUSH, ZRegister.HL)) ++ fixTsx(ctx, compileDerefPointer(ctx, e)) ++ List(
            ZLine.register(POP, ZRegister.BC),
            ZLine.ld8(ZRegister.MEM_HL, ZRegister.C),
            ZLine.register(INC_16, ZRegister.HL),
            ZLine.ld8(ZRegister.MEM_HL, ZRegister.B)) ++
            (if (e.targetType.size > 2)
              signExtendHighestByte(ctx, B) ++
                List.fill(e.targetType.size - 2)(List(
                  ZLine.register(INC_16, HL),
                  ZLine.ld8(MEM_HL, A)
                )).flatten
            else Nil)
        }
      case _: SeparateBytesExpression =>
        ctx.log.error("Invalid `:`", target.position)
        Nil
    }
  }

  def storeEHL(ctx: CompilationContext, target: LhsExpression, signedSource: Boolean): List[ZLine] = {
    import ZRegister._
    val env = ctx.env
    val targetType = AbstractExpressionCompiler.getExpressionType(ctx, target)
    val preparePointer = target match {
      case VariableExpression(vname) =>
        env.get[Variable](vname) match {
          case v: VariableInMemory => return storeEHL(ctx, v.toAddress, v.typ.size, signedSource)
          case v: StackVariable =>
            if (ctx.options.flag(CompilationFlag.UseIxForStack)){
              return storeEHLViaIX(ctx, v.baseOffset, v.typ.size, signedSource)
            } else if (ctx.options.flag(CompilationFlag.UseIyForStack)){
              return storeEHLViaIY(ctx, v.baseOffset, v.typ.size, signedSource)
            } else {
              calculateStackAddressToHL(ctx, v.baseOffset)
            }
        }
      case e: DerefExpression => compileDerefPointer(ctx, e)
    }
    List(ZLine.register(PUSH, HL)) ++
      stashDEIfChanged(ctx, fixTsx(ctx, preparePointer)) ++
      List(
        ZLine.register(POP, BC),
        ZLine.ld8(MEM_HL, C),
        ZLine.register(INC_16, HL),
        ZLine.ld8(MEM_HL, B),
        ZLine.register(INC_16, HL),
        ZLine.ld8(MEM_HL, E)) ++ (
      if (targetType.size > 3) signExtendHighestByte(ctx, E, signedSource) ++ List.fill(targetType.size - 3)(List(ZLine.register(INC_16, HL), ZLine.ld8(MEM_HL, A))).flatten
      else Nil)
  }

  def storeDEHL(ctx: CompilationContext, target: LhsExpression, signedSource: Boolean): List[ZLine] = {
    import ZRegister._
    val env = ctx.env
    val targetType = AbstractExpressionCompiler.getExpressionType(ctx, target)
    val preparePointer = target match {
      case VariableExpression(vname) =>
        env.get[Variable](vname) match {
          case v: VariableInMemory => return storeDEHL(ctx, v.toAddress, v.typ.size, signedSource)
          case v: StackVariable =>
            if (ctx.options.flag(CompilationFlag.UseIxForStack)){
              return storeDEHLViaIX(ctx, v.baseOffset, v.typ.size, signedSource)
            } else if (ctx.options.flag(CompilationFlag.UseIyForStack)){
              return storeDEHLViaIY(ctx, v.baseOffset, v.typ.size, signedSource)
            } else {
              calculateStackAddressToHL(ctx, v.baseOffset)
            }
        }
      case e: DerefExpression => compileDerefPointer(ctx, e)
    }
    List(ZLine.register(PUSH, HL)) ++
      stashDEIfChanged(ctx, fixTsx(ctx, preparePointer)) ++
      List(
        ZLine.register(POP, BC),
        ZLine.ld8(MEM_HL, C),
        ZLine.register(INC_16, HL),
        ZLine.ld8(MEM_HL, B),
        ZLine.register(INC_16, HL),
        ZLine.ld8(MEM_HL, E),
        ZLine.register(INC_16, HL),
        ZLine.ld8(MEM_HL, D)) ++ (
      if (targetType.size > 4) signExtendHighestByte(ctx, D, signedSource) ++ List.fill(targetType.size - 4)(List(ZLine.register(INC_16, HL), ZLine.ld8(MEM_HL, A))).flatten
      else Nil)
  }

  def storeLarge(ctx: CompilationContext, target: LhsExpression, source: Expression): List[ZLine] = {
    import ZRegister._
    def fuse(firstRead: List[ZLine], firstStore: List[ZLine]): List[ZLine] = {
      // TODO: ?
      (firstRead.last, firstStore.last) match {
        case (
          ZLine0(LD, TwoRegisters(A, _) | TwoRegistersOffset(A, _, _), _),
          ZLine0(LD, TwoRegisters(_, A) | TwoRegistersOffset(_, A, _), _)) =>
          firstRead.init ++ firstStore.init ++ List(firstRead.last, firstStore.last)
      }
    }
    val env = ctx.env
    val targetType = AbstractExpressionCompiler.getExpressionType(ctx, target)
    env.eval(source) match {
      case Some(constant) =>
        val stores = compileByteStores(ctx, target, targetType.size, includeStep = true)
        stores.zipWithIndex.flatMap {
          case (store, ix) =>
            store.init ++ List(ZLine.ldImm8(A, constant.subbyte(ix)), store.last)
        }
      case None =>
        val size = targetType.size
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
            val fixedStores = (stashDEIfChanged(ctx, stores.head.init) :+ stores.head.last) :: stores.tail
            fuse(fixedReads.head, fixedStores.head) ++ fixedReads.tail.zip(fixedStores.tail).flatMap(t => t._1 ++ t._2)
          } else if (reads.tail.exists(_.exists(_.readsRegister(H)))) { // TODO: when?
            ctx.log.debug("Weird branch of storeLarge reached!")
            if (ctx.log.traceEnabled) {
              ctx.log.trace("reads:")
              reads.flatten.foreach(l => ctx.log.trace(l.toString, None))
              ctx.log.trace("stores:")
              stores.flatten.foreach(l => ctx.log.trace(l.toString, None))
            }
            val fixedReads = (reads.head.init ++ List(ZLine.ld8(B, H), reads.head.last)) :: reads.tail.map(_.map {
              case l@ZLine0(LD, TwoRegisters(reg, H), _) => l.copy(registers = TwoRegisters(reg, B))
              case l@ZLine0(LD, TwoRegisters(reg, L), _) => l.copy(registers = TwoRegisters(reg, C))
              case l => l
            })
            val fixedStores = stores.map(_.map {
              case l@ZLine0(LD, TwoRegisters(H, reg), _) => l.copy(registers = TwoRegisters(B, reg))
              case l@ZLine0(LD, TwoRegisters(L, reg), _) => l.copy(registers = TwoRegisters(C, reg))
              case l => l
            })
            fuse(fixedReads.head, fixedStores.head) ++ fixedReads.tail.zip(fixedStores.tail).flatMap(t => t._1 ++ t._2)
          } else {
            fuse(reads.head, stores.head) ++ reads.tail.zip(stores.tail).flatMap(t => t._1 ++ t._2)
          }
        } else {
          fuse(reads.head, stores.head) ++ reads.tail.zip(stores.tail).flatMap(t => t._1 ++ t._2)
        }
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
            env.get[VariableLikeThing](vname) match {
              case s: StackOffsetThing =>
                s.subbyte match {
                  case None =>
                    val list = List(
                      calculateStackAddressToHL(ctx, s.offset) :+ ZLine.ld8(ZRegister.A, ZRegister.L),
                      calculateStackAddressToHL(ctx, s.offset) :+ ZLine.ld8(ZRegister.A, ZRegister.H)) ++ List.fill(size)(List(ZLine.ldImm8(ZRegister.A, 0)))
                    list.take(size)
                  case Some(0) =>
                    val list = (calculateStackAddressToHL(ctx, s.offset) :+ ZLine.ld8(ZRegister.A, ZRegister.L)) :: List.fill(size)(List(ZLine.ldImm8(ZRegister.A, 0)))
                    list.take(size)
                  case Some(1) =>
                    val list = (calculateStackAddressToHL(ctx, s.offset) :+ ZLine.ld8(ZRegister.A, ZRegister.H)) :: List.fill(size)(List(ZLine.ldImm8(ZRegister.A, 0)))
                    list.take(size)
                  case _ => throw new IllegalArgumentException
                }
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
                  case e@DerefExpression(_, _, _) =>
                    import ZRegister._
                    val prepareHL = compileDerefPointer(ctx, e)
                    List.tabulate(size) { i =>
                      if (i == 0) {
                        prepareHL :+ ZLine.ld8(A, MEM_HL)
                      } else if (i < e.targetType.size) {
                        List(ZLine.register(INC_16, HL), ZLine.ld8(A, MEM_HL))
                      } else if (e.targetType.isSigned) {
                        signExtendHighestByte(ctx, ZRegister.MEM_HL)
                      } else {
                        List(ZLine.ldImm8(A, 0))
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
      case de@DerefExpression(inner, offset, targetType) =>
        import ZRegister._
        val prepareHL = compileDerefPointer(ctx, de)
        List.tabulate(size) { i =>
          if (i == 0) {
            prepareHL :+ ZLine.ld8(MEM_HL, A)
          } else if (i < targetType.size) {
            if (includeStep) List(ZLine.register(INC_16, HL), ZLine.ld8(MEM_HL, A))
            else List(ZLine.ld8(MEM_HL, A))
          } else Nil
        }
      case _ =>
        ctx.log.error("Cannot modify large object accessed via such complex expression", lhs.position)
        List.fill(size)(Nil)
    }
  }

}
