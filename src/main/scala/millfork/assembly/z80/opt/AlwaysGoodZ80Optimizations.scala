package millfork.assembly.z80.opt

import millfork.assembly.AssemblyOptimization
import millfork.assembly.z80._
import millfork.assembly.z80.ZOpcode._
import millfork.env.{CompoundConstant, Constant, MathOperator, NumericConstant}
import millfork.node.ZRegister

/**
  * @author Karol Stasiak
  */
object AlwaysGoodZ80Optimizations {

  def change8BitLoadTarget(line: ZLine, newTarget: ZRegister.Value): ZLine = {
    line match {
      case ZLine(LD, TwoRegistersOffset(_, s, o), p, _) => ZLine(LD, TwoRegistersOffset(newTarget, s, o), p)
      case ZLine(LD, TwoRegisters(_, s), p, _) => ZLine(LD, TwoRegisters(newTarget, s), p)
    }
  }

  def for7Registers(f: ZRegister.Value => AssemblyRuleSet) = MultipleAssemblyRules(
    List(ZRegister.A, ZRegister.B, ZRegister.C, ZRegister.D, ZRegister.E, ZRegister.H, ZRegister.L).map(f))

  def for5LargeRegisters(f: ZRegister.Value => AssemblyRuleSet) = MultipleAssemblyRules(
    List(ZRegister.HL, ZRegister.BC, ZRegister.DE, ZRegister.IX, ZRegister.IY).map(f))

  def for6Registers(f: ZRegister.Value => AssemblyRuleSet) = MultipleAssemblyRules(
    List(ZRegister.B, ZRegister.C, ZRegister.D, ZRegister.E, ZRegister.H, ZRegister.L).map(f))

  val UsingKnownValueFromAnotherRegister = new RuleBasedAssemblyOptimization("Using known value from another register",
    needsFlowInfo = FlowInfoRequirement.ForwardFlow,
    for7Registers(register =>
      (Elidable & IsRegular8BitLoadFrom(register) & MatchRegister(register, 0)) ~~> ((code, ctx) =>
        code.map(x => x.copy(
          parameter = NumericConstant(ctx.get[Int](0), 1),
          registers = x.registers match {
            case TwoRegisters(t, _) => TwoRegisters(t, ZRegister.IMM_8)
            case TwoRegistersOffset(t@(ZRegister.MEM_IX_D | ZRegister.MEM_IY_D), _, o) => TwoRegistersOffset(t, ZRegister.IMM_8, o)
            case TwoRegistersOffset(t, ZRegister.MEM_IX_D | ZRegister.MEM_IY_D, _) => TwoRegisters(t, ZRegister.IMM_8)
          }
        )))
    ),
    (Elidable & MatchSourceIxOffsetOf8BitLoad(0) & MatchValueAtMatchedIxOffset(0, 1)) ~~> ((code, ctx) =>
      code.map(x => x.copy(
        parameter = NumericConstant(ctx.get[Int](1), 1),
        registers = x.registers match {
          case TwoRegisters(t, _) => TwoRegisters(t, ZRegister.IMM_8)
          case TwoRegistersOffset(t@(ZRegister.MEM_IX_D | ZRegister.MEM_IY_D), _, o) => TwoRegistersOffset(t, ZRegister.IMM_8, o)
          case TwoRegistersOffset(t, ZRegister.MEM_IX_D | ZRegister.MEM_IY_D, _) => TwoRegisters(t, ZRegister.IMM_8)
        }
      ))
      ),
    for6Registers(register =>
      (Elidable & HasRegisterParam(register) & HasOpcodeIn(Set(AND, ADD, ADC, SUB, SBC, XOR, OR, CP)) & MatchRegister(register, 0)) ~~> ((code, ctx) =>
        code.map(x => x.copy(
          parameter = NumericConstant(ctx.get[Int](0), 1),
          registers = OneRegister(ZRegister.IMM_8)
        )))
    ),
    (Elidable & MatchIxOffset(0) & HasOpcodeIn(Set(AND, ADD, ADC, SUB, SBC, XOR, OR, CP)) & MatchValueAtMatchedIxOffset(0, 1)) ~~> ((code, ctx) =>
      code.map(x => x.copy(
        parameter = NumericConstant(ctx.get[Int](1), 1),
        registers = OneRegister(ZRegister.IMM_8)
      ))
      ),
  )

  val ReloadingKnownValueFromMemory = new RuleBasedAssemblyOptimization("Reloading known value from memory",
    needsFlowInfo = FlowInfoRequirement.ForwardFlow,
    for7Registers(register =>
      Is8BitLoad(ZRegister.MEM_HL, register) ~
        (Linear & Not(Changes(ZRegister.H)) & Not(Changes(ZRegister.L)) & Not(ChangesMemory) & Not(Changes(register)) & Not(IsRegular8BitLoadFrom(ZRegister.MEM_HL))).* ~
        (Elidable & Is8BitLoad(register, ZRegister.MEM_HL)) ~~> { code => code.init }
    ),
    for7Registers(register =>
      Is8BitLoad(ZRegister.MEM_HL, register) ~
        (Linear & Not(Changes(ZRegister.H)) & Not(Changes(ZRegister.L)) & Not(ChangesMemory) & Not(Changes(register)) & Not(IsRegular8BitLoadFrom(ZRegister.MEM_HL))).* ~
        (Elidable & IsRegular8BitLoadFrom(ZRegister.MEM_HL)) ~~> { code =>
        val last = code.last
        code.init :+ last.copy(registers = TwoRegisters(last.registers.asInstanceOf[TwoRegisters].target, register))
      }
    ),
    (Is8BitLoad(ZRegister.MEM_ABS_8, ZRegister.A) & MatchParameter(0)).captureLine(1) ~
      (Linear & DoesntChangeMemoryAt(1) & Not(Changes(ZRegister.A))).* ~
      (Elidable & Is8BitLoad(ZRegister.A, ZRegister.MEM_ABS_8) & MatchParameter(0)) ~~> { code => code.init },

    (Is8BitLoad(ZRegister.MEM_HL, ZRegister.A) & MatchConstantInHL(0)).captureLine(1) ~
      (Linear & DoesntChangeMemoryAt(1) & Not(Changes(ZRegister.A))).* ~
      (Elidable & Is8BitLoad(ZRegister.A, ZRegister.MEM_ABS_8) & MatchParameter(0)) ~~> { code => code.init },

    (Is8BitLoad(ZRegister.MEM_ABS_8, ZRegister.A) & MatchParameter(0)).captureLine(1) ~
      (Linear & DoesntChangeMemoryAt(1) & Not(Changes(ZRegister.A))).* ~
      (Elidable & Is8BitLoad(ZRegister.A, ZRegister.MEM_HL) & MatchConstantInHL(0)) ~~> { code => code.init },

    (Is8BitLoad(ZRegister.MEM_HL, ZRegister.A) & MatchConstantInHL(0)).captureLine(1) ~
      (Linear & DoesntChangeMemoryAt(1) & Not(Changes(ZRegister.A))).* ~
      (Elidable & Is8BitLoad(ZRegister.A, ZRegister.MEM_HL) & MatchConstantInHL(0)) ~~> { code => code.init },

    (Is16BitLoad(ZRegister.MEM_ABS_16, ZRegister.HL) & MatchParameter(0)).captureLine(1) ~
      (Linear & DoesntChangeMemoryAt(1) & Not(Changes(ZRegister.HL))).* ~
      (Elidable & Is16BitLoad(ZRegister.HL, ZRegister.MEM_ABS_16) & MatchParameter(0)) ~~> { code => code.init },

    (Is8BitLoad(ZRegister.MEM_ABS_8, ZRegister.A) & MatchParameter(0) & MatchRegister(ZRegister.A, 2)).captureLine(1) ~
      (Linear & DoesntChangeMemoryAt(1) & Not(Is8BitLoad(ZRegister.A, ZRegister.MEM_ABS_8))).* ~
      (Elidable & Is8BitLoad(ZRegister.A, ZRegister.MEM_ABS_8) & MatchParameter(0)) ~~> { (code, ctx) =>
      code.init :+ ZLine.ldImm8(ZRegister.A, ctx.get[Int](2))
    },

    (Is16BitLoad(ZRegister.MEM_ABS_16, ZRegister.HL) & MatchParameter(0) & MatchConstantInHL(2)).captureLine(1) ~
      (Linear & DoesntChangeMemoryAt(1) & Not(Is16BitLoad(ZRegister.HL, ZRegister.MEM_ABS_16))).* ~
      (Elidable & Is16BitLoad(ZRegister.HL, ZRegister.MEM_ABS_16) & MatchParameter(0)) ~~> { (code, ctx) =>
      code.init :+ ZLine.ldImm16(ZRegister.HL, ctx.get[Constant](2))
    },
  )

  val PointlessLoad = new RuleBasedAssemblyOptimization("Pointless load",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    // 0-6
    for7Registers(register =>
      (Elidable & Is8BitLoadTo(register) & DoesntMatterWhatItDoesWith(register)) ~~> (_ => Nil)
    ),
    // 7-11
    for5LargeRegisters(register =>
      (Elidable & Is16BitLoadTo(register) & DoesntMatterWhatItDoesWith(register)) ~~> (_ => Nil)
    ),
    // 12-18
    for7Registers(register =>
      (Is8BitLoad(register, ZRegister.IMM_8) & MatchImmediate(0)) ~
        (Linear & Not(Changes(register))).* ~
        (Elidable & Is8BitLoad(register, ZRegister.IMM_8) & MatchImmediate(0)) ~~> (_.init)
    ),
    // 19-23
    for5LargeRegisters(register =>
      (Is16BitLoad(register, ZRegister.IMM_16) & MatchImmediate(0)) ~
        (Linear & Not(Changes(register))).* ~
        (Elidable & Is16BitLoad(register, ZRegister.IMM_16) & MatchImmediate(0)) ~~> (_.init)
    ),
    // 24
    (Elidable & Is8BitLoadTo(ZRegister.MEM_HL)) ~
      (Linear & Not(ConcernsMemory) & Not(Changes(ZRegister.HL))).* ~
      Is8BitLoadTo(ZRegister.MEM_HL) ~~> (_.tail),
    // 25
    (Elidable & Is8BitLoadTo(ZRegister.MEM_DE)) ~
      (Linear & Not(ConcernsMemory) & Not(Changes(ZRegister.DE))).* ~
      Is8BitLoadTo(ZRegister.MEM_DE) ~~> (_.tail),
    // 26
    (Elidable & Is8BitLoadTo(ZRegister.MEM_BC)) ~
      (Linear & Not(ConcernsMemory) & Not(Changes(ZRegister.BC))).* ~
      Is8BitLoadTo(ZRegister.MEM_BC) ~~> (_.tail),
    // 27
    (Elidable & MatchTargetIxOffsetOf8BitLoad(0) & MatchUnimportantIxOffset(0)) ~~> (_ => Nil),
    // 28-34
    for7Registers(register =>
      (Elidable & Is8BitLoadTo(register) & NoOffset & MatchSourceRegisterAndOffset(1)) ~
        (Linear & Not(Concerns(register)) & DoesntChangeMatchedRegisterAndOffset(1)).* ~
        (Elidable & IsRegular8BitLoadFrom(register) & DoesntMatterWhatItDoesWith(register)) ~~> { code =>
        val last = code.last
        val head = code.head
        code.tail.init :+ ZLine(LD, (last.registers, head.registers) match {
          case (TwoRegisters(t, _), TwoRegisters(_, s)) => TwoRegisters(t, s)
          case (TwoRegistersOffset(t, _, o), TwoRegisters(_, s)) => TwoRegistersOffset(t, s, o)
          case (TwoRegisters(t, _), TwoRegistersOffset(_, s, o)) => TwoRegistersOffset(t, s, o)
          case _ => ???
        }, head.parameter)
      }
    ),
    // 35-41
    for7Registers(register =>
      (Elidable & Is8BitLoad(register, register)) ~~> (_ => Nil)
    ),
    // 42-48
    for7Registers(register =>
      (Elidable & Is8BitLoadTo(register) & MatchSourceRegisterAndOffset(0)) ~
        (Linear & Not(Concerns(register)) & DoesntChangeMatchedRegisterAndOffset(0)).* ~
        (Elidable & HasOpcodeIn(Set(ADD, ADC, XOR, OR, AND, CP, SUB, SBC)) & HasRegisters(OneRegister(register)) & DoesntMatterWhatItDoesWith(register)) ~~> ((code,ctx) =>
          code.tail.init :+ code.last.copy(registers = ctx.get[RegisterAndOffset](0).toOneRegister)
        )
    ),

  )

  val PointlessStackStashing = new RuleBasedAssemblyOptimization("Pointless stack stashing",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    // 0-4
    for5LargeRegisters(register => {
      (Elidable & HasOpcode(PUSH) & HasRegisterParam(register)) ~
        (Linear & Not(HasOpcode(POP)) & Not(Changes(register))).* ~
      (Elidable & HasOpcode(POP) & HasRegisterParam(register)) ~~> (_.tail.init)
    }),

  )

  private def simplifiable16BitAddWithSplitTarget(targetH: ZRegister.Value, targetL: ZRegister.Value, target: ZRegister.Value, source: ZRegister.Value) = MultipleAssemblyRules(List(
    (Is8BitLoad(targetH, ZRegister.IMM_8) & MatchImmediate(1)) ~
      (Linear & Not(Changes(target))).* ~
      (Is8BitLoad(targetL, ZRegister.IMM_8) & MatchImmediate(2)) ~
      (Linear & Not(Changes(target)) & Not(Changes(source))).* ~
      (Is16BitLoad(source, ZRegister.IMM_16) & MatchImmediate(0)) ~
      (Linear & Not(Changes(target)) & Not(Changes(source))).* ~
      (Elidable & HasOpcode(ADD_16) & HasRegisters(TwoRegisters(target, source)) & DoesntMatterWhatItDoesWithFlags) ~~> { (code, ctx) =>
      val value = (ctx.get[Constant](0) + ctx.get[Constant](1).asl(8) + ctx.get[Constant](2)).quickSimplify
      code.init :+ ZLine.ldImm16(target, value)
    },
    (Is8BitLoad(targetL, ZRegister.IMM_8) & MatchImmediate(2)) ~
      (Linear & Not(Changes(target))).* ~
      (Is8BitLoad(targetH, ZRegister.IMM_8) & MatchImmediate(1)) ~
      (Linear & Not(Changes(target)) & Not(Changes(source))).* ~
      (Is16BitLoad(source, ZRegister.IMM_16) & MatchImmediate(0)) ~
      (Linear & Not(Changes(target)) & Not(Changes(source))).* ~
      (Elidable & HasOpcode(ADD_16) & HasRegisters(TwoRegisters(target, source)) & DoesntMatterWhatItDoesWithFlags) ~~> { (code, ctx) =>
      val value = (ctx.get[Constant](0) + ctx.get[Constant](1).asl(8) + ctx.get[Constant](2)).quickSimplify
      code.init :+ ZLine.ldImm16(target, value)
    },
    (Is16BitLoad(source, ZRegister.IMM_16) & MatchImmediate(0)) ~
      (Linear & Not(Changes(targetL)) & Not(Changes(source))).* ~
      (Is8BitLoad(targetL, ZRegister.IMM_8) & MatchImmediate(2)) ~
      (Linear & Not(Changes(target)) & Not(Changes(source))).* ~
      (Is8BitLoad(targetH, ZRegister.IMM_8) & MatchImmediate(1)) ~
      (Linear & Not(Changes(target)) & Not(Changes(source))).* ~
      (Elidable & HasOpcode(ADD_16) & HasRegisters(TwoRegisters(target, source)) & DoesntMatterWhatItDoesWithFlags) ~~> { (code, ctx) =>
      val value = (ctx.get[Constant](0) + ctx.get[Constant](1).asl(8) + ctx.get[Constant](2)).quickSimplify
      code.init :+ ZLine.ldImm16(target, value)
    },
    (Is16BitLoad(source, ZRegister.IMM_16) & MatchImmediate(0)) ~
      (Linear & Not(Changes(targetH)) & Not(Changes(source))).* ~
      (Is8BitLoad(targetH, ZRegister.IMM_8) & MatchImmediate(1)) ~
      (Linear & Not(Changes(target)) & Not(Changes(source))).* ~
      (Is8BitLoad(targetL, ZRegister.IMM_8) & MatchImmediate(2)) ~
      (Linear & Not(Changes(target)) & Not(Changes(source))).* ~
      (Elidable & HasOpcode(ADD_16) & HasRegisters(TwoRegisters(target, source)) & DoesntMatterWhatItDoesWithFlags) ~~> { (code, ctx) =>
      val value = (ctx.get[Constant](0) + ctx.get[Constant](1).asl(8) + ctx.get[Constant](2)).quickSimplify
      code.init :+ ZLine.ldImm16(target, value)
    },
  ))

  val SimplifiableMaths = new RuleBasedAssemblyOptimization("Simplifiable maths",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    for6Registers(register =>
      (Elidable & HasOpcode(ADD) & MatchRegister(ZRegister.A, 0) & HasRegisterParam(register) & MatchRegister(register, 1) &
        DoesntMatterWhatItDoesWithFlags) ~~> ((code, ctx) => List(ZLine.ldImm8(ZRegister.A, (ctx.get[Int](0) + ctx.get[Int](1)) & 0xff))),
    ),
    simplifiable16BitAddWithSplitTarget(ZRegister.H, ZRegister.L, ZRegister.HL, ZRegister.BC),
    simplifiable16BitAddWithSplitTarget(ZRegister.H, ZRegister.L, ZRegister.HL, ZRegister.DE),
    simplifiable16BitAddWithSplitTarget(ZRegister.IXH, ZRegister.IXL, ZRegister.IX, ZRegister.BC),
    simplifiable16BitAddWithSplitTarget(ZRegister.IXH, ZRegister.IXL, ZRegister.IX, ZRegister.DE),
    simplifiable16BitAddWithSplitTarget(ZRegister.IYH, ZRegister.IYL, ZRegister.IY, ZRegister.BC),
    simplifiable16BitAddWithSplitTarget(ZRegister.IYH, ZRegister.IYL, ZRegister.IY, ZRegister.DE),

    (Elidable & HasOpcode(ADD_16) & HasRegisters(TwoRegisters(ZRegister.HL, ZRegister.BC)) & MatchRegister(ZRegister.BC, 0) & MatchRegister(ZRegister.HL, 1) & DoesntMatterWhatItDoesWithFlags) ~~> { (code, ctx) =>
      List(ZLine.ldImm16(ZRegister.HL, ctx.get[Int](0) + ctx.get[Int](1)))
    },
    (Elidable & HasOpcode(ADD_16) & HasRegisters(TwoRegisters(ZRegister.HL, ZRegister.DE)) & MatchRegister(ZRegister.DE, 0) & MatchRegister(ZRegister.HL, 1) & DoesntMatterWhatItDoesWithFlags) ~~> { (code, ctx) =>
      List(ZLine.ldImm16(ZRegister.HL, ctx.get[Int](0) + ctx.get[Int](1)))
    },


    (Elidable & HasOpcode(ADD_16) & HasRegisters(TwoRegisters(ZRegister.HL, ZRegister.BC)) & MatchRegister(ZRegister.BC, 0) & MatchConstantInHL(1) & DoesntMatterWhatItDoesWithFlags) ~~> { (code, ctx) =>
      List(ZLine.ldImm16(ZRegister.HL, (ctx.get[Constant](1) + ctx.get[Int](0)).quickSimplify))
    },
    (Elidable & HasOpcode(ADD_16) & HasRegisters(TwoRegisters(ZRegister.HL, ZRegister.DE)) & MatchRegister(ZRegister.DE, 0) & MatchConstantInHL(1) & DoesntMatterWhatItDoesWithFlags) ~~> { (code, ctx) =>
      List(ZLine.ldImm16(ZRegister.HL, (ctx.get[Constant](1) + ctx.get[Int](0)).quickSimplify))
    },


    (Elidable & Is8BitLoad(ZRegister.D, ZRegister.H)) ~
      (Elidable & Is8BitLoad(ZRegister.E, ZRegister.L)) ~
      (Elidable & Is8BitLoadTo(ZRegister.L)) ~
      (Elidable & Is8BitLoadTo(ZRegister.H)) ~
      (HasOpcode(ADD_16) & HasRegisters(TwoRegisters(ZRegister.HL, ZRegister.DE)) & DoesntMatterWhatItDoesWith(ZRegister.D, ZRegister.E)) ~~> { code =>
      List(
        change8BitLoadTarget(code(2), ZRegister.E),
        change8BitLoadTarget(code(3), ZRegister.D),
        code.last)
    },
    (Elidable & Is8BitLoad(ZRegister.D, ZRegister.H)) ~
      (Elidable & Is8BitLoad(ZRegister.E, ZRegister.L)) ~
      (Elidable & Is8BitLoadTo(ZRegister.H)) ~
      (Elidable & Is8BitLoadTo(ZRegister.L)) ~
      (HasOpcode(ADD_16) & HasRegisters(TwoRegisters(ZRegister.HL, ZRegister.DE)) & DoesntMatterWhatItDoesWith(ZRegister.D, ZRegister.E)) ~~> { code =>
      List(
        change8BitLoadTarget(code(2), ZRegister.D),
        change8BitLoadTarget(code(3), ZRegister.E),
        code.last)
    },

    (Elidable & HasOpcodeIn(Set(ADD, OR, XOR, SUB)) & Has8BitImmediate(0) & DoesntMatterWhatItDoesWithFlags) ~~> (_ => Nil),
    (Elidable & HasOpcode(AND) & Has8BitImmediate(0xff) & DoesntMatterWhatItDoesWithFlags) ~~> (_ => Nil),
    (Elidable & HasOpcode(AND) & Has8BitImmediate(0) & DoesntMatterWhatItDoesWithFlags) ~~> (_ => List(ZLine.ldImm8(ZRegister.A, 0))),
    (Elidable & HasOpcode(AND) & Has8BitImmediate(0) & DoesntMatterWhatItDoesWithFlags) ~~> (_ => List(ZLine.ldImm8(ZRegister.A, 0))),


    (Elidable & HasOpcode(OR) & Match8BitImmediate(1) & MatchRegister(ZRegister.A, 0)) ~
      (Elidable & HasOpcodeIn(Set(JP, JR)) & HasRegisters(IfFlagSet(ZFlag.S)) & DoesntMatterWhatItDoesWithFlags) ~
      Where(ctx => ctx.get[Constant](1).isInstanceOf[NumericConstant]) ~~> { (code, ctx) =>
      val value = (ctx.get[Int](0) | ctx.get[NumericConstant](1).value).toInt & 0xff
      if (value.&(0x80) == 0) List(ZLine.ldImm8(ZRegister.A, value))
      else List(ZLine.ldImm8(ZRegister.A, value), code.last.copy(registers = NoRegisters))
    },

    (Elidable & HasOpcode(OR) & Match8BitImmediate(1) & MatchRegister(ZRegister.A, 0)) ~
      (Elidable & HasOpcodeIn(Set(JP, JR)) & HasRegisters(IfFlagClear(ZFlag.S)) & DoesntMatterWhatItDoesWithFlags) ~
      Where(ctx => ctx.get[Constant](1).isInstanceOf[NumericConstant]) ~~> { (code, ctx) =>
      val value = (ctx.get[Int](0) | ctx.get[NumericConstant](1).value).toInt & 0xff
      if (value.&(0x80) != 0) List(ZLine.ldImm8(ZRegister.A, value))
      else List(ZLine.ldImm8(ZRegister.A, value), code.last.copy(registers = NoRegisters))
    },

    (Elidable & HasOpcode(ADD) & Match8BitImmediate(1) & MatchRegister(ZRegister.A, 0) & DoesntMatterWhatItDoesWithFlags) ~~> {(code, ctx) =>
      List(ZLine.ldImm8(ZRegister.A, (ctx.get[Constant](1) + ctx.get[Int](0)).quickSimplify))
    },

    (Elidable & HasOpcode(SUB) & Match8BitImmediate(1) & MatchRegister(ZRegister.A, 0) & DoesntMatterWhatItDoesWithFlags) ~~> {(code, ctx) =>
      List(ZLine.ldImm8(ZRegister.A, (NumericConstant(ctx.get[Int](0) & 0xff, 1) - ctx.get[Constant](1)).quickSimplify))
    },

    (Elidable & HasOpcode(OR) & Match8BitImmediate(1) & MatchRegister(ZRegister.A, 0) & DoesntMatterWhatItDoesWithFlags) ~~> {(code, ctx) =>
      List(ZLine.ldImm8(ZRegister.A, CompoundConstant(MathOperator.Or, NumericConstant(ctx.get[Int](0) & 0xff, 1), ctx.get[Constant](1)).quickSimplify))
    },

    (Elidable & HasOpcode(XOR) & Match8BitImmediate(1) & MatchRegister(ZRegister.A, 0) & DoesntMatterWhatItDoesWithFlags) ~~> {(code, ctx) =>
      List(ZLine.ldImm8(ZRegister.A, CompoundConstant(MathOperator.Exor, NumericConstant(ctx.get[Int](0) & 0xff, 1), ctx.get[Constant](1)).quickSimplify))
    },

    (Elidable & HasOpcode(AND) & Match8BitImmediate(1) & MatchRegister(ZRegister.A, 0) & DoesntMatterWhatItDoesWithFlags) ~~> {(code, ctx) =>
      List(ZLine.ldImm8(ZRegister.A, CompoundConstant(MathOperator.And, NumericConstant(ctx.get[Int](0) & 0xff, 1), ctx.get[Constant](1)).quickSimplify))
    },

    (Elidable & HasOpcode(ADD) & Match8BitImmediate(1) & MatchRegister(ZRegister.A, 0)) ~
      (Elidable & HasOpcode(DAA) & DoesntMatterWhatItDoesWithFlags) ~~> {(code, ctx) =>
      List(ZLine.ldImm8(ZRegister.A, CompoundConstant(MathOperator.DecimalPlus, NumericConstant(ctx.get[Int](0) & 0xff, 1), ctx.get[Constant](1)).quickSimplify))
    },

    (Elidable & HasOpcode(SUB) & Match8BitImmediate(1) & MatchRegister(ZRegister.A, 0)) ~
      (Elidable & HasOpcode(DAA) & DoesntMatterWhatItDoesWithFlags) ~~> {(code, ctx) =>
      List(ZLine.ldImm8(ZRegister.A, CompoundConstant(MathOperator.DecimalMinus, NumericConstant(ctx.get[Int](0) & 0xff, 1), ctx.get[Constant](1)).quickSimplify))
    },

  )

  val FreeHL = new RuleBasedAssemblyOptimization("Free HL",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    (Elidable & Is16BitLoad(ZRegister.HL, ZRegister.IMM_16)) ~
      (Elidable & Is8BitLoadTo(ZRegister.MEM_HL) & DoesntMatterWhatItDoesWith(ZRegister.HL, ZRegister.A)) ~~> (code =>
      List(
        code(1).copy(registers = TwoRegisters(ZRegister.A, code(1).registers.asInstanceOf[TwoRegisters].source)),
        code.head.copy(opcode = LD, registers = TwoRegisters(ZRegister.MEM_ABS_8, ZRegister.A)),
      )),
    (Elidable & Is16BitLoad(ZRegister.HL, ZRegister.IMM_16)) ~
      (Elidable & Is8BitLoad(ZRegister.A, ZRegister.MEM_HL) & DoesntMatterWhatItDoesWith(ZRegister.HL)) ~~> (code =>
      List(
        code.head.copy(opcode = LD, registers = TwoRegisters(ZRegister.A, ZRegister.MEM_ABS_8)),
      )),
    (Elidable & Is16BitLoad(ZRegister.HL, ZRegister.IMM_16)) ~
      (Elidable & IsRegular8BitLoadFrom(ZRegister.MEM_HL) & DoesntMatterWhatItDoesWith(ZRegister.HL) & DoesntMatterWhatItDoesWith(ZRegister.A)) ~~> (code =>
      List(
        code.head.copy(opcode = LD, registers = TwoRegisters(ZRegister.A, ZRegister.MEM_ABS_8)),
        code(1).copy(registers = TwoRegisters(code(1).registers.asInstanceOf[TwoRegisters].target, ZRegister.A)),
      )),
    (Elidable & Is16BitLoad(ZRegister.HL, ZRegister.IMM_16)) ~
      (Elidable & Is8BitLoad(ZRegister.D, ZRegister.H)) ~
      (Elidable & Is8BitLoad(ZRegister.E, ZRegister.L) & DoesntMatterWhatItDoesWith(ZRegister.HL)) ~~> (code =>
      List(
        code.head.copy(registers = TwoRegisters(ZRegister.DE, ZRegister.IMM_16))
      )),
    (Elidable & Is16BitLoad(ZRegister.HL, ZRegister.IMM_16)) ~
      (Elidable & Is8BitLoad(ZRegister.B, ZRegister.H)) ~
      (Elidable & Is8BitLoad(ZRegister.C, ZRegister.L) & DoesntMatterWhatItDoesWith(ZRegister.HL)) ~~> (code =>
      List(
        code.head.copy(registers = TwoRegisters(ZRegister.BC, ZRegister.IMM_16))
      )),
  )

  val UnusedCodeRemoval = new RuleBasedAssemblyOptimization("Unreachable code removal",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (HasOpcodeIn(Set(JP, JR)) & HasRegisters(NoRegisters)) ~ (Not(HasOpcode(LABEL)) & Elidable).+ ~~> (c => c.head :: Nil)
  )

  val UnusedLabelRemoval = new RuleBasedAssemblyOptimization("Unused label removal",
    needsFlowInfo = FlowInfoRequirement.JustLabels,
    (Elidable & HasOpcode(LABEL) & HasCallerCount(0)) ~~> (_ => Nil)
  )

  val BranchInPlaceRemoval = new RuleBasedAssemblyOptimization("Branch in place",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (HasOpcodeIn(Set(JP, JR)) & MatchJumpTarget(0) & Elidable) ~
      HasOpcodeIn(ZOpcodeClasses.NoopDiscards).* ~
      (HasOpcode(LABEL) & MatchJumpTarget(0)) ~~> (c => c.last :: Nil),
  )

  val All: List[AssemblyOptimization[ZLine]] = List[AssemblyOptimization[ZLine]](
    BranchInPlaceRemoval,
    EmptyMemoryStoreRemoval,
    FreeHL,
    PointlessLoad,
    PointlessStackStashing,
    ReloadingKnownValueFromMemory,
    SimplifiableMaths,
    UnusedCodeRemoval,
    UnusedLabelRemoval,
    UsingKnownValueFromAnotherRegister,
  )
}

