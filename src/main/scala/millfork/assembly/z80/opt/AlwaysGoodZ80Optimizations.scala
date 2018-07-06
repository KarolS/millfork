package millfork.assembly.z80.opt

import millfork.assembly.AssemblyOptimization
import millfork.assembly.z80._
import millfork.assembly.z80.ZOpcode._
import millfork.env.{Constant, NumericConstant}
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
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
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
      (Elidable & Is8BitLoad(ZRegister.A, ZRegister.MEM_ABS_8) & MatchParameter(0)) ~~> { code => code.init }
  )

  val PointlessLoad = new RuleBasedAssemblyOptimization("Pointless load",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    for7Registers(register =>
      (Elidable & Is8BitLoadTo(register) & DoesntMatterWhatItDoesWith(register)) ~~> (_ => Nil)
    ),
    for5LargeRegisters(register =>
      (Elidable & Is16BitLoadTo(register) & DoesntMatterWhatItDoesWith(register)) ~~> (_ => Nil)
    ),
    for7Registers(register =>
      (Is8BitLoad(register, ZRegister.IMM_8) & MatchImmediate(0)) ~
        (Linear & Not(Changes(register))).* ~
        (Elidable & Is8BitLoad(register, ZRegister.IMM_8) & MatchImmediate(0)) ~~> (_.init)
    ),
    for5LargeRegisters(register =>
      (Is16BitLoad(register, ZRegister.IMM_16) & MatchImmediate(0)) ~
        (Linear & Not(Changes(register))).* ~
        (Elidable & Is16BitLoad(register, ZRegister.IMM_16) & MatchImmediate(0)) ~~> (_.init)
    ),

    (Elidable & Is8BitLoadTo(ZRegister.MEM_HL)) ~
      (Linear & Not(ConcernsMemory) & Not(Changes(ZRegister.HL))).* ~
      Is8BitLoadTo(ZRegister.MEM_HL) ~~> (_.tail),

    (Elidable & Is8BitLoadTo(ZRegister.MEM_DE)) ~
      (Linear & Not(ConcernsMemory) & Not(Changes(ZRegister.DE))).* ~
      Is8BitLoadTo(ZRegister.MEM_DE) ~~> (_.tail),

    (Elidable & Is8BitLoadTo(ZRegister.MEM_BC)) ~
      (Linear & Not(ConcernsMemory) & Not(Changes(ZRegister.BC))).* ~
      Is8BitLoadTo(ZRegister.MEM_BC) ~~> (_.tail),

    (Elidable & MatchTargetIxOffsetOf8BitLoad(0) & MatchUnimportantIxOffset(0)) ~~> (_ => Nil),

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

    for7Registers(register =>
      (Elidable & Is8BitLoad(register, register)) ~~> (_ => Nil)
    ),

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

    (Elidable & HasOpcodeIn(Set(ADD, OR, AND, XOR, SUB)) & Has8BitImmediate(0) & DoesntMatterWhatItDoesWithFlags) ~~> (_ => Nil),
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

  val UnusedLabelRemoval = new RuleBasedAssemblyOptimization("Unused label removal",
    needsFlowInfo = FlowInfoRequirement.JustLabels,
    (Elidable & HasOpcode(LABEL) & HasCallerCount(0)) ~~> (_ => Nil)
  )

  val All: List[AssemblyOptimization[ZLine]] = List[AssemblyOptimization[ZLine]](
    FreeHL,
    PointlessLoad,
    ReloadingKnownValueFromMemory,
    SimplifiableMaths,
    UnusedLabelRemoval,
    UsingKnownValueFromAnotherRegister,
  )
}

