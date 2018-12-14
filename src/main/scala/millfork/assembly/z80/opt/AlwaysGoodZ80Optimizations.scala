package millfork.assembly.z80.opt

import millfork.assembly.AssemblyOptimization
import millfork.assembly.z80._
import millfork.assembly.z80.ZOpcode._
import millfork.env.Constant
import millfork.node.ZRegister
import ZRegister._

/**
  * Optimizations valid for Z80 and EZ80
  * @author Karol Stasiak
  */
object AlwaysGoodZ80Optimizations {

  def change8BitLoadTarget(line: ZLine, newTarget: ZRegister.Value): ZLine = {
    line match {
      case ZLine(LD, TwoRegistersOffset(_, s, o), p, _, sl) => line.copy(registers = TwoRegistersOffset(newTarget, s, o))
      case ZLine(LD, TwoRegisters(_, s), p, _, sl) => line.copy(registers = TwoRegisters(newTarget, s))
    }
  }

  def for7Registers(f: ZRegister.Value => AssemblyRuleSet) = MultipleAssemblyRules(
    List(ZRegister.A, ZRegister.B, ZRegister.C, ZRegister.D, ZRegister.E, ZRegister.H, ZRegister.L).map(f))

  def for5LargeRegisters(f: ZRegister.Value => AssemblyRuleSet) = MultipleAssemblyRules(
    List(ZRegister.HL, ZRegister.BC, ZRegister.DE, ZRegister.IX, ZRegister.IY).map(f))

  def for6Registers(f: ZRegister.Value => AssemblyRuleSet) = MultipleAssemblyRules(
    List(ZRegister.B, ZRegister.C, ZRegister.D, ZRegister.E, ZRegister.H, ZRegister.L).map(f))

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

  val SimplifiableMaths = new RuleBasedAssemblyOptimization("Simplifiable maths (Z80)",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    simplifiable16BitAddWithSplitTarget(ZRegister.IXH, ZRegister.IXL, ZRegister.IX, ZRegister.BC),
    simplifiable16BitAddWithSplitTarget(ZRegister.IXH, ZRegister.IXL, ZRegister.IX, ZRegister.DE),
    simplifiable16BitAddWithSplitTarget(ZRegister.IYH, ZRegister.IYL, ZRegister.IY, ZRegister.BC),
    simplifiable16BitAddWithSplitTarget(ZRegister.IYH, ZRegister.IYL, ZRegister.IY, ZRegister.DE),

    (Elidable & HasOpcode(NEG)) ~
      (Elidable & HasOpcode(ADD) & Has8BitImmediate(0xff) & DoesntMatterWhatItDoesWithFlags) ~~> (_ => List(ZLine.implied(CPL))),

    (Elidable & HasOpcode(OR) & HasRegisters(OneRegister(A)) & HasRegister(BC, 0)) ~
      (Elidable & HasOpcode(SBC_16) & HasRegisters(TwoRegisters(HL, BC)) & DoesntMatterWhatItDoesWithFlagsExceptZero) ~~> { code =>
      List(ZLine.ld8(A, H), ZLine.register(OR, L))
    },

    (Elidable & HasOpcode(OR) & HasRegisters(OneRegister(A)) & HasRegister(DE, 0)) ~
      (Elidable & HasOpcode(SBC_16) & HasRegisters(TwoRegisters(HL, DE)) & DoesntMatterWhatItDoesWithFlagsExceptZero) ~~> { code =>
      List(ZLine.ld8(A, H), ZLine.register(OR, L))
    },


  )

  val FreeHL = new RuleBasedAssemblyOptimization("Free HL (Z80)",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,

    (Elidable & Is16BitLoad(ZRegister.HL, ZRegister.MEM_ABS_16)) ~
      (Elidable & Is8BitLoad(ZRegister.D, ZRegister.H)) ~
      (Elidable & Is8BitLoad(ZRegister.E, ZRegister.L) & DoesntMatterWhatItDoesWith(ZRegister.HL)) ~~> (code =>
      List(
        code.head.copy(registers = TwoRegisters(ZRegister.DE, ZRegister.MEM_ABS_16))
      )),
    (Elidable & Is16BitLoad(ZRegister.HL, ZRegister.MEM_ABS_16)) ~
      (Elidable & Is8BitLoad(ZRegister.B, ZRegister.H)) ~
      (Elidable & Is8BitLoad(ZRegister.C, ZRegister.L) & DoesntMatterWhatItDoesWith(ZRegister.HL)) ~~> (code =>
      List(
        code.head.copy(registers = TwoRegisters(ZRegister.BC, ZRegister.MEM_ABS_16))
      )),

    MultipleAssemblyRules(Seq(ZRegister.BC, ZRegister.DE).map { registerPair =>
      (Elidable & HasOpcode(LD_16) & HasRegisters(TwoRegisters(registerPair, ZRegister.IMM_16)) & MatchParameter(0)) ~
        (Elidable & HasOpcode(LD_16) & HasRegisters(TwoRegisters(ZRegister.HL, ZRegister.MEM_ABS_16)) & MatchParameter(1)) ~
        (Elidable & HasOpcode(OR) & HasRegisters(OneRegister(ZRegister.A))) ~
        (Elidable & HasOpcode(SBC_16) & HasRegisters(TwoRegisters(ZRegister.HL, registerPair)) &
          DoesntMatterWhatItDoesWith(ZRegister.A, ZRegister.HL, registerPair) &
          DoesntMatterWhatItDoesWithFlagsExceptCarry) ~~> { (code, ctx) =>
        import ZRegister._
        val value = ctx.get[Constant](0)
        val variable = ctx.get[Constant](1)
        List(
          ZLine.ldAbs8(A, variable),
          ZLine.imm8(SUB, value.loByte),
          ZLine.ldAbs8(A, variable + 1),
          ZLine.imm8(SBC, value.hiByte))
      }
    }),

    (Elidable & Is8BitLoad(ZRegister.H, ZRegister.B)) ~
      (Elidable & Is8BitLoad(ZRegister.L, ZRegister.C)) ~
      (Elidable & Is8BitLoad(ZRegister.MEM_HL, ZRegister.A) & DoesntMatterWhatItDoesWith(ZRegister.HL)) ~~> {_ =>
      List(ZLine.ld8(ZRegister.MEM_BC, ZRegister.A))
    },

    (Elidable & Is8BitLoad(ZRegister.H, ZRegister.D)) ~
      (Elidable & Is8BitLoad(ZRegister.L, ZRegister.E)) ~
      (Elidable & Is8BitLoad(ZRegister.MEM_HL, ZRegister.A) & DoesntMatterWhatItDoesWith(ZRegister.HL)) ~~> {_ =>
      List(ZLine.ld8(ZRegister.MEM_DE, ZRegister.A))
    },

    (Elidable & Is8BitLoad(ZRegister.H, ZRegister.B)) ~
      (Elidable & Is8BitLoad(ZRegister.L, ZRegister.C)) ~
      (Elidable & Is8BitLoad(ZRegister.A, ZRegister.MEM_HL) & DoesntMatterWhatItDoesWith(ZRegister.HL)) ~~> {_ =>
      List(ZLine.ld8(ZRegister.A, ZRegister.MEM_BC))
    },

    (Elidable & Is8BitLoad(ZRegister.H, ZRegister.D)) ~
      (Elidable & Is8BitLoad(ZRegister.L, ZRegister.E)) ~
      (Elidable & Is8BitLoad(ZRegister.A, ZRegister.MEM_HL) & DoesntMatterWhatItDoesWith(ZRegister.HL)) ~~> {_ =>
      List(ZLine.ld8(ZRegister.A, ZRegister.MEM_DE))
    },

  )

  val All: List[AssemblyOptimization[ZLine]] = List[AssemblyOptimization[ZLine]](
    FreeHL,
    SimplifiableMaths,
  )
}

