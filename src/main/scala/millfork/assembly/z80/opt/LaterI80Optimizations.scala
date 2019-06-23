package millfork.assembly.z80.opt

import millfork.assembly.AssemblyOptimization
import millfork.assembly.z80._
import millfork.node.ZRegister
import ZOpcode._
import ZRegister._
import millfork.env.Constant


/**
  * @author Karol Stasiak
  */
object LaterI80Optimizations {
  val VariousSmallOptimizations = new RuleBasedAssemblyOptimization("Various small optimizations",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,

    (Elidable & Is8BitLoadTo(A) & Has8BitImmediate(0) & DoesntMatterWhatItDoesWithFlags) ~~> { _ =>
      List(ZLine.register(XOR, A))
    },
    (Elidable & HasOpcode(CP) & DoesntMatterWhatItDoesWithFlags) ~~> { _ =>
      Nil
    },
    (Elidable & HasOpcode(CP) & Has8BitImmediate(0) & DoesntMatterWhatItDoesWithFlagsOtherThanSZ) ~~> { _ =>
      List(ZLine.register(OR, A))
    },
    (Elidable & HasOpcode(CP) & Has8BitImmediate(1) & DoesntMatterWhatItDoesWithFlagsOtherThanSZ & DoesntMatterWhatItDoesWith(A)) ~~> { _ =>
      List(ZLine.register(DEC, A))
    },
    (Elidable & HasOpcode(CP) & Has8BitImmediate(255) & DoesntMatterWhatItDoesWithFlagsOtherThanSZ & DoesntMatterWhatItDoesWith(A)) ~~> { _ =>
      List(ZLine.register(INC, A))
    },

    (Elidable & HasOpcodeIn(Set(JP, JR)) & HasRegisters(IfFlagClear(ZFlag.C)) & MatchParameter(0)) ~
      (Elidable & HasOpcode(INC) & HasRegisterParam(A) & DoesntMatterWhatItDoesWithFlags) ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> (code => List(ZLine.imm8(ADC, 0), code.last)),

    (Elidable & HasOpcodeIn(Set(JP, JR)) & HasRegisters(IfFlagSet(ZFlag.C)) & MatchParameter(0)) ~
      (Elidable & HasOpcode(INC) & HasRegisterParam(A) & DoesntMatterWhatItDoesWithFlags) ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> (code => List(ZLine.implied(CCF), ZLine.imm8(ADC, 0), code.last)),

    (Elidable & HasOpcodeIn(Set(JP, JR)) & HasRegisters(IfFlagClear(ZFlag.C)) & MatchParameter(0)) ~
      (Elidable & HasOpcode(DEC) & HasRegisterParam(A) & DoesntMatterWhatItDoesWithFlags) ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> (code => List(ZLine.imm8(SBC, 0), code.last)),

    (Elidable & HasOpcodeIn(Set(JP, JR)) & HasRegisters(IfFlagSet(ZFlag.C)) & MatchParameter(0)) ~
      (Elidable & HasOpcode(DEC) & HasRegisterParam(A) & DoesntMatterWhatItDoesWithFlags) ~
      (HasOpcode(LABEL) & MatchParameter(0)) ~~> (code => List(ZLine.implied(CCF), ZLine.imm8(SBC, 0), code.last)),

    (Elidable & Is8BitLoadTo(B) & Match8BitImmediate(1)) ~
      (Elidable & Is8BitLoadTo(C) & Match8BitImmediate(0)) ~~> { (code, ctx) =>
      val l = ctx.get[Constant](0)
      val h = ctx.get[Constant](1)
      List(ZLine.ldImm16(BC, h.asl(8).+(l).quickSimplify).pos(code.map(_.source)))
    },

    (Elidable & Is8BitLoadTo(C) & Match8BitImmediate(0)) ~
      (Elidable & Is8BitLoadTo(B) & Match8BitImmediate(1)) ~~> { (code, ctx) =>
      val l = ctx.get[Constant](0)
      val h = ctx.get[Constant](1)
      List(ZLine.ldImm16(BC, h.asl(8).+(l).quickSimplify).pos(code.map(_.source)))
    },

    (Elidable & Is8BitLoadTo(D) & Match8BitImmediate(1)) ~
      (Elidable & Is8BitLoadTo(E) & Match8BitImmediate(0)) ~~> { (code, ctx) =>
      val l = ctx.get[Constant](0)
      val h = ctx.get[Constant](1)
      List(ZLine.ldImm16(DE, h.asl(8).+(l).quickSimplify).pos(code.map(_.source)))
    },

    (Elidable & Is8BitLoadTo(E) & Match8BitImmediate(0)) ~
      (Elidable & Is8BitLoadTo(D) & Match8BitImmediate(1)) ~~> { (code, ctx) =>
      val l = ctx.get[Constant](0)
      val h = ctx.get[Constant](1)
      List(ZLine.ldImm16(DE, h.asl(8).+(l).quickSimplify).pos(code.map(_.source)))
    },

    (Elidable & HasOpcode(LD_16) & HasRegisters(TwoRegisters(HL, IMM_16))) ~
      (Elidable & Is8BitLoad(E, L)) ~
      (Elidable & Is8BitLoad(D, H) & DoesntMatterWhatItDoesWith(HL)) ~~> { code =>
      List(code.head.copy(registers = TwoRegisters(DE, IMM_16)))
    },

    (Elidable & HasOpcode(LD_16) & HasRegisters(TwoRegisters(HL, IMM_16))) ~
      (Elidable & Is8BitLoad(C, L)) ~
      (Elidable & Is8BitLoad(B, H) & DoesntMatterWhatItDoesWith(HL)) ~~> { code =>
      List(code.head.copy(registers = TwoRegisters(BC, IMM_16)))
    },
  )

  val FreeHL = new RuleBasedAssemblyOptimization("Free HL (later)",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,

    (Elidable & Is8BitLoad(H, B)) ~
      (Elidable & Is8BitLoad(L, C)) ~
      (Elidable & Is8BitLoad(MEM_HL, A) & DoesntMatterWhatItDoesWith(B, C)) ~~> { _ =>
      List(ZLine.ld8(MEM_BC, A))
    },
    (Elidable & Is8BitLoad(H, B)) ~
      (Elidable & Is8BitLoad(L, C)) ~
      (Elidable & Is8BitLoad(A, MEM_HL) & DoesntMatterWhatItDoesWith(B, C)) ~~> { _ =>
      List(ZLine.ld8(A, MEM_BC))
    },
    (Elidable & Is8BitLoad(H, D)) ~
      (Elidable & Is8BitLoad(L, E)) ~
      (Elidable & Is8BitLoad(MEM_DE, A) & DoesntMatterWhatItDoesWith(D, E)) ~~> { _ =>
      List(ZLine.ld8(MEM_DE, A))
    },
    (Elidable & Is8BitLoad(H, D)) ~
      (Elidable & Is8BitLoad(L, E)) ~
      (Elidable & Is8BitLoad(A, MEM_HL) & DoesntMatterWhatItDoesWith(D, E)) ~~> { _ =>
      List(ZLine.ld8(A, MEM_DE))
    },
  )

  val TailCall = new RuleBasedAssemblyOptimization("Tail call",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (Elidable & HasOpcode(CALL)) ~
      (Elidable & HasOpcodeIn(ZOpcodeClasses.NoopDiscards)).* ~
      (Elidable & HasOpcode(RET) & HasRegisters(NoRegisters)) ~~> { code =>
      List(code.head.copy(opcode = JP))
    },
    (Elidable & HasOpcode(CALL)) ~
      HasOpcode(LABEL).* ~
      (Elidable & HasOpcodeIn(ZOpcodeClasses.NoopDiscards)).*.capture(0) ~
      (Elidable & HasOpcode(RET) & HasRegisters(NoRegisters)) ~~> { (code, ctx) =>
      ctx.get[List[ZLine]](0) ++ (code.head.copy(opcode = JP) :: code.tail)
    },
  )

  val All: List[AssemblyOptimization[ZLine]] = List(
    VariousSmallOptimizations,
    FreeHL,
    TailCall,
  )
}
