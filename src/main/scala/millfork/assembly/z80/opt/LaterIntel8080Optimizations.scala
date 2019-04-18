package millfork.assembly.z80.opt

import millfork.assembly.AssemblyOptimization
import millfork.assembly.z80.{ZLine, ZOpcode}
import millfork.node.ZRegister
import ZOpcode._
import ZRegister._
import millfork.env.Constant

/**
  * @author Karol Stasiak
  */
object LaterIntel8080Optimizations {
  val UseExDeHl = new RuleBasedAssemblyOptimization("Use EX DE,HL",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,

    (Elidable & Is8BitLoad(D, H)) ~
      (Elidable & Is8BitLoad(E, L) & DoesntMatterWhatItDoesWith(HL)) ~~> { _ =>
      List(ZLine.implied(EX_DE_HL))
    },

    (Elidable & Is8BitLoad(H, D)) ~
      (Elidable & Is8BitLoad(L, E) & DoesntMatterWhatItDoesWith(DE)) ~~> { _ =>
      List(ZLine.implied(EX_DE_HL))
    },
  )

  val Store16BitConstantsDirectly = new RuleBasedAssemblyOptimization("Store 16-bit constants directly ",
    needsFlowInfo = FlowInfoRequirement.BothFlows,

      (Elidable & Is8BitLoad(MEM_ABS_8, A) & MatchParameter(1) & MatchRegister(A, 0)) ~
      (Elidable & Is8BitLoad(MEM_ABS_8, A) & MatchParameter(2) & DoesntMatterWhatItDoesWith(A) & DoesntMatterWhatItDoesWithFlags & DoesntMatterWhatItDoesWith(HL)) ~
      Where(ctx => ctx.get[Constant](1).+(1).quickSimplify == ctx.get[Constant](2)) ~~> { (code, ctx) =>
      val l = ctx.get[Int](0)
      val addr = ctx.get[Constant](1)
      List(
        ZLine.ldImm16(HL, 0x101 * l).pos(code.head.source),
        ZLine.ldAbs16(addr, HL).pos(code.tail.map(_.source)))
    },
  )

  val All: List[AssemblyOptimization[ZLine]] = List(
    UseExDeHl, Store16BitConstantsDirectly
  )

}
