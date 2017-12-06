package millfork.assembly.opt

import millfork.assembly._
import millfork.assembly.Opcode._
import millfork.assembly.AddrMode._
import millfork.env._

/**
  * @author Karol Stasiak
  */
object DangerousOptimizations {

  val ConstantIndexOffsetPropagation = new RuleBasedAssemblyOptimization("Constant index offset propagation",
    // TODO: try to guess when overflow can happen
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    (Elidable & HasOpcode(CLC)).? ~
      (Elidable & HasClear(State.C) & HasOpcode(ADC) & MatchImmediate(0) & DoesntMatterWhatItDoesWith(State.V, State.C)) ~
      (
        (HasOpcode(TAY) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.A)) ~
        (Linear & Not(ConcernsY)).*
        ).capture(1) ~
      (Elidable & HasAddrMode(AbsoluteY) & DoesntMatterWhatItDoesWith(State.Y)) ~~> { (code, ctx) =>
      val last = code.last
      ctx.get[List[AssemblyLine]](1) :+ last.copy(parameter = last.parameter.+(ctx.get[Constant](0)).quickSimplify)
    },
    (Elidable & HasOpcode(CLC)).? ~
      (Elidable & HasClear(State.C) & HasOpcode(ADC) & MatchImmediate(0) & DoesntMatterWhatItDoesWith(State.V, State.C)) ~
      (
        (HasOpcode(TAX) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.A)) ~
        (Linear & Not(ConcernsX)).*
        ).capture(1) ~
      (Elidable & HasAddrMode(AbsoluteX) & DoesntMatterWhatItDoesWith(State.X)) ~~> { (code, ctx) =>
      val last = code.last
      ctx.get[List[AssemblyLine]](1) :+ last.copy(parameter = last.parameter.+(ctx.get[Constant](0)).quickSimplify)
    },
    (Elidable & HasOpcode(INY) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~
      (Elidable & HasAddrMode(AbsoluteY) & DoesntMatterWhatItDoesWith(State.Y)) ~~> { (code, ctx) =>
      val last = code.last
      List(last.copy(parameter = last.parameter.+(1).quickSimplify))
    },
    (Elidable & HasOpcode(DEY) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~
      (Elidable & HasAddrMode(AbsoluteY) & DoesntMatterWhatItDoesWith(State.Y)) ~~> { (code, ctx) =>
      val last = code.last
      List(last.copy(parameter = last.parameter.+(-1).quickSimplify))
    },
    (Elidable & HasOpcode(INX) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~
      (Elidable & HasAddrMode(AbsoluteX) & DoesntMatterWhatItDoesWith(State.X)) ~~> { (code, ctx) =>
      val last = code.last
      List(last.copy(parameter = last.parameter.+(1).quickSimplify))
    },
    (Elidable & HasOpcode(DEX) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~
      (Elidable & HasAddrMode(AbsoluteX) & DoesntMatterWhatItDoesWith(State.X)) ~~> { (code, ctx) =>
      val last = code.last
      List(last.copy(parameter = last.parameter.+(-1).quickSimplify))
    },
  )

  val All: List[AssemblyOptimization] = List(ConstantIndexOffsetPropagation)
}
