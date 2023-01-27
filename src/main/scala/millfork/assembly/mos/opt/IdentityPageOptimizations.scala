package millfork.assembly.mos.opt

import millfork.assembly.mos.AddrMode._
import millfork.assembly.mos.{AssemblyLine, State}
import millfork.assembly.mos.Opcode._
import millfork.env.NumericConstant

object IdentityPageOptimizations {

  val SimplifiableAccess = new RuleBasedAssemblyOptimization("Simplifiable identity page access",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (Elidable & HasOpcode(LDA) & HasAddrMode(AbsoluteX) & HasIdentityPageParameter) ~~> { c =>
      List(AssemblyLine.implied(TXA))
    },
    (Elidable & HasOpcode(LDA) & HasAddrMode(AbsoluteY) & HasIdentityPageParameter) ~~> { c =>
      List(AssemblyLine.implied(TYA))
    },
  )

  val UseInsteadOfStack = new RuleBasedAssemblyOptimization("Use identity page instead of stack",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    (Elidable & HasOpcode(PHA)) ~
      (Not(ConcernsX) & Not(ConcernsStack)).*.capture(0) ~
      (Elidable & HasOpcode(TSX) & HasAddrMode(AbsoluteX) & HasIdentityPageParameter) ~
      (Elidable & HasAddrMode(AbsoluteX) & HasParameterWhere{
        case NumericConstant(0x101, _) => true
        case _ => false
      }).capture(1) ~
      (Elidable & HasOpcode(INX)) ~
      (Elidable & HasOpcode(TXS) & DoesntMatterWhatItDoesWith(State.X)) ~~> { (code, ctx) =>
      List(AssemblyLine.implied(TAX)) ++ ctx.get[List[AssemblyLine]](0) ++ List(ctx.get[AssemblyLine](1).copy(parameter = ctx.identityPage))
    },
    (Elidable & HasOpcode(PHA)) ~
      (Not(ConcernsY) & Not(ConcernsStack)).*.capture(0) ~
      (Elidable & HasOpcode(TSX) & HasAddrMode(AbsoluteX) & HasIdentityPageParameter) ~
      (Elidable & HasAddrMode(AbsoluteX) & HasParameterWhere{
        case NumericConstant(0x101, _) => true
        case _ => false
      }).capture(1) ~
      (Elidable & HasOpcode(INX)) ~
      (Elidable & HasOpcode(TXS) & DoesntMatterWhatItDoesWith(State.Y)) ~~> { (code, ctx) =>
      List(AssemblyLine.implied(TAY)) ++ ctx.get[List[AssemblyLine]](0) ++ List(ctx.get[AssemblyLine](1).copy(parameter = ctx.identityPage, addrMode = AbsoluteY))
    },
  )

  val All: List[RuleBasedAssemblyOptimization] = List(
    SimplifiableAccess,
    UseInsteadOfStack)

}
