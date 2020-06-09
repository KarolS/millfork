package millfork.assembly.m6809.opt


import millfork.assembly.AssemblyOptimization
import millfork.assembly.m6809.MOpcode._
import millfork.assembly.m6809.{MLine, MState}
import millfork.node.M6809Register

/**
  * @author Karol Stasiak
  */
object AlwaysGoodMOptimizations {

  val PointlessLoad = new RuleBasedAssemblyOptimization("Pointless load",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    (Elidable & HasOpcodeIn(LDA, ANDA, ORA, EORA) & DoesntMatterWhatItDoesWith(MState.A, MState.NF, MState.ZF, MState.VF)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(LDB, ANDB, ORB, EORB) & DoesntMatterWhatItDoesWith(MState.B, MState.NF, MState.ZF, MState.VF)) ~~> (_ => Nil),
    (Elidable & HasOpcode(LDD) & DoesntMatterWhatItDoesWith(MState.A, MState.B, MState.NF, MState.ZF, MState.VF)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(LDX, LEAX) & DoesntMatterWhatItDoesWith(MState.X, MState.NF, MState.ZF, MState.VF)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(LDY, LEAY) & DoesntMatterWhatItDoesWith(MState.Y, MState.NF, MState.ZF, MState.VF)) ~~> (_ => Nil),
  )

  val SimplifiableZeroStore = new RuleBasedAssemblyOptimization("Simplifiable zero store",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    (Elidable & HasOpcode(LDA) & HasImmediate(0) & DoesntMatterWhatItDoesWith(MState.CF)) ~~> {
      _ => List(MLine.inherentA(CLR))
    },
    (Elidable & HasOpcode(LDB) & HasImmediate(0) & DoesntMatterWhatItDoesWith(MState.CF)) ~~> {
      _ => List(MLine.inherentB(CLR))
    },
    (Elidable & HasOpcode(LDD) & HasImmediate(0) & DoesntMatterWhatItDoesWith(MState.A, MState.CF)) ~~> {
      _ => List(MLine.inherentB(CLR))
    },
    (Elidable & HasOpcode(LDD) & HasImmediate(0) & DoesntMatterWhatItDoesWith(MState.B, MState.CF)) ~~> {
      _ => List(MLine.inherentA(CLR))
    },
    (Elidable & HasOpcode(STA) & HasA(0) & DoesntMatterWhatItDoesWith(MState.CF)) ~~> {
      code => code.map(_.copy(opcode = CLR))
    },
    (Elidable & HasOpcode(STB) & HasB(0) & DoesntMatterWhatItDoesWith(MState.CF)) ~~> {
      code => code.map(_.copy(opcode = CLR))
    },
  )

  val PointlessRegisterTransfers = new RuleBasedAssemblyOptimization("Pointless register transfers",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    (Elidable & IsTfr(M6809Register.D, M6809Register.X)) ~
      (Elidable & IsTfr(M6809Register.X, M6809Register.D)) ~~> (_.init),
    (Elidable & IsTfr(M6809Register.A, M6809Register.B)) ~
      (Elidable & IsTfr(M6809Register.B, M6809Register.A)) ~~> (_.init),
    (Elidable & IsTfrTo(M6809Register.B) & DoesntMatterWhatItDoesWith(MState.B)) ~~> (_ => Nil),
    (Elidable & IsTfrTo(M6809Register.A) & DoesntMatterWhatItDoesWith(MState.A)) ~~> (_ => Nil),
    (Elidable & IsTfrTo(M6809Register.D) & DoesntMatterWhatItDoesWith(MState.A, MState.B)) ~~> (_ => Nil),
    (Elidable & IsTfrTo(M6809Register.X) & DoesntMatterWhatItDoesWith(MState.X)) ~~> (_ => Nil),
  )

  val All: Seq[AssemblyOptimization[MLine]] = Seq(
    PointlessLoad,
    PointlessRegisterTransfers,
    SimplifiableZeroStore
  )
}
