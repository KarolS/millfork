package millfork.assembly.mos.opt

import millfork.assembly.mos.Opcode._
import millfork.assembly.mos.AddrMode._
import millfork.assembly.AssemblyOptimization
import millfork.assembly.mos.{AssemblyLine, State}
import millfork.env.{CompoundConstant, Constant, MathOperator}

/**
  * @author Karol Stasiak
  */
object ZeropageRegisterOptimizations {

  val functionsThatUsePseudoregisterAsInput = Set("__mul_u8u8u8")

  val ConstantMultiplication = new RuleBasedAssemblyOptimization("Constant multiplication",
    needsFlowInfo = FlowInfoRequirement.ForwardFlow,
    (HasOpcode(STA) & RefersTo("__reg", 0) & MatchAddrMode(0) & MatchParameter(1) & MatchA(4)) ~
      (Linear & Not(RefersToOrUses("__reg", 1)) & DoesntChangeMemoryAt(0, 1)).* ~
      (HasOpcode(STA) & RefersTo("__reg", 1) & MatchA(5)) ~
      (Elidable & HasOpcode(JSR) & RefersTo("__mul_u8u8u8", 0)) ~~> { (code, ctx) =>
      val product = ctx.get[Int](4) * ctx.get[Int](5)
      code.init :+ AssemblyLine.immediate(LDA, product & 0xff)
    },

    // TODO: constants other than power of 2:

    (Elidable & HasOpcode(STA) & RefersTo("__reg", 0) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(RefersToOrUses("__reg", 1)) & DoesntChangeMemoryAt(0, 1)).* ~
      (HasOpcode(STA) & RefersTo("__reg", 1) & MatchA(4)) ~
      Where(ctx => {
        val constant = ctx.get[Int](4)
        (constant & (constant - 1)) == 0
      }) ~
      (Elidable & HasOpcode(JSR) & RefersTo("__mul_u8u8u8", 0)) ~~> { (code, ctx) =>
      val constant = ctx.get[Int](4)
      if (constant == 0) {
          code.init :+ AssemblyLine.immediate(LDA, 0)
      } else {
        code.init ++ (code.head.copy(opcode = LDA) :: List.fill(Integer.numberOfTrailingZeros(constant))(AssemblyLine.implied(ASL)))
      }
    },

    (HasOpcode(STA) & RefersTo("__reg", 0) & MatchAddrMode(0) & MatchParameter(1) & MatchA(4)) ~
      Where(ctx => {
        val constant = ctx.get[Int](4)
        (constant & (constant - 1)) == 0
      }) ~
      (Linear & Not(RefersToOrUses("__reg", 1)) & DoesntChangeMemoryAt(0, 1)).* ~
      (HasOpcode(STA) & RefersTo("__reg", 1)) ~
      (Elidable & HasOpcode(JSR) & RefersTo("__mul_u8u8u8", 0)) ~~> { (code, ctx) =>
      val constant = ctx.get[Int](4)
      if (constant == 0) {
          code.init :+ AssemblyLine.immediate(LDA, 0)
      } else {
        code.init ++ List.fill(Integer.numberOfTrailingZeros(constant))(AssemblyLine.implied(ASL))
      }
    },
  )

  // TODO: do this in a smarter way
  val DeadRegStore = new RuleBasedAssemblyOptimization("Dead zeropage register store",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (Elidable & HasOpcode(STA) & RefersTo("__reg", 0) & MatchAddrMode(0) & MatchParameter(1)) ~
      (LinearOrLabel & DoesNotConcernMemoryAt(0, 1)).* ~
      (HasOpcodeIn(Set(RTS, RTL)) | CallsAnyExcept(functionsThatUsePseudoregisterAsInput)) ~~> (_.tail),
    (Elidable & HasOpcode(STA) & RefersTo("__reg", 1) & MatchAddrMode(0) & MatchParameter(1)) ~
      (LinearOrLabel & DoesNotConcernMemoryAt(0, 1)).* ~
      (HasOpcodeIn(Set(RTS, RTL)) | CallsAnyExcept(functionsThatUsePseudoregisterAsInput)) ~~> (_.tail),
  )

  val DeadRegStoreFromFlow = new RuleBasedAssemblyOptimization("Dead zeropage register store from flow",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    (Elidable & HasOpcode(STA) & RefersTo("__reg", 0) & DoesntMatterWhatItDoesWithReg(0)) ~~> (_.tail),
    (Elidable & HasOpcode(STA) & RefersTo("__reg", 1) & DoesntMatterWhatItDoesWithReg(1)) ~~> (_.tail),

    (Elidable & HasOpcode(LDY) & RefersTo("__reg", 0)) ~
      (Linear & Not(ConcernsY) & Not(RefersToOrUses("__reg", 0))).*.capture(2) ~
      (Elidable & (HasA(0) & HasOpcode(STA) | HasOpcode(STZ)) & RefersTo("__reg", 0) & DoesntMatterWhatItDoesWith(State.A)) ~
      ((Linear & Not(ConcernsY) & Not(RefersToOrUses("__reg", 0))).* ~
        (Elidable & RefersToOrUses("__reg", 0) & HasAddrMode(IndexedY) & DoesntMatterWhatItDoesWithReg(0) & DoesntMatterWhatItDoesWith(State.Y))).capture(3) ~~> ((code, ctx) =>
      ctx.get[List[AssemblyLine]](2) ++ List(AssemblyLine.immediate(LDY, 0)) ++ ctx.get[List[AssemblyLine]](3)
      )

  )

  val All: List[AssemblyOptimization[AssemblyLine]] = List(
    ConstantMultiplication,
    DeadRegStore,
    DeadRegStoreFromFlow,
  )

}
