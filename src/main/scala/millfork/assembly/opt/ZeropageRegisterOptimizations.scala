package millfork.assembly.opt

import millfork.assembly.Opcode._
import millfork.assembly.AddrMode._
import millfork.assembly.AssemblyLine
import millfork.env.{CompoundConstant, Constant, MathOperator}

/**
  * @author Karol Stasiak
  */
object ZeropageRegisterOptimizations {

  private val functionsThatUsePseudoregisterAsInput = Set("__mul_u8u8u8")

  val ConstantMultiplication = new RuleBasedAssemblyOptimization("Constant multiplication",
    needsFlowInfo = FlowInfoRequirement.ForwardFlow,
    (HasOpcode(STA) & RefersTo("__reg", 0) & MatchAddrMode(0) & MatchParameter(1) & MatchA(4)) ~
      (Linear & Not(RefersTo("__reg", 1)) & DoesntChangeMemoryAt(0, 1)).* ~
      (HasOpcode(STA) & RefersTo("__reg", 1) & MatchA(5)) ~
      (Elidable & HasOpcode(JSR) & RefersTo("__mul_u8u8u8", 0)) ~~> { (code, ctx) =>
      val product = ctx.get[Int](4) * ctx.get[Int](5)
      code.init :+ AssemblyLine.immediate(LDA, product & 0xff)
    },

    (Elidable & HasOpcode(STA) & RefersTo("__reg", 0) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(RefersTo("__reg", 1)) & DoesntChangeMemoryAt(0, 1)).* ~
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
      (Linear & Not(RefersTo("__reg", 1)) & DoesntChangeMemoryAt(0, 1)).* ~
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

  val All: List[AssemblyOptimization] = List(
    ConstantMultiplication,
    DeadRegStore,
  )

}
