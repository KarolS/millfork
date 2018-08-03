package millfork.assembly.mos.opt

import millfork.assembly.mos.Opcode._
import millfork.assembly.mos.AddrMode._
import millfork.assembly.AssemblyOptimization
import millfork.assembly.mos.{AssemblyLine, Opcode, State}
import millfork.env.{CompoundConstant, Constant, MathOperator}

/**
  * @author Karol Stasiak
  */
object ZeropageRegisterOptimizations {

  val functionsThatUsePseudoregisterAsInput: Map[String, Set[Int]] = Map(
    "__mul_u8u8u8" -> Set(0, 1),
    "__adc_decimal" -> Set(2, 3),
    "__sbc_decimal" -> Set(2, 3),
    "__sub_decimal" -> Set(2, 3))

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

  val ConstantDecimalMath = new RuleBasedAssemblyOptimization("Constant decimal math",
    needsFlowInfo = FlowInfoRequirement.BothFlows,

    (HasOpcode(STA) & RefersTo("__reg", 2) & MatchAddrMode(0) & MatchParameter(1) & MatchA(4)) ~
      (Linear & Not(RefersToOrUses("__reg", 3)) & DoesntChangeMemoryAt(0, 1)).* ~
      (HasOpcode(STA) & RefersTo("__reg", 3) & MatchA(5)) ~
      (Elidable & HasOpcode(JSR) & RefersTo("__add_decimal", 0) & HasClear(State.C) & DoesntMatterWhatItDoesWith(State.C)) ~~> { (code, ctx) =>
      val sum = asDecimal(ctx.get[Int](4)& 0xff, ctx.get[Int](5)& 0xff, _ + _)
      code.init :+ AssemblyLine.immediate(LDA, sum & 0xff)
    },

    (HasOpcode(STA) & RefersTo("__reg", 2) & MatchAddrMode(0) & MatchParameter(1) & MatchA(4)) ~
      (Linear & Not(RefersToOrUses("__reg", 3)) & DoesntChangeMemoryAt(0, 1)).* ~
      (HasOpcode(STA) & RefersTo("__reg", 3) & MatchA(5)) ~
      (Elidable & HasOpcode(JSR) & RefersTo("__add_decimal", 0) & HasClear(State.C)) ~~> { (code, ctx) =>
      val sum = asDecimal(ctx.get[Int](4)& 0xff, ctx.get[Int](5)& 0xff, _ + _)
      if (sum > 0xff) {
        code.init ++ List(AssemblyLine.immediate(LDA, sum & 0xff), AssemblyLine.implied(SEC))
      } else {
        code.init ++ List(AssemblyLine.immediate(LDA, sum & 0xff), AssemblyLine.implied(CLC))
      }
    },

    (HasOpcode(STA) & RefersTo("__reg", 2) & MatchAddrMode(0) & MatchParameter(1) & MatchA(4)) ~
      (Linear & Not(RefersToOrUses("__reg", 3)) & DoesntChangeMemoryAt(0, 1)).* ~
      (HasOpcode(STA) & RefersTo("__reg", 3) & MatchA(5)) ~
      (Elidable & HasOpcode(JSR) & RefersTo("__add_decimal", 0) & HasSet(State.C) & DoesntMatterWhatItDoesWith(State.C)) ~~> { (code, ctx) =>
      val sum = asDecimal(asDecimal(ctx.get[Int](4) & 0xff, ctx.get[Int](5) & 0xff, _ + _), 1, _ + _)
      code.init :+ AssemblyLine.immediate(LDA, sum & 0xff)
    },

    (HasOpcode(STA) & RefersTo("__reg", 2) & MatchAddrMode(0) & MatchParameter(1) & MatchA(4)) ~
      (Linear & Not(RefersToOrUses("__reg", 3)) & DoesntChangeMemoryAt(0, 1)).* ~
      (HasOpcode(STA) & RefersTo("__reg", 3) & MatchA(5)) ~
      (Elidable & HasOpcode(JSR) & RefersTo("__add_decimal", 0) & HasSet(State.C)) ~~> { (code, ctx) =>
      val sum = asDecimal(asDecimal(ctx.get[Int](4) & 0xff, ctx.get[Int](5) & 0xff, _ + _), 1, _ + _)
      if (sum > 0xff) {
        code.init ++ List(AssemblyLine.immediate(LDA, sum & 0xff), AssemblyLine.implied(SEC))
      } else {
        code.init ++ List(AssemblyLine.immediate(LDA, sum & 0xff), AssemblyLine.implied(CLC))
      }
    },

    (HasOpcode(STA) & RefersTo("__reg", 2) & MatchAddrMode(0) & MatchParameter(1) & MatchA(4)) ~
      (Linear & Not(RefersToOrUses("__reg", 3)) & DoesntChangeMemoryAt(0, 1)).* ~
      (HasOpcode(STA) & RefersTo("__reg", 3) & MatchA(5)) ~
      Where(ctx => ctx.get[Int](4) > ctx.get[Int](5)) ~
      (Elidable & HasOpcode(JSR) & RefersTo("__sub_decimal", 0) & HasClear(State.C) & DoesntMatterWhatItDoesWith(State.C)) ~~> { (code, ctx) =>
      val diff = asDecimal(ctx.get[Int](4)& 0xff, ctx.get[Int](5)& 0xff, _ - _)
      code.init :+ AssemblyLine.immediate(LDA, diff & 0xff)
    },
  )

  // TODO: do this in a smarter way
  val DeadRegStore = new RuleBasedAssemblyOptimization("Dead zeropage register store",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (Elidable & HasOpcode(STA) & RefersTo("__reg", 0) & MatchAddrMode(0) & MatchParameter(1)) ~
      (LinearOrLabel & DoesNotConcernMemoryAt(0, 1)).* ~
      (HasOpcodeIn(Set(RTS, RTL)) | CallsAnyExcept(functionsThatUsePseudoregisterAsInput.filter(_._2.contains(0)).keySet)) ~~> (_.tail),

    (Elidable & HasOpcode(STA) & RefersTo("__reg", 1) & MatchAddrMode(0) & MatchParameter(1)) ~
      (LinearOrLabel & DoesNotConcernMemoryAt(0, 1)).* ~
      (HasOpcodeIn(Set(RTS, RTL)) | CallsAnyExcept(functionsThatUsePseudoregisterAsInput.filter(_._2.contains(1)).keySet)) ~~> (_.tail),

    (Elidable & HasOpcode(STA) & RefersTo("__reg", 2) & MatchAddrMode(0) & MatchParameter(1)) ~
      (LinearOrLabel & DoesNotConcernMemoryAt(0, 1)).* ~
      (HasOpcodeIn(Set(RTS, RTL)) | CallsAnyExcept(functionsThatUsePseudoregisterAsInput.filter(_._2.contains(2)).keySet)) ~~> (_.tail),

    (Elidable & HasOpcode(STA) & RefersTo("__reg", 3) & MatchAddrMode(0) & MatchParameter(1)) ~
      (LinearOrLabel & DoesNotConcernMemoryAt(0, 1)).* ~
      (HasOpcodeIn(Set(RTS, RTL)) | CallsAnyExcept(functionsThatUsePseudoregisterAsInput.filter(_._2.contains(3)).keySet)) ~~> (_.tail),
  )

  val DeadRegStoreFromFlow = new RuleBasedAssemblyOptimization("Dead zeropage register store from flow",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    (Elidable & HasOpcode(STA) & RefersTo("__reg", 0) & DoesntMatterWhatItDoesWithReg(0)) ~~> (_.tail),
    (Elidable & HasOpcode(STA) & RefersTo("__reg", 1) & DoesntMatterWhatItDoesWithReg(1)) ~~> (_.tail),
    (Elidable & HasOpcode(STA) & RefersTo("__reg", 2) & DoesntMatterWhatItDoesWithReg(2)) ~~> (_.tail),
    (Elidable & HasOpcode(STA) & RefersTo("__reg", 3) & DoesntMatterWhatItDoesWithReg(3)) ~~> (_.tail),

    (Elidable & HasOpcode(LDY) & RefersTo("__reg", 0)) ~
      (Linear & Not(ConcernsY) & Not(RefersToOrUses("__reg", 0))).*.capture(2) ~
      (Elidable & (HasA(0) & HasOpcode(STA) | HasOpcode(STZ)) & RefersTo("__reg", 0) & DoesntMatterWhatItDoesWith(State.A)) ~
      ((Linear & Not(ConcernsY) & Not(RefersToOrUses("__reg", 0))).* ~
        (Elidable & RefersToOrUses("__reg", 0) & HasAddrMode(IndexedY) & DoesntMatterWhatItDoesWithReg(0) & DoesntMatterWhatItDoesWith(State.Y))).capture(3) ~~> ((code, ctx) =>
      ctx.get[List[AssemblyLine]](2) ++ List(AssemblyLine.immediate(LDY, 0)) ++ ctx.get[List[AssemblyLine]](3)
      )

  )

  val StashInRegInsteadOfStack = new RuleBasedAssemblyOptimization("Stashing in zeropage register instead of stack",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    MultipleAssemblyRules((0 to 1).map{ zregIndex =>
      (Elidable & HasOpcode(PHA) & DoesntMatterWhatItDoesWithReg(zregIndex)) ~
        (Linear & Not(ConcernsS) & Not(RefersToOrUses("__reg", zregIndex))).*.capture(21) ~
        (Elidable & HasOpcode(TSX)) ~
        HasOpcodeIn(CLC, SED, CLD, SEC).*.capture(22) ~
        (Elidable & MatchOpcode(1) & HasAddrMode(AbsoluteX) & HasParameterWhere(p => p.isProvably(0x101))).* ~
        (Elidable & HasOpcode(INX)) ~
        (Elidable & HasOpcode(TXS) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~~> { (code, ctx) =>
        AssemblyLine.zeropage(STA, ctx.zreg(zregIndex)) :: (
          ctx.get[List[AssemblyLine]](21) ++
            ctx.get[List[AssemblyLine]](22) ++ List(
            AssemblyLine.zeropage(ctx.get[Opcode.Value](1), ctx.zreg(zregIndex)),
            AssemblyLine.implied(TSX)))
      }
    })
  )


  private def parseNormalToDecimalValue(a: Long): Long = {
    if (a < 0) -parseNormalToDecimalValue(-a)
    var x = a
    var result = 0L
    var multiplier = 1L
    while (x > 0) {
      result += multiplier * (x % 16L)
      x /= 16L
      multiplier *= 10L
    }
    result
  }

  private def storeDecimalValueInNormalRespresentation(a: Long): Long = {
    if (a < 0) -storeDecimalValueInNormalRespresentation(-a)
    var x = a
    var result = 0L
    var multiplier = 1L
    while (x > 0) {
      result += multiplier * (x % 10L)
      x /= 10L
      multiplier *= 16L
    }
    result
  }

  private def asDecimal(a: Long, b: Long, f: (Long, Long) => Long): Long =
    storeDecimalValueInNormalRespresentation(f(parseNormalToDecimalValue(a), parseNormalToDecimalValue(b)))

  val All: List[AssemblyOptimization[AssemblyLine]] = List(
    ConstantDecimalMath,
    ConstantMultiplication,
    DeadRegStore,
    DeadRegStoreFromFlow,
    StashInRegInsteadOfStack,
  )

}
