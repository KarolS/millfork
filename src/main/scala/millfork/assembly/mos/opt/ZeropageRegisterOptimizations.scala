package millfork.assembly.mos.opt

import millfork.assembly.mos.Opcode._
import millfork.assembly.mos.AddrMode._
import millfork.assembly.AssemblyOptimization
import millfork.assembly.mos.{AssemblyLine, AssemblyLine0, Opcode, State}
import millfork.DecimalUtils.asDecimal
import millfork.assembly.z80.opt.HasRegister
import millfork.error.FatalErrorReporting
/**
  * @author Karol Stasiak
  */
object ZeropageRegisterOptimizations {

  val functionsThatUsePseudoregisterAsInput: Map[String, Set[Int]] = Map(
    "call" -> Set(2, 3),
    "__mul_u8u8u8" -> Set(0, 1),
    "__mod_u8u8u8u8" -> Set(0, 1),
    "__div_u8u8u8u8" -> Set(0, 1),
    "__mod_u16u8u16u8" -> Set(0, 1, 2),
    "__div_u16u8u16u8" -> Set(0, 1, 2),
    "__mul_u16u8u16" -> Set(0, 1, 2),
    "__adc_decimal" -> Set(2, 3),
    "__sbc_decimal" -> Set(2, 3),
    "__sub_decimal" -> Set(2, 3))

  val ConstantInlinedMultiplication = new RuleBasedAssemblyOptimization("Constant inlined multiplication",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    (Elidable & HasOpcode(LDA) & HasImmediate(0) & MatchZpReg(4, 0) & MatchZpReg(5, 1)) ~
      (Elidable & HasOpcodeIn(JMP, BEQ) & MatchParameter(13)) ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(11)) ~
      (Elidable & HasOpcode(CLC)) ~
      (Elidable & HasOpcode(ADC) & RefersTo("__reg", 0)) ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(12)) ~
      (Elidable & HasOpcode(ASL) & RefersTo("__reg", 0)) ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(13)) ~
      (Elidable & HasOpcode(LSR) & RefersTo("__reg", 1)) ~
      (Elidable & HasOpcode(BCS) & MatchParameter(11)) ~
      (Elidable & HasOpcode(BNE) & MatchParameter(12) & DoesntMatterWhatItDoesWith(State.N, State.Z, State.V)) ~
      (Not(RefersTo("__reg")) & DoesntMatterWhatItDoesWithReg(0) & DoesntMatterWhatItDoesWithReg(1)) ~~> { (code, ctx) =>
      val product = ctx.get[Int](4) * ctx.get[Int](5)
      List(AssemblyLine.immediate(LDA, product & 0xff), AssemblyLine.implied(CLC), code.last)
    },
  )

  private def compileMultiply[T](multiplicand: Int, add1:List[T], asl: List[T]): List[T] = {
    if (multiplicand == 0) FatalErrorReporting.reportFlyingPig("Trying to optimize multiplication by 0 in a wrong way!")
    def impl(m: Int): List[List[T]] = {
      if (m == 1) Nil
      else if (m % 2 == 0) asl :: impl(m / 2)
      else add1 :: asl :: impl(m / 2)
    }
    impl(multiplicand).reverse.flatten
  }

  val ConstantMultiplication = new RuleBasedAssemblyOptimization("Constant multiplication",
    needsFlowInfo = FlowInfoRequirement.ForwardFlow,
    (HasOpcode(STA) & RefersTo("__reg", 0) & MatchAddrMode(0) & MatchParameter(1) & MatchA(4)) ~
      (Linear & Not(RefersToOrUses("__reg", 1)) & DoesntChangeMemoryAt(0, 1)).* ~
      (HasOpcode(STA) & RefersTo("__reg", 1) & MatchA(5)) ~
      (Elidable & HasOpcode(JSR) & RefersTo("__mul_u8u8u8", 0)) ~~> { (code, ctx) =>
      val product = ctx.get[Int](4) * ctx.get[Int](5)
      code.init :+ AssemblyLine.immediate(LDA, product & 0xff)
    },

    (Elidable & HasOpcode(JSR) & RefersTo("__mul_u8u8u8", 0) & MatchZpReg(4, 0) & MatchZpReg(5, 1)) ~~> { (code, ctx) =>
      val product = ctx.get[Int](4) * ctx.get[Int](5)
      code.init :+ AssemblyLine.immediate(LDA, product & 0xff)
    },

    // TODO: constants other than power of 2:

    (Elidable & HasOpcode(STA) & RefersTo("__reg", 0) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & Not(RefersToOrUses("__reg", 1)) & DoesntChangeMemoryAt(0, 1)).* ~
      (HasOpcode(STA) & RefersTo("__reg", 1) & MatchA(4)) ~
      (Elidable & HasOpcode(JSR) & RefersTo("__mul_u8u8u8", 0)) ~~> { (code, ctx) =>
      val constant = ctx.get[Int](4)
      if (constant == 0) {
          code.init :+ AssemblyLine.immediate(LDA, 0)
      } else {
        code.init ++ (code.head.copy(opcode = LDA) :: compileMultiply(constant, List(AssemblyLine.implied(CLC), code.head.copy(opcode = ADC)), List(AssemblyLine.implied(ASL))))
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
        code.init ++ compileMultiply(constant, List(AssemblyLine.implied(CLC), code.init.last.copy(opcode = ADC)), List(AssemblyLine.implied(ASL)))
      }
    },

    (Elidable & HasOpcode(STA) & RefersTo("__reg", 2) & MatchAddrMode(0) & MatchParameter(1) & MatchA(4)) ~
      Where(ctx => {
        val constant = ctx.get[Int](4)
        (constant & (constant - 1)) == 0
      }) ~
      (Linear & Not(RefersToOrUses("__reg", 2)) & DoesntChangeMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(JSR) & RefersTo("__mul_u16u8u16", 0)) ~~> { (code, ctx) =>
      val constant = ctx.get[Int](4)
      if (constant == 0) {
          code.init :+ AssemblyLine.immediate(LDA, 0)
      } else {
        val loAsl = code.head.copy(opcode = ASL, parameter = (code.head.parameter - 2).quickSimplify)
        val hiRol = code.head.copy(opcode = ROL, parameter = (code.head.parameter - 1).quickSimplify)
        val shift = List(loAsl, hiRol)
        code.init ++ List.fill(Integer.numberOfTrailingZeros(constant))(shift).flatten ++ List(loAsl.copy(opcode = LDA), hiRol.copy(opcode = LDX))
      }
    },
  )

  val ConstantDivision = new RuleBasedAssemblyOptimization("Constant division",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    // TODO: constants other than power of 2:

    (HasOpcodeIn(STA, STX, STY) & RefersTo("__reg", 1) & MatchStoredRegister(2) & MatchAddrMode(0) & MatchParameter(1)) ~
      Where({ ctx =>
        val a = ctx.get[Int](2)
        a != 0 && a.-(1).&(a) == 0
      }) ~
      (Linear & DoesntChangeMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(JSR) & RefersTo("__div_u8u8u8u8", 0)
        & DoesntMatterWhatItDoesWith(State.C, State.Z, State.N, State.V) // everything else (including Y) should be preserved
        & DoesntMatterWhatItDoesWithReg(0)
        & DoesntMatterWhatItDoesWithReg(1)) ~~> { (code, ctx) =>
      val count = Integer.numberOfTrailingZeros(ctx.get[Int](2))
      val zreg = ctx.zeropageRegister.get
      code.init ++ List(AssemblyLine.zeropage(LDA, zreg)) ++ List.fill(count)(AssemblyLine.implied(LSR))
    },

    (HasOpcodeIn(STA, STX, STY) & RefersTo("__reg", 1) & MatchStoredRegister(2) & MatchAddrMode(0) & MatchParameter(1)) ~
      Where({ ctx =>
        val a = ctx.get[Int](2)
        a != 0 && a.-(1).&(a) == 0
      }) ~
      (Linear & DoesntChangeMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(JSR) & RefersTo("__mod_u8u8u8u8", 0)
        & DoesntMatterWhatItDoesWith(State.C, State.Z, State.N, State.V, State.X) // everything else (including Y) should be preserved
        & DoesntMatterWhatItDoesWithReg(0)
        & DoesntMatterWhatItDoesWithReg(1)) ~~> { (code, ctx) =>
      val a = ctx.get[Int](2)
      val zreg = ctx.zeropageRegister.get
      code.init ++ List(AssemblyLine.zeropage(LDA, zreg), AssemblyLine.immediate(AND, a - 1))
    },

    (HasOpcodeIn(STA, STX, STY) & RefersTo("__reg", 2) & MatchStoredRegister(2) & MatchAddrMode(0) & MatchParameter(1)) ~
      Where({ ctx =>
        val a = ctx.get[Int](2)
        a != 0 && a.-(1).&(a) == 0
      }) ~
      (Linear & DoesntChangeMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(JSR) & RefersTo("__div_u16u8u16u8", 0)
        & DoesntMatterWhatItDoesWith(State.C, State.Z, State.N, State.V) // everything else (including Y) should be preserved
        & DoesntMatterWhatItDoesWithReg(0)
        & DoesntMatterWhatItDoesWithReg(1)
        & DoesntMatterWhatItDoesWithReg(2)) ~~> { (code, ctx) =>
      val count = Integer.numberOfTrailingZeros(ctx.get[Int](2))
      val zreg = ctx.zeropageRegister.get
      code.init ++
        List.fill(count)(List(AssemblyLine.zeropage(LSR, zreg, 1), AssemblyLine.zeropage(ROR, zreg))).flatten ++
        List(AssemblyLine.zeropage(LDA, zreg), AssemblyLine.zeropage(LDX, zreg, 1))
    },

    (HasOpcodeIn(STA, STX, STY) & RefersTo("__reg", 2) & MatchStoredRegister(2) & MatchAddrMode(0) & MatchParameter(1)) ~
      Where({ ctx =>
        val a = ctx.get[Int](2)
        a != 0 && a.-(1).&(a) == 0 && a <= 128
      }) ~
      (Linear & DoesntChangeMemoryAt(0, 1)).* ~
      (Elidable & HasOpcode(JSR) & RefersTo("__mod_u16u8u16u8", 0)
        & DoesntMatterWhatItDoesWith(State.C, State.Z, State.N, State.V, State.X) // everything else (including Y) should be preserved
        & DoesntMatterWhatItDoesWithReg(0)
        & DoesntMatterWhatItDoesWithReg(1)
        & DoesntMatterWhatItDoesWithReg(2)) ~~> { (code, ctx) =>
      val a = ctx.get[Int](2)
      val zreg = ctx.zeropageRegister.get
      code.init ++ List(AssemblyLine.zeropage(LDA, zreg), AssemblyLine.immediate(AND, a - 1), AssemblyLine.immediate(LDX, 0))
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
      (HasOpcodeIn(Set(RTS, RTL)) | HasOpcodeIn(JSR, JMP) & CallsAnyExcept(functionsThatUsePseudoregisterAsInput.filter(_._2.contains(0)).keySet)) ~~> (_.tail),

    (Elidable & HasOpcode(STA) & RefersTo("__reg", 1) & MatchAddrMode(0) & MatchParameter(1)) ~
      (LinearOrLabel & DoesNotConcernMemoryAt(0, 1)).* ~
      (HasOpcodeIn(Set(RTS, RTL)) | HasOpcodeIn(JSR, JMP) & CallsAnyExcept(functionsThatUsePseudoregisterAsInput.filter(_._2.contains(1)).keySet)) ~~> (_.tail),

    (Elidable & HasOpcode(STA) & RefersTo("__reg", 2) & MatchAddrMode(0) & MatchParameter(1)) ~
      (LinearOrLabel & DoesNotConcernMemoryAt(0, 1)).* ~
      (HasOpcodeIn(Set(RTS, RTL)) | HasOpcodeIn(JSR, JMP) & CallsAnyExcept(functionsThatUsePseudoregisterAsInput.filter(_._2.contains(2)).keySet)) ~~> (_.tail),

    (Elidable & HasOpcode(STA) & RefersTo("__reg", 3) & MatchAddrMode(0) & MatchParameter(1)) ~
      (LinearOrLabel & DoesNotConcernMemoryAt(0, 1)).* ~
      (HasOpcodeIn(Set(RTS, RTL)) | HasOpcodeIn(JSR, JMP) & CallsAnyExcept(functionsThatUsePseudoregisterAsInput.filter(_._2.contains(3)).keySet)) ~~> (_.tail),
  )

  val DeadRegStoreFromFlow = new RuleBasedAssemblyOptimization("Dead zeropage register store from flow",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    (Elidable & HasOpcodeIn(STA, STX, SAX, STY, STZ) & RefersTo("__reg", 0) & DoesntMatterWhatItDoesWithReg(0)) ~~> (_.tail),
    (Elidable & HasOpcodeIn(STA, STX, SAX, STY, STZ) & RefersTo("__reg", 1) & DoesntMatterWhatItDoesWithReg(1)) ~~> (_.tail),
    (Elidable & HasOpcodeIn(STA, STX, SAX, STY, STZ) & RefersTo("__reg", 2) & DoesntMatterWhatItDoesWithReg(2)) ~~> (_.tail),
    (Elidable & HasOpcodeIn(STA, STX, SAX, STY, STZ) & RefersTo("__reg", 3) & DoesntMatterWhatItDoesWithReg(3)) ~~> (_.tail),
    (Elidable & HasOpcodeIn(ROL, ROR, ASL, LSR, INC, DEC) & RefersTo("__reg", 0) & Not(DoesntMatterWhatItDoesWithReg(0))).* ~
      (Elidable & HasOpcodeIn(ROL, ROR, ASL, LSR, INC, DEC) & RefersTo("__reg", 0) & DoesntMatterWhatItDoesWithReg(0) & DoesntMatterWhatItDoesWith(State.C, State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(ROL, ROR, ASL, LSR, INC, DEC) & RefersTo("__reg", 1) & Not(DoesntMatterWhatItDoesWithReg(1))).* ~
      (Elidable & HasOpcodeIn(ROL, ROR, ASL, LSR, INC, DEC) & RefersTo("__reg", 1) & DoesntMatterWhatItDoesWithReg(1) & DoesntMatterWhatItDoesWith(State.C, State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(ROL, ROR, ASL, LSR, INC, DEC) & RefersTo("__reg", 2) & Not(DoesntMatterWhatItDoesWithReg(2))).* ~
      (Elidable & HasOpcodeIn(ROL, ROR, ASL, LSR, INC, DEC) & RefersTo("__reg", 2) & DoesntMatterWhatItDoesWithReg(2) & DoesntMatterWhatItDoesWith(State.C, State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(ROL, ROR, ASL, LSR, INC, DEC) & RefersTo("__reg", 3) & Not(DoesntMatterWhatItDoesWithReg(3))).* ~
      (Elidable & HasOpcodeIn(ROL, ROR, ASL, LSR, INC, DEC) & RefersTo("__reg", 3) & DoesntMatterWhatItDoesWithReg(3) & DoesntMatterWhatItDoesWith(State.C, State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(INC, DEC) & RefersTo("__reg", 0) & Not(DoesntMatterWhatItDoesWithReg(0))).* ~
      (Elidable & HasOpcodeIn(INC, DEC) & RefersTo("__reg", 0) & DoesntMatterWhatItDoesWithReg(0) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(INC, DEC) & RefersTo("__reg", 1) & Not(DoesntMatterWhatItDoesWithReg(1))).* ~
      (Elidable & HasOpcodeIn(INC, DEC) & RefersTo("__reg", 1) & DoesntMatterWhatItDoesWithReg(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(INC, DEC) & RefersTo("__reg", 2) & Not(DoesntMatterWhatItDoesWithReg(2))).* ~
      (Elidable & HasOpcodeIn(INC, DEC) & RefersTo("__reg", 2) & DoesntMatterWhatItDoesWithReg(2) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_ => Nil),
    (Elidable & HasOpcodeIn(INC, DEC) & RefersTo("__reg", 3) & Not(DoesntMatterWhatItDoesWithReg(3))).* ~
      (Elidable & HasOpcodeIn(INC, DEC) & RefersTo("__reg", 3) & DoesntMatterWhatItDoesWithReg(3) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_ => Nil),

    (Elidable & HasOpcode(LDY) & RefersTo("__reg", 0)) ~
      (Linear & Not(ConcernsY) & Not(RefersToOrUses("__reg", 0))).*.capture(2) ~
      (Elidable & (HasA(0) & HasOpcode(STA) | HasZ(0) & HasOpcode(STZ)) & RefersTo("__reg", 0) & DoesntMatterWhatItDoesWith(State.A)) ~
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
    }),
    MultipleAssemblyRules((0 to 1).map { zregIndex =>
      (Elidable & HasOpcode(PHA) & DoesntMatterWhatItDoesWithReg(zregIndex)) ~
        (Linear & Not(ConcernsS) & Not(RefersToOrUses("__reg", zregIndex))).*.capture(21) ~
        (Elidable & HasOpcode(STA) & HasAddrModeIn(Absolute, ZeroPage) & RefersToOrUses("__reg", zregIndex)) ~
        (Elidable & HasOpcode(PLA)) ~
        (HasOpcodeIn(LDY, LDX, CLC, SEC, CLD, SED)).*.capture(22) ~
        (Elidable & HasOpcodeIn(ORA, EOR, ADC, AND) & HasAddrModeIn(Absolute, ZeroPage) & RefersToOrUses("__reg", zregIndex) & DoesntMatterWhatItDoesWithReg(zregIndex)) ~~> { (code, ctx) =>
        List(AssemblyLine.zeropage(STA, ctx.zreg(zregIndex)).pos(code.head.source)) ++
          ctx.get[List[AssemblyLine]](21) ++
          ctx.get[List[AssemblyLine]](22) ++
          List(code.last)
      }
    }),
  )

  val PointlessLoad = new RuleBasedAssemblyOptimization("Pointless load from zeropage register",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    MultipleAssemblyRules((0 until 4).flatMap{ zpregIndex =>
      List(
        (Elidable & HasOpcode(LDA) & RefersTo("__reg", zpregIndex) &
          MatchZpReg(4, zpregIndex) & MatchA(4) & 
          DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_ => Nil),
        (Elidable & HasOpcode(LDX) & RefersTo("__reg", zpregIndex) &
          MatchZpReg(4, zpregIndex) & MatchX(4) & 
          DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_ => Nil),
        (Elidable & HasOpcode(LDY) & RefersTo("__reg", zpregIndex) &
          MatchZpReg(4, zpregIndex) & MatchY(4) &
          DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> (_ => Nil),

        (Elidable & HasOpcode(LDA) & RefersTo("__reg", zpregIndex) &
          MatchZpReg(4, zpregIndex) & MatchX(4)) ~~> (_ => List(AssemblyLine.implied(TXA))),
        (Elidable & HasOpcode(LDA) & RefersTo("__reg", zpregIndex) &
          MatchZpReg(4, zpregIndex) & MatchY(4)) ~~> (_ => List(AssemblyLine.implied(TYA))),
        (Elidable & HasOpcode(LDX) & RefersTo("__reg", zpregIndex) &
          MatchZpReg(4, zpregIndex) & MatchA(4)) ~~> (_ => List(AssemblyLine.implied(TAX))),
        (Elidable & HasOpcode(LDY) & RefersTo("__reg", zpregIndex) &
          MatchZpReg(4, zpregIndex) & MatchA(4)) ~~> (_ => List(AssemblyLine.implied(TAY))),
        (Elidable & HasOpcode(LAX) & RefersTo("__reg", zpregIndex) &
          MatchZpReg(4, zpregIndex) & MatchX(4)) ~~> (_ => List(AssemblyLine.implied(TXA))),
        (Elidable & HasOpcode(LAX) & RefersTo("__reg", zpregIndex) &
          MatchZpReg(4, zpregIndex) & MatchA(4)) ~~> (_ => List(AssemblyLine.implied(TAX))),

        (Elidable & HasOpcode(ADC) & RefersTo("__reg", zpregIndex) &
          MatchZpReg(5, zpregIndex) & MatchA(4) & HasClear(State.D) & HasClear(State.C) &
          DoesntMatterWhatItDoesWith(State.C, State.V)) ~~> { (code, ctx) =>
          val sum = ctx.get[Int](4) + ctx.get[Int](5)
          List(AssemblyLine.immediate(LDA, sum & 0xff))
        },

        (Elidable & HasOpcode(SBC) & RefersTo("__reg", zpregIndex) &
          MatchZpReg(5, zpregIndex) & MatchA(4) & HasClear(State.D) & HasSet(State.C) &
          DoesntMatterWhatItDoesWith(State.C, State.V)) ~~> { (code, ctx) =>
          val sum = ctx.get[Int](4) - ctx.get[Int](5)
          List(AssemblyLine.immediate(LDA, sum & 0xff))
        },

        (Elidable & HasOpcode(AND) & RefersTo("__reg", zpregIndex) &
          MatchZpReg(5, zpregIndex) & MatchA(4)) ~~> { (code, ctx) =>
          val sum = ctx.get[Int](4) & ctx.get[Int](5)
          List(AssemblyLine.immediate(LDA, sum & 0xff))
        },

        (Elidable & HasOpcode(ORA) & RefersTo("__reg", zpregIndex) &
          MatchZpReg(5, zpregIndex) & MatchA(4)) ~~> { (code, ctx) =>
          val sum = ctx.get[Int](4) | ctx.get[Int](5)
          List(AssemblyLine.immediate(LDA, sum & 0xff))
        },

        (Elidable & HasOpcode(EOR) & RefersTo("__reg", zpregIndex) &
          MatchZpReg(5, zpregIndex) & MatchA(4)) ~~> { (code, ctx) =>
          val sum = ctx.get[Int](4) ^ ctx.get[Int](5)
          List(AssemblyLine.immediate(LDA, sum & 0xff))
        }
      )
    })
  )

  val LoadingKnownValue = new RuleBasedAssemblyOptimization("Loading known value from register",
    needsFlowInfo = FlowInfoRequirement.ForwardFlow,
    MultipleAssemblyRules((0 to 4).map{ zregIndex =>
      (Elidable & HasOpcodeIn(LDA, ADC, SBC, CMP, EOR, AND, ORA, LDX, LDY, CPX, CPY) & RefersTo("__reg", zregIndex) & MatchZpReg(1, zregIndex)) ~~> { (code, ctx) =>
        List(AssemblyLine.immediate(code.head.opcode, ctx.get[Int](1)))
      }
    })
  )

  private val simplifiableIndexingFiller: AssemblyPattern =
    (Elidable & HasOpcode(LDY) & HasImmediate(0) & DoesntMatterWhatItDoesWith(State.N, State.Z) | Linear & Not(ConcernsY) & Not(RefersToOrUses("__reg"))).*

  private val finalIndexingOperation: AssemblyLinePattern =
    HasY(0) & HasAddrMode(IndexedY) & RefersToOrUses("__reg", 0) & DoesntMatterWhatItDoesWith(State.Y) & DoesntMatterWhatItDoesWithReg(0) & DoesntMatterWhatItDoesWithReg(1)

  val SimplifiablePointerIndexing = new RuleBasedAssemblyOptimization("Simplifiable pointer indexing",
    needsFlowInfo = FlowInfoRequirement.BothFlows,

    (Elidable & HasOpcode(ADC) & HasClear(State.D) & HasClear(State.C) & HasAddrModeIn(Absolute, ZeroPage, Immediate) & Not(RefersToOrUses("__reg"))) ~
      (Elidable & HasOpcode(STA) & HasAddrModeIn(Absolute, ZeroPage) & RefersTo("__reg", 0)) ~
      (Elidable & HasOpcode(BCC) & MatchParameter(1)) ~
      (Elidable & HasOpcode(INC) & HasAddrModeIn(Absolute, ZeroPage) & RefersTo("__reg", 1)) ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(1) & IsNotALabelUsedManyTimes & DoesntMatterWhatItDoesWith(State.A, State.V, State.C, State.N, State.Z)) ~
      (simplifiableIndexingFiller ~ finalIndexingOperation).capture(2) ~~> { (code, ctx) =>
      code(1) :: code.head.copy(opcode = LDY) :: ctx.get[List[AssemblyLine]](2).filter(l => l.opcode != LDY)
    },

    (Elidable & HasOpcode(ADC) & HasClear(State.D) & HasClear(State.C) & HasAddrModeIn(Absolute, ZeroPage) & RefersTo("__reg", 0)) ~
      (Elidable & HasOpcode(STA) & HasAddrModeIn(Absolute, ZeroPage) & RefersTo("__reg", 0)) ~
      (Elidable & HasOpcode(BCC) & MatchParameter(1)) ~
      (Elidable & HasOpcode(INC) & HasAddrModeIn(Absolute, ZeroPage) & RefersTo("__reg", 1)) ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(1) & IsNotALabelUsedManyTimes & DoesntMatterWhatItDoesWith(State.A, State.V, State.C, State.N, State.Z)) ~
      (simplifiableIndexingFiller ~ finalIndexingOperation).capture(2) ~~> { (code, ctx) =>
      AssemblyLine.implied(TAY).pos(code.head.source) :: ctx.get[List[AssemblyLine]](2).filter(l => l.opcode != LDY)
    },

    (Elidable & HasOpcode(ADC) & HasClear(State.D) & HasClear(State.C) & HasAddrModeIn(Absolute, ZeroPage, Immediate) & Not(RefersToOrUses("__reg"))) ~
      (Elidable & HasOpcode(STA) & HasAddrModeIn(Absolute, ZeroPage) & RefersTo("__reg", 0)) ~
      (Elidable & HasOpcode(TXA)) ~
      (Elidable & HasOpcode(ADC) & HasImmediate(0)) ~
      (Elidable & HasOpcode(STA) & HasAddrModeIn(Absolute, ZeroPage) & RefersTo("__reg", 1) & DoesntMatterWhatItDoesWith(State.A, State.V, State.C, State.N, State.Z)) ~
      (simplifiableIndexingFiller ~ finalIndexingOperation).capture(2) ~~> { (code, ctx) =>
      code(1) :: code(4).copy(opcode = STX) :: code.head.copy(opcode = LDY) :: ctx.get[List[AssemblyLine]](2).filter(l => l.opcode != LDY)
    },

    (Elidable & HasOpcode(ADC) & HasClear(State.D) & HasClear(State.C) & HasAddrModeIn(Absolute, ZeroPage, Immediate) & Not(RefersToOrUses("__reg"))) ~
      (Elidable & HasOpcode(STA) & HasAddrModeIn(Absolute, ZeroPage) & RefersTo("__reg", 0)) ~
      (Elidable & HasOpcode(LDA) & HasAddrModeIn(Absolute, ZeroPage, Immediate) & Not(RefersTo("__reg"))) ~
      (Elidable & HasOpcode(ADC) & HasImmediate(0)) ~
      (Elidable & HasOpcode(STA) & HasAddrModeIn(Absolute, ZeroPage) & RefersTo("__reg", 1) & DoesntMatterWhatItDoesWith(State.A, State.V, State.C, State.N, State.Z)) ~
      (simplifiableIndexingFiller ~ finalIndexingOperation).capture(2) ~~> { (code, ctx) =>
      code(1) :: code(2) :: code(4) :: code.head.copy(opcode = LDY) :: ctx.get[List[AssemblyLine]](2).filter(l => l.opcode != LDY)
    },

  )

  val SimplifiableAddingOfOneBit = new RuleBasedAssemblyOptimization("Simplifiable adding of one bit",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    (Elidable & HasOpcode(AND) & HasImmediate(1)) ~
      (Elidable & HasOpcode(STA) & RefersTo("__reg", 0) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.A, State.N, State.Z, State.C)) ~
      (Linear & Not(ConcernsC) & DoesNotConcernMemoryAt(0, 1)).*.capture(5) ~
      (Elidable & HasOpcode(ASL) & HasAddrMode(Implied)) ~
      (Elidable & HasOpcode(CLC)) ~
      (Elidable & HasClear(State.D) & HasOpcode(ADC) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.V)) ~~> { (code, ctx) =>
      AssemblyLine.implied(ROR) :: (ctx.get[List[AssemblyLine]](5) :+ AssemblyLine.implied(ROL))
    },
    (Elidable & HasOpcode(ASL) & HasAddrMode(Implied)) ~
      (Elidable & HasOpcode(STA) & RefersTo("__reg", 0) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.A, State.N, State.Z, State.C)) ~
      (Linear & Not(HasOpcode(AND)) & Not(ConcernsC) & DoesNotConcernMemoryAt(0, 1)).*.capture(5) ~
      (Elidable & HasOpcode(AND) & HasImmediate(1)) ~
      (Elidable & HasOpcode(CLC)) ~
      (Elidable & HasClear(State.D) & HasOpcode(ADC) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.V)) ~~> { (code, ctx) =>
      code(1) :: (ctx.get[List[AssemblyLine]](5) ++ List(AssemblyLine.implied(ROR), code(1).copy(opcode = LDA), AssemblyLine.implied(ROL)))
    },
    (Elidable & HasOpcode(ASL) & HasAddrMode(Implied)) ~
      (Elidable & HasOpcode(STA) & RefersTo("__reg", 0) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.A, State.N, State.Z, State.C)) ~
      (Linear & Not(HasOpcode(ANC)) & Not(ConcernsC) & DoesNotConcernMemoryAt(0, 1)).*.capture(5) ~
      (Elidable & HasOpcode(ANC) & HasImmediate(1)) ~
      (Elidable & HasClear(State.D) & HasOpcode(ADC) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.V)) ~~> { (code, ctx) =>
      code(1) :: (ctx.get[List[AssemblyLine]](5) ++ List(AssemblyLine.implied(ROR), code(1).copy(opcode = LDA), AssemblyLine.implied(ROL)))
    },
  )

  val All: List[AssemblyOptimization[AssemblyLine]] = List(
    ConstantDecimalMath,
    ConstantDivision,
    ConstantMultiplication,
    ConstantInlinedMultiplication,
    LoadingKnownValue,
    DeadRegStore,
    DeadRegStoreFromFlow,
    PointlessLoad,
    SimplifiableAddingOfOneBit,
    SimplifiablePointerIndexing,
    StashInRegInsteadOfStack,
  )

}
