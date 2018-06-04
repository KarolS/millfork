package millfork.assembly.opt

import java.util.concurrent.atomic.AtomicInteger

import millfork.assembly._
import millfork.assembly.Opcode._
import millfork.assembly.AddrMode._
import millfork.assembly.OpcodeClasses._
import millfork.env.{Constant, NormalFunction, NumericConstant}

/**
  * @author Karol Stasiak
  */
object UndocumentedOptimizations {

  val counter = new AtomicInteger(30000)

  def getNextLabel(prefix: String) = f".$prefix%s__${counter.getAndIncrement()}%05d"

  private val LdxAddrModes = Set(ZeroPage, Absolute, Immediate, AbsoluteY, ZeroPageY)
  private val LaxAddrModeRestriction = Not(HasAddrModeIn(Set(AbsoluteX, ZeroPageX, IndexedX, Immediate)))
  private val SaxAddrModeRestriction = HasAddrModeIn(Set(IndexedX, ZeroPage, Absolute, AbsoluteY))

  //noinspection ScalaUnnecessaryParentheses
  val UseLax = new RuleBasedAssemblyOptimization("Using undocumented instruction LAX",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    (HasOpcode(LDA) & Elidable & MatchAddrMode(0) & MatchParameter(1) & LaxAddrModeRestriction) ~
      (LinearOrLabel & Not(ConcernsA) & Not(ChangesMemory) & Not(HasOpcode(LDX)) & DoesntChangeIndexingInAddrMode(0)).*.capture(2) ~
      (HasOpcode(LDX) & Elidable & MatchAddrMode(0) & MatchParameter(1)) ~~> { (code, ctx) =>
      ctx.get[List[AssemblyLine]](2) :+ code.head.copy(opcode = LAX)
    },
    (HasOpcode(LDX) & Elidable & MatchAddrMode(0) & MatchParameter(1) & LaxAddrModeRestriction) ~
      (LinearOrLabel & Not(ConcernsX) & Not(ChangesMemory) & Not(HasOpcode(LDA)) & DoesntChangeIndexingInAddrMode(0)).*.capture(2) ~
      (HasOpcode(LDA) & Elidable & MatchAddrMode(0) & MatchParameter(1)) ~~> { (code, ctx) =>
      ctx.get[List[AssemblyLine]](2) :+ code.head.copy(opcode = LAX)
    },

    (HasOpcode(LDA) & Elidable & MatchAddrMode(0) & LaxAddrModeRestriction) ~
      (LinearOrLabel & Not(ConcernsA) & Not(ChangesMemory) & Not(HasOpcode(TAX)) & DoesntChangeIndexingInAddrMode(0)).*.capture(2) ~
      (HasOpcode(TAX) & Elidable) ~~> { (code, ctx) =>
      ctx.get[List[AssemblyLine]](2) :+ code.head.copy(opcode = LAX)
    },
    (HasOpcode(LDX) & Elidable & MatchAddrMode(0) & LaxAddrModeRestriction) ~
      (LinearOrLabel & Not(ConcernsX) & Not(ChangesMemory) & Not(HasOpcode(TXA)) & DoesntChangeIndexingInAddrMode(0)).*.capture(2) ~
      (HasOpcode(TXA) & Elidable) ~~> { (code, ctx) =>
      ctx.get[List[AssemblyLine]](2) :+ code.head.copy(opcode = LAX)
    },

    (HasOpcode(LDA) & Elidable & MatchAddrMode(0) & MatchParameter(1) & LaxAddrModeRestriction) ~
      (LinearOrLabel & Not(ConcernsX) & Not(ChangesA) & Not(ChangesMemory) & Not(HasOpcode(LDX)) & DoesntChangeIndexingInAddrMode(0)).*.capture(2) ~
      (HasOpcode(LDX) & Elidable & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> { (code, ctx) =>
      code.head.copy(opcode = LAX) :: ctx.get[List[AssemblyLine]](2)
    },
    (HasOpcode(LDX) & Elidable & MatchAddrMode(0) & MatchParameter(1) & LaxAddrModeRestriction) ~
      (LinearOrLabel & Not(ConcernsA) & Not(ChangesX) & Not(ChangesMemory) & Not(HasOpcode(LDA)) & DoesntChangeIndexingInAddrMode(0)).*.capture(2) ~
      (HasOpcode(LDA) & Elidable & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> { (code, ctx) =>
      code.head.copy(opcode = LAX) :: ctx.get[List[AssemblyLine]](2)
    },

    (HasOpcode(LDA) & Elidable & MatchAddrMode(0) & LaxAddrModeRestriction) ~
      (LinearOrLabel & Not(ConcernsX) & Not(ChangesA) & Not(ChangesMemory) & Not(HasOpcode(TAX)) & DoesntChangeIndexingInAddrMode(0)).*.capture(2) ~
      (HasOpcode(TAX) & Elidable & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> { (code, ctx) =>
      code.head.copy(opcode = LAX) :: ctx.get[List[AssemblyLine]](2)
    },
    (HasOpcode(LDX) & Elidable & MatchAddrMode(0) & LaxAddrModeRestriction) ~
      (LinearOrLabel & Not(ConcernsA) & Not(ChangesX) & Not(ChangesMemory) & Not(HasOpcode(TXA)) & DoesntChangeIndexingInAddrMode(0)).*.capture(2) ~
      (HasOpcode(TXA) & Elidable & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> { (code, ctx) =>
      code.head.copy(opcode = LAX) :: ctx.get[List[AssemblyLine]](2)
    },
  )

  val SaxModes: Set[AddrMode.Value] = Set(ZeroPage, IndexedX, ZeroPageY, Absolute)

  val UseSax = new RuleBasedAssemblyOptimization("Using undocumented instruction SAX",
    needsFlowInfo = FlowInfoRequirement.BackwardFlow,
    (HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.X)) ~
      (Linear & Not(ConcernsA) & Not(ConcernsX) & DoesntChangeMemoryAt(0, 1)).*.capture(10) ~
      (HasOpcode(AND) & Elidable & MatchAddrMode(2) & MatchParameter(3) & Not(ReadsX)) ~
      (Linear & Not(ConcernsA) & Not(ConcernsX) & DoesntChangeMemoryAt(0, 1)).*.capture(11) ~
      (HasOpcode(STA) & Elidable & MatchAddrMode(4) & MatchParameter(5) & HasAddrModeIn(SaxModes) & DoesntChangeMemoryAt(0, 1)) ~
      (Linear & Not(ConcernsA) & Not(ConcernsX) & DoesntChangeMemoryAt(0, 1)).*.capture(12) ~
      (HasOpcode(LDA) & Elidable & MatchAddrMode(0) & MatchParameter(1)) ~~> { (code, ctx) =>
      val lda = code.head
      val ldx = AssemblyLine(LDX, ctx.get[AddrMode.Value](2), ctx.get[Constant](3))
      val sax = AssemblyLine(SAX, ctx.get[AddrMode.Value](4), ctx.get[Constant](5))
      val fragment0 = lda :: ctx.get[List[AssemblyLine]](10)
      val fragment1 = ldx :: ctx.get[List[AssemblyLine]](11)
      val fragment2 = sax :: ctx.get[List[AssemblyLine]](12)
      List(fragment0, fragment1, fragment2).flatten
    },
  )

  def andConstant(const: Constant, mask: Int): Option[Long] = const match {
    case NumericConstant(n, _) => Some(n & mask)
    case _ => None
  }

  val UseAnc = new RuleBasedAssemblyOptimization("Using undocumented instruction ANC",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    (Elidable & HasOpcode(LDA) & HasImmediate(0)) ~
      (Elidable & HasOpcode(CLC)) ~~> (_ => List(AssemblyLine.immediate(ANC, 0))),
    (Elidable & HasOpcode(LDA) & HasImmediate(0) & HasClear(State.C)) ~~> (_ => List(AssemblyLine.immediate(ANC, 0))),
    (Elidable & HasOpcode(AND) & MatchImmediate(0)) ~
      Where(c => andConstant(c.get[Constant](0), 0x80).contains(0)) ~
      (Elidable & HasOpcode(CLC)) ~~> ((_, ctx) => List(AssemblyLine.immediate(ANC, ctx.get[Constant](0)))),
    (Elidable & HasOpcode(AND) & MatchImmediate(0)) ~
      Where(c => andConstant(c.get[Constant](0), 0x80).contains(0x80)) ~
      (Elidable & HasOpcode(SEC)) ~~> ((_, ctx) => List(AssemblyLine.immediate(ANC, ctx.get[Constant](0)))),
    (Elidable & HasOpcode(AND) & MatchImmediate(0)) ~
      (Elidable & HasOpcode(CMP) & HasImmediate(0x80) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~~> ((_, ctx) => List(AssemblyLine.immediate(ANC, ctx.get[Constant](0)))),
    (Elidable & HasOpcode(AND) & MatchImmediate(0)) ~
      (Elidable & HasOpcode(CMP) & HasImmediate(0x80) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~~> ((_, ctx) => List(AssemblyLine.immediate(ANC, ctx.get[Constant](0)))),
    (Elidable & HasOpcode(AND) & MatchImmediate(0) & HasClear(State.C)) ~
      Where(c => andConstant(c.get[Constant](0), 0x80).contains(0)) ~~> ((_, ctx) => List(AssemblyLine.immediate(ANC, ctx.get[Constant](0)))),
    (Elidable & HasOpcode(AND) & MatchImmediate(0) & HasSet(State.C)) ~
      Where(c => andConstant(c.get[Constant](0), 0x80).contains(0)) ~~> ((_, ctx) => List(AssemblyLine.immediate(ANC, ctx.get[Constant](0)))),
    (Elidable & HasOpcode(AND) & MatchImmediate(0)) ~
      (Elidable & HasOpcodeIn(Set(ROL, ASL)) & HasAddrMode(Implied) & DoesntMatterWhatItDoesWith(State.Z, State.N, State.A)) ~~> ((_, ctx) => List(AssemblyLine.immediate(ANC, ctx.get[Constant](0)))),
  )

  val UseSbx = new RuleBasedAssemblyOptimization("Using undocumented instruction SBX",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    (Elidable & HasOpcode(DEX) & DoesntMatterWhatItDoesWith(State.A)).+.captureLength(0) ~
      Where(_.get[Int](0) > 2) ~~> ((_, ctx) => List(
      AssemblyLine.implied(TXA),
      AssemblyLine.immediate(SBX, ctx.get[Constant](0)),
    )),
    (Elidable & HasOpcode(INX) & DoesntMatterWhatItDoesWith(State.A)).+.captureLength(0) ~
      Where(_.get[Int](0) > 2) ~~> ((_, ctx) => List(
      AssemblyLine.implied(TXA),
      AssemblyLine.immediate(SBX, Constant.Zero - ctx.get[Constant](0)),
    )),
    HasOpcode(TXA) ~
      (Elidable & HasOpcode(CLC)).? ~
      (Elidable & HasClear(State.C) & HasClear(State.D) & HasOpcode(ADC) & MatchImmediate(0)) ~
      (Elidable & HasOpcode(TAX) & DoesntMatterWhatItDoesWith(State.C, State.A)) ~~> ((code, ctx) => List(
      code.head,
      AssemblyLine.immediate(SBX, Constant.Zero - ctx.get[Constant](0)),
    )),
    HasOpcode(TXA) ~
      (Elidable & HasOpcode(SEC)).? ~
      (Elidable & HasSet(State.C) &  HasClear(State.D) & HasOpcode(SBC) & MatchImmediate(0)) ~
      (Elidable & HasOpcode(TAX) & DoesntMatterWhatItDoesWith(State.C, State.A)) ~~> ((code, ctx) => List(
      code.head,
      AssemblyLine.immediate(SBX, ctx.get[Constant](0)),
    )),
    (HasOpcodeIn(Set(LDA, TYA)) & MatchAddrMode(0) & MatchParameter(1)) ~
    (Elidable & HasOpcode(AND)) ~
    (Elidable & HasOpcode(TAX)) ~
    (Elidable & HasOpcodeIn(Set(LDA, TYA)) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.N, State.Z)) ~~> {code =>
      List(code.head, code(1).copy(opcode = LDX), AssemblyLine.immediate(SBX, 0))
    }
  )


  val UseAlr = new RuleBasedAssemblyOptimization("Using undocumented instruction ALR",
    needsFlowInfo = FlowInfoRequirement.NoRequirement,
    (Elidable & HasOpcode(AND) & HasAddrMode(Immediate)) ~
      (Elidable & HasOpcode(LSR) & HasAddrMode(Implied)) ~~> { code =>
      List(AssemblyLine.immediate(ALR, code.head.parameter))
    },
    (Elidable & HasOpcode(LSR) & HasAddrMode(Implied)) ~
      (Elidable & HasOpcode(AND) & HasAddrMode(Immediate)) ~~> { code =>
      List(AssemblyLine.immediate(ALR, code.last.parameter.asl(1).loByte))
    },
    (Elidable & HasOpcode(LSR) & HasAddrMode(Implied)) ~
      (Elidable & HasOpcode(CLC)) ~~> { _ =>
      List(AssemblyLine.immediate(ALR, 0xFE))
    },
  )

  val UseArr = new RuleBasedAssemblyOptimization("Using undocumented instruction ARR",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    (HasClear(State.D) & Elidable & HasOpcode(AND) & HasAddrMode(Immediate)) ~
      (Elidable & HasOpcode(ROR) & HasAddrMode(Implied) & DoesntMatterWhatItDoesWith(State.C, State.V)) ~~> { code =>
      List(AssemblyLine.immediate(ARR, code.head.parameter))
    },
  )

  private def extraRmw(legal: Opcode.Value, illegal: Opcode.Value) =
    (Elidable & HasOpcode(LDA) & HasAddrModeIn(Set(IndexedX, IndexedY, AbsoluteY, IndexedY)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(legal) & HasAddrMode(Implied)) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.A, State.N, State.Z, State.C, State.V)) ~~> { (code, ctx) =>
      code.head.copy(opcode = illegal) :: Nil
    }

  private def trivialSequence1(o1: Opcode.Value, o2: Opcode.Value, extra: AssemblyLinePattern, combined: Opcode.Value) =
    (Elidable & HasOpcode(o1) & HasAddrModeIn(Set(ZeroPage, Absolute)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & DoesNotConcernMemoryAt(0, 1) & extra).* ~
      (Elidable & HasOpcode(o2) & HasAddrModeIn(Set(ZeroPage, Absolute)) & MatchParameter(1)) ~~> { (code, ctx) =>
      code.tail.init :+ AssemblyLine(combined, Absolute, ctx.get[Constant](1))
    }

  private def trivialSequence2(o1: Opcode.Value, o2: Opcode.Value, extra: AssemblyLinePattern, combined: Opcode.Value) =
    (Elidable & HasOpcode(o1) & Not(HasAddrMode(Immediate)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & DoesNotConcernMemoryAt(0, 1) & extra).* ~
      (Elidable & HasOpcode(o2) & MatchAddrMode(0) & MatchParameter(1)) ~~> { (code, ctx) =>
      code.tail.init :+ AssemblyLine(combined, ctx.get[AddrMode.Value](0), ctx.get[Constant](1))
    }

  // ROL c LDA c AND d => LDA d RLA c
  private def trivialCommutativeSequence(o1: Opcode.Value, o2: Opcode.Value, combined: Opcode.Value) = {
    (Elidable & HasOpcode(o1) & Not(HasAddrMode(Immediate)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(LDA) & Not(HasAddrMode(Immediate)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(o2) & MatchAddrMode(2) & MatchParameter(3)) ~~> { code =>
      List(code(2).copy(opcode = LDA), code(1).copy(opcode = combined))
    }
  }

  val UseSlo = new RuleBasedAssemblyOptimization("Using undocumented instruction SLO",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    trivialSequence1(ASL, ORA, Not(ConcernsC), SLO),
    trivialSequence2(ASL, ORA, Not(ConcernsC), SLO),
    trivialCommutativeSequence(ASL, ORA, SLO),
    extraRmw(ASL, SLO),
    (Elidable & HasOpcode(ASL) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.C, State.Z, State.N)) ~
      (Linear & Not(ConcernsMemory)).* ~
      (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~~> { (code, ctx) =>
      code.tail.init ++ List(AssemblyLine.immediate(LDA, 0), AssemblyLine(SLO, ctx.get[AddrMode.Value](0), ctx.get[Constant](1)))
    },
    (Elidable & HasOpcode(ASL) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~~> { (code, ctx) =>
      List(AssemblyLine.immediate(LDA, 0), AssemblyLine(SLO, ctx.get[AddrMode.Value](0), ctx.get[Constant](1)))
    },
    (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.C, State.Z, State.N)) ~
      (Linear & Not(ConcernsMemory) & Not(ConcernsA) & Not(ChangesC)).*.capture(2) ~
      (Elidable & HasOpcode(ASL) & HasAddrMode(Implied) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~
      (Linear & Not(ConcernsMemory) & Not(ChangesA)).*.capture(3) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1)) ~~> { (_, ctx) =>
      List(AssemblyLine.immediate(LDA, 0), AssemblyLine(SLO, ctx.get[AddrMode.Value](0), ctx.get[Constant](1))) ++
        ctx.get[List[AssemblyLine]](2) ++
        ctx.get[List[AssemblyLine]](3)
    },
    (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.C, State.Z, State.N)) ~
      (Linear & Not(ConcernsMemory) & Not(ConcernsA)).*.capture(2) ~
      (Elidable & HasOpcode(ASL) & HasAddrMode(Implied) & DoesntMatterWhatItDoesWith(State.C, State.Z, State.N)) ~
      (Linear & Not(ConcernsMemory) & Not(ChangesA)).*.capture(3) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1)) ~~> { (_, ctx) =>
      List(AssemblyLine.immediate(LDA, 0), AssemblyLine(SLO, ctx.get[AddrMode.Value](0), ctx.get[Constant](1))) ++
        ctx.get[List[AssemblyLine]](2) ++
        ctx.get[List[AssemblyLine]](3)
    },
    (Elidable & HasA(0) & HasOpcode(ASL) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Linear & DoesntChangeMemoryAt(0, 1) & Not(ChangesA)).* ~
      (Elidable & HasA(0) & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.Z, State.N, State.C)) ~~> { code =>
      code.head.copy(opcode = SLO) :: code.tail.init
    },
  )

  val UseSre = new RuleBasedAssemblyOptimization("Using undocumented instruction SRE",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    trivialSequence1(LSR, EOR, Not(ConcernsC), SRE),
    trivialSequence2(LSR, EOR, Not(ConcernsC), SRE),
    trivialCommutativeSequence(LSR, EOR, SRE),
    extraRmw(LSR, SRE),
    (Elidable & HasOpcode(LSR) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.C, State.Z, State.N)) ~
      (Linear & Not(ConcernsMemory)).* ~
      (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~~> { (code, ctx) =>
      code.tail.init ++ List(AssemblyLine.immediate(LDA, 0), AssemblyLine(SRE, ctx.get[AddrMode.Value](0), ctx.get[Constant](1)))
    },
    (Elidable & HasOpcode(LSR) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~~> { (code, ctx) =>
      List(AssemblyLine.immediate(LDA, 0), AssemblyLine(SRE, ctx.get[AddrMode.Value](0), ctx.get[Constant](1)))
    },
    (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.C, State.Z, State.N)) ~
      (Linear & Not(ConcernsMemory) & Not(ConcernsA) & Not(ChangesC)).*.capture(2) ~
      (Elidable & HasOpcode(LSR) & HasAddrMode(Implied) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~
      (Linear & Not(ConcernsMemory) & Not(ChangesA)).*.capture(3) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1)) ~~> { (_, ctx) =>
      List(AssemblyLine.immediate(LDA, 0), AssemblyLine(SRE, ctx.get[AddrMode.Value](0), ctx.get[Constant](1))) ++
        ctx.get[List[AssemblyLine]](2) ++
        ctx.get[List[AssemblyLine]](3)
    },
    (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.C, State.Z, State.N)) ~
      (Linear & Not(ConcernsMemory) & Not(ConcernsA)).*.capture(2) ~
      (Elidable & HasOpcode(LSR) & HasAddrMode(Implied) & DoesntMatterWhatItDoesWith(State.C, State.Z, State.N)) ~
      (Linear & Not(ConcernsMemory) & Not(ChangesA)).*.capture(3) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1)) ~~> { (_, ctx) =>
      List(AssemblyLine.immediate(LDA, 0), AssemblyLine(SRE, ctx.get[AddrMode.Value](0), ctx.get[Constant](1))) ++
        ctx.get[List[AssemblyLine]](2) ++
        ctx.get[List[AssemblyLine]](3)
    },
  )

  val UseRla = new RuleBasedAssemblyOptimization("Using undocumented instruction RLA",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    trivialSequence1(ROL, AND, Not(ConcernsC), RLA),
    trivialSequence2(ROL, AND, Not(ConcernsC), RLA),
    trivialCommutativeSequence(ROL, AND, RLA),
    extraRmw(ROL, RLA),
    (Elidable & HasOpcode(ROL) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.C, State.Z, State.N)) ~
      (Linear & Not(ConcernsMemory)).* ~
      (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~~> { (code, ctx) =>
      code.tail.init ++ List(AssemblyLine.immediate(LDA, 0xff), AssemblyLine(RLA, ctx.get[AddrMode.Value](0), ctx.get[Constant](1)))
    },
    (Elidable & HasOpcode(ROL) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~~> { (code, ctx) =>
      List(AssemblyLine.immediate(LDA, 0xff), AssemblyLine(RLA, ctx.get[AddrMode.Value](0), ctx.get[Constant](1)))
    },
    (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~
      (Linear & Not(ConcernsMemory) & Not(ConcernsA) & Not(ConcernsC)).*.capture(2) ~
      (Elidable & HasOpcode(ROL) & HasAddrMode(Implied) & DoesntMatterWhatItDoesWith(State.C, State.Z, State.N)) ~
      (Linear & Not(ConcernsMemory) & Not(ChangesA)).*.capture(3) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1)) ~~> { (_, ctx) =>
      List(AssemblyLine.immediate(LDA, 0xff), AssemblyLine(RLA, ctx.get[AddrMode.Value](0), ctx.get[Constant](1))) ++
        ctx.get[List[AssemblyLine]](2) ++
        ctx.get[List[AssemblyLine]](3)
    },
    (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~
      (Linear & Not(ConcernsMemory) & Not(ConcernsA) & Not(ConcernsC)).*.capture(2) ~
      (Elidable & HasOpcode(ROL) & HasAddrMode(Implied) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~
      (Linear & Not(ConcernsMemory) & Not(ChangesA)).*.capture(3) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1)) ~~> { (_, ctx) =>
      List(AssemblyLine.immediate(LDA, 0xff), AssemblyLine(RLA, ctx.get[AddrMode.Value](0), ctx.get[Constant](1))) ++
        ctx.get[List[AssemblyLine]](2) ++
        ctx.get[List[AssemblyLine]](3)
    },
  )

  val UseRra = new RuleBasedAssemblyOptimization("Using undocumented instruction RRA",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    // TODO: is it ok? carry flag and stuff?
//    trivialSequence1(ROR, ADC, Not(ConcernsC), RRA),
//    trivialSequence2(ROR, ADC, Not(ConcernsC), RRA),
//    trivialCommutativeSequence(ROR, ADC, RRA),
    extraRmw(ROR, RRA),
  )

  val UseDcp = new RuleBasedAssemblyOptimization("Using undocumented instruction DCP",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    trivialSequence1(DEC, CMP, Not(ConcernsC), DCP),
    trivialSequence2(DEC, CMP, Not(ConcernsC), DCP),
    (Elidable & HasOpcode(LDA) & HasAddrModeIn(Set(IndexedY, AbsoluteY, IndexedY)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(SEC)).? ~
      (Elidable & HasOpcode(SBC) & HasImmediate(1) & HasSet(State.C) & HasClear(State.D)).? ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.A, State.N, State.Z, State.C, State.V)) ~~> { (code, ctx) =>
      code.head.copy(opcode = DCP) :: Nil
    },
    (Elidable & HasOpcode(LDA) & HasAddrModeIn(Set(IndexedX, ZeroPageX, AbsoluteX))) ~
      (Elidable & HasOpcode(TAX)) ~
      (Elidable & HasOpcode(DEC) & HasAddrMode(AbsoluteX) & DoesntMatterWhatItDoesWith(State.A, State.Y, State.X, State.C, State.Z, State.N, State.V)) ~~> { code =>
      List(code.head.copy(opcode = LDY), code.last.copy(opcode = DCP, addrMode = AbsoluteY))
    },
    (Elidable & HasOpcode(DEC) & Not(HasAddrMode(Immediate)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(LDA) & Not(HasAddrMode(Immediate)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(CMP) & MatchAddrMode(2) & MatchParameter(3) & DoesntMatterWhatItDoesWith(State.C, State.N, State.A)) ~~> { code =>
      List(code(2).copy(opcode = LDA), code(1).copy(opcode = DCP))
    },
    (Elidable & HasOpcode(DEC) & Not(HasAddrMode(Immediate)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(LDA) & Not(HasAddrMode(Immediate)) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.C, State.N, State.A)) ~~> { code =>
      List(AssemblyLine.immediate(LDA, 0), code(1).copy(opcode = DCP))
    },
    (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1) & HasAddrModeIn(Set(ZeroPage, Absolute))) ~
    (Elidable & HasOpcode(BNE) & MatchParameter(2)) ~
      (Elidable & HasOpcode(DEC) & MatchAddrMode(30) & MatchParameter(31) & DoesntChangeMemoryAt(0, 1)) ~
      (Elidable & HasOpcode(LABEL) & MatchParameter(2) & HasCallerCount(1)) ~
      (Elidable & HasOpcode(DEC) & HasAddrModeIn(Set(ZeroPage, Absolute)) & MatchParameter(1) &
        DoesntChangeMemoryAt(30, 31) &
        DoesntMatterWhatItDoesWith(State.Z, State.C, State.N, State.A)) ~~> { code =>
      List(
        AssemblyLine.immediate(LDA, 0xff),
        code.head.copy(opcode =  DCP),
        code(1), // BNE
        code(2), // DEC ptr+1
        code(3)  // LABEL
      )
    },
  )

  val UseIsc = new RuleBasedAssemblyOptimization("Using undocumented instruction ISC",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    trivialSequence1(INC, SBC, Not(ReadsC), ISC),
    trivialSequence2(INC, SBC, Not(ReadsC), ISC),
    (Elidable & HasOpcode(LDA) & HasAddrModeIn(Set(IndexedY, AbsoluteY, IndexedX)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(CLC)).? ~
      (Elidable & HasOpcode(ADC) & HasImmediate(1) & HasClear(State.C) & HasClear(State.D)).? ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(0) & MatchParameter(1) & DoesntMatterWhatItDoesWith(State.A, State.N, State.Z, State.C, State.V)) ~~> { (code, ctx) =>
      code.head.copy(opcode = ISC) :: Nil
    },
    (Elidable & HasOpcode(LDA) & HasImmediate(0) & HasClear(State.D)) ~
      (Elidable & HasOpcode(ADC) & MatchAddrMode(1) & MatchParameter(2) & HasAddrModeIn(Set(IndexedX, IndexedY, AbsoluteY))) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(1) & MatchParameter(2) & DoesntMatterWhatItDoesWith(State.A, State.C, State.Z, State.N, State.V)) ~~> { code =>
      val label = getNextLabel("is")
      List(
        AssemblyLine.relative(BCC, label),
        code.last.copy(opcode = ISC),
        AssemblyLine.label(label))
    },
    (Elidable & HasOpcode(LDA) & MatchAddrMode(1) & MatchParameter(2) & HasAddrModeIn(Set(IndexedX, IndexedY, AbsoluteY))) ~
      (Elidable & HasOpcode(ADC) & HasImmediate(0) & HasClear(State.D)) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(1) & MatchParameter(2) & DoesntMatterWhatItDoesWith(State.A, State.C, State.Z, State.N, State.V)) ~~> { code =>
      val label = getNextLabel("is")
      List(
        AssemblyLine.relative(BCC, label),
        code.last.copy(opcode = ISC),
        AssemblyLine.label(label))
    },
    (Elidable & HasOpcode(CLC)).? ~
      (Elidable & HasOpcode(LDA) & HasImmediate(1) & HasClear(State.D) & HasClear(State.C)) ~
      (Elidable & HasOpcode(ADC) & MatchAddrMode(1) & MatchParameter(2) & HasAddrModeIn(Set(IndexedX, IndexedY, AbsoluteY))) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(1) & MatchParameter(2) & DoesntMatterWhatItDoesWith(State.A, State.C, State.Z, State.N, State.V)) ~~> { code =>
      List(code.last.copy(opcode = ISC))
    },
    (Elidable & HasOpcode(CLC)).? ~
      (Elidable & HasOpcode(LDA) & MatchAddrMode(1) & HasClear(State.D) & HasClear(State.C) & MatchAddrMode(2) & HasAddrModeIn(Set(IndexedX, IndexedY, AbsoluteY))) ~
      (Elidable & HasOpcode(ADC) & HasImmediate(1)) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(1) & MatchAddrMode(2) & DoesntMatterWhatItDoesWith(State.A, State.C, State.Z, State.N, State.V)) ~~> { code =>
      List(code.last.copy(opcode = ISC))
    },
    (Elidable & HasOpcode(SEC)).? ~
      (Elidable & HasOpcode(LDA) & HasImmediate(0) & HasClear(State.D) & HasSet(State.C)) ~
      (Elidable & HasOpcode(ADC) & MatchAddrMode(1) & MatchParameter(2) & HasAddrModeIn(Set(IndexedX, IndexedY, AbsoluteY))) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(1) & MatchParameter(2) & DoesntMatterWhatItDoesWith(State.A, State.C, State.Z, State.N, State.V)) ~~> { code =>
      List(code.last.copy(opcode = ISC))
    },
    (Elidable & HasOpcode(SEC)).? ~
      (Elidable & HasOpcode(LDA) & MatchAddrMode(1) & HasClear(State.D) & HasSet(State.C) & MatchAddrMode(2) & HasAddrModeIn(Set(IndexedX, IndexedY, AbsoluteY))) ~
      (Elidable & HasOpcode(ADC) & HasImmediate(0)) ~
      (Elidable & HasOpcode(STA) & MatchAddrMode(1) & MatchAddrMode(2) & DoesntMatterWhatItDoesWith(State.A, State.C, State.Z, State.N, State.V)) ~~> { code =>
      List(code.last.copy(opcode = ISC))
    },
    (Elidable & HasOpcode(LDA) & HasAddrModeIn(Set(IndexedX, ZeroPageX, AbsoluteX))) ~
      (Elidable & HasOpcode(TAX)) ~
      (Elidable & HasOpcode(INC) & HasAddrMode(AbsoluteX) & DoesntMatterWhatItDoesWith(State.A, State.Y, State.X, State.C, State.Z, State.N, State.V)) ~~> { code =>
      List(code.head.copy(opcode = LDY), code.last.copy(opcode = ISC, addrMode = AbsoluteY))
    },
    (Elidable & HasOpcode(INC) & Not(HasAddrMode(Immediate)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(LDA) & Not(HasAddrMode(Immediate)) & MatchAddrMode(0) & MatchParameter(1)) ~
      (Elidable & HasOpcode(CMP) & HasClear(State.D) & MatchAddrMode(2) & MatchParameter(3) & DoesntMatterWhatItDoesWith(State.V, State.C, State.N, State.A)) ~~> { code =>
      List(code(2).copy(opcode = LDA), AssemblyLine.implied(SEC), code(1).copy(opcode = ISC))
    }
  )

  val UseMultiple = new RuleBasedAssemblyOptimization("Using multiple undocumented instructions",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    (Elidable & HasOpcode(LDA) & LaxAddrModeRestriction & HasClear(State.D)) ~
      (Elidable & HasOpcode(CLC)).? ~
      (Elidable & HasOpcode(ADC) & HasImmediate(2) & HasClear(State.C)) ~
      (Elidable & HasOpcode(TAX) & DoesntMatterWhatItDoesWith(State.C, State.V)) ~~> { (code, ctx) =>
      List(code.head.copy(opcode = LAX), AssemblyLine(SBX, Immediate, Constant.Zero - ctx.get[Constant](2)))
    },
    (Elidable & HasOpcode(LDA) & LaxAddrModeRestriction & HasClear(State.D)) ~
      (Elidable & HasOpcode(SEC)).? ~
      (Elidable & HasOpcode(SBC) & HasImmediate(2) & HasSet(State.C)) ~
      (Elidable & HasOpcode(TAX) & DoesntMatterWhatItDoesWith(State.C, State.V)) ~~> { (code, ctx) =>
      List(code.head.copy(opcode = LAX), AssemblyLine(SBX, Immediate, ctx.get[Constant](2)))
    },
    (Elidable & HasOpcode(LDA) & LaxAddrModeRestriction & MatchAddrMode(0) & MatchParameter(1)) ~
      (Not(ReadsX) & HasOpcodeIn(Set(ANC, ALR, ARR, ADC, AND, EOR, ORA, ADC, SBC, STA, LDY, STY)) |
        HasAddrMode(Implied) & HasOpcodeIn(Set(ASL, LSR, ROL, ROR, TAY, TYA, SEC, CLC, SED, CLD))).* ~
      (Elidable & HasOpcode(LDA) & MatchAddrMode(0) & MatchParameter(1)) ~
      HasOpcode(LDY).? ~
      (Elidable & HasOpcode(AND)) ~
      HasOpcode(LDY).? ~
      (Elidable & HasOpcode(STA) & SaxAddrModeRestriction & DoesntMatterWhatItDoesWith(State.X)) ~
      (Elidable & (HasOpcode(TAX) | HasOpcodeIn(Set(LDA, LDX, LAX)) & MatchAddrMode(0) & MatchParameter(1))).? ~~> { (code, ctx) =>
      var rest = code
      var result = List[AssemblyLine]()
      rest.last.opcode match {
        case STA => ()
        case TAX | LDX => rest = rest.init
        case LDA | LAX =>
          rest = rest.init
          result = List(AssemblyLine.implied(TXA))
      }
      result = rest.last.copy(opcode = SAX) :: result
      rest = rest.init
      rest.last.opcode match {
        case LDY =>
          result = rest.last :: result
          rest = rest.init
        case AND => ()
      }
      result = rest.last.copy(opcode = LDA) :: result
      rest = rest.init
      rest.last.opcode match {
        case LDY =>
          result = rest.last :: result
          rest = rest.init
        case LDA => ()
      }
      rest = rest.init
      rest.head.copy(opcode = LAX) :: (rest.tail ++ result)
    },
  )

  private def idempotent(illegal: Opcode.Value, pointless: Opcode.Value) =
    (HasOpcode(illegal) & MatchAddrMode(0) & MatchParameter(1)) ~
    (Elidable & HasOpcode(pointless) & MatchAddrMode(0) & MatchParameter(1)) ~~> { (code, ctx) =>
      code.take(1)
    }

  private def preferLegal(illegal: Opcode.Value, legal: Opcode.Value) =
    (Elidable & HasOpcode(illegal) & HasAddrModeIn(Set(Absolute, AbsoluteX, ZeroPage, ZeroPageX)) & DoesntMatterWhatItDoesWith(State.A, State.N, State.Z, State.C, State.V))  ~~> { (code, ctx) =>
      code.map(_.copy(opcode = legal))
    }

  val CleaningUp = new RuleBasedAssemblyOptimization("Simplifying code that uses undocumented instructions",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    idempotent(SLO, ORA),
    idempotent(SRE, EOR),
    idempotent(ROL, AND),
    idempotent(DCP, CMP),
    preferLegal(SLO, ASL),
    preferLegal(SRE, LSR),
    preferLegal(RLA, ROL),
    preferLegal(RRA, ROR),
    preferLegal(DCP, DEC),
    preferLegal(ISC, INC),
    HasOpcodeIn(Set(TAX, TXA, LAX)) ~ (Elidable & HasOpcode(SBX) & HasImmediate(0) & DoesntMatterWhatItDoesWith(State.C, State.N, State.Z)) ~~> (code => List(code.head)),
    HasOpcodeIn(Set(TAX, TXA, LAX)) ~ (Elidable & HasOpcode(SBX) & HasImmediate(1) & DoesntMatterWhatItDoesWith(State.C)) ~~> (code => List(code.head, AssemblyLine.implied(DEX))),
    HasOpcodeIn(Set(TAX, TXA, LAX)) ~ (Elidable & HasOpcode(SBX) & HasImmediate(0xff) & DoesntMatterWhatItDoesWith(State.C)) ~~> (code => List(code.head, AssemblyLine.implied(INX))),
    (Elidable & HasOpcode(LAX) & DoesntMatterWhatItDoesWith(State.X)) ~~> (_.map(_.copy(opcode = LDA))),
    (Elidable & HasOpcode(LAX) & HasAddrModeIn(LdxAddrModes) & DoesntMatterWhatItDoesWith(State.A)) ~~> (_.map(_.copy(opcode = LDX))),
    (Elidable & HasOpcode(ANC) & DoesntMatterWhatItDoesWith(State.C)) ~~> (_.map(_.copy(opcode = AND))),
  )

  val All: List[AssemblyOptimization] = List(
    UseLax,
    UseSax,
    UseSbx,
    UseAnc,
    UseSlo,
    UseSre,
    UseAlr,
    UseArr,
    UseRla,
    UseRra,
    UseIsc,
    UseDcp,
    UseMultiple,
    CleaningUp,
  )
}
