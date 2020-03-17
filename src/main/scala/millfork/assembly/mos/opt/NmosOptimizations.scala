package millfork.assembly.mos.opt

import millfork.assembly._
import millfork.assembly.mos._
import millfork.assembly.mos.Opcode._
import AddrMode._
import millfork.assembly.mos.OpcodeClasses._
import millfork.env.{Constant, NormalFunction, NumericConstant}
/**
  * @author Karol Stasiak
  */
object NmosOptimizations {

  private val LdxAddrModes = Set(Immediate, Absolute, ZeroPage, ZeroPageY, AbsoluteY)
  private val LdyAddrModes = Set(Immediate, Absolute, ZeroPage, ZeroPageX, AbsoluteX)
  private val StxAddrModes = Set(Absolute, ZeroPage, ZeroPageY)
  private val StyAddrModes = Set(Absolute, ZeroPage, ZeroPageX)
  private val StaAddrModes = Set(Absolute, ZeroPage, ZeroPageX, AbsoluteX, IndexedY, IndexedX, AbsoluteY)
  private val CpxyAddrModes = Set(Immediate, Absolute, ZeroPage)

  private def incDecThroughIndexRegister(amount: Int, dec: Boolean, carrySet: Boolean, useX: Boolean) = {
    val ldAddrModes = if (useX) LdxAddrModes else LdyAddrModes
    val stAddrModes = if (useX) StxAddrModes else StyAddrModes
    val ldOp = if (useX) LDX else LDY
    val stOp = if (useX) STX else STY
    val changeOp = if (dec) if (useX) DEX else DEY else if (useX) INX else INY
    val addOp = if (dec) SBC else ADC
    val addParam = if (dec ^ carrySet) amount + 1 else amount
    val indexState = if (useX) State.X else State.Y
    val cState = if (carrySet) HasSet(State.C) else HasClear(State.C)
    val carryOp = if (carrySet) SEC else CLC

    (Elidable & HasOpcode(LDA) & HasAddrModeIn(ldAddrModes) & HasClear(State.D)).capture(11) ~
      (Elidable & HasOpcode(carryOp)).? ~
      (Elidable & HasOpcode(addOp) & HasImmediate(addParam) & cState) ~
      (Elidable & HasOpcode(STA) & HasAddrModeIn(stAddrModes) & DoesntMatterWhatItDoesWith(State.A, State.C, State.V, indexState)).capture(12) ~~> { (_, ctx) =>
      ctx.get[List[AssemblyLine]](11).head.copy(opcode = ldOp) ::
        (List.fill(amount)(AssemblyLine.implied(changeOp)) :+
          ctx.get[List[AssemblyLine]](12).head.copy(opcode = stOp))
    }
  }

  val IncrementThroughIndexRegisters = new RuleBasedAssemblyOptimization("Increment through index registers",
    needsFlowInfo = FlowInfoRequirement.BothFlows,
    incDecThroughIndexRegister(1, dec = false, carrySet = false, useX = true),
    incDecThroughIndexRegister(1, dec = false, carrySet = false, useX = false),
    incDecThroughIndexRegister(1, dec = false, carrySet = true, useX = true),
    incDecThroughIndexRegister(1, dec = false, carrySet = true, useX = false),
    incDecThroughIndexRegister(1, dec = true, carrySet = true, useX = true),
    incDecThroughIndexRegister(1, dec = true, carrySet = true, useX = false),
    incDecThroughIndexRegister(2, dec = false, carrySet = false, useX = true),
    incDecThroughIndexRegister(2, dec = false, carrySet = false, useX = false),
    incDecThroughIndexRegister(2, dec = false, carrySet = true, useX = true),
    incDecThroughIndexRegister(2, dec = false, carrySet = true, useX = false),
    incDecThroughIndexRegister(2, dec = true, carrySet = true, useX = true),
    incDecThroughIndexRegister(2, dec = true, carrySet = true, useX = false),

    (Elidable & HasOpcode(TYA) & HasClear(State.D)) ~
      (Elidable & HasOpcode(CLC)) ~
      (Elidable & HasOpcode(ADC) & HasImmediate(1) & DoesntMatterWhatItDoesWith(State.C, State.N, State.Z, State.V)) ~~> { code =>
      AssemblyLine.implied(INY).pos(code(2).source) :: code.head :: AssemblyLine.implied(DEY).pos(code(2).source) :: code.drop(3)
    },
    (Elidable & HasOpcode(TXA) & HasClear(State.D)) ~
      (Elidable & HasOpcode(CLC)) ~
      (Elidable & HasOpcode(ADC) & HasImmediate(1) & DoesntMatterWhatItDoesWith(State.C, State.N, State.Z, State.V)) ~~> { code =>
      AssemblyLine.implied(INX).pos(code(2).source) :: code.head :: AssemblyLine.implied(DEX).pos(code(2).source) :: code.drop(3)
    },
  )

  val UseIndexedX = new RuleBasedAssemblyOptimization("Using indexed-indirect addressing mode",
    needsFlowInfo = FlowInfoRequirement.BothFlows,

    (Elidable & HasOpcode(LDY) & HasImmediate(0) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~
      (Linear & Not(ConcernsY)).* ~
      (Elidable & HasAddrMode(IndexedY) & HasX(0) & DoesntMatterWhatItDoesWith(State.Y)) ~~> { code =>
      code.tail.init :+ code.last.copy(addrMode = IndexedX)
    },

    (Elidable & HasOpcode(LDY) & HasImmediate(0) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~
      (Linear & Not(ConcernsY)).* ~
      (Elidable & HasOpcodeIn(Set(ISC, DCP, SLO, SRE, RRA, RLA)) & HasAddrMode(IndexedY) & HasX(0xff) & DoesntMatterWhatItDoesWith(State.Y, State.X)) ~~> { code =>
      code.tail.init ++ List(AssemblyLine.implied(INX), code.last.copy(addrMode = IndexedX))
    },

    (Elidable & HasOpcode(LDY) & HasImmediate(0) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~
      (Linear & Not(ConcernsY)).* ~
      (Elidable & HasOpcodeIn(Set(ISC, DCP, SLO, SRE, RRA, RLA)) & HasAddrMode(IndexedY) & HasX(1) & DoesntMatterWhatItDoesWith(State.Y, State.X)) ~~> { code =>
      code.tail.init ++ List(AssemblyLine.implied(DEX), code.last.copy(addrMode = IndexedX))
    },

    (Elidable & HasOpcode(LDY) & HasImmediate(0) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~
      (Linear & Not(ConcernsY)).* ~
      (Elidable & HasAddrMode(IndexedY) & MatchX(1) & DoesntMatterWhatItDoesWith(State.Y)) ~~> { (code, ctx)=>
      val lastLine = code.last
      code.tail.init ++ List(lastLine.copy(addrMode = IndexedX, parameter = lastLine.parameter - ctx.get[Int](1)))
    },

    (Elidable & HasOpcode(LDY) & HasImmediate(0) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~
      (Linear & Not(ConcernsY)).*.capture(2) ~
      (Elidable & HasAddrMode(IndexedY) & DoesntMatterWhatItDoesWith(State.Y, State.X)).capture(0) ~
      Linear.*.capture(3) ~
      (Elidable & HasOpcode(LDX) & MatchImmediate(1) & DoesntMatterWhatItDoesWith(State.Z, State.N)) ~~> { (code, ctx) =>
      val mainLine = ctx.get[List[AssemblyLine]](0).head
      ctx.get[List[AssemblyLine]](2) ++ List(
        code.last,
        mainLine.copy(addrMode = IndexedX, parameter = mainLine.parameter - ctx.get[Int](1))) ++
        ctx.get[List[AssemblyLine]](3)
    },

  )

  val All: List[AssemblyOptimization[AssemblyLine]] = List(
    IncrementThroughIndexRegisters,
    UseIndexedX
  )
}
