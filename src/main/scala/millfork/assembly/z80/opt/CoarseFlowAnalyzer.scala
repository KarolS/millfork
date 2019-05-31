package millfork.assembly.z80.opt

import millfork.assembly.opt.{AnyStatus, FlowCache, SingleStatus, Status}
import millfork.assembly.z80._
import millfork.env._
import millfork.node.ZRegister
import millfork.{CompilationFlag, CompilationOptions, Cpu}

/**
  * @author Karol Stasiak
  */
object CoarseFlowAnalyzer {

  val cache = new FlowCache[ZLine, CpuStatus]("z80 forward")

  def analyze(f: NormalFunction, code: List[ZLine], compilationOptions: CompilationOptions): List[CpuStatus] = {
    cache.get(code).foreach(return _)
    val initialStatus = CpuStatus()
    val functionStartStatus = CpuStatus()
    val emptyStatus = CpuStatus()
    val flagArray = Array.fill[CpuStatus](code.length)(emptyStatus)
    val codeArray = code.toArray
    val z80 = compilationOptions.flag(CompilationFlag.EmitZ80Opcodes)

    val preservesB: Set[String] = Set("__mul_u8u8u8")
    val preservesC: Set[String] = if (z80) Set("__mul_u8u8u8") else Set()
    val preservesD: Set[String] = Set()
    val preservesE: Set[String] = Set()
    val preservesH: Set[String] = Set("__mul_u8u8u8")
    val preservesL: Set[String] = Set("__mul_u8u8u8")

    var changed = true
    while (changed) {
      changed = false
      var currentStatus: CpuStatus = functionStartStatus
      for (i <- codeArray.indices) {
        import millfork.assembly.z80.ZOpcode._
        if (flagArray(i) != currentStatus) {
          changed = true
          flagArray(i) = currentStatus
        }
        codeArray(i) match {
          case ZLine0(LABEL, _, MemoryAddressConstant(Label(l))) =>
            val L = l
            currentStatus = codeArray.indices.flatMap(j => codeArray(j) match {
              case ZLine0(DJNZ, _, MemoryAddressConstant(Label(L))) => Some(flagArray(j).copy(b = AnyStatus))
              case ZLine0(_, _, MemoryAddressConstant(Label(L))) => Some(flagArray(j))
              case _ => None
            }).fold(currentStatus)(_ ~ _)

          case ZLine0(CALL, reg, MemoryAddressConstant(fun: FunctionInMemory)) =>
            val n = fun.name
            val mayBeCalled = reg match {
              case IfFlagSet(f) => !currentStatus.getFlag(f).contains(false)
              case IfFlagClear(f) => !currentStatus.getFlag(f).contains(true)
              case _ => true
            }
            if (mayBeCalled) {
              val result = initialStatus.copy(memIx = currentStatus.memIx)
              currentStatus = result.copy(
                b = if (preservesB(n)) currentStatus.b else result.b,
                c = if (preservesC(n)) currentStatus.c else result.c,
                d = if (preservesD(n)) currentStatus.d else result.d,
                e = if (preservesE(n)) currentStatus.e else result.e,
                h = if (preservesH(n)) currentStatus.h else result.h,
                l = if (preservesL(n)) currentStatus.l else result.l
              )
            }

          case ZLine0(NOP | DISCARD_A | DISCARD_BC | DISCARD_DE | DISCARD_HL | DISCARD_F | SIM, _, _) =>
            ()
          case ZLine0(PUSH, _, _) =>
            ()
          case ZLine0(POP, OneRegister(r), _) =>
            currentStatus = currentStatus.setRegister(r, AnyStatus)

          case ZLine0(JR | JP | RET, IfFlagSet(flag), _) =>
            currentStatus = currentStatus.setFlag(flag, newStatus = false)

          case ZLine0(JR | JP | RET, IfFlagClear(flag), _) =>
            currentStatus = currentStatus.setFlag(flag, newStatus = true)

          case ZLine0(JR | JP | RET, NoRegisters | OneRegister(_), _) =>
            currentStatus = initialStatus.copy(memIx = currentStatus.memIx)

          case ZLine0(DJNZ, _, _) =>
            // TODO
            currentStatus = currentStatus.copy(
              b = Status.SingleZero,
              zf = Status.SingleTrue,
              nf = AnyStatus,
              cf = AnyStatus,
              hf = AnyStatus,
              sf = AnyStatus)

          case ZLine0(BYTE, _, _) =>
            currentStatus = initialStatus

          case ZLine0(SUB, OneRegister(ZRegister.IMM_8), NumericConstant(0, _)) =>
            currentStatus = currentStatus.copy(
              nf = Status.SingleTrue,
              cf = Status.SingleFalse,
              zf = currentStatus.a.z(),
              sf = currentStatus.a.n(),
              pf = if (z80) Status.SingleFalse else AnyStatus,
              hf = Status.SingleFalse)

          case l@ZLine0(ADD, OneRegister(ZRegister.IMM_8), NumericConstant(n, _)) =>
            val (newA, newC) = currentStatus.a.adc(n.toInt, currentStatus.cf, Status.SingleFalse)
            currentStatus = currentStatus.copy(a = newA,
              nf = Status.SingleFalse, cf = newC, zf = newA.z(), sf = newA.n(), pf = AnyStatus, hf = AnyStatus)
          case l@ZLine0(ADC, OneRegister(ZRegister.IMM_8), NumericConstant(n, _)) =>
            val (newA, newC) = currentStatus.a.adc(n.toInt, currentStatus.cf, Status.SingleFalse)
            currentStatus = currentStatus.copy(a = newA,
              nf = Status.SingleFalse, cf = newC, zf = newA.z(), sf = newA.n(), pf = AnyStatus, hf = AnyStatus)
          case l@ZLine0(ADD, OneRegister(s), _) =>
            val (newA, newC) = currentStatus.a.adc(currentStatus.getRegister(s), Status.SingleFalse)
            currentStatus = currentStatus.copy(a = newA,
              nf = Status.SingleFalse, cf = newC, zf = newA.z(), sf = newA.n(), pf = AnyStatus, hf = AnyStatus)
          case l@ZLine0(ADC, OneRegister(s), _) =>
            val (newA, newC) = currentStatus.a.adc(currentStatus.getRegister(s), currentStatus.cf)
            currentStatus = currentStatus.copy(a = newA,
              nf = Status.SingleFalse, cf = newC, zf = newA.z(), sf = newA.n(), pf = AnyStatus, hf = AnyStatus)

          case ZLine0(SUB, OneRegister(s), _) =>
            currentStatus = currentStatus.copy(a = (currentStatus.a <*> currentStatus.getRegister(s)) ((m, n) => (m - n) & 0xff),
              nf = Status.SingleTrue, cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)

          case ZLine0(AND, OneRegister(s), _) =>
            currentStatus = currentStatus.copy(a = (currentStatus.a <*> currentStatus.getRegister(s)) ((m, n) => (m & n) & 0xff),
              nf = Status.SingleFalse, cf = Status.SingleFalse, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)
          case ZLine0(OR, OneRegister(ZRegister.A), _) =>
            currentStatus = currentStatus.copy(nf = Status.SingleFalse, cf = Status.SingleFalse, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)
          case ZLine0(XOR, OneRegister(ZRegister.A), _) =>
            currentStatus = currentStatus.copy(a = Status.SingleZero, nf = Status.SingleFalse, cf = Status.SingleFalse, zf = Status.SingleTrue, sf = Status.SingleFalse, pf = AnyStatus, hf = AnyStatus)
          case ZLine0(OR, OneRegister(s), _) =>
            currentStatus = currentStatus.copy(a = (currentStatus.a <*> currentStatus.getRegister(s)) ((m, n) => (m | n) & 0xff),
              nf = Status.SingleFalse, cf = Status.SingleFalse, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)
          case ZLine0(XOR, OneRegister(s), _) =>
            currentStatus = currentStatus.copy(a = (currentStatus.a <*> currentStatus.getRegister(s)) ((m, n) => (m ^ n) & 0xff),
              nf = Status.SingleFalse, cf = Status.SingleFalse, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)

          case ZLine0(INC, OneRegister(r), _) =>
            val newV = currentStatus.getRegister(r).map(i => i.+(1).&(0xff))
            currentStatus = currentStatus.
              copy(cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus).
              setRegister(r, newV)
          case ZLine0(DEC, OneRegister(r), _) =>
            val newV = currentStatus.getRegister(r).map(i => i.-(1).&(0xff))
            currentStatus = currentStatus.
              copy(cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus).
              setRegister(r, newV)
          case ZLine0(INC, OneRegisterOffset(r, o), _) =>
            val newV = currentStatus.getRegister(r, o).map(i => i.+(1).&(0xff))
            currentStatus = currentStatus.
              copy(cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus).
              setRegister(r, newV, o)
          case ZLine0(DEC, OneRegisterOffset(r, o), _) =>
            val newV = currentStatus.getRegister(r, o).map(i => i.-(1).&(0xff))
            currentStatus = currentStatus.
              copy(cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus).
              setRegister(r, newV, o)

          case ZLine0(INC_16, OneRegister(r), _) =>
            val newV = currentStatus.getRegister(r).map(i => i.+(1).&(0xffff))
            currentStatus = currentStatus.
              copy(cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus).
              setRegister(r, newV)
          case ZLine0(DEC_16, OneRegister(r), _) =>
            val newV = currentStatus.getRegister(r).map(i => i.-(1).&(0xffff))
            currentStatus = currentStatus.
              copy(cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus).
              setRegister(r, newV)

          case ZLine0(LD_AHLI, _, _) =>
            val newHL = currentStatus.getRegister(ZRegister.HL).map(i => i.+(1).&(0xffff))
            currentStatus = currentStatus.copy(a = AnyStatus).setRegister(ZRegister.HL, newHL)
          case ZLine0(LD_HLIA, _, _) =>
            val newHL = currentStatus.getRegister(ZRegister.HL).map(i => i.+(1).&(0xffff))
            currentStatus = currentStatus.setRegister(ZRegister.HL, newHL)
          case ZLine0(LD_AHLD, _, _) =>
            val newHL = currentStatus.getRegister(ZRegister.HL).map(i => i.-(1).&(0xffff))
            currentStatus = currentStatus.copy(a = AnyStatus).setRegister(ZRegister.HL, newHL)
          case ZLine0(LD_HLDA, _, _) =>
            val newHL = currentStatus.getRegister(ZRegister.HL).map(i => i.-(1).&(0xffff))
            currentStatus = currentStatus.setRegister(ZRegister.HL, newHL)

          case ZLine0(op, OneRegister(r), _) if ZOpcodeClasses.SET(op) =>
            currentStatus = currentStatus.setRegister(r, currentStatus.getRegister(r).map(i => i | 1.<<(ZOpcodeClasses.SET_seq.indexOf(op))))
          case ZLine0(op, OneRegister(r), _) if ZOpcodeClasses.RES(op) =>
            currentStatus = currentStatus.setRegister(r, currentStatus.getRegister(r).map(i => i & ~1.<<(ZOpcodeClasses.RES_seq.indexOf(op))))

          case l@ZLine0(ADD, OneRegisterOffset(s, o), _) =>
            val (newA, newC) = currentStatus.a.adc(currentStatus.getRegister(s, o), Status.SingleFalse)
            currentStatus = currentStatus.copy(a = newA,
              nf = Status.SingleFalse, cf = newC, zf = newA.z(), sf = newA.n(), pf = AnyStatus, hf = AnyStatus)
          case l@ZLine0(ADC, OneRegisterOffset(s, o), _) =>
            val (newA, newC) = currentStatus.a.adc(currentStatus.getRegister(s, o), currentStatus.cf)
            currentStatus = currentStatus.copy(a = newA,
              nf = Status.SingleFalse, cf = newC, zf = newA.z(), sf = newA.n(), pf = AnyStatus, hf = AnyStatus)
          case ZLine0(SUB, OneRegisterOffset(s, o), _) =>
            currentStatus = currentStatus.copy(a = (currentStatus.a <*> currentStatus.getRegister(s, o)) ((m, n) => (m - n) & 0xff),
              cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)
          case ZLine0(AND, OneRegisterOffset(s, o), _) =>
            currentStatus = currentStatus.copy(a = (currentStatus.a <*> currentStatus.getRegister(s, o)) ((m, n) => (m & n) & 0xff),
              cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)
          case ZLine0(OR, OneRegisterOffset(s, o), _) =>
            currentStatus = currentStatus.copy(a = (currentStatus.a <*> currentStatus.getRegister(s, o)) ((m, n) => (m | n) & 0xff),
              cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)
          case ZLine0(XOR, OneRegisterOffset(s, o), _) =>
            currentStatus = currentStatus.copy(a = (currentStatus.a <*> currentStatus.getRegister(s, o)) ((m, n) => (m ^ n) & 0xff),
              cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)

          case ZLine0(CP, _, _) =>
            currentStatus = currentStatus.copy(cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)

          case ZLine0(LD_16, TwoRegisters(t, ZRegister.IMM_16), NumericConstant(value, _)) =>
            currentStatus = currentStatus.setRegister(t, SingleStatus(value.toInt))
          case ZLine0(LD_16, TwoRegisters(ZRegister.HL, ZRegister.IMM_16), xx) =>
            currentStatus = currentStatus.setHL(SingleStatus(xx))
          case ZLine0(LD, TwoRegisters(t, ZRegister.IMM_8), NumericConstant(value, _)) =>
            currentStatus = currentStatus.setRegister(t, SingleStatus(value.toInt))
          case ZLine0(LD, TwoRegistersOffset(t, ZRegister.IMM_8, o), NumericConstant(value, _)) =>
            currentStatus = currentStatus.setRegister(t, SingleStatus(value.toInt), o)
          case ZLine0(LD | LD_16, TwoRegisters(t, s), _) =>
            currentStatus = currentStatus.setRegister(t, currentStatus.getRegister(s))
          case ZLine0(LD | LD_16, TwoRegistersOffset(t, s, o), _) =>
            currentStatus = currentStatus.setRegister(t, currentStatus.getRegister(s, o), o)
          case ZLine0(EX_DE_HL, _, _) =>
            currentStatus = currentStatus.copy(d = currentStatus.h, e = currentStatus.l, h = currentStatus.d, l = currentStatus.e)
          case ZLine0(ADD_16, TwoRegisters(t, s), _) =>
            currentStatus = currentStatus.copy(cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)
              .setRegister(t, (currentStatus.getRegister(t) <*> currentStatus.getRegister(s)) ((m, n) => (m + n) & 0xffff))

          case ZLine0(SLA, OneRegister(r), _) =>
            currentStatus = currentStatus.copy(cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)
              .setRegister(r, currentStatus.getRegister(r).map(_.<<(1).&(0xff)))
          case ZLine0(SRL, OneRegister(r), _) =>
            currentStatus = currentStatus.copy(cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)
              .setRegister(r, currentStatus.getRegister(r).map(_.>>(1).&(0x7f)))


          case ZLine0(RLA | RRA | RLCA | RRCA, _, _) =>
            currentStatus = currentStatus.copy(
              a = AnyStatus, cf = AnyStatus,
              zf = AnyStatus,
              pf = AnyStatus, hf = Status.SingleFalse)

          case ZLine0(SCF, _, _) =>
            currentStatus = currentStatus.copy(cf = Status.SingleTrue, hf = Status.SingleFalse, nf = Status.SingleFalse)
          case ZLine0(CCF, _, _) =>
            currentStatus = currentStatus.copy(cf = currentStatus.cf.negate, hf = AnyStatus, nf = AnyStatus)

          case ZLine0(CPL, _, _) =>
            currentStatus = currentStatus.copy(a = currentStatus.a.map(_ ^ 0xff), hf = AnyStatus, nf = Status.SingleTrue)
          case ZLine0(NEG, _, _) =>
            currentStatus = currentStatus.copy(
              a = currentStatus.a.map(x => (-x)&0xff),
              hf = AnyStatus,
              zf = currentStatus.a.z(),
              sf = AnyStatus,
              cf = AnyStatus,
              pf = AnyStatus,
              nf = Status.SingleTrue)
          case ZLine0(DAA, _, _) =>
            // TODO: full algorithm?
            currentStatus = currentStatus.copy(
              a = if (currentStatus.h.contains(false)) currentStatus.a else AnyStatus,
              hf = AnyStatus,
              zf = AnyStatus,
              sf = AnyStatus,
              cf = AnyStatus,
              pf = AnyStatus)

          case ZLine0(BIT7, OneRegister(r), _) =>
            currentStatus = currentStatus.copy(
              zf = currentStatus.getRegister(r).bit7.negate,
              hf = Status.SingleTrue,
              sf = AnyStatus,
              pf = AnyStatus,
              nf = Status.SingleFalse)
          case ZLine0(BIT0, OneRegister(r), _) =>
            currentStatus = currentStatus.copy(
              zf = currentStatus.getRegister(r).bit0.negate,
              hf = Status.SingleTrue,
              sf = AnyStatus,
              pf = AnyStatus,
              nf = Status.SingleFalse)

          case ZLine0(RIM, _, _) =>
            currentStatus = currentStatus.copy(a = AnyStatus)

          case ZLine0(opcode, registers, _) =>
            currentStatus = currentStatus.copy(cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)
            if (ZOpcodeClasses.ChangesAAlways(opcode)) currentStatus = currentStatus.copy(a = AnyStatus)
            if (ZOpcodeClasses.ChangesBCAlways(opcode)) currentStatus = currentStatus.copy(b = AnyStatus, c = AnyStatus)
            if (ZOpcodeClasses.ChangesDEAlways(opcode)) currentStatus = currentStatus.copy(d = AnyStatus, e = AnyStatus)
            registers match {
              case OneRegister(r) => currentStatus = currentStatus.setRegister(r, AnyStatus)
              case TwoRegisters(r1, r2) => currentStatus = currentStatus.setRegister(r1, AnyStatus).setRegister(r2, AnyStatus)
              case OneRegisterOffset(r1, o) => currentStatus = currentStatus.setRegister(r1, AnyStatus, o)
              case TwoRegistersOffset(r1, r2, o) => currentStatus = currentStatus.setRegister(r1, AnyStatus, o).setRegister(r2, AnyStatus, o)
              case _ => ()
            }
        }
      }
//                        flagArray.zip(codeArray).foreach{
//                          case (fl, y) => if (y.isPrintable) println(f"$fl%-32s $y%-32s")
//                        }
//                        println("---------------------")
    }

    cache.put(code, flagArray.toList)
  }
}
