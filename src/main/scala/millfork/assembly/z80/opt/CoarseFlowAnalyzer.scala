package millfork.assembly.z80.opt

import millfork.assembly.opt.{AnyStatus, SingleStatus, Status}
import millfork.assembly.z80._
import millfork.env._
import millfork.node.ZRegister
import millfork.{CompilationFlag, CompilationOptions, Cpu}

/**
  * @author Karol Stasiak
  */
object CoarseFlowAnalyzer {

  def analyze(f: NormalFunction, code: List[ZLine], compilationOptions: CompilationOptions): List[CpuStatus] = {
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
          case ZLine(LABEL, _, MemoryAddressConstant(Label(l)), _) =>
            val L = l
            currentStatus = codeArray.indices.flatMap(j => codeArray(j) match {
              case ZLine(DJNZ, _, MemoryAddressConstant(Label(L)), _) => Some(flagArray(j).copy(b = AnyStatus))
              case ZLine(_, _, MemoryAddressConstant(Label(L)), _) => Some(flagArray(j))
              case _ => None
            }).fold(currentStatus)(_ ~ _)

          case ZLine(CALL, _, MemoryAddressConstant(fun: FunctionInMemory), _) =>
            val n = fun.name
            val result = initialStatus.copy(memIx = currentStatus.memIx)
            currentStatus = result.copy(
              b = if (preservesB(n)) currentStatus.b else result.b,
              c = if (preservesC(n)) currentStatus.c else result.c,
              d = if (preservesD(n)) currentStatus.d else result.d,
              e = if (preservesE(n)) currentStatus.e else result.e,
              h = if (preservesH(n)) currentStatus.h else result.h,
              l = if (preservesL(n)) currentStatus.l else result.l
            )

          case ZLine(CALL, _, _, _) =>
            currentStatus = initialStatus.copy(memIx = currentStatus.memIx)
          case ZLine(BYTE, _, _, _) =>
            currentStatus = initialStatus

          case ZLine(ADD, OneRegister(ZRegister.IMM_8), NumericConstant(0, _), _) =>
            currentStatus = currentStatus.copy(
              nf = Status.SingleFalse,
              cf = Status.SingleFalse,
              zf = currentStatus.a.map(_.&(0xff) == 0),
              sf = currentStatus.a.map(_.&(0x80) == 0),
              pf = if (z80) Status.SingleFalse else AnyStatus,
              hf = Status.SingleFalse)
          case ZLine(SUB, OneRegister(ZRegister.IMM_8), NumericConstant(0, _), _) =>
            currentStatus = currentStatus.copy(
              nf = Status.SingleTrue,
              cf = Status.SingleFalse,
              zf = currentStatus.a.map(_.&(0xff) == 0),
              sf = currentStatus.a.map(_.&(0x80) == 0),
              pf = if (z80) Status.SingleFalse else AnyStatus,
              hf = Status.SingleFalse)
          case l@ZLine(ADD, OneRegister(s), _, _) =>
            currentStatus = currentStatus.copy(a = (currentStatus.a <*> currentStatus.getRegister(s)) ((m, n) => (m + n) & 0xff),
              nf = Status.SingleFalse, cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)
          case ZLine(SUB, OneRegister(s), _, _) =>
            currentStatus = currentStatus.copy(a = (currentStatus.a <*> currentStatus.getRegister(s)) ((m, n) => (m - n) & 0xff),
              nf = Status.SingleTrue, cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)
          case ZLine(AND, OneRegister(s), _, _) =>
            currentStatus = currentStatus.copy(a = (currentStatus.a <*> currentStatus.getRegister(s)) ((m, n) => (m & n) & 0xff),
              nf = Status.SingleFalse, cf = Status.SingleFalse, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)
          case ZLine(OR, OneRegister(ZRegister.A), _, _) =>
            currentStatus = currentStatus.copy(nf = Status.SingleFalse, cf = Status.SingleFalse, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)
          case ZLine(XOR, OneRegister(ZRegister.A), _, _) =>
            currentStatus = currentStatus.copy(a = Status.SingleZero, nf = Status.SingleFalse, cf = Status.SingleFalse, zf = Status.SingleTrue, sf = Status.SingleFalse, pf = AnyStatus, hf = AnyStatus)
          case ZLine(OR, OneRegister(s), _, _) =>
            currentStatus = currentStatus.copy(a = (currentStatus.a <*> currentStatus.getRegister(s)) ((m, n) => (m | n) & 0xff),
              nf = Status.SingleFalse, cf = Status.SingleFalse, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)
          case ZLine(XOR, OneRegister(s), _, _) =>
            currentStatus = currentStatus.copy(a = (currentStatus.a <*> currentStatus.getRegister(s)) ((m, n) => (m ^ n) & 0xff),
              nf = Status.SingleFalse, cf = Status.SingleFalse, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)

          case ZLine(INC, OneRegister(r), _, _) =>
            currentStatus = currentStatus.
              copy(cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus).
              setRegister(r, currentStatus.getRegister(r).map(i => i.+(1).&(0xff)))
          case ZLine(DEC, OneRegister(r), _, _) =>
            currentStatus = currentStatus.
              copy(cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus).
              setRegister(r, currentStatus.getRegister(r).map(i => i.-(1).&(0xff)))
          case ZLine(op, OneRegister(r), _, _) if ZOpcodeClasses.SET(op) =>
            currentStatus = currentStatus.setRegister(r, currentStatus.getRegister(r).map(i => i | 1.<<(ZOpcodeClasses.SET_seq.indexOf(op))))
          case ZLine(op, OneRegister(r), _, _) if ZOpcodeClasses.RES(op) =>
            currentStatus = currentStatus.setRegister(r, currentStatus.getRegister(r).map(i => i & ~1.<<(ZOpcodeClasses.RES_seq.indexOf(op))))

          case ZLine(ADD, OneRegisterOffset(s, o), _, _) =>
            currentStatus = currentStatus.copy(a = (currentStatus.a <*> currentStatus.getRegister(s, o)) ((m, n) => (m + n) & 0xff),
              cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)
          case ZLine(SUB, OneRegisterOffset(s, o), _, _) =>
            currentStatus = currentStatus.copy(a = (currentStatus.a <*> currentStatus.getRegister(s, o)) ((m, n) => (m - n) & 0xff),
              cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)
          case ZLine(AND, OneRegisterOffset(s, o), _, _) =>
            currentStatus = currentStatus.copy(a = (currentStatus.a <*> currentStatus.getRegister(s, o)) ((m, n) => (m & n) & 0xff),
              cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)
          case ZLine(OR, OneRegisterOffset(s, o), _, _) =>
            currentStatus = currentStatus.copy(a = (currentStatus.a <*> currentStatus.getRegister(s, o)) ((m, n) => (m | n) & 0xff),
              cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)
          case ZLine(XOR, OneRegisterOffset(s, o), _, _) =>
            currentStatus = currentStatus.copy(a = (currentStatus.a <*> currentStatus.getRegister(s, o)) ((m, n) => (m ^ n) & 0xff),
              cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)

          case ZLine(CP, _, _, _) =>
            currentStatus = currentStatus.copy(cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)

          case ZLine(LD_16, TwoRegisters(t, ZRegister.IMM_16), NumericConstant(value, _), _) =>
            currentStatus = currentStatus.setRegister(t, SingleStatus(value.toInt))
          case ZLine(LD_16, TwoRegisters(ZRegister.HL, ZRegister.IMM_16), xx, _) =>
            currentStatus = currentStatus.setHL(SingleStatus(xx))
          case ZLine(LD, TwoRegisters(t, ZRegister.IMM_8), NumericConstant(value, _), _) =>
            currentStatus = currentStatus.setRegister(t, SingleStatus(value.toInt))
          case ZLine(LD, TwoRegistersOffset(t, ZRegister.IMM_8, o), NumericConstant(value, _), _) =>
            currentStatus = currentStatus.setRegister(t, SingleStatus(value.toInt), o)
          case ZLine(LD | LD_16, TwoRegisters(t, s), _, _) =>
            currentStatus = currentStatus.setRegister(t, currentStatus.getRegister(s))
          case ZLine(LD | LD_16, TwoRegistersOffset(t, s, o), _, _) =>
            currentStatus = currentStatus.setRegister(t, currentStatus.getRegister(s, o), o)
          case ZLine(EX_DE_HL, _, _, _) =>
            currentStatus = currentStatus.copy(d = currentStatus.h, e = currentStatus.l, h = currentStatus.d, l = currentStatus.e)
          case ZLine(ADD_16, TwoRegisters(t, s), _, _) =>
            currentStatus = currentStatus.copy(cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)
              .setRegister(t, (currentStatus.getRegister(t) <*> currentStatus.getRegister(s)) ((m, n) => (m + n) & 0xffff))

          case ZLine(SLA, OneRegister(r), _, _) =>
            currentStatus = currentStatus.copy(cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)
              .setRegister(r, currentStatus.getRegister(r).map(_.<<(1).&(0xff)))
          case ZLine(SRL, OneRegister(r), _, _) =>
            currentStatus = currentStatus.copy(cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)
              .setRegister(r, currentStatus.getRegister(r).map(_.>>(1).&(0x7f)))


          case ZLine(RLA | RRA | RLCA | RRCA, _, _, _) =>
            currentStatus = currentStatus.copy(
              a = AnyStatus, cf = AnyStatus,
              zf = AnyStatus,
              pf = AnyStatus, hf = Status.SingleFalse)

          case ZLine(SCF, _, _, _) =>
            currentStatus = currentStatus.copy(cf = Status.SingleTrue, hf = Status.SingleFalse, nf = Status.SingleFalse)
          case ZLine(CCF, _, _, _) =>
            currentStatus = currentStatus.copy(cf = currentStatus.cf.negate, hf = AnyStatus, nf = AnyStatus)

          case ZLine(opcode, registers, _, _) =>
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

    flagArray.toList
  }
}
