package millfork.assembly.z80.opt

import millfork.assembly.opt.{AnyStatus, SingleStatus}
import millfork.assembly.z80._
import millfork.env.{Label, MemoryAddressConstant, NormalFunction, NumericConstant}
import millfork.node.ZRegister
import millfork.CompilationOptions

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

          case ZLine(CALL, _, _, _) =>
            currentStatus = initialStatus.copy(memIx = currentStatus.memIx)
          case ZLine(BYTE, _, _, _) =>
            currentStatus = initialStatus

          case ZLine(ADD, OneRegister(s), _, _) =>
            currentStatus = currentStatus.copy(a = (currentStatus.a <*> currentStatus.getRegister(s)) ((m, n) => (m + n) & 0xff),
              cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)
          case ZLine(SUB, OneRegister(s), _, _) =>
            currentStatus = currentStatus.copy(a = (currentStatus.a <*> currentStatus.getRegister(s)) ((m, n) => (m - n) & 0xff),
              cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)
          case ZLine(AND, OneRegister(s), _, _) =>
            currentStatus = currentStatus.copy(a = (currentStatus.a <*> currentStatus.getRegister(s)) ((m, n) => (m & n) & 0xff),
              cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)
          case ZLine(OR, OneRegister(s), _, _) =>
            currentStatus = currentStatus.copy(a = (currentStatus.a <*> currentStatus.getRegister(s)) ((m, n) => (m | n) & 0xff),
              cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)
          case ZLine(XOR, OneRegister(s), _, _) =>
            currentStatus = currentStatus.copy(a = (currentStatus.a <*> currentStatus.getRegister(s)) ((m, n) => (m ^ n) & 0xff),
              cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)

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

          case ZLine(LD, TwoRegisters(t, ZRegister.IMM_8), NumericConstant(value, _), _) =>
            currentStatus = currentStatus.setRegister(t, SingleStatus(value.toInt))
          case ZLine(LD, TwoRegistersOffset(t, ZRegister.IMM_8, o), NumericConstant(value, _), _) =>
            currentStatus = currentStatus.setRegister(t, SingleStatus(value.toInt), o)
          case ZLine(LD | LD_16, TwoRegisters(t, s), _, _) =>
            currentStatus = currentStatus.setRegister(t, currentStatus.getRegister(s))
          case ZLine(LD | LD_16, TwoRegistersOffset(t, s, o), _, _) =>
            currentStatus = currentStatus.setRegister(t, currentStatus.getRegister(s, o), o)
          case ZLine(ADD_16, TwoRegisters(t, s), _, _) =>
            currentStatus = currentStatus.copy(cf = AnyStatus, zf = AnyStatus, sf = AnyStatus, pf = AnyStatus, hf = AnyStatus)
              .setRegister(t, (currentStatus.getRegister(t) <*> currentStatus.getRegister(s)) ((m, n) => (m + n) & 0xffff))
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
