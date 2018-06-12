package millfork.assembly.mos.opt

import millfork.{CompilationFlag, CompilationOptions}
import millfork.assembly.mos.AssemblyLine
import millfork.assembly.mos.OpcodeClasses
import millfork.env.{Label, MemoryAddressConstant, NormalFunction, NumericConstant}

/**
  * @author Karol Stasiak
  */
object CoarseFlowAnalyzer {

  def analyze(f: NormalFunction, code: List[AssemblyLine], compilationOptions: CompilationOptions): List[CpuStatus] = {
    val ceFlag = compilationOptions.flag(CompilationFlag.Emit65CE02Opcodes)
    val cmosFlag = compilationOptions.flag(CompilationFlag.EmitCmosOpcodes)
    val initialStatus =
      if (compilationOptions.flag(CompilationFlag.Emit65CE02Opcodes)) CpuStatus.initialStatusCE
      else CpuStatus.initialStatusStandard
    val functionStartStatus =
      if (f.interrupt) {
        if (ceFlag) CpuStatus.initialInterruptStatusCE
        else if (cmosFlag) CpuStatus.initialInterruptStatusCE
        else CpuStatus.initialInterruptStatusStandard
      } else initialStatus
    val emptyStatus =
      if (compilationOptions.flag(CompilationFlag.Emit65CE02Opcodes)) CpuStatus.emptyStatusCE
      else CpuStatus.emptyStatusStandard
    val flagArray = Array.fill[CpuStatus](code.length)(emptyStatus)
    val codeArray = code.toArray

    var changed = true
    while (changed) {
      changed = false
      var currentStatus: CpuStatus = functionStartStatus
      for (i <- codeArray.indices) {
        import millfork.assembly.mos.Opcode._
        import millfork.assembly.AddrMode._
        if (flagArray(i) != currentStatus) {
          changed = true
          flagArray(i) = currentStatus
        }
        codeArray(i) match {
          case AssemblyLine(LABEL, _, MemoryAddressConstant(Label(l)), _) =>
            val L = l
            currentStatus = codeArray.indices.flatMap(j => codeArray(j) match {
              case AssemblyLine(_, _, MemoryAddressConstant(Label(L)), _) => Some(flagArray(j))
              case _ => None
            }).fold(currentStatus)(_ ~ _)

          case AssemblyLine(JSR | BYTE, _, _, _) =>
            currentStatus = initialStatus

          case AssemblyLine(op, Implied, _, _) if FlowAnalyzerForImplied.hasDefinition(op) =>
            currentStatus = FlowAnalyzerForImplied.get(op)(currentStatus)

          case AssemblyLine(op, Immediate | WordImmediate, NumericConstant(nn, _), _) if FlowAnalyzerForImmediate.hasDefinition(op) =>
            currentStatus = FlowAnalyzerForImmediate.get(op)(nn.toInt, currentStatus)

          case AssemblyLine(op, _, _, _) if FlowAnalyzerForTheRest.hasDefinition(op) =>
            currentStatus = FlowAnalyzerForTheRest.get(op)(currentStatus)

          case AssemblyLine(opcode, addrMode, _, _) =>
            currentStatus = currentStatus.copy(src = AnyStatus)
            if (OpcodeClasses.ChangesX(opcode)) currentStatus = currentStatus.copy(x = AnyStatus)
            if (OpcodeClasses.ChangesY(opcode)) currentStatus = currentStatus.copy(y = AnyStatus)
            if (OpcodeClasses.ChangesAAlways(opcode)) currentStatus = currentStatus.copy(a = AnyStatus, a0 = AnyStatus, a7 = AnyStatus)
            if (addrMode == Implied && OpcodeClasses.ChangesAIfImplied(opcode)) currentStatus = currentStatus.copy(a = AnyStatus, a0 = AnyStatus, a7 = AnyStatus)
            if (OpcodeClasses.ChangesAHAlways(opcode)) currentStatus = currentStatus.copy(ah = AnyStatus)
            if (addrMode == Implied && OpcodeClasses.ChangesAHIfImplied(opcode)) currentStatus = currentStatus.copy(ah = AnyStatus)
            if (OpcodeClasses.ChangesNAndZ(opcode)) currentStatus = currentStatus.nz
            if (OpcodeClasses.ChangesC(opcode)) currentStatus = currentStatus.copy(c = AnyStatus)
            if (OpcodeClasses.ChangesV(opcode)) currentStatus = currentStatus.copy(v = AnyStatus)
        }
      }
//                  flagArray.zip(codeArray).foreach{
//                    case (fl, y) => if (y.isPrintable) println(f"$fl%-32s $y%-32s")
//                  }
//                  println("---------------------")
    }

    flagArray.toList
  }
}
