package millfork.test.emu

import millfork.output.{AfterCodeByteAllocator, CurrentBankFragmentOutput, UpwardByteAllocator, VariableAllocator}
import millfork.{Cpu, CpuFamily, OutputStyle, Platform}

/**
  * @author Karol Stasiak
  */
object EmuPlatform {
  private val pointers: List[Int] = (0 until 256 by 2).toList

  def get(cpu: Cpu.Value) = new Platform(
    cpu,
    Map(),
    Nil,
    CurrentBankFragmentOutput(0, 0xffff),
    Map("default" -> new UpwardByteAllocator(0x200, 0xb000)),
    Map("default" -> new VariableAllocator(
      if (CpuFamily.forType(cpu) == CpuFamily.M6502) pointers else Nil,
      new AfterCodeByteAllocator(0xff00))),
    if (CpuFamily.forType(cpu) == CpuFamily.M6502) 2 else 0,
    pointers,
    ".bin",
    false,
    Map("default" -> 0),
    "default",
    OutputStyle.Single
  )
}
