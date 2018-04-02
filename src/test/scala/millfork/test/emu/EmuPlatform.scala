package millfork.test.emu

import millfork.output.{AfterCodeByteAllocator, CurrentBankFragmentOutput, UpwardByteAllocator, VariableAllocator}
import millfork.{Cpu, OutputStyle, Platform}

/**
  * @author Karol Stasiak
  */
object EmuPlatform {
  def get(cpu: Cpu.Value) = new Platform(
    cpu,
    Map(),
    Nil,
    CurrentBankFragmentOutput(0, 0xffff),
    Map("default" -> new UpwardByteAllocator(0x200, 0xb000)),
    Map("default" -> new VariableAllocator((0 until 256 by 2).toList, new AfterCodeByteAllocator(0xff00))),
    ".bin",
    false,
    Map("default" -> 0),
    "default",
    OutputStyle.Single
  )
}
