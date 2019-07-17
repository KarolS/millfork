package millfork.test.emu

import millfork.output.{AfterCodeByteAllocator, CurrentBankFragmentOutput, UpwardByteAllocator, VariableAllocator}
import millfork.parser.TextCodec
import millfork.{Cpu, CpuFamily, OutputStyle, Platform, ViceDebugOutputFormat}

/**
  * @author Karol Stasiak
  */
object EmuPlatform {
  private val pointers: List[Int] = (0 until 256).toList

  def get(cpu: Cpu.Value) = new Platform(
    cpu,
    Map(),
    Nil,
    TextCodec.Ascii,
    TextCodec.Ascii,
    Platform.builtInCpuFeatures(cpu),
    CurrentBankFragmentOutput(0, 0xffff),
    Map(
      "default" -> (if (cpu == Cpu.Intel8086) new UpwardByteAllocator(0x100, 0xb000) else new UpwardByteAllocator(0x200, 0xb000)),
      "second" -> new UpwardByteAllocator(0x8000, 0xa000),
      "third" -> new UpwardByteAllocator(0xa000, 0xc000)
    ),
    Map(
      "default" -> new VariableAllocator(
        if (CpuFamily.forType(cpu) == CpuFamily.M6502) pointers else Nil,
        new AfterCodeByteAllocator(0x200, 0xff00)),
      "second" -> new VariableAllocator(Nil, new AfterCodeByteAllocator(0x8000, 0xa000)),
      "third" -> new VariableAllocator(Nil, new AfterCodeByteAllocator(0xa000, 0xc000))
    ),
    if (CpuFamily.forType(cpu) == CpuFamily.M6502) 4 else 0,
    pointers,
    ".bin",
    false,
    false,
    Map("default" -> 0, "second" -> 2, "third" -> 3),
    "default",
    None,
    ViceDebugOutputFormat,
    OutputStyle.Single
  )
}
