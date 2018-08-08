package millfork

import millfork.compiler.LabelGenerator
import millfork.error.{ConsoleLogger, Logger}

/**
  * @author Karol Stasiak
  */
case class CompilationOptions(platform: Platform,
                              commandLineFlags: Map[CompilationFlag.Value, Boolean],
                              outputFileName: Option[String],
                              zpRegisterSize: Int,
                              featureOverrides: Map[String, Long],
                              jobContext: JobContext) {

  import CompilationFlag._
  import Cpu._

  @inline
  def log: Logger = jobContext.log
  @inline
  def nextLabel: LabelGenerator = jobContext.nextLabel

  val flags: Map[CompilationFlag.Value, Boolean] = CompilationFlag.values.map { f =>
    f -> commandLineFlags.getOrElse(f, platform.flagOverrides.getOrElse(f, Cpu.defaultFlags(platform.cpu)(f)))
  }.toMap

  def flag(f: CompilationFlag.Value) = flags(f)

  {
    log.setFatalWarnings(flags(CompilationFlag.FatalWarnings))

    var invalids = Set[CompilationFlag.Value]()

    if (CpuFamily.forType(platform.cpu) != CpuFamily.M6502) invalids ++= Set(
      EmitCmosOpcodes, EmitCmosNopOpcodes, EmitHudsonOpcodes, Emit65CE02Opcodes, EmitEmulation65816Opcodes, EmitNative65816Opcodes,
      PreventJmpIndirectBug, LargeCode, ReturnWordsViaAccumulator, LUnixRelocatableCode, RorWarning)

    if (CpuFamily.forType(platform.cpu) != CpuFamily.I80) invalids ++= Set(
      EmitExtended80Opcodes, EmitZ80Opcodes, EmitSharpOpcodes, EmitIntel8080Opcodes, EmitEZ80Opcodes,
      UseIxForStack, UseIyForStack, UseShadowRegistersForInterrupts, UseIntelSyntaxForInput, UseIntelSyntaxForOutput)

    invalids = invalids.filter(flags)

    if (invalids.nonEmpty) {
      log.error("Invalid flags enabled for the current CPU family: " + invalids.mkString(", "))
    }
    if (CpuFamily.forType(platform.cpu) != CpuFamily.M6502 && zpRegisterSize > 0) {
      log.error("Invalid flags enabled for the current CPU family: zp_register" + invalids.mkString(", "))
    }
    CpuFamily.forType(platform.cpu) match {
      case CpuFamily.M6502 =>
        if (flags(DecimalMode)) {
          if (platform.cpu == Ricoh || platform.cpu == StrictRicoh) {
            log.warn("Decimal mode enabled for Ricoh architecture")
          }
        }
        if (platform.cpu == Sixteen) {
          if (flags(LargeCode)) {
            log.warn("Large code model doesn't work correctly yet")
          }
        }
        if (platform.cpu != Sixteen) {
          if (flags(LargeCode)) {
            log.error("Cannot use large code model on architectures other than 65816")
          }
          if (flags(ReturnWordsViaAccumulator)) {
            log.error("Cannot return words via accumulator on non-65816 architecture")
          }
          if (flags(EmitNative65816Opcodes) || flags(EmitEmulation65816Opcodes)) {
            log.error("65816 opcodes enabled for non-65816 architecture")
          }
        }
        if (platform.cpu != CE02) {
          if (flags(Emit65CE02Opcodes)) {
            log.error("65CE02 opcodes enabled for non-65CE02 architecture")
          }
        }
        if (flags(Emit65CE02Opcodes)) {
          log.warn("65CE02 opcodes are highly experimental")
        }
        if (platform.cpu != HuC6280) {
          if (flags(EmitHudsonOpcodes)) {
            log.error("HuC6280 opcodes enabled for non-HuC6280 architecture")
          }
        }
        if (!CmosCompatible(platform.cpu)) {
          if (!flags(PreventJmpIndirectBug)) {
            log.warn("JMP bug prevention should be enabled for non-CMOS architecture")
          }
          if (flags(EmitCmosOpcodes)) {
            log.error("CMOS opcodes enabled for non-CMOS architecture")
          }
        }
        if (flags(EmitIllegals)) {
          if (CmosCompatible(platform.cpu)) {
            log.error("Illegal opcodes enabled for architecture that doesn't support them")
          }
          if (platform.cpu == StrictRicoh || platform.cpu == StrictMos) {
            log.warn("Illegal opcodes enabled for strict architecture")
          }
        }
      case CpuFamily.I80 =>
        if (flags(EmitIllegals)) {
          if (platform.cpu != Z80) {
            log.error("Illegal opcodes enabled for architecture that doesn't support them")
          }
        }
        if (flags(UseIxForStack) || flags(UseIxForScratch)) {
          if (platform.cpu != Z80) {
            log.error("IX register enabled for architecture that doesn't support it")
          } else if (!flags(EmitZ80Opcodes)) {
            log.error("IX register is enabled but instructions using it are disabled")
          }
        }
        if (flags(UseIyForStack) || flags(UseIyForScratch)) {
          if (platform.cpu != Z80) {
            log.error("IY register enabled for architecture that doesn't support it")
          } else if (!flags(EmitZ80Opcodes)) {
            log.error("IY register is enabled but instructions using it are disabled")
          }
        }
        if (flags(UseIxForScratch) && flags(UseIxForStack)) {
          log.error("Cannot use the IX register for both stack variables and scratch simultaneously")
        }
        if (flags(UseIyForScratch) && flags(UseIyForStack)) {
          log.error("Cannot use the IY register for both stack variables and scratch simultaneously")
        }
        if (flags(UseIxForStack) && flags(UseIyForStack)) {
          log.error("Cannot use both IX and IY registers for stack variables simultaneously")
        }
        if (flags(UseShadowRegistersForInterrupts)) {
          if (platform.cpu != Z80) {
            log.error("Shadow registers enabled for architecture that doesn't support them")
          } else if (!flags(EmitZ80Opcodes)) {
            log.error("Shadow registers are enabled but instructions using them are disabled")
          }
        }
        if (flags(EmitZ80Opcodes)) {
          if (platform.cpu != Z80 && platform.cpu != EZ80) {
            log.error("Z80 opcodes enabled for architecture that doesn't support them")
          }
        }
        if (flags(EmitEZ80Opcodes)) {
          if (platform.cpu != Z80 && platform.cpu != EZ80) {
            log.error("eZ80 opcodes enabled for architecture that doesn't support them")
          }
        }
        if (flags(EmitSharpOpcodes)) {
          if (platform.cpu != Sharp) {
            log.error("Sharp LR35902 opcodes enabled for architecture that doesn't support them")
          }
        }
        if (flags(EmitExtended80Opcodes)) {
          if (platform.cpu != Sharp && platform.cpu != Z80 && platform.cpu != EZ80) {
            log.error("Extended 8080-like opcodes enabled for architecture that doesn't support them")
          }
        }
        if (flags(EmitIntel8080Opcodes)) {
          if (platform.cpu != Intel8080 && platform.cpu != Z80 && platform.cpu != EZ80) {
            log.error("Intel 8080 opcodes enabled for architecture that doesn't support them")
          }
        }
    }
  }

  def features: Map[String, Long] = {
    @inline
    def toLong(b: Boolean): Long = if (b) 1L else 0L
    val featuresFromOptions = Map[String, Long](
      "OPTIMIZE_FOR_SIZE" -> toLong(flag(CompilationFlag.OptimizeForSize)),
      "OPTIMIZE_FOR_SPEED" -> toLong(flag(CompilationFlag.OptimizeForSpeed)),
      "OPTIMIZE_INLINE" -> toLong(flag(CompilationFlag.InlineFunctions)),
      "OPTIMIZE_IPO" -> toLong(flag(CompilationFlag.InterproceduralOptimization)),
      "CPUFEATURE_DECIMAL_MODE" -> toLong(flag(CompilationFlag.DecimalMode)),
      "CPUFEATURE_Z80" -> toLong(flag(CompilationFlag.EmitZ80Opcodes)),
      "CPUFEATURE_EZ80" -> toLong(flag(CompilationFlag.EmitEZ80Opcodes)),
      "CPUFEATURE_8080" -> toLong(flag(CompilationFlag.EmitIntel8080Opcodes)),
      "CPUFEATURE_GAMEBOY" -> toLong(flag(CompilationFlag.EmitSharpOpcodes)),
      "CPUFEATURE_65C02" -> toLong(flag(CompilationFlag.EmitCmosOpcodes)),
      "CPUFEATURE_65CE02" -> toLong(flag(CompilationFlag.Emit65CE02Opcodes)),
      "CPUFEATURE_HUC6280" -> toLong(flag(CompilationFlag.EmitHudsonOpcodes)),
      "CPUFEATURE_65816_EMULATION" -> toLong(flag(CompilationFlag.EmitEmulation65816Opcodes)),
      "CPUFEATURE_65816_NATIVE" -> toLong(flag(CompilationFlag.EmitNative65816Opcodes)),
      "CPUFEATURE_6502_ILLEGALS" -> toLong(platform.cpuFamily == CpuFamily.M6502 && flag(CompilationFlag.EmitIllegals)),
      "CPUFEATURE_Z80_ILLEGALS" -> toLong(platform.cpuFamily == CpuFamily.I80 && flag(CompilationFlag.EmitIllegals)),
      "SYNTAX_INTEL" -> toLong(platform.cpuFamily == CpuFamily.I80 && flag(CompilationFlag.UseIntelSyntaxForInput)),
      "SYNTAX_ZILOG" -> toLong(platform.cpuFamily == CpuFamily.I80 && !flag(CompilationFlag.UseIntelSyntaxForInput)),
      "USES_ZPREG" -> toLong(platform.cpuFamily == CpuFamily.M6502 && zpRegisterSize > 0),
      "USES_IX_STACK" -> toLong(flag(CompilationFlag.UseIxForStack)),
      "USES_IY_STACK" -> toLong(flag(CompilationFlag.UseIyForStack)),
      "USES_SHADOW_REGISTERS" -> toLong(flag(CompilationFlag.UseShadowRegistersForInterrupts)),
      "ZPREG_SIZE" -> (if (platform.cpuFamily == CpuFamily.M6502) zpRegisterSize.toLong else 0)
    )
    platform.features ++ featuresFromOptions ++ featureOverrides
  }

}

object CpuFamily extends Enumeration {
  val M6502, I80, M6800, I86, M68K, ARM = Value

  def forType(cpu: Cpu.Value): CpuFamily.Value = {
    import Cpu._
    cpu match {
      case Mos | StrictMos | Ricoh | StrictRicoh | Cmos | HuC6280 | CE02 | Sixteen => M6502
      case Intel8080 | Sharp | Z80 | EZ80 => I80
    }
  }
}

object Cpu extends Enumeration {

  val Mos, StrictMos, Ricoh, StrictRicoh, Cmos, HuC6280, CE02, Sixteen, Intel8080, Z80, EZ80, Sharp = Value

  val CmosCompatible = Set(Cmos, HuC6280, CE02, Sixteen)

  import CompilationFlag._

  private val mosAlwaysDefaultFlags = Set(
    VariableOverlap, CompactReturnDispatchParams
  )

  private val i80AlwaysDefaultFlags = Set(
    VariableOverlap, CompactReturnDispatchParams
  )

  def defaultFlags(x: Cpu.Value): Set[CompilationFlag.Value] = x match {
    case StrictMos =>
      mosAlwaysDefaultFlags ++ Set(DecimalMode, PreventJmpIndirectBug)
    case Mos =>
      mosAlwaysDefaultFlags ++ Set(DecimalMode, PreventJmpIndirectBug)
    case Ricoh =>
      mosAlwaysDefaultFlags ++ Set(PreventJmpIndirectBug)
    case StrictRicoh =>
      mosAlwaysDefaultFlags ++ Set(PreventJmpIndirectBug)
    case Cmos =>
      mosAlwaysDefaultFlags ++ Set(DecimalMode, EmitCmosOpcodes)
    case HuC6280 =>
      mosAlwaysDefaultFlags ++ Set(DecimalMode, EmitCmosOpcodes, EmitHudsonOpcodes)
    case CE02 =>
      mosAlwaysDefaultFlags ++ Set(DecimalMode, EmitCmosOpcodes, Emit65CE02Opcodes)
    case Sixteen =>
      mosAlwaysDefaultFlags ++ Set(DecimalMode, EmitCmosOpcodes, EmitEmulation65816Opcodes, EmitNative65816Opcodes, ReturnWordsViaAccumulator)
    case Intel8080 =>
      i80AlwaysDefaultFlags ++ Set(EmitIntel8080Opcodes, UseIntelSyntaxForInput, UseIntelSyntaxForOutput)
    case Z80 =>
      i80AlwaysDefaultFlags ++ Set(EmitIntel8080Opcodes, EmitExtended80Opcodes, EmitZ80Opcodes, UseIxForStack, UseShadowRegistersForInterrupts)
    case EZ80 =>
      i80AlwaysDefaultFlags ++ Set(EmitIntel8080Opcodes, EmitExtended80Opcodes, EmitZ80Opcodes, UseIxForStack, UseShadowRegistersForInterrupts, EmitEZ80Opcodes)
    case Sharp =>
      i80AlwaysDefaultFlags ++ Set(EmitExtended80Opcodes, EmitSharpOpcodes)
  }

  def fromString(name: String)(implicit log: Logger): Cpu.Value = name match {
    case "nmos" => Mos
    case "6502" => Mos
    case "6510" => Mos
    case "strict" => StrictMos
    case "strictnmos" => StrictMos
    case "strict6502" => StrictMos
    case "strict6510" => StrictMos
    case "cmos" => Cmos
    case "65sc02" => Cmos
    case "sc02" => Cmos
    case "65c02" => Cmos
    case "c02" => Cmos
    case "hudson" => HuC6280
    case "huc6280" => HuC6280
    case "c6280" => HuC6280
    case "6280" => HuC6280
    case "65ce02" => CE02
    case "ce02" => CE02
    case "65816" => Sixteen
    case "816" => Sixteen
    case "ricoh" => Ricoh
    case "2a03" => Ricoh
    case "2a07" => Ricoh
    case "strictricoh" => StrictRicoh
    case "strict2a03" => StrictRicoh
    case "strict2a07" => StrictRicoh
    case "z80" => Z80
      // disabled for now:
//    case "ez80" => EZ80
    case "gameboy"  => Sharp
    case "gb"  => Sharp
    case "sharp"  => Sharp
    case "lr35902"  => Sharp
    case "8080" => Intel8080
    case "i8080" => Intel8080
    case "intel8080" => Intel8080
    case _ => log.fatal("Unknown CPU achitecture: " + name)
  }

  def getMaxSizeReturnableViaRegisters(cpu: Cpu.Value, compilationOptions: CompilationOptions): Int =
    CpuFamily.forType(cpu) match {
      case CpuFamily.M6502 => 2
      case CpuFamily.I80 | CpuFamily.I86 => 4
      case _ => ???
    }
}

object CompilationFlag extends Enumeration {
  val
  // common compilation options:
  EmitIllegals, DecimalMode, ReadOnlyArrays, LenientTextEncoding,
  // compilation options for MOS:
  EmitCmosOpcodes, EmitCmosNopOpcodes, EmitHudsonOpcodes, Emit65CE02Opcodes, EmitEmulation65816Opcodes, EmitNative65816Opcodes,
  PreventJmpIndirectBug, LargeCode, ReturnWordsViaAccumulator,
  // compilation options for I80
  EmitIntel8080Opcodes, EmitExtended80Opcodes, EmitZ80Opcodes, EmitEZ80Opcodes, EmitSharpOpcodes,
  UseShadowRegistersForInterrupts,
  UseIxForStack, UseIyForStack,
  UseIxForScratch, UseIyForScratch,
  UseIntelSyntaxForInput,
  UseIntelSyntaxForOutput,
  // optimization options:
  DangerousOptimizations, InlineFunctions, InterproceduralOptimization, OptimizeForSize, OptimizeForSpeed, OptimizeForSonicSpeed,
  // memory allocation options
  VariableOverlap, CompactReturnDispatchParams, LUnixRelocatableCode,
  // runtime check options
  CheckIndexOutOfBounds,
  // special options
  SingleThreaded,
  // warning options
  ExtraComparisonWarnings,
  RorWarning,
  NonZeroTerminatedLiteralWarning,
  FatalWarnings,
  // special options for internal compiler use
  InternalCurrentlyOptimizingForMeasurement = Value

  val allWarnings: Set[CompilationFlag.Value] = Set(ExtraComparisonWarnings)

  val fromString = Map(
    "lunix" -> LUnixRelocatableCode,
    "emit_illegals" -> EmitIllegals,
    "emit_cmos" -> EmitCmosOpcodes,
    "emit_65ce02" -> Emit65CE02Opcodes,
    "emit_huc6280" -> EmitHudsonOpcodes,
    "emit_z80" -> EmitZ80Opcodes,
    "emit_ez80" -> EmitEZ80Opcodes,
    "emit_x80" -> EmitExtended80Opcodes,
    "emit_8080" -> EmitIntel8080Opcodes,
    "emit_sharp" -> EmitSharpOpcodes,
    "ix_stack" -> UseIxForStack,
    "iy_stack" -> UseIyForStack,
    "ix_scratch" -> UseIxForScratch,
    "iy_scratch" -> UseIyForScratch,
    "use_shadow_registers_for_irq" -> UseShadowRegistersForInterrupts,
    "output_intel_syntax" -> UseIntelSyntaxForOutput,
    "input_intel_syntax" -> UseIntelSyntaxForInput,
    "ipo" -> InterproceduralOptimization,
    "inline" -> InlineFunctions,
    "dangerous_optimizations" -> DangerousOptimizations,
    "decimal_mode" -> DecimalMode,
    "ro_arrays" -> ReadOnlyArrays,
    "ror_warn" -> RorWarning,
    "prevent_jmp_indirect_bug" -> PreventJmpIndirectBug,
    "compact_dispatch_params" -> CompactReturnDispatchParams,
    "lenient_encoding" -> LenientTextEncoding,
  )

}