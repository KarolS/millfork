package millfork

import millfork.error.ErrorReporting

/**
  * @author Karol Stasiak
  */
case class CompilationOptions(platform: Platform, commandLineFlags: Map[CompilationFlag.Value, Boolean], outputFileName: Option[String], zpRegisterSize: Int) {

  import CompilationFlag._
  import Cpu._

  val flags: Map[CompilationFlag.Value, Boolean] = CompilationFlag.values.map { f =>
    f -> commandLineFlags.getOrElse(f, platform.flagOverrides.getOrElse(f, Cpu.defaultFlags(platform.cpu)(f)))
  }.toMap

  def flag(f: CompilationFlag.Value) = flags(f)

  {
    var invalids = Set[CompilationFlag.Value]()

    if (CpuFamily.forType(platform.cpu) != CpuFamily.M6502) invalids ++= Set(
      EmitCmosOpcodes, EmitCmosNopOpcodes, EmitHudsonOpcodes, Emit65CE02Opcodes, EmitEmulation65816Opcodes, EmitNative65816Opcodes,
      PreventJmpIndirectBug, LargeCode, ReturnWordsViaAccumulator, LUnixRelocatableCode, RorWarning)

    if (CpuFamily.forType(platform.cpu) != CpuFamily.I80) invalids ++= Set(EmitExtended80Opcodes, EmitZ80Opcodes, EmitSharpOpcodes, UseIxForStack)

    invalids = invalids.filter(flags)

    if (invalids.nonEmpty) {
      ErrorReporting.error("Invalid flags enabled for the currect CPU family: " + invalids.mkString(", "))
    }
    if (CpuFamily.forType(platform.cpu) != CpuFamily.M6502 && zpRegisterSize > 0) {
      ErrorReporting.error("Invalid flags enabled for the currect CPU family: zp_register" + invalids.mkString(", "))
    }
    CpuFamily.forType(platform.cpu) match {
      case CpuFamily.M6502 =>
        if (flags(DecimalMode)) {
          if (platform.cpu == Ricoh || platform.cpu == StrictRicoh) {
            ErrorReporting.warn("Decimal mode enabled for Ricoh architecture", this)
          }
        }
        if (platform.cpu == Sixteen) {
          if (flags(LargeCode)) {
            ErrorReporting.warn("Large code model doesn't work correctly yet", this)
          }
        }
        if (platform.cpu != Sixteen) {
          if (flags(LargeCode)) {
            ErrorReporting.error("Cannot use large code model on architectures other than 65816")
          }
          if (flags(ReturnWordsViaAccumulator)) {
            ErrorReporting.error("Cannot return words via accumulator on non-65816 architecture")
          }
          if (flags(EmitNative65816Opcodes) || flags(EmitEmulation65816Opcodes)) {
            ErrorReporting.error("65816 opcodes enabled for non-65816 architecture")
          }
        }
        if (platform.cpu != CE02) {
          if (flags(Emit65CE02Opcodes)) {
            ErrorReporting.error("65CE02 opcodes enabled for non-65CE02 architecture")
          }
        }
        if (flags(Emit65CE02Opcodes)) {
          ErrorReporting.warn("65CE02 opcodes are highly experimental", this)
        }
        if (platform.cpu != HuC6280) {
          if (flags(EmitHudsonOpcodes)) {
            ErrorReporting.error("HuC6280 opcodes enabled for non-HuC6280 architecture")
          }
        }
        if (!CmosCompatible(platform.cpu)) {
          if (!flags(PreventJmpIndirectBug)) {
            ErrorReporting.warn("JMP bug prevention should be enabled for non-CMOS architecture", this)
          }
          if (flags(EmitCmosOpcodes)) {
            ErrorReporting.error("CMOS opcodes enabled for non-CMOS architecture")
          }
        }
        if (flags(EmitIllegals)) {
          if (CmosCompatible(platform.cpu)) {
            ErrorReporting.error("Illegal opcodes enabled for architecture that doesn't support them")
          }
          if (platform.cpu == StrictRicoh || platform.cpu == StrictMos) {
            ErrorReporting.warn("Illegal opcodes enabled for strict architecture", this)
          }
        }
      case CpuFamily.I80 =>
        if (flags(EmitIllegals)) {
          if (platform.cpu != Z80) {
            ErrorReporting.error("Illegal opcodes enabled for architecture that doesn't support them")
          }
        }
        if (flags(UseIxForStack)) {
          if (platform.cpu != Z80) {
            ErrorReporting.error("IX register enabled for architecture that doesn't support it")
          }
        }
        if (flags(EmitZ80Opcodes)) {
          if (platform.cpu != Z80) {
            ErrorReporting.error("Z80 opcodes enabled for architecture that doesn't support them")
          }
        }
        if (flags(EmitSharpOpcodes)) {
          if (platform.cpu != Sharp) {
            ErrorReporting.error("Sharp LR35902 opcodes enabled for architecture that doesn't support them")
          }
        }
        if (flags(EmitExtended80Opcodes)) {
          if (platform.cpu != Sharp && platform.cpu != Z80) {
            ErrorReporting.error("Extended 8080-like opcodes enabled for architecture that doesn't support them")
          }
        }
    }
  }
}

object CpuFamily extends Enumeration {
  val M6502, I80, M6809, I8086, M65K, ARM = Value

  def forType(cpu: Cpu.Value): CpuFamily.Value = {
    import Cpu._
    cpu match {
      case Mos | StrictMos | Ricoh | StrictRicoh | Cmos | HuC6280 | CE02 | Sixteen => M6502
      case Intel8080 | Sharp | Z80 => I80
    }
  }
}

object Cpu extends Enumeration {

  val Mos, StrictMos, Ricoh, StrictRicoh, Cmos, HuC6280, CE02, Sixteen, Intel8080, Z80, Sharp = Value

  val CmosCompatible = Set(Cmos, HuC6280, CE02, Sixteen)

  import CompilationFlag._

  private val mosAlwaysDefaultFlags = Set(
    VariableOverlap, CompactReturnDispatchParams
  )

  private val i8080AlwaysDefaultFlags = Set(
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
      i8080AlwaysDefaultFlags
    case Z80 =>
      i8080AlwaysDefaultFlags ++ Set(EmitExtended80Opcodes, EmitZ80Opcodes, UseIxForStack)
    case Sharp =>
      i8080AlwaysDefaultFlags ++ Set(EmitExtended80Opcodes, EmitSharpOpcodes)
  }

  def fromString(name: String): Cpu.Value = name match {
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
    case _ => ErrorReporting.fatal("Unknown CPU achitecture: " + name)
  }
}

object CompilationFlag extends Enumeration {
  val
  // common compilation options:
  EmitIllegals, DecimalMode, ReadOnlyArrays,
  // compilation options for MOS:
  EmitCmosOpcodes, EmitCmosNopOpcodes, EmitHudsonOpcodes, Emit65CE02Opcodes, EmitEmulation65816Opcodes, EmitNative65816Opcodes,
  PreventJmpIndirectBug, LargeCode, ReturnWordsViaAccumulator,
  // compilation options for Z80
  EmitExtended80Opcodes, EmitZ80Opcodes, EmitSharpOpcodes, UseIxForStack,
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
    "emit_x80" -> EmitExtended80Opcodes,
    "emit_sharp" -> EmitSharpOpcodes,
    "ix_stack" -> UseIxForStack,
    "ipo" -> InterproceduralOptimization,
    "inline" -> InlineFunctions,
    "dangerous_optimizations" -> DangerousOptimizations,
    "decimal_mode" -> DecimalMode,
    "ro_arrays" -> ReadOnlyArrays,
    "ror_warn" -> RorWarning,
    "prevent_jmp_indirect_bug" -> PreventJmpIndirectBug,
    "compact_dispatch_params" -> CompactReturnDispatchParams,
  )

}