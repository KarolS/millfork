package millfork

import millfork.error.ErrorReporting

/**
  * @author Karol Stasiak
  */
case class CompilationOptions(platform: Platform, commandLineFlags: Map[CompilationFlag.Value, Boolean]) {

  import CompilationFlag._
  import Cpu._

  val flags: Map[CompilationFlag.Value, Boolean] = CompilationFlag.values.map { f =>
    f -> commandLineFlags.getOrElse(f, platform.flagOverrides.getOrElse(f, Cpu.defaultFlags(platform.cpu)(f)))
  }.toMap

  def flag(f: CompilationFlag.Value) = flags(f)

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
}

object Cpu extends Enumeration {

  val Mos, StrictMos, Ricoh, StrictRicoh, Cmos, HuC6280, CE02, Sixteen = Value

  val CmosCompatible = Set(Cmos, HuC6280, CE02, Sixteen)

  import CompilationFlag._

  private val alwaysDefaultFlags = Set(
    VariableOverlap, CompactReturnDispatchParams, ZeropagePseudoregister
  )

  def defaultFlags(x: Cpu.Value): Set[CompilationFlag.Value] = x match {
    case StrictMos =>
      alwaysDefaultFlags ++ Set(DecimalMode, PreventJmpIndirectBug)
    case Mos =>
      alwaysDefaultFlags ++ Set(DecimalMode, PreventJmpIndirectBug)
    case Ricoh =>
      alwaysDefaultFlags ++ Set(PreventJmpIndirectBug)
    case StrictRicoh =>
      alwaysDefaultFlags ++ Set(PreventJmpIndirectBug)
    case Cmos =>
      alwaysDefaultFlags ++ Set(DecimalMode, EmitCmosOpcodes)
    case HuC6280 =>
      alwaysDefaultFlags ++ Set(DecimalMode, EmitCmosOpcodes, EmitHudsonOpcodes)
    case CE02 =>
      alwaysDefaultFlags ++ Set(DecimalMode, EmitCmosOpcodes, Emit65CE02Opcodes)
    case Sixteen =>
      alwaysDefaultFlags ++ Set(DecimalMode, EmitCmosOpcodes, EmitEmulation65816Opcodes, EmitNative65816Opcodes, ReturnWordsViaAccumulator)
  }

  def fromString(name: String): Cpu.Value = name match {
    case "nmos" => Mos
    case "6502" => Mos
    case "6510" => Mos
    case "strict" => StrictMos
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
    case _ => ErrorReporting.fatal("Unknown CPU achitecture: " + name)
  }
}

object CompilationFlag extends Enumeration {
  val
  // compilation options:
  EmitIllegals, EmitCmosOpcodes, EmitCmosNopOpcodes, EmitHudsonOpcodes, Emit65CE02Opcodes, EmitEmulation65816Opcodes, EmitNative65816Opcodes,
  ZeropagePseudoregister, DecimalMode, ReadOnlyArrays, PreventJmpIndirectBug, LargeCode, ReturnWordsViaAccumulator,
  // optimization options:
  DangerousOptimizations, InlineFunctions, OptimizeForSize, OptimizeForSpeed, OptimizeForSonicSpeed,
  // memory allocation options
  VariableOverlap, CompactReturnDispatchParams,
  // runtime check options
  CheckIndexOutOfBounds,
  // special options
  SingleThreaded,
  // warning options
  ExtraComparisonWarnings,
  RorWarning,
  FatalWarnings = Value

  val allWarnings: Set[CompilationFlag.Value] = Set(ExtraComparisonWarnings)

  val fromString = Map(
    "emit_illegals" -> EmitIllegals,
    "emit_cmos" -> EmitCmosOpcodes,
    "emit_65ce02" -> Emit65CE02Opcodes,
    "emit_huc6280" -> EmitHudsonOpcodes,
    "zeropage_register" -> ZeropagePseudoregister,
    "decimal_mode" -> DecimalMode,
    "ro_arrays" -> ReadOnlyArrays,
    "ror_warn" -> RorWarning,
    "prevent_jmp_indirect_bug" -> PreventJmpIndirectBug,
    "compact_dispatch_params" -> CompactReturnDispatchParams,
  )

}