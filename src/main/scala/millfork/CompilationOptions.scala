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
  if (platform.cpu != Cmos) {
    if (!flags(PreventJmpIndirectBug)) {
      ErrorReporting.warn("JMP bug prevention should be enabled for non-CMOS architecture", this)
    }
    if (flags(EmitCmosOpcodes)) {
      ErrorReporting.warn("CMOS opcodes enabled for non-CMOS architecture", this)
    }
  }
  if (flags(EmitIllegals)) {
    if (platform.cpu == Cmos) {
      ErrorReporting.warn("Illegal opcodes enabled for CMOS architecture", this)
    }
    if (platform.cpu == StrictRicoh || platform.cpu == Ricoh) {
      ErrorReporting.warn("Illegal opcodes enabled for strict architecture", this)
    }
  }
}

object Cpu extends Enumeration {

  val Mos, StrictMos, Ricoh, StrictRicoh, Cmos = Value

  import CompilationFlag._

  def defaultFlags(x: Cpu.Value): Set[CompilationFlag.Value] = x match {
    case StrictMos => Set(DecimalMode, PreventJmpIndirectBug, VariableOverlap, CompactReturnDispatchParams)
    case Mos => Set(DecimalMode, PreventJmpIndirectBug, VariableOverlap, CompactReturnDispatchParams)
    case Ricoh => Set(PreventJmpIndirectBug, VariableOverlap, CompactReturnDispatchParams)
    case StrictRicoh => Set(PreventJmpIndirectBug, VariableOverlap, CompactReturnDispatchParams)
    case Cmos => Set(EmitCmosOpcodes, VariableOverlap, CompactReturnDispatchParams)
  }

  def fromString(name: String): Cpu.Value = name match {
    case "nmos" => Mos
    case "6502" => Mos
    case "6510" => Mos
    case "strict" => StrictMos
    case "cmos" => Cmos
    case "65c02" => Cmos
    case "ricoh" => Ricoh
    case "2a03" => Ricoh
    case "2a07" => Ricoh
    case "strictricoh" => StrictRicoh
    case "strict2a03" => StrictRicoh
    case "strict2a07" => StrictRicoh
    case _ => ErrorReporting.fatal("Unknown CPU achitecture")
  }
}

object CompilationFlag extends Enumeration {
  val
  // compilation options:
  EmitIllegals, EmitCmosOpcodes, DecimalMode, ReadOnlyArrays, PreventJmpIndirectBug,
  // optimization options:
  DetailedFlowAnalysis, DangerousOptimizations, InlineFunctions,
  // memory allocation options
  VariableOverlap, CompactReturnDispatchParams,
  // runtime check options
  CheckIndexOutOfBounds,
  // warning options
  ExtraComparisonWarnings,
  RorWarning,
  FatalWarnings = Value

  val allWarnings: Set[CompilationFlag.Value] = Set(ExtraComparisonWarnings)

  val fromString = Map(
    "emit_illegals" -> EmitIllegals,
    "emit_cmos" -> EmitCmosOpcodes,
    "decimal_mode" -> DecimalMode,
    "ro_arrays" -> ReadOnlyArrays,
    "ror_warn" -> RorWarning,
    "prevent_jmp_indirect_bug" -> PreventJmpIndirectBug,
    "compact_dispatch_params" -> CompactReturnDispatchParams,
  )

}