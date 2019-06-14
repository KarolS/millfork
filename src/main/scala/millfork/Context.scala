package millfork

import millfork.error.Logger

/**
  * @author Karol Stasiak
  */

case class Context(errorReporting: Logger,
                   inputFileNames: List[String],
                   outputFileName: Option[String] = None,
                   runFileName: Option[String] = None,
                   optimizationLevel: Option[Int] = None,
                   zpRegisterSize: Option[Int] = None,
                   platform: Option[String] = None,
                   outputAssembly: Boolean = false,
                   outputLabels: Boolean = false,
                   outputLabelsFormatOverride: Option[DebugOutputFormat] = None,
                   includePath: List[String] = Nil,
                   extraIncludePath: Seq[String] = IndexedSeq(),
                   flags: Map[CompilationFlag.Value, Boolean] = Map(),
                   features: Map[String, Long] = Map(),
                   verbosity: Option[Int] = None) {
  def changeFlag(f: CompilationFlag.Value, b: Boolean): Context = {
    if (flags.contains(f)) {
      if (flags(f) != b) {
        errorReporting.error("Conflicting flags")
      }
      this
    } else {
      copy(flags = this.flags + (f -> b))
    }
  }

  def filloutFlags(): Context = {
    var addons = Map[CompilationFlag.Value, Boolean]()
    if (flags.contains(CompilationFlag.EmitNative65816Opcodes)
      || flags.contains(CompilationFlag.EmitEmulation65816Opcodes)
      || flags.contains(CompilationFlag.EmitHudsonOpcodes)
      || flags.contains(CompilationFlag.Emit65CE02Opcodes)) {
      addons += CompilationFlag.EmitCmosOpcodes -> true
    }
    if (flags.contains(CompilationFlag.EmitEZ80Opcodes)) {
      addons += CompilationFlag.EmitZ80Opcodes -> true
    }
    if (flags.contains(CompilationFlag.EmitZ80Opcodes) || flags.contains(CompilationFlag.EmitSharpOpcodes)) {
      addons += CompilationFlag.EmitExtended80Opcodes -> true
    }
    if (flags.contains(CompilationFlag.EmitZ80Opcodes) || flags.contains(CompilationFlag.EmitIntel8085Opcodes)) {
      addons += CompilationFlag.EmitIntel8080Opcodes -> true
    }
    if (flags.contains(CompilationFlag.OptimizeForSpeed)) {
      addons += CompilationFlag.InlineFunctions -> true
    }
    if (flags.contains(CompilationFlag.OptimizeForSize)) {
      addons += CompilationFlag.SubroutineExtraction -> true
    }
    if (flags.contains(CompilationFlag.DangerousOptimizations)) {
      addons += CompilationFlag.InterproceduralOptimization -> true
      addons += CompilationFlag.OptimizeStdlib -> true
    }
    if (flags.contains(CompilationFlag.OptimizeForDebugging)) {
      addons += CompilationFlag.VariableOverlap -> false
      addons += CompilationFlag.RegisterVariables -> false
      addons += CompilationFlag.FunctionDeduplication -> false
      addons += CompilationFlag.SubroutineExtraction -> false
      addons += CompilationFlag.FunctionFallthrough -> false
      addons += CompilationFlag.OptimizeStdlib -> false
    }
    copy(flags = flags ++ addons.filterKeys(k => !flags.contains(k)))
  }
}
