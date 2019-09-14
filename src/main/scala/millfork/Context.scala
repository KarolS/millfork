package millfork

import millfork.error.Logger

/**
  * @author Karol Stasiak
  */

case class Context(errorReporting: Logger,
                   inputFileNames: List[String],
                   outputFileName: Option[String] = None,
                   runFileName: Option[String] = None,
                   runParams: Seq[String] = Vector(),
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
  
  def isFlagSet(f: CompilationFlag.Value): Boolean = flags.getOrElse(f, false)

  def filloutFlags(): Context = {
    var addons = Map[CompilationFlag.Value, Boolean]()
    if (isFlagSet(CompilationFlag.EmitNative65816Opcodes)
      || isFlagSet(CompilationFlag.EmitEmulation65816Opcodes)
      || isFlagSet(CompilationFlag.EmitHudsonOpcodes)
      || isFlagSet(CompilationFlag.Emit65CE02Opcodes)) {
      addons += CompilationFlag.EmitCmosOpcodes -> true
    }
    if (isFlagSet(CompilationFlag.EmitEZ80Opcodes)) {
      addons += CompilationFlag.EmitZ80Opcodes -> true
    }
    if (isFlagSet(CompilationFlag.EmitZ80Opcodes) || isFlagSet(CompilationFlag.EmitSharpOpcodes)) {
      addons += CompilationFlag.EmitExtended80Opcodes -> true
    }
    if (isFlagSet(CompilationFlag.EmitZ80Opcodes) || isFlagSet(CompilationFlag.EmitIntel8085Opcodes)) {
      addons += CompilationFlag.EmitIntel8080Opcodes -> true
    }
    if (isFlagSet(CompilationFlag.OptimizeForSpeed)) {
      addons += CompilationFlag.InlineFunctions -> true
    }
    if (isFlagSet(CompilationFlag.DangerousOptimizations)) {
      addons += CompilationFlag.InterproceduralOptimization -> true
      addons += CompilationFlag.OptimizeStdlib -> true
    }
    if (isFlagSet(CompilationFlag.OptimizeForDebugging)) {
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
