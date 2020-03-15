package millfork

import millfork.buildinfo.BuildInfo
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
  @inline
  def isBigEndian: Boolean = platform.isBigEndian

  val flags: Map[CompilationFlag.Value, Boolean] = CompilationFlag.values.map { f =>
    f -> commandLineFlags.getOrElse(f, platform.flagOverrides.getOrElse(f, Cpu.defaultFlags(platform.cpu)(f)))
  }.toMap

  def flag(f: CompilationFlag.Value): Boolean = flags(f)

  {
    log.setFatalWarnings(flags(CompilationFlag.FatalWarnings))

    var invalids = Set[CompilationFlag.Value]()

    if (CpuFamily.forType(platform.cpu) != CpuFamily.M6502) invalids ++= Set(
      EmitCmosOpcodes, EmitCmosNopOpcodes, EmitHudsonOpcodes, EmitSC02Opcodes, EmitRockwellOpcodes, EmitWdcOpcodes, Emit65CE02Opcodes, EmitEmulation65816Opcodes, EmitNative65816Opcodes, LUnixRelocatableCode,
      PreventJmpIndirectBug, LargeCode, ReturnWordsViaAccumulator, LUnixRelocatableCode, RorWarning, SoftwareStack)

    if (CpuFamily.forType(platform.cpu) != CpuFamily.I80 && CpuFamily.forType(platform.cpu) != CpuFamily.I86) invalids ++= Set(
      EmitIntel8085Opcodes, EmitIntel8080Opcodes, UseIxForStack, UseIntelSyntaxForInput, UseIntelSyntaxForOutput)

    if (CpuFamily.forType(platform.cpu) != CpuFamily.I80) invalids ++= Set(
      EmitExtended80Opcodes, EmitZ80Opcodes, EmitSharpOpcodes, EmitEZ80Opcodes, EmitZ80NextOpcodes,
      UseIyForStack, UseIxForScratch, UseIyForScratch, UseShadowRegistersForInterrupts)

    if (CpuFamily.forType(platform.cpu) != CpuFamily.M6809) invalids ++= Set(
      UseUForStack, UseYForStack)

    invalids = invalids.filter(flags)

    if (invalids.nonEmpty) {
      log.error("Invalid flags enabled for the current CPU family: " + invalids.mkString(", "))
    }
    if (CpuFamily.forType(platform.cpu) != CpuFamily.M6502 && zpRegisterSize > 0) {
      log.error("Invalid flags enabled for the current CPU family: zp_register")
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
          if (platform.cpu != Z80 && platform.cpu != Intel8085 && platform.cpu != Z80Next) {
            log.error("Illegal opcodes enabled for architecture that doesn't support them")
          }
        }
        if (flags(UseIxForStack) || flags(UseIxForScratch)) {
          if (!Z80Compatible(platform.cpu)) {
            log.error("IX register enabled for architecture that doesn't support it")
          } else if (!flags(EmitZ80Opcodes)) {
            log.error("IX register is enabled but instructions using it are disabled")
          }
        }
        if (flags(UseIyForStack) || flags(UseIyForScratch)) {
          if (!Z80Compatible(platform.cpu)) {
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
          if (!Z80Compatible(platform.cpu)) {
            log.error("Shadow registers enabled for architecture that doesn't support them")
          } else if (!flags(EmitZ80Opcodes)) {
            log.error("Shadow registers are enabled but instructions using them are disabled")
          }
        }
        if (flags(EmitZ80Opcodes)) {
          if (!Z80Compatible(platform.cpu)) {
            log.error("Z80 opcodes enabled for architecture that doesn't support them")
          }
        }
        if (flags(EmitEZ80Opcodes)) {
          if (platform.cpu != EZ80) {
            log.error("eZ80 opcodes enabled for architecture that doesn't support them")
          }
        }
        if (flags(EmitSharpOpcodes)) {
          if (platform.cpu != Sharp) {
            log.error("Sharp LR35902 opcodes enabled for architecture that doesn't support them")
          }
        }
        if (flags(EmitZ80NextOpcodes)) {
          if (platform.cpu != Z80Next) {
            log.error("ZX Spectrum Next opcodes enabled for architecture that doesn't support them")
          }
        }
        if (flags(EmitExtended80Opcodes)) {
          if (platform.cpu != Sharp && !Z80Compatible(platform.cpu)) {
            log.error("Extended 8080-like opcodes enabled for architecture that doesn't support them")
          }
        }
        if (flags(EmitIntel8080Opcodes)) {
          if (!Intel8080Compatible(platform.cpu)) {
            log.error("Intel 8080 opcodes enabled for architecture that doesn't support them")
          }
        }
        if (flags(EmitIntel8085Opcodes)) {
          if (!Intel8085Compatible(platform.cpu)) {
            log.error("Intel 8085 opcodes enabled for architecture that doesn't support them")
          }
        }
        if (!flags(CompilationFlag.EmitSharpOpcodes) && !flags(CompilationFlag.EmitIntel8080Opcodes)) {
          log.error("Either Sharp LR35902 or Intel 8080 opcodes have to be enabled")
        }
      case CpuFamily.I86 =>
      case CpuFamily.M6809 =>
        if (flags(UseUForStack) && flags(UseYForStack)) {
          log.error("Cannot use both U and Y registers for stack variables simultaneously")
        }
    }
  }

  private def parseVersion(): Long = {
    val tokens = BuildInfo.version.split("[.-]")

    def extract(ix: Int):Int = {
      tokens.lift(ix).filter(_ != "SNAPSHOT").getOrElse("0").toInt
    }

    val major = extract(0)
    val minor = extract(1)
    val patch = extract(2)
    major * 10000 + minor * 100 + patch
  }

  def features: Map[String, Long] = {
    @inline
    def toLong(b: Boolean): Long = if (b) 1L else 0L
    val featuresFromOptions = Map[String, Long](
      "MILLFORK_VERSION" -> parseVersion(),
      "OPTIMIZE_FOR_SIZE" -> toLong(flag(CompilationFlag.OptimizeForSize)),
      "OPTIMIZE_FOR_SPEED" -> toLong(flag(CompilationFlag.OptimizeForSpeed)),
      "OPTIMIZE_INLINE" -> toLong(flag(CompilationFlag.InlineFunctions)),
      "OPTIMIZE_IPO" -> toLong(flag(CompilationFlag.InterproceduralOptimization)),
      "CPUFEATURE_DECIMAL_MODE" -> toLong(flag(CompilationFlag.DecimalMode)),
      "CPUFEATURE_Z80" -> toLong(flag(CompilationFlag.EmitZ80Opcodes)),
      "CPUFEATURE_EZ80" -> toLong(flag(CompilationFlag.EmitEZ80Opcodes)),
      "CPUFEATURE_8080" -> toLong(flag(CompilationFlag.EmitIntel8080Opcodes)),
      "CPUFEATURE_8085" -> toLong(flag(CompilationFlag.EmitIntel8085Opcodes)),
      "CPUFEATURE_GAMEBOY" -> toLong(flag(CompilationFlag.EmitSharpOpcodes)),
      "CPUFEATURE_65C02" -> toLong(flag(CompilationFlag.EmitCmosOpcodes)),
      "CPUFEATURE_65SC02" -> toLong(flag(CompilationFlag.EmitSC02Opcodes)),
      "CPUFEATURE_ROCKWELL_65C02" -> toLong(flag(CompilationFlag.EmitRockwellOpcodes)),
      "CPUFEATURE_WDC_65C02" -> toLong(flag(CompilationFlag.EmitWdcOpcodes)),
      "CPUFEATURE_65CE02" -> toLong(flag(CompilationFlag.Emit65CE02Opcodes)),
      "CPUFEATURE_HUC6280" -> toLong(flag(CompilationFlag.EmitHudsonOpcodes)),
      "CPUFEATURE_65816_EMULATION" -> toLong(flag(CompilationFlag.EmitEmulation65816Opcodes)),
      "CPUFEATURE_65816_NATIVE" -> toLong(flag(CompilationFlag.EmitNative65816Opcodes)),
      "CPUFEATURE_6502_ILLEGALS" -> toLong(platform.cpuFamily == CpuFamily.M6502 && flag(CompilationFlag.EmitIllegals)),
      "CPUFEATURE_Z80_ILLEGALS" -> toLong(flag(CompilationFlag.EmitZ80Opcodes) && flag(CompilationFlag.EmitIllegals)),
      "CPUFEATURE_Z80_NEXT" -> toLong(flag(CompilationFlag.EmitZ80NextOpcodes)),
      "CPUFEATURE_8085_ILLEGALS" -> toLong(flag(CompilationFlag.EmitIntel8080Opcodes) && flag(CompilationFlag.EmitIllegals)),
      "BIG_ENDIAN" -> toLong(Cpu.isBigEndian(platform.cpu)),
      "LITTLE_ENDIAN" -> toLong(!Cpu.isBigEndian(platform.cpu)),
      "INIT_RW_MEMORY" -> toLong(platform.ramInitialValuesBank.isDefined),
      "SYNTAX_INTEL" -> toLong(platform.cpuFamily == CpuFamily.I80 && flag(CompilationFlag.UseIntelSyntaxForInput)),
      "SYNTAX_ZILOG" -> toLong(platform.cpuFamily == CpuFamily.I80 && !flag(CompilationFlag.UseIntelSyntaxForInput)),
      "TINY_RW_MEMORY" -> toLong(platform.variableAllocators("default").totalHimemSize <= 256),
      "USES_ZPREG" -> toLong(platform.cpuFamily == CpuFamily.M6502 && zpRegisterSize > 0),
      "USES_IX_STACK" -> toLong(flag(CompilationFlag.UseIxForStack)),
      "USES_IY_STACK" -> toLong(flag(CompilationFlag.UseIyForStack)),
      "USES_U_STACK" -> toLong(flag(CompilationFlag.UseUForStack)),
      "USES_Y_STACK" -> toLong(flag(CompilationFlag.UseYForStack)),
      "USES_SOFTWARE_STACK" -> toLong(flag(CompilationFlag.SoftwareStack)),
      "USES_SHADOW_REGISTERS" -> toLong(flag(CompilationFlag.UseShadowRegistersForInterrupts)),
      "ZPREG_SIZE" -> (if (platform.cpuFamily == CpuFamily.M6502) zpRegisterSize.toLong else 0)
    )
    platform.features ++ featuresFromOptions ++ featureOverrides
  }

}

object CpuFamily extends Enumeration {
  /**
    * Family based on the MOS 6502 processor and its descendants
    */
  val M6502: CpuFamily.Value = Value
  /**
    * Family based on the Intel 8080 processor and similar architectures
    */
  val I80: CpuFamily.Value = Value
  /**
    * Family based on the Motorola 6800 processor
    */
  val M6800: CpuFamily.Value = Value
  /**
    * Family based on the Motorola 6809 processor
    */
  val M6809: CpuFamily.Value = Value
  /**
    * Family based on the Intel 8086/8088 processor and its descendants
    */
  val I86: CpuFamily.Value = Value
  /**
    * Family based on the Motorola 68000 processor and its descendants
    */
  val M68K: CpuFamily.Value = Value
  /**
    * Family based on the ARM processors
    */
  val ARM: CpuFamily.Value = Value

  def forType(cpu: Cpu.Value): CpuFamily.Value = {
    import Cpu._
    cpu match {
      case Mos | StrictMos | Ricoh | StrictRicoh | Cmos | SC02 | Rockwell | Wdc | HuC6280 | CE02 | Sixteen => M6502
      case Intel8080 | Intel8085 | StrictIntel8085 | Sharp | Z80 | StrictZ80 | EZ80 | Z80Next => I80
      case Intel8086 | Intel80186 => I86
      case Cpu.Motorola6809 => M6809
    }
  }

  def isBigEndian(family: CpuFamily.Value): Boolean = family match {
    case M6502 | I80 | I86 | ARM => false
    case M6800 | M6809 | M68K => true
  }
}

object Cpu extends Enumeration {

  /**
    * The MOS 6502/6507/6510 processor
    */
  val Mos: Cpu.Value = Value
  /**
    * The MOS 6502/6507/6510 processor, without illegal instructions
    */
  val StrictMos: Cpu.Value = Value
  /**
    * The Ricoh 2A03/2A07 processor
    */
  val Ricoh: Cpu.Value = Value
  /**
    * The Ricoh 2A03/2A07 processor, without illegal instructions
    */
  val StrictRicoh: Cpu.Value = Value
  /**
    * The base 65C02 processor
    */
  val Cmos: Cpu.Value = Value
  /**
    * The 65SC02 processor
    */
  val SC02: Cpu.Value = Value
  /**
    * The Rockwell 65C02 processor
    */
  val Rockwell: Cpu.Value = Value
  /**
    * The WDC 65C02 processor
    */
  val Wdc: Cpu.Value = Value
  /**
    * The Hudson Soft HuC6280 processor
    */
  val HuC6280: Cpu.Value = Value
  /**
    * The CSG 65CE02 processor
    */
  val CE02: Cpu.Value = Value
  /**
    * The WDC 65816 processor
    */
  val Sixteen: Cpu.Value = Value
  /**
    * The Intel 8080 processor
    */
  val Intel8080: Cpu.Value = Value
  /**
    * The Intel 8085 processor
    */
  val Intel8085: Cpu.Value = Value
  /**
    * The Intel 8085 processor, without illegal instructions
    */
  val StrictIntel8085: Cpu.Value = Value
  /**
    * The Zilog Z80 processor
    */
  val Z80: Cpu.Value = Value
  /**
    * The Zilog Z80 processor, without illegal instructions
    */
  val StrictZ80: Cpu.Value = Value
  /**
    * The Zilog eZ80 processor
    */
  val EZ80: Cpu.Value = Value
  /**
    * The Z80 core from the ZX Spectrum Next
    */
  val Z80Next: Cpu.Value = Value
  /**
    * The Sharp LR35902 processor
    */
  val Sharp: Cpu.Value = Value
  /**
    * The Intel 8086 or 8088 processor
    */
  val Intel8086: Cpu.Value = Value
  /**
    * The Intel 80186 or 80188 processor
    */
  val Intel80186: Cpu.Value = Value
  /**
    * The Motorola 6809 processor
    */
  val Motorola6809: Cpu.Value = Value

  /**
    * Processors that can run code for WDC 65C02
    */
  val CmosCompatible: Set[Cpu.Value] = Set(Cmos, HuC6280, CE02, Sixteen, SC02, Rockwell, Wdc)
  /**
    * Processors that can run code for Zilog Z80
    */
  val Z80Compatible: Set[Cpu.Value] = Set(Z80, StrictZ80, EZ80, Z80Next)
  /**
    * Processors that can run code for Intel 8080
    */
  val Intel8080Compatible: Set[Cpu.Value] = Set(Intel8080, Intel8085, StrictIntel8085, Z80, StrictZ80, EZ80, Z80Next)
  /**
    * Processors that can run code for Intel 8085
    */
  val Intel8085Compatible: Set[Cpu.Value] = Set(Intel8085, StrictIntel8085)

  import CompilationFlag._

  private val alwaysDefaultFlags = Set(
    VariableOverlap, CompactReturnDispatchParams, FunctionFallthrough, RegisterVariables, FunctionDeduplication, EnableBreakpoints,
    NonZeroTerminatedLiteralWarning, CallToOverlappingBankWarning,
  )

  private val mosAlwaysDefaultFlags = alwaysDefaultFlags

  private val i80AlwaysDefaultFlags = alwaysDefaultFlags

  private val m6809AlwaysDefaultFlags = alwaysDefaultFlags

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
    case SC02 =>
      mosAlwaysDefaultFlags ++ Set(DecimalMode, EmitCmosOpcodes, EmitSC02Opcodes)
    case Rockwell =>
      mosAlwaysDefaultFlags ++ Set(DecimalMode, EmitCmosOpcodes, EmitSC02Opcodes, EmitRockwellOpcodes)
    case Wdc =>
      mosAlwaysDefaultFlags ++ Set(DecimalMode, EmitCmosOpcodes, EmitSC02Opcodes, EmitRockwellOpcodes, EmitWdcOpcodes)
    case HuC6280 =>
      mosAlwaysDefaultFlags ++ Set(DecimalMode, EmitCmosOpcodes, EmitSC02Opcodes, EmitRockwellOpcodes, EmitHudsonOpcodes)
    case CE02 =>
      mosAlwaysDefaultFlags ++ Set(DecimalMode, EmitCmosOpcodes, EmitSC02Opcodes, EmitRockwellOpcodes, Emit65CE02Opcodes)
    case Sixteen =>
      mosAlwaysDefaultFlags ++ Set(DecimalMode, EmitCmosOpcodes, EmitSC02Opcodes, EmitEmulation65816Opcodes, EmitNative65816Opcodes, ReturnWordsViaAccumulator)
    case Intel8080 =>
      i80AlwaysDefaultFlags ++ Set(EmitIntel8080Opcodes, UseIntelSyntaxForInput, UseIntelSyntaxForOutput)
    case StrictIntel8085 | Intel8085 =>
      i80AlwaysDefaultFlags ++ Set(EmitIntel8080Opcodes, EmitIntel8085Opcodes, UseIntelSyntaxForInput, UseIntelSyntaxForOutput)
    case StrictZ80 | Z80 =>
      i80AlwaysDefaultFlags ++ Set(EmitIntel8080Opcodes, EmitExtended80Opcodes, EmitZ80Opcodes, UseIxForStack, UseShadowRegistersForInterrupts)
    case Z80Next =>
      i80AlwaysDefaultFlags ++ Set(EmitIntel8080Opcodes, EmitExtended80Opcodes, EmitZ80Opcodes, UseIxForStack, UseShadowRegistersForInterrupts, EmitIllegals, EmitZ80NextOpcodes)
    case EZ80 =>
      i80AlwaysDefaultFlags ++ Set(EmitIntel8080Opcodes, EmitExtended80Opcodes, EmitZ80Opcodes, UseIxForStack, UseShadowRegistersForInterrupts, EmitEZ80Opcodes)
    case Sharp =>
      i80AlwaysDefaultFlags ++ Set(EmitExtended80Opcodes, EmitSharpOpcodes)
    case Intel8086 | Intel80186 =>
      i80AlwaysDefaultFlags ++ Set(EmitIntel8080Opcodes, UseIxForStack, EmitIntel8085Opcodes, EmitIllegals)
    case Motorola6809 =>
      m6809AlwaysDefaultFlags
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
    case "65sc02" => SC02
    case "sc02" => SC02
    case "65c02" => Rockwell
    case "c02" => Rockwell
    case "w65c02" => Wdc
    case "wdc65c02" => Wdc
    case "wdc" => Wdc
    case "hudson" => HuC6280
    case "huc6280" => HuC6280
    case "c6280" => HuC6280
    case "6280" => HuC6280
    case "65ce02" => CE02
    case "ce02" => CE02
    case "65816" => Sixteen
    case "65c816" => Sixteen
    case "816" => Sixteen
    case "5a22" => Sixteen
    case "ricoh" => Ricoh
    case "2a03" => Ricoh
    case "2a07" => Ricoh
    case "strictricoh" => StrictRicoh
    case "strict2a03" => StrictRicoh
    case "strict2a07" => StrictRicoh
    case "z80" => Z80
    case "strictz80" => Z80
    case "zx80next" => Z80Next
      // disabled for now:
//    case "ez80" => EZ80
    case "gameboy"  => Sharp
    case "gb"  => Sharp
    case "sharp"  => Sharp
    case "lr35902"  => Sharp
    case "8080" => Intel8080
    case "i8080" => Intel8080
    case "intel8080" => Intel8080
    case "8085" => Intel8085
    case "i8085" => Intel8085
    case "intel8085" => Intel8085
    case "strict8085" => StrictIntel8085
    case "stricti8085" => StrictIntel8085
    case "strictintel8085" => StrictIntel8085
    case "intel8086" => Intel8086
    case "i8086" => Intel8086
    case "8086" => Intel8086
    case "intel80186" => Intel80186
    case "i80186" => Intel80186
    case "80186" => Intel80186
    case "intel80286" => Intel80186
    case "i80286" => Intel80186
    case "80286" => Intel80186
      // undocumented for now
    case "6809" => Motorola6809
    case _ => log.fatal("Unknown CPU achitecture: " + name)
  }

  def getMaxSizeReturnableViaRegisters(cpu: Cpu.Value, compilationOptions: CompilationOptions): Int =
    CpuFamily.forType(cpu) match {
      case CpuFamily.M6502 => 2
      case CpuFamily.I80 | CpuFamily.I86 => 4
      case CpuFamily.M6809 => 2
      case _ => ???
    }

  def isBigEndian(cpu: Cpu.Value): Boolean = CpuFamily.isBigEndian(CpuFamily.forType(cpu))
}

object CompilationFlag extends Enumeration {
  val
  // common compilation options:
  EmitIllegals, DecimalMode, LenientTextEncoding, LineNumbersInAssembly, SourceInAssembly, EnableBreakpoints,
  // compilation options for MOS:
  EmitCmosOpcodes, EmitCmosNopOpcodes, EmitSC02Opcodes, EmitRockwellOpcodes, EmitWdcOpcodes, EmitHudsonOpcodes, Emit65CE02Opcodes, EmitEmulation65816Opcodes, EmitNative65816Opcodes,
  PreventJmpIndirectBug, LargeCode, ReturnWordsViaAccumulator, SoftwareStack,
  // compilation options for I80
  EmitIntel8080Opcodes, EmitIntel8085Opcodes, EmitExtended80Opcodes, EmitZ80Opcodes, EmitEZ80Opcodes, EmitSharpOpcodes, EmitZ80NextOpcodes,
  UseShadowRegistersForInterrupts,
  UseIxForStack, UseIyForStack,
  UseIxForScratch, UseIyForScratch,
  UseIntelSyntaxForInput,
  UseIntelSyntaxForOutput,
  // compilation options for 6809
  UseUForStack, UseYForStack,
  // optimization options:
  OptimizeForSize, OptimizeForSpeed, OptimizeForSonicSpeed, OptimizeForDebugging,
  DangerousOptimizations, InlineFunctions, InterproceduralOptimization,
  FunctionFallthrough, RegisterVariables, FunctionDeduplication, SubroutineExtraction,
  OptimizeStdlib,
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
  CallToOverlappingBankWarning,
  FatalWarnings,
  // special options for internal compiler use
  EnableInternalTestSyntax,
  InternalCurrentlyOptimizingForMeasurement = Value

  val allWarnings: Set[CompilationFlag.Value] = Set(ExtraComparisonWarnings)

  val fromString: Map[String, CompilationFlag.Value] = Map(
    "lunix" -> LUnixRelocatableCode,
    "emit_illegals" -> EmitIllegals,
    "emit_cmos" -> EmitCmosOpcodes,
    "emit_65sc02" -> EmitSC02Opcodes,
    "emit_rockwell" -> EmitRockwellOpcodes,
    "emit_wdc" -> EmitWdcOpcodes,
    "emit_65ce02" -> Emit65CE02Opcodes,
    "emit_huc6280" -> EmitHudsonOpcodes,
    "emit_z80" -> EmitZ80Opcodes,
    "emit_ez80" -> EmitEZ80Opcodes,
    "emit_x80" -> EmitExtended80Opcodes,
    "emit_8080" -> EmitIntel8080Opcodes,
    "emit_8085" -> EmitIntel8085Opcodes,
    "emit_sharp" -> EmitSharpOpcodes,
    "ix_stack" -> UseIxForStack,
    "iy_stack" -> UseIyForStack,
    "ix_scratch" -> UseIxForScratch,
    "iy_scratch" -> UseIyForScratch,
    "u_stack" -> UseUForStack,
    "y_stack" -> UseYForStack,
    "software_stack" -> SoftwareStack,
    "use_shadow_registers_for_irq" -> UseShadowRegistersForInterrupts,
    "output_intel_syntax" -> UseIntelSyntaxForOutput,
    "input_intel_syntax" -> UseIntelSyntaxForInput,
    "ipo" -> InterproceduralOptimization,
    "optimize_stdlib" -> OptimizeStdlib,
    "function_fallthrough" -> FunctionFallthrough,
    "function_deduplication" -> FunctionDeduplication,
    "subroutine_extraction" -> SubroutineExtraction,
    "inline" -> InlineFunctions,
    "dangerous_optimizations" -> DangerousOptimizations,
    "decimal_mode" -> DecimalMode,
    "ror_warn" -> RorWarning,
    "prevent_jmp_indirect_bug" -> PreventJmpIndirectBug,
    "compact_dispatch_params" -> CompactReturnDispatchParams,
    "lenient_encoding" -> LenientTextEncoding,
    "breakpoints" -> EnableBreakpoints,
  )

}