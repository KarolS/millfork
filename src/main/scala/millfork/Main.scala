package millfork

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.Locale

import millfork.assembly.opt._
import millfork.buildinfo.BuildInfo
import millfork.cli.{CliParser, CliStatus}
import millfork.env.Environment
import millfork.error.ErrorReporting
import millfork.node.StandardCallGraph
import millfork.output.Assembler
import millfork.parser.SourceLoadingQueue

/**
  * @author Karol Stasiak
  */

case class Context(inputFileNames: List[String],
                   outputFileName: Option[String] = None,
                   runFileName: Option[String] = None,
                   optimizationLevel: Option[Int] = None,
                   platform: Option[String] = None,
                   outputAssembly: Boolean = false,
                   outputLabels: Boolean = false,
                   includePath: List[String] = Nil,
                   flags: Map[CompilationFlag.Value, Boolean] = Map(),
                   verbosity: Option[Int] = None) {
  def changeFlag(f: CompilationFlag.Value, b: Boolean): Context = {
    if (flags.contains(f)) {
      if (flags(f) != b) {
        ErrorReporting.error("Conflicting flags")
      }
      this
    } else {
      copy(flags = this.flags + (f -> b))
    }
  }
}

object Main {


  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      ErrorReporting.info("For help, use --help")
    }
    val startTime = System.nanoTime()
    val (status, c) = parser.parse(Context(Nil), args.toList)
    status match {
      case CliStatus.Quit => return
      case CliStatus.Failed =>
        ErrorReporting.fatalQuit("Invalid command line")
      case CliStatus.Ok => ()
    }
    ErrorReporting.assertNoErrors("Invalid command line")
    if (c.inputFileNames.isEmpty) {
      ErrorReporting.fatalQuit("No input files")
    }
    ErrorReporting.verbosity = c.verbosity.getOrElse(0)
    val optLevel = c.optimizationLevel.getOrElse(0)
    val platform = Platform.lookupPlatformFile(c.includePath, c.platform.getOrElse {
      ErrorReporting.info("No platform selected, defaulting to `c64`")
      "c64"
    })
    val options = CompilationOptions(platform, c.flags)
    ErrorReporting.debug("Effective flags: ")
    options.flags.toSeq.sortBy(_._1).foreach{
      case (f, b) => ErrorReporting.debug(f"    $f%-30s : $b%s")
    }

    val output = c.outputFileName.getOrElse("a")
    val assOutput = output + ".asm"
    val labelOutput = output + ".lbl"
    val prgOutput = if (!output.endsWith(platform.fileExtension)) output + platform.fileExtension else output

    val unoptimized = new SourceLoadingQueue(
      initialFilenames = c.inputFileNames,
      includePath = c.includePath,
      options = options).run()

    val program = if (optLevel > 0) {
      OptimizationPresets.NodeOpt.foldLeft(unoptimized)((p, opt) => p.applyNodeOptimization(opt, options))
    } else {
      unoptimized
    }
    val callGraph = new StandardCallGraph(program)

    val env = new Environment(None, "")
    env.collectDeclarations(program, options)

    val assemblyOptimizations = optLevel match {
      case 0 => Nil
      case 1 => OptimizationPresets.QuickPreset
      case i if i >= 9 => List(SuperOptimizer)
      case _ =>
        val goodExtras = List(
          if (options.flag(CompilationFlag.EmitEmulation65816Opcodes)) SixteenOptimizations.AllForEmulation else Nil,
          if (options.flag(CompilationFlag.EmitNative65816Opcodes)) SixteenOptimizations.AllForNative else Nil,
          if (options.flag(CompilationFlag.ZeropagePseudoregister)) ZeropageRegisterOptimizations.All else Nil,
        ).flatten
        val extras = List(
          if (options.flag(CompilationFlag.EmitIllegals)) UndocumentedOptimizations.All else Nil,
          if (options.flag(CompilationFlag.Emit65CE02Opcodes)) CE02Optimizations.All else Nil,
          if (options.flag(CompilationFlag.EmitCmosOpcodes)) CmosOptimizations.All else LaterOptimizations.Nmos,
          if (options.flag(CompilationFlag.EmitHudsonOpcodes)) HudsonOptimizations.All else Nil,
          if (options.flag(CompilationFlag.EmitEmulation65816Opcodes)) SixteenOptimizations.AllForEmulation else Nil,
          if (options.flag(CompilationFlag.EmitNative65816Opcodes)) SixteenOptimizations.AllForNative else Nil,
          if (options.flag(CompilationFlag.DangerousOptimizations)) DangerousOptimizations.All else Nil,
        ).flatten
        val goodCycle = List.fill(optLevel - 2)(OptimizationPresets.Good ++ goodExtras).flatten
        goodCycle ++ OptimizationPresets.AssOpt ++ extras ++ goodCycle
    }

    // compile
    val assembler = new Assembler(program, env)
    val result = assembler.assemble(callGraph, assemblyOptimizations, options)
    ErrorReporting.assertNoErrors("Codegen failed")
    ErrorReporting.debug(f"Unoptimized code size: ${assembler.unoptimizedCodeSize}%5d B")
    ErrorReporting.debug(f"Optimized code size:   ${assembler.optimizedCodeSize}%5d B")
    ErrorReporting.debug(f"Gain:                   ${(100L * (assembler.unoptimizedCodeSize - assembler.optimizedCodeSize) / assembler.unoptimizedCodeSize.toDouble).round}%5d%%")
    ErrorReporting.debug(f"Initialized variables: ${assembler.initializedVariablesSize}%5d B")

    if (c.outputAssembly) {
      val path = Paths.get(assOutput)
      ErrorReporting.debug("Writing assembly to " + path.toAbsolutePath)
      Files.write(path, result.asm.mkString("\n").getBytes(StandardCharsets.UTF_8))
    }
    if (c.outputLabels) {
      val path = Paths.get(labelOutput)
      ErrorReporting.debug("Writing labels to " + path.toAbsolutePath)
      Files.write(path, result.labels.sortWith { (a, b) =>
        val aLocal = a._1.head == '.'
        val bLocal = b._1.head == '.'
        if (aLocal == bLocal) a._1 < b._1
        else b._1 < a._1
      }.groupBy(_._2).values.map(_.head).toSeq.sortBy(_._2).map { case (l, a) =>
        val normalized = l.replace('$', '_').replace('.', '_')
        s"al ${a.toHexString} .$normalized"
      }.mkString("\n").getBytes(StandardCharsets.UTF_8))
    }
    val path = Paths.get(prgOutput)
    ErrorReporting.debug("Writing output to " + path.toAbsolutePath)
    ErrorReporting.debug(s"Total output size: ${result.code.length} bytes")
    Files.write(path, result.code)
    ErrorReporting.debug(s"Total time: ${Math.round((System.nanoTime() - startTime)/1e6)} ms")
    c.runFileName.foreach(program =>
      new ProcessBuilder(program, path.toAbsolutePath.toString).start()
    )
  }

  private def parser = new CliParser[Context] {

    fluff("Main options:", "")

    parameter("-o", "--out").required().placeholder("<file>").action { (p, c) =>
      assertNone(c.outputFileName, "Output already defined")
      c.copy(outputFileName = Some(p))
    }.description("The output file name, without extension.").onWrongNumber(_ => ErrorReporting.fatalQuit("No output file specified"))

    flag("-s").action { c =>
      c.copy(outputAssembly = true)
    }.description("Generate also the assembly output.")

    flag("-g").action { c =>
      c.copy(outputLabels = true)
    }.description("Generate also the label file.")

    parameter("-t", "--target").placeholder("<platform>").action { (p, c) =>
      assertNone(c.platform, "Platform already defined")
      c.copy(platform = Some(p))
    }.description("Target platform, any of: c64, c16, plus4, vic20, vic20_3k, vic20_8k, pet, c128, a8.")

    parameter("-I", "--include-dir").repeatable().placeholder("<dir>;<dir>;...").action { (paths, c) =>
      val n = paths.split(";")
      c.copy(includePath = c.includePath ++ n)
    }.description("Include paths for modules.")

    parameter("-r", "--run").placeholder("<program>").action { (p, c) =>
      assertNone(c.runFileName, "Run program already defined")
      c.copy(runFileName = Some(p))
    }.description("Program to run after successful compilation.")

    endOfFlags("--").description("Marks the end of options.")

    fluff("", "Verbosity options:", "")

    flag("-q", "--quiet").action { c =>
      assertNone(c.verbosity, "Cannot use -v and -q together")
      c.copy(verbosity = Some(-1))
    }.description("Supress all messages except for errors.")

    private val verbose = flag("-v", "--verbose").maxCount(3).action { c =>
      if (c.verbosity.exists(_ < 0)) ErrorReporting.error("Cannot use -v and -q together", None)
      c.copy(verbosity = Some(1 + c.verbosity.getOrElse(0)))
    }.description("Increase verbosity.")
    flag("-vv").repeatable().action(c => verbose.encounter(verbose.encounter(verbose.encounter(c)))).description("Increase verbosity even more.")
    flag("-vvv").repeatable().action(c => verbose.encounter(verbose.encounter(c))).description("Increase verbosity even more.")

    fluff("", "Code generation options:", "")

    boolean("-fcmos-ops", "-fno-cmos-ops").action { (c, v) =>
      c.changeFlag(CompilationFlag.EmitCmosOpcodes, v)
    }.description("Whether should emit CMOS opcodes.")
    boolean("-f65ce02-ops", "-fno-65ce02-ops").action { (c, v) =>
      c.changeFlag(CompilationFlag.Emit65CE02Opcodes, v)
    }.description("Whether should emit 65CE02 opcodes.")
    boolean("-fhuc6280-ops", "-fno-huc6280-ops").action { (c, v) =>
      c.changeFlag(CompilationFlag.EmitHudsonOpcodes, v)
    }.description("Whether should emit HuC6280huc6280 opcodes.")
    flag("-fno-65816-ops").action { c =>
      c.changeFlag(CompilationFlag.EmitEmulation65816Opcodes, b = false)
      c.changeFlag(CompilationFlag.EmitNative65816Opcodes, b = false)
      c.changeFlag(CompilationFlag.ReturnWordsViaAccumulator, b = false)
    }.description("Don't emit 65816 opcodes.")
    flag("-femulation-65816-ops").action { c =>
      c.changeFlag(CompilationFlag.EmitEmulation65816Opcodes, b = true)
      c.changeFlag(CompilationFlag.EmitNative65816Opcodes, b = false)
      c.changeFlag(CompilationFlag.ReturnWordsViaAccumulator, b = false)
    }.description("Emit 65816 opcodes (experimental).")
    flag("-fnative-65816-ops").action { c =>
      c.changeFlag(CompilationFlag.EmitEmulation65816Opcodes, b = true)
      c.changeFlag(CompilationFlag.EmitNative65816Opcodes, b = true)
    }.description("Emit 65816 opcodes (experimental).")
    boolean("-flarge-code", "-fsmall-code").action { (c, v) =>
      c.changeFlag(CompilationFlag.LargeCode, v)
    }.description("Whether should use 24-bit or 16-bit jumps to subroutines (not yet implemented).").hidden()
    boolean("-fillegals", "-fno-illegals").action { (c, v) =>
      c.changeFlag(CompilationFlag.EmitIllegals, v)
    }.description("Whether should emit illegal (undocumented) NMOS opcodes. Requires -O2 or higher to have an effect.")
    boolean("-fzp-register", "-fno-zp-register").action { (c, v) =>
      c.changeFlag(CompilationFlag.ZeropagePseudoregister, v)
    }.description("Whether should use 2 bytes of zeropage as a pseudoregister.")
    boolean("-fjmp-fix", "-fno-jmp-fix").action { (c, v) =>
      c.changeFlag(CompilationFlag.PreventJmpIndirectBug, v)
    }.description("Whether should prevent indirect JMP bug on page boundary.")
    boolean("-fdecimal-mode", "-fno-decimal-mode").action { (c, v) =>
      c.changeFlag(CompilationFlag.DecimalMode, v)
    }.description("Whether decimal mode should be available.")
    boolean("-fvariable-overlap", "-fno-variable-overlap").action { (c, v) =>
      c.changeFlag(CompilationFlag.VariableOverlap, v)
    }.description("Whether variables should overlap if their scopes do not intersect.")
    boolean("-fcompact-dispatch-params", "-fno-compact-dispatch-params").action { (c, v) =>
      c.changeFlag(CompilationFlag.CompactReturnDispatchParams, v)
    }.description("Whether parameter values in return dispatch statements may overlap other objects.")
    boolean("-fbounds-checking", "-fno-bounds-checking").action { (c, v) =>
      c.changeFlag(CompilationFlag.VariableOverlap, v)
    }.description("Whether should insert bounds checking on array access.")

    fluff("", "Optimization options:", "")


    flag("-O0").action { c =>
      assertNone(c.optimizationLevel, "Optimization level already defined")
      c.copy(optimizationLevel = Some(0))
    }.description("Disable all optimizations.")
    flag("-O").action { c =>
      assertNone(c.optimizationLevel, "Optimization level already defined")
      c.copy(optimizationLevel = Some(1))
    }.description("Optimize code.")
    for (i <- 1 to 9) {
      val f = flag("-O" + i).action { c =>
        assertNone(c.optimizationLevel, "Optimization level already defined")
        c.copy(optimizationLevel = Some(i))
      }.description("Optimize code even more.")
      if (i == 1 || i > 3) f.hidden()
    }
    flag("--inline").action { c =>
      c.changeFlag(CompilationFlag.InlineFunctions, true)
    }.description("Inline functions automatically.")
    flag("-Os", "--size").action { c =>
      c.changeFlag(CompilationFlag.OptimizeForSize, true)
      c.changeFlag(CompilationFlag.OptimizeForSpeed, false)
      c.changeFlag(CompilationFlag.OptimizeForSonicSpeed, false)
    }.description("Prefer smaller code even if it is slightly slower (experimental).")
    flag("-Of", "--fast").action { c =>
      c.changeFlag(CompilationFlag.OptimizeForSize, false)
      c.changeFlag(CompilationFlag.OptimizeForSpeed, true)
      c.changeFlag(CompilationFlag.OptimizeForSonicSpeed, false)
    }.description("Prefer faster code even if it is slightly bigger (experimental).")
    flag("-Ob", "--blast-processing").action { c =>
      c.changeFlag(CompilationFlag.OptimizeForSize, false)
      c.changeFlag(CompilationFlag.OptimizeForSpeed, true)
      c.changeFlag(CompilationFlag.OptimizeForSonicSpeed, true)
      c.changeFlag(CompilationFlag.InlineFunctions, true)
    }.description("Prefer faster code even if it is much bigger (experimental). Implies --inline.")
    flag("--detailed-flow").action { c =>
      c.changeFlag(CompilationFlag.DetailedFlowAnalysis, true)
    }.description("Use detailed flow analysis (experimental).")
    flag("--dangerous-optimizations").action { c =>
      c.changeFlag(CompilationFlag.DangerousOptimizations, true)
    }.description("Use dangerous optimizations (experimental).")

    fluff("", "Warning options:", "")

    flag("-Wall", "--Wall").action { c =>
      CompilationFlag.allWarnings.foldLeft(c) { (c, f) => c.changeFlag(f, true) }
    }.description("Enable extra warnings.")

    flag("-Wfatal", "--Wfatal").action { c =>
      c.changeFlag(CompilationFlag.FatalWarnings, true)
    }.description("Treat warnings as errors.")

    fluff("", "Other options:", "")

    flag("--help").action(c => {
      printHelp(20).foreach(println(_))
      assumeStatus(CliStatus.Quit)
      c
    }).description("Display this message.")

    flag("--version").action(c => {
      println("millfork version " + BuildInfo.version)
      assumeStatus(CliStatus.Quit)
      System.exit(0)
      c
    }).description("Print the version and quit.")


    default.action { (p, c) =>
      if (p.startsWith("-")) {
        ErrorReporting.error(s"Invalid option `$p`", None)
        c
      } else {
        c.copy(inputFileNames = c.inputFileNames :+ p)
      }
    }

    def assertNone[T](value: Option[T], msg: String): Unit = {
      if (value.isDefined) {
        ErrorReporting.error(msg, None)
      }
    }
  }
}
