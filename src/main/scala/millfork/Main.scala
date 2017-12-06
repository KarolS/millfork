package millfork

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.Locale

import millfork.assembly.opt.{CmosOptimizations, DangerousOptimizations, SuperOptimizer, UndocumentedOptimizations}
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
    val options = new CompilationOptions(platform, c.flags)
    ErrorReporting.debug("Effective flags: " + options.flags)

    val output = c.outputFileName.getOrElse("a")
    val assOutput = output + ".asm"
    val labelOutput = output + ".lbl"
    val prgOutput = if (!output.endsWith(platform.fileExtension)) output + platform.fileExtension else output

    val unoptimized = new SourceLoadingQueue(
      initialFilenames = c.inputFileNames,
      includePath = c.includePath,
      options = options).run()

    val program = if (optLevel > 0) {
      OptimizationPresets.NodeOpt.foldLeft(unoptimized)((p, opt) => p.applyNodeOptimization(opt))
    } else {
      unoptimized
    }
    val callGraph = new StandardCallGraph(program)

    val env = new Environment(None, "")
    env.collectDeclarations(program, options)
    val extras = List(
      if (options.flag(CompilationFlag.EmitIllegals)) UndocumentedOptimizations.All else Nil,
      if (options.flag(CompilationFlag.EmitCmosOpcodes)) CmosOptimizations.All else Nil,
      if (options.flag(CompilationFlag.DangerousOptimizations)) DangerousOptimizations.All else Nil,
    ).flatten
    val goodCycle = List.fill(optLevel - 1)(OptimizationPresets.Good ++ extras).flatten
    val assemblyOptimizations = if (optLevel <= 0) Nil else if (optLevel >= 9) List(SuperOptimizer) else {
      goodCycle ++ OptimizationPresets.AssOpt ++ extras ++ goodCycle
    }

    // compile
    val assembler = new Assembler(env)
    val result = assembler.assemble(callGraph, assemblyOptimizations, options)
    ErrorReporting.assertNoErrors("Codegen failed")
    ErrorReporting.debug(f"Unoptimized code size: ${assembler.unoptimizedCodeSize}%5d B")
    ErrorReporting.debug(f"Optimized code size:   ${assembler.optimizedCodeSize}%5d B")
    ErrorReporting.debug(f"Gain:                   ${(100L * (assembler.unoptimizedCodeSize - assembler.optimizedCodeSize) / assembler.unoptimizedCodeSize.toDouble).round}%5d%%")
    ErrorReporting.debug(f"Initialized arrays:    ${assembler.initializedArraysSize}%5d B")

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
    Files.write(path, result.code)
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
    boolean("-fillegals", "-fno-illegals").action { (c, v) =>
      c.changeFlag(CompilationFlag.EmitIllegals, v)
    }.description("Whether should emit illegal (undocumented) NMOS opcodes.")
    boolean("-fjmp-fix", "-fno-jmp-fix").action { (c, v) =>
      c.changeFlag(CompilationFlag.PreventJmpIndirectBug, v)
    }.description("Whether should prevent indirect JMP bug on page boundary.")
    boolean("-fdecimal-mode", "-fno-decimal-mode").action { (c, v) =>
      c.changeFlag(CompilationFlag.DecimalMode, v)
    }.description("Whether should decimal mode be available.")
    boolean("-fvariable-overlap", "-fno-variable-overlap").action { (c, v) =>
      c.changeFlag(CompilationFlag.VariableOverlap, v)
    }.description("Whether should variables overlap if their scopes do not intersect.")

    fluff("", "Optimization options:", "")


    flag("-O0").action { c =>
      assertNone(c.optimizationLevel, "Optimization level already defined")
      c.copy(optimizationLevel = Some(0))
    }.description("Disable all optimizations.")
    flag("-O").action { c =>
      assertNone(c.optimizationLevel, "Optimization level already defined")
      c.copy(optimizationLevel = Some(1))
    }.description("Optimize code.")
    for (i <- 2 to 9) {
      val f = flag("-O" + i).action { c =>
        assertNone(c.optimizationLevel, "Optimization level already defined")
        c.copy(optimizationLevel = Some(i))
      }.description("Optimize code even more.")
      if (i > 3) f.hidden()
    }
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
