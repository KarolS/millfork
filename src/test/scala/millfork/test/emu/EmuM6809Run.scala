package millfork.test.emu

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import fastparse.core.Parsed.{Failure, Success}
import millfork._
import millfork.assembly.AssemblyOptimization
import millfork.assembly.m6809.MLine
import millfork.compiler.m6809.M6809Compiler
import millfork.compiler.{CompilationContext, LabelGenerator}
import millfork.env.{Environment, InitializedArray, InitializedMemoryVariable, NormalFunction}
import millfork.node.opt.NodeOptimization
import millfork.node.{Program, StandardCallGraph}
import millfork.output.{M6809Assembler, MemoryBank}
import millfork.parser.{PreprocessingResult, Preprocessor, Z80Parser}
import org.scalatest.Matchers

import scala.collection.JavaConverters._
import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
object EmuM6809Run {

  private def preload(cpu: millfork.Cpu.Value, filename: String):  Option[Program] = {
    TestErrorReporting.log.info(s"Loading $filename for $cpu")
    val source = Files.readAllLines(Paths.get(filename), StandardCharsets.US_ASCII).asScala.mkString("\n")
    val options = CompilationOptions(EmuPlatform.get(cpu), Map(
          CompilationFlag.LenientTextEncoding -> true
        ), None, 0, Map(), JobContext(TestErrorReporting.log, new LabelGenerator))
    val PreprocessingResult(preprocessedSource, features, _) = Preprocessor.preprocessForTest(options, source)
    TestErrorReporting.log.debug(s"Features: $features")
    TestErrorReporting.log.info(s"Parsing $filename")
    val parser = Z80Parser(filename, preprocessedSource, "", options, features, useIntelSyntax = false)
    parser.toAst match {
      case Success(x, _) => Some(x)
      case f: Failure[_, _] =>
        TestErrorReporting.log.error(f.toString)
        TestErrorReporting.log.error(f.extra.toString)
        TestErrorReporting.log.error(f.lastParser.toString)
        TestErrorReporting.log.error("Syntax error", Some(parser.lastPosition))
        TestErrorReporting.log.error("Parsing error")
        ???
    }
  }

  private lazy val cache: mutable.Map[(millfork.Cpu.Value, String), Option[Program]] = mutable.Map[(millfork.Cpu.Value, String), Option[Program]]()
  private def get(cpu: millfork.Cpu.Value, path: String): Program =
    synchronized { cache.getOrElseUpdate(cpu->path, preload(cpu, path)).getOrElse(throw new IllegalStateException()) }

  def cachedMath(cpu: millfork.Cpu.Value): Program = get(cpu, "include/m6809_math.mfk")
  def cachedStdio(cpu: millfork.Cpu.Value): Program = get(cpu, "src/test/resources/include/dummy_stdio.mfk")
}

class EmuM6809Run(cpu: millfork.Cpu.Value, nodeOptimizations: List[NodeOptimization], assemblyOptimizations: List[AssemblyOptimization[MLine]]) extends Matchers {
  def inline: Boolean = false

  def optimizeForSize: Boolean = false

  private val TooManyCycles: Long = 1500000

  def apply(source: String): MemoryBank = {
    apply2(source)._2
  }

  def apply2(source: String): (Timings, MemoryBank) = {
    Console.out.flush()
    Console.err.flush()
    val log = TestErrorReporting.log
    println(source)
    val platform = EmuPlatform.get(cpu)
    val extraFlags = Map(
      CompilationFlag.DangerousOptimizations -> true,
      CompilationFlag.EnableInternalTestSyntax -> true,
      CompilationFlag.InlineFunctions -> this.inline,
      CompilationFlag.OptimizeStdlib -> this.inline,
      CompilationFlag.OptimizeForSize -> this.optimizeForSize,
      CompilationFlag.SubroutineExtraction -> optimizeForSize,
      CompilationFlag.EmitIllegals -> false,
      CompilationFlag.LenientTextEncoding -> true)
    val options = CompilationOptions(platform, millfork.Cpu.defaultFlags(cpu).map(_ -> true).toMap ++ extraFlags, None, 0, Map(), JobContext(log, new LabelGenerator))
    println(cpu)
    println(options.flags.filter(_._2).keys.toSeq.sorted)
    log.hasErrors = false
    log.verbosity = 999
    var effectiveSource = source
    if (!source.contains("_panic")) effectiveSource += "\n void _panic(){while(true){}}"
    log.setSource(Some(effectiveSource.linesIterator.toIndexedSeq))
    val PreprocessingResult(preprocessedSource, features, pragmas) = Preprocessor.preprocessForTest(options, effectiveSource)
    // tests use Intel syntax only when forced to:
    val parserF = Z80Parser("", preprocessedSource, "", options, features, pragmas.contains("intel_syntax"))
    parserF.toAst match {
      case Success(unoptimized, _) =>
        log.assertNoErrors("Parse failed")


        // prepare
        val withLibraries = {
          var tmp = unoptimized
//          tmp += EmuM6809Run.cachedMath(cpu) // TODO: add this only after you implement maths
          if (source.contains("import stdio")) {
            tmp += EmuM6809Run.cachedStdio(cpu)
          }
          tmp
        }
        val program = nodeOptimizations.foldLeft(withLibraries.applyImportantAliases)((p, opt) => p.applyNodeOptimization(opt, options))
        val callGraph = new StandardCallGraph(program, log)
        val env = new Environment(None, "", CpuFamily.I80, options)
        env.collectDeclarations(program, options)

        val hasOptimizations = assemblyOptimizations.nonEmpty
        var unoptimizedSize = 0L
        // print unoptimized asm
        env.allPreallocatables.foreach {
          case f: NormalFunction =>
            val unoptimized = M6809Compiler.compile(CompilationContext(f.environment, f, 0, options, Set()))
            unoptimizedSize += unoptimized.map(_.sizeInBytes).sum
          case d: InitializedArray =>
            unoptimizedSize += d.contents.length
          case d: InitializedMemoryVariable =>
            unoptimizedSize += d.typ.size
        }

        log.assertNoErrors("Compile failed")


        // compile
        val env2 = new Environment(None, "", CpuFamily.I80, options)
        env2.collectDeclarations(program, options)
        val assembler = new M6809Assembler(program, env2, platform)
        val output = assembler.assemble(callGraph, assemblyOptimizations, options)
        println(";;; compiled: -----------------")
        output.asm.takeWhile(s => !(s.startsWith(".") && s.contains("= $"))).filterNot(_.contains("////; DISCARD_")).foreach(println)
        println(";;; ---------------------------")
        assembler.labelMap.foreach { case (l, (_, addr)) => println(f"$l%-15s $$$addr%04x") }

        val optimizedSize = assembler.mem.banks("default").initialized.count(identity).toLong
        if (unoptimizedSize == optimizedSize) {
          println(f"Size:             $unoptimizedSize%5d B")
        } else {
          println(f"Unoptimized size: $unoptimizedSize%5d B")
          println(f"Optimized size:   $optimizedSize%5d B")
          println(f"Gain:              ${(100L * (unoptimizedSize - optimizedSize) / unoptimizedSize.toDouble).round}%5d%%")
        }

        if (log.hasErrors) {
          fail("Code generation failed")
        }

        val memoryBank = assembler.mem.banks("default")
        (0x1f0 until 0x200).foreach(i => memoryBank.readable(i) = true)
        (0xff00 to 0xffff).foreach{i =>
          memoryBank.readable(i) = true
          memoryBank.writeable(i) = true
        }

        (0x200 until 0x2000).takeWhile(memoryBank.occupied(_)).map(memoryBank.output).grouped(16).map(_.map(i => f"$i%02x").mkString(" ")).foreach(log.debug(_))
        val timings = platform.cpu match {
          case _ =>
            Timings(-1, -1) -> memoryBank
        }
        log.clearErrors()
        timings
      case f: Failure[_, _] =>
        println(f)
        println(f.extra.toString)
        println(f.lastParser.toString)
        log.error("Syntax error", Some(parserF.lastPosition))
        fail("Parsing error")
    }
  }

}
