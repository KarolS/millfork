package millfork.test.emu

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import fastparse.core.Parsed.{Failure, Success}
import millfork._
import millfork.assembly.AssemblyOptimization
import millfork.assembly.m6809.MLine
import millfork.assembly.m6809.opt.VeryLateM6809AssemblyOptimizations
import millfork.compiler.m6809.M6809Compiler
import millfork.compiler.{CompilationContext, LabelGenerator}
import millfork.env.{Environment, InitializedArray, InitializedMemoryVariable, NormalFunction}
import millfork.error.ConsoleLogger
import millfork.node.opt.NodeOptimization
import millfork.node.{Program, StandardCallGraph}
import millfork.output.{M6809Assembler, MemoryBank}
import millfork.parser.{M6809Parser, MosParser, PreprocessingResult, Preprocessor, Z80Parser}
import org.roug.osnine.{BusStraight, MC6809}
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

  def apply(source: String): MemoryBank = {
    apply2(source)._2
  }

  def emitIllegals = false

  def inline = false

  def blastProcessing = false

  def optimizeForSize = false

  private val TooManyCycles: Long = 1000000

  private def formatBool(b: Int, c: Char): Char = if (b != 0) c else '-'


  def apply2(source: String): (Timings, MemoryBank) = {
    Console.out.flush()
    Console.err.flush()
    val log = TestErrorReporting.log
    println(source)
    val platform = EmuPlatform.get(cpu)
    val options = CompilationOptions(platform, Map(
      CompilationFlag.EnableInternalTestSyntax -> true,
      CompilationFlag.DecimalMode -> true,
      CompilationFlag.LenientTextEncoding -> true,
      CompilationFlag.EmitIllegals -> this.emitIllegals,
      CompilationFlag.InlineFunctions -> this.inline,
      CompilationFlag.OptimizeStdlib -> this.inline,
      CompilationFlag.InterproceduralOptimization -> true,
      CompilationFlag.CompactReturnDispatchParams -> true,
      CompilationFlag.SubroutineExtraction -> optimizeForSize,
      CompilationFlag.OptimizeForSize -> optimizeForSize,
      CompilationFlag.OptimizeForSpeed -> blastProcessing,
      CompilationFlag.OptimizeForSonicSpeed -> blastProcessing
      //      CompilationFlag.CheckIndexOutOfBounds -> true,
    ), None, 0, Map(), JobContext(log, new LabelGenerator))
    log.hasErrors = false
    log.verbosity = 999
    var effectiveSource = source
    if (!source.contains("_panic")) effectiveSource += "\n void _panic(){while(true){}}"
    if (source.contains("call(")) effectiveSource += "\nnoinline asm word call(word d) {\nJMP ,X\n}\n"
    log.setSource(Some(effectiveSource.linesIterator.toIndexedSeq))
    val PreprocessingResult(preprocessedSource, features, _) = Preprocessor.preprocessForTest(options, effectiveSource)
    val parserF = M6809Parser("", preprocessedSource, "", options, features)
    parserF.toAst match {
      case Success(unoptimized, _) =>
        log.assertNoErrors("Parse failed")

        // prepare
        val withLibraries = {
          var tmp = unoptimized
          if(source.contains("import stdio"))
            tmp += EmuRun.cachedStdio
          if(!options.flag(CompilationFlag.DecimalMode) && (source.contains("+'") || source.contains("-'") || source.contains("<<'") || source.contains("*'")))
            tmp += EmuRun.cachedBcd
          tmp
        }
        val program = nodeOptimizations.foldLeft(withLibraries.applyImportantAliases)((p, opt) => p.applyNodeOptimization(opt, options))
        program.checkSegments(log, platform.codeAllocators.keySet)
        log.assertNoErrors("Failed")
        val callGraph = new StandardCallGraph(program, log)
        val env = new Environment(None, "", CpuFamily.M6809, options)
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
        val env2 = new Environment(None, "", CpuFamily.M6502, options)
        env2.collectDeclarations(program, options)
        val assembler = new M6809Assembler(program, env2, platform)
        val output = assembler.assemble(callGraph, assemblyOptimizations, options, VeryLateM6809AssemblyOptimizations.All)
        println(";;; compiled: -----------------")
        output.asm.takeWhile(s => !(s.startsWith(".") && s.contains("= $"))).filterNot(_.contains("; DISCARD_")).foreach(println)
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
        if (source.contains("return [")) {
          for (_ <- 0 until 10; i <- 0xfffe.to(0, -1)) {
            if (memoryBank.readable(i)) memoryBank.readable(i + 1) = true
          }
        }
        val timings =  run(log, memoryBank, platform.codeAllocators("default").startAt)
        log.clearErrors()
        timings
      case f: Failure[_, _] =>
        println(f)
        println(f.extra.toString)
        println(f.lastParser.toString)
        log.error("Syntax error", Some(parserF.lastPosition))
        fail("syntax error")
    }
  }

  private def debugState(q: MC6809): Unit =
    println(f"D=${q.d.get()}%04X X=${q.x.get()}%04X Y=${q.y.get()}%04X U=${q.u.get()}%04X S=${q.s.get()}%04X PC=${q.pc.get()}%04X " +
      formatBool(q.cc.bit_e, 'E') +formatBool(q.cc.bit_f, 'F') + formatBool(q.cc.bit_h, 'H') +
      formatBool(q.cc.bit_i, 'I') + formatBool(q.cc.bit_n, 'N') + formatBool(q.cc.bit_z, 'Z') +
      formatBool(q.cc.bit_v, 'V') + formatBool(q.cc.bit_c, 'C'))

  def run(log: ConsoleLogger, memoryBank: MemoryBank, startAt: Int): (Timings, MemoryBank) = {
    (0x200 until 0x2000).takeWhile(memoryBank.occupied(_)).map(memoryBank.output).grouped(16).map(_.map(i => f"$i%02x").mkString(" ")).foreach(log.debug(_))
    val bus = new BusStraight()
    bus.addMemorySegment(new M6809Memory(memoryBank, startAt))
    val cpu = new MC6809(bus)
    bus.clearNMI()
    cpu.pc.set(startAt)
    cpu.s.set(0xfff0)

//    val method = classOf[MC6809].getDeclaredMethod("setTraceInstructions", classOf[Boolean])
//    method.setAccessible(true)
//    method.invoke(cpu, true.asInstanceOf[AnyRef])

//    debugState(cpu)
    while (cpu.pc.get() > 2) {
      cpu.execute()
//      debugState(cpu)
      bus.getCycleCounter should be < TooManyCycles
    }
    Timings(bus.getCycleCounter, bus.getCycleCounter) -> memoryBank
  }
}


