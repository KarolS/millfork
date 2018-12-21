package millfork.test.emu

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import com.codingrodent.microprocessor.Z80.{CPUConstants, Z80Core}
import eu.rekawek.coffeegb.AddressSpace
import eu.rekawek.coffeegb.cpu.{Cpu, InterruptManager, SpeedMode}
import eu.rekawek.coffeegb.gpu.Gpu
import fastparse.core.Parsed.{Failure, Success}
import millfork.assembly.AssemblyOptimization
import millfork.assembly.z80.ZLine
import millfork.compiler.{CompilationContext, LabelGenerator}
import millfork.env.{Environment, InitializedArray, InitializedMemoryVariable, NormalFunction}
import millfork.error.ConsoleLogger
import millfork.node.{Program, StandardCallGraph}
import millfork.node.opt.NodeOptimization
import millfork.output.{MemoryBank, Z80Assembler}
import millfork.parser.{MosParser, PreprocessingResult, Preprocessor, Z80Parser}
import millfork._
import millfork.compiler.z80.Z80Compiler
import org.scalatest.Matchers

import scala.collection.JavaConverters._
import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
object EmuZ80Run {

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

  def cachedMath(cpu: millfork.Cpu.Value): Program = get(cpu, "include/i80_math.mfk")
  def cachedStdio(cpu: millfork.Cpu.Value): Program = get(cpu, "src/test/resources/include/dummy_stdio.mfk")
}

class EmuZ80Run(cpu: millfork.Cpu.Value, nodeOptimizations: List[NodeOptimization], assemblyOptimizations: List[AssemblyOptimization[ZLine]]) extends Matchers {
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
      CompilationFlag.InlineFunctions -> this.inline,
      CompilationFlag.OptimizeStdlib -> this.inline,
      CompilationFlag.OptimizeForSize -> this.optimizeForSize,
      CompilationFlag.EmitIllegals -> (cpu == millfork.Cpu.Z80),
      CompilationFlag.LenientTextEncoding -> true)
    val options = CompilationOptions(platform, millfork.Cpu.defaultFlags(cpu).map(_ -> true).toMap ++ extraFlags, None, 0, Map(), JobContext(log, new LabelGenerator))
    log.hasErrors = false
    log.verbosity = 999
    var effectiveSource = source
    if (!source.contains("_panic")) effectiveSource += "\n void _panic(){while(true){}}"
    log.setSource(Some(effectiveSource.lines.toIndexedSeq))
    val PreprocessingResult(preprocessedSource, features, pragmas) = Preprocessor.preprocessForTest(options, effectiveSource)
    // tests use Intel syntax only when forced to:
    val parserF = Z80Parser("", preprocessedSource, "", options, features, pragmas.contains("intel_syntax"))
    parserF.toAst match {
      case Success(unoptimized, _) =>
        log.assertNoErrors("Parse failed")


        // prepare
        val withLibraries = {
          var tmp = unoptimized
          tmp += EmuZ80Run.cachedMath(cpu)
          if (source.contains("import stdio")) {
            tmp += EmuZ80Run.cachedStdio(cpu)
          }
          tmp
        }
        val program = nodeOptimizations.foldLeft(withLibraries.applyImportantAliases)((p, opt) => p.applyNodeOptimization(opt, options))
        val callGraph = new StandardCallGraph(program, log)
        val env = new Environment(None, "", CpuFamily.I80, options.jobContext)
        env.collectDeclarations(program, options)

        val hasOptimizations = assemblyOptimizations.nonEmpty
        var unoptimizedSize = 0L
        // print unoptimized asm
        env.allPreallocatables.foreach {
          case f: NormalFunction =>
            val unoptimized = Z80Compiler.compile(CompilationContext(f.environment, f, 0, options, Set()))
            unoptimizedSize += unoptimized.map(_.sizeInBytes).sum
          case d: InitializedArray =>
            unoptimizedSize += d.contents.length
          case d: InitializedMemoryVariable =>
            unoptimizedSize += d.typ.size
        }

        log.assertNoErrors("Compile failed")


        // compile
        val env2 = new Environment(None, "", CpuFamily.I80, options.jobContext)
        env2.collectDeclarations(program, options)
        val assembler = new Z80Assembler(program, env2, platform)
        val output = assembler.assemble(callGraph, assemblyOptimizations, options)
        println(";;; compiled: -----------------")
        output.asm.takeWhile(s => !(s.startsWith(".") && s.contains("= $"))).filterNot(_.contains("////; DISCARD_")).foreach(println)
        println(";;; ---------------------------")
        assembler.labelMap.foreach { case (l, addr) => println(f"$l%-15s $$$addr%04x") }

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

        // CALL $0200
        // HALT
        memoryBank.output(0x1f0) = 0xCD.toByte
        memoryBank.output(0x1f1) = 0
        memoryBank.output(0x1f2) = 2
        memoryBank.output(0x1f3) = 0x76.toByte

        (0x200 until 0x2000).takeWhile(memoryBank.occupied(_)).map(memoryBank.output).grouped(16).map(_.map(i => f"$i%02x").mkString(" ")).foreach(log.debug(_))
        val resetN = source.contains("-'") && !options.flag(CompilationFlag.EmitExtended80Opcodes)
        val resetNMethod = {
          val clazz = classOf[Z80Core]
          val method = clazz.getDeclaredMethod("resetN")
          method.setAccessible(true)
          method
        }
        val timings = platform.cpu match {
          case millfork.Cpu.Z80 | millfork.Cpu.Intel8080 =>
            val cpu = new Z80Core(Z80Memory(memoryBank), DummyIO)
            cpu.reset()
            cpu.setProgramCounter(0x1f0)
            cpu.resetTStates()
            while (!cpu.getHalt) {
              cpu.executeOneInstruction()
              if (resetN) {
                resetNMethod.invoke(cpu)
              }
//              dump(cpu)
              cpu.getTStates should be < TooManyCycles
            }
            val tStates = cpu.getTStates
            Timings(tStates, tStates) -> memoryBank
          case millfork.Cpu.Sharp =>
            var ticks = 0L
            val cpu = GameboyStubs(memoryBank).cpu
            cpu.getRegisters.setPC(0x1f0)
            while (cpu.getState != Cpu.State.HALTED) {
              cpu.tick()
//              dump(cpu)
              ticks += 4
              ticks should be < TooManyCycles
            }
            Timings(ticks, ticks) -> memoryBank
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

  def formatZ80Flags(f: Int): String = {
    val array = Array[Char]('s', 'z', 'y', 'h', 'x', 'p', 'n', 'c')
    for (x <- 0 to 7) {
      if (f.&(1 << x) != 0) {
        array(7 - x) = (array(7 - x).toInt - 32).toChar
      }
    }
    new String(array)
  }

  def dump(cpu: Z80Core): Unit = {
    val pc = cpu.getRegisterValue(CPUConstants.RegisterNames.PC)
    val a = cpu.getRegisterValue(CPUConstants.RegisterNames.A)
    val bc = cpu.getRegisterValue(CPUConstants.RegisterNames.BC)
    val de = cpu.getRegisterValue(CPUConstants.RegisterNames.DE)
    val hl = cpu.getRegisterValue(CPUConstants.RegisterNames.HL)
    val f = cpu.getRegisterValue(CPUConstants.RegisterNames.F)
    println(f"PC=$pc%04x A=$a%02x,BC=$bc%04x,DE=$de%04x,HL=$hl%04x F=${formatZ80Flags(f)}%s")
  }

  def dump(cpu: Cpu): Unit = {
    val regs = cpu.getRegisters
    val pc = regs.getPC
    val a = regs.getA
    val bc = regs.getBC
    val de = regs.getDE
    val hl = regs.getHL
    val f = regs.getFlags.toString
    println(f"PC=$pc%04x A=$a%02x,BC=$bc%04x,DE=$de%04x,HL=$hl%04x F=$f%s")
  }

}
