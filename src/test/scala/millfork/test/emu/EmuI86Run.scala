package millfork.test.emu

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import com.codingrodent.microprocessor.Z80.Z80Core
import fastparse.core.Parsed.{Failure, Success}
import fr.neatmonster.ibmpc.{IBMCGA, Intel8086, Intel8255, Intel8259, Motorola6845}
import javax.swing.SwingUtilities
import millfork._
import millfork.assembly.AssemblyOptimization
import millfork.assembly.z80.ZLine
import millfork.compiler.z80.Z80Compiler
import millfork.compiler.{CompilationContext, LabelGenerator}
import millfork.env.{Environment, InitializedArray, InitializedMemoryVariable, NormalFunction}
import millfork.node.opt.NodeOptimization
import millfork.node.{Program, StandardCallGraph}
import millfork.output.{MemoryBank, Z80ToX86Crossassembler}
import millfork.parser.{PreprocessingResult, Preprocessor, Z80Parser}
import org.scalatest.Matchers

import scala.collection.JavaConverters._
import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
object EmuI86Run {

  private def preload(filename: String):  Option[Program] = {
    TestErrorReporting.log.info(s"Loading $filename for Intel8086")
    val source = Files.readAllLines(Paths.get(filename), StandardCharsets.US_ASCII).asScala.mkString("\n")
    val options = CompilationOptions(EmuPlatform.get(millfork.Cpu.Intel8086), Map(
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

  private lazy val cache: mutable.Map[String, Option[Program]] = mutable.Map[String, Option[Program]]()
  private def get(path: String): Program =
    synchronized { cache.getOrElseUpdate(path, preload(path)).getOrElse(throw new IllegalStateException()) }

  def cachedMath(): Program = get("include/i80_math.mfk") // TODO
  def cachedStdio(): Program = get("src/test/resources/include/dummy_stdio.mfk")

  val leakSaverCpu = new Intel8086()
  val leakSaverCrtc = new Motorola6845()
  val leakSaverPic = new Intel8259()
  val leakSaverPpi = new Intel8255(leakSaverPic)
}

class EmuI86Run(nodeOptimizations: List[NodeOptimization], assemblyOptimizations: List[AssemblyOptimization[ZLine]]) extends Matchers {
  def inline: Boolean = false

  def optimizeForSize: Boolean = false

  private val TooManyCycles: Long = 1500000

  def apply(source: String): MemoryBank = {
    apply2(source)._2
  }

  def apply2(source: String): (Timings, MemoryBank) = {
    if (!Settings.enableIntel8086Tests) return Timings(-1, -1) -> new MemoryBank()
    Console.out.flush()
    Console.err.flush()
    val log = TestErrorReporting.log
    println(source)
    val platform = EmuPlatform.get(millfork.Cpu.Intel8086)
    val extraFlags = Map(
      CompilationFlag.EnableInternalTestSyntax -> true,
      CompilationFlag.InlineFunctions -> this.inline,
      CompilationFlag.OptimizeStdlib -> this.inline,
      CompilationFlag.OptimizeForSize -> this.optimizeForSize,
      CompilationFlag.SubroutineExtraction -> optimizeForSize,
      CompilationFlag.LenientTextEncoding -> true)
    val options = CompilationOptions(platform, millfork.Cpu.defaultFlags(millfork.Cpu.Intel8086).map(_ -> true).toMap ++ extraFlags, None, 0, Map(), JobContext(log, new LabelGenerator))
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
          tmp += EmuI86Run.cachedMath()
          if (source.contains("import stdio")) {
            tmp += EmuI86Run.cachedStdio()
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
            val unoptimized = Z80Compiler.compile(CompilationContext(f.environment, f, 0, options, Set()))
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
        val assembler = new Z80ToX86Crossassembler(program, env2, platform)
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
        (0xff00 to 0xffff).foreach{i =>
          memoryBank.readable(i) = true
          memoryBank.writeable(i) = true
        }

        (0x100 until 0x2000).takeWhile(memoryBank.occupied(_)).map(memoryBank.output).grouped(16).map(_.map(i => f"$i%02x").mkString(" ")).foreach(log.debug(_))
        val resetN = source.contains("-'") && !options.flag(CompilationFlag.EmitExtended80Opcodes)
        val resetNMethod = {
          val clazz = classOf[Z80Core]
          val method = clazz.getDeclaredMethod("resetN")
          method.setAccessible(true)
          method
        }
        val timings = {
          val cpu = new Intel8086()
          cpu.reset()
          val memory = sniffField[Array[Int]](cpu, "memory")
          0x100.until(1<<16).foreach(i => memory(i) = memoryBank.output(i) & 0xff)
          // far jmp 0:0
          "ea:0000:0000".filter(Character.isJavaIdentifierPart).grouped(2).zipWithIndex.foreach{case (c, ix) => memory(0xFFFF0+ix) = Integer.parseInt(c, 16)}
          // xor ax,ax / mov ds,ax / mov ss,ax / mov sp,0xfffe / near call 0x0100 / hlt
          "31c0/8ed8/8ed0/bc:feff/e8:f400/f4".filter(Character.isJavaIdentifierPart).grouped(2).zipWithIndex.foreach{case (c, ix) => memory(ix) = Integer.parseInt(c, 16)}
          var clocks = 0L
          do {
//            print(f"CS:IP=${sniffField[Int](cpu, "cs")}%04x:${sniffField[Int](cpu, "ip")}%04x")
//            print(f" AX=${sniffField[Int](cpu, "ah")}%02x${sniffField[Int](cpu, "al")}%02x")
//            print(f" BX=${sniffField[Int](cpu, "bh")}%02x${sniffField[Int](cpu, "bl")}%02x")
//            print(f" CX=${sniffField[Int](cpu, "ch")}%02x${sniffField[Int](cpu, "cl")}%02x")
//            print(f" DX=${sniffField[Int](cpu, "dh")}%02x${sniffField[Int](cpu, "dl")}%02x")
//            println()
            clocks = sniffField[Long](cpu, "clocks")
          } while(clocks < 100000 && sniffMethod[Boolean](cpu, "tick"))
          SwingUtilities.getWindowAncestor(sniffField[IBMCGA](cpu, "cga")).setVisible(false)
          // CGA's leak, so at least don't let other objects leak
          setField(sniffField[IBMCGA](cpu, "cga"), "cpu", EmuI86Run.leakSaverCpu)
          setField(sniffField[IBMCGA](cpu, "cga"), "ppi", EmuI86Run.leakSaverPpi)
          setField(sniffField[IBMCGA](cpu, "cga"), "crtc", EmuI86Run.leakSaverCrtc)
          if (clocks >= 100000) throw new RuntimeException("Timeout")
          0x100.until(1<<16).foreach(i => memoryBank.output(i) = memory(i).toByte)
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

  def sniffField[T](obj: AnyRef, fieldName: String): T = {
    val f = obj.getClass.getDeclaredField(fieldName)
    f.setAccessible(true)
    f.get(obj).asInstanceOf[T]
  }

  def setField(obj: AnyRef, fieldName: String, value: Any): Unit = {
    val f = obj.getClass.getDeclaredField(fieldName)
    f.setAccessible(true)
    f.set(obj, value)
  }

  def sniffMethod[T](intel8086: Intel8086, fieldName: String): T = {
    val m = intel8086.getClass.getDeclaredMethod(fieldName)
    m.setAccessible(true)
    m.invoke(intel8086).asInstanceOf[T]
  }

}
