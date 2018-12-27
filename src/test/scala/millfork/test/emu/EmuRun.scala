package millfork.test.emu

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import com.grapeshot.halfnes.{CPU, CPURAM}
import com.loomcom.symon.InstructionTable.CpuBehavior
import com.loomcom.symon.{Bus, Cpu, CpuState}
import fastparse.core.Parsed.{Failure, Success}
import millfork.assembly.AssemblyOptimization
import millfork.assembly.mos.AssemblyLine
import millfork.compiler.{CompilationContext, LabelGenerator}
import millfork.compiler.mos.MosCompiler
import millfork.env.{Environment, InitializedArray, InitializedMemoryVariable, NormalFunction}
import millfork.error.Logger
import millfork.node.{Program, StandardCallGraph}
import millfork.node.opt.NodeOptimization
import millfork.output.{MemoryBank, MosAssembler}
import millfork.parser.{MosParser, PreprocessingResult, Preprocessor}
import millfork.{CompilationFlag, CompilationOptions, CpuFamily, JobContext}
import org.scalatest.Matchers

import scala.collection.JavaConverters._

/**
  * @author Karol Stasiak
  */
case class Timings(nmos: Long, cmos: Long)

object EmuRun {

  private def preload(filename: String):  Option[Program] = {
    TestErrorReporting.log.info(s"Loading $filename")
    val source = Files.readAllLines(Paths.get(filename), StandardCharsets.US_ASCII).asScala.mkString("\n")
    val options = CompilationOptions(EmuPlatform.get(millfork.Cpu.Mos), Map(
          CompilationFlag.LenientTextEncoding -> true
        ), None, 4, Map(), JobContext(TestErrorReporting.log, new LabelGenerator))
    val PreprocessingResult(preprocessedSource, features, _) = Preprocessor.preprocessForTest(options, source)
    TestErrorReporting.log.info(s"Parsing $filename")
    MosParser("", preprocessedSource, "", options, features).toAst match {
      case Success(x, _) => Some(x)
      case _ => None
    }
  }

  private lazy val cachedZpregO:  Option[Program]= preload("include/zp_reg.mfk")
  private lazy val cachedBcdO:  Option[Program] = preload("include/bcd_6502.mfk")
  private lazy val cachedStdioO:  Option[Program] = preload("src/test/resources/include/dummy_stdio.mfk")
  def cachedZpreg: Program = synchronized { cachedZpregO.getOrElse(throw new IllegalStateException()) }
  def cachedStdio: Program = synchronized { cachedStdioO.getOrElse(throw new IllegalStateException()) }
  def cachedBcd: Program = synchronized { cachedBcdO.getOrElse(throw new IllegalStateException()) }
}

class EmuRun(cpu: millfork.Cpu.Value, nodeOptimizations: List[NodeOptimization], assemblyOptimizations: List[AssemblyOptimization[AssemblyLine]]) extends Matchers {

  def apply(source: String): MemoryBank = {
    apply2(source)._2
  }

  def emitIllegals = false

  def inline = false

  def blastProcessing = false

  def optimizeForSize = false

  def softwareStack = false

  def native16 = false

  private val timingNmos = Array[Int](
    7, 6, 0, 8, 3, 3, 5, 5, 3, 2, 2, 2, 4, 4, 6, 6,
    2, 5, 0, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
    6, 6, 0, 8, 3, 3, 5, 5, 4, 2, 2, 2, 4, 4, 6, 6,
    2, 5, 0, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,

    6, 6, 0, 8, 3, 3, 5, 5, 3, 2, 2, 2, 3, 4, 6, 6,
    2, 5, 0, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
    6, 6, 0, 8, 3, 3, 5, 5, 4, 2, 2, 2, 5, 4, 6, 6,
    2, 5, 0, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,

    2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4,
    2, 6, 0, 6, 4, 4, 4, 4, 2, 5, 2, 5, 5, 5, 5, 5,
    2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4,
    2, 5, 0, 5, 4, 4, 4, 4, 2, 4, 2, 4, 4, 4, 4, 4,

    2, 6, 2, 8, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6,
    2, 5, 0, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
    2, 6, 2, 8, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6,
    2, 5, 0, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
  )

  private val timingCmos = Array[Int](
    7, 6, 2, 1, 5, 3, 5, 5, 3, 2, 2, 1, 6, 4, 6, 5,
    2, 5, 5, 1, 5, 4, 6, 5, 2, 4, 2, 1, 6, 4, 6, 5,
    6, 6, 2, 1, 3, 3, 5, 5, 4, 2, 2, 1, 4, 4, 6, 5,
    2, 5, 5, 1, 4, 4, 6, 5, 2, 4, 2, 1, 4, 4, 6, 5,

    6, 6, 2, 1, 3, 3, 5, 5, 3, 2, 2, 1, 3, 4, 6, 5,
    2, 5, 5, 1, 4, 4, 6, 5, 2, 4, 3, 1, 8, 4, 6, 5,
    6, 6, 2, 1, 3, 3, 5, 5, 4, 2, 2, 1, 6, 4, 6, 5,
    2, 5, 5, 1, 4, 4, 6, 5, 2, 4, 4, 1, 6, 4, 6, 5,

    3, 6, 2, 1, 3, 3, 3, 5, 2, 2, 2, 1, 4, 4, 4, 5,
    2, 6, 5, 1, 4, 4, 4, 5, 2, 5, 2, 1, 4, 5, 5, 5,
    2, 6, 2, 1, 3, 3, 3, 5, 2, 2, 2, 1, 4, 4, 4, 5,
    2, 5, 5, 1, 4, 4, 4, 5, 2, 4, 2, 1, 4, 4, 4, 5,

    2, 6, 2, 1, 3, 3, 5, 5, 2, 2, 2, 3, 4, 4, 6, 5,
    2, 5, 5, 1, 4, 4, 6, 5, 2, 4, 3, 3, 4, 4, 7, 5,
    2, 6, 2, 1, 3, 3, 5, 5, 2, 2, 2, 1, 4, 4, 6, 5,
    2, 5, 5, 1, 4, 4, 6, 5, 2, 4, 4, 1, 4, 4, 7, 5,
  )

  private val variableLength = Set(0x10, 0x30, 0x50, 0x70, 0x90, 0xb0, 0xd0, 0xf0)

  private val TooManyCycles: Long = 1000000

  private def formatBool(b: Boolean, c: Char) = if (b) c else '-'

  private def formatState(q: CpuState): String =
    f"A=${q.a}%02X X=${q.x}%02X Y=${q.y}%02X S=${q.sp}%02X PC=${q.pc}%04X " +
      formatBool(q.negativeFlag, 'N') + formatBool(q.overflowFlag, 'V') + formatBool(q.breakFlag, 'B') +
      formatBool(q.decimalModeFlag, 'D') + formatBool(q.irqDisableFlag, 'I') + formatBool(q.zeroFlag, 'Z') + formatBool(q.carryFlag, 'C')

  def apply2(source: String): (Timings, MemoryBank) = {
    Console.out.flush()
    Console.err.flush()
    val log = TestErrorReporting.log
    println(source)
    val platform = EmuPlatform.get(cpu)
    val options = CompilationOptions(platform, Map(
      CompilationFlag.DecimalMode -> millfork.Cpu.defaultFlags(cpu).contains(CompilationFlag.DecimalMode),
      CompilationFlag.LenientTextEncoding -> true,
      CompilationFlag.EmitIllegals -> this.emitIllegals,
      CompilationFlag.InlineFunctions -> this.inline,
      CompilationFlag.OptimizeStdlib -> this.inline,
      CompilationFlag.InterproceduralOptimization -> true,
      CompilationFlag.CompactReturnDispatchParams -> true,
      CompilationFlag.SoftwareStack -> softwareStack,
      CompilationFlag.EmitCmosOpcodes -> millfork.Cpu.CmosCompatible.contains(platform.cpu),
      CompilationFlag.EmitEmulation65816Opcodes -> (platform.cpu == millfork.Cpu.Sixteen),
      CompilationFlag.EmitNative65816Opcodes -> (platform.cpu == millfork.Cpu.Sixteen && native16),
      CompilationFlag.Emit65CE02Opcodes -> (platform.cpu == millfork.Cpu.CE02),
      CompilationFlag.EmitHudsonOpcodes -> (platform.cpu == millfork.Cpu.HuC6280),
      CompilationFlag.SubroutineExtraction -> optimizeForSize,
      CompilationFlag.OptimizeForSize -> optimizeForSize,
      CompilationFlag.OptimizeForSpeed -> blastProcessing,
      CompilationFlag.OptimizeForSonicSpeed -> blastProcessing
      //      CompilationFlag.CheckIndexOutOfBounds -> true,
    ), None, 4, Map(), JobContext(log, new LabelGenerator))
    log.hasErrors = false
    log.verbosity = 999
    if (native16 && platform.cpu != millfork.Cpu.Sixteen) throw new IllegalStateException
    var effectiveSource = source
    if (!source.contains("_panic")) effectiveSource += "\n void _panic(){while(true){}}"
    if (native16) effectiveSource +=
      """
        |
        |asm void __init_16bit() @$200 {
        |    clc
        |    xce
        |    sep #$30
        |}
      """.stripMargin
    log.setSource(Some(effectiveSource.lines.toIndexedSeq))
    val PreprocessingResult(preprocessedSource, features, _) = Preprocessor.preprocessForTest(options, effectiveSource)
    val parserF = MosParser("", preprocessedSource, "", options, features)
    parserF.toAst match {
      case Success(unoptimized, _) =>
        log.assertNoErrors("Parse failed")

        // prepare
        val withLibraries = {
          var tmp = unoptimized
          if(source.contains("import zp_reg"))
            tmp += EmuRun.cachedZpreg
          if(source.contains("import stdio"))
            tmp += EmuRun.cachedStdio
          if(!options.flag(CompilationFlag.DecimalMode) && (source.contains("+'") || source.contains("-'") || source.contains("<<'") || source.contains("*'")))
            tmp += EmuRun.cachedBcd
          tmp
        }
        val program = nodeOptimizations.foldLeft(withLibraries.applyImportantAliases)((p, opt) => p.applyNodeOptimization(opt, options))
        val callGraph = new StandardCallGraph(program, log)
        val env = new Environment(None, "", CpuFamily.M6502, options.jobContext)
        env.collectDeclarations(program, options)

        val hasOptimizations = assemblyOptimizations.nonEmpty
        var unoptimizedSize = 0L
        // print unoptimized asm
        env.allPreallocatables.foreach {
          case f: NormalFunction =>
            val unoptimized = MosCompiler.compile(CompilationContext(f.environment, f, 0, options, Set()))
            unoptimizedSize += unoptimized.map(_.sizeInBytes).sum
          case d: InitializedArray =>
            unoptimizedSize += d.contents.length
          case d: InitializedMemoryVariable =>
            unoptimizedSize += d.typ.size
        }

        log.assertNoErrors("Compile failed")


        // compile
        val env2 = new Environment(None, "", CpuFamily.M6502, options.jobContext)
        env2.collectDeclarations(program, options)
        val assembler = new MosAssembler(program, env2, platform)
        val output = assembler.assemble(callGraph, assemblyOptimizations, options)
        println(";;; compiled: -----------------")
        output.asm.takeWhile(s => !(s.startsWith(".") && s.contains("= $"))).filterNot(_.contains("; DISCARD_")).foreach(println)
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
        if (source.contains("return [")) {
          for (_ <- 0 until 10; i <- 0xfffe.to(0, -1)) {
            if (memoryBank.readable(i)) memoryBank.readable(i + 1) = true
          }
        }
        val timings = platform.cpu match {
          case millfork.Cpu.Cmos =>
            runViaSymon(log, memoryBank, platform.codeAllocators("default").startAt, CpuBehavior.CMOS_6502)
          case millfork.Cpu.Sixteen =>
            runViaJs(log, memoryBank, platform.codeAllocators("default").startAt)
          case millfork.Cpu.Ricoh =>
            runViaHalfnes(log, memoryBank, platform.codeAllocators("default").startAt)
          case millfork.Cpu.Mos =>
            log.fatal("There's no NMOS emulator with decimal mode support")
            Timings(-1, -1) -> memoryBank
          case millfork.Cpu.StrictMos | millfork.Cpu.StrictRicoh =>
            runViaSymon(log, memoryBank, platform.codeAllocators("default").startAt, CpuBehavior.NMOS_6502)
          case _ =>
            log.trace("No emulation support for " + platform.cpu)
            Timings(-1, -1) -> memoryBank
        }
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

  def runViaHalfnes(log: Logger, memoryBank: MemoryBank, org: Int): (Timings, MemoryBank) = {
    val cpu = new CPU(new CPURAM(memoryBank))
    cpu.reset()
    cpu.PC = org
    // stack underflow cannot be easily detected directly,
    // but since the stack is full of zeroes, an underflowing RTS jumps to $0001
    while (cpu.PC.&(0xffff) > 1 && cpu.clocks < TooManyCycles) {
      //      println(cpu.status())
      cpu.runcycle(0, 0)
    }
    println("clocks: " + cpu.clocks)
    System.out.flush()
    cpu.clocks.toLong should be < TooManyCycles
    println(cpu.clocks + " NMOS cycles")
    cpu.flagstobyte().&(8).==(0) should be(true)
    Timings(cpu.clocks, 0) -> memoryBank
  }

  def runViaSymon(log: Logger, memoryBank: MemoryBank, org: Int, behavior: CpuBehavior): (Timings, MemoryBank) = {
    val cpu = new Cpu
    cpu.setBehavior(behavior)
    val ram = new SymonTestRam(memoryBank)
    val bus = new Bus(1 << 16)
    bus.addCpu(cpu)
    bus.addDevice(ram)
    cpu.setBus(bus)
    cpu.setProgramCounter(org)
    cpu.setStackPointer(0xff)
    val legal = MosAssembler.getStandardLegalOpcodes

    var countNmos = 0L
    var countCmos = 0L
    while (cpu.getStackPointer > 1 && countCmos < TooManyCycles) {
      // println(cpu.disassembleNextOp())
      val pcBefore = cpu.getProgramCounter
      cpu.step()
      val pcAfter = cpu.getProgramCounter
      // println(formatState(cpu.getCpuState))
      val instruction = cpu.getInstruction
      if (behavior == CpuBehavior.NMOS_6502 || behavior == CpuBehavior.NMOS_WITH_ROR_BUG) {
        if (!legal(instruction)) {
          throw new RuntimeException("unexpected illegal: " + instruction.toHexString)
        }
      }
      countNmos += timingNmos(instruction)
      countCmos += timingCmos(instruction)
      if (variableLength(instruction)) {
        val jump = pcAfter - pcBefore
        if (jump <= 0 || jump > 3) {
          countNmos += 1
          countCmos += 1
        }
      }
    }
    log.trace(f"[$$c000] = ${memoryBank.readByte(0xc000)}%02X")
    countCmos should be < TooManyCycles
    println(countNmos + " NMOS cycles")
    println(countCmos + " CMOS cycles")
    cpu.getDecimalModeFlag should be(false)
    Timings(countNmos, countCmos) -> memoryBank
  }

  def runViaJs(log: Logger, memoryBank: MemoryBank, org: Int): (Timings, MemoryBank) = {
    val (cycles, newOutput) = NashornEmulator.run(memoryBank.output, 80, 0x200)
    System.arraycopy(newOutput, 0, memoryBank.output, 0, 1 << 16)
    Timings(cycles, cycles) -> memoryBank
  }
}
