package millfork.test.emu

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import com.grapeshot.halfnes.{CPU, CPURAM}
import com.loomcom.symon.InstructionTable.CpuBehavior
import com.loomcom.symon.{Bus, Cpu, CpuState}
import fastparse.core.Parsed.{Failure, Success}
import millfork.assembly.AssemblyOptimization
import millfork.assembly.mos.AssemblyLine
import millfork.compiler.CompilationContext
import millfork.compiler.mos.MosCompiler
import millfork.env.{Environment, InitializedArray, InitializedMemoryVariable, NormalFunction}
import millfork.error.ErrorReporting
import millfork.node.StandardCallGraph
import millfork.node.opt.NodeOptimization
import millfork.output.{MemoryBank, MosAssembler}
import millfork.parser.{MosParser, Preprocessor}
import millfork.{CompilationFlag, CompilationOptions, CpuFamily}
import org.scalatest.Matchers

import scala.collection.JavaConverters._

/**
  * @author Karol Stasiak
  */
case class Timings(nmos: Long, cmos: Long)

class EmuRun(cpu: millfork.Cpu.Value, nodeOptimizations: List[NodeOptimization], assemblyOptimizations: List[AssemblyOptimization[AssemblyLine]]) extends Matchers {

  def apply(source: String): MemoryBank = {
    apply2(source)._2
  }

  def emitIllegals = false

  def inline = false

  def blastProcessing = false

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
    println(source)
    val platform = EmuPlatform.get(cpu)
    val options = CompilationOptions(platform, Map(
      CompilationFlag.LenientTextEncoding -> true,
      CompilationFlag.EmitIllegals -> this.emitIllegals,
      CompilationFlag.InlineFunctions -> this.inline,
      CompilationFlag.InterproceduralOptimization -> true,
      CompilationFlag.CompactReturnDispatchParams -> true,
      CompilationFlag.EmitCmosOpcodes -> millfork.Cpu.CmosCompatible.contains(platform.cpu),
      CompilationFlag.EmitEmulation65816Opcodes -> (platform.cpu == millfork.Cpu.Sixteen),
      CompilationFlag.Emit65CE02Opcodes -> (platform.cpu == millfork.Cpu.CE02),
      CompilationFlag.EmitHudsonOpcodes -> (platform.cpu == millfork.Cpu.HuC6280),
      CompilationFlag.OptimizeForSpeed -> blastProcessing,
      CompilationFlag.OptimizeForSonicSpeed -> blastProcessing
      //      CompilationFlag.CheckIndexOutOfBounds -> true,
    ), None, 2)
    ErrorReporting.hasErrors = false
    ErrorReporting.verbosity = 999
    var effectiveSource = source
    if (!source.contains("_panic")) effectiveSource += "\n void _panic(){while(true){}}"
    if (source.contains("import zp_reg"))
      effectiveSource += Files.readAllLines(Paths.get("include/zp_reg.mfk"), StandardCharsets.US_ASCII).asScala.mkString("\n", "\n", "")
    ErrorReporting.setSource(Some(effectiveSource.lines.toIndexedSeq))
    val (preprocessedSource, features) = Preprocessor.preprocessForTest(options, effectiveSource)
    val parserF = MosParser("", preprocessedSource, "", options, features)
    parserF.toAst match {
      case Success(unoptimized, _) =>
        ErrorReporting.assertNoErrors("Parse failed")


        // prepare
        val program = nodeOptimizations.foldLeft(unoptimized)((p, opt) => p.applyNodeOptimization(opt, options))
        val callGraph = new StandardCallGraph(program)
        val env = new Environment(None, "", CpuFamily.M6502)
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

        ErrorReporting.assertNoErrors("Compile failed")


        // compile
        val env2 = new Environment(None, "", CpuFamily.M6502)
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

        if (ErrorReporting.hasErrors) {
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
            runViaSymon(memoryBank, platform.codeAllocators("default").startAt, CpuBehavior.CMOS_6502)
          case millfork.Cpu.Ricoh =>
            runViaHalfnes(memoryBank, platform.codeAllocators("default").startAt)
          case millfork.Cpu.Mos =>
            ErrorReporting.fatal("There's no NMOS emulator with decimal mode support")
            Timings(-1, -1) -> memoryBank
          case millfork.Cpu.StrictMos | millfork.Cpu.StrictRicoh =>
            runViaSymon(memoryBank, platform.codeAllocators("default").startAt, CpuBehavior.NMOS_6502)
          case _ =>
            ErrorReporting.trace("No emulation support for " + platform.cpu)
            Timings(-1, -1) -> memoryBank
        }
        ErrorReporting.clearErrors()
        timings
      case f: Failure[_, _] =>
        println(f)
        println(f.extra.toString)
        println(f.lastParser.toString)
        ErrorReporting.error("Syntax error", Some(parserF.lastPosition))
        fail("syntax error")
    }
  }

  def runViaHalfnes(memoryBank: MemoryBank, org: Int): (Timings, MemoryBank) = {
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

  def runViaSymon(memoryBank: MemoryBank, org: Int, behavior: CpuBehavior): (Timings, MemoryBank) = {
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
    ErrorReporting.trace(f"[$$c000] = ${memoryBank.readByte(0xc000)}%02X")
    countCmos should be < TooManyCycles
    println(countNmos + " NMOS cycles")
    println(countCmos + " CMOS cycles")
    cpu.getDecimalModeFlag should be(false)
    Timings(countNmos, countCmos) -> memoryBank
  }

}
