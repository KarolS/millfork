package millfork.test.emu

import com.grapeshot.halfnes.{CPU, CPURAM}
import com.loomcom.symon.InstructionTable.CpuBehavior
import com.loomcom.symon.{Bus, Cpu, CpuState}
import fastparse.core.Parsed.{Failure, Success}
import millfork.assembly.opt.AssemblyOptimization
import millfork.compiler.{CompilationContext, MlCompiler}
import millfork.env.{Environment, InitializedArray, NormalFunction}
import millfork.error.ErrorReporting
import millfork.node.StandardCallGraph
import millfork.node.opt.NodeOptimization
import millfork.output.{Assembler, MemoryBank}
import millfork.parser.MfParser
import millfork.{CompilationFlag, CompilationOptions}
import org.scalatest.Matchers

/**
  * @author Karol Stasiak
  */
case class Timings(nmos: Long, cmos: Long)

class EmuRun(cpu: millfork.Cpu.Value, nodeOptimizations: List[NodeOptimization], assemblyOptimizations: List[AssemblyOptimization], quantum: Boolean) extends Matchers {

  def apply(source: String): MemoryBank = {
    apply2(source)._2
  }

  def emitIllegals = false

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
    val options = new CompilationOptions(platform, Map(
      CompilationFlag.EmitIllegals -> this.emitIllegals,
      CompilationFlag.DetailedFlowAnalysis -> quantum,
    ))
    ErrorReporting.hasErrors = false
    ErrorReporting.verbosity = 999
    val parserF = MfParser("", source, "", options)
    parserF.toAst match {
      case Success(unoptimized, _) =>
        ErrorReporting.assertNoErrors("Parse failed")


        // prepare
        val program = nodeOptimizations.foldLeft(unoptimized)((p, opt) => p.applyNodeOptimization(opt))
        val callGraph = new StandardCallGraph(program)
        val env = new Environment(None, "")
        env.collectDeclarations(program, options)

        val hasOptimizations = assemblyOptimizations.nonEmpty
        var optimizedSize = 0L
        var unoptimizedSize = 0L
        // print asm
        env.allPreallocatables.foreach {
          case f: NormalFunction =>
            val result = MlCompiler.compile(CompilationContext(f.environment, f, 0, options))
            val unoptimized = result.linearize
            if (hasOptimizations) {
              val optimized = assemblyOptimizations.foldLeft(unoptimized) { (c, opt) =>
                opt.optimize(f, c, options)
              }
              println("Unoptimized:")
              unoptimized.filter(_.isPrintable).foreach(println(_))
              println("Optimized:")
              optimized.filter(_.isPrintable).foreach(println(_))
              unoptimizedSize += unoptimized.map(_.sizeInBytes).sum
              optimizedSize += optimized.map(_.sizeInBytes).sum
            } else {
              unoptimized.filter(_.isPrintable).foreach(println(_))
              unoptimizedSize += unoptimized.map(_.sizeInBytes).sum
              optimizedSize += unoptimized.map(_.sizeInBytes).sum
            }
          case d: InitializedArray =>
            println(d.name)
            d.contents.foreach(c => println("    !byte " + c))
            unoptimizedSize += d.contents.length
            optimizedSize += d.contents.length
        }

        ErrorReporting.assertNoErrors("Compile failed")

        if (unoptimizedSize == optimizedSize) {
          println(f"Size:             $unoptimizedSize%5d B")
        } else {
          println(f"Unoptimized size: $unoptimizedSize%5d B")
          println(f"Optimized size:   $optimizedSize%5d B")
          println(f"Gain:              ${(100L * (unoptimizedSize - optimizedSize) / unoptimizedSize.toDouble).round}%5d%%")
        }

        // compile
        val assembler = new Assembler(env)
        assembler.assemble(callGraph, assemblyOptimizations, options)
        assembler.labelMap.foreach { case (l, addr) => println(f"$l%-15s $$$addr%04x") }

        ErrorReporting.assertNoErrors("Code generation failed")

        val memoryBank = assembler.mem.banks(0)
        platform.cpu match {
          case millfork.Cpu.Cmos =>
            runViaSymon(memoryBank, platform.org, CpuBehavior.CMOS_6502)
          case millfork.Cpu.Ricoh =>
            runViaHalfnes(memoryBank, platform.org)
          case millfork.Cpu.Mos =>
            ErrorReporting.fatal("There's no NMOS emulator with decimal mode support")
            Timings(-1, -1) -> memoryBank
          case _ =>
            runViaSymon(memoryBank, platform.org, CpuBehavior.NMOS_6502)
        }
      case f: Failure[_, _] =>
        println(f)
        println(f.extra.toString)
        println(f.lastParser.toString)
        ErrorReporting.error("Syntax error", Some(parserF.lastPosition))
        ???
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
    val legal = Assembler.getStandardLegalOpcodes

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
    countCmos should be < TooManyCycles
    println(countNmos + " NMOS cycles")
    println(countCmos + " CMOS cycles")
    cpu.getDecimalModeFlag should be(false)
    Timings(countNmos, countCmos) -> memoryBank
  }

}
