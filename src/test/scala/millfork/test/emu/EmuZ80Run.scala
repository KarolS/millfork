package millfork.test.emu

import com.codingrodent.microprocessor.Z80.{CPUConstants, Z80Core}
import eu.rekawek.coffeegb.AddressSpace
import eu.rekawek.coffeegb.cpu.{Cpu, InterruptManager, SpeedMode}
import eu.rekawek.coffeegb.gpu.Gpu
import fastparse.core.Parsed.{Failure, Success}
import millfork.assembly.AssemblyOptimization
import millfork.assembly.z80.ZLine
import millfork.compiler.CompilationContext
import millfork.env.{Environment, InitializedArray, InitializedMemoryVariable, NormalFunction}
import millfork.error.ErrorReporting
import millfork.node.StandardCallGraph
import millfork.node.opt.NodeOptimization
import millfork.output.{MemoryBank, Z80Assembler}
import millfork.parser.{Preprocessor, Z80Parser}
import millfork.{CompilationFlag, CompilationOptions, CpuFamily}
import millfork.compiler.z80.Z80Compiler
import org.scalatest.Matchers

/**
  * @author Karol Stasiak
  */
class EmuZ80Run(cpu: millfork.Cpu.Value, nodeOptimizations: List[NodeOptimization], assemblyOptimizations: List[AssemblyOptimization[ZLine]]) extends Matchers {
  def inline: Boolean = false

  private val TooManyCycles: Long = 1500000

  def apply(source: String): MemoryBank = {
    apply2(source)._2
  }

  def apply2(source: String): (Timings, MemoryBank) = {
    Console.out.flush()
    Console.err.flush()
    println(source)
    val platform = EmuPlatform.get(cpu)
    val extraFlags = Map(
      CompilationFlag.InlineFunctions -> this.inline,
      CompilationFlag.EmitIllegals -> (cpu == millfork.Cpu.Z80),
      CompilationFlag.LenientTextEncoding -> true)
    val options = CompilationOptions(platform, millfork.Cpu.defaultFlags(cpu).map(_ -> true).toMap ++ extraFlags, None, 0)
    ErrorReporting.hasErrors = false
    ErrorReporting.verbosity = 999
    var effectiveSource = source
    if (!source.contains("_panic")) effectiveSource += "\n void _panic(){while(true){}}"
    ErrorReporting.setSource(Some(effectiveSource.lines.toIndexedSeq))
    val (preprocessedSource, features) = Preprocessor.preprocessForTest(options, effectiveSource)
    val parserF = Z80Parser("", preprocessedSource, "", options, features)
    parserF.toAst match {
      case Success(unoptimized, _) =>
        ErrorReporting.assertNoErrors("Parse failed")


        // prepare
        val program = nodeOptimizations.foldLeft(unoptimized)((p, opt) => p.applyNodeOptimization(opt, options))
        val callGraph = new StandardCallGraph(program)
        val env = new Environment(None, "", CpuFamily.I80)
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

        ErrorReporting.assertNoErrors("Compile failed")


        // compile
        val env2 = new Environment(None, "", CpuFamily.I80)
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

        if (ErrorReporting.hasErrors) {
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

        (0x200 until 0x2000).takeWhile(memoryBank.occupied(_)).map(memoryBank.output).grouped(16).map(_.map(i => f"$i%02x").mkString(" ")).foreach(ErrorReporting.debug(_))

        val timings = platform.cpu match {
          case millfork.Cpu.Z80 | millfork.Cpu.Intel8080 =>
            val cpu = new Z80Core(Z80Memory(memoryBank), DummyIO)
            cpu.reset()
            cpu.setProgramCounter(0x1f0)
            cpu.resetTStates()
            while (!cpu.getHalt) {
              cpu.executeOneInstruction()
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
        ErrorReporting.clearErrors()
        timings
      case f: Failure[_, _] =>
        println(f)
        println(f.extra.toString)
        println(f.lastParser.toString)
        ErrorReporting.error("Syntax error", Some(parserF.lastPosition))
        ???
    }
  }

  def dump(cpu: Z80Core): Unit = {
    val a = cpu.getRegisterValue(CPUConstants.RegisterNames.A)
    val bc = cpu.getRegisterValue(CPUConstants.RegisterNames.BC)
    val de = cpu.getRegisterValue(CPUConstants.RegisterNames.DE)
    val hl = cpu.getRegisterValue(CPUConstants.RegisterNames.HL)
    println(f"A=$a%02x,BC=$bc%04x,DE=$de%04x,HL=$hl%04x")
  }

  def dump(cpu: Cpu): Unit = {
    val regs = cpu.getRegisters
    val a = regs.getA
    val bc = regs.getBC
    val de = regs.getDE
    val hl = regs.getHL
    println(f"A=$a%02x,BC=$bc%04x,DE=$de%04x,HL=$hl%04x")
  }

}
