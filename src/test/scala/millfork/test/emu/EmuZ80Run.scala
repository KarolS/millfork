package millfork.test.emu

import fastparse.core.Parsed.{Failure, Success}
import millfork.assembly.AssemblyOptimization
import millfork.assembly.z80.ZLine
import millfork.compiler.CompilationContext
import millfork.compiler.mos.MosCompiler
import millfork.env.{Environment, InitializedArray, InitializedMemoryVariable, NormalFunction}
import millfork.error.ErrorReporting
import millfork.node.StandardCallGraph
import millfork.node.opt.NodeOptimization
import millfork.output.{MemoryBank, MosAssembler, Z80Assembler}
import millfork.parser.Z80Parser
import millfork.CompilationOptions
import millfork.compiler.z80.Z80Compiler
import org.scalatest.Matchers

/**
  * @author Karol Stasiak
  */
class EmuZ80Run(cpu: millfork.Cpu.Value, nodeOptimizations: List[NodeOptimization], assemblyOptimizations: List[AssemblyOptimization[ZLine]]) extends Matchers {

  private val variableLength = Set(0x10, 0x30, 0x50, 0x70, 0x90, 0xb0, 0xd0, 0xf0)

  private val TooManyCycles: Long = 1000000

  def apply2(source: String): (Timings, MemoryBank) = {
    Console.out.flush()
    Console.err.flush()
    println(source)
    val platform = EmuPlatform.get(cpu)
    val options = CompilationOptions(platform, millfork.Cpu.defaultFlags(cpu).map(_ -> true).toMap)
    ErrorReporting.hasErrors = false
    ErrorReporting.verbosity = 999
    var effectiveSource = source
    if (!source.contains("_panic")) effectiveSource += "\n void _panic(){while(true){}}"
    val parserF = Z80Parser("", effectiveSource, "", options)
    parserF.toAst match {
      case Success(unoptimized, _) =>
        ErrorReporting.assertNoErrors("Parse failed")


        // prepare
        val program = nodeOptimizations.foldLeft(unoptimized)((p, opt) => p.applyNodeOptimization(opt, options))
        val callGraph = new StandardCallGraph(program)
        val env = new Environment(None, "")
        env.collectDeclarations(program, options)

        val hasOptimizations = assemblyOptimizations.nonEmpty
        var unoptimizedSize = 0L
        // print unoptimized asm
        env.allPreallocatables.foreach {
          case f: NormalFunction =>
            val unoptimized = Z80Compiler.compile(CompilationContext(f.environment, f, 0, options))
            unoptimizedSize += unoptimized.map(_.sizeInBytes).sum
          case d: InitializedArray =>
            unoptimizedSize += d.contents.length
          case d: InitializedMemoryVariable =>
            unoptimizedSize += d.typ.size
        }

        ErrorReporting.assertNoErrors("Compile failed")


        // compile
        val env2 = new Environment(None, "")
        env2.collectDeclarations(program, options)
        val assembler = new Z80Assembler(program, env2, platform)
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

        ErrorReporting.assertNoErrors("Code generation failed")

        val memoryBank = assembler.mem.banks("default")
        if (source.contains("return [")) {
          for (_ <- 0 until 10; i <- 0xfffe.to(0, -1)) {
            if (memoryBank.readable(i)) memoryBank.readable(i + 1) = true
          }
        }
        platform.cpu match {
          case _ =>
            Timings(-1, -1) -> memoryBank
        }
      case f: Failure[_, _] =>
        println(f)
        println(f.extra.toString)
        println(f.lastParser.toString)
        ErrorReporting.error("Syntax error", Some(parserF.lastPosition))
        ???
    }
  }

}
