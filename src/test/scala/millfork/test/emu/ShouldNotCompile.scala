package millfork.test.emu

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import fastparse.core.Parsed.{Failure, Success}
import millfork.compiler.{CompilationContext, LabelGenerator}
import millfork.compiler.mos.MosCompiler
import millfork.env.{Environment, InitializedArray, InitializedMemoryVariable, NormalFunction}
import millfork.node.StandardCallGraph
import millfork.parser.{M6809Parser, MosParser, PreprocessingResult, Preprocessor, Z80Parser}
import millfork._
import millfork.compiler.m6809.M6809Compiler
import millfork.compiler.z80.Z80Compiler
import millfork.output.{M6809Assembler, MosAssembler, Z80Assembler}
import org.scalatest.Matchers

import scala.collection.JavaConverters._


object ShouldNotCompile extends Matchers {

  def apply(source: String): Unit = {
    checkCase(Cpu.Mos, source)
    checkCase(Cpu.Z80, source)
    checkCase(Cpu.Motorola6809, source)
  }

  private def checkCase(cpu: Cpu.Value, source: String) {
    Console.out.flush()
    Console.err.flush()
    val log = TestErrorReporting.log
    println(source)
    val platform = EmuPlatform.get(cpu)
    val options = CompilationOptions(platform, Map(CompilationFlag.LenientTextEncoding -> true), None, platform.zpRegisterSize, Map(), JobContext(log, new LabelGenerator))
    log.hasErrors = false
    log.verbosity = 999
    var effectiveSource = source
    if (!source.contains("_panic")) effectiveSource += "\n void _panic(){while(true){}}"
    if (source.contains("call(")) {
      platform.cpuFamily match {
        case CpuFamily.M6502 =>
          effectiveSource += "\nnoinline asm word call(word ax) {\nJMP ((__reg.b2b3))\n}\n"
        case CpuFamily.M6809 =>
          effectiveSource += "\nnoinline asm word call(word x) {\nJMP ,x\n}\n"
        case CpuFamily.I80 =>
          if (options.flag(CompilationFlag.UseIntelSyntaxForInput))
            effectiveSource += "\nnoinline asm word call(word de) {\npush d\nret\n}\n"
          else effectiveSource += "\nnoinline asm word call(word de) {\npush de\nret\n}\n"
      }
    }
    if (source.contains("import zp_reg"))
      effectiveSource += Files.readAllLines(Paths.get("include/zp_reg.mfk"), StandardCharsets.US_ASCII).asScala.mkString("\n", "\n", "")
    log.setSource(Some(effectiveSource.linesIterator.toIndexedSeq))
    val PreprocessingResult(preprocessedSource, features, _) = Preprocessor.preprocessForTest(options, effectiveSource)
    val parserF =
      platform.cpuFamily match {
        case CpuFamily.M6502 =>
          MosParser("", preprocessedSource, "", options, features)
        case CpuFamily.M6809 =>
          M6809Parser("", preprocessedSource, "", options, features)
        case CpuFamily.I80 =>
          Z80Parser("", preprocessedSource, "", options, features, options.flag(CompilationFlag.UseIntelSyntaxForInput))
      }
    parserF.toAst match {
      case Success(program, _) =>
        log.assertNoErrors("Parse failed")

        // prepare
        val callGraph = new StandardCallGraph(program, log)
        val cpuFamily = CpuFamily.forType(cpu)
        val env = new Environment(None, "", cpuFamily, options)
        env.collectDeclarations(program, options)

        var unoptimizedSize = 0L
        // print unoptimized asm
        env.allPreallocatables.foreach {
          case f: NormalFunction =>
            val unoptimized = cpuFamily match {
              case CpuFamily.M6502 => MosCompiler.compile(CompilationContext(f.environment, f, 0, options, Set()))
              case CpuFamily.I80 => Z80Compiler.compile(CompilationContext(f.environment, f, 0, options, Set()))
              case CpuFamily.M6809 => M6809Compiler.compile(CompilationContext(f.environment, f, 0, options, Set()))
              case _ => Nil
            }
            unoptimizedSize += unoptimized.map(_.sizeInBytes).sum
          case d: InitializedArray =>
            unoptimizedSize += d.contents.length
          case d: InitializedMemoryVariable =>
            unoptimizedSize += d.typ.size
        }

        if (!log.hasErrors) {
          val env2 = new Environment(None, "", cpuFamily, options)
          env2.collectDeclarations(program, options)
          cpuFamily match {
            case CpuFamily.M6502 =>
              val assembler = new MosAssembler(program, env2, platform)
              val output = assembler.assemble(callGraph, Nil, options)
              output.asm.takeWhile(s => !(s.startsWith(".") && s.contains("= $"))).filterNot(_.contains("; DISCARD_")).foreach(println)
              fail("Failed: Compilation succeeded for 6502")
            case CpuFamily.I80 =>
              val assembler = new Z80Assembler(program, env2, platform)
              val output = assembler.assemble(callGraph, Nil, options)
              output.asm.takeWhile(s => !(s.startsWith(".") && s.contains("= $"))).filterNot(_.contains("; DISCARD_")).foreach(println)
              fail("Failed: Compilation succeeded for Z80")
            case CpuFamily.M6809 =>
              val assembler = new M6809Assembler(program, env2, platform)
              val output = assembler.assemble(callGraph, Nil, options)
              output.asm.takeWhile(s => !(s.startsWith(".") && s.contains("= $"))).filterNot(_.contains("; DISCARD_")).foreach(println)
              fail("Failed: Compilation succeeded for 6809")
            case _ =>
              fail("Failed: Compilation succeeded for unknown CPU")
          }
        }
        log.clearErrors()

      case f: Failure[_, _] =>
        println(f.extra.toString)
        println(f.lastParser.toString)
        log.error("Syntax error: " + parserF.lastLabel, Some(parserF.lastPosition))
        fail("syntax error")
    }
  }
}
