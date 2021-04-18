package millfork.test.emu

import fastparse.core.Parsed.{Failure, Success}
import millfork._
import millfork.compiler.m6809.M6809Compiler
import millfork.compiler.mos.MosCompiler
import millfork.compiler.z80.Z80Compiler
import millfork.compiler.{CompilationContext, LabelGenerator}
import millfork.env.{Environment, InitializedArray, InitializedMemoryVariable, NormalFunction}
import millfork.node.StandardCallGraph
import millfork.output.{M6809Assembler, MosAssembler, Z80Assembler}
import millfork.parser._
import org.scalatest.Matchers

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.collection.JavaConverters._


object ShouldParse extends Matchers {

  def apply(source: String, cpus: Iterable[Cpu.Value] = Set(Cpu.Mos)): Unit = {
    for (cpu <- cpus) {
      checkCase(cpu, source)
    }
  }

  private def checkCase(cpu: Cpu.Value, source: String) {
    Console.out.flush()
    Console.err.flush()
    val log = TestErrorReporting.log
    val platform = EmuPlatform.get(cpu)
    val flags = CpuFamily.forType(cpu) match {
      case CpuFamily.M6809 => Map(CompilationFlag.LenientTextEncoding -> true, CompilationFlag.UseUForStack -> true)
      case _ => Map(CompilationFlag.LenientTextEncoding -> true)
    }
    val options = CompilationOptions(platform, flags, None, platform.zpRegisterSize, Map(), EmuPlatform.textCodecRepository, JobContext(log, new LabelGenerator))
    log.hasErrors = false
    log.verbosity = 0
    val PreprocessingResult(preprocessedSource, features, _) = Preprocessor.preprocessForTest(options, source)
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
      case f: Failure[_, _] =>
        println(f.extra.toString)
        println(f.lastParser.toString)
        if (parserF.lastLabel != "") {
          options.log.error(s"Syntax error: ${parserF.lastLabel} expected", Some(parserF.lastPosition))
        } else {
          options.log.error("Syntax error", Some(parserF.lastPosition))
        }
        fail("syntax error")
    }
  }
}
