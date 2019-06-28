package millfork.test.emu

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import fastparse.core.Parsed.{Failure, Success}
import millfork.compiler.{CompilationContext, LabelGenerator}
import millfork.compiler.mos.MosCompiler
import millfork.env.{Environment, InitializedArray, InitializedMemoryVariable, NormalFunction}
import millfork.node.StandardCallGraph
import millfork.parser.{MosParser, PreprocessingResult, Preprocessor}
import millfork._
import org.scalatest.Matchers

import scala.collection.JavaConverters._


object ShouldNotCompile extends Matchers {

  def apply(source: String): Unit = {
    checkCase(Cpu.Mos, source)
    checkCase(Cpu.Z80, source)
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
    if (source.contains("import zp_reg"))
      effectiveSource += Files.readAllLines(Paths.get("include/zp_reg.mfk"), StandardCharsets.US_ASCII).asScala.mkString("\n", "\n", "")
    log.setSource(Some(effectiveSource.linesIterator.toIndexedSeq))
    val PreprocessingResult(preprocessedSource, features, _) = Preprocessor.preprocessForTest(options, effectiveSource)
    val parserF = MosParser("", preprocessedSource, "", options, features)
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
            val unoptimized = MosCompiler.compile(CompilationContext(f.environment, f, 0, options, Set()))
            unoptimizedSize += unoptimized.map(_.sizeInBytes).sum
          case d: InitializedArray =>
            unoptimizedSize += d.contents.length
          case d: InitializedMemoryVariable =>
            unoptimizedSize += d.typ.size
        }

        if (!log.hasErrors) {
          val familyName = cpuFamily match {
            case CpuFamily.M6502 => "6502"
            case CpuFamily.I80 => "Z80"
            case _ => "unknown CPU"
          }
          fail("Failed: Compilation succeeded for " + familyName)
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
