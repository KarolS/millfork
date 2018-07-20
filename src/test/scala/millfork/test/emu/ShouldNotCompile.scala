package millfork.test.emu

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import fastparse.core.Parsed.{Failure, Success}
import millfork.compiler.CompilationContext
import millfork.compiler.mos.MosCompiler
import millfork.env.{Environment, InitializedArray, InitializedMemoryVariable, NormalFunction}
import millfork.error.ErrorReporting
import millfork.node.StandardCallGraph
import millfork.parser.{MosParser, Preprocessor}
import millfork.{CompilationFlag, CompilationOptions, Cpu, CpuFamily}
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
    println(source)
    val platform = EmuPlatform.get(cpu)
    val options = CompilationOptions(platform, Map(CompilationFlag.LenientTextEncoding -> true), None, platform.zpRegisterSize)
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
      case Success(program, _) =>
        ErrorReporting.assertNoErrors("Parse failed")

        // prepare
        val callGraph = new StandardCallGraph(program)
        val cpuFamily = CpuFamily.forType(cpu)
        val env = new Environment(None, "", cpuFamily)
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

        if (!ErrorReporting.hasErrors) {
          val familyName = cpuFamily match {
            case CpuFamily.M6502 => "6502"
            case CpuFamily.I80 => "Z80"
            case _ => "unknown CPU"
          }
          fail("Failed: Compilation succeeded for " + familyName)
        }
        ErrorReporting.clearErrors()

      case f: Failure[_, _] =>
        println(f.extra.toString)
        println(f.lastParser.toString)
        ErrorReporting.error("Syntax error: " + parserF.lastLabel, Some(parserF.lastPosition))
        fail("syntax error")
    }
  }
}
