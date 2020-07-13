package millfork.test.auxilary

import java.io.File

import millfork.{CompilationOptions, Cpu, JobContext}
import millfork.compiler.LabelGenerator
import millfork.error.ConsoleLogger
import millfork.parser.{TableTextCodec, TextCodecRepository, TextCodecWithFlags}
import millfork.test.emu.EmuPlatform
import org.scalatest.{FunSuite, Matchers}

/**
  * @author Karol Stasiak
  */
class EncodingSanitySuite extends FunSuite with Matchers {


  test("Encoding sanity test") {
    val log = new ConsoleLogger()
    val repo = new TextCodecRepository(List("include"))
    val options = CompilationOptions(EmuPlatform.get(Cpu.Intel8080), Map(), None, 0, Map(), new TextCodecRepository(List("D:/dokumenty/millfork/include/encoding")), JobContext(log, new LabelGenerator))

    def roundtrip(codec: TableTextCodec, str1: String, str2: String): Unit = {
      val l1 = codec.encode(log, None, str1.toCharArray.map(_.toInt).toList, options, lenient = false)
      val l2 = codec.encode(log, None, str2.toCharArray.map(_.toInt).toList, options, lenient = false)
      if (l1 != l2) {
        fail(s"Strings $str1 and $str2 encoded to $l1 and $l2 in encoding ${codec.name}")
      }
    }

    for (encoding <- new File("include/encoding").list()) {
      repo.forName(encoding.stripSuffix(".tbl"), None, log).codec match {
        case codec:TableTextCodec =>
          codec.escapeSequences.foreach {
            case ("copy", _) => roundtrip(codec, "{copy}", "©")
            case ("ss", _) => roundtrip(codec, "{ss}", "ß")
            case ("pi", _) => roundtrip(codec, "{pi}", "π")
            case ("yen", _) => roundtrip(codec, "{yen}", "¥")
            case ("pound", _) => roundtrip(codec, "{pound}", "£")
            case ("cent", _) => roundtrip(codec, "{cent}", "¢")
            case _ =>
          }
        case _ =>

      }
      log.assertNoErrors("No errors")
    }
  }


}
