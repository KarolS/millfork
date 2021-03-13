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
    val options = CompilationOptions(EmuPlatform.get(Cpu.Intel8080), Map(), None, 0, Map(), new TextCodecRepository(List("include")), JobContext(log, new LabelGenerator))

    def roundtrip(codec: TableTextCodec, str1: String, str2: String): Unit = {
      val l1 = codec.encode(log, None, str1.toCharArray.map(_.toInt).toList, options, lenient = false)
      val l2 = codec.encode(log, None, str2.toCharArray.map(_.toInt).toList, options, lenient = false)
      if (l1 != l2) {
        fail(s"Strings $str1 and $str2 encoded to $l1 and $l2 in encoding ${codec.name}")
      }
    }
    def getSingleByte(codec: TableTextCodec, str1: String): Int = {
      val l1 = codec.encode(log, None, str1.toCharArray.map(_.toInt).toList, options, lenient = false)
      if (l1.size != 1) {
        fail(s"String $str1 encoded to $l1 in encoding ${codec.name}")
      }
      l1.head
    }

    val allEncodingFiles = new File("./include/encoding").list()
    log.info(s"Testing ${allEncodingFiles.size} encodings")
    for (encoding <- allEncodingFiles) {
      log.info(s"Testing $encoding")
      repo.forName(encoding.stripSuffix(".tbl"), None, log).codec match {
        case codec:TableTextCodec =>
          codec.escapeSequences.foreach {
            case ("copy", _) => roundtrip(codec, "{copy}", "©")
            case ("ss", _) => roundtrip(codec, "{ss}", "ß")
            case ("pi", _) => roundtrip(codec, "{pi}", "π")
            case ("yen", _) => roundtrip(codec, "{yen}", "¥")
            case ("pound", _) => roundtrip(codec, "{pound}", "£")
            case ("cent", _) => roundtrip(codec, "{cent}", "¢")
            case ("euro", _) => roundtrip(codec, "{euro}", "€")
            case ("apos", _) => roundtrip(codec, "{apos}", "'")
            case ("q", _) => roundtrip(codec, "{q}", "\"")
            case _ =>
          }
          codec.map.foreach {
            case '©' => roundtrip(codec, "{copy}", "©")
            case 'ß' => roundtrip(codec, "{ss}", "ß")
            case 'π' => roundtrip(codec, "{pi}", "π")
            case '¥' => roundtrip(codec, "{yen}", "¥")
            case '£' => roundtrip(codec, "{pound}", "£")
            case '¢' => roundtrip(codec, "{cent}", "¢")
            case '€' => roundtrip(codec, "{euro}", "€")
            case '\'' => roundtrip(codec, "{apos}", "'")
            case '"' => roundtrip(codec, "{q}", "\"")
            case '{' => getSingleByte(codec, "{lbrace}") should equal (codec.map.indexOf('{'))
            case '}' => getSingleByte(codec, "{rbrace}") should equal (codec.map.indexOf('}'))
            case _ =>
          }
        case _ =>

      }
      log.assertNoErrors("No errors")
    }
  }


}
