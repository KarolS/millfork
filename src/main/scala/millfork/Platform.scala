package millfork

import java.io.{File, StringReader}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import millfork.error.ErrorReporting
import millfork.output._
import org.apache.commons.configuration2.INIConfiguration

/**
  * @author Karol Stasiak
  */

object OutputStyle extends Enumeration {
  val Single, PerBank, LUnix = Value
}

class Platform(
                val cpu: Cpu.Value,
                val flagOverrides: Map[CompilationFlag.Value, Boolean],
                val startingModules: List[String],
                val outputPackager: OutputPackager,
                val codeAllocators: Map[String, UpwardByteAllocator],
                val variableAllocators: Map[String, VariableAllocator],
                val freeZpPointers: List[Int],
                val fileExtension: String,
                val generateBbcMicroInfFile: Boolean,
                val bankNumbers: Map[String, Int],
                val defaultCodeBank: String,
                val outputStyle: OutputStyle.Value
              ) {
  def cpuFamily: CpuFamily.Value = CpuFamily.forType(this.cpu)
}

object Platform {

  def lookupPlatformFile(includePath: List[String], platformName: String): Platform = {
    includePath.foreach { dir =>
      val file = Paths.get(dir, platformName + ".ini").toFile
      ErrorReporting.debug("Checking " + file)
      if (file.exists()) {
        return load(file)
      }
    }
    ErrorReporting.fatal(s"Platfom definition `$platformName` not found", None)
  }

  def load(file: File): Platform = {
    val conf = new INIConfiguration()
    val bytes = Files.readAllBytes(file.toPath)
    conf.read(new StringReader(new String(bytes, StandardCharsets.UTF_8)))

    val cs = conf.getSection("compilation")
    val cpu = Cpu.fromString(cs.get(classOf[String], "arch", "strict"))
    val value65816 = cs.get(classOf[String], "emit_65816", "")
    val flagOverrides = (value65816.toLowerCase match {
      case "" => Nil
      case "false" | "none" | "no" | "off" | "0" =>
        List(
          CompilationFlag.EmitEmulation65816Opcodes -> false,
          CompilationFlag.EmitNative65816Opcodes -> false,
          CompilationFlag.ReturnWordsViaAccumulator -> false)
      case "emulation" =>
        List(
          CompilationFlag.EmitEmulation65816Opcodes -> true,
          CompilationFlag.EmitNative65816Opcodes -> false,
          CompilationFlag.ReturnWordsViaAccumulator -> false)
      case "native" =>
        List(
          CompilationFlag.EmitEmulation65816Opcodes -> true,
          CompilationFlag.EmitNative65816Opcodes -> true)
      case _ =>
        ErrorReporting.error(s"Unsupported `emit_65816` value: $value65816")
        Nil
    }).toMap ++ CompilationFlag.fromString.flatMap { case (k, f) =>
      val value = cs.get(classOf[String], k, "")
      value.toLowerCase match {
        case "" | null => None
        case "false" | "off" | "no" | "0" => Some(f -> false)
        case "true" | "on" | "yes" | "1" => Some(f -> true)
        case _ =>
          ErrorReporting.error(s"Unsupported `$k` value: $value")
          None
      }
    }
    val startingModules = cs.get(classOf[String], "modules", "").split("[, ]+").filter(_.nonEmpty).toList

    val as = conf.getSection("allocation")

    val banks = as.get(classOf[String], "segments", "default").split("[, ]+").filter(_.nonEmpty).toList
    if (!banks.contains("default")) {
      ErrorReporting.error("A segment named `default` is required")
    }
    if (banks.toSet.size != banks.length) {
      ErrorReporting.error("Duplicate segment name")
    }
    val BankRegex = """\A[A-Za-z0-9_]+\z""".r
    banks.foreach {
      case BankRegex(_*) => // ok
      case b => ErrorReporting.error(s"Invalid segment name: `$b`")
    }

    val bankStarts = banks.map(b => b -> (as.get(classOf[String], s"segment_${b}_start") match {
      case "" | null => ErrorReporting.error(s"Undefined segment_${b}_start"); 0
      case x => parseNumber(x)
    })).toMap
    val bankDataStarts = banks.map(b => b -> (as.get(classOf[String], s"segment_${b}_datastart", "after_code") match {
      case "" | "after_code" => None
      case x => Some(parseNumber(x))
    })).toMap
    val bankEnds = banks.map(b => b -> (as.get(classOf[String], s"segment_${b}_end") match {
      case "" | null => ErrorReporting.error(s"Undefined segment_${b}_end"); 0xffff
      case x => parseNumber(x)
    })).toMap
    val bankCodeEnds = banks.map(b => b -> (as.get(classOf[String], s"segment_${b}_codeend", "") match {
      case "" => bankEnds(b)
      case x => parseNumber(x)
    })).toMap
    val defaultCodeBank = as.get(classOf[String], "default_code_segment") match {
      case "" | null => "default"
      case x => x
    }
    // used by 65816:
    val bankNumbers = banks.map(b => b -> (as.get(classOf[String], s"segment_${b}_bank", "00") match {
      case "" => 0
      case x => parseNumber(x)
    })).toMap

    // TODO: validate stuff
    banks.foreach(b => {
      if (bankNumbers(b) < 0 || bankNumbers(b) > 255) ErrorReporting.error(s"Segment $b has invalid bank")
      if (bankStarts(b) >= bankCodeEnds(b)) ErrorReporting.error(s"Segment $b has invalid range")
      if (bankCodeEnds(b) > bankEnds(b)) ErrorReporting.error(s"Segment $b has invalid range")
      if (bankStarts(b) >= bankEnds(b)) ErrorReporting.error(s"Segment $b has invalid range")
      bankDataStarts(b).foreach(dataStarts => if (dataStarts >= bankEnds(b)) ErrorReporting.error(s"Segment $b has invalid range"))
    })

    val freePointers = as.get(classOf[String], "zp_pointers", "all") match {
      case "all" => List.tabulate(128)(_ * 2)
      case xs => xs.split("[, ]+").flatMap(parseNumberOrRange).toList
    }

    val codeAllocators = banks.map(b => b -> new UpwardByteAllocator(bankStarts(b), bankCodeEnds(b)))
    val variableAllocators = banks.map(b => b -> new VariableAllocator(
      if (b == "default" && CpuFamily.forType(cpu) == CpuFamily.M6502) freePointers else Nil, bankDataStarts(b) match {
        case None => new AfterCodeByteAllocator(bankEnds(b))
        case Some(start) => new UpwardByteAllocator(start, bankEnds(b))
      }))

    val os = conf.getSection("output")
    val outputPackager = SequenceOutput(os.get(classOf[String], "format", "").split("[, ]+").filter(_.nonEmpty).map {
      case "startaddr" => StartAddressOutput
      case "startpage" => StartPageOutput
      case "endaddr" => EndAddressOutput
      case "pagecount" => PageCountOutput
      case "allocated" => AllocatedDataOutput
      case "d88" => D88Output
      case n => n.split(":").filter(_.nonEmpty) match {
        case Array(b, s, e) => BankFragmentOutput(b, parseNumber(s), parseNumber(e))
        case Array(s, e) => CurrentBankFragmentOutput(parseNumber(s), parseNumber(e))
        case Array(b) => ConstOutput(parseNumber(b).toByte)
        case x => ErrorReporting.fatal(s"Invalid output format: `$x`")
      }
    }.toList)
    val fileExtension = os.get(classOf[String], "extension", ".bin")
    val generateBbcMicroInfFile = os.get(classOf[Boolean], "bbc_inf", false)
    val outputStyle = os.get(classOf[String], "style", "single") match {
      case "" | "single" => OutputStyle.Single
      case "per_bank" | "per_segment" => OutputStyle.PerBank
      case "lunix" => OutputStyle.LUnix
      case x => ErrorReporting.fatal(s"Invalid output style: `$x`")
    }

    new Platform(cpu, flagOverrides, startingModules, outputPackager,
      codeAllocators.toMap,
      variableAllocators.toMap,
      freePointers,
      if (fileExtension == "" || fileExtension.startsWith(".")) fileExtension else "." + fileExtension,
      generateBbcMicroInfFile,
      bankNumbers,
      defaultCodeBank,
      outputStyle)
  }

  def parseNumberOrRange(s:String): Seq[Int] = {
    if (s.contains("-")) {
      var segments = s.split("-")
      if (segments.length != 2) {
        ErrorReporting.fatal(s"Invalid range: `$s`")
      }
      Range(parseNumber(segments(0)), parseNumber(segments(1)), 2)
    } else {
      Seq(parseNumber(s))
    }
  }

  def parseNumber(s: String): Int = {
    if (s.startsWith("$")) {
      Integer.parseInt(s.substring(1), 16)
    } else if (s.startsWith("0x")) {
      Integer.parseInt(s.substring(2), 16)
    } else if (s.startsWith("0X")) {
      Integer.parseInt(s.substring(2), 16)
    } else if (s.startsWith("%")) {
      Integer.parseInt(s.substring(1), 2)
    } else if (s.startsWith("0b")) {
      Integer.parseInt(s.substring(2), 2)
    } else if (s.startsWith("0B")) {
      Integer.parseInt(s.substring(2), 2)
    } else if (s.startsWith("0o")) {
      Integer.parseInt(s.substring(2), 8)
    } else if (s.startsWith("0O")) {
      Integer.parseInt(s.substring(2), 8)
    } else if (s.startsWith("0q")) {
      Integer.parseInt(s.substring(2), 4)
    } else if (s.startsWith("0Q")) {
      Integer.parseInt(s.substring(2), 4)
    } else {
      s.toInt
    }
  }
}