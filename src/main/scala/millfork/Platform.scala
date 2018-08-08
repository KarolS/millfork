package millfork

import java.io.{File, StringReader}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import millfork.error.Logger
import millfork.output._
import millfork.parser.TextCodec
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
                val defaultCodec: TextCodec,
                val screenCodec: TextCodec,
                val features: Map[String, Long],
                val outputPackager: OutputPackager,
                val codeAllocators: Map[String, UpwardByteAllocator],
                val variableAllocators: Map[String, VariableAllocator],
                val zpRegisterSize: Int,
                val freeZpPointers: List[Int],
                val fileExtension: String,
                val generateBbcMicroInfFile: Boolean,
                val bankNumbers: Map[String, Int],
                val defaultCodeBank: String,
                val outputStyle: OutputStyle.Value
              ) {
  def hasZeroPage: Boolean = cpuFamily == CpuFamily.M6502

  def cpuFamily: CpuFamily.Value = CpuFamily.forType(this.cpu)
}

object Platform {

  def lookupPlatformFile(includePath: List[String], platformName: String)(implicit log: Logger): Platform = {
    includePath.foreach { dir =>
      val file = Paths.get(dir, platformName + ".ini").toFile
      log.debug("Checking " + file)
      if (file.exists()) {
        return load(file)
      }
    }
    log.fatal(s"Platfom definition `$platformName` not found", None)
  }

  def load(file: File)(implicit log: Logger): Platform = {
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
        log.error(s"Unsupported `emit_65816` value: $value65816")
        Nil
    }).toMap ++ CompilationFlag.fromString.flatMap { case (k, f) =>
      val value = cs.get(classOf[String], k, "")
      value.toLowerCase match {
        case "" | null => None
        case "false" | "off" | "no" | "0" => Some(f -> false)
        case "true" | "on" | "yes" | "1" => Some(f -> true)
        case _ =>
          log.error(s"Unsupported `$k` value: $value")
          None
      }
    }
    val startingModules = cs.get(classOf[String], "modules", "").split("[, ]+").filter(_.nonEmpty).toList
    val zpRegisterSize = cs.get(classOf[String], "zeropage_register", "").toLowerCase match {
      case "" | null => if (CpuFamily.forType(cpu) == CpuFamily.M6502) 4 else 0
      case "true" | "on" | "yes" => 4
      case "false" | "off" | "no" | "0" => 0
      case x => x.toInt
    }
    if (zpRegisterSize < 0 || zpRegisterSize > 128) {
      log.error("Invalid zeropage register size: " + zpRegisterSize)
    }

    val codecName = cs.get(classOf[String], "encoding", "ascii")
    val srcCodecName = cs.get(classOf[String], "screen_encoding", codecName)
    val (codec, czt) = TextCodec.forName(codecName, None, log)
    if (czt) {
      log.error("Default encoding cannot be zero-terminated")
    }
    val (srcCodec, szt) = TextCodec.forName(srcCodecName, None, log)
    if (szt) {
      log.error("Default screen encoding cannot be zero-terminated")
    }

    val as = conf.getSection("allocation")

    val banks = as.get(classOf[String], "segments", "default").split("[, ]+").filter(_.nonEmpty).toList
    if (!banks.contains("default")) {
      log.error("A segment named `default` is required")
    }
    if (banks.toSet.size != banks.length) {
      log.error("Duplicate segment name")
    }
    val BankRegex = """\A[A-Za-z0-9_]+\z""".r
    banks.foreach {
      case BankRegex(_*) => // ok
      case b => log.error(s"Invalid segment name: `$b`")
    }

    val bankStarts = banks.map(b => b -> (as.get(classOf[String], s"segment_${b}_start") match {
      case "" | null => log.error(s"Undefined segment_${b}_start"); 0
      case x => parseNumber(x)
    })).toMap
    val bankDataStarts = banks.map(b => b -> (as.get(classOf[String], s"segment_${b}_datastart", "after_code") match {
      case "" | "after_code" => None
      case x => Some(parseNumber(x))
    })).toMap
    val bankEnds = banks.map(b => b -> (as.get(classOf[String], s"segment_${b}_end") match {
      case "" | null => log.error(s"Undefined segment_${b}_end"); 0xffff
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
      if (bankNumbers(b) < 0 || bankNumbers(b) > 255) log.error(s"Segment $b has invalid bank")
      if (bankStarts(b) >= bankCodeEnds(b)) log.error(s"Segment $b has invalid range")
      if (bankCodeEnds(b) > bankEnds(b)) log.error(s"Segment $b has invalid range")
      if (bankStarts(b) >= bankEnds(b)) log.error(s"Segment $b has invalid range")
      bankDataStarts(b).foreach(dataStarts => if (dataStarts >= bankEnds(b)) log.error(s"Segment $b has invalid range"))
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
      case "tap" => TapOutput
      case n => n.split(":").filter(_.nonEmpty) match {
        case Array(b, s, e) => BankFragmentOutput(b, parseNumber(s), parseNumber(e))
        case Array(s, e) => CurrentBankFragmentOutput(parseNumber(s), parseNumber(e))
        case Array(b) => ConstOutput(parseNumber(b).toByte)
        case x => log.fatal(s"Invalid output format: `$x`")
      }
    }.toList)
    val fileExtension = os.get(classOf[String], "extension", ".bin")
    val generateBbcMicroInfFile = os.get(classOf[Boolean], "bbc_inf", false)
    val outputStyle = os.get(classOf[String], "style", "single") match {
      case "" | "single" => OutputStyle.Single
      case "per_bank" | "per_segment" => OutputStyle.PerBank
      case "lunix" => OutputStyle.LUnix
      case x => log.fatal(s"Invalid output style: `$x`")
    }

    val builtInFeatures = builtInCpuFeatures(cpu)

    import scala.collection.JavaConverters._
    val ds = conf.getSection("define")
    val definedFeatures = ds.getKeys().asScala.toList.map { k =>
      val value = ds.get(classOf[String], k).trim() match {
        case "true" | "on" | "yes" => 1L
        case "false" | "off" | "no" | "" => 0L
        case x => x.toLong
      }
      k -> value
    }.toMap

    new Platform(
      cpu,
      flagOverrides,
      startingModules,
      codec,
      srcCodec,
      builtInFeatures ++ definedFeatures,
      outputPackager,
      codeAllocators.toMap,
      variableAllocators.toMap,
      zpRegisterSize,
      freePointers,
      if (fileExtension == "" || fileExtension.startsWith(".")) fileExtension else "." + fileExtension,
      generateBbcMicroInfFile,
      bankNumbers,
      defaultCodeBank,
      outputStyle)
  }

  def builtInCpuFeatures(cpu: Cpu.Value): Map[String, Long] = {
    @inline
    def toLong(b: Boolean): Long = if (b) 1L else 0L
    Map[String, Long](
      "ARCH_6502" -> toLong(CpuFamily.forType(cpu) == CpuFamily.M6502),
      "CPU_6502" -> toLong(Set(Cpu.Mos, Cpu.StrictMos, Cpu.Ricoh, Cpu.StrictRicoh)(cpu)),
      "CPU_65C02" -> toLong(cpu == Cpu.Cmos),
      "CPU_65CE02" -> toLong(cpu == Cpu.CE02),
      "CPU_65816" -> toLong(cpu == Cpu.Sixteen),
      "CPU_HUC6280" -> toLong(cpu == Cpu.HuC6280),
      "ARCH_I80" -> toLong(CpuFamily.forType(cpu) == CpuFamily.I80),
      "CPU_Z80" -> toLong(cpu == Cpu.Z80),
      "CPU_EZ80" -> toLong(cpu == Cpu.EZ80),
      "CPU_8080" -> toLong(cpu == Cpu.Intel8080),
      "CPU_GAMEBOY" -> toLong(cpu == Cpu.Sharp),
      "ARCH_X86" -> toLong(CpuFamily.forType(cpu) == CpuFamily.I86),
      "ARCH_6800" -> toLong(CpuFamily.forType(cpu) == CpuFamily.M6800),
      "ARCH_ARM" -> toLong(CpuFamily.forType(cpu) == CpuFamily.ARM),
      "ARCH_68K" -> toLong(CpuFamily.forType(cpu) == CpuFamily.M68K)
      // TODO
    )
  }

  def parseNumberOrRange(s:String)(implicit log: Logger): Seq[Int] = {
    if (s.contains("-")) {
      val segments = s.split("-")
      if (segments.length != 2) {
        log.fatal(s"Invalid range: `$s`")
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