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

class Platform(
                val cpu: Cpu.Value,
                val flagOverrides: Map[CompilationFlag.Value, Boolean],
                val startingModules: List[String],
                val outputPackager: OutputPackager,
                val codeAllocator: UpwardByteAllocator,
                val variableAllocator: VariableAllocator,
                val fileExtension: String,
                var defaultCodeBank: Int = 0,
              )

object Platform {

  val C64 = new Platform(
    Cpu.Mos,
    Map(),
    List("c64_hardware", "c64_loader"),
    SequenceOutput(List(StartAddressOutput, AllocatedDataOutput)),
    new UpwardByteAllocator(0x80D, 0xA000),
    new VariableAllocator(
      List(0xC1, 0xC3, 0xFB, 0xFD, 0x39, 0x3B, 0x3D, 0x43, 0x4B),
      new AfterCodeByteAllocator(0xA000)
    ),
    ".prg"
  )

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
        case "" => None
        case "false" | "off" | "no" | "0" => Some(f -> false)
        case "true" | "on" | "yes" | "1" => Some(f -> true)
        case _ =>
          ErrorReporting.error(s"Unsupported `$k` value: $value")
          None
      }
    }
    val startingModules = cs.get(classOf[String], "modules", "").split("[, ]+").filter(_.nonEmpty).toList

    val as = conf.getSection("allocation")
    val org = as.get(classOf[String], "main_org", "") match {
      case "" => ErrorReporting.fatal(s"Undefined main_org")
      case m => parseNumber(m)
    }
    val freePointers = as.get(classOf[String], "zp_pointers", "all") match {
      case "all" => List.tabulate(128)(_ * 2)
      case xs => xs.split("[, ]+").map(parseNumber).toList
    }
    val himemEnd = as.get(classOf[String], "himem_end", "") match {
      case "" => ErrorReporting.fatal(s"Undefined himem_end")
      case end => parseNumber(end) + 1
    }
    val byteAllocator = as.get(classOf[String], "himem_start", "") match {
      case "" => ErrorReporting.fatal(s"Undefined himem_start")
      case "after_code" => new AfterCodeByteAllocator(himemEnd)
      case start => new UpwardByteAllocator(parseNumber(start), himemEnd)
    }

    val os = conf.getSection("output")
    val outputPackager = SequenceOutput(os.get(classOf[String], "format", "").split("[, ]+").filter(_.nonEmpty).map {
      case "startaddr" => StartAddressOutput
      case "endaddr" => EndAddressOutput
      case "allocated" => AllocatedDataOutput
      case n => n.split(":").filter(_.nonEmpty) match {
        case Array(b, s, e) => BankFragmentOutput(parseNumber(b), parseNumber(s), parseNumber(e))
        case Array(s, e) => CurrentBankFragmentOutput(parseNumber(s), parseNumber(e))
        case Array(b) => ConstOutput(parseNumber(b).toByte)
        case x => ErrorReporting.fatal(s"Invalid output format: `$x`")
      }
    }.toList)
    var fileExtension = os.get(classOf[String], "extension", ".bin")

    new Platform(cpu, flagOverrides, startingModules, outputPackager,
      new UpwardByteAllocator(org, 0xffff),
      new VariableAllocator(freePointers, byteAllocator),
      if (fileExtension.startsWith(".")) fileExtension else "." + fileExtension)
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