package millfork

import millfork.output.{BankLayoutInFile, FormattableLabel}

import java.util.regex.Pattern
import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

/**
  * @author Karol Stasiak
  */

object DebugOutputFormat {
  val map: Map[String, DebugOutputFormat] = Map(
    "raw" -> RawDebugOutputFormat,
    "vice" -> ViceDebugOutputFormat,
    "nesasm" -> NesasmDebugOutputFormat,
    "fns" -> NesasmDebugOutputFormat,
    "fceux" -> FceuxDebugOutputFormat,
    "nl" -> FceuxDebugOutputFormat,
    "mlb" -> MesenOutputFormat,
    "mesen" -> MesenOutputFormat,
    "asm6f" -> MesenOutputFormat,
    "ld65" -> Ld65OutputFormat,
    "ca65" -> Ld65OutputFormat,
    "cc65" -> Ld65OutputFormat,
    "dbg" -> Ld65OutputFormat,
    "sym" -> SymDebugOutputFormat)
}

sealed trait DebugOutputFormat {

  def formatAll(b: BankLayoutInFile, labels: Seq[FormattableLabel], breakpoints: Seq[(Int, Int)]): String = {
    val labelPart = labelsHeader + labels.map(formatLine).mkString("\n") + "\n"
    if (breakpoints.isEmpty) {
      labelPart
    } else {
      labelPart + breakpointsHeader + breakpoints.flatMap(formatBreakpointTupled).mkString("\n") + "\n"
    }
  }

  def formatLine(label: FormattableLabel): String

  final def formatBreakpointTupled(value: (Int, Int)): Seq[String] = formatBreakpoint(value._1, value._2).toSeq

  def formatBreakpoint(bank: Int, value: Int): Option[String]

  def fileExtension(bank: Int): String

  def filePerBank: Boolean

  //noinspection MutatorLikeMethodIsParameterless
  def addOutputExtension: Boolean

  def labelsHeader: String = ""

  def breakpointsHeader: String = ""
}

object RawDebugOutputFormat extends DebugOutputFormat {
  override def formatLine(label: FormattableLabel): String = {
    f"${label.bankNumber}%02X:${label.startValue}%04X:${label.endValue.fold("")(_.formatted("%04X"))}%s:${label.category}%s:$label%s"
  }

  override def fileExtension(bank: Int): String = ".labels"

  override def filePerBank: Boolean = false

  override def addOutputExtension: Boolean = false

  override def formatBreakpoint(bank: Int, value: Int): Option[String] =
    Some(f"$bank%02X:$value%04X::b:<breakpoint@$value%04X>")
}

object ViceDebugOutputFormat extends DebugOutputFormat {
  override def formatLine(label: FormattableLabel): String = {
    val normalized = label.labelName.replace('$', '_').replace('.', '_')
    s"al ${label.startValue.toHexString} .$normalized"
  }

  override def fileExtension(bank: Int): String = ".lbl"

  override def filePerBank: Boolean = false

  override def addOutputExtension: Boolean = false

  override def formatBreakpoint(bank: Int, value: Int): Option[String] = Some(s"break ${value.toHexString}")
}

object NesasmDebugOutputFormat extends DebugOutputFormat {
  override def formatLine(label: FormattableLabel): String = {
    label.labelName + " = $" + label.startValue.toHexString
  }

  override def fileExtension(bank: Int): String = ".fns"

  override def filePerBank: Boolean = false

  override def addOutputExtension: Boolean = false

  override def formatBreakpoint(bank: Int, value: Int): Option[String] = None
}

object SymDebugOutputFormat extends DebugOutputFormat {
  override def formatLine(label: FormattableLabel): String = {
    f"${label.bankNumber}%02x:${label.startValue}%04x ${label.labelName}%s"
  }

  override def fileExtension(bank: Int): String = ".sym"

  override def filePerBank: Boolean = false

  override def addOutputExtension: Boolean = false

  override def formatBreakpoint(bank: Int, value: Int): Option[String] = Some(f"$bank%02x:$value%04x")

  override def labelsHeader: String = "[labels]\n"

  override def breakpointsHeader: String = "[breakpoints]\n"
}

object FceuxDebugOutputFormat extends DebugOutputFormat {
  override def formatLine(label: FormattableLabel): String = {
    f"$$${label.startValue}%04x#${label.labelName}%s#"
  }

  override def fileExtension(bank: Int): String = if (bank == 0xff) ".ram.nl" else s".$bank.nl"

  override def filePerBank: Boolean = true

  override def addOutputExtension: Boolean = true

  override def formatBreakpoint(bank: Int, value: Int): Option[String] = None
}

object MesenOutputFormat extends DebugOutputFormat {

  override def formatAll(b: BankLayoutInFile, labels: Seq[FormattableLabel], breakpoints: Seq[(Int, Int)]): String = {
    val allStarts = labels.groupBy(_.bankName).mapValues(_.map(_.startValue).toSet)
    labels.flatMap{ l =>
      val mesenShift = b.getMesenShift(l.bankName)
      val shiftedStart = l.startValue + mesenShift
      var shiftedEnd = l.endValue.map(_ + mesenShift).filter(_ != shiftedStart)
      l.endValue match {
        case None =>
        case Some(e) =>
          // Mesen does not like labels of form XXX-XXX, where both ends are equal
          breakable {
            for (i <- l.startValue.+(1) to e) {
              if (allStarts.getOrElse(l.bankName, Set.empty).contains(i)) {
                shiftedEnd = None
                break
              }
            }
          }
      }
      if (shiftedStart >= 0 && shiftedEnd.forall(_ >= 0)) {
        val normalized = l.labelName.replace('$', '_').replace('.', '_')
        val comment = (l.category match {
          case 'F' => "function "
          case 'A' => "initialized array "
          case 'V' => "initialized variable "
          case 'a' => "array "
          case 'v' => "variable "
          case _ => ""
        }) + l.labelName
        Some(f"${l.mesenSymbol}%s:${shiftedStart}%04X${shiftedEnd.fold("")(e => f"-$e%04X")}%s:$normalized%s:$comment%s")
      } else {
        None
      }
    }.mkString("\n")
  }

  override def formatLine(label: FormattableLabel): String = throw new UnsupportedOperationException()

  override def formatBreakpoint(bank: Int, value: Int): Option[String] = None

  override def fileExtension(bank: Int): String = ".mlb"

  override def filePerBank: Boolean = false

  override def addOutputExtension: Boolean = false
}

object Ld65OutputFormat extends DebugOutputFormat {
  override def formatLine(label: FormattableLabel): String = throw new UnsupportedOperationException()

  override def formatBreakpoint(bank: Int, value: Int): Option[String] = None

  override def fileExtension(bank: Int): String = ".dbg"

  override def filePerBank: Boolean = false

  override def addOutputExtension: Boolean = true

  override def formatAll(b: BankLayoutInFile, labels: Seq[FormattableLabel], breakpoints: Seq[(Int, Int)]): String = {
    val q = '"'
    val allBanksInFile = b.allBanks()
    val allBanks = (labels.map(_.bankName).distinct ++ allBanksInFile).distinct
    val result = mutable.ListBuffer[String]()
    result += "version\tmajor=2,minor=0"
    result += s"info\tcsym=0,file=0,lib=0,line=0,mod=0,scope=1,seg=${allBanks.size},span=0,sym=${labels.size},type=4"
    for ((bank, ix) <- allBanks.zipWithIndex) {
      val common =  s"seg\tid=${ix},name=$q${bank}$q,start=0x${b.getStart(bank).toHexString},size=0x0,addrsize=absolute"
      if (allBanksInFile.contains(bank)) {
        result += common + s",ooffs=${b.ld65Offset(bank)}"
      } else {
        result += common
      }
    }
    result += "scope\tid=0,name=\"\""
    for ((label, ix) <- labels.sortBy(l => (l.bankName, l.startValue, l.labelName)).zipWithIndex) {
      val name = label.labelName.replace('$', '@').replace('.', '@')
      val sb = new StringBuilder
      sb ++= s"sym\tid=${ix},name=$q${name}$q,addrsize=absolute,"
      label.endValue match {
        case Some(e) =>
          if (!labels.exists(l => l.ne(label) && l.startValue >= label.startValue && l.startValue <= e)) {
            sb ++= s"size=${ e - label.startValue + 1 },"
          }
        case None =>
      }
      sb ++= s"scope=0,val=0x${label.startValue.toHexString},seg=${allBanks.indexOf(label.bankName)},type=lab"
      result += sb.toString
    }
    result.mkString("\n")
  }
}
