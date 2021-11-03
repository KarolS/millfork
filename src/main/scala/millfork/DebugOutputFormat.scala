package millfork

import millfork.output.{BankLayoutInFile, FormattableLabel}

import java.util.regex.Pattern
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