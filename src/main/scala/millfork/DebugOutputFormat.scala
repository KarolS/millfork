package millfork

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
    "sym" -> SymDebugOutputFormat)
}

sealed trait DebugOutputFormat {

  def formatAll(labels: Seq[(String, Int, Int, Char, Option[Int])], breakpoints: Seq[(Int, Int)]): String = {
    val labelPart = labelsHeader + labels.map(formatLineTupled).mkString("\n") + "\n"
    if (breakpoints.isEmpty) {
      labelPart
    } else {
      labelPart + breakpointsHeader + breakpoints.flatMap(formatBreakpointTupled).mkString("\n") + "\n"
    }
  }

  final def formatLineTupled(labelAndValue: (String, Int, Int, Char, Option[Int])): String =
    formatLine(labelAndValue._1, labelAndValue._2, labelAndValue._3, labelAndValue._4, labelAndValue._5)

  def formatLine(label: String, bank: Int, startValue: Int, category: Char, endValue: Option[Int]): String

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
  override def formatLine(label: String, bank: Int, startValue: Int, category: Char, endValue: Option[Int]): String = {
    f"$bank%02X:$startValue%04X:${endValue.fold("")(_.formatted("%04X"))}%s:$category%s:$label%s"
  }

  override def fileExtension(bank: Int): String = ".labels"

  override def filePerBank: Boolean = false

  override def addOutputExtension: Boolean = false

  override def formatBreakpoint(bank: Int, value: Int): Option[String] =
    Some(f"$bank%02X:$value%04X::b:<breakpoint@$value%04X>")
}

object ViceDebugOutputFormat extends DebugOutputFormat {
  override def formatLine(label: String, bank: Int, startValue: Int, category: Char, endValue: Option[Int]): String = {
    val normalized = label.replace('$', '_').replace('.', '_')
    s"al ${startValue.toHexString} .$normalized"
  }

  override def fileExtension(bank: Int): String = ".lbl"

  override def filePerBank: Boolean = false

  override def addOutputExtension: Boolean = false

  override def formatBreakpoint(bank: Int, value: Int): Option[String] = Some(s"break ${value.toHexString}")
}

object NesasmDebugOutputFormat extends DebugOutputFormat {
  override def formatLine(label: String, bank: Int, startValue: Int, category: Char, endValue: Option[Int]): String = {
    label + " = $" + startValue.toHexString
  }

  override def fileExtension(bank: Int): String = ".fns"

  override def filePerBank: Boolean = false

  override def addOutputExtension: Boolean = false

  override def formatBreakpoint(bank: Int, value: Int): Option[String] = None
}

object SymDebugOutputFormat extends DebugOutputFormat {
  override def formatLine(label: String, bank: Int, startValue: Int, category: Char, endValue:Option[Int]): String = {
    f"$bank%02x:$startValue%04x $label%s"
  }

  override def fileExtension(bank: Int): String = ".sym"

  override def filePerBank: Boolean = false

  override def addOutputExtension: Boolean = false

  override def formatBreakpoint(bank: Int, value: Int): Option[String] = Some(f"$bank%02x:$value%04x")

  override def labelsHeader: String = "[labels]\n"

  override def breakpointsHeader: String = "[breakpoints]\n"
}

object FceuxDebugOutputFormat extends DebugOutputFormat {
  override def formatLine(label: String, bank: Int, startValue: Int, category: Char, endValue: Option[Int]): String = {
    f"$$$startValue%04x#$label%s#"
  }

  override def fileExtension(bank: Int): String = if (bank == 0xff) ".ram.nl" else s".$bank.nl"

  override def filePerBank: Boolean = true

  override def addOutputExtension: Boolean = true

  override def formatBreakpoint(bank: Int, value: Int): Option[String] = None
}
