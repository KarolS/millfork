package millfork

/**
  * @author Karol Stasiak
  */

object DebugOutputFormat {
  val map: Map[String, DebugOutputFormat] = Map(
    "vice" -> ViceDebugOutputFormat,
    "nesasm" -> NesasmDebugOutputFormat,
    "fns" -> NesasmDebugOutputFormat,
    "fceux" -> FceuxDebugOutputFormat,
    "nl" -> FceuxDebugOutputFormat,
    "sym" -> SymDebugOutputFormat)
}

sealed trait DebugOutputFormat extends Function[(String, (Int, Int)), String] {

  def apply(labelAndValue: (String, (Int, Int))): String = formatLine(labelAndValue._1, labelAndValue._2._1, labelAndValue._2._2)

  def formatLine(label: String, bank: Int, value: Int): String

  def fileExtension(bank: Int): String

  def filePerBank: Boolean

  def addOutputExtension: Boolean
}

object ViceDebugOutputFormat extends DebugOutputFormat {
  override def formatLine(label: String, bank: Int, value: Int): String = {
    val normalized = label.replace('$', '_').replace('.', '_')
    s"al ${value.toHexString} .$normalized"
  }

  override def fileExtension(bank: Int): String = ".lbl"

  override def filePerBank: Boolean = false

  override def addOutputExtension: Boolean = false
}

object NesasmDebugOutputFormat extends DebugOutputFormat {
  override def formatLine(label: String, bank: Int, value: Int): String = {
    label + " = $" + value.toHexString
  }

  override def fileExtension(bank: Int): String = ".fns"

  override def filePerBank: Boolean = false

  override def addOutputExtension: Boolean = false
}

object SymDebugOutputFormat extends DebugOutputFormat {
  override def formatLine(label: String, bank: Int, value: Int): String = {
    f"$bank%02x:$value%04x $label%s"
  }

  override def fileExtension(bank: Int): String = ".sym"

  override def filePerBank: Boolean = false

  override def addOutputExtension: Boolean = false
}

object FceuxDebugOutputFormat extends DebugOutputFormat {
  override def formatLine(label: String, bank: Int, value: Int): String = {
    f"$$$value%04x#$label%s#"
  }

  override def fileExtension(bank: Int): String = if (bank == 0xff) ".ram.nl" else s".$bank.nl"

  override def filePerBank: Boolean = true

  override def addOutputExtension: Boolean = true
}
