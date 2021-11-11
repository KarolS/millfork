package millfork.output

/**
  * @author Karol Stasiak
  */
object BankLayoutInFile{
  def empty: BankLayoutInFile = new BankLayoutInFile(Map.empty, Map.empty)
}

class BankLayoutInFile(startInFile: Map[String, Int], firstAddress: Map[String, Int]) {

  val fileHeaderSize: Int = if (startInFile.isEmpty) 0 else startInFile.values.min

  // Mesen represents all label values as offsets from the start of ROM data in the .nes file,
  // not as actual addresses
  def getMesenShift(bank: String): Int = {
    val s = startInFile.getOrElse(bank, 0)
    val f = firstAddress.getOrElse(bank, 0)
    // for example, a typical 32K NROM PRG has:
    // s = 16
    // h = 16
    // f = 0x8000
    // addresses = 0x8000-0xffff
    // Mesen Label values = 0x0000-0x7fff
    // shift = -0x8000
    s - fileHeaderSize - f
  }

  def wasWritten(bank: String): Boolean = startInFile.contains(bank)

  def getStart(bank:String): Int = firstAddress.getOrElse(bank, 0) // ??

  def ld65Offset(bank: String): Int = startInFile(bank)

  def allBanks(): Set[String] = firstAddress.keySet
}
