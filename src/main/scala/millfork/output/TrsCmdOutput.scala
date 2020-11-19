package millfork.output

/**
  * @author Karol Stasiak
  */
class TrsCmdOutput(symbol: String) extends OutputPackager {
  override def packageOutput(mem: CompiledMemory, bank: String): Array[Byte] = {
    val b = mem.banks(bank)
    val start = b.start
    val run = mem.getAddress(symbol)
    b.output.slice(start, b.end + 1).grouped(256).zipWithIndex.flatMap{ case (chunk, index) =>
      // chunk type 1: data
      // chunk length: 1 byte, includes the load address, goes 3-258
      // load address: 2 bytes, little-endian
      Array[Byte](1, (chunk.length + 2).toByte, start.toByte, start.>>(8).+(index).toByte) ++ chunk
    }.toArray ++ Array[Byte](
      // chunk type 2: relocation address
      // chunk length: for type 2, it's always 2 bytes
      // relocation address: 2 bytes, little-endian
      2, 2, run.toByte, run.>>(8).toByte
    )
  }
}
