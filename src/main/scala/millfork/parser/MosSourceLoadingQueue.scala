package millfork.parser

import millfork.{CompilationFlag, CompilationOptions}
import millfork.assembly.mos.AssemblyLine

/**
  * @author Karol Stasiak
  */
class MosSourceLoadingQueue(initialFilenames: List[String],
                            includePath: List[String],
                            options: CompilationOptions) extends AbstractSourceLoadingQueue[AssemblyLine](initialFilenames, includePath, options) {

  override def createParser(filename: String, src: String, parentDir: String, featureConstants: Map[String, Long], pragmas: Set[String]): MfParser[AssemblyLine] =
    MosParser(filename, src, parentDir, options, featureConstants)

  override def standardModules: IndexedSeq[String] = IndexedSeq("m6502/zp_reg", "m6502/bcd_6502")

  def enqueueStandardModules(): Unit = {
    if (options.zpRegisterSize > 0) {
      moduleQueue.enqueue(() => parseDefaultModule("m6502/zp_reg"))
    }
    if (options.zpRegisterSize >= 4 && !options.flag(CompilationFlag.DecimalMode)) {
      moduleQueue.enqueue(() => parseDefaultModule("m6502/bcd_6502"))
    }
  }

  override val supportedPragmas: Set[String] = Set()
}
