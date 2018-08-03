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

  def enqueueStandardModules(): Unit = {
    if (options.zpRegisterSize > 0) {
      moduleQueue.enqueue(() => parseModule("zp_reg", includePath, Left(None)))
    }
    if (options.zpRegisterSize >= 4 && !options.flag(CompilationFlag.DecimalMode)) {
      moduleQueue.enqueue(() => parseModule("bcd_6502", includePath, Left(None)))
    }
  }

  override val supportedPragmas: Set[String] = Set()
}
