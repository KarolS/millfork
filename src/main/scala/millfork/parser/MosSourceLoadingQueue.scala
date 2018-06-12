package millfork.parser

import millfork.{CompilationFlag, CompilationOptions}
import millfork.assembly.mos.AssemblyLine

/**
  * @author Karol Stasiak
  */
class MosSourceLoadingQueue(initialFilenames: List[String],
                            includePath: List[String],
                            options: CompilationOptions) extends AbstractSourceLoadingQueue[AssemblyLine](initialFilenames, includePath, options) {

  override def createParser(filename: String, src: String, parentDir: String): MfParser[AssemblyLine] = MosParser(filename, src, parentDir, options)

  def enqueueStandardModules(): Unit = {
    if (options.flag(CompilationFlag.ZeropagePseudoregister)) {
      moduleQueue.enqueue(() => parseModule("zp_reg", includePath, Left(None)))
    }
  }

}
