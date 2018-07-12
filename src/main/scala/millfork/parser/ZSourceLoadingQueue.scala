package millfork.parser

import millfork.assembly.mos.AssemblyLine
import millfork.assembly.z80.ZLine
import millfork.{CompilationFlag, CompilationOptions}

/**
  * @author Karol Stasiak
  */
class ZSourceLoadingQueue(initialFilenames: List[String],
                          includePath: List[String],
                          options: CompilationOptions) extends AbstractSourceLoadingQueue[ZLine](initialFilenames, includePath, options) {

  override def createParser(filename: String, src: String, parentDir: String, featureConstants: Map[String, Long]): MfParser[ZLine] =
    Z80Parser(filename, src, parentDir, options, featureConstants)

  def enqueueStandardModules(): Unit = {
    // TODO
  }

}
