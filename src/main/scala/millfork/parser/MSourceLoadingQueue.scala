package millfork.parser

import millfork.assembly.m6809.MLine
import millfork.assembly.z80.ZLine
import millfork.{CompilationFlag, CompilationOptions}

/**
  * @author Karol Stasiak
  */
class MSourceLoadingQueue(initialFilenames: List[String],
                          includePath: List[String],
                          options: CompilationOptions) extends AbstractSourceLoadingQueue[MLine](initialFilenames, includePath, options) {

  override def createParser(filename: String, src: String, parentDir: String, featureConstants: Map[String, Long], pragmas: Set[String]): MfParser[MLine] = {
    new M6809Parser(filename, src, parentDir, options, featureConstants)
  }

  override def standardModules: IndexedSeq[String] = IndexedSeq.empty

  def enqueueStandardModules(): Unit = {
    // TODO
  }

  override val supportedPragmas: Set[String] = Set("intel_syntax", "zilog_syntax")
}
