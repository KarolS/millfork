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

  override def createParser(filename: String, src: String, parentDir: String, featureConstants: Map[String, Long], pragmas: Set[String]): MfParser[ZLine] = {
    var useIntelSyntax = options.flag(CompilationFlag.UseIntelSyntaxForInput)
    if (pragmas("intel_syntax") && pragmas("zilog_syntax")) options.log.error("Conflicting pragmas: #pragma intel_syntax and #pragma zilog_syntax")
    if (pragmas("zilog_syntax")) useIntelSyntax = false
    if (pragmas("intel_syntax")) useIntelSyntax = true
    Z80Parser(filename, src, parentDir, options, featureConstants, useIntelSyntax)
  }

  override def standardModules: IndexedSeq[String] = IndexedSeq.empty

  def enqueueStandardModules(): Unit = {
    // TODO
  }

  override val supportedPragmas: Set[String] = Set("intel_syntax", "zilog_syntax")
}
