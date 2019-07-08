package millfork.parser

import fastparse.all
import millfork.CompilationOptions
import millfork.assembly.m6809.MLine
import millfork.node.{ExecutableStatement, ParameterDeclaration, Position, Statement}
import millfork.output.{MemoryAlignment, NoAlignment, WithinPageAlignment}

/**
  * @author Karol Stasiak
  */
class M6809Parser(filename: String,
                     input: String,
                     currentDirectory: String,
                     options: CompilationOptions,
                     featureConstants: Map[String, Long],
                     useIntelSyntax: Boolean) extends MfParser[MLine](filename, input, currentDirectory, options, featureConstants) {
  override def allowIntelHexAtomsInAssembly: Boolean = false

  override def asmParamDefinition: all.P[ParameterDeclaration] = ???

  def fastAlignmentForArrays: MemoryAlignment = WithinPageAlignment

  def fastAlignmentForFunctions: MemoryAlignment = NoAlignment

  override def asmStatement: all.P[ExecutableStatement] = ???

  override def validateAsmFunctionBody(p: Position, flags: Set[String], name: String, statements: Option[List[Statement]]): Unit = {
    // TODO
  }
}
