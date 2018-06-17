package millfork.parser

import fastparse.all
import millfork.CompilationOptions
import millfork.assembly.z80.ZLine
import millfork.node.{ExecutableStatement, ParameterDeclaration, Position, Statement}

/**
  * @author Karol Stasiak
  */
case class Z80Parser(filename: String, input: String, currentDirectory: String, options: CompilationOptions) extends MfParser[ZLine](filename, input, currentDirectory, options) {
  override def asmParamDefinition: all.P[ParameterDeclaration] = ???

  override def asmStatement: all.P[ExecutableStatement] = ???

  override def validateAsmFunctionBody(p: Position, flags: Set[String], name: String, statements: Option[List[Statement]]): Unit = ???
}
