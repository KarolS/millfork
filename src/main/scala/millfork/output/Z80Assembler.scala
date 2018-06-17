package millfork.output

import millfork.{CompilationOptions, Platform}
import millfork.assembly.z80.ZLine
import millfork.compiler.z80.Z80Compiler
import millfork.env.{Environment, NormalFunction}
import millfork.node.Program

/**
  * @author Karol Stasiak
  */
class Z80Assembler(program: Program,
                   rootEnv: Environment,
                   platform: Platform) extends AbstractAssembler[ZLine](program, rootEnv, platform, Z80InliningCalculator, Z80Compiler) {
  override def performFinalOptimizationPass(f: NormalFunction, actuallyOptimize: Boolean, options: CompilationOptions, code: List[ZLine]): List[ZLine] = code

  override def emitInstruction(bank: String, options: CompilationOptions, index: Int, instr: ZLine): Int = {
    // TODO
    index
  }
}
object Z80Assembler {

}