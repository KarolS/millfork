package millfork.output

import millfork.{CompilationOptions, Platform}
import millfork.assembly.z80.ZLine
import millfork.compiler.z80.Z80Compiler
import millfork.env.{Environment, NormalFunction}
import millfork.node.{NiceFunctionProperty, Program}

import scala.collection.mutable

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

  override def injectLabels(labelMap: Map[String, Int], code: List[ZLine]): List[ZLine] = code // TODO

  override def gatherNiceFunctionProperties(niceFunctionProperties: mutable.Set[(NiceFunctionProperty, String)], functionName: String, code: List[ZLine]): Unit = {
    // do nothing yet
  }
}
object Z80Assembler {

}