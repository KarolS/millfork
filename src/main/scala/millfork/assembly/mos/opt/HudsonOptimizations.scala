package millfork.assembly.mos.opt

import millfork.CompilationOptions
import millfork.assembly.{AssemblyOptimization, OptimizationContext}
import millfork.assembly.mos.AssemblyLine
import millfork.assembly.mos.Opcode._
import millfork.assembly.mos.AddrMode._
import millfork.env.{NormalFunction, NumericConstant}

/**
  * @author Karol Stasiak
  */
object HudsonOptimizations {

  val All: List[AssemblyOptimization[AssemblyLine]] = List()

  def removeLoadZero(f: NormalFunction, code: List[AssemblyLine], optimizationContext: OptimizationContext): List[AssemblyLine] = {
    ReverseFlowAnalyzer.analyze(f, code, optimizationContext).zip(code).map {
      case (i, l) if i.n != Unimportant || i.z != Unimportant => l
      case (i, AssemblyLine(LDA, Immediate, NumericConstant(0, _), true)) => AssemblyLine.implied(CLA)
      case (_, AssemblyLine(LDX, Immediate, NumericConstant(0, _), true)) => AssemblyLine.implied(CLX)
      case (_, AssemblyLine(LDY, Immediate, NumericConstant(0, _), true)) => AssemblyLine.implied(CLY)
      case (_, l) => l
    }
  }
}
