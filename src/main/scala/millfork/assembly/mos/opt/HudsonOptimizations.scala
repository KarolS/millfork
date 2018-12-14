package millfork.assembly.mos.opt

import millfork.CompilationOptions
import millfork.assembly.{AssemblyOptimization, Elidability, OptimizationContext}
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
      case (i, AssemblyLine(LDA, Immediate, NumericConstant(0, _), Elidability.Elidable | Elidability.Volatile, s)) => AssemblyLine.implied(CLA).pos(s)
      case (_, AssemblyLine(LDX, Immediate, NumericConstant(0, _), Elidability.Elidable | Elidability.Volatile, s)) => AssemblyLine.implied(CLX).pos(s)
      case (_, AssemblyLine(LDY, Immediate, NumericConstant(0, _), Elidability.Elidable | Elidability.Volatile, s)) => AssemblyLine.implied(CLY).pos(s)
      case (_, l) => l
    }
  }
}
