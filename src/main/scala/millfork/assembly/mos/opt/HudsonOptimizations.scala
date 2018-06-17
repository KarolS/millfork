package millfork.assembly.mos.opt

import millfork.assembly.AssemblyOptimization
import millfork.assembly.mos.AssemblyLine
import millfork.assembly.mos.Opcode._
import millfork.assembly.mos.AddrMode._
import millfork.env.NumericConstant

/**
  * @author Karol Stasiak
  */
object HudsonOptimizations {

  val All: List[AssemblyOptimization[AssemblyLine]] = List()

  def removeLoadZero(code: List[AssemblyLine]): List[AssemblyLine] = code.map{
    case AssemblyLine(LDA, Immediate, NumericConstant(0, _), true) => AssemblyLine.implied(CLA)
    case AssemblyLine(LDX, Immediate, NumericConstant(0, _), true) => AssemblyLine.implied(CLX)
    case AssemblyLine(LDY, Immediate, NumericConstant(0, _), true) => AssemblyLine.implied(CLY)
    case l => l
  }
}
