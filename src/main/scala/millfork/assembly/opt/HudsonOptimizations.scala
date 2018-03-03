package millfork.assembly.opt

import millfork.assembly.AssemblyLine
import millfork.assembly.Opcode._
import millfork.assembly.AddrMode._
import millfork.env.NumericConstant

/**
  * @author Karol Stasiak
  */
object HudsonOptimizations {

  val All: List[AssemblyOptimization] = List()

  def removeLoadZero(code: List[AssemblyLine]): List[AssemblyLine] = code.map{
    case AssemblyLine(LDA, Immediate, NumericConstant(0, _), true) => AssemblyLine.implied(CLA)
    case AssemblyLine(LDX, Immediate, NumericConstant(0, _), true) => AssemblyLine.implied(CLX)
    case AssemblyLine(LDY, Immediate, NumericConstant(0, _), true) => AssemblyLine.implied(CLY)
    case l => l
  }
}
