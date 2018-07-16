package millfork.assembly.z80.opt

import millfork.assembly.z80.{OneRegister, TwoRegisters, ZLine}
import millfork.assembly.{AssemblyOptimization, OptimizationContext}
import millfork.env._
import millfork.error.ErrorReporting
import millfork.node.ZRegister

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */
object EmptyMemoryStoreRemoval extends AssemblyOptimization[ZLine] {
  override def name = "Removing pointless stores to automatic variables"

  override def optimize(f: NormalFunction, code: List[ZLine], optimizationContext: OptimizationContext): List[ZLine] = {
    val vs = VariableStatus(f, code, optimizationContext, _ => true).getOrElse(return code)
    if (vs.localVariables.isEmpty) {
      return code
    }
    import ZRegister._

    val toRemove = mutable.Set[Int]()
    val badVariables = mutable.Set[String]()

    for((v, lifetime) <- vs.variablesWithLifetimes) {
      val lastaccess = lifetime.last
      if (lastaccess >= 0) {
        val lastVariableAccess = code(lastaccess)
        import millfork.assembly.z80.ZOpcode._
        if (lastVariableAccess match {
          case ZLine(LD, TwoRegisters(MEM_HL, _), _, true) => true
          case ZLine(LD | LD_16, TwoRegisters(MEM_ABS_8 | MEM_ABS_16, _), _, true) => true
          case ZLine(INC | DEC, OneRegister(MEM_HL), _, true) =>
            val importances = vs.codeWithFlow(lastaccess)._1.importanceAfter
            Seq(importances.sf, importances.zf).forall(_ == Unimportant)
          case ZLine(SLA | SLL | SRA | SRL | RL | RR | RLC | RRC, OneRegister(MEM_HL), _, true) =>
            val importances = vs.codeWithFlow(lastaccess)._1.importanceAfter
            Seq(importances.sf, importances.zf, importances.cf).forall(_ == Unimportant)
          case _ => false
        }) {
          badVariables += v.name
          toRemove += lastaccess
        }
      }
    }
    if (toRemove.isEmpty) {
      code
    } else {
      ErrorReporting.debug(s"Removing pointless store(s) to ${badVariables.mkString(", ")}")
      code.zipWithIndex.filter(x => !toRemove(x._2)).map(_._1)
    }
  }
}
