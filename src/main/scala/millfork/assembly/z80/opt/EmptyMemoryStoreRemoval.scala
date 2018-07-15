package millfork.assembly.z80.opt

import millfork.assembly.opt.SingleStatus
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
    val paramVariables = f.params match {
//      case NormalParamSignature(List(MemoryVariable(_, typ, _))) if typ.size == 1 =>
//        Set[String]()
      case NormalParamSignature(ps) =>
        ps.map(_.name).toSet
      case _ =>
        // assembly functions do not get this optimization
        return code
    }
    val flow = FlowAnalyzer.analyze(f, code, optimizationContext.options, FlowInfoRequirement.BothFlows)
    import millfork.node.ZRegister._
    val stillUsedVariables = code.flatMap {
      case ZLine(_, TwoRegisters(MEM_ABS_8 | MEM_ABS_16, _), MemoryAddressConstant(th), _) => Some(th.name)
      case ZLine(_, TwoRegisters(_, MEM_ABS_8 | MEM_ABS_16), MemoryAddressConstant(th), _) => Some(th.name)
      case ZLine(_, TwoRegisters(_, IMM_16), MemoryAddressConstant(th), _) => Some(th.name)
      case ZLine(_, TwoRegisters(MEM_ABS_8 | MEM_ABS_16, _), CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(_, _)), _) => Some(th.name)
      case ZLine(_, TwoRegisters(_, MEM_ABS_8 | MEM_ABS_16), CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(_, _)), _) => Some(th.name)
      case ZLine(_, TwoRegisters(_, IMM_16), CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(_, _)), _) => Some(th.name)
      case _ => None
    }.toSet
    val variablesWithAddressesTaken = code.zipWithIndex.flatMap {
      case (ZLine(_, _, SubbyteConstant(MemoryAddressConstant(th), _), _), _) =>
        Some(th.name)
      case (ZLine(_, _, SubbyteConstant(CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(_, _)), _), _), _) =>
        Some(th.name)
      case (ZLine(_,
      TwoRegisters(ZRegister.MEM_HL, _) | TwoRegisters(_, ZRegister.MEM_HL) | OneRegister(ZRegister.MEM_HL),
      _, _), i) =>
        flow(i)._1.statusBefore.hl match {
          case SingleStatus(MemoryAddressConstant(th)) =>
            if (flow(i)._1.importanceAfter.hlNumeric != Unimportant) Some(th.name)
            else None
          case SingleStatus(CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(_, _))) =>
            if (flow(i)._1.importanceAfter.hlNumeric != Unimportant) Some(th.name)
            else None
          case _ => None // TODO: ???
        }
      case _ => None
    }.toSet
    val allLocalVariables = f.environment.getAllLocalVariables
    val localVariables = allLocalVariables.filter {
      case MemoryVariable(name, typ, VariableAllocationMethod.Auto | VariableAllocationMethod.Zeropage) =>
        typ.size > 0 && !paramVariables(name) && stillUsedVariables(name) && !variablesWithAddressesTaken(name)
      case _ => false
    }

    if (localVariables.isEmpty) {
      return code
    }

    val toRemove = mutable.Set[Int]()
    val badVariables = mutable.Set[String]()

    for(v <- localVariables) {
      val lifetime = VariableLifetime.apply(v.name, flow)
      val lastaccess = lifetime.last
      if (lastaccess >= 0) {
        val lastVariableAccess = code(lastaccess)
        import millfork.assembly.z80.ZOpcode._
        if (lastVariableAccess match {
          case ZLine(LD, TwoRegisters(MEM_HL, _), _, true) => true
          case ZLine(LD | LD_16, TwoRegisters(MEM_ABS_8 | MEM_ABS_16, _), _, true) => true
          case ZLine(INC | DEC, OneRegister(MEM_HL), _, true) =>
            val importances = flow(lastaccess)._1.importanceAfter
            Seq(importances.sf, importances.zf).forall(_ == Unimportant)
          case ZLine(SLA | SLL | SRA | SRL | RL | RR | RLC | RRC, OneRegister(MEM_HL), _, true) =>
            val importances = flow(lastaccess)._1.importanceAfter
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
