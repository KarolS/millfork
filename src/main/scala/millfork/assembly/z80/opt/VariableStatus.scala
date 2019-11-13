package millfork.assembly.z80.opt

import millfork.CompilationFlag
import millfork.assembly.OptimizationContext
import millfork.assembly.opt.SingleStatus
import millfork.assembly.z80.{OneRegister, TwoRegisters, ZLine, ZLine0}
import millfork.env._
import millfork.node.ZRegister

/**
  * @author Karol Stasiak
  */

class VariableStatus(val paramVariables: Set[String],
                     val stillUsedVariables: Set[String],
                     val variablesWithAddressesTaken: Set[String],
                     val variablesWithRegisterHint: Set[String],
                     val localVariables: List[Variable],
                     val variablesWithLifetimes: List[(Variable, Range)],
                     val variablesWithLifetimesMap: Map[String, Range],
                     val codeWithFlow: List[(FlowInfo, ZLine)],
                     val paramRegs: Set[ZRegister.Value]) {

  override def toString = s"VariableStatus(paramVariables=$paramVariables, stillUsedVariables=$stillUsedVariables, variablesWithAddressesTaken=$variablesWithAddressesTaken, localVariables=$localVariables, variablesWithLifetimesMap=$variablesWithLifetimesMap)"
}

object VariableStatus {
  def apply(f: NormalFunction, code: List[ZLine], optimizationContext: OptimizationContext, typFilter: Type => Boolean, allowParams: Boolean): Option[VariableStatus] = {
    val flow = FlowAnalyzer.analyze(f, code, optimizationContext, FlowInfoRequirement.BothFlows)
    import millfork.node.ZRegister._
    val (paramVariables, paramRegs) = f.params match {
      case NormalParamSignature(List(MemoryVariable(_, typ, _))) if typ.size == 1 =>
        Set[String]() -> Set(ZRegister.A)
      case NormalParamSignature(List(MemoryVariable(_, typ, _))) if typ.size == 2 =>
        Set[String]() -> Set(ZRegister.HL, ZRegister.H, ZRegister.L)
      case NormalParamSignature(List(MemoryVariable(_, typ, _))) if typ.size == 3 =>
        Set[String]() -> Set(ZRegister.HL, ZRegister.H, ZRegister.L, ZRegister.DE, ZRegister.E)
      case NormalParamSignature(List(MemoryVariable(_, typ, _))) if typ.size == 4 =>
        Set[String]() -> Set(ZRegister.HL, ZRegister.H, ZRegister.L, ZRegister.DE, ZRegister.D, ZRegister.E)
      case NormalParamSignature(ps) =>
        ps.map(_.name).toSet -> Set[ZRegister.Value]()
      case _ =>
        // assembly functions do not get this optimization
        return None
    }
    val stillUsedVariables = code.flatMap {
      case ZLine0(_, TwoRegisters(MEM_ABS_8 | MEM_ABS_16, _), MemoryAddressConstant(th)) => Some(th.name)
      case ZLine0(_, TwoRegisters(_, MEM_ABS_8 | MEM_ABS_16), MemoryAddressConstant(th)) => Some(th.name)
      case ZLine0(_, TwoRegisters(_, IMM_16), MemoryAddressConstant(th)) => Some(th.name)
      case ZLine0(_, TwoRegisters(MEM_ABS_8 | MEM_ABS_16, _), CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(_, _))) => Some(th.name)
      case ZLine0(_, TwoRegisters(_, MEM_ABS_8 | MEM_ABS_16), CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(_, _))) => Some(th.name)
      case ZLine0(_, TwoRegisters(_, IMM_16), CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(_, _))) => Some(th.name)
      case _ => None
    }.toSet
    val variablesWithAddressesTaken = code.zipWithIndex.flatMap {
      case (l@ZLine0(_, _, SubbyteConstant(MemoryAddressConstant(th), _)), _) =>
//        println(th.name -> l)
        Some(th.name)
      case (l@ZLine0(_, _, SubbyteConstant(CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(_, _)), _)), _) =>
//        println(th.name -> l)
        Some(th.name)
      case (l@ZLine0(_,
      TwoRegisters(ZRegister.MEM_HL, _) | TwoRegisters(_, ZRegister.MEM_HL) | OneRegister(ZRegister.MEM_HL),
      _), i) =>
        flow(i)._1.statusBefore.hl match {
          case SingleStatus(MemoryAddressConstant(th)) =>
            if (flow(i)._1.importanceAfter.hlNumeric != Unimportant) {
//              println(th.name -> l)
              Some(th.name)
            }else None
          case SingleStatus(CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(_, _))) =>
            if (flow(i)._1.importanceAfter.hlNumeric != Unimportant) {
//              println(th.name -> l)
              Some(th.name)
            } else None
          case _ => None // TODO: ???
        }
      case _ => None
    }.toSet
    val allLocalVariables = f.environment.getAllLocalVariables
    val localVariables = allLocalVariables.filter {
      case MemoryVariable(name, typ, VariableAllocationMethod.Auto | VariableAllocationMethod.Zeropage) =>
        typFilter(typ) && (allowParams || !paramVariables(name)) && stillUsedVariables(name) && !variablesWithAddressesTaken(name)
      case StackVariable(name, typ, _) => typFilter(typ)
      case _ => false
    }
    val variablesWithRegisterHint = f.environment.getAllLocalVariables.filter {
      case MemoryVariable(name, typ, VariableAllocationMethod.Register) =>
        typFilter(typ) && (typ.size == 1 || typ.size == 2) && !paramVariables(name) && stillUsedVariables(name) && !variablesWithAddressesTaken(name)
      case _ => false
    }.map(_.name).toSet
    val variablesWithLifetimes = localVariables.map {
      case v: MemoryVariable =>
        v -> VariableLifetime.apply(v.name, flow, stretchBackwards = paramVariables(v.name))
      case v: StackVariable =>
        v -> StackVariableLifetime.apply(v.baseOffset, flow)
    }
    val stackPrefix =
      if (optimizationContext.options.flag(CompilationFlag.UseIxForStack)) "IX+"
      else if (optimizationContext.options.flag(CompilationFlag.UseIyForStack)) "IY+"
      else "SP+"
    val variablesWithLifetimesMap = variablesWithLifetimes.map {
      case (v: MemoryVariable, lt) => v.name -> lt
      case (v: StackVariable, lt) => (stackPrefix + v.baseOffset) -> lt
    }.toMap
    Some(new VariableStatus(
      paramVariables,
      stillUsedVariables,
      variablesWithAddressesTaken,
      variablesWithRegisterHint,
      localVariables,
      variablesWithLifetimes,
      variablesWithLifetimesMap,
      flow,
      paramRegs))
  }

}