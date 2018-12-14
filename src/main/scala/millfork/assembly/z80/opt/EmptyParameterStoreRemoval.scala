package millfork.assembly.z80.opt

import millfork.Cpu
import millfork.assembly.z80.ZOpcode._
import millfork.assembly.z80.{TwoRegisters, ZLine, ZLine0}
import millfork.assembly.{AssemblyOptimization, Elidability, OptimizationContext}
import millfork.env._

/**
  * @author Karol Stasiak
  */
object EmptyParameterStoreRemoval extends AssemblyOptimization[ZLine] {
  override def name = "Removing pointless stores to foreign variables"

  override def optimize(f: NormalFunction, code: List[ZLine], optimizationContext: OptimizationContext): List[ZLine] = {
    val usedFunctions = code.flatMap {
      case ZLine0(CALL | JP | JR, _, MemoryAddressConstant(th)) => Some(th.name)
      case ZLine0(CALL | JP | JR, _, NumericConstant(addr, _)) => Some("$" + addr.toHexString)
      case _ => None
    }.toSet
    val foreignVariables = f.environment.root.things.values.flatMap {
      case other: NormalFunction =>
        val address = other.address match {
          case Some(NumericConstant(addr, _)) => "$" + addr.toHexString
          case _ => ""
        }
        if (other.name == f.name || usedFunctions(other.name) || usedFunctions(address)) {
          Nil
        } else {
          val params = other.params match {
            case NormalParamSignature(ps) => ps.map(_.name)
            case _ => Nil
          }
          val locals = other.environment.things.values.flatMap{
            case th: MemoryVariable if th.alloc == VariableAllocationMethod.Auto => Some(th.name)
            case _ => None
          }
          if (other.returnType.size > Cpu.getMaxSizeReturnableViaRegisters(optimizationContext.options.platform.cpu, optimizationContext.options)) {
            other.name + ".return" :: (params ++ locals)
          } else {
            params ++ locals
          }
        }
      case _ => Nil
    }.toSet
    import millfork.node.ZRegister._
    val stillReadOrStoredVariables = code.flatMap {
      case ZLine0(_, _, MemoryAddressConstant(th)) => Some(th.name)
      case ZLine0(_, _, CompoundConstant(_, MemoryAddressConstant(th), _)) => Some(th.name)
      case ZLine0(_, _, SubbyteConstant(MemoryAddressConstant(th), _)) => Some(th.name)
      case _ => None
    }.toSet
    val stillReadVariables = code.flatMap {
      case ZLine(LD | LD_16, TwoRegisters(MEM_ABS_8 | MEM_ABS_16, _), MemoryAddressConstant(th), Elidability.Elidable, _) => Nil
      case ZLine(LD | LD_16, TwoRegisters(MEM_ABS_8 | MEM_ABS_16, _), CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(_, _)), Elidability.Elidable, _) => Nil
      case ZLine0(_, _, MemoryAddressConstant(th)) => Some(th.name)
      case ZLine0(_, _, CompoundConstant(_, MemoryAddressConstant(th), _)) => Some(th.name)
      case ZLine0(_, _, SubbyteConstant(MemoryAddressConstant(th), _)) => Some(th.name)
      case _ => None
    }.toSet

    val unusedForeignVariables = (foreignVariables & stillReadOrStoredVariables) -- stillReadVariables
    if (unusedForeignVariables.isEmpty) {
      return code
    }

    optimizationContext.log.debug(s"Removing pointless store(s) to foreign variables ${unusedForeignVariables.mkString(", ")}")
    code.filterNot {
      case ZLine0(LD | LD_16, TwoRegisters(MEM_ABS_8 | MEM_ABS_16, _), MemoryAddressConstant(th)) =>
        unusedForeignVariables(th.name)
      case ZLine(LD | LD_16, TwoRegisters(MEM_ABS_8 | MEM_ABS_16, _), CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(_, _)), Elidability.Elidable, _) =>
        unusedForeignVariables(th.name)
      case _ => false
    }
  }
}
