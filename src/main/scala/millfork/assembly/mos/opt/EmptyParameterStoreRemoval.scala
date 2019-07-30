package millfork.assembly.mos.opt

import millfork.Cpu
import millfork.assembly.mos.AddrMode._
import millfork.assembly.mos.{AssemblyLine, AssemblyLine0}
import millfork.assembly.mos.Opcode._
import millfork.assembly.{AssemblyOptimization, Elidability, OptimizationContext}
import millfork.env._
import millfork.error.ConsoleLogger

/**
  * @author Karol Stasiak
  */
object EmptyParameterStoreRemoval extends AssemblyOptimization[AssemblyLine] {
  override def name = "Removing pointless stores to foreign variables"

  private val storeInstructions = Set(STA, STX, STY, SAX, STZ, STA_W, STX_W, STY_W, STZ_W)
  private val storeAddrModes = Set(Absolute, ZeroPage, AbsoluteX, AbsoluteY, ZeroPageX, ZeroPageY, LongAbsolute, LongAbsoluteX)

  override def optimize(f: NormalFunction, code: List[AssemblyLine], optimizationContext: OptimizationContext): List[AssemblyLine] = {
    val usedFunctions = code.flatMap {
      case AssemblyLine0(JSR | BSR | JMP, _, MemoryAddressConstant(th)) => Some(th.name)
      case AssemblyLine0(JSR | BSR | JMP, _, NumericConstant(addr, _)) => Some("$" + addr.toHexString)
      case _ => None
    }.toSet
    val foreignVariables = f.environment.root.things.values.flatMap {
      case other: NormalFunction if !other.name.endsWith(".trampoline") =>
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
            case th: MemoryVariable if th.alloc == VariableAllocationMethod.Zeropage => Some(th.name) // TODO: ???
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
    val stillReadOrStoredVariables = code.flatMap {
      case AssemblyLine0(_, _, MemoryAddressConstant(th)) => Some(th.name)
      case AssemblyLine0(_, _, CompoundConstant(_, MemoryAddressConstant(th), _)) => Some(th.name)
      case AssemblyLine0(_, Immediate, SubbyteConstant(MemoryAddressConstant(th), _)) => Some(th.name)
      case _ => None
    }.toSet
    val stillReadVariables = code.flatMap {
      case AssemblyLine(op, am, MemoryAddressConstant(th), Elidability.Elidable, _)
        if storeInstructions(op) && storeAddrModes(am) => Nil
      case AssemblyLine(op, am, CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(_, _)), Elidability.Elidable, _)
        if storeInstructions(op) && storeAddrModes(am) => Nil
      case AssemblyLine0(_, _, MemoryAddressConstant(th)) => Some(th.name)
      case AssemblyLine0(_, _, CompoundConstant(_, MemoryAddressConstant(th), _)) => Some(th.name)
      case AssemblyLine0(_, Immediate, SubbyteConstant(MemoryAddressConstant(th), _)) => Some(th.name)
      case _ => None
    }.toSet

    val unusedForeignVariables = (foreignVariables & stillReadOrStoredVariables) -- stillReadVariables
    if (unusedForeignVariables.isEmpty) {
      return code
    }

    optimizationContext.log.debug(s"Removing pointless store(s) to foreign variables ${unusedForeignVariables.mkString(", ")}")
    code.filterNot {
      case AssemblyLine0(op, am, MemoryAddressConstant(th))
        if storeInstructions(op) && storeAddrModes(am) =>
        unusedForeignVariables(th.name)
      case AssemblyLine(op, am, CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(_, _)), Elidability.Elidable, _)
        if storeInstructions(op) && storeAddrModes(am) =>
        unusedForeignVariables(th.name)
      case _ => false
    }
  }
}
