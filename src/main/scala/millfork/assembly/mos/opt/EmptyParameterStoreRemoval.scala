package millfork.assembly.mos.opt

import millfork.Cpu
import millfork.assembly.mos.AddrMode._
import millfork.assembly.mos.{AssemblyLine, AssemblyLine0}
import millfork.assembly.mos.Opcode._
import millfork.assembly.{AssemblyOptimization, Elidability, OptimizationContext}
import millfork.env._
import millfork.error.ConsoleLogger

import scala.collection.mutable

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
    val foreignVariables = mutable.Set[String]()
    f.environment.root.things.values.foreach {
      case other: NormalFunction if !other.name.endsWith(".trampoline") =>
        val address = other.address match {
          case Some(NumericConstant(addr, _)) => "$" + addr.toHexString
          case _ => ""
        }
        if (other.name == f.name || usedFunctions(other.name) || usedFunctions(address)) {
          // do nothing
        } else {
          other.params match {
            case NormalParamSignature(ps) =>
              ps.foreach(p => foreignVariables += p.name)
            case _ =>
          }
          other.environment.things.values.foreach {
            case th: MemoryVariable if th.alloc == VariableAllocationMethod.Auto =>
              foreignVariables += th.name
            case th: MemoryVariable if th.alloc == VariableAllocationMethod.Zeropage =>
              foreignVariables += th.name // TODO: ???
            case _ =>
          }
          if (other.returnType.size > Cpu.getMaxSizeReturnableViaRegisters(optimizationContext.options.platform.cpu, optimizationContext.options)) {
            foreignVariables += other.name + ".return"
          }
        }
      case _ =>
    }
    val stillReadOrStoredVariables = mutable.Set[String]()
    code.foreach {
      case AssemblyLine0(_, _, MemoryAddressConstant(th)) => stillReadOrStoredVariables += th.name
      case AssemblyLine0(_, _, CompoundConstant(_, MemoryAddressConstant(th), _)) => stillReadOrStoredVariables += th.name
      case AssemblyLine0(_, Immediate, SubbyteConstant(MemoryAddressConstant(th), _)) => stillReadOrStoredVariables += th.name
      case _ =>
    }
    val stillReadVariables = mutable.Set[String]()
    code.foreach {
      case AssemblyLine(op, am, MemoryAddressConstant(th), Elidability.Elidable, _)
        if storeInstructions(op) && storeAddrModes(am) =>
      case AssemblyLine(op, am, CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(_, _)), Elidability.Elidable, _)
        if storeInstructions(op) && storeAddrModes(am) =>
      case AssemblyLine0(_, _, MemoryAddressConstant(th)) => stillReadVariables += th.name
      case AssemblyLine0(_, _, CompoundConstant(_, MemoryAddressConstant(th), _)) => stillReadVariables += th.name
      case AssemblyLine0(_, Immediate, SubbyteConstant(MemoryAddressConstant(th), _)) => stillReadVariables += th.name
      case _ =>
    }

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
