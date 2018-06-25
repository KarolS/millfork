package millfork.assembly.mos.opt

import millfork.CompilationOptions
import millfork.assembly.{AssemblyOptimization, OptimizationContext}
import millfork.assembly.mos.AssemblyLine
import millfork.env._
import millfork.assembly.mos.Opcode._
import millfork.assembly.mos.AddrMode._
import millfork.error.ErrorReporting

import scala.collection.{immutable, mutable}

/**
  * @author Karol Stasiak
  */
object EmptyMemoryStoreRemoval extends AssemblyOptimization[AssemblyLine] {
  override def name = "Removing pointless stores to automatic variables"

  private val storeAddrModes = Set(Absolute, ZeroPage, AbsoluteX, AbsoluteY, ZeroPageX, ZeroPageY)

  override def optimize(f: NormalFunction, code: List[AssemblyLine], optimizationContext: OptimizationContext): List[AssemblyLine] = {
    val paramVariables = f.params match {
      case NormalParamSignature(List(MemoryVariable(_, typ, _))) if typ.size == 1 =>
        Set[String]()
      case NormalParamSignature(ps) =>
        ps.map(_.name).toSet
      case _ =>
        // assembly functions do not get this optimization
        return code
    }
    val stillUsedVariables = code.flatMap {
      case AssemblyLine(_, _, MemoryAddressConstant(th), _) => Some(th.name)
      case AssemblyLine(_, _, CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(_, _)), _) => Some(th.name)
      case _ => None
    }.toSet
    val variablesWithAddressesTaken = code.flatMap {
      case AssemblyLine(_, Immediate, SubbyteConstant(MemoryAddressConstant(th), _), _) =>
        Some(th.name)
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
    var importances: immutable.Seq[CpuImportance] = null

    for(v <- localVariables) {
      val lifetime = VariableLifetime.apply(v.name, code)
      val lastaccess = lifetime.last
      if (lastaccess >= 0) {
        val lastVariableAccess = code(lastaccess)
        if (lastVariableAccess.elidable && storeAddrModes(lastVariableAccess.addrMode) && (lastVariableAccess.opcode match {
          case STA | STX | STY | SAX | STZ | SHX | SHY | AHX =>
            true
          case TSB | TRB =>
            if (importances eq null) importances = ReverseFlowAnalyzer.analyze(f, code, optimizationContext)
            importances(lastaccess).z != Important
          case INC | DEC =>
            if (importances eq null) importances = ReverseFlowAnalyzer.analyze(f, code, optimizationContext)
            importances(lastaccess).z != Important &&
              importances(lastaccess).n != Important
          case ASL | LSR | ROL | ROR | DCP =>
            if (importances eq null) importances = ReverseFlowAnalyzer.analyze(f, code, optimizationContext)
            importances(lastaccess).z != Important &&
              importances(lastaccess).n != Important &&
              importances(lastaccess).c != Important
          case ISC =>
            if (importances eq null) importances = ReverseFlowAnalyzer.analyze(f, code, optimizationContext)
            importances(lastaccess).z != Important &&
              importances(lastaccess).n != Important &&
              importances(lastaccess).a != Important
          case DCP | SLO | SRE | RLA =>
            if (importances eq null) importances = ReverseFlowAnalyzer.analyze(f, code, optimizationContext)
            importances(lastaccess).z != Important &&
              importances(lastaccess).n != Important &&
              importances(lastaccess).c != Important &&
              importances(lastaccess).a != Important
          case RRA =>
            if (importances eq null) importances = ReverseFlowAnalyzer.analyze(f, code, optimizationContext)
            importances(lastaccess).z != Important &&
              importances(lastaccess).n != Important &&
              importances(lastaccess).c != Important &&
              importances(lastaccess).a != Important &&
              importances(lastaccess).v != Important
          case _ => // last variable access is important, or we're in a loop, do not remove
            false
        })) {
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
