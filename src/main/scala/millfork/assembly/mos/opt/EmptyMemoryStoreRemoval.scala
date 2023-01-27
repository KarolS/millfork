package millfork.assembly.mos.opt

import millfork.CompilationOptions
import millfork.assembly.{AssemblyOptimization, OptimizationContext}
import millfork.assembly.mos.{AssemblyLine, AssemblyLine0}
import millfork.env._
import millfork.assembly.mos.Opcode._
import millfork.assembly.mos.AddrMode._
import millfork.error.ConsoleLogger

import scala.collection.{immutable, mutable}

/**
  * @author Karol Stasiak
  */
object EmptyMemoryStoreRemoval extends AssemblyOptimization[AssemblyLine] {
  override def name = "Removing pointless stores to automatic variables"

  override def minimumRequiredLines: Int = 2

  private val storeAddrModes = Set(Absolute, ZeroPage, AbsoluteX, AbsoluteY, ZeroPageX, ZeroPageY)
  private val directStorageOpcodes = Set(
    STA, STX, STY, SAX, STZ, SHX, SHY, AHX,
    STA_W, STX_W, STY_W, STZ_W
  )

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
      case AssemblyLine0(_, _, MemoryAddressConstant(th)) => Some(th.name)
      case AssemblyLine0(_, _, CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(_, _))) => Some(th.name)
      case _ => None
    }.toSet
    val variablesWithAddressesTaken = code.flatMap {
      case AssemblyLine0(_, Immediate, SubbyteConstant(MemoryAddressConstant(th), _)) =>
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
      val firstaccess = lifetime.head
      val lastaccess = lifetime.last
      if (firstaccess >= 0 && lastaccess >= 0) {
        val firstVariableAccess = code(firstaccess)
        val lastVariableAccess = code(lastaccess)
        if (lastVariableAccess.elidable &&
          storeAddrModes(lastVariableAccess.addrMode) &&
          storeAddrModes(firstVariableAccess.addrMode) &&
          directStorageOpcodes(firstVariableAccess.opcode) &&
          (lastVariableAccess.opcode match {
          case op if directStorageOpcodes(op) =>
            true
          case TSB | TRB =>
            if (importances eq null) importances = ReverseFlowAnalyzer.analyze(f, code, optimizationContext)
            importances(lastaccess).z != Important
          case INC | DEC | INC_W | DEC_W =>
            if (importances eq null) importances = ReverseFlowAnalyzer.analyze(f, code, optimizationContext)
            importances(lastaccess).z != Important &&
              importances(lastaccess).n != Important
          case ASL | LSR | ROL | ROR | DCP | ASL_W | LSR_W | ROL_W | ROR_W =>
            if (importances eq null) importances = ReverseFlowAnalyzer.analyze(f, code, optimizationContext)
            importances(lastaccess).z != Important &&
              importances(lastaccess).n != Important &&
              importances(lastaccess).c != Important
          case ISC =>
            if (importances eq null) importances = ReverseFlowAnalyzer.analyze(f, code, optimizationContext)
            importances(lastaccess).z != Important &&
              importances(lastaccess).n != Important &&
              importances(lastaccess).a != Important
          case SLO | SRE | RLA =>
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
      optimizationContext.log.debug(s"Removing pointless store(s) to ${badVariables.mkString(", ")}")
      code.zipWithIndex.filter(x => !toRemove(x._2)).map(_._1)
    }
  }
}
