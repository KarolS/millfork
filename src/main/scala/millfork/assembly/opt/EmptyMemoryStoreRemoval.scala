package millfork.assembly.opt

import millfork.CompilationOptions
import millfork.assembly.AssemblyLine
import millfork.env._
import millfork.assembly.Opcode._
import millfork.error.ErrorReporting

import scala.collection.{immutable, mutable}

/**
  * @author Karol Stasiak
  */
object EmptyMemoryStoreRemoval extends AssemblyOptimization {
  override def name = "Removing pointless stores to automatic variables"

  private val storeOpcodes = Set(STA, STX, STY, STZ, SAX)

  override def optimize(f: NormalFunction, code: List[AssemblyLine], options: CompilationOptions): List[AssemblyLine] = {
    val paramVariables = f.params match {
      case NormalParamSignature(ps) =>
        ps.map(_.name).toSet
      case _ =>
        // assembly functions do not get this optimization
        return code
    }
    val stillUsedVariables = code.flatMap {
      case AssemblyLine(_, _, MemoryAddressConstant(th), _) => Some(th.name)
      case _ => None
    }.toSet
    val localVariables = f.environment.getAllLocalVariables.filter {
      case MemoryVariable(name, typ, VariableAllocationMethod.Auto) =>
        typ.size == 1 && !paramVariables(name) && stillUsedVariables(name)
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
        if (code(lastaccess).opcode match {
          case  STA | STX | STY | SAX =>
            true
          case INC | DEC =>
            if (importances eq null) {
              importances = ReverseFlowAnalyzer.analyze(f, code)
            }
            importances(lastaccess).z != Important && importances(lastaccess).n != Important
          case _ => // last variable access is important, or we're in a loop, do not remove
            false
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
