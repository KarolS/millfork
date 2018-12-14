package millfork.assembly.mos.opt

import millfork.CompilationOptions
import millfork.assembly.{AssemblyOptimization, Elidability, OptimizationContext}
import millfork.assembly.mos.Opcode._
import millfork.assembly.mos.AddrMode._
import millfork.assembly.mos.{AssemblyLine, AssemblyLine0, OpcodeClasses}
import millfork.assembly.opt.SingleStatus
import millfork.env._
import millfork.error.{ConsoleLogger, Logger}

/**
  * @author Karol Stasiak
  */
object LocalVariableReadOptimization extends AssemblyOptimization[AssemblyLine] {

  override def name: String = "Local variable read optimization"

  override def optimize(f: NormalFunction, code: List[AssemblyLine], optimizationContext: OptimizationContext): List[AssemblyLine] = {

    val stillUsedVariables = code.flatMap {
      case AssemblyLine0(_, _, MemoryAddressConstant(th: MemoryVariable)) => th match {
        case MemoryVariable(name, typ, VariableAllocationMethod.Auto | VariableAllocationMethod.Register)
          if typ.size == 1 => Some(name)
        case _ => None
      }
      case _ => None
    }.toSet
    val variablesWithAddressesTaken = code.flatMap {
      case AssemblyLine0(_, _, SubbyteConstant(MemoryAddressConstant(th), _)) => Some(th.name)
      case _ => None
    }.toSet
    val eligibleVariables = (stillUsedVariables -- variablesWithAddressesTaken).filterNot(_.startsWith("__"))

    if (eligibleVariables.isEmpty) {
      return code
    }

    val statuses = CoarseFlowAnalyzer.analyze(f, code, optimizationContext)
    val (optimized, result) = optimizeImpl(code.zip(statuses), eligibleVariables, Map())
    if (optimized) {
      optimizationContext.log.debug("Optimized local variable reads")
      reportOptimizedBlock(optimizationContext.log, code, result)
      result
    } else {
      code
    }
  }

  private implicit class TupleOps(val tuple: (Boolean, List[AssemblyLine])) {
    def ::(head: AssemblyLine): (Boolean, List[AssemblyLine]) = (tuple._1, head :: tuple._2)
  }

  def optimizeImpl(code: List[(AssemblyLine, CpuStatus)], variables: Set[String], map: Map[String, Int]): (Boolean, List[AssemblyLine]) = code match {

    case (AssemblyLine(op@(
      LDA | LDX | LDY | LDZ |
      ADC | ORA | EOR | AND | SBC |
      CMP | CPX | CPY | CPZ), Absolute | ZeroPage, MemoryAddressConstant(th), Elidability.Elidable, s), _) :: xs
      if variables(th.name) && map.contains(th.name) =>
      true -> (AssemblyLine.immediate(op, map(th.name)).pos(s) :: optimizeImpl(xs, variables, map)._2)

    case (x@AssemblyLine0(STA, Absolute | ZeroPage, MemoryAddressConstant(th)), status) :: xs
      if variables(th.name) =>
      val newMap = status.a match {
        case SingleStatus(n) => map + (th.name -> n)
        case _ => map - th.name
      }
      x :: optimizeImpl(xs, variables, newMap)

    case (x@AssemblyLine0(STX, Absolute | ZeroPage, MemoryAddressConstant(th)), status) :: xs
      if variables(th.name) =>
      val newMap = status.x match {
        case SingleStatus(n) => map + (th.name -> n)
        case _ => map - th.name
      }
      x :: optimizeImpl(xs, variables, newMap)

    case (x@AssemblyLine0(STY, Absolute | ZeroPage, MemoryAddressConstant(th: ThingInMemory)), status) :: xs
      if variables(th.name) =>
      val newMap = status.y match {
        case SingleStatus(n) => map + (th.name -> n)
        case _ => map - th.name
      }
      x :: optimizeImpl(xs, variables, newMap)

    case (x@AssemblyLine0(STZ, Absolute | ZeroPage, MemoryAddressConstant(th)), status) :: xs
      if variables(th.name) =>
      val newMap = status.iz match {
        case SingleStatus(n) => map + (th.name -> n)
        case _ => map - th.name
      }
      x :: optimizeImpl(xs, variables, newMap)

    case (x@AssemblyLine0(SAX, Absolute | ZeroPage, MemoryAddressConstant(th)), status) :: xs
      if variables(th.name) =>
      val newMap = (status.a, status.x) match {
        case (SingleStatus(m), SingleStatus(n)) => map + (th.name -> (m & n))
        case (_, SingleStatus(0)) => map + (th.name -> 0)
        case (SingleStatus(0), _) => map + (th.name -> 0)
        case _ => map - th.name
      }
      x :: optimizeImpl(xs, variables, newMap)

    case (x@AssemblyLine0(INC | ISC, Absolute | ZeroPage, MemoryAddressConstant(th)), _) :: xs
      if map.contains(th.name) =>
      x :: optimizeImpl(xs, variables, map + (th.name -> map(th.name).+(1).&(0xff)))

    case (x@AssemblyLine0(DEC | DCP, Absolute | ZeroPage, MemoryAddressConstant(th)), _) :: xs
      if map.contains(th.name) =>
      x :: optimizeImpl(xs, variables, map + (th.name -> map(th.name).-(1).&(0xff)))

    case (x@AssemblyLine0(ASL | SLO, Absolute | ZeroPage, MemoryAddressConstant(th)), _) :: xs
      if map.contains(th.name) =>
      x :: optimizeImpl(xs, variables, map + (th.name -> map(th.name).<<(1).&(0xff)))

    case (x@AssemblyLine0(LSR | SRE, Absolute | ZeroPage, MemoryAddressConstant(th)), _) :: xs
      if map.contains(th.name) =>
      x :: optimizeImpl(xs, variables, map + (th.name -> map(th.name).&(0xff).>>(1)))

      // TODO: consider handling some more opcodes
    case (x@AssemblyLine0(op, Absolute | ZeroPage, MemoryAddressConstant(th)), _) :: xs
      if OpcodeClasses.ChangesMemoryAlways(op) && map.contains(th.name) =>
      x :: optimizeImpl(xs, variables, map - th.name)

    case (x@AssemblyLine0(LABEL, _, _), _) :: xs => x :: optimizeImpl(xs, variables, Map())
    case (x, _) :: xs => x :: optimizeImpl(xs, variables, map)
    case Nil => (false, Nil)
  }

  def reportOptimizedBlock(log: Logger, oldCode: List[AssemblyLine], newCode: List[AssemblyLine]): Unit = {
    if (log.traceEnabled) {
      oldCode.foreach(l => log.trace(l.toString))
      log.trace("     ↓")
      newCode.foreach(l => log.trace(l.toString))
    }
  }
}
