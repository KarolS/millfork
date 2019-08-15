package millfork.assembly.m6809.opt

import millfork.CompilationFlag
import millfork.assembly.OptimizationContext
import millfork.assembly.m6809.{MLine, MLine0}
import millfork.assembly.opt.{AnyStatus, FlowCache, SingleStatus, Status}
import millfork.env._
import millfork.node.{M6809Register, NiceFunctionProperty}

import scala.util.control.Breaks._

/**
  * @author Karol Stasiak
  */
object ForwardFlowAnalysis {

  val cache = new FlowCache[MLine, CpuStatus]("m6809 forward")

  def analyze(f: NormalFunction, code: List[MLine], optimizationContext: OptimizationContext): List[CpuStatus] = {
    cache.get(code).foreach(return _)
    val compilationOptions = optimizationContext.options
    val bpInU = compilationOptions.flag(CompilationFlag.UseUForStack)
    val bpInY = compilationOptions.flag(CompilationFlag.UseYForStack)
    val BP = if (bpInU) M6809Register.U else if (bpInY) M6809Register.Y else M6809Register.CC // CC is a nonsense sentinel value
    val niceFunctionProperties = optimizationContext.niceFunctionProperties
    def extractNiceConstant[T](callee: String)(matcher: NiceFunctionProperty => Option[T]): Status[T] = {
      var result: Status[T] = AnyStatus
      breakable {
        niceFunctionProperties.foreach{ np =>
          if (np._2 == callee) matcher(np._1) match {
            case Some(x) =>
              result = SingleStatus(x)
              break
            case _ =>
          }
        }
      }
      result
    }
    val initialStatus = CpuStatus.initialStatusStandard
    val functionStartStatus = CpuStatus.initialStatusStandard
    val emptyStatus = CpuStatus()
    val flagArray = Array.fill[CpuStatus](code.length)(emptyStatus)
    val codeArray = code.toArray

    var changed = true
    while (changed) {
      changed = false
      var currentStatus: CpuStatus = functionStartStatus
      for (i <- codeArray.indices) {
        import millfork.assembly.m6809.MOpcode._
        if (flagArray(i) != currentStatus) {
          changed = true
          flagArray(i) = currentStatus
        }
        codeArray(i) match {
          case MLine0(LABEL, _, MemoryAddressConstant(Label(l))) =>
            val L = l
            currentStatus = codeArray.indices.flatMap(j => codeArray(j) match {
              case MLine0(_, _, MemoryAddressConstant(Label(L))) => Some(flagArray(j))
              case _ => None
            }).fold(currentStatus)(_ ~ _)

          case MLine0(JSR, _, MemoryAddressConstant(th)) =>
            currentStatus = initialStatus.copy(
              memStack = currentStatus.memStack,
              u = if (bpInU) currentStatus.u else AnyStatus,
              y = if (bpInY) currentStatus.y else AnyStatus
            )

          case MLine0(JSR | BYTE, _, _) =>
            currentStatus = initialStatus

          case MLine0(NOP, _, _) =>
            ()

          case MLine0(opcode, addrMode, _) =>
            // TODO
            currentStatus = initialStatus
        }
      }
//                  flagArray.zip(codeArray).foreach{
//                    case (fl, y) => if (y.isPrintable) println(f"$fl%-32s $y%-32s")
//                  }
//                  println("---------------------")
    }

    cache.put(code, flagArray.toList)
  }
}
