package millfork.assembly.m6809.opt

import millfork.CompilationFlag
import millfork.assembly.OptimizationContext
import millfork.assembly.m6809.{Absolute, Immediate, Inherent, InherentA, InherentB, MLine, MLine0, TwoRegisters}
import millfork.assembly.opt.Status.SingleFalse
import millfork.assembly.opt.{AnyStatus, FlowCache, SingleStatus, Status}
import millfork.env._
import millfork.node.M6809NiceFunctionProperty.{DoesntChangeA, DoesntChangeB, DoesntChangeU, DoesntChangeX, DoesntChangeY}
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

          case MLine0(JSR, am, MemoryAddressConstant(th)) =>
            var prU = bpInU
            var prY = bpInY
            var prA = false
            var prB = false
            var prX = false
            (am, th) match {
              case (Absolute(false), fun: FunctionInMemory) =>
                val nfp = optimizationContext.niceFunctionProperties
                val fn = fun.name
                if (nfp(DoesntChangeX -> fn)) prX = true
                if (nfp(DoesntChangeY -> fn)) prY = true
                if (nfp(DoesntChangeU -> fn)) prU = true
                if (nfp(DoesntChangeA -> fn)) prA = true
                if (nfp(DoesntChangeB -> fn)) prB = true
              case _ =>
            }
            currentStatus = initialStatus.copy(
              memStack = currentStatus.memStack,
              u = if (prU) currentStatus.u else AnyStatus,
              x = if (prX) currentStatus.x else AnyStatus,
              y = if (prY) currentStatus.y else AnyStatus,
              a = if (prA) currentStatus.a else AnyStatus,
              b = if (prB) currentStatus.b else AnyStatus
            )

          case MLine0(JSR | BYTE, _, _) =>
            currentStatus = initialStatus

          case MLine0(op, Immediate, constant) if ForwardFlowAnalysisForImmediate.hasDefinition(op) =>
            ForwardFlowAnalysisForImmediate.get(op)(constant, currentStatus)
          case MLine0(op, Inherent, _) if ForwardFlowAnalysisForInherent.hasDefinition(op) =>
            ForwardFlowAnalysisForInherent.get(op)(currentStatus)
          case MLine0(op, InherentA, _) if ForwardFlowAnalysisForInherentA.hasDefinition(op) =>
            ForwardFlowAnalysisForInherentA.get(op)(currentStatus)
          case MLine0(op, InherentB, _) if ForwardFlowAnalysisForInherentB.hasDefinition(op) =>
            ForwardFlowAnalysisForInherentB.get(op)(currentStatus)

          case MLine0(LDA, _, _) =>
            currentStatus = currentStatus.copy(a = AnyStatus, n = AnyStatus, z = AnyStatus, v = SingleFalse)
          case MLine0(LDB, _, _) =>
            currentStatus = currentStatus.copy(b = AnyStatus, n = AnyStatus, z = AnyStatus, v = SingleFalse)
          case MLine0(LDD, _, _) =>
            currentStatus = currentStatus.copy(a = AnyStatus, b = AnyStatus, n = AnyStatus, z = AnyStatus, v = SingleFalse)
          case MLine0(LDX, _, _) =>
            currentStatus = currentStatus.copy(x = AnyStatus, n = AnyStatus, z = AnyStatus, v = SingleFalse)
          case MLine0(LDY, _, _) =>
            currentStatus = currentStatus.copy(y = AnyStatus, n = AnyStatus, z = AnyStatus, v = SingleFalse)
          case MLine0(STA | STB | STD | STX | STU | STY | STS, _, _) =>
            // don't change
          case MLine0(TFR, TwoRegisters(source, target), _) =>
            import M6809Register._
            (source, target) match {
              case (A, B) => currentStatus = currentStatus.copy(b = currentStatus.a)
              case (B, A) => currentStatus = currentStatus.copy(a = currentStatus.b)
              case (CC, A) => currentStatus = currentStatus.copy(a = AnyStatus)
              case (CC, B) => currentStatus = currentStatus.copy(b = AnyStatus)
              case (A, CC) | (B, CC) => currentStatus = currentStatus.copy(c = AnyStatus, z = AnyStatus, n = AnyStatus, v = AnyStatus)
              case (S, D) => currentStatus = currentStatus.copy(a = AnyStatus, b = AnyStatus)
              case (S, X) => currentStatus = currentStatus.copy(x = AnyStatus)
              case (S, Y) => currentStatus = currentStatus.copy(y = AnyStatus)
              case (S, U) => currentStatus = currentStatus.copy(u = AnyStatus)
              case (D, X) => currentStatus = currentStatus.copy(x = currentStatus.d.map(n => NumericConstant(n, 2)))
              case (D, Y) => currentStatus = currentStatus.copy(y = currentStatus.d.map(n => NumericConstant(n, 2)))
              case (D, U) => currentStatus = currentStatus.copy(u = currentStatus.d.map(n => NumericConstant(n, 2)))
              case (X, Y) => currentStatus = currentStatus.copy(y = currentStatus.x)
              case (X, U) => currentStatus = currentStatus.copy(u = currentStatus.x)
              case (Y, X) => currentStatus = currentStatus.copy(x = currentStatus.y)
              case (Y, U) => currentStatus = currentStatus.copy(u = currentStatus.y)
              case (U, X) => currentStatus = currentStatus.copy(x = currentStatus.u)
              case (U, Y) => currentStatus = currentStatus.copy(y = currentStatus.u)
              case (X, D) =>
                val (h, l) = currentStatus.x.toHiLo
                currentStatus = currentStatus.copy(a = h, b = l)
              case (Y, D) =>
                val (h, l) = currentStatus.y.toHiLo
                currentStatus = currentStatus.copy(a = h, b = l)
              case (U, D) =>
                val (h, l) = currentStatus.u.toHiLo
                currentStatus = currentStatus.copy(a = h, b = l)
              case _ => currentStatus = initialStatus
            }
          case MLine0(EXG, TwoRegisters(source, target), _) =>
            import M6809Register._
            (source, target) match {
              case (A, B) | (B, A) => currentStatus = currentStatus.copy(a = currentStatus.b, b = currentStatus.a)
              case (A, CC) | (CC, A) => currentStatus = currentStatus.copy(a = AnyStatus, c = AnyStatus, v = AnyStatus, n = AnyStatus, z = AnyStatus)
              case (B, CC) | (CC, B) => currentStatus = currentStatus.copy(b = AnyStatus, c = AnyStatus, v = AnyStatus, n = AnyStatus, z = AnyStatus)
              case (X, Y) | (Y, X) => currentStatus = currentStatus.copy(y = currentStatus.x, x = currentStatus.y)
              case (U, Y) | (Y, U) => currentStatus = currentStatus.copy(y = currentStatus.u, u = currentStatus.y)
              case (X, U) | (U, X) => currentStatus = currentStatus.copy(u = currentStatus.x, x = currentStatus.u)
              case (X, D) | (D, X) =>
                val (h, l) = currentStatus.x.toHiLo
                currentStatus = currentStatus.copy(a = h, b = l, x = currentStatus.d.map(n => NumericConstant(n, 2)))
              case (Y, D) | (D, Y) =>
                val (h, l) = currentStatus.y.toHiLo
                currentStatus = currentStatus.copy(a = h, b = l, y = currentStatus.d.map(n => NumericConstant(n, 2)))
              case (U, D) | (D, U) =>
                val (h, l) = currentStatus.u.toHiLo
                currentStatus = currentStatus.copy(a = h, b = l, u = currentStatus.d.map(n => NumericConstant(n, 2)))
              case _ => currentStatus = initialStatus
            }

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
