package millfork.assembly.z80.opt

import millfork.assembly.opt.SingleStatus
import millfork.assembly.z80.{NoRegisters, OneRegister, TwoRegisters, ZLine, ZLine0, ZOpcode}
import millfork.env._
import millfork.error.ConsoleLogger
import millfork.node.ZRegister

/**
  * @author Karol Stasiak
  */
object VariableLifetime {

  // This only works for non-stack variables.
  // TODO: this is also probably very wrong
  def apply(variableName: String, codeWithFlow: List[(FlowInfo, ZLine)]): Range = {
    import ZRegister._
    import ZOpcode._

    val pointerLeaks: Boolean = isPointerLeaky(codeWithFlow, variableName)
    val pointerReadAt = codeWithFlow.zipWithIndex.filter{
      case ((_, ZLine0(_, TwoRegisters(MEM_HL | MEM_DE | MEM_BC, _), _)), _) => true
      case ((_, ZLine0(_, TwoRegisters(_, MEM_HL | MEM_DE | MEM_BC), _)), _) => true
      case ((_, ZLine0(_, OneRegister(MEM_HL | MEM_DE | MEM_BC), _)), _) => true
      case ((_, ZLine0(CALL, _, _)), _) => true
      case _ => false
    }.map(_._2)


    val flags = codeWithFlow.map {
      case (_, ZLine0(_, _, MemoryAddressConstant(MemoryVariable(n, _, _)))) => n == variableName
      case (_, ZLine0(_, _, CompoundConstant(MathOperator.Plus, MemoryAddressConstant(MemoryVariable(n, _, _)), NumericConstant(_, 1)))) => n == variableName
      case (i, ZLine0(_, TwoRegisters(MEM_HL, _) | TwoRegisters(_, MEM_HL) | OneRegister(MEM_HL), _)) =>
        i.statusBefore.hl match {
          case SingleStatus(MemoryAddressConstant(MemoryVariable(n, _, _))) => n == variableName
          case SingleStatus(CompoundConstant(MathOperator.Plus, MemoryAddressConstant(MemoryVariable(n, _, _)), NumericConstant(_, 1))) => n == variableName
          case _ => false
        }
      case _ => false
    }.toArray

    if(pointerLeaks) {
      pointerReadAt.foreach(i => flags(i) = true)
    }

    val code = codeWithFlow.map(_._2)
    val range = expandRangeToCoverLoops(code, flags)

//    val log = new ConsoleLogger
//    log.verbosity = 3
//    log.trace("Lifetime for " + variableName)
//    code.zipWithIndex.foreach {
//      case (line, index) =>
//        if (index >= range.start && index < range.end) {
//          log.trace(f"$line%-42s  <")
//        } else {
//          log.trace(line.toString)
//        }
//    }

    range
  }


  def isPointerLeaky(codeWithFlow: List[(FlowInfo, ZLine)], variableName: String): Boolean = {
    if (codeWithFlow.isEmpty) return false
    var i = 0
    var inHl = false
    var inBc = false
    var inDe = false
    import ZOpcode._
    import ZRegister._
    var previousFlow = codeWithFlow.head._1
    def fail(line: ZLine): Unit = {
//      println(s"Pointer for variable $variableName leaks because of $line")
    }

    for((flow, line) <- codeWithFlow) {
      val imp = flow.importanceAfter
      line match {
        case ZLine0(ZOpcode.LD_16, TwoRegisters(HL, IMM_16), MemoryAddressConstant(MemoryVariable(n, _, _))) if n == variableName =>
          inHl = true
        case ZLine0(ZOpcode.LD_16, TwoRegisters(BC, IMM_16), MemoryAddressConstant(MemoryVariable(n, _, _))) if n == variableName =>
          inBc = true
        case ZLine0(ZOpcode.LD_16, TwoRegisters(DE, IMM_16), MemoryAddressConstant(MemoryVariable(n, _, _))) if n == variableName =>
          inDe = true
        case ZLine0(ZOpcode.LD_16, TwoRegisters(HL, _), _) => inHl = false
        case ZLine0(ZOpcode.LD_16, TwoRegisters(BC, _), _) => inBc = false
        case ZLine0(ZOpcode.LD_16, TwoRegisters(DE, _), _) => inDe = false
        case ZLine0(_, TwoRegisters(_, HL), _) if inHl =>
          fail(line)
          return true
        case ZLine0(_, TwoRegisters(_, BC), _) if inBc =>
          fail(line)
          return true
        case ZLine0(_, TwoRegisters(_, DE), _) if inDe =>
          fail(line)
          return true
        case ZLine0(LD_HLSP, _, _) => inHl = false
        case ZLine0(LD_DESP, _, _) => inDe = false
        case ZLine0(LHLX, _, _) if inHl => inHl = false
        case ZLine0(SHLX, _, _) if inDe || inHl =>
          fail(line)
          return false
        case ZLine0(POP, OneRegister(HL), _) => inHl = false
        case ZLine0(POP, OneRegister(BC), _) => inBc = false
        case ZLine0(POP, OneRegister(DE), _) => inDe = false
        case ZLine0(PUSH, OneRegister(HL), _) if inHl =>
          fail(line)
          return true
        case ZLine0(PUSH, OneRegister(BC), _) if inBc =>
          fail(line)
          return true
        case ZLine0(PUSH, OneRegister(DE), _) if inDe =>
          fail(line)
          return true
        case ZLine0(RET | RETI | RETN | JP | JR, NoRegisters, _) =>
          inHl = false
          inBc = false
          inDe = false
        case ZLine0(LABEL, _, _) if inHl && (imp.h == Important || imp.l == Important) =>
          fail(line)
          return true
        case ZLine0(LABEL, _, _) if inBc && (imp.b == Important || imp.c == Important) =>
          fail(line)
          return true
        case ZLine0(LABEL, _, _) if inDe && (imp.d == Important || imp.e == Important) =>
          fail(line)
          return true
        case _ if !line.accessesMemoryViaGivenRegister(MEM_HL) && line.readsRegister(H) && inHl =>
          fail(line)
          return true
        case _ if !line.accessesMemoryViaGivenRegister(MEM_HL) && line.readsRegister(L) && inHl =>
          fail(line)
          return true
        case _ if !line.accessesMemoryViaGivenRegister(MEM_BC) && line.readsRegister(B) && inBc =>
          fail(line)
          return true
        case _ if !line.accessesMemoryViaGivenRegister(MEM_BC) && line.readsRegister(C) && inBc =>
          fail(line)
          return true
        case _ if !line.accessesMemoryViaGivenRegister(MEM_DE) && line.readsRegister(D) && inDe =>
          fail(line)
          return true
        case _ if !line.accessesMemoryViaGivenRegister(MEM_DE) && line.readsRegister(E) && inDe =>
          fail(line)
          return true
        case ZLine0(LD, TwoRegisters(H | L, _), _) => inHl = false
        case ZLine0(LD, TwoRegisters(B | C, _), _) => inBc = false
        case ZLine0(LD, TwoRegisters(D | E, _), _) => inDe = false
        case _ => // TODO: ???
      }
      i += 1
      previousFlow = flow
    }
    false
  }

  def expandRangeToCoverLoops(code: List[ZLine], flags: Array[Boolean]): Range = {
    if (flags.forall(!_)) return Range(0, 0)
    var min = flags.indexOf(true)
    var max = flags.lastIndexOf(true) + 1
    var changed = true
    val labelMap = code.zipWithIndex.flatMap(a => a._1.parameter match {
      case MemoryAddressConstant(Label(l)) => List(l -> a._2)
      case _ => Nil
    }).groupBy(_._1).mapValues(_.map(_._2).toSet).view.force

    while (changed) {
      changed = false
      for ((label, indices) <- labelMap) {
        if (indices.exists(i => i >= min && i < max)) {
          indices.foreach { i =>
            val before = max - min
            min = min min i
            max = max max (i + 1)
            if (max - min != before) {
              changed = true
            }
          }
        }
      }
    }

    Range(min, max)
  }
}
