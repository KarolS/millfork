package millfork.assembly.mos.opt

import millfork.assembly.mos.{AssemblyLine, AssemblyLine0, OpcodeClasses, State}
import millfork.env._
import millfork.error.ConsoleLogger
import millfork.node.NiceFunctionProperty

/**
  * @author Karol Stasiak
  */
object VariableLifetime {

  // This only works for non-stack variables.
  def apply(variableName: String, code: List[AssemblyLine], expandToIncludeIndexing: Boolean = false, expandToIncludeUsesOfLoadedIndices: Option[Set[(NiceFunctionProperty, String)]] = None): Range = {
    val flags = code.map(_.parameter match {
      case MemoryAddressConstant(MemoryVariable(n, _, _)) if n == variableName => true
      case CompoundConstant(MathOperator.Plus, MemoryAddressConstant(MemoryVariable(n, _, _)), NumericConstant(_, 1)) if n == variableName => true
      case _ => false
    })
    if (flags.forall(!_)) return Range(0, 0)
    var min = flags.indexOf(true)
    var max = flags.lastIndexOf(true) + 1
    var changed = true
    val labelMap = code.zipWithIndex.flatMap(a => a._1.parameter match {
      case MemoryAddressConstant(Label(l)) => List(l -> a._2)
      case StructureConstant(_, List(_, MemoryAddressConstant(Label(l)))) => List(l -> a._2)
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

    if (expandToIncludeIndexing) {
      import millfork.assembly.mos.Opcode._
      import millfork.assembly.mos.AddrMode._
      val linearChuckAfter = code.drop(max).takeWhile{ line => line.opcode match {
        case LABEL | JSR | BSR | RTS | RTI => false
        case op if OpcodeClasses.AllDirectJumps(op) => false
        case _ => true
      }}
      val lastIndexing = linearChuckAfter.lastIndexWhere(line => line.addrMode match {
        case IndexedY | IndexedX | IndexedSY => true
        case AbsoluteY | AbsoluteX | LongAbsoluteX => true
        case ZeroPageX | ZeroPageY => true
        case _ => false
      })
      if (lastIndexing >= 0) {
        max += lastIndexing + 1
      }
    }

    if (expandToIncludeUsesOfLoadedIndices.isDefined) {
      // if the range ends with something like `LDY variableName`, then include the lifetime of that value in the register
      // does not handle the Z register
      import millfork.assembly.mos.Opcode._
      val flow = ReverseFlowAnalyzer.analyze(code, expandToIncludeUsesOfLoadedIndices.get)
      val maskX = Array.fill(code.length)(false)
      val maskY = Array.fill(code.length)(false)
      def mark(start: Int, mask: Array[Boolean], state: CpuImportance => Importance, readsReg: AssemblyLine => Boolean): Unit = {
        var i = start
        if (mask(i)) return
        while (true) {
          val line = code(i)
          println(line)
          if (state(flow(i)) != Important && !readsReg(line)) return
          println("masking...")
          mask(i) = true
          val op = code(i).opcode
          line match {
            case AssemblyLine0(_, _, MemoryAddressConstant(Label(l1))) if OpcodeClasses.AllDirectJumps(op) =>
              for (j <- labelMap.getOrElse(l1, Set())) {
                mark(j, mask, state, readsReg)
              }
            case _ =>
          }
          if (op == RTS || op == RTI || op == BRA || op == JMP || op == BRL || op == KIL) return
          if (state(flow(i)) != Important) return
          i += 1
          if (i >= code.length) return
        }
      }
      @inline
      def markX(i: Int): Unit = mark(i, maskX, _.x, _.reads(State.X))
      @inline
      def markY(i: Int): Unit = mark(i, maskY, _.y, _.reads(State.Y))
      code.indices.foreach { i =>
        val l = code(i)
        l match {
          case AssemblyLine0(LDX | LDX_W, _, MemoryAddressConstant(th)) if th.name == variableName => markX(i)
          case AssemblyLine0(LDY | LDY_W, _, MemoryAddressConstant(th)) if th.name == variableName => markY(i)
          case _ =>
        }
      }
      @inline
      def spread(arr: Array[Boolean]): Unit = {
        val first = arr.indexOf(true)
        if (first >= 0) min = min min first
        val last = arr.lastIndexOf(true)
        if (last >= 0) max = max max (last + 1)
      }
      spread(maskX)
      spread(maskY)
    }

//    val log = new ConsoleLogger
//    log.verbosity = 3
//    log.trace("Lifetime for " + variableName)
//    code.zipWithIndex.foreach {
//      case (line, index) =>
//        if (index >= min && index < max) {
//          log.trace(f"$line%-30s  <")
//        } else {
//          log.trace(line.toString)
//        }
//    }

    Range(min, max)
  }
}
