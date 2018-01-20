package millfork.assembly.opt

import millfork.{CompilationFlag, CompilationOptions}
import millfork.assembly._
import millfork.env._
import millfork.error.ErrorReporting

import scala.collection.mutable

/**
  * @author Karol Stasiak
  */

object FlowInfoRequirement extends Enumeration {

  val NoRequirement, JustLabels, BothFlows, ForwardFlow, BackwardFlow = Value

  def assertForward(x: FlowInfoRequirement.Value): Unit = x match {
    case BothFlows | ForwardFlow => ()
    case NoRequirement | JustLabels | BackwardFlow => ErrorReporting.fatal("Forward flow info required")
  }

  def assertBackward(x: FlowInfoRequirement.Value): Unit = x match {
    case BothFlows | BackwardFlow => ()
    case NoRequirement | JustLabels | ForwardFlow => ErrorReporting.fatal("Backward flow info required")
  }
}

class RuleBasedAssemblyOptimization(val name: String, val needsFlowInfo: FlowInfoRequirement.Value, val rules: AssemblyRule*) extends AssemblyOptimization {

  rules.foreach(_.pattern.validate(needsFlowInfo))

  override def optimize(f: NormalFunction, code: List[AssemblyLine], options: CompilationOptions): List[AssemblyLine] = {
    val effectiveCode = code.map(a => a.copy(parameter = a.parameter.quickSimplify))
    val taggedCode = FlowAnalyzer.analyze(f, effectiveCode, options, needsFlowInfo)
    optimizeImpl(f, taggedCode, options)
  }

  def optimizeImpl(f: NormalFunction, code: List[(FlowInfo, AssemblyLine)], options: CompilationOptions): List[AssemblyLine] = {
    code match {
      case Nil => Nil
      case head :: tail =>
        for ((rule, index) <- rules.zipWithIndex) {
          val ctx = new AssemblyMatchingContext
          rule.pattern.matchTo(ctx, code) match {
            case Some(rest: List[(FlowInfo, AssemblyLine)]) =>
              val matchedChunkToOptimize: List[AssemblyLine] = code.take(code.length - rest.length).map(_._2)
              val optimizedChunk: List[AssemblyLine] = rule.result(matchedChunkToOptimize, ctx)
              ErrorReporting.debug(s"Applied $name ($index)")
              if (needsFlowInfo != FlowInfoRequirement.NoRequirement) {
                val before = code.head._1.statusBefore
                val after = code(matchedChunkToOptimize.length - 1)._1.importanceAfter
                ErrorReporting.trace(s"Before: $before")
                ErrorReporting.trace(s"After:  $after")
              }
              matchedChunkToOptimize.filter(_.isPrintable).foreach(l => ErrorReporting.trace(l.toString))
              ErrorReporting.trace("     ↓")
              optimizedChunk.filter(_.isPrintable).foreach(l => ErrorReporting.trace(l.toString))
              if (needsFlowInfo != FlowInfoRequirement.NoRequirement) {
                return optimizedChunk ++ optimizeImpl(f, rest, options)
              } else {
                return optimize(f, optimizedChunk ++ rest.map(_._2), options)
              }
            case None => ()
          }
        }
        head._2 :: optimizeImpl(f, tail, options)
    }
  }
}

class AssemblyMatchingContext {
  private val map = mutable.Map[Int, Any]()

  def addObject(i: Int, o: Any): Boolean = {
    if (map.contains(i)) {
      map(i) == o
    } else {
      map(i) = o
      true
    }
  }

  def dontMatch(i: Int, o: Any): Boolean = {
    if (map.contains(i)) {
      map(i) != o
    } else {
      false
    }
  }

  def get[T: Manifest](i: Int): T = {
    val t = map(i)
    val clazz = implicitly[Manifest[T]].runtimeClass match {
      case java.lang.Integer.TYPE => classOf[java.lang.Integer]
      case java.lang.Boolean.TYPE => classOf[java.lang.Boolean]
      // TODO
      case x => x
    }
    if (clazz.isInstance(t)) {
      t.asInstanceOf[T]
    } else {
      if (i eq null) {
        ErrorReporting.fatal(s"Value at index $i is null")
      } else {
        ErrorReporting.fatal(s"Value at index $i is a ${t.getClass.getSimpleName}, not a ${clazz.getSimpleName}")
      }
    }
  }

  def isExternallyLinearBlock(i: Int): Boolean = {
    val labels = mutable.Set[String]()
    val jumps = mutable.Set[String]()
    get[List[AssemblyLine]](i).foreach {
      case AssemblyLine(Opcode.RTS | Opcode.RTI | Opcode.BRK, _, _, _) =>
        return false
      case AssemblyLine(Opcode.JMP, AddrMode.Indirect, _, _) =>
        return false
      case AssemblyLine(Opcode.LABEL, _, MemoryAddressConstant(Label(l)), _) =>
        labels += l
      case AssemblyLine(Opcode.JMP, AddrMode.Absolute, MemoryAddressConstant(Label(l)), _) =>
        jumps += l
      case AssemblyLine(Opcode.JMP, AddrMode.Absolute, _, _) =>
        return false
      case AssemblyLine(_, AddrMode.Relative, MemoryAddressConstant(Label(l)), _) =>
        jumps += l
      case AssemblyLine(br, _, _, _) if OpcodeClasses.ShortBranching(br) =>
        return false
      case _ => ()
    }
    // if a jump leads inside the block, then it's internal
    // if a jump leads outside the block, then it's external
    jumps --= labels
    jumps.isEmpty
  }

  def areMemoryReferencesProvablyNonOverlapping(param1: Int, addrMode1: Int, param2: Int, addrMode2: Int): Boolean = {
    val p1 = get[Constant](param1).quickSimplify
    val a1 = get[AddrMode.Value](addrMode1)
    val p2 = get[Constant](param2).quickSimplify
    val a2 = get[AddrMode.Value](addrMode2)
    import AddrMode._
    val badAddrModes = Set(IndexedX, IndexedY, ZeroPageIndirect, AbsoluteIndexedX)
    if (badAddrModes(a1) || badAddrModes(a2)) return false

    def handleKnownDistance(distance: Short): Boolean = {
      val indexingAddrModes = Set(AbsoluteIndexedX, AbsoluteX, ZeroPageX, AbsoluteY, ZeroPageY)
      val a1Indexing = indexingAddrModes(a1)
      val a2Indexing = indexingAddrModes(a2)
      (a1Indexing, a2Indexing) match {
        case (false, false) => distance != 0
        case (true, false) => distance > 255 || distance < 0
        case (false, true) => distance > 0 || distance < -255
        case (true, true) => distance > 255 || distance < -255
      }
    }

    (p1, p2) match {
      case (NumericConstant(n1, _), NumericConstant(n2, _)) =>
        handleKnownDistance((n2 - n1).toShort)
      case (a, CompoundConstant(MathOperator.Plus, b, NumericConstant(distance, _))) if a.quickSimplify == b.quickSimplify =>
        handleKnownDistance(distance.toShort)
      case (CompoundConstant(MathOperator.Plus, a, NumericConstant(distance, _)), b) if a.quickSimplify == b.quickSimplify =>
        handleKnownDistance((-distance).toShort)
      case (a, CompoundConstant(MathOperator.Minus, b, NumericConstant(distance, _))) if a.quickSimplify == b.quickSimplify =>
        handleKnownDistance((-distance).toShort)
      case (CompoundConstant(MathOperator.Minus, a, NumericConstant(distance, _)), b) if a.quickSimplify == b.quickSimplify =>
        handleKnownDistance(distance.toShort)
      case (MemoryAddressConstant(MemoryVariable(a, _, _)), MemoryAddressConstant(MemoryVariable(b, _, _))) =>
        a.takeWhile(_ != '.') != a.takeWhile(_ != '.') // TODO: ???
      case _ =>
        false
    }
  }
}

case class AssemblyRule(pattern: AssemblyPattern, result: (List[AssemblyLine], AssemblyMatchingContext) => List[AssemblyLine]) {

}

trait AssemblyPattern {

  def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = ()

  def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, AssemblyLine)]): Option[List[(FlowInfo, AssemblyLine)]]

  def ~(x: AssemblyPattern) = Concatenation(this, x)

  def ~(x: AssemblyLinePattern) = Concatenation(this, x)

  def ~~>(result: (List[AssemblyLine], AssemblyMatchingContext) => List[AssemblyLine]) = AssemblyRule(this, result)

  def ~~>(result: List[AssemblyLine] => List[AssemblyLine]) = AssemblyRule(this, (code, _) => result(code))

  def capture(i: Int) = Capture(i, this)

  def captureLength(i: Int) = CaptureLength(i, this)

  protected def memoryAccessDoesntOverlap(a1: AddrMode.Value, p1: Constant, a2: AddrMode.Value, p2: Constant): Boolean = {
    import AddrMode._
    val badAddrModes = Set(IndexedX, IndexedY, ZeroPageIndirect, AbsoluteIndexedX)
    if (badAddrModes(a1) || badAddrModes(a2)) return false
    val goodAddrModes = Set(Implied, Immediate, Relative)
    if (goodAddrModes(a1) || goodAddrModes(a2)) return true

    def handleKnownDistance(distance: Short): Boolean = {
      val indexingAddrModes = Set(AbsoluteIndexedX, AbsoluteX, ZeroPageX, AbsoluteY, ZeroPageY)
      val a1Indexing = indexingAddrModes(a1)
      val a2Indexing = indexingAddrModes(a2)
      (a1Indexing, a2Indexing) match {
        case (false, false) => distance != 0
        case (true, false) => distance > 255 || distance < 0
        case (false, true) => distance > 0 || distance < -255
        case (true, true) => distance > 255 || distance < -255
      }
    }

    (p1.quickSimplify, p2.quickSimplify) match {
      case (NumericConstant(n1, _), NumericConstant(n2, _)) =>
        handleKnownDistance((n2 - n1).toShort)
      case (a, CompoundConstant(MathOperator.Plus, b, NumericConstant(distance, _))) if a.quickSimplify == b.quickSimplify =>
        handleKnownDistance(distance.toShort)
      case (CompoundConstant(MathOperator.Plus, a, NumericConstant(distance, _)), b) if a.quickSimplify == b.quickSimplify =>
        handleKnownDistance((-distance).toShort)
      case (a, CompoundConstant(MathOperator.Minus, b, NumericConstant(distance, _))) if a.quickSimplify == b.quickSimplify =>
        handleKnownDistance((-distance).toShort)
      case (CompoundConstant(MathOperator.Minus, a, NumericConstant(distance, _)), b) if a.quickSimplify == b.quickSimplify =>
        handleKnownDistance(distance.toShort)
      case (MemoryAddressConstant(_: ThingInMemory), NumericConstant(_, _)) =>
        true // TODO: ???
      case (NumericConstant(_, _), MemoryAddressConstant(_: ThingInMemory)) =>
        true // TODO: ???
      case (CompoundConstant(MathOperator.Plus | MathOperator.Minus, MemoryAddressConstant(a: ThingInMemory), NumericConstant(_, _)), NumericConstant(_, _)) =>
        true // TODO: ???
      case (NumericConstant(_, _), CompoundConstant(MathOperator.Plus | MathOperator.Minus, MemoryAddressConstant(a: ThingInMemory), NumericConstant(_, _))) =>
        true // TODO: ???
      case (MemoryAddressConstant(a: ThingInMemory), MemoryAddressConstant(b: ThingInMemory)) =>
        a.name.takeWhile(_ != '.') != b.name.takeWhile(_ != '.') // TODO: ???
      case (CompoundConstant(MathOperator.Plus | MathOperator.Minus, MemoryAddressConstant(a: ThingInMemory), NumericConstant(_, _)),
      MemoryAddressConstant(b: ThingInMemory)) =>
        a.name.takeWhile(_ != '.') != b.name.takeWhile(_ != '.') // TODO: ???
      case (MemoryAddressConstant(a: ThingInMemory),
      CompoundConstant(MathOperator.Plus | MathOperator.Minus, MemoryAddressConstant(b: ThingInMemory), NumericConstant(_, _))) =>
        a.name.takeWhile(_ != '.') != b.name.takeWhile(_ != '.') // TODO: ???
      case (CompoundConstant(MathOperator.Plus | MathOperator.Minus, MemoryAddressConstant(a: ThingInMemory), NumericConstant(_, _)),
      CompoundConstant(MathOperator.Plus | MathOperator.Minus, MemoryAddressConstant(b: ThingInMemory), NumericConstant(_, _))) =>
        a.name.takeWhile(_ != '.') != b.name.takeWhile(_ != '.') // TODO: ???
      case _ =>
        false
    }
  }
}

case class Capture(i: Int, pattern: AssemblyPattern) extends AssemblyPattern {
  override def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, AssemblyLine)]): Option[List[(FlowInfo, AssemblyLine)]] =
    for {
      rest <- pattern.matchTo(ctx, code)
    } yield {
      ctx.addObject(i, code.take(code.length - rest.length).map(_._2))
      rest
    }

  override def toString: String = s"(?<$i>$pattern)"
}

case class CaptureLength(i: Int, pattern: AssemblyPattern) extends AssemblyPattern {
  override def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, AssemblyLine)]): Option[List[(FlowInfo, AssemblyLine)]] =
    for {
      rest <- pattern.matchTo(ctx, code)
    } yield {
      ctx.addObject(i, code.length - rest.length)
      rest
    }

  override def toString: String = s"(?<$i>$pattern)"
}


case class Where(predicate: (AssemblyMatchingContext => Boolean)) extends AssemblyPattern {
  def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, AssemblyLine)]): Option[List[(FlowInfo, AssemblyLine)]] = {
    if (predicate(ctx)) Some(code) else None
  }

  override def toString: String = "Where(...)"
}

case class Concatenation(l: AssemblyPattern, r: AssemblyPattern) extends AssemblyPattern {

  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = {
    l.validate(needsFlowInfo)
    r.validate(needsFlowInfo)
  }

  def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, AssemblyLine)]): Option[List[(FlowInfo, AssemblyLine)]] = {
    for {
      middle <- l.matchTo(ctx, code)
      end <- r.matchTo(ctx, middle)
    } yield end
  }

  override def toString: String = (l, r) match {
    case (_: Both, _: Both) => s"($l) · ($r)"
    case (_, _: Both) => s"$l · ($r)"
    case (_: Both, _) => s"($l) · $r"
    case _ => s"$l · $r"
  }
}

case class Many(rule: AssemblyLinePattern) extends AssemblyPattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = {
    rule.validate(needsFlowInfo)
  }

  def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, AssemblyLine)]): Option[List[(FlowInfo, AssemblyLine)]] = {
    var c = code
    while (true) {
      c match {
        case Nil =>
          return Some(Nil)
        case x :: xs =>
          if (rule.matchLineTo(ctx, x._1, x._2)) {
            c = xs
          } else {
            return Some(c)
          }
      }
    }
    None
  }

  override def toString: String = s"[$rule]*"
}

case class ManyWhereAtLeastOne(rule: AssemblyLinePattern, atLeastOneIsThis: AssemblyLinePattern) extends AssemblyPattern {

  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = {
    rule.validate(needsFlowInfo)
  }

  def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, AssemblyLine)]): Option[List[(FlowInfo, AssemblyLine)]] = {
    var c = code
    var oneFound = false
    while (true) {
      c match {
        case Nil =>
          return Some(Nil)
        case x :: xs =>
          if (atLeastOneIsThis.matchLineTo(ctx, x._1, x._2)) {
            oneFound = true
          }
          if (rule.matchLineTo(ctx, x._1, x._2)) {
            c = xs
          } else {
            if (oneFound) {
              return Some(c)
            } else {
              return None
            }
          }
      }
    }
    None
  }

  override def toString: String = s"[∃$atLeastOneIsThis:$rule]*"
}

case class Opt(rule: AssemblyLinePattern) extends AssemblyPattern {

  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = {
    rule.validate(needsFlowInfo)
  }

  def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, AssemblyLine)]): Option[List[(FlowInfo, AssemblyLine)]] = {
    code match {
      case Nil =>
        Some(Nil)
      case x :: xs =>
        if (rule.matchLineTo(ctx, x._1, x._2)) {
          Some(xs)
        } else {
          Some(code)
        }
    }
  }

  override def toString: String = s"[$rule]?"
}

trait AssemblyLinePattern extends AssemblyPattern {
  def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, AssemblyLine)]): Option[List[(FlowInfo, AssemblyLine)]] = code match {
    case Nil => None
    case x :: xs => if (matchLineTo(ctx, x._1, x._2)) Some(xs) else None
  }

  def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean

  def unary_! : AssemblyLinePattern = Not(this)

  def ? : AssemblyPattern = Opt(this)

  def * : AssemblyPattern = Many(this)

  def + : AssemblyPattern = this ~ Many(this)

  def |(x: AssemblyLinePattern): AssemblyLinePattern = Either(this, x)

  def &(x: AssemblyLinePattern): AssemblyLinePattern = Both(this, x)
}

trait TrivialAssemblyLinePattern extends AssemblyLinePattern with (AssemblyLine => Boolean) {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = this (line)
}

case class WhereNoMemoryAccessOverlapBetweenTwoLineLists(ix1: Int, ix2: Int) extends AssemblyPattern {
  override def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, AssemblyLine)]): Option[List[(FlowInfo, AssemblyLine)]] = {
    val s1s = ctx.get[List[AssemblyLine]](ix1)
    val s2s = ctx.get[List[AssemblyLine]](ix2)
    if (s1s.forall(s1 => s2s.forall(s2 => memoryAccessDoesntOverlap(s1.addrMode, s1.parameter, s2.addrMode, s2.parameter)))) Some(code) else None
  }
}

//noinspection LanguageFeature
object AssemblyLinePattern {
  implicit def __implicitOpcodeIn(ops: Set[Opcode.Value]): AssemblyLinePattern = HasOpcodeIn(ops)
}

case class MatchA(i: Int) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.statusBefore.a match {
      case SingleStatus(value) => ctx.addObject(i, value)
      case _ => false
    }
}

case class MatchX(i: Int) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.statusBefore.x match {
      case SingleStatus(value) => ctx.addObject(i, value)
      case _ => false
    }
}

case class MatchY(i: Int) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.statusBefore.y match {
      case SingleStatus(value) => ctx.addObject(i, value)
      case _ => false
    }
}

case class HasA(value: Int) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.statusBefore.a.contains(value)
}

case class HasX(value: Int) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.statusBefore.x.contains(value)
}

case class HasY(value: Int) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.statusBefore.y.contains(value)
}

case class DoesntMatterWhatItDoesWith(states: State.Value*) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertBackward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    states.forall(state => flowInfo.importanceAfter.isUnimportant(state))

  override def toString: String = states.mkString("[¯\\_(ツ)_/¯:", ",", "]")
}

case class HasSet(state: State.Value) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.hasSet(state)
}

case class HasClear(state: State.Value) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.hasClear(state)
}

case object Anything extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean = true
}

case class Not(inner: AssemblyLinePattern) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = inner.validate(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    !inner.matchLineTo(ctx, flowInfo, line)

  override def toString: String = "¬" + inner
}

case class Both(l: AssemblyLinePattern, r: AssemblyLinePattern) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = {
    l.validate(needsFlowInfo)
    r.validate(needsFlowInfo)
  }

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    l.matchLineTo(ctx, flowInfo, line) && r.matchLineTo(ctx, flowInfo, line)

  override def toString: String = l + " ∧ " + r
}

case class Either(l: AssemblyLinePattern, r: AssemblyLinePattern) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = {
    l.validate(needsFlowInfo)
    r.validate(needsFlowInfo)
  }

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    l.matchLineTo(ctx, flowInfo, line) || r.matchLineTo(ctx, flowInfo, line)

  override def toString: String = s"($l ∨ $r)"
}

case object Elidable extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    line.elidable
}

case object Linear extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    OpcodeClasses.AllLinear(line.opcode)
}

case object LinearOrBranch extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    OpcodeClasses.AllLinear(line.opcode) || OpcodeClasses.ShortBranching(line.opcode)
}

case object LinearOrLabel extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    line.opcode == Opcode.LABEL || OpcodeClasses.AllLinear(line.opcode)
}

case object ReadsA extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    OpcodeClasses.ReadsAAlways(line.opcode) || line.addrMode == AddrMode.Implied && OpcodeClasses.ReadsAIfImplied(line.opcode)
}

case object ReadsMemory extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    line.addrMode match {
      case AddrMode.Indirect => true
      case AddrMode.Implied | AddrMode.Immediate => false
      case _ =>
        OpcodeClasses.ReadsMemoryIfNotImpliedOrImmediate(line.opcode)
    }
}

case object ReadsX extends TrivialAssemblyLinePattern {
  val XAddrModes = Set(AddrMode.AbsoluteX, AddrMode.IndexedX, AddrMode.ZeroPageX, AddrMode.AbsoluteIndexedX)

  override def apply(line: AssemblyLine): Boolean =
    OpcodeClasses.ReadsXAlways(line.opcode) || XAddrModes(line.addrMode)
}

case object ReadsY extends TrivialAssemblyLinePattern {
  val YAddrModes = Set(AddrMode.AbsoluteY, AddrMode.IndexedY, AddrMode.ZeroPageY)

  override def apply(line: AssemblyLine): Boolean =
    OpcodeClasses.ReadsYAlways(line.opcode) || YAddrModes(line.addrMode)
}

case object ConcernsC extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    OpcodeClasses.ReadsC(line.opcode) && OpcodeClasses.ChangesC(line.opcode)
}

case object ConcernsA extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    OpcodeClasses.ConcernsAAlways(line.opcode) || line.addrMode == AddrMode.Implied && OpcodeClasses.ConcernsAIfImplied(line.opcode)
}

case object ConcernsX extends TrivialAssemblyLinePattern {
  val XAddrModes = Set(AddrMode.AbsoluteX, AddrMode.IndexedX, AddrMode.ZeroPageX)

  override def apply(line: AssemblyLine): Boolean =
    OpcodeClasses.ConcernsXAlways(line.opcode) || XAddrModes(line.addrMode)
}

case object ConcernsY extends TrivialAssemblyLinePattern {
  val YAddrModes = Set(AddrMode.AbsoluteY, AddrMode.IndexedY, AddrMode.ZeroPageY)

  override def apply(line: AssemblyLine): Boolean =
    OpcodeClasses.ConcernsYAlways(line.opcode) || YAddrModes(line.addrMode)
}

case object ChangesA extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    OpcodeClasses.ChangesAAlways(line.opcode) || line.addrMode == AddrMode.Implied && OpcodeClasses.ChangesAIfImplied(line.opcode)
}

case object ChangesMemory extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    OpcodeClasses.ChangesMemoryAlways(line.opcode) || line.addrMode != AddrMode.Implied && OpcodeClasses.ChangesMemoryIfNotImplied(line.opcode)
}

case class DoesntChangeMemoryAt(addrMode1: Int, param1: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = {
    val p1 = ctx.get[Constant](param1)
    val p2 = line.parameter
    val a1 = ctx.get[AddrMode.Value](addrMode1)
    val a2 = line.addrMode
    val changesSomeMemory = OpcodeClasses.ChangesMemoryAlways(line.opcode) || line.addrMode != AddrMode.Implied && OpcodeClasses.ChangesMemoryIfNotImplied(line.opcode)
    !changesSomeMemory || memoryAccessDoesntOverlap(a1, p1, a2, p2)
  }
}

case object ConcernsMemory extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    ReadsMemory(line) && ChangesMemory(line)
}

case class DoesNotConcernMemoryAt(addrMode1: Int, param1: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = {
    val p1 = ctx.get[Constant](param1)
    val p2 = line.parameter
    val a1 = ctx.get[AddrMode.Value](addrMode1)
    val a2 = line.addrMode
    memoryAccessDoesntOverlap(a1, p1, a2, p2)
  }
}

case class HasOpcode(op: Opcode.Value) extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    line.opcode == op

  override def toString: String = op.toString
}

case class HasOpcodeIn(ops: Set[Opcode.Value]) extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    ops(line.opcode)

  override def toString: String = ops.mkString("{", ",", "}")
}

case class HasAddrMode(am: AddrMode.Value) extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    line.addrMode == am

  override def toString: String = am.toString
}

case class HasAddrModeIn(ams: Set[AddrMode.Value]) extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    ams(line.addrMode)

  override def toString: String = ams.mkString("{", ",", "}")
}

case class HasImmediate(i: Int) extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    line.addrMode == AddrMode.Immediate && (line.parameter.quickSimplify match {
      case NumericConstant(j, _) => (i & 0xff) == (j & 0xff)
      case _ => false
    })

  override def toString: String = "#" + i
}

case class MatchObject(i: Int, f: Function[AssemblyLine, Any]) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    ctx.addObject(i, f(line))

  override def toString: String = s"(?<$i>...)"
}

case class MatchParameter(i: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    ctx.addObject(i, line.parameter.quickSimplify)

  override def toString: String = s"(?<$i>Param)"
}

case class DontMatchParameter(i: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    ctx.dontMatch(i, line.parameter.quickSimplify)

  override def toString: String = s"¬(?<$i>Param)"
}

case class MatchAddrMode(i: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    ctx.addObject(i, line.addrMode)

  override def toString: String = s"¬(?<$i>AddrMode)"
}

case class MatchOpcode(i: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    ctx.addObject(i, line.opcode)

  override def toString: String = s"¬(?<$i>Op)"
}

case class MatchImmediate(i: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    if (line.addrMode == AddrMode.Immediate) {
      ctx.addObject(i, line.parameter.quickSimplify)
    } else false

  override def toString: String = s"(?<$i>#)"
}


case class DoesntChangeIndexingInAddrMode(i: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    ctx.get[AddrMode.Value](i) match {
      case AddrMode.ZeroPageX | AddrMode.AbsoluteX | AddrMode.IndexedX | AddrMode.AbsoluteIndexedX => !OpcodeClasses.ChangesX.contains(line.opcode)
      case AddrMode.ZeroPageY | AddrMode.AbsoluteY | AddrMode.IndexedY => !OpcodeClasses.ChangesY.contains(line.opcode)
      case _ => true
    }

  override def toString: String = s"¬(?<$i>AddrMode)"
}

case class Before(pattern: AssemblyPattern) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit = {
    pattern.validate(needsFlowInfo)
  }

  override def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, AssemblyLine)]): Option[List[(FlowInfo, AssemblyLine)]] = code match {
    case Nil => None
    case x :: xs => pattern.matchTo(ctx, xs) match {
      case Some(m) => Some(xs)
      case None => None
    }
  }

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = ???
}

case class HasCallerCount(count: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    line match {
      case AssemblyLine(Opcode.LABEL, _, MemoryAddressConstant(Label(l)), _) => flowInfo.labelUseCount(l) == count
      case _ => false
    }
}

case class MatchElidableCopyOf(i: Int, firstLinePattern: AssemblyLinePattern, lastLinePattern: AssemblyLinePattern) extends AssemblyPattern {
  override def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, AssemblyLine)]): Option[List[(FlowInfo, AssemblyLine)]] = {
    val pattern = ctx.get[List[AssemblyLine]](i)
    if (code.length < pattern.length) return None
    val (before, after) = code.splitAt(pattern.length)
    val lastIndex = code.length - 1
    for (((a, (f, b)), ix) <- pattern.zip(before).zipWithIndex) {
      if (!b.elidable) return None
      if (a.opcode != b.opcode) return None
      if (a.addrMode != b.addrMode) return None
      if (a.parameter.quickSimplify != b.parameter.quickSimplify) return None
      if (ix == 0 && !firstLinePattern.matchLineTo(ctx, f, b)) return None
      if (ix == lastIndex && !lastLinePattern.matchLineTo(ctx, f, b)) return None
    }
    Some(after)
  }
}