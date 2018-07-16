package millfork.assembly.mos.opt

import millfork.CompilationOptions
import millfork.assembly._
import millfork.assembly.mos._
import millfork.assembly.opt.SingleStatus
import millfork.env._
import millfork.error.ErrorReporting
import millfork.node.{MosNiceFunctionProperty, NiceFunctionProperty}

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

trait AssemblyRuleSet{
  def flatten: Seq[AssemblyRule]
}

class RuleBasedAssemblyOptimization(val name: String, val needsFlowInfo: FlowInfoRequirement.Value, val rules: AssemblyRuleSet*) extends AssemblyOptimization[AssemblyLine] {

  private val actualRules = rules.flatMap(_.flatten)
  actualRules.foreach(_.pattern.validate(needsFlowInfo))


  override def optimize(f: NormalFunction, code: List[AssemblyLine], optimizationContext: OptimizationContext): List[AssemblyLine] = {
    val effectiveCode = code.map(a => a.copy(parameter = a.parameter.quickSimplify))
    val taggedCode = FlowAnalyzer.analyze(f, effectiveCode, optimizationContext, needsFlowInfo)
    optimizeImpl(f, taggedCode, optimizationContext)
  }

  def optimizeImpl(f: NormalFunction, code: List[(FlowInfo, AssemblyLine)], optimizationContext: OptimizationContext): List[AssemblyLine] = {
    code match {
      case Nil => Nil
      case head :: tail =>
        for ((rule, index) <- actualRules.zipWithIndex) {
          val ctx = new AssemblyMatchingContext(optimizationContext.options, optimizationContext.labelMap, optimizationContext.zreg, optimizationContext.niceFunctionProperties)
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
                return optimizedChunk ++ optimizeImpl(f, rest, optimizationContext)
              } else {
                return optimize(f, optimizedChunk ++ rest.map(_._2), optimizationContext)
              }
            case None => ()
          }
        }
        head._2 :: optimizeImpl(f, tail, optimizationContext)
    }
  }
}

class AssemblyMatchingContext(val compilationOptions: CompilationOptions,
                              val labelMap: Map[String, Int],
                              val zeropageRegister: Option[ThingInMemory],
                              val niceFunctionProperties: Set[(NiceFunctionProperty, String)]) {

  def functionChangesA(name: String): Boolean = !niceFunctionProperties(MosNiceFunctionProperty.DoesntChangeA -> name)

  def functionChangesX(name: String): Boolean = !niceFunctionProperties(MosNiceFunctionProperty.DoesntChangeX -> name)

  def functionChangesY(name: String): Boolean = !niceFunctionProperties(MosNiceFunctionProperty.DoesntChangeY -> name)

  def functionChangesIZ(name: String): Boolean = !niceFunctionProperties(MosNiceFunctionProperty.DoesntChangeIZ -> name)

  def functionChangesAH(name: String): Boolean = !niceFunctionProperties(MosNiceFunctionProperty.DoesntChangeAH -> name)

  def functionChangesC(name: String): Boolean = !niceFunctionProperties(MosNiceFunctionProperty.DoesntChangeC -> name)

  def functionReadsD(name: String): Boolean = !niceFunctionProperties(MosNiceFunctionProperty.DoesntConcernD -> name)

  def functionChangesMemory(name: String): Boolean = !niceFunctionProperties(NiceFunctionProperty.DoesntWriteMemory -> name)

  def functionReadsMemory(name: String): Boolean = !niceFunctionProperties(NiceFunctionProperty.DoesntReadMemory -> name)

  private val map = mutable.Map[Int, Any]()

  override def toString: String = if (map.isEmpty) "<empty context>" else map.mkString(", ")

  def addObject(i: Int, o: Any): Boolean = {
    if (map.contains(i)) {
      map(i) == o
    } else {
      map(i) = o
      true
    }
  }

  def addAddrModeLoosely(i: Int, o: AddrMode.Value): Boolean = {
    if (map.contains(i)) {
      val a = map(i).asInstanceOf[AddrMode.Value]
      a == o || a == AddrMode.ZeroPage && o == AddrMode.Absolute || a == AddrMode.Absolute && o == AddrMode.ZeroPage
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

  private def getImpl[T: Manifest](i: Int): AnyRef = {
    if (!map.contains(i)) return null
    val t = map(i)
    val clazz = implicitly[Manifest[T]].runtimeClass match {
      case java.lang.Integer.TYPE => classOf[java.lang.Integer]
      case java.lang.Boolean.TYPE => classOf[java.lang.Boolean]
      // TODO
      case x => x
    }
    if (clazz.isInstance(t)) {
      t.asInstanceOf[AnyRef]
    } else {
      if (i eq null) {
        ErrorReporting.fatal(s"Value at index $i is null")
      } else {
        ErrorReporting.fatal(s"Value at index $i is a ${t.getClass.getSimpleName}, not a ${clazz.getSimpleName}")
      }
    }
  }

  def get[T: Manifest](i: Int): T = {
    val v = getImpl[T](i)
    if (v eq null) {
      ErrorReporting.fatal(s"Value at index $i is null")
    }
    v.asInstanceOf[T]
  }

  def getOrDefault[T: Manifest](i: Int, defau: T): T = {
    val v = getImpl[T](i)
    if (v eq null) {
      defau
    } else {
      v.asInstanceOf[T]
    }
  }

  def isExternallyLinearBlock(i: Int): Boolean = {
    val labels = mutable.Set[String]()
    val jumps = mutable.Set[String]()
    get[List[AssemblyLine]](i).foreach {
      // JSR and BSR are allowed
      case AssemblyLine(Opcode.RTS | Opcode.RTI | Opcode.RTL | Opcode.BRK, _, _, _) =>
        return false
      case AssemblyLine(Opcode.JMP, AddrMode.Indirect | AddrMode.AbsoluteIndexedX | AddrMode.LongIndirect, _, _) =>
        return false
      case AssemblyLine(Opcode.LABEL, _, MemoryAddressConstant(Label(l)), _) =>
        labels += l
      case AssemblyLine(Opcode.JMP, AddrMode.Absolute, MemoryAddressConstant(Label(l)), _) =>
        jumps += l
      case AssemblyLine(Opcode.JMP, AddrMode.Absolute | AddrMode.LongAbsolute, _, _) =>
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

  def zreg(i: Int): Constant = {
    MemoryAddressConstant(zeropageRegister.get) + i
  }

}

case class AssemblyRule(pattern: AssemblyPattern, result: (List[AssemblyLine], AssemblyMatchingContext) => List[AssemblyLine]) extends AssemblyRuleSet {
  override def flatten: Seq[AssemblyRule] = List(this)
}

case class MultipleAssemblyRules(list: Seq[AssemblyRuleSet]) extends AssemblyRuleSet {
  override def flatten: Seq[AssemblyRule] = list.flatMap(_.flatten)
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

}
object HelperCheckers {
  import AddrMode._
  private val badAddrModes = Set(IndexedX, IndexedY, IndexedZ, LongIndexedY, LongIndexedZ, IndexedSY, Indirect, TripleAbsolute, Stack)
  private val goodAddrModes = Set(Implied, Immediate, WordImmediate, Relative, LongRelative)

  def memoryAccessDoesntOverlap(l1: AssemblyLine, l2: AssemblyLine): Boolean = {
    val a1 = l1.addrMode
    val a2 = l2.addrMode
    if (goodAddrModes(a1) || goodAddrModes(a2)) return true
    if (badAddrModes(a1) || badAddrModes(a2)) return false
    if ((a1 == IndexedSY) != (a2 == IndexedSY)) return true // bold assertion, but usually true
    val p1 = l1.parameter
    val p2 = l2.parameter
    val w1 = OpcodeClasses.AccessesWordInMemory(l1.opcode)
    val w2 = OpcodeClasses.AccessesWordInMemory(l2.opcode)

    def distinctThings(a: String, b: String): Boolean = {
      if (a == "__reg") return b != "__reg"
      if (b == "__reg") return a != "__reg"
      a.takeWhile(_ != '.') != b.takeWhile(_ != '.')
    }

    def handleKnownDistance(distance: Short): Boolean = {
      // `distance` is the distance between the first byte that can be addressed by l1 (b1) and the first byte that can be addressed by l2 (b2): (b2-b1)
      val indexingAddrModes = Set(AbsoluteIndexedX, AbsoluteX, ZeroPageX, AbsoluteY, ZeroPageY, LongAbsoluteX)
      val a1Indexing = indexingAddrModes(a1)
      val a2Indexing = indexingAddrModes(a2)
      (a1Indexing, a2Indexing) match {
        case (false, false) => distance != 0 && (distance != 1 || !w1) && (distance != -1 || !w2)
        case (true, false) => distance > 255 || distance < 0 && (distance != 256 || !w1) && (distance != -1 || !w2)
        case (false, true) => distance > 0 || distance < -255 && (distance != 1 || !w1) && (distance != -256 || !w2)
        case (true, true) => distance > 255 || distance < -255 && (distance != 265 || !w1) && (distance != -256 || !w2)
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
        distinctThings(a.name, b.name) // TODO: ???
      case (CompoundConstant(op@(MathOperator.Plus | MathOperator.Minus), MemoryAddressConstant(a: ThingInMemory), NumericConstant(offset, _)),
      MemoryAddressConstant(b: ThingInMemory)) =>
        if (a.name == b.name) {
          if (op == MathOperator.Plus) {
            handleKnownDistance((-offset).toShort)
          } else {
            handleKnownDistance(offset.toShort)
          }
        } else {
          distinctThings(a.name, b.name) // TODO: ???
        }
      case (MemoryAddressConstant(a: ThingInMemory),
      CompoundConstant(op@(MathOperator.Plus | MathOperator.Minus), MemoryAddressConstant(b: ThingInMemory), NumericConstant(offset, _))) =>
        if (a.name == b.name) {
          if (op == MathOperator.Minus) {
            handleKnownDistance((-offset).toShort)
          } else {
            handleKnownDistance(offset.toShort)
          }
        } else {
          distinctThings(a.name, b.name) // TODO: ???
        }
      case (CompoundConstant(op1@(MathOperator.Plus | MathOperator.Minus), MemoryAddressConstant(a: ThingInMemory), NumericConstant(o1, _)),
      CompoundConstant(op2@(MathOperator.Plus | MathOperator.Minus), MemoryAddressConstant(b: ThingInMemory), NumericConstant(o2, _))) =>
        if (a.name == b.name) {
          val offset1 = if (op1==MathOperator.Plus) o1 else -o1
          val offset2 = if (op2==MathOperator.Plus) o2 else -o2
          handleKnownDistance((offset2 - offset1).toShort)
        } else {
          distinctThings(a.name, b.name) // TODO: ???
        }
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


case class Where(predicate: AssemblyMatchingContext => Boolean) extends AssemblyPattern {
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

  def |(x: AssemblyLinePattern): AssemblyLinePattern = EitherPattern(this, x)

  def &(x: AssemblyLinePattern): AssemblyLinePattern = Both(this, x)
}

//noinspection ScalaUnnecessaryParentheses
trait TrivialAssemblyLinePattern extends AssemblyLinePattern with (AssemblyLine => Boolean) {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = this (line)
}

case class Match(predicate: AssemblyMatchingContext => Boolean) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = predicate(ctx)

  override def toString: String = "Match(...)"
}

case class WhereNoMemoryAccessOverlapBetweenTwoLineLists(ix1: Int, ix2: Int) extends AssemblyPattern {
  override def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, AssemblyLine)]): Option[List[(FlowInfo, AssemblyLine)]] = {
    val s1s = ctx.get[List[AssemblyLine]](ix1)
    val s2s = ctx.get[List[AssemblyLine]](ix2)
    if (s1s.forall(s1 => s2s.forall(s2 => HelperCheckers.memoryAccessDoesntOverlap(s1, s2)))) Some(code) else None
  }
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

case class HasZ(value: Int) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.statusBefore.iz.contains(value)
}

case class DoesntMatterWhatItDoesWith(states: State.Value*) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertBackward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    states.forall(state => flowInfo.importanceAfter.isUnimportant(state))

  override def toString: String = states.mkString("[¯\\_(ツ)_/¯:", ",", "]")
}

case class DoesntMatterWhatItDoesWithReg(index: Int) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertBackward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.importanceAfter.isPseudoregisterUnimportant(index)

  override def toString: String = s"[¯\\_(ツ)_/¯: __reg+$index]"
}

case class HasSet(state: State.Value) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.hasSet(state)
}

case object XContainsStackPointer extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.statusBefore.eqSX
}

case class HasSourceOfNZ(state: State.Value) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.statusBefore.src.exists(s => s.matches(state))
}

object HasAccu8 extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.hasSet(State.M)
}

object HasAccu16 extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.hasClear(State.M)
}

object HasIndex8 extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.hasSet(State.W)
}

object HasIndex16 extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.hasClear(State.W)
}

case class HasClear(state: State.Value) extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.hasClear(state)
}

case object HasClearBitA0 extends AssemblyLinePattern {
  override def validate(needsFlowInfo: FlowInfoRequirement.Value): Unit =
    FlowInfoRequirement.assertForward(needsFlowInfo)

  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    flowInfo.statusBefore.a0.contains(false)
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

case class EitherPattern(l: AssemblyLinePattern, r: AssemblyLinePattern) extends AssemblyLinePattern {
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

case object DebugMatching extends AssemblyPattern {
  override def matchTo(ctx: AssemblyMatchingContext, code: List[(FlowInfo, AssemblyLine)]): Option[List[(FlowInfo, AssemblyLine)]] = {
    println(ctx)
    code.foreach(println)
    Some(code)
  }
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

case object ReadsAH extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    OpcodeClasses.ReadsAHAlways(line.opcode) || line.addrMode == AddrMode.Implied && OpcodeClasses.ReadsAHIfImplied(line.opcode)
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

case object IsNonvolatile extends TrivialAssemblyLinePattern {
  // TODO: this is a big hack
  override def apply(line: AssemblyLine): Boolean =
    line.addrMode match {
      case AddrMode.Implied | AddrMode.Immediate => true
      case _ => line.parameter match {
        case MemoryAddressConstant(_) => true
        case CompoundConstant(_, MemoryAddressConstant(_), _) => true
        case _ => false
      }
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

case object ConcernsAH extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    OpcodeClasses.ConcernsAHAlways(line.opcode) || line.addrMode == AddrMode.Implied && OpcodeClasses.ConcernsAHIfImplied(line.opcode)
}

case object ConcernsX extends TrivialAssemblyLinePattern {
  val XAddrModes = Set(AddrMode.AbsoluteX, AddrMode.AbsoluteIndexedX, AddrMode.LongAbsoluteX, AddrMode.IndexedX, AddrMode.ZeroPageX)

  override def apply(line: AssemblyLine): Boolean =
    OpcodeClasses.ConcernsXAlways(line.opcode) || XAddrModes(line.addrMode)
}

case object ConcernsS extends TrivialAssemblyLinePattern {
  val SAddrModes = Set(AddrMode.Stack, AddrMode.IndexedSY)

  override def apply(line: AssemblyLine): Boolean =
    OpcodeClasses.ConcernsSAlways(line.opcode) || SAddrModes(line.addrMode)
}

case object ConcernsY extends TrivialAssemblyLinePattern {
  val YAddrModes = Set(AddrMode.AbsoluteY, AddrMode.IndexedSY, AddrMode.IndexedY, AddrMode.LongIndexedY, AddrMode.ZeroPageY)

  override def apply(line: AssemblyLine): Boolean =
    OpcodeClasses.ConcernsYAlways(line.opcode) || YAddrModes(line.addrMode)
}

case object ConcernsStack extends TrivialAssemblyLinePattern {
  val SAddrModes = Set(AddrMode.IndexedSY, AddrMode.Stack)

  override def apply(line: AssemblyLine): Boolean =
    OpcodeClasses.ConcernsStackAlways(line.opcode) || SAddrModes(line.addrMode)
}

case object ConcernsIZ extends TrivialAssemblyLinePattern {
  val IZAddrModes = Set(AddrMode.IndexedZ, AddrMode.LongIndexedZ)

  override def apply(line: AssemblyLine): Boolean =
    OpcodeClasses.ConcernsIZAlways(line.opcode) || IZAddrModes(line.addrMode)
}

case object ChangesA extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = {
    import Opcode._
    import AddrMode._
    line match {
      case AssemblyLine(JSR, Absolute | LongAbsolute, MemoryAddressConstant(th), _) => ctx.functionChangesA(th.name)
      case AssemblyLine(_, Implied, _, _) => OpcodeClasses.ChangesAIfImplied(line.opcode) || OpcodeClasses.ChangesAAlways(line.opcode)
      case _ => OpcodeClasses.ChangesAAlways(line.opcode)
    }
  }
}

case object ChangesX extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = {
    import Opcode._
    import AddrMode._
    line match {
      case AssemblyLine(JSR, Absolute | LongAbsolute, MemoryAddressConstant(th), _) => ctx.functionChangesX(th.name)
      case _ => OpcodeClasses.ChangesX(line.opcode)
    }
  }
}

case object ChangesY extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = {
    import Opcode._
    import AddrMode._
    line match {
      case AssemblyLine(JSR, Absolute | LongAbsolute, MemoryAddressConstant(th), _) => ctx.functionChangesY(th.name)
      case _ => OpcodeClasses.ChangesY(line.opcode)
    }
  }
}

case object ReadsNOrZ extends HasOpcodeIn(OpcodeClasses.ReadsNOrZ)

case object ReadsC extends HasOpcodeIn(OpcodeClasses.ReadsC)

case object ReadsD extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = {
    import Opcode._
    import AddrMode._
    line match {
      case AssemblyLine(JSR | BSR, Absolute | LongAbsolute, MemoryAddressConstant(th), _) => ctx.functionReadsD(th.name)
      case _ => OpcodeClasses.ReadsD(line.opcode)
    }
  }
}

case object ReadsV extends HasOpcodeIn(OpcodeClasses.ReadsV)

case object ChangesNAndZ extends HasOpcodeIn(OpcodeClasses.ChangesNAndZ)

case object ChangesC extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = {
    import Opcode._
    import AddrMode._
    line match {
      case AssemblyLine(JSR | BSR, Absolute | LongAbsolute, MemoryAddressConstant(th), _) => ctx.functionChangesC(th.name)
      case _ => OpcodeClasses.ChangesC(line.opcode)
    }
  }
}

case object ChangesV extends HasOpcodeIn(OpcodeClasses.ChangesV)

case object ChangesStack extends HasOpcodeIn(OpcodeClasses.ChangesStack)

case object ChangesIZ extends HasOpcodeIn(OpcodeClasses.ChangesIZ)

case object ChangesS extends HasOpcodeIn(OpcodeClasses.ChangesS)

case object SupportsAbsolute extends HasOpcodeIn(OpcodeClasses.SupportsAbsolute)

case object SupportsAbsoluteX extends HasOpcodeIn(OpcodeClasses.SupportsAbsoluteX)

case object SupportsAbsoluteY extends HasOpcodeIn(OpcodeClasses.SupportsAbsoluteY)

case object ShortConditionalBranching extends HasOpcodeIn(OpcodeClasses.ShortConditionalBranching)

case object ShortBranching extends HasOpcodeIn(OpcodeClasses.ShortBranching)

case object OverwritesA extends HasOpcodeIn(OpcodeClasses.OverwritesA)

case object OverwritesX extends HasOpcodeIn(OpcodeClasses.OverwritesX)

case object OverwritesY extends HasOpcodeIn(OpcodeClasses.OverwritesY)

case object NoopDiscardsFlags extends HasOpcodeIn(OpcodeClasses.NoopDiscardsFlags)

case object NoopDiscardsFlagsOrLabel extends HasOpcodeIn(OpcodeClasses.NoopDiscardsFlags + Opcode.LABEL)

case object ChangesAH extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = {
    import Opcode._
    import AddrMode._
    line match {
      case AssemblyLine(JSR, Absolute | LongAbsolute, MemoryAddressConstant(th), _) => ctx.functionChangesAH(th.name)
      case AssemblyLine(_, Implied, _, _) => OpcodeClasses.ChangesAHIfImplied(line.opcode) || OpcodeClasses.ChangesAHAlways(line.opcode)
      case _ => OpcodeClasses.ChangesAHAlways(line.opcode)
    }
  }
}

case object ChangesM extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean = line match {
    case AssemblyLine(Opcode.SEP | Opcode.REP, AddrMode.Immediate, NumericConstant(n, _), _) => (n & 0x20) != 0
    case AssemblyLine(Opcode.SEP | Opcode.REP | Opcode.PLP | Opcode.XCE, _, _, _) => true
    case _ => false
  }
}
case object ChangesW extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean = line match {
    case AssemblyLine(Opcode.SEP | Opcode.REP, AddrMode.Immediate, NumericConstant(n, _), _) => (n & 0x10) != 0
    case AssemblyLine(Opcode.SEP | Opcode.REP | Opcode.PLP | Opcode.XCE, _, _, _) => true
    case _ => false
  }
}

case object ChangesMemory extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = {
    import AddrMode._
    import Opcode._
    line match {
      case AssemblyLine(JSR | BSR, Absolute | LongAbsolute, MemoryAddressConstant(th), _) => ctx.functionChangesMemory(th.name)
      case AssemblyLine(op, Implied, _, _) => OpcodeClasses.ChangesMemoryAlways(op)
      case AssemblyLine(op, _, _, _) => OpcodeClasses.ChangesMemoryAlways(op) || OpcodeClasses.ChangesMemoryIfNotImplied(op)
      case _ => false
    }
  }
}

case class DoesntChangeMemoryAt(addrMode1: Int, param1: Int, opcode: Opcode.Value = Opcode.NOP) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = {
    import AddrMode._
    import Opcode._
    line match {
      case AssemblyLine(JSR | BSR, Absolute | LongAbsolute, MemoryAddressConstant(th), _) => !ctx.functionChangesMemory(th.name)
      case _ =>
        val p1 = ctx.get[Constant](param1)
        val a1 = ctx.get[AddrMode.Value](addrMode1)
        val changesSomeMemory = OpcodeClasses.ChangesMemoryAlways(line.opcode) || line.addrMode != AddrMode.Implied && OpcodeClasses.ChangesMemoryIfNotImplied(line.opcode)
        // TODO: NOP
        // this will break if the actual instruction was 16-bit
        !changesSomeMemory || HelperCheckers.memoryAccessDoesntOverlap(AssemblyLine(opcode, a1, p1), line)
    }
  }
}

case object ConcernsMemory extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    ReadsMemory.matchLineTo(ctx, flowInfo, line) || ChangesMemory.matchLineTo(ctx, flowInfo, line)
}

case class DoesNotConcernMemoryAt(addrMode1: Int, param1: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = {
    {
      import AddrMode._
      import Opcode._
      line match {
        case AssemblyLine(JSR | BSR, Absolute | LongAbsolute, MemoryAddressConstant(th), _) => !ctx.functionReadsMemory(th.name) && !ctx.functionChangesMemory(th.name)
        case _ =>
          val p1 = ctx.get[Constant](param1)
          val a1 = ctx.get[AddrMode.Value](addrMode1)
          // TODO: NOP
          // this will break if the actual instruction was 16-bit
          HelperCheckers.memoryAccessDoesntOverlap(AssemblyLine(Opcode.NOP, a1, p1), line)
      }
    }
  }
}

case class HasOpcode(op: Opcode.Value) extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    line.opcode == op

  override def toString: String = op.toString
}

case class RefersTo(identifier: String, offset: Int = 999) extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean = {
    (line.addrMode == AddrMode.ZeroPage || line.addrMode == AddrMode.Absolute || line.addrMode == AddrMode.LongAbsolute) && (line.parameter match {
      case MemoryAddressConstant(th) =>
        (offset == 999 || offset == 0) && th.name == identifier
      case CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(nn, _)) =>
        (offset == 999 || offset == nn) && th.name == identifier
      case CompoundConstant(MathOperator.Plus, NumericConstant(nn, _), MemoryAddressConstant(th)) =>
        (offset == 999 || offset == nn) && th.name == identifier
      case _ => false
    })
  }

  override def toString: String = s"<$identifier+$offset>"
}

case class RefersToOrUses(identifier: String, offset: Int = 999) extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean = {
    line.addrMode match {
      case AddrMode.ZeroPage | AddrMode.Absolute | AddrMode.LongAbsolute | AddrMode.Indirect => line.parameter match {
        case MemoryAddressConstant(th) =>
          (offset == 999 || offset == 0) && th.name == identifier
        case CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(nn, _)) =>
          (offset == 999 || offset == nn) && th.name == identifier
        case CompoundConstant(MathOperator.Plus, NumericConstant(nn, _), MemoryAddressConstant(th)) =>
          (offset == 999 || offset == nn) && th.name == identifier
        case CompoundConstant(MathOperator.Minus, MemoryAddressConstant(th), NumericConstant(nn, _)) =>
          (offset == 999 || offset == -nn) && th.name == identifier
        case _ => false
      }
      case AddrMode.IndexedY | AddrMode.LongIndexedY | AddrMode.IndexedZ | AddrMode.LongIndexedZ => line.parameter match {
        case MemoryAddressConstant(th) =>
          (offset == 999 || offset == 0 || offset == 1) && th.name == identifier
        case CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(nn, _)) =>
          (offset == 999 || offset == nn || offset == nn + 1) && th.name == identifier
        case CompoundConstant(MathOperator.Plus, NumericConstant(nn, _), MemoryAddressConstant(th)) =>
          (offset == 999 || offset == nn || offset == nn + 1) && th.name == identifier
        case CompoundConstant(MathOperator.Minus, MemoryAddressConstant(th), NumericConstant(nn, _)) =>
          (offset == 999 || offset == -nn || offset == -nn + 1) && th.name == identifier
        case _ => false
      }
      case AddrMode.AbsoluteX | AddrMode.AbsoluteY | AddrMode.AbsoluteIndexedX => line.parameter match {
        case MemoryAddressConstant(th) =>
          (offset == 999 || offset >= 0 && offset <= 255) && th.name == identifier
        case CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(nn, _)) =>
          (offset == 999 || offset >= nn && offset <= nn + 255) && th.name == identifier
        case CompoundConstant(MathOperator.Plus, NumericConstant(nn, _), MemoryAddressConstant(th)) =>
          (offset == 999 || offset >= nn && offset <= nn + 255) && th.name == identifier
        case CompoundConstant(MathOperator.Minus, MemoryAddressConstant(th), NumericConstant(nn, _)) =>
          (offset == 999 || offset >= -nn && offset <= -nn + 255) && th.name == identifier
        case _ => false
      }
      case AddrMode.IndexedX => line.parameter match {
        case MemoryAddressConstant(th) => th.name == identifier
        case CompoundConstant(MathOperator.Plus, MemoryAddressConstant(th), NumericConstant(nn, _)) => th.name == identifier
        case CompoundConstant(MathOperator.Plus, NumericConstant(nn, _), MemoryAddressConstant(th)) => th.name == identifier
        case CompoundConstant(MathOperator.Minus, MemoryAddressConstant(th), NumericConstant(nn, _)) => th.name == identifier
        case _ => false
      }
      case _ => false
    }
  }

  override def toString: String = s"<$identifier+$offset>"
}

case class CallsAnyOf(identifiers: Set[String]) extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean = {
    (line.addrMode == AddrMode.Absolute ||
      line.addrMode == AddrMode.LongAbsolute ||
      line.addrMode == AddrMode.LongRelative) && (line.parameter match {
      case MemoryAddressConstant(th) => identifiers(th.name)
      case _ => false
    })
  }

  override def toString: String = identifiers.mkString("(JSR {", ",", "})")
}

case class CallsAnyExcept(identifiers: Set[String]) extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean = {
    (line.addrMode == AddrMode.Absolute ||
      line.addrMode == AddrMode.LongAbsolute ||
      line.addrMode == AddrMode.LongRelative) && (line.parameter match {
      case MemoryAddressConstant(th) => !identifiers(th.name)
      case _ => false
    })
  }

  override def toString: String = identifiers.mkString("(JSR ¬{", ",", "})")
}

class HasOpcodeIn(val ops: Set[Opcode.Value]) extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    ops(line.opcode)

  override def toString: String = ops.mkString("{", ",", "}")

  def |(that: HasOpcodeIn): HasOpcodeIn = new HasOpcodeIn(ops ++ that.ops)

  def --(that: HasOpcodeIn): HasOpcodeIn = new HasOpcodeIn(ops -- that.ops)
}

object HasOpcodeIn {
  def apply(ops: Opcode.Value*): HasOpcodeIn = new HasOpcodeIn(ops.toSet)
  def apply(ops: Set[Opcode.Value]): HasOpcodeIn = new HasOpcodeIn(ops)
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

  def |(that: HasAddrModeIn): HasAddrModeIn = HasAddrModeIn(ams ++ that.ams)

  def --(that: HasAddrModeIn): HasAddrModeIn = HasAddrModeIn(ams -- that.ams)
}
object HasAddrModeIn {
  def apply(ams: AddrMode.Value*): HasAddrModeIn = HasAddrModeIn(ams.toSet)
}

case class HasImmediate(i: Int) extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    line.addrMode == AddrMode.Immediate && (line.parameter.quickSimplify match {
      case NumericConstant(j, _) => (i & 0xff) == (j & 0xff)
      case _ => false
    })

  override def toString: String = "#" + i
}

case class HasImmediateWhere(predicate: Int => Boolean) extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean =
    line.addrMode == AddrMode.Immediate && (line.parameter.quickSimplify match {
      case NumericConstant(j, _) => predicate(j.toInt & 0xff)
      case _ => false
    })
}

case class HasParameterWhere(predicate: Constant => Boolean) extends TrivialAssemblyLinePattern {
  override def apply(line: AssemblyLine): Boolean = predicate(line.parameter)
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
    ctx.addAddrModeLoosely(i, line.addrMode)

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

case class MatchNumericImmediate(i: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    if (line.addrMode == AddrMode.Immediate) {
      line.parameter.quickSimplify match {
        case NumericConstant(value, _) => ctx.addObject(i, value.toInt & 0xff)
        case _ => false
      }
    } else false

  override def toString: String = s"(?<$i>#)"
}


case class DoesntChangeIndexingInAddrMode(i: Int) extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean =
    ctx.get[AddrMode.Value](i) match {
      case AddrMode.ZeroPageX | AddrMode.AbsoluteX | AddrMode.LongAbsoluteX | AddrMode.IndexedX | AddrMode.AbsoluteIndexedX => !ChangesX.matchLineTo(ctx, flowInfo, line)
      case AddrMode.ZeroPageY | AddrMode.AbsoluteY | AddrMode.IndexedY | AddrMode.LongIndexedY => !ChangesY.matchLineTo(ctx, flowInfo, line)
      case AddrMode.IndexedZ | AddrMode.LongIndexedZ => !ChangesIZ.matchLineTo(ctx, flowInfo, line)
      case AddrMode.Stack => !OpcodeClasses.ChangesS.contains(line.opcode)
      case AddrMode.IndexedSY => !OpcodeClasses.ChangesS.contains(line.opcode) && !ChangesY.matchLineTo(ctx, flowInfo, line)
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

case object IsZeroPage extends AssemblyLinePattern {
  override def matchLineTo(ctx: AssemblyMatchingContext, flowInfo: FlowInfo, line: AssemblyLine): Boolean = {
    import Opcode._
    line match {
      case AssemblyLine(_, AddrMode.ZeroPage, _, _) => true
      case l@AssemblyLine(LDA | STA | CMP |
                          LDX | STX | CPX |
                          LDY | STY | CPY |
                          LDZ | STZ | CPZ |
                          BIT |
                          ADC | SBC | AND | ORA | EOR |
                          INC | DEC | ROL | ROR | ASL | LSR |
                          ISC | DCP | LAX | SAX | RLA | RRA | SLO | SRE, AddrMode.Absolute, p, true) =>
        p match {
          case NumericConstant(n, _) => n <= 255
          case MemoryAddressConstant(th) => ctx.labelMap.getOrElse(th.name, 0x800) < 0x100
          case CompoundConstant(MathOperator.Plus,
          MemoryAddressConstant(th),
          NumericConstant(n, _)) => ctx.labelMap.getOrElse(th.name, 0x800) + n < 0x100
          case _ => false
        }
      case _ => false
    }
  }
}